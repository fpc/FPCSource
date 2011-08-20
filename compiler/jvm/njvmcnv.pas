{
    Copyright (c) 1998-2011 by Florian Klaempfl and Jonas Maebe

    Generate JVM code for type converting nodes

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}
unit njvmcnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncnv,ncgcnv,
      symtype;

    type
       tjvmtypeconvnode = class(tcgtypeconvnode)
          function typecheck_dynarray_to_openarray: tnode; override;
          function typecheck_string_to_chararray: tnode; override;
          function pass_1: tnode; override;

          procedure second_int_to_int;override;
         { procedure second_string_to_string;override; }
         { procedure second_cstring_to_pchar;override; }
         { procedure second_string_to_chararray;override; }
         { procedure second_array_to_pointer;override; }
          function first_int_to_real: tnode; override;
         { procedure second_pointer_to_array;override; }
         { procedure second_chararray_to_string;override; }
         { procedure second_char_to_string;override; }
          procedure second_int_to_real;override;
         { procedure second_real_to_real;override; }
         { procedure second_cord_to_pointer;override; }
         { procedure second_proc_to_procvar;override; }
          procedure second_bool_to_int;override;
          procedure second_int_to_bool;override;
         { procedure second_load_smallset;override;  }
         { procedure second_ansistring_to_pchar;override; }
         { procedure second_pchar_to_string;override; }
         { procedure second_class_to_intf;override; }
         { procedure second_char_to_char;override; }
          function target_specific_explicit_typeconv: boolean; override;
          function target_specific_general_typeconv: boolean; override;
         protected
          function do_target_specific_explicit_typeconv(check_only: boolean; out resnode: tnode): boolean;
       end;

       tjvmasnode = class(tcgasnode)
        protected
         function target_specific_typecheck: boolean;override;
        public
         function pass_1 : tnode;override;
         procedure pass_generate_code; override;
       end;

       tjvmisnode = class(tisnode)
        protected
         function target_specific_typecheck: boolean;override;
        public
         function pass_1 : tnode;override;
         procedure pass_generate_code; override;
       end;

implementation

   uses
      verbose,globals,globtype,
      symconst,symdef,symsym,symtable,aasmbase,aasmdata,
      defutil,defcmp,jvmdef,
      cgbase,cgutils,pass_1,pass_2,
      nbas,ncon,ncal,nld,nmem,procinfo,
      nutils,
      cpubase,aasmcpu,
      tgobj,hlcgobj,hlcgcpu;


{*****************************************************************************
                            TypeCheckTypeConv
*****************************************************************************}

   function tjvmtypeconvnode.typecheck_dynarray_to_openarray: tnode;
     begin
       { all arrays are equal in Java }
       left.resultdef:=resultdef;
       result:=left;
       left:=nil;
     end;


   function tjvmtypeconvnode.typecheck_string_to_chararray: tnode;
     var
       newblock: tblocknode;
       newstat: tstatementnode;
       restemp: ttempcreatenode;
       chartype: string;
     begin
       if (left.nodetype = stringconstn) and
          (tstringconstnode(left).cst_type=cst_conststring) then
         inserttypeconv(left,cunicodestringtype);
       { even constant strings have to be handled via a helper }
       if is_widechar(tarraydef(resultdef).elementdef) then
         chartype:='widechar'
       else
         chartype:='char';
       newblock:=internalstatements(newstat);
       restemp:=ctempcreatenode.create(resultdef,resultdef.size,tt_persistent,false);
       addstatement(newstat,restemp);
       addstatement(newstat,ccallnode.createintern('fpc_'+tstringdef(left.resultdef).stringtypname+
         '_to_'+chartype+'array',ccallparanode.create(left,ccallparanode.create(
         ctemprefnode.create(restemp),nil))));
       addstatement(newstat,ctempdeletenode.create_normal_temp(restemp));
       addstatement(newstat,ctemprefnode.create(restemp));
       result:=newblock;
       left:=nil;
     end;


{*****************************************************************************
                             FirstTypeConv
*****************************************************************************}

    function tjvmtypeconvnode.first_int_to_real: tnode;
      begin
        if not is_64bitint(left.resultdef) then
          if is_signed(left.resultdef) or
             (left.resultdef.size<4) then
            inserttypeconv(left,s32inttype)
          else
            inserttypeconv(left,u32inttype);
        firstpass(left);
        result := nil;
        expectloc:=LOC_FPUREGISTER;
      end;


    function tjvmtypeconvnode.pass_1: tnode;
      begin
        if (nf_explicit in flags) then
          begin
            do_target_specific_explicit_typeconv(false,result);
            if assigned(result) then
              exit;
          end;
        result:=inherited pass_1;
      end;


{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    procedure tjvmtypeconvnode.second_int_to_int;
      var
        ressize,
        leftsize : longint;
      begin
        { insert range check if not explicit conversion }
        if not(nf_explicit in flags) then
          hlcg.g_rangecheck(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef);

        { is the result size smaller? when typecasting from void
          we always reuse the current location, because there is
          nothing that we can load in a register }
        ressize:=resultdef.size;
        leftsize :=left.resultdef.size;
        if ((ressize<>leftsize) or
            ((location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) and
             (location.reference.arrayreftype<>art_none) and
             (is_widechar(left.resultdef)<>is_widechar(resultdef))) or
            is_bitpacked_access(left)) and
           not is_void(left.resultdef) then
          begin
            location_copy(location,left.location);
            { reuse a loc_reference when the newsize is smaller than
              than the original, except
                a) for arrays (they use different load instructions for
                   differently sized data types)
                b) when going from 8 to 4 bytes, because these are different
                   data types

               -- note that this is different from other targets, and will
                  break stuff like passing byte(shortintvar) to a var-parameter;
                  although that may be "fixed" again because we have to use
                  copy-in/copy-out to emulate var-parameters anyway... }
            if (location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) and
               (location.reference.arrayreftype=art_none) and
               (ressize<leftsize) and
               (leftsize<=4) then
              begin
                location.size:=def_cgsize(resultdef);
                { no adjustment of the ffset even though Java is big endian,
                  because the load instruction will remain the same }
              end
            else
              hlcg.location_force_reg(current_asmdata.CurrAsmList,location,left.resultdef,resultdef,false);
          end
        else
          begin
            location_copy(location,left.location);
            location.size:=def_cgsize(resultdef);
            if (ressize < sizeof(aint)) and
               (location.loc in [LOC_REGISTER,LOC_CREGISTER]) and
               (def_cgsize(left.resultdef)<>def_cgsize(resultdef)) then
              begin
                location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
                location.loc:=LOC_REGISTER;
                hlcg.a_load_reg_reg(current_asmdata.CurrAsmList,left.resultdef,resultdef,left.location.register,location.register);
              end;
          end;
      end;


    procedure tjvmtypeconvnode.second_int_to_real;
      var
        srcsize, ressize: longint;

      procedure convertsignedstackloc;
        begin
          case srcsize of
            4:
              case ressize of
                4:
                  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_i2f));
                8:
                  begin
                    current_asmdata.CurrAsmList.concat(taicpu.op_none(a_i2d));
                    thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,1);
                  end;
                else
                  internalerror(2011010601);
              end;
            8:
              case ressize of
                4:
                  begin
                    current_asmdata.CurrAsmList.concat(taicpu.op_none(a_l2f));
                    thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1);
                  end;
                8:
                  current_asmdata.CurrAsmList.concat(taicpu.op_none(a_l2d));
                else
                  internalerror(2011010602);
              end;
            else
              internalerror(2011010603);
          end;
        end;

      var
        signeddef : tdef;
        l1 : tasmlabel;

      begin
        srcsize:=left.resultdef.size;
        ressize:=resultdef.size;

        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getfpuregister(current_asmdata.CurrAsmList,resultdef);

        { first always convert as if it's a signed number }
        thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
        convertsignedstackloc;
        if not is_signed(left.resultdef) then
          begin
            { if it was unsigned, add high(cardinal)+1/high(qword)+1 in case
              the signed interpretation is < 0 }
            current_asmdata.getjumplabel(l1);
            if srcsize=4 then
              signeddef:=s32inttype
            else
              signeddef:=s64inttype;
            hlcg.a_cmp_const_loc_label(current_asmdata.CurrAsmList,signeddef,OC_GTE,0,left.location,l1);
            if srcsize=4 then
              thlcgjvm(hlcg).a_loadfpu_const_stack(current_asmdata.CurrAsmList,resultdef,4294967296.0)
            else
              thlcgjvm(hlcg).a_loadfpu_const_stack(current_asmdata.CurrAsmList,resultdef,18446744073709551616.0);
            if ressize=4 then
              current_asmdata.CurrAsmList.concat(taicpu.op_none(a_fadd))
            else
              current_asmdata.CurrAsmList.concat(taicpu.op_none(a_dadd));
            hlcg.a_label(current_asmdata.CurrAsmList,l1);
          end;
        thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,resultdef,location.register);
      end;


    procedure tjvmtypeconvnode.second_bool_to_int;
      var
         newsize: tcgsize;
         oldTrueLabel,oldFalseLabel : tasmlabel;
      begin
         oldTrueLabel:=current_procinfo.CurrTrueLabel;
         oldFalseLabel:=current_procinfo.CurrFalseLabel;
         current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
         current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
         secondpass(left);
         location_copy(location,left.location);
         newsize:=def_cgsize(resultdef);
         { byte(bytebool) or word(wordbool) or longint(longbool) must be }
         { accepted for var parameters and assignments, and must not     }
         { change the ordinal value or value location.                   }
         { htypechk.valid_for_assign ensures that such locations with a  }
         { size<sizeof(register) cannot be LOC_CREGISTER (they otherwise }
         { could be in case of a plain assignment), and LOC_REGISTER can }
         { never be an assignment target. The remaining LOC_REGISTER/    }
         { LOC_CREGISTER locations do have to be sign/zero-extended.     }

         {   -- Note: this does not work for Java and 2/4 byte sized
                      values, because bytebool/wordbool are signed and
                      are stored in 4 byte locations -> will result in
                      "byte" with the value high(cardinal); see remark
                      in second_int_to_int above regarding consequences }
         if not(nf_explicit in flags) or
            (location.loc in [LOC_FLAGS,LOC_JUMP]) or
            ((newsize<>left.location.size) and
             ((left.resultdef.size<>resultdef.size) or
              not(left.resultdef.size in [4,8]))
            ) then
           hlcg.location_force_reg(current_asmdata.CurrAsmList,location,left.resultdef,resultdef,true)
         else
           { may differ in sign, e.g. bytebool -> byte   }
           location.size:=newsize;
         current_procinfo.CurrTrueLabel:=oldTrueLabel;
         current_procinfo.CurrFalseLabel:=oldFalseLabel;
      end;


    procedure tjvmtypeconvnode.second_int_to_bool;
      var
        hlabel1,hlabel2,oldTrueLabel,oldFalseLabel : tasmlabel;
        newsize  : tcgsize;
      begin
        oldTrueLabel:=current_procinfo.CurrTrueLabel;
        oldFalseLabel:=current_procinfo.CurrFalseLabel;
        current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
        current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
        secondpass(left);
        if codegenerror then
          exit;

        { Explicit typecasts from any ordinal type to a boolean type }
        { must not change the ordinal value                          }
        if (nf_explicit in flags) and
           not(left.location.loc in [LOC_FLAGS,LOC_JUMP]) then
          begin
             location_copy(location,left.location);
             newsize:=def_cgsize(resultdef);
             { change of size? change sign only if location is LOC_(C)REGISTER? Then we have to sign/zero-extend }
             if (tcgsize2size[newsize]<>tcgsize2size[left.location.size]) or
                ((newsize<>left.location.size) and (location.loc in [LOC_REGISTER,LOC_CREGISTER])) then
               hlcg.location_force_reg(current_asmdata.CurrAsmList,location,left.resultdef,resultdef,true)
             else
               location.size:=newsize;
             current_procinfo.CurrTrueLabel:=oldTrueLabel;
             current_procinfo.CurrFalseLabel:=oldFalseLabel;
             exit;
          end;

       location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
       location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
       current_asmdata.getjumplabel(hlabel2);
       case left.location.loc of
         LOC_CREFERENCE,LOC_REFERENCE,LOC_REGISTER,LOC_CREGISTER:
           begin
             current_asmdata.getjumplabel(hlabel1);
             hlcg.a_cmp_const_loc_label(current_asmdata.CurrAsmList,left.resultdef,OC_EQ,0,left.location,hlabel1);
           end;
         LOC_JUMP :
           begin
             hlabel1:=current_procinfo.CurrFalseLabel;
             hlcg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrTrueLabel);
           end;
         else
           internalerror(10062);
       end;

       if not(is_cbool(resultdef)) then
         thlcgjvm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,resultdef,1,R_INTREGISTER)
       else
         thlcgjvm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,resultdef,-1,R_INTREGISTER);
       { we jump over the next constant load -> they don't appear on the
         stack simulataneously }
       thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1);
       hlcg.a_jmp_always(current_asmdata.CurrAsmList,hlabel2);
       hlcg.a_label(current_asmdata.CurrAsmList,hlabel1);
       thlcgjvm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,resultdef,0,R_INTREGISTER);
       hlcg.a_label(current_asmdata.CurrAsmList,hlabel2);
       thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,resultdef,location.register);

       current_procinfo.CurrTrueLabel:=oldTrueLabel;
       current_procinfo.CurrFalseLabel:=oldFalseLabel;
     end;


    procedure get_most_nested_types(var fromdef, todef: tdef);
      begin
       while is_dynamic_array(fromdef) and
             is_dynamic_array(todef) do
         begin
           fromdef:=tarraydef(fromdef).elementdef;
           todef:=tarraydef(todef).elementdef;
         end;
      end;


    function tjvmtypeconvnode.do_target_specific_explicit_typeconv(check_only: boolean; out resnode: tnode): boolean;

      { handle explicit typecast from int to to real or vice versa }
      function int_real_explicit_typecast(fdef: tfloatdef; const singlemethod, doublemethod: string): tnode;
        var
          csym: ttypesym;
          psym: tsym;
        begin
         { use the float/double to raw bits methods to get the bit pattern }
          if fdef.floattype=s32real then
            begin
              csym:=search_system_type('JLFLOAT');
              psym:=search_struct_member(tobjectdef(csym.typedef),singlemethod);
            end
          else
            begin
              csym:=search_system_type('JLDOUBLE');
              psym:=search_struct_member(tobjectdef(csym.typedef),doublemethod);
            end;
          if not assigned(psym) or
             (psym.typ<>procsym) then
            internalerror(2011012901);
          { call the (static class) method to get the raw bits }
          result:=ccallnode.create(ccallparanode.create(left,nil),
            tprocsym(psym),psym.owner,
            cloadvmtaddrnode.create(ctypenode.create(csym.typedef)),[]);
          { convert the result to the result type of this type conversion node }
          inserttypeconv_explicit(result,resultdef);
          { left is reused }
          left:=nil;
        end;


      var
        fromclasscompatible,
        toclasscompatible: boolean;
        fromdef,
        todef,
        jlclass: tdef;
        fromarrtype,
        toarrtype: char;
      begin
        resnode:=nil;
        { This routine is only called for explicit typeconversions of same-sized
          entities that aren't handled by normal type conversions -> bit pattern
          reinterpretations. In the JVM, many of these also need special
          handling because of the type safety. }

        { don't allow conversions between object-based and non-object-based
          types }
        fromclasscompatible:=
          (left.resultdef.typ=objectdef) or
          is_dynamic_array(left.resultdef) or
          ((left.resultdef.typ in [recorddef,stringdef]) and
           (resultdef.typ=objectdef));
        toclasscompatible:=
          (resultdef.typ=objectdef) or
          is_dynamic_array(resultdef) or
          ((resultdef.typ in [recorddef,stringdef]) and
           (left.resultdef.typ=objectdef));
        if fromclasscompatible and toclasscompatible then
          begin
            { we need an as-node to check the validity of the conversion (since
              it wasn't handled by another type conversion, we know it can't
              have been valid normally)

              Exceptions: (most nested) destination is
                * java.lang.Object, since everything is compatible with that type
                * related to source
                * a primitive that are represented by the same type in Java
                  (e.g., byte and shortint) }
            fromdef:=left.resultdef;
            todef:=resultdef;
            get_most_nested_types(fromdef,todef);
            fromarrtype:=jvmarrtype_setlength(fromdef);
            toarrtype:=jvmarrtype_setlength(todef);
            if (compare_defs(fromdef,todef,nothingn)<te_equal) and
               not fromdef.is_related(todef) and
               (((fromdef.typ in [objectdef,recorddef,stringdef]) and
                 not is_dynamic_array(fromdef)) or
                (todef<>java_jlobject)) and
               ((fromarrtype in ['A','R']) or
                (fromarrtype<>toarrtype)) then
              begin
                if not check_only and
                   not assignment_side then
                  begin
                    resnode:=ctypenode.create(resultdef);
                    if resultdef.typ=objectdef then
                      resnode:=cloadvmtaddrnode.create(resnode);
                    resnode:=casnode.create(left,resnode);
                    left:=nil;
                  end
              end
            { typecasting from a child to a parent type on the assignment side
              will (rightly) mess up the type safety verification of the JVM }
            else if assignment_side and
                    (compare_defs(fromdef,todef,nothingn)<te_equal) then
              CGMessage(type_e_no_managed_assign_generic_typecast);
            result:=true;
            exit;
          end;

        { from classrefdef to JLClass and JLObject and back }
        if (left.resultdef.typ=classrefdef) or
           (resultdef.typ=classrefdef) then
          begin
            if (left.resultdef.typ=classrefdef) and
               (resultdef.typ=classrefdef) then
              begin
                if not tclassrefdef(left.resultdef).pointeddef.is_related(resultdef) and
                   not tclassrefdef(resultdef).pointeddef.is_related(left.resultdef) then
                 CGMessage2(type_e_illegal_type_conversion,left.resultdef.typename,resultdef.typename);
              end
            else
              begin
                jlclass:=search_system_type('JLCLASS').typedef;
                if (left.resultdef<>jlclass) and
                   (left.resultdef<>java_jlobject) and
                   (resultdef<>jlclass) and
                   (resultdef<>java_jlobject) then
                  CGMessage2(type_e_illegal_type_conversion,left.resultdef.typename,resultdef.typename);
              end;
            result:=true;
            exit;
          end;


        { don't allow conversions between different classes of primitive types,
          except for a few special cases }

        { float to int/enum explicit type conversion: get the bits }
        if (convtype<>tc_int_2_real) and
           (left.resultdef.typ=floatdef) and
           (is_integer(resultdef) or
            (resultdef.typ=enumdef)) then
          begin
            if not check_only then
              resnode:=int_real_explicit_typecast(tfloatdef(left.resultdef),'FLOATTORAWINTBITS','DOUBLETORAWLONGBITS');
            result:=true;
            exit;
          end;
        { int to float explicit type conversion: also use the bits }
        if (convtype<>tc_int_2_real) and
           (is_integer(left.resultdef) or
            (left.resultdef.typ=enumdef)) and
           (resultdef.typ=floatdef) then
          begin
            if not check_only then
              resnode:=int_real_explicit_typecast(tfloatdef(resultdef),'INTBITSTOFLOAT','LONGBITSTODOUBLE');
            result:=true;
            exit;
          end;
        { nothing special required when going between ordinals and enums }
        if (left.resultdef.typ in [orddef,enumdef]) and
           (resultdef.typ in [orddef,enumdef]) then
          begin
            result:=false;
            exit;
          end;

{ifndef nounsupported}
        result:=false;
        exit;
{endif}

        { Todo:
            * int to set and vice versa
            * set to float and vice versa (via int) (maybe)
            * regular array of primitive to primitive and vice versa (maybe)
            * packed record to primitive and vice versa (maybe)
          Definitely not:
            * unpacked record to anything and vice versa (no alignment rules
              for Java)
        }
        { anything not explicitly handled is a problem }
        CGMessage2(type_e_illegal_type_conversion,left.resultdef.typename,resultdef.typename);
      end;


    function tjvmtypeconvnode.target_specific_explicit_typeconv: boolean;
      var
        dummyres: tnode;
      begin
        result:=do_target_specific_explicit_typeconv(true,dummyres);
      end;



    function tjvmtypeconvnode.target_specific_general_typeconv: boolean;
      begin
        result:=false;
        { deal with explicit typecasts between records and classes (for
          FpcBaseRecordType) }
        if ((left.resultdef.typ=recorddef) and
            (resultdef.typ=objectdef) and
            left.resultdef.is_related(resultdef)) or
           ((left.resultdef.typ=objectdef) and
            (resultdef.typ=recorddef) and
            resultdef.is_related(left.resultdef)) and
           (nf_explicit in flags) then
          begin
            convtype:=tc_equal;
            result:=true;
            exit;
          end;

{$ifndef nounsupported}
        if ((left.resultdef.typ=procvardef) and
            ((resultdef=methodpointertype) or
             (resultdef=search_system_type('TMETHOD').typedef))) or
           ((resultdef.typ=procvardef) and
            ((left.resultdef=methodpointertype)  or
             (left.resultdef=search_system_type('TMETHOD').typedef))) then
          begin
            convtype:=tc_equal;
            result:=true;
            exit;
          end;
{$endif}
      end;


    {*****************************************************************************
                         AsNode and IsNode common helpers
    *****************************************************************************}

  function asis_target_specific_typecheck(node: tasisnode): boolean;
    var
      fromelt, toelt: tdef;
      realfromdef,
      realtodef: tdef;

    function isrecordconv(var res: boolean): boolean;
      begin
        if is_record(realtodef) then
          begin
            result:=true;
            res:=
              (realfromdef=java_jlobject) or
              (realfromdef=java_fpcbaserecordtype);
          end
        else if is_record(realfromdef) then
          begin
            result:=true;
            res:=
              (realtodef=java_jlobject) or
              (realtodef=java_fpcbaserecordtype)
          end
        else
          result:=false;
      end;

    function isstringconv(var res: boolean): boolean;
      begin
        if is_wide_or_unicode_string(realtodef) then
          begin
            result:=true;
            res:=
              (realfromdef=java_jlobject) or
              (realfromdef=java_jlstring)
          end
        else if is_wide_or_unicode_string(realfromdef) then
          begin
            result:=true;
            res:=
              (realtodef=java_jlobject) or
              (realtodef=java_jlstring)
          end
        else
          result:=false;
      end;

    begin
      realfromdef:=maybe_find_real_class_definition(node.left.resultdef,false);
      realtodef:=node.right.resultdef;
      if realtodef.typ=classrefdef then
        realtodef:=tclassrefdef(realtodef).pointeddef;
      realtodef:=maybe_find_real_class_definition(realtodef,false);
      if not isrecordconv(result) and
         not isstringconv(result) then
        { dynamic arrays can be converted to java.lang.Object and vice versa }
        if realtodef=java_jlobject then
          { dynamic array to java.lang.Object }
          result:=is_dynamic_array(realfromdef)
        else if is_dynamic_array(realtodef) then
          begin
            { <x> to dynamic array: only if possibly valid }
            fromelt:=node.left.resultdef;
            toelt:=realtodef;
            get_most_nested_types(fromelt,toelt);
            { final levels must be convertable:
                a) from array (dynamic or not) to java.lang.Object or vice versa,
                 or
                b) the same primitive/class type
            }
            if not isrecordconv(result) then
              result:=
               (compare_defs(fromelt,toelt,node.left.nodetype) in [te_exact,te_equal]) or
               (((fromelt.typ=objectdef) or
                 (fromelt.typ=arraydef)) and
                ((toelt.typ=objectdef) or
                 (toelt.typ=arraydef)));
          end
        else
          begin
            if (node.right.resultdef.typ<>classrefdef) then
              result:=false
            else
              result:=true;
          end;
      if result then
        if node.nodetype=asn then
          begin
            if realtodef.typ<>classrefdef then
              node.resultdef:=realtodef
            else
              node.resultdef:=tclassrefdef(realtodef).pointeddef
          end
        else
          node.resultdef:=pasbool8type;
    end;


  function asis_pass_1(node: tasisnode; const methodname: string): tnode;
    var
      ps: tsym;
      call: tnode;
      jlclass: tobjectdef;
    begin
      result:=nil;
      firstpass(node.left);
      if not(node.right.nodetype in [typen,loadvmtaddrn]) then
        begin
          if (node.nodetype=isn) or
             not assigned(tasnode(node).call) then
            begin
              if not is_javaclassref(node.right.resultdef) then
                internalerror(2011041920);
              firstpass(node.right);
              jlclass:=tobjectdef(search_system_type('JLCLASS').typedef);
              ps:=search_struct_member(jlclass,methodname);
              if not assigned(ps) or
                 (ps.typ<>procsym) then
                internalerror(2011041910);
              call:=ccallnode.create(ccallparanode.create(node.left,nil),tprocsym(ps),ps.owner,ctypeconvnode.create_explicit(node.right,jlclass),[]);
              node.left:=nil;
              node.right:=nil;
              firstpass(call);
              if codegenerror then
                exit;
              if node.nodetype=isn then
                result:=call
              else
                begin
                  tasnode(node).call:=call;
                  node.expectloc:=call.expectloc;
                end;
            end;
        end
      else
        begin
          node.expectloc:=LOC_REGISTER;
          result:=nil;
        end;
    end;


  function asis_generate_code(node: tasisnode; opcode: tasmop): boolean;
    var
      checkdef: tdef;
    begin
      if (node.nodetype=asn) and
         assigned(tasnode(node).call) then
        begin
          result:=false;
          exit;
        end;
      result:=true;
      secondpass(node.left);
      thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,node.left.resultdef,node.left.location);
      location_freetemp(current_asmdata.CurrAsmList,node.left.location);
      { Perform a checkcast instruction, which will raise an exception in case
        the actual type does not match/inherit from the expected type.

        Object types need the full type name (package+class name), arrays only
        the array definition }
      if node.nodetype=asn then
        checkdef:=node.resultdef
      else if node.right.resultdef.typ=classrefdef then
        checkdef:=tclassrefdef(node.right.resultdef).pointeddef
      else
        checkdef:=node.right.resultdef;
      { replace special types with their equivalent class type }
      if is_wide_or_unicode_string(checkdef) then
        checkdef:=java_jlstring;
      if checkdef.typ in [objectdef,recorddef] then
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(opcode,current_asmdata.RefAsmSymbol(tabstractrecorddef(checkdef).jvm_full_typename(true))))
      else
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(opcode,current_asmdata.RefAsmSymbol(jvmencodetype(checkdef,false))));
      location_reset(node.location,LOC_REGISTER,OS_ADDR);
      node.location.register:=hlcg.getaddressregister(current_asmdata.CurrAsmList,node.resultdef);
      thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,node.resultdef,node.location.register);
    end;

    {*****************************************************************************
                                 TJVMAsNode
    *****************************************************************************}

  function tjvmasnode.target_specific_typecheck: boolean;
    begin
      result:=asis_target_specific_typecheck(self);
    end;


  function tjvmasnode.pass_1: tnode;
    begin
      result:=asis_pass_1(self,'CAST');
    end;


  procedure tjvmasnode.pass_generate_code;
    begin
      if not asis_generate_code(self,a_checkcast) then
        inherited;
    end;


  {*****************************************************************************
                               TJVMIsNode
  *****************************************************************************}


  function tjvmisnode.target_specific_typecheck: boolean;
    begin
      result:=asis_target_specific_typecheck(self);
    end;


  function tjvmisnode.pass_1: tnode;
    begin
      result:=asis_pass_1(self,'ISINSTANCE');
    end;


  procedure tjvmisnode.pass_generate_code;
    begin
      if not asis_generate_code(self,a_instanceof) then
        inherited;
    end;




begin
  ctypeconvnode:=tjvmtypeconvnode;
  casnode:=tjvmasnode;
  cisnode:=tjvmisnode;
end.
