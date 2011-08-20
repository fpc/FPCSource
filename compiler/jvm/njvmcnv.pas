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
          function simplify(forinline: boolean): tnode; override;

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
          procedure second_elem_to_openarray; override;
          function target_specific_explicit_typeconv: boolean; override;
          function target_specific_general_typeconv: boolean; override;
         protected
          function do_target_specific_explicit_typeconv(check_only: boolean; out resnode: tnode): boolean;
       end;

       tjvmasnode = class(tcgasnode)
        protected
         { to discern beween "obj as tclassref" and "tclassref(obj)" }
         classreftypecast: boolean;
         function target_specific_typecheck: boolean;override;
        public
         function pass_1 : tnode;override;
         procedure pass_generate_code; override;
         function dogetcopy: tnode; override;
         function docompare(p: tnode): boolean; override;
         constructor ppuload(t: tnodetype; ppufile: tcompilerppufile); override;
         procedure ppuwrite(ppufile: tcompilerppufile); override;
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

    function isvalidprocvartypeconv(fromdef, todef: tdef): boolean;

      var
        tmethoddef: tdef;

      function docheck(def1,def2: tdef): boolean;
        begin
          result:=false;
          if def1.typ<>procvardef then
            exit;
          if tprocvardef(def1).is_addressonly then
            result:=
              (def2=java_jlobject) or
              (def2=voidpointertype)
          else
            begin
              if not assigned(tmethoddef) then
                tmethoddef:=search_system_type('TMETHOD').typedef;
              result:=
                (def2=methodpointertype) or
                (def2=tmethoddef);
            end;
        end;

      begin
        tmethoddef:=nil;
        result:=
          docheck(fromdef,todef) or
          docheck(todef,fromdef);
      end;


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

    function tjvmtypeconvnode.simplify(forinline: boolean): tnode;
      begin
        result:=inherited simplify(forinline);
        if assigned(result) then
          exit;
        { string constants passed to java.lang.String must be converted to
          widestring }
        if (left.nodetype=stringconstn) and
           not(tstringconstnode(left).cst_type in [cst_unicodestring,cst_widestring]) and
           (maybe_find_real_class_definition(resultdef,false)=java_jlstring) then
          inserttypeconv(left,cunicodestringtype);
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


    procedure tjvmtypeconvnode.second_elem_to_openarray;
      var
        primitivetype: boolean;
        opc: tasmop;
        mangledname: string;
        basereg: tregister;
        arrayref: treference;
      begin
        { create an array with one element of the required type }
        thlcgjvm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,s32inttype,1,R_INTREGISTER);
        mangledname:=jvmarrtype(left.resultdef,primitivetype);
        if primitivetype then
          opc:=a_newarray
        else
          opc:=a_anewarray;
        { doesn't change stack height: one int replaced by one reference }
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(opc,current_asmdata.RefAsmSymbol(mangledname)));
        { store the data in the newly created array }
        basereg:=hlcg.getaddressregister(current_asmdata.CurrAsmList,java_jlobject);
        thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,java_jlobject,basereg);
        reference_reset_base(arrayref,basereg,0,4);
        arrayref.arrayreftype:=art_indexconst;
        arrayref.indexoffset:=0;
        hlcg.a_load_loc_ref(current_asmdata.CurrAsmList,left.resultdef,left.resultdef,left.location,arrayref);
        location_reset_ref(location,LOC_REFERENCE,OS_ADDR,4);
        tg.gethltemp(current_asmdata.CurrAsmList,java_jlobject,4,tt_normal,location.reference);
        hlcg.a_load_reg_ref(current_asmdata.CurrAsmList,java_jlobject,java_jlobject,basereg,location.reference);
      end;


    procedure get_most_nested_types(var fromdef, todef: tdef);
      begin
       while is_dynamic_array(fromdef) and
             is_dynamic_array(todef) do
         begin
           fromdef:=tarraydef(fromdef).elementdef;
           todef:=tarraydef(todef).elementdef;
         end;
       fromdef:=maybe_find_real_class_definition(fromdef,false);
       todef:=maybe_find_real_class_definition(todef,false);
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

      function ptr_no_typecheck_required(fromdef, todef: tdef): boolean;

        function check_type_equality(def1,def2: tdef): boolean;
          begin
            result:=true;
            if is_ansistring(def1) and
               (def2=java_ansistring) then
              exit;
            if is_wide_or_unicode_string(def1) and
               (def2=java_jlstring) then
              exit;
            if def1.typ=pointerdef then
              begin
                if is_shortstring(tpointerdef(def1).pointeddef) and
                   (def2=java_shortstring) then
                  exit;
              end;
            result:=false;
          end;

        function check_array_type_equality(def1,def2: tdef): boolean;
          begin
            result:=true;
            if is_shortstring(def1) and
               (def2=java_shortstring) then
              exit;
            result:=false;
          end;

        begin
          result:=true;
          if (todef=java_jlobject) or
             (todef=voidpointertype) then
            exit;
          if compare_defs(fromdef,todef,nothingn)>=te_equal then
            exit;
          { trecorddef.is_related() must work for inheritance/method checking,
            but do not allow records to be directly typecasted into class/
            pointer types (you have to use FpcBaseRecordType(@rec) instead) }
          if not is_record(fromdef) and
             fromdef.is_related(todef) then
            exit;
          if check_type_equality(fromdef,todef) then
            exit;
          if check_type_equality(todef,fromdef) then
            exit;
          if (fromdef.typ=pointerdef) and
             (tpointerdef(fromdef).pointeddef.typ=recorddef) and
             (todef=java_fpcbaserecordtype) then
            exit;
          { all classrefs are currently java.lang.Class at the bytecode level }
          if (fromdef.typ=classrefdef) and
             (todef.typ=objectdef) and
             (todef=search_system_type('JLCLASS').typedef) then
            exit;
          if (fromdef.typ=classrefdef) and
             (todef.typ=classrefdef) and
             tclassrefdef(fromdef).pointeddef.is_related(tclassrefdef(todef).pointeddef) then
            exit;
          { special case: "array of shortstring" to "array of ShortstringClass"
            and "array of <record>" to "array of FpcRecordBaseType" (normally
            you have to use ShortstringClass(@shortstrvar) etc, but that's not
            possible in case of passing arrays to e.g. setlength) }
          if is_dynamic_array(left.resultdef) and
             is_dynamic_array(resultdef) then
            begin
             if check_array_type_equality(fromdef,todef) or
                check_array_type_equality(todef,fromdef) then
               exit;
             if is_record(fromdef) and
                (todef=java_fpcbaserecordtype) then
               exit;
            end;
          result:=false;
        end;

      var
        fromclasscompatible,
        toclasscompatible,
        procvarconv: boolean;
        fromdef,
        todef: tdef;
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
        procvarconv:=isvalidprocvartypeconv(left.resultdef,resultdef);
        fromclasscompatible:=
          (left.resultdef.typ=formaldef) or
          (left.resultdef.typ=pointerdef) or
          (left.resultdef.typ=objectdef) or
          is_dynamic_array(left.resultdef) or
          ((left.resultdef.typ in [stringdef,classrefdef]) and
           not is_shortstring(left.resultdef)) or
          procvarconv;
        toclasscompatible:=
          (resultdef.typ=pointerdef) or
          (resultdef.typ=objectdef) or
          is_dynamic_array(resultdef) or
          ((resultdef.typ in [stringdef,classrefdef]) and
           not is_shortstring(resultdef)) or
          procvarconv;
        { typescasts from void (the result of untyped_ptr^) to an implicit
          pointertype (record, array, ...) also needs a typecheck }
        if is_void(left.resultdef) and
           jvmimplicitpointertype(resultdef) then
          begin
            fromclasscompatible:=true;
            toclasscompatible:=true;
          end;

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

            { in case of arrays, check the compatibility of the innermost types }
            fromdef:=left.resultdef;
            todef:=resultdef;
            get_most_nested_types(fromdef,todef);
            fromarrtype:=jvmarrtype_setlength(fromdef);
            toarrtype:=jvmarrtype_setlength(todef);
            if not ptr_no_typecheck_required(fromdef,todef) then
              begin
                if (fromarrtype in ['A','R','T']) or
                   (fromarrtype<>toarrtype) then
                  begin
                    if not check_only and
                       not assignment_side then
                      begin
                        resnode:=ctypenode.create(resultdef);
                        if resultdef.typ=objectdef then
                          resnode:=cloadvmtaddrnode.create(resnode);
                        resnode:=casnode.create_internal(left,resnode);
                        if resultdef.typ=classrefdef then
                          tjvmasnode(resnode).classreftypecast:=true;
                        left:=nil;
                      end
                  end
                { typecasting from a child to a parent type on the assignment side
                  will (rightly) mess up the type safety verification of the JVM }
                else if assignment_side then
                  CGMessage(type_e_no_managed_assign_generic_typecast);
              end;
            result:=true;
            exit;
          end;

        { a formaldef can be converted to anything, but not on the assignment
          side }
        if (left.resultdef.typ=formaldef) and
           not assignment_side then
          exit;

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
        if (is_integer(left.resultdef) or
            (left.resultdef.typ=enumdef)) and
           (resultdef.typ=floatdef) then
          begin
            if (convtype<>tc_int_2_real) then
              begin
                if not check_only then
                  resnode:=int_real_explicit_typecast(tfloatdef(resultdef),'INTBITSTOFLOAT','LONGBITSTODOUBLE');
                result:=true;
              end
            else
              result:=false;
            exit;
          end;
        { nothing special required when going between ordinals and enums }
        if (left.resultdef.typ in [orddef,enumdef]) and
           (resultdef.typ in [orddef,enumdef]) then
          begin
            result:=false;
            exit;
          end;

{$ifndef nounsupported}
        if (left.resultdef.typ in [orddef,enumdef,setdef]) and
           (resultdef.typ in [orddef,enumdef,setdef]) then
          begin
            result:=false;
            exit;
          end;

        { non-literal type conversions }
        if convtype in
             [tc_char_2_string,
              tc_char_2_chararray,
              tc_string_2_string,
              tc_string_2_chararray,
              tc_real_2_real,
              tc_proc_2_procvar,
              tc_arrayconstructor_2_set,
              tc_set_to_set,
              tc_class_2_intf,
              tc_array_2_dynarray] then
          begin
            result:=false;
            exit;
          end;
{$endif}

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
        result:=true;
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
{$ifndef nounsupported}
        { generated in nmem; replace voidpointertype with java_jlobject }
        if nf_load_procvar in flags then
          begin
            self.totypedef:=java_jlobject;
            resultdef:=java_jlobject;
          end;
        if isvalidprocvartypeconv(left.resultdef,resultdef) then
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
      realtodef: tdef;
    begin
      if not(nf_internal in node.flags) then
        begin
          { handle using normal code }
          result:=false;
          exit;
        end;
      result:=true;
      { these are converted type conversion nodes, to insert the checkcast
        operations }
      realtodef:=node.right.resultdef;
      if (realtodef.typ=classrefdef) and
         ((node.nodetype<>asn) or
          not tjvmasnode(node).classreftypecast) then
        realtodef:=tclassrefdef(realtodef).pointeddef;
      realtodef:=maybe_find_real_class_definition(realtodef,false);
      if result then
        if node.nodetype=asn then
          node.resultdef:=realtodef
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
      if checkdef=voidpointertype then
        checkdef:=java_jlobject
      else if checkdef.typ=pointerdef then
        checkdef:=tpointerdef(checkdef).pointeddef;
{$ifndef nounsupported}
      if checkdef.typ=procvardef then
        checkdef:=java_jlobject
{$endif}
      else if is_wide_or_unicode_string(checkdef) then
        checkdef:=java_jlstring
      else if is_ansistring(checkdef) then
        checkdef:=java_ansistring
      else if is_shortstring(checkdef) then
        checkdef:=java_shortstring;
      if checkdef.typ in [objectdef,recorddef] then
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(opcode,current_asmdata.RefAsmSymbol(tabstractrecorddef(checkdef).jvm_full_typename(true))))
      else if checkdef.typ=classrefdef then
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(opcode,current_asmdata.RefAsmSymbol('java/lang/Class')))
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


  function tjvmasnode.dogetcopy: tnode;
    begin
      result:=inherited dogetcopy;
      tjvmasnode(result).classreftypecast:=classreftypecast;
    end;


  function tjvmasnode.docompare(p: tnode): boolean;
    begin
      result:=
        inherited docompare(p) and
        (tjvmasnode(p).classreftypecast=classreftypecast);
    end;


  constructor tjvmasnode.ppuload(t: tnodetype; ppufile: tcompilerppufile);
    begin
      inherited;
      classreftypecast:=boolean(ppufile.getbyte);
    end;


  procedure tjvmasnode.ppuwrite(ppufile: tcompilerppufile);
    begin
      inherited ppuwrite(ppufile);
      ppufile.putbyte(byte(classreftypecast));
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
