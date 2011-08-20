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
         protected
          function target_specific_explicit_typeconv: tnode; override;
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


    function tjvmtypeconvnode.target_specific_explicit_typeconv: tnode;

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
        frominclass,
        toinclass: boolean;
        fromdef,
        todef: tdef;
      begin
        result:=nil;
        { This routine is only called for explicit typeconversions of same-sized
          entities that aren't handled by normal type conversions -> bit pattern
          reinterpretations. In the JVM, many of these also need special
          handling because of the type safety. }

        { don't allow conversions between object-based and non-object-based
          types }
        frominclass:=
          (left.resultdef.typ=objectdef) or
          is_dynamic_array(left.resultdef);
        toinclass:=
          (resultdef.typ=objectdef) or
          is_dynamic_array(resultdef);
        if frominclass and
           toinclass then
          begin
            { we need an as-node to check the validity of the conversion (since
              it wasn't handled by another type conversion, we know it can't
              have been valid normally)

              Exception: (most nested) destination is java.lang.Object, since
                everything is compatible with that type }
            fromdef:=left.resultdef;
            todef:=resultdef;
            get_most_nested_types(fromdef,todef);
            if ((fromdef.typ<>objectdef) and
                not is_dynamic_array(fromdef)) or
               (todef<>java_jlobject) then
              begin
                result:=ctypenode.create(resultdef);
                if resultdef.typ=objectdef then
                  result:=cloadvmtaddrnode.create(result);
                result:=casnode.create(left,result);
                left:=nil;
              end;
            exit;
          end;

        { don't allow conversions between different classes of primitive types,
          except for a few special cases }

        { float to int/enum explicit type conversion: get the bits }
        if (left.resultdef.typ=floatdef) and
           (is_integer(resultdef) or
            (resultdef.typ=enumdef)) then
          begin
            result:=int_real_explicit_typecast(tfloatdef(left.resultdef),'FLOATTORAWINTBITS','DOUBLETORAWLONGBITS');
            exit;
          end;
        { int to float explicit type conversion: also use the bits }
        if (is_integer(left.resultdef) or
            (left.resultdef.typ=enumdef)) and
           (resultdef.typ=floatdef) then
          begin
            result:=int_real_explicit_typecast(tfloatdef(resultdef),'INTBITSTOFLOAT','LONGBITSTODOUBLE');
            exit;
          end;
        { nothing special required when going between ordinals and enums }
        if (left.resultdef.typ in [orddef,enumdef])=(resultdef.typ in [orddef,enumdef]) then
          exit;

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


    {*****************************************************************************
                         AsNode and IsNode common helpers
    *****************************************************************************}

  function asis_target_specific_typecheck(node: tasisnode): boolean;
    var
      fromelt, toelt: tdef;
    begin
      { dynamic arrays can be converted to java.lang.Object and vice versa }
      if node.right.resultdef=java_jlobject then
        { dynamic array to java.lang.Object }
        result:=is_dynamic_array(node.left.resultdef)
      else if is_dynamic_array(node.right.resultdef) then
        begin
          { <x> to dynamic array: only if possibly valid }
          fromelt:=node.left.resultdef;
          toelt:=node.right.resultdef;
          get_most_nested_types(fromelt,toelt);
          { final levels must be convertable:
              a) from dynarray to java.lang.Object or vice versa, or
              b) the same primitive/class type
          }
          result:=
           (compare_defs(fromelt,toelt,node.left.nodetype) in [te_exact,te_equal]) or
           (((fromelt.typ=objectdef) or
             is_dynamic_array(fromelt)) and
            ((toelt.typ=objectdef) or
             is_dynamic_array(toelt)));
        end
      else
        begin
          { full class reference support requires using the Java reflection API,
            not yet implemented }
          if (node.right.nodetype<>loadvmtaddrn) or
             (tloadvmtaddrnode(node.right).left.nodetype<>typen) then
            internalerror(2011012601);
          result:=false;
        end;
      if result then
        if node.nodetype=asn then
          begin
            if node.right.resultdef.typ<>classrefdef then
              node.resultdef:=node.right.resultdef
            else
              node.resultdef:=tclassrefdef(node.right.resultdef).pointeddef
          end
        else
          node.resultdef:=pasbool8type;
    end;


  procedure asis_generate_code(node: tasisnode; opcode: tasmop);
    var
      checkdef: tdef;
    begin
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
      if checkdef.typ=objectdef then
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(opcode,current_asmdata.RefAsmSymbol(tobjectdef(checkdef).jvm_full_typename(true))))
      else
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(opcode,current_asmdata.RefAsmSymbol(jvmencodetype(checkdef))));
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
      { call-by-reference does not exist in Java, so it's no problem to
        change a memory location to a register }
      firstpass(left);
      if right.nodetype<>typen then
        firstpass(right);
      expectloc:=LOC_REGISTER;
      result:=nil;
    end;


  procedure tjvmasnode.pass_generate_code;
    begin
      asis_generate_code(self,a_checkcast);
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
      firstpass(left);
      if right.nodetype<>typen then
        firstpass(right);
      expectloc:=LOC_REGISTER;
      result:=nil;
    end;


  procedure tjvmisnode.pass_generate_code;
    begin
      asis_generate_code(self,a_instanceof);
    end;




begin
  ctypeconvnode:=tjvmtypeconvnode;
  casnode:=tjvmasnode;
  cisnode:=tjvmisnode;
end.
