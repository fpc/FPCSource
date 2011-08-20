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
      node,ncnv,ncgcnv,defcmp;

    type
       tjvmtypeconvnode = class(tcgtypeconvnode)
         protected
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
       end;

implementation

   uses
      verbose,globals,globtype,
      symconst,symtype,symdef,aasmbase,aasmdata,
      defutil,
      cgbase,cgutils,pass_1,pass_2,
      ncon,ncal,procinfo,
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
        href : treference;
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
        href: treference;
        hreg2 : tregister;
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


begin
  ctypeconvnode:=tjvmtypeconvnode;
end.
