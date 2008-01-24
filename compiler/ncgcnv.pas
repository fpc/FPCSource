{
    Copyright (c) 2000-2002 by Florian Klaempfl

    Generate assembler for nodes that handle type conversions which are
    the same for all (most) processors

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

 ****************************************************************************
}
unit ncgcnv;

{$i fpcdefs.inc}

interface

    uses
       node,ncnv,defutil,defcmp;

    type
       tcgtypeconvnode = class(ttypeconvnode)
         procedure second_int_to_int;override;
         procedure second_cstring_to_pchar;override;
         procedure second_cstring_to_int;override;
         procedure second_string_to_chararray;override;
         procedure second_array_to_pointer;override;
         procedure second_pointer_to_array;override;
         procedure second_char_to_string;override;
         procedure second_real_to_real;override;
         procedure second_cord_to_pointer;override;
         procedure second_proc_to_procvar;override;
         procedure second_nil_to_methodprocvar;override;
         procedure second_bool_to_int;override;
         procedure second_bool_to_bool;override;
         procedure second_ansistring_to_pchar;override;
         procedure second_class_to_intf;override;
         procedure second_char_to_char;override;
         procedure second_nothing;override;
         procedure pass_generate_code;override;
       end;

       tcgasnode = class(tasnode)
         procedure pass_generate_code;override;
       end;

  implementation

    uses
      cutils,verbose,globtype,globals,
      aasmbase,aasmtai,aasmdata,aasmcpu,symconst,symdef,paramgr,
      ncon,ncal,
      cpubase,systems,
      procinfo,pass_2,
      cgbase,
      cgutils,cgobj,
      ncgutil,
      tgobj
      ;


    procedure tcgtypeconvnode.second_int_to_int;
      var
        orgsize,
        newsize : tcgsize;
        ressize,
        leftsize : longint;
      begin
        newsize:=def_cgsize(resultdef);

        { insert range check if not explicit conversion }
        if not(nf_explicit in flags) then
          cg.g_rangecheck(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef);

        { is the result size smaller? when typecasting from void
          we always reuse the current location, because there is
          nothing that we can load in a register }
        ressize := resultdef.size;
        leftsize := left.resultdef.size;
        if (ressize<>leftsize) and
           not is_void(left.resultdef) then
          begin
            location_copy(location,left.location);
            { reuse a loc_reference when the newsize is smaller than
              than the original, else load it to a register }
            if (location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) and
               (ressize<leftsize) then
              begin
                location.size:=newsize;
                if (target_info.endian = ENDIAN_BIG) then
                  inc(location.reference.offset,leftsize-ressize);
              end
            else
              location_force_reg(current_asmdata.CurrAsmList,location,newsize,false);
{$ifndef cpu64bit}
            // if is_signed(left.resultdef) and
{$endif cpu64bit}
          end
        else
          begin
            { no special loading is required, reuse current location }

            { that's not true, if you go from signed to unsiged or   }
            { vice versa, you need sign extension/removal if the     }
            { value is already in a register (at least for archs     }
            { which don't have 8bit register components etc) (JM)    }
            location_copy(location,left.location);
            location.size:=newsize;
            orgsize := def_cgsize(left.resultdef);
            if (ressize < tcgsize2size[OS_INT]) and
               (location.loc in [LOC_REGISTER,LOC_CREGISTER]) and
               (orgsize <> newsize) then
              begin
                location.register := cg.getintregister(current_asmdata.CurrAsmList,newsize);
                location.loc := LOC_REGISTER;
                cg.a_load_reg_reg(current_asmdata.CurrAsmList,orgsize,newsize,left.location.register,location.register);
              end;
          end;
      end;


    procedure tcgtypeconvnode.second_cstring_to_pchar;

      var
        hr : treference;

      begin
         if left.nodetype<>stringconstn then
           internalerror(200601131);
         location_reset(location,LOC_REGISTER,OS_ADDR);
         case tstringconstnode(left).cst_type of
           cst_conststring :
             begin
               location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
               cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,left.location.reference,location.register);
             end;
           cst_shortstring :
             begin
               inc(left.location.reference.offset);
               location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
               cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,left.location.reference,location.register);
             end;
           cst_ansistring :
             begin
               if tstringconstnode(left).len=0 then
                begin
                  reference_reset(hr);
                  hr.symbol:=current_asmdata.RefAsmSymbol('FPC_EMPTYCHAR');
                  location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
                  cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,hr,location.register);
                end
               else
                begin
                  location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
                  cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,left.location.reference,location.register);
                end;
             end;
           cst_longstring:
             begin
               {!!!!!!!}
               internalerror(8888);
             end;
           cst_widestring:
             begin
               if tstringconstnode(left).len=0 then
                begin
                  reference_reset(hr);
                  hr.symbol:=current_asmdata.RefAsmSymbol('FPC_EMPTYCHAR');
                  location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
                  cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,hr,location.register);
                end
               else
                begin
                  location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                  cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_INT,left.location.reference,
                    location.register);
                end;
             end;
         end;
      end;


    procedure tcgtypeconvnode.second_cstring_to_int;
      begin
        { this can't happen because constants are already processed in
          pass 1 }
        internalerror(200510013);
      end;


    procedure tcgtypeconvnode.second_string_to_chararray;
      begin
        if is_chararray(left.resultdef) then
          begin
            location_copy(location,left.location);
            exit;
          end;
        { should be handled already in resultdef pass (JM) }
        internalerror(200108292);
      end;


    procedure tcgtypeconvnode.second_array_to_pointer;

      begin
         location_reset(location,LOC_REGISTER,OS_ADDR);
         location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
         cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,left.location.reference,location.register);
      end;


    procedure tcgtypeconvnode.second_pointer_to_array;

      begin
        location_reset(location,LOC_REFERENCE,OS_NO);
        case left.location.loc of
          LOC_CREGISTER,
          LOC_REGISTER :
            begin
            {$ifdef cpu_uses_separate_address_registers}
              if getregtype(left.location.register)<>R_ADDRESSREGISTER then
                begin
                  location.reference.base:=rg.getaddressregister(current_asmdata.CurrAsmList);
                  cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,
                          left.location.register,location.reference.base);
                end
              else
            {$endif}
                location.reference.base := left.location.register;
            end;
          LOC_REFERENCE,
          LOC_CREFERENCE :
            begin
              location.reference.base:=cg.getaddressregister(current_asmdata.CurrAsmList);
              cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,left.location.reference,
                location.reference.base);
              location_freetemp(current_asmdata.CurrAsmList,left.location);
            end;
          else
            internalerror(2002032216);
        end;
      end;


    procedure tcgtypeconvnode.second_char_to_string;
      begin
         location_reset(location,LOC_REFERENCE,OS_NO);
         case tstringdef(resultdef).stringtype of
           st_shortstring :
             begin
               tg.GetTemp(current_asmdata.CurrAsmList,256,tt_normal,location.reference);
               cg.a_load_loc_ref(current_asmdata.CurrAsmList,left.location.size,left.location,
                 location.reference);
               location_freetemp(current_asmdata.CurrAsmList,left.location);
             end;
           { the rest is removed in the resultdef pass and converted to compilerprocs }
           else
            internalerror(4179);
        end;
      end;


    procedure tcgtypeconvnode.second_real_to_real;
{$ifdef x86}
      var
        tr: treference;
{$endif x86}
      begin
         location_reset(location,expectloc,def_cgsize(resultdef));
{$ifdef x86}
         { extended types in memory which should be loaded into the sse unit
           must be converted by the fpu first, so force them to be loaded into
           the fpu }
         if (expectloc=LOC_MMREGISTER) and
            (left.location.size in [OS_F80,OS_C64]) then
           begin
             if (left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
               location_force_fpureg(current_asmdata.CurrAsmList,left.location,false);
             { round them down to the proper precision }
             tg.gettemp(current_asmdata.currasmlist,resultdef.size,tt_normal,tr);
             cg.a_loadfpu_reg_ref(current_asmdata.CurrAsmList,left.location.size,location.size,left.location.register,tr);
             location_reset(left.location,LOC_REFERENCE,location.size);
             left.location.reference:=tr;
           end;
{$endif x86}
         case left.location.loc of
            LOC_FPUREGISTER,
            LOC_CFPUREGISTER:
              begin
                case expectloc of
                  LOC_FPUREGISTER:
                    begin
                      { on sparc a move from double -> single means from two to one register. }
                      { On all other platforms it also needs rounding to avoid that           }
                      { single(double_regvar) = double_regvar is true in all cases            }
                      location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
                      cg.a_loadfpu_reg_reg(current_asmdata.CurrAsmList,left.location.size,location.size,left.location.register,location.register);
                    end;
                  LOC_MMREGISTER:
                    begin
                      location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,false);
                      location.register:=cg.getmmregister(current_asmdata.CurrAsmList,location.size);
                      cg.a_loadmm_reg_reg(current_asmdata.CurrAsmList,left.location.size,location.size,left.location.register,location.register,mms_movescalar);
                    end
                  else
                    internalerror(2003012262);
                end;
                exit
              end;
            LOC_CREFERENCE,
            LOC_REFERENCE:
              begin
                 if expectloc=LOC_MMREGISTER then
                   begin
                     location.register:=cg.getmmregister(current_asmdata.CurrAsmList,location.size);
                     cg.a_loadmm_loc_reg(current_asmdata.CurrAsmList,location.size,left.location,location.register,mms_movescalar)
                   end
                  else
                    begin
                      location_force_fpureg(current_asmdata.CurrAsmList,left.location,false);
                      location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
                      cg.a_loadfpu_reg_reg(current_asmdata.CurrAsmList,left.location.size,location.size,left.location.register,location.register);
                    end;
                 location_freetemp(current_asmdata.CurrAsmList,left.location);
              end;
            LOC_MMREGISTER,
            LOC_CMMREGISTER:
              begin
                case expectloc of
                  LOC_FPUREGISTER:
                    begin
                      location_force_fpureg(current_asmdata.CurrAsmList,left.location,false);
                      location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
                      cg.a_loadfpu_reg_reg(current_asmdata.CurrAsmList,left.location.size,location.size,left.location.register,location.register);
                    end;
                  LOC_MMREGISTER:
                    begin
                      location.register:=cg.getmmregister(current_asmdata.CurrAsmList,location.size);
                      cg.a_loadmm_reg_reg(current_asmdata.CurrAsmList,left.location.size,location.size,left.location.register,location.register,mms_movescalar);
                    end;
                  else
                    internalerror(2003012261);
                end;
              end;
            else
              internalerror(2002032215);
         end;
      end;


    procedure tcgtypeconvnode.second_cord_to_pointer;
      begin
        { this can't happen because constants are already processed in
          pass 1 }
        internalerror(47423985);
      end;


    procedure tcgtypeconvnode.second_proc_to_procvar;
      begin
        if tabstractprocdef(resultdef).is_addressonly then
          begin
            location_reset(location,LOC_REGISTER,OS_ADDR);
            location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
            cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,left.location.reference,location.register);
          end
        else
          location_copy(location,left.location);
      end;

    procedure Tcgtypeconvnode.second_nil_to_methodprocvar;
    
    var r:Treference;

    begin
      tg.gettemp(current_asmdata.currasmlist,2*sizeof(aword),tt_normal,r);
      location_reset(location,LOC_REFERENCE,OS_NO);
      location.reference:=r;
      cg.a_load_const_ref(current_asmdata.currasmlist,OS_ADDR,0,r);
      inc(r.offset,sizeof(aword));
      cg.a_load_const_ref(current_asmdata.currasmlist,OS_ADDR,0,r);
    end;

    procedure tcgtypeconvnode.second_bool_to_int;
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
         if not(nf_explicit in flags) or
            (location.loc in [LOC_FLAGS,LOC_JUMP]) or
            { change of size/signedness? Then we have to sign/ }
            { zero-extend in case of a loc_(c)register         }
            ((newsize<>left.location.size) and
             ((left.resultdef.size<>resultdef.size) or
              not(location.loc in [LOC_REFERENCE,LOC_CREFERENCE]))) then
           location_force_reg(current_asmdata.CurrAsmList,location,newsize,true)
         else
           { may differ in sign, e.g. bytebool -> byte   }
           location.size:=newsize;
         current_procinfo.CurrTrueLabel:=oldTrueLabel;
         current_procinfo.CurrFalseLabel:=oldFalseLabel;
      end;


    procedure tcgtypeconvnode.second_bool_to_bool;
      begin
        { we can reuse the conversion already available
          in bool_to_int to resize the value. But when the
          size of the new boolean is smaller we need to calculate
          the value as is done in int_to_bool. This is needed because
          the bits that define the true status can be outside the limits
          of the new size and truncating the register can result in a 0
          value }
        if (left.expectloc in [LOC_FLAGS,LOC_JUMP]) then
          begin
            secondpass(left);
            if (left.location.loc <> left.expectloc) then
              internalerror(20060409);
            location_copy(location,left.location);
          end
         else if (resultdef.size=left.resultdef.size) and
                 not(is_cbool(resultdef) xor
                     is_cbool(left.resultdef)) then
           second_bool_to_int
         else
           second_int_to_bool
      end;


    procedure tcgtypeconvnode.second_ansistring_to_pchar;
      var
         l1 : tasmlabel;
         hr : treference;
      begin
         location_reset(location,LOC_REGISTER,OS_ADDR);
         current_asmdata.getjumplabel(l1);
         case left.location.loc of
            LOC_CREGISTER,LOC_REGISTER:
              begin
               {$ifdef cpu_uses_separate_address_registers}
                 if getregtype(left.location.register)<>R_ADDRESSREGISTER then
                   begin
                     location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
                     cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,
                              left.location.register,location.register);
                   end
                 else
               {$endif}
                    location.register := left.location.register;
              end;
            LOC_CREFERENCE,LOC_REFERENCE:
              begin
                location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
                cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,left.location.reference,location.register);
              end;
            else
              internalerror(2002032214);
         end;
         cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,OS_ADDR,OC_NE,0,location.register,l1);
         reference_reset(hr);
         hr.symbol:=current_asmdata.RefAsmSymbol('FPC_EMPTYCHAR');
         cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,hr,location.register);
         cg.a_label(current_asmdata.CurrAsmList,l1);
      end;


    procedure tcgtypeconvnode.second_class_to_intf;
      var
         l1 : tasmlabel;
         hd : tobjectdef;
         ImplIntf : TImplementedInterface;
      begin
         location_reset(location,LOC_REGISTER,OS_ADDR);
         case left.location.loc of
            LOC_CREFERENCE,
            LOC_REFERENCE:
              begin
                 location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
                 cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,left.location.reference,location.register);
                 location_freetemp(current_asmdata.CurrAsmList,left.location);
              end;
            LOC_CREGISTER:
              begin
                 location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
                 cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,left.location.register,location.register);
              end;
            LOC_REGISTER:
              location.register:=left.location.register;
            else
              internalerror(121120001);
         end;
         current_asmdata.getjumplabel(l1);
         cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,OS_ADDR,OC_EQ,0,location.register,l1);
         hd:=tobjectdef(left.resultdef);
         while assigned(hd) do
           begin
             ImplIntf:=hd.find_implemented_interface(tobjectdef(resultdef));
             if assigned(ImplIntf) then
               begin
                 cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_ADD,OS_ADDR,ImplIntf.ioffset,location.register);
                 break;
               end;
             hd:=hd.childof;
           end;
         if hd=nil then
           internalerror(2002081301);
         cg.a_label(current_asmdata.CurrAsmList,l1);
      end;


    procedure tcgtypeconvnode.second_char_to_char;
      begin
        internalerror(2007081202);
      end;


    procedure tcgtypeconvnode.second_nothing;
      var
        newsize : tcgsize;
      begin
        { we reuse the old value }
        location_copy(location,left.location);

        { Floats should never be returned as LOC_CONSTANT, do the
          moving to memory before the new size is set.
          Also when converting from a float to a non-float
          or the other way round, move to memory first to prevent
          invalid LOC_FPUREGISTER locations }
        if (
            (resultdef.typ=floatdef) and
            (location.loc=LOC_CONSTANT)
           ) or
           (
            (left.resultdef.typ=floatdef) xor
            (resultdef.typ=floatdef)
           ) then
          location_force_mem(current_asmdata.CurrAsmList,location);

        { but use the new size, but we don't know the size of all arrays }
        newsize:=def_cgsize(resultdef);
        location.size:=newsize;
      end;


{$ifdef TESTOBJEXT2}
    procedure tcgtypeconvnode.checkobject;
      begin
        { no checking by default }
      end;
{$endif TESTOBJEXT2}


    procedure tcgtypeconvnode.pass_generate_code;
      begin
        { the boolean routines can be called with LOC_JUMP and
          call secondpass themselves in the helper }
        if not(convtype in [tc_bool_2_int,tc_bool_2_bool,tc_int_2_bool]) then
         begin
           secondpass(left);
           if codegenerror then
            exit;
         end;

        second_call_helper(convtype);

{$ifdef TESTOBJEXT2}
         { Check explicit conversions to objects pointers !! }
         if p^.explizit and
            (p^.resultdef.typ=pointerdef) and
            (tpointerdef(p^.resultdef).definition.typ=objectdef) and not
            (tobjectdef(tpointerdef(p^.resultdef).definition).isclass) and
            ((tobjectdef(tpointerdef(p^.resultdef).definition).options and oo_hasvmt)<>0) and
            (cs_check_range in current_settings.localswitches) then
           checkobject;
{$endif TESTOBJEXT2}
      end;


    procedure tcgasnode.pass_generate_code;
      begin
        secondpass(call);
        location_copy(location,call.location);
      end;


begin
  ctypeconvnode := tcgtypeconvnode;
  casnode := tcgasnode;
end.
