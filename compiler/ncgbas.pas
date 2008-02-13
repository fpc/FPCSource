{
    Copyright (c) 2000-2002 by Florian Klaempfl

    This unit implements some basic nodes

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
unit ncgbas;

{$i fpcdefs.inc}

interface

    uses
       cpubase,cgutils,
       node,nbas;

    type
       tcgnothingnode = class(tnothingnode)
          procedure pass_generate_code;override;
       end;

       tcgasmnode = class(tasmnode)
          procedure pass_generate_code;override;
       end;

       tcgstatementnode = class(tstatementnode)
          procedure pass_generate_code;override;
       end;

       tcgblocknode = class(tblocknode)
          procedure pass_generate_code;override;
       end;

       tcgtempcreatenode = class(ttempcreatenode)
          procedure pass_generate_code;override;
       end;

       tcgtemprefnode = class(ttemprefnode)
          procedure pass_generate_code;override;
          { Changes the location of this temp to ref. Useful when assigning }
          { another temp to this one. The current location will be freed.   }
          { Can only be called in pass 2 (since earlier, the temp location  }
          { isn't known yet)                                                }
          procedure changelocation(const ref: treference);
       end;

       tcgtempdeletenode = class(ttempdeletenode)
          procedure pass_generate_code;override;
       end;

  implementation

    uses
      globtype,globals,systems,
      cutils,verbose,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      symsym,symconst,symdef,defutil,
      nflw,pass_2,
      cgbase,cgobj,
      procinfo,
      tgobj
      ;

{*****************************************************************************
                                 TNOTHING
*****************************************************************************}

    procedure tcgnothingnode.pass_generate_code;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         { avoid an abstract rte }
      end;


{*****************************************************************************
                               TSTATEMENTNODE
*****************************************************************************}

    procedure tcgstatementnode.pass_generate_code;
      var
         hp : tstatementnode;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         hp:=self;
         while assigned(hp) do
          begin
            if assigned(hp.left) then
             begin
               secondpass(hp.left);
               { Compiler inserted blocks can return values }
               location_copy(hp.location,hp.left.location);
             end;
            hp:=tstatementnode(hp.right);
          end;
      end;


{*****************************************************************************
                               TASMNODE
*****************************************************************************}

    procedure tcgasmnode.pass_generate_code;

      procedure ReLabel(var p:tasmsymbol);
        begin
          { Only relabel local tasmlabels }
          if (p.bind = AB_LOCAL) and
             (p is tasmlabel) then
           begin
             if not assigned(p.altsymbol) then
               current_asmdata.GenerateAltSymbol(p);
             p:=p.altsymbol;
             p.increfs;
           end;
        end;

      procedure ResolveRef(var op:toper);
        var
          sym : tabstractnormalvarsym;
{$ifdef x86}
          scale : byte;
{$endif x86}
          forceref,
          getoffset : boolean;
          indexreg : tregister;
          sofs : longint;
        begin
          if (op.typ=top_local) then
            begin
              sofs:=op.localoper^.localsymofs;
              indexreg:=op.localoper^.localindexreg;
{$ifdef x86}
              scale:=op.localoper^.localscale;
{$endif x86}
              getoffset:=op.localoper^.localgetoffset;
              forceref:=op.localoper^.localforceref;
              sym:=tabstractnormalvarsym(pointer(op.localoper^.localsym));
              dispose(op.localoper);
              case sym.localloc.loc of
                LOC_REFERENCE :
                  begin
                    if getoffset then
                      begin
                        if indexreg=NR_NO then
                          begin
                            op.typ:=top_const;
                            op.val:=sym.localloc.reference.offset+sofs;
                          end
                        else
                          begin
                            op.typ:=top_ref;
                            new(op.ref);
                            reference_reset_base(op.ref^,indexreg,sym.localloc.reference.offset+sofs);
                          end;
                      end
                    else
                      begin
                        op.typ:=top_ref;
                        new(op.ref);
                        reference_reset_base(op.ref^,sym.localloc.reference.base,sym.localloc.reference.offset+sofs);
                        op.ref^.index:=indexreg;
{$ifdef x86}
                        op.ref^.scalefactor:=scale;
{$endif x86}
                      end;
                  end;
                LOC_REGISTER :
                  begin
                    if getoffset then
                      Message(asmr_e_invalid_reference_syntax);
                    { Subscribed access }
                    if forceref or
                       (sofs<>0) then
                      begin
                        op.typ:=top_ref;
                        new(op.ref);
                        reference_reset_base(op.ref^,sym.localloc.register,sofs);
                        op.ref^.index:=indexreg;
{$ifdef x86}
                        op.ref^.scalefactor:=scale;
{$endif x86}
                      end
                    else
                      begin
                        op.typ:=top_reg;
                        op.reg:=sym.localloc.register;
                      end;
                  end;
              end;
            end;
        end;

      var
        hp,hp2 : tai;
        i : longint;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         if (nf_get_asm_position in flags) then
           begin
             { Add a marker, to be sure the list is not empty }
             current_asmdata.CurrAsmList.concat(tai_marker.create(mark_Position));
             currenttai:=tai(current_asmdata.CurrAsmList.last);
             exit;
           end;

         { Allocate registers used in the assembler block }
         cg.alloccpuregisters(current_asmdata.CurrAsmList,R_INTREGISTER,used_regs_int);

         if (po_inline in current_procinfo.procdef.procoptions) then
           begin
             hp:=tai(p_asm.first);
             while assigned(hp) do
              begin
                hp2:=tai(hp.getcopy);
                case hp2.typ of
                  ait_label :
                     ReLabel(tasmsymbol(tai_label(hp2).labsym));
                  ait_const :
                     begin
                       if assigned(tai_const(hp2).sym) then
                         ReLabel(tai_const(hp2).sym);
                       if assigned(tai_const(hp2).endsym) then
                         ReLabel(tai_const(hp2).endsym);
                     end;
                  ait_instruction :
                     begin
                       { remove cached insentry, because the new code can
                         require an other less optimized instruction }
{$ifdef i386}
{$ifndef NOAG386BIN}
                       taicpu(hp2).ResetPass1;
{$endif}
{$endif}
                       { fixup the references }
                       for i:=1 to taicpu(hp2).ops do
                        begin
                          ResolveRef(taicpu(hp2).oper[i-1]^);
                          with taicpu(hp2).oper[i-1]^ do
                           begin
                             case typ of
                               top_ref :
                                 begin
                                   if assigned(ref^.symbol) then
                                     ReLabel(ref^.symbol);
                                   if assigned(ref^.relsymbol) then
                                     ReLabel(ref^.relsymbol);
                                 end;
                             end;
                           end;
                        end;
{$ifdef x86}
                        { can only be checked now that all local operands }
                        { have been resolved                              }
                        taicpu(hp2).CheckIfValid;
{$endif x86}
                     end;
                end;
                current_asmdata.CurrAsmList.concat(hp2);
                hp:=tai(hp.next);
              end;
             { restore used symbols }
             current_asmdata.ResetAltSymbols;
           end
         else
           begin
             hp:=tai(p_asm.first);
             while assigned(hp) do
              begin
                case hp.typ of
                  ait_instruction :
                     begin
                       { remove cached insentry, because the new code can
                         require an other less optimized instruction }
{$ifdef i386}
{$ifndef NOAG386BIN}
                       taicpu(hp).ResetPass1;
{$endif}
{$endif}
                       { fixup the references }
                       for i:=1 to taicpu(hp).ops do
                         ResolveRef(taicpu(hp).oper[i-1]^);
{$ifdef x86}
                      { can only be checked now that all local operands }
                      { have been resolved                              }
                      taicpu(hp).CheckIfValid;
{$endif x86}
                     end;
                end;
                hp:=tai(hp.next);
              end;
             { insert the list }
             current_asmdata.CurrAsmList.concatlist(p_asm);
           end;

         { Release register used in the assembler block }
         cg.dealloccpuregisters(current_asmdata.CurrAsmList,R_INTREGISTER,used_regs_int);
       end;


{*****************************************************************************
                             TBLOCKNODE
*****************************************************************************}

    procedure tcgblocknode.pass_generate_code;
      var
        hp : tstatementnode;
        oldexitlabel : tasmlabel;
        oldflowcontrol : tflowcontrol;
      begin
        location_reset(location,LOC_VOID,OS_NO);

        { replace exitlabel? }
        if nf_block_with_exit in flags then
          begin
            oldexitlabel:=current_procinfo.CurrExitLabel;
            current_asmdata.getjumplabel(current_procinfo.CurrExitLabel);
            oldflowcontrol:=flowcontrol;
            { the nested block will not span an exit statement of the parent }
            exclude(flowcontrol,fc_exit);
          end;

        { do second pass on left node }
        if assigned(left) then
         begin
           hp:=tstatementnode(left);
           while assigned(hp) do
            begin
              if assigned(hp.left) then
               begin
                 secondpass(hp.left);
                 location_copy(hp.location,hp.left.location);
               end;
              location_copy(location,hp.location);
              hp:=tstatementnode(hp.right);
            end;
         end;

        { write exitlabel }
        if nf_block_with_exit in flags then
          begin
            cg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrExitLabel);
            current_procinfo.CurrExitLabel:=oldexitlabel;
            { the exit statements inside this block are not exit statements }
            { out of the parent                                             }
            flowcontrol:=oldflowcontrol+(flowcontrol - [fc_exit]);
          end;
      end;


{*****************************************************************************
                          TTEMPCREATENODE
*****************************************************************************}

    procedure tcgtempcreatenode.pass_generate_code;
      begin
        location_reset(location,LOC_VOID,OS_NO);

        { if we're secondpassing the same tcgtempcreatenode twice, we have a bug }
        if (ti_valid in tempinfo^.flags) then
          internalerror(200108222);

        { get a (persistent) temp }
        if tempinfo^.typedef.needs_inittable then
          begin
            location_reset(tempinfo^.location,LOC_REFERENCE,def_cgsize(tempinfo^.typedef));
            tg.GetTempTyped(current_asmdata.CurrAsmList,tempinfo^.typedef,tempinfo^.temptype,tempinfo^.location.reference);
            { the temp could have been used previously either because the memory location was reused or
              because we're in a loop }
            cg.g_finalize(current_asmdata.CurrAsmList,tempinfo^.typedef,tempinfo^.location.reference);
          end
        else if (ti_may_be_in_reg in tempinfo^.flags) then
          begin
            if tempinfo^.typedef.typ=floatdef then
              begin
{$ifdef x86}
                if use_sse(tempinfo^.typedef) then
                  begin
                    if (tempinfo^.temptype = tt_persistent) then
                      location_reset(tempinfo^.location,LOC_CMMREGISTER,def_cgsize(tempinfo^.typedef))
                    else
                      location_reset(tempinfo^.location,LOC_MMREGISTER,def_cgsize(tempinfo^.typedef));
                    tempinfo^.location.register:=cg.getmmregister(current_asmdata.CurrAsmList,tempinfo^.location.size);
                  end
                else
{$endif x86}
                  begin
                    if (tempinfo^.temptype = tt_persistent) then
                      location_reset(tempinfo^.location,LOC_CFPUREGISTER,def_cgsize(tempinfo^.typedef))
                    else
                      location_reset(tempinfo^.location,LOC_FPUREGISTER,def_cgsize(tempinfo^.typedef));
                    tempinfo^.location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,tempinfo^.location.size);
                  end;
              end
            else
              begin
                if (tempinfo^.temptype = tt_persistent) then
                  location_reset(tempinfo^.location,LOC_CREGISTER,def_cgsize(tempinfo^.typedef))
                else
                  location_reset(tempinfo^.location,LOC_REGISTER,def_cgsize(tempinfo^.typedef));
{$ifndef cpu64bitalu}
                if tempinfo^.location.size in [OS_64,OS_S64] then
                  begin
                    tempinfo^.location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                    tempinfo^.location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                  end
                else
{$endif not cpu64bitalu}
                  tempinfo^.location.register:=cg.getintregister(current_asmdata.CurrAsmList,tempinfo^.location.size);
              end;
          end
        else
          begin
            location_reset(tempinfo^.location,LOC_REFERENCE,def_cgsize(tempinfo^.typedef));
            tg.GetTemp(current_asmdata.CurrAsmList,size,tempinfo^.temptype,tempinfo^.location.reference);
          end;
        include(tempinfo^.flags,ti_valid);
      end;


{*****************************************************************************
                             TTEMPREFNODE
*****************************************************************************}

    procedure tcgtemprefnode.pass_generate_code;
      begin
        { check if the temp is valid }
        if not(ti_valid in tempinfo^.flags) then
          internalerror(200108231);
        location:=tempinfo^.location;
        case tempinfo^.location.loc of
          LOC_REFERENCE:
            begin
              inc(location.reference.offset,offset);
              { ti_valid should be excluded if it's a normal temp }
            end;
          LOC_REGISTER,
          LOC_FPUREGISTER,
          LOC_MMREGISTER :
            exclude(tempinfo^.flags,ti_valid);
        end;
      end;


    procedure tcgtemprefnode.changelocation(const ref: treference);
      begin
        { check if the temp is valid }
        if not(ti_valid in tempinfo^.flags) then
          internalerror(200306081);
        if (tempinfo^.location.loc<>LOC_REFERENCE) then
          internalerror(2004020203);
        if (tempinfo^.temptype = tt_persistent) then
          tg.ChangeTempType(current_asmdata.CurrAsmList,tempinfo^.location.reference,tt_normal);
        tg.ungettemp(current_asmdata.CurrAsmList,tempinfo^.location.reference);
        tempinfo^.location.reference := ref;
        tg.ChangeTempType(current_asmdata.CurrAsmList,tempinfo^.location.reference,tempinfo^.temptype);
        { adapt location }
        location.reference := ref;
        inc(location.reference.offset,offset);
      end;


{*****************************************************************************
                           TTEMPDELETENODE
*****************************************************************************}

    procedure tcgtempdeletenode.pass_generate_code;
      begin
        location_reset(location,LOC_VOID,OS_NO);

        case tempinfo^.location.loc of
          LOC_REFERENCE:
            begin
              if release_to_normal then
                tg.ChangeTempType(current_asmdata.CurrAsmList,tempinfo^.location.reference,tt_normal)
              else
                begin
                  tg.UnGetTemp(current_asmdata.CurrAsmList,tempinfo^.location.reference);
                  exclude(tempinfo^.flags,ti_valid);
                end;
            end;
          LOC_CREGISTER,
          LOC_REGISTER:
            begin
              if not(cs_opt_regvar in current_settings.optimizerswitches) or
                 (pi_has_goto in current_procinfo.flags) then
                begin
                  { make sure the register allocator doesn't reuse the }
                  { register e.g. in the middle of a loop              }
{$ifndef cpu64bitalu}
                  if tempinfo^.location.size in [OS_64,OS_S64] then
                    begin
                      cg.a_reg_sync(current_asmdata.CurrAsmList,tempinfo^.location.register64.reghi);
                      cg.a_reg_sync(current_asmdata.CurrAsmList,tempinfo^.location.register64.reglo);
                    end
                  else
{$endif not cpu64bitalu}
                    cg.a_reg_sync(current_asmdata.CurrAsmList,tempinfo^.location.register);
                end;
              if release_to_normal then
                tempinfo^.location.loc := LOC_REGISTER
              else
                exclude(tempinfo^.flags,ti_valid);
            end;
          LOC_CFPUREGISTER,
          LOC_FPUREGISTER:
            begin
              if not(cs_opt_regvar in current_settings.optimizerswitches) or
                 (pi_has_goto in current_procinfo.flags) then
                begin
                  { make sure the register allocator doesn't reuse the }
                  { register e.g. in the middle of a loop              }
                  cg.a_reg_sync(current_asmdata.CurrAsmList,tempinfo^.location.register);
                end;
              if release_to_normal then
                tempinfo^.location.loc := LOC_FPUREGISTER
              else
                exclude(tempinfo^.flags,ti_valid);
            end;
          LOC_CMMREGISTER,
          LOC_MMREGISTER:
            begin
              if not(cs_opt_regvar in current_settings.optimizerswitches) or
                 (pi_has_goto in current_procinfo.flags) then
                begin
                  { make sure the register allocator doesn't reuse the }
                  { register e.g. in the middle of a loop              }
                  cg.a_reg_sync(current_asmdata.CurrAsmList,tempinfo^.location.register);
                end;
              if release_to_normal then
                tempinfo^.location.loc := LOC_MMREGISTER
              else
                exclude(tempinfo^.flags,ti_valid);
            end;
          else
            internalerror(200507161);
        end;
      end;


begin
   cnothingnode:=tcgnothingnode;
   casmnode:=tcgasmnode;
   cstatementnode:=tcgstatementnode;
   cblocknode:=tcgblocknode;
   ctempcreatenode:=tcgtempcreatenode;
   ctemprefnode:=tcgtemprefnode;
   ctempdeletenode:=tcgtempdeletenode;
end.
