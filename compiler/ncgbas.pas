{
    $Id$
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
       cpubase,
       node,nbas;

    type
       tcgnothingnode = class(tnothingnode)
          procedure pass_2;override;
       end;

       tcgasmnode = class(tasmnode)
          procedure pass_2;override;
       end;

       tcgstatementnode = class(tstatementnode)
          procedure pass_2;override;
       end;

       tcgblocknode = class(tblocknode)
          procedure pass_2;override;
       end;

       tcgtempcreatenode = class(ttempcreatenode)
          procedure pass_2;override;
       end;

       tcgtemprefnode = class(ttemprefnode)
          procedure pass_2;override;
          { Changes the location of this temp to ref. Useful when assigning }
          { another temp to this one. The current location will be freed.   }
          { Can only be called in pass 2 (since earlier, the temp location  }
          { isn't known yet)                                                }
          procedure changelocation(const ref: treference);
       end;

       tcgtempdeletenode = class(ttempdeletenode)
          procedure pass_2;override;
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,cpuinfo,
      aasmbase,aasmtai,aasmcpu,symsym,symconst,
      defutil,
      nflw,pass_2,
      cgbase,
      cgutils,cgobj,
      procinfo,
      tgobj
      ;

{*****************************************************************************
                                 TNOTHING
*****************************************************************************}

    procedure tcgnothingnode.pass_2;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         { avoid an abstract rte }
      end;


{*****************************************************************************
                               TSTATEMENTNODE
*****************************************************************************}

    procedure tcgstatementnode.pass_2;
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

    procedure tcgasmnode.pass_2;

      procedure ReLabel(var p:tasmsymbol);
        begin
          { Only relabel local tasmlabels }
          if (p.defbind = AB_LOCAL) and
             (p is tasmlabel) then
           begin
             if not assigned(p.altsymbol) then
               objectlibrary.GenerateAltSymbol(p);
             p:=p.altsymbol;
             p.increfs;
           end;
        end;

      procedure ResolveRef(var op:toper);
        var
          sym : tvarsym;
{$ifdef x86}
          scale : byte;
{$endif x86}
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
              sym:=tvarsym(pointer(op.localoper^.localsym));
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
                    if sofs<>0 then
                      begin
                        op.typ:=top_ref;
                        new(op.ref);
                        reference_reset_base(op.ref^,sym.localloc.register,sofs);
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
        skipnode : boolean;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         if (nf_get_asm_position in flags) then
           begin
             { Add a marker, to be sure the list is not empty }
             exprasmlist.concat(tai_marker.create(marker_position));
             currenttai:=tai(exprasmlist.last);
             exit;
           end;

         { Allocate registers used in the assembler block }
         cg.alloccpuregisters(exprasmlist,R_INTREGISTER,used_regs_int);

         if (current_procinfo.procdef.proccalloption=pocall_inline) then
           begin
             objectlibrary.CreateUsedAsmSymbolList;
             hp:=tai(p_asm.first);
             while assigned(hp) do
              begin
                hp2:=tai(hp.getcopy);
                skipnode:=false;
                case hp2.typ of
                  ait_label :
                     ReLabel(tasmsymbol(tai_label(hp2).l));
                  ait_const_64bit,
                  ait_const_32bit,
                  ait_const_16bit,
                  ait_const_8bit,
                  ait_const_rva_symbol,
                  ait_const_indirect_symbol :
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
                     end;
                   ait_marker :
                     begin
                     { it's not an assembler block anymore }
                       if (tai_marker(hp2).kind in [AsmBlockStart, AsmBlockEnd]) then
                        skipnode:=true;
                     end;
                end;
                if not skipnode then
                  exprasmList.concat(hp2)
                else
                  hp2.free;
                hp:=tai(hp.next);
              end;
             { restore used symbols }
             objectlibrary.UsedAsmSymbolListResetAltSym;
             objectlibrary.DestroyUsedAsmSymbolList;
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
                     end;
                end;
                hp:=tai(hp.next);
              end;
             { insert the list }
             exprasmList.concatlist(p_asm);
           end;

         { Release register used in the assembler block }
         cg.dealloccpuregisters(exprasmlist,R_INTREGISTER,used_regs_int);
       end;


{*****************************************************************************
                             TBLOCKNODE
*****************************************************************************}

    procedure tcgblocknode.pass_2;
      var
        hp : tstatementnode;
      begin
        location_reset(location,LOC_VOID,OS_NO);

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
      end;


{*****************************************************************************
                          TTEMPCREATENODE
*****************************************************************************}

    procedure tcgtempcreatenode.pass_2;
      var
        cgsize: tcgsize;
      begin
        location_reset(location,LOC_VOID,OS_NO);

        { if we're secondpassing the same tcgtempcreatenode twice, we have a bug }
        if tempinfo^.valid then
          internalerror(200108222);

        { get a (persistent) temp }
        if tempinfo^.restype.def.needs_inittable then
          begin
            tg.GetTempTyped(exprasmlist,tempinfo^.restype.def,tempinfo^.temptype,tempinfo^.loc.ref);
            tempinfo^.loc.loc := LOC_REFERENCE;
          end
        else if tempinfo^.may_be_in_reg then
          begin
            cgsize := def_cgsize(tempinfo^.restype.def);
            if tempinfo^.restype.def.deftype <> floatdef then
              begin
                if (TCGSize2Size[cgsize]>TCGSize2Size[OS_INT]) then
                  internalerror(2004020202);
                tempinfo^.loc.reg := cg.getintregister(exprasmlist,cgsize);
                if (tempinfo^.temptype = tt_persistent) then
                  begin
                    { !!tell rgobj this register is now a regvar, so it can't be freed!! }
                    tempinfo^.loc.loc := LOC_CREGISTER
                  end
                else
                  tempinfo^.loc.loc := LOC_REGISTER;
              end
            else
              begin
                tempinfo^.loc.reg := cg.getfpuregister(exprasmlist,cgsize);
                if (tempinfo^.temptype = tt_persistent) then
                  tempinfo^.loc.loc := LOC_CFPUREGISTER
                else
                  tempinfo^.loc.loc := LOC_FPUREGISTER;
              end;
          end
        else
          begin
            tg.GetTemp(exprasmlist,size,tempinfo^.temptype,tempinfo^.loc.ref);
            tempinfo^.loc.loc := LOC_REFERENCE;
          end;
        tempinfo^.valid := true;
      end;


{*****************************************************************************
                             TTEMPREFNODE
*****************************************************************************}

    procedure tcgtemprefnode.pass_2;
      begin
        { check if the temp is valid }
        if not tempinfo^.valid then
          internalerror(200108231);
        case tempinfo^.loc.loc of
          LOC_REFERENCE:
            begin
              { set the temp's location }
              location_reset(location,LOC_REFERENCE,def_cgsize(tempinfo^.restype.def));
              location.reference := tempinfo^.loc.ref;
              inc(location.reference.offset,offset);
            end;
          LOC_REGISTER,
          LOC_CREGISTER,
          LOC_FPUREGISTER,
          LOC_CFPUREGISTER:
            begin
              if offset <> 0 then
                internalerror(2004020205);
              { LOC_CREGISTER, not LOC_REGISTER, otherwise we can't assign anything to it }
              location_reset(location,tempinfo^.loc.loc,def_cgsize(tempinfo^.restype.def));
              location.register := tempinfo^.loc.reg;
            end;
          else
            internalerror(2004020204);
        end;
      end;


    procedure tcgtemprefnode.changelocation(const ref: treference);
      begin
        { check if the temp is valid }
        if not tempinfo^.valid then
          internalerror(200306081);
        if (tempinfo^.loc.loc = LOC_REGISTER) then
          internalerror(2004020203);
        if (tempinfo^.temptype = tt_persistent) then
          tg.ChangeTempType(exprasmlist,tempinfo^.loc.ref,tt_normal);
        tg.ungettemp(exprasmlist,tempinfo^.loc.ref);
        tempinfo^.loc.ref := ref;
        tg.ChangeTempType(exprasmlist,tempinfo^.loc.ref,tempinfo^.temptype);
        { adapt location }
        location.reference := ref;
        inc(location.reference.offset,offset);
      end;


{*****************************************************************************
                           TTEMPDELETENODE
*****************************************************************************}

    procedure tcgtempdeletenode.pass_2;
      begin
        location_reset(location,LOC_VOID,OS_NO);

        case tempinfo^.loc.loc of
          LOC_REFERENCE:
            begin
              if release_to_normal then
                tg.ChangeTempType(exprasmlist,tempinfo^.loc.ref,tt_normal)
              else
                tg.UnGetTemp(exprasmlist,tempinfo^.loc.ref);
            end;
          LOC_CREGISTER,
          LOC_REGISTER:
            begin
              { make sure the register allocator doesn't reuse the }
              { register e.g. in the middle of a loop              }
              cg.a_reg_sync(exprasmlist,tempinfo^.loc.reg);
              if release_to_normal then
                tempinfo^.loc.loc := LOC_REGISTER;
            end;
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
{
  $Log$
  Revision 1.68  2004-09-26 17:45:30  peter
    * simple regvar support, not yet finished

  Revision 1.67  2004/09/25 14:23:54  peter
    * ungetregister is now only used for cpuregisters, renamed to
      ungetcpuregister
    * renamed (get|unget)explicitregister(s) to ..cpuregister
    * removed location-release/reference_release

  Revision 1.66  2004/09/21 17:25:12  peter
    * paraloc branch merged

  Revision 1.65.4.1  2004/08/31 20:43:06  peter
    * paraloc patch

  Revision 1.65  2004/07/16 19:45:15  jonas
    + temps can now also hold fpu values in registers (take care with use,
      bacause of the x86 fpu stack)
    * fpu parameters to node-inlined procedures can now also be put in
      a register

  Revision 1.64  2004/06/20 08:55:29  florian
    * logs truncated

  Revision 1.63  2004/06/16 20:07:08  florian
    * dwarf branch merged

  Revision 1.62  2004/05/23 18:28:41  peter
    * methodpointer is loaded into a temp when it was a calln

  Revision 1.61  2004/05/23 15:06:20  peter
    * implicit_finally flag must be set in pass1
    * add check whether the implicit frame is generated when expected

  Revision 1.60.2.3  2004/04/27 18:18:25  peter
    * aword -> aint

  Revision 1.60.2.2  2004/04/12 19:34:45  peter
    * basic framework for dwarf CFI

}
