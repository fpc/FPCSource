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
       end;

       tcgtempdeletenode = class(ttempdeletenode)
          procedure pass_2;override;
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      aasmbase,aasmtai,aasmcpu,symsym,
      cpubase,
      nflw,pass_2,
      cgbase,cginfo,cgobj,tgobj,rgobj
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

      var
        hp,hp2 : tai;
        localfixup,parafixup,
        i : longint;
        skipnode : boolean;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         if getposition then
           begin
             currenttai:=tai(exprasmlist.last);
             exit;
           end;

         { Allocate registers used in the assembler block }
         rg.allocexplicitregistersint(exprasmlist,used_regs_int);

         if inlining_procedure then
           begin
             objectlibrary.CreateUsedAsmSymbolList;
             localfixup:=current_procinfo.procdef.localst.address_fixup;
             parafixup:=current_procinfo.procdef.parast.address_fixup;
             hp:=tai(p_asm.first);
             while assigned(hp) do
              begin
                hp2:=tai(hp.getcopy);
                skipnode:=false;
                case hp2.typ of
                  ait_label :
                     begin
                       { regenerate the labels by setting altsymbol }
                       ReLabel(tasmsymbol(tai_label(hp2).l));
                     end;
                  ait_const_rva,
                  ait_const_symbol :
                     begin
                       ReLabel(tai_const_symbol(hp2).sym);
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
                          with taicpu(hp2).oper[i-1] do
                           begin
                             case typ of
                               top_ref :
                                 begin
                                   case ref^.options of
                                     ref_parafixup :
                                       ref^.offsetfixup:=parafixup;
                                     ref_localfixup :
                                       ref^.offsetfixup:=localfixup;
                                   end;
                                   if assigned(ref^.symbol) then
                                    ReLabel(ref^.symbol);
                                 end;
                               top_symbol :
                                 begin
                                   ReLabel(sym);
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
                   else
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
             { if the routine is an inline routine, then we must hold a copy
               because it can be necessary for inlining later }
             if (current_procinfo.procdef.proccalloption=pocall_inline) then
               exprasmList.concatlistcopy(p_asm)
             else
               exprasmList.concatlist(p_asm);
           end;

         { Release register used in the assembler block }
         rg.deallocexplicitregistersint(exprasmlist,used_regs_int);
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
      begin
        location_reset(location,LOC_VOID,OS_NO);

        { if we're secondpassing the same tcgtempcreatenode twice, we have a bug }
        if tempinfo^.valid then
          internalerror(200108222);

        { get a (persistent) temp }
        tg.GetTemp(exprasmlist,size,tempinfo^.temptype,tempinfo^.ref);
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
        { set the temp's location }
        location_reset(location,LOC_REFERENCE,def_cgsize(tempinfo^.restype.def));
        location.reference := tempinfo^.ref;
        inc(location.reference.offset,offset);
      end;

{*****************************************************************************
                           TTEMPDELETENODE
*****************************************************************************}

    procedure tcgtempdeletenode.pass_2;
      begin
        location_reset(location,LOC_VOID,OS_NO);

        if release_to_normal then
          tg.ChangeTempType(exprasmlist,tempinfo^.ref,tt_normal)
        else
          tg.UnGetTemp(exprasmlist,tempinfo^.ref);
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
  Revision 1.39  2003-09-07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.38  2003/09/03 15:55:00  peter
    * NEWRA branch merged

  Revision 1.37.2.1  2003/08/27 20:23:55  peter
    * remove old ra code

  Revision 1.37  2003/06/13 21:19:30  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.36  2003/06/09 18:26:46  peter
    * remove temptype, use tempinfo.temptype instead

  Revision 1.35  2003/06/09 12:20:47  peter
    * getposition added to retrieve the the current tai item

  Revision 1.34  2003/05/17 13:30:08  jonas
    * changed tt_persistant to tt_persistent :)
    * tempcreatenode now doesn't accept a boolean anymore for persistent
      temps, but a ttemptype, so you can also create ansistring temps etc

  Revision 1.33  2003/04/27 11:21:33  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.32  2002/04/25 20:15:39  florian
    * block nodes within expressions shouldn't release the used registers,
      fixed using a flag till the new rg is ready

  Revision 1.31  2003/04/22 23:50:22  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.30  2003/04/17 07:50:24  daniel
    * Some work on interference graph construction

  Revision 1.29  2003/03/28 19:16:56  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.28  2002/11/27 15:33:19  peter
    * fixed relabeling to relabel only tasmlabel (formerly proclocal)

  Revision 1.27  2002/11/27 02:37:13  peter
    * case statement inlining added
    * fixed inlining of write()
    * switched statementnode left and right parts so the statements are
      processed in the correct order when getcopy is used. This is
      required for tempnodes

  Revision 1.26  2002/11/17 16:31:56  carl
    * memory optimization (3-4%) : cleanup of tai fields,
       cleanup of tdef and tsym fields.
    * make it work for m68k

  Revision 1.25  2002/11/15 16:29:30  peter
    * made tasmsymbol.refs private (merged)

  Revision 1.24  2002/11/15 01:58:51  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.23  2002/08/23 16:14:48  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.22  2002/08/11 14:32:26  peter
    * renamed current_library to objectlibrary

  Revision 1.21  2002/08/11 13:24:11  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.20  2002/07/01 18:46:22  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.19  2002/05/18 13:34:09  peter
    * readded missing revisions

  Revision 1.18  2002/05/16 19:46:37  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.16  2002/05/13 19:54:37  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.15  2002/05/12 16:53:07  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.14  2002/04/23 19:16:34  peter
    * add pinline unit that inserts compiler supported functions using
      one or more statements
    * moved finalize and setlength from ninl to pinline

  Revision 1.13  2002/04/21 19:02:03  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.12  2002/04/04 19:05:57  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.11  2002/03/31 20:26:34  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

}
