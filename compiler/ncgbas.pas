{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

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

{$i defines.inc}

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
      aasm,symsym,
      cpubase,cpuasm,
      nflw,pass_2,
      cga,
      cgbase,tgobj,rgobj
      ;
{*****************************************************************************
                                 TNOTHING
*****************************************************************************}

    procedure tcgnothingnode.pass_2;
      begin
         { avoid an abstract rte }
      end;


{*****************************************************************************
                               TSTATEMENTNODE
*****************************************************************************}

    procedure tcgstatementnode.pass_2;
      var
         hp : tnode;
      begin
         hp:=self;
         while assigned(hp) do
          begin
            if assigned(tstatementnode(hp).right) then
             begin
               rg.cleartempgen;
               secondpass(tstatementnode(hp).right);
               { Compiler inserted blocks can return values }
               location_copy(location,tstatementnode(hp).right.location);
             end;
            hp:=tstatementnode(hp).left;
          end;
      end;


{*****************************************************************************
                               TASMNODE
*****************************************************************************}

    procedure tcgasmnode.pass_2;

      procedure ReLabel(var p:tasmsymbol);
        begin
          if p.proclocal then
           begin
             if not assigned(p.altsymbol) then
              begin
                { generatealtsymbol will also increase the refs }
                p.GenerateAltSymbol;
                UsedAsmSymbolListInsert(p);
              end
             else
              begin
                { increase the refs, they will be decreased when the
                  asmnode is destroyed }
                inc(p.refs);
              end;
             p:=p.altsymbol;
           end;
        end;

      var
        hp,hp2 : tai;
        localfixup,parafixup,
        i : longint;
        skipnode : boolean;
      begin
         if inlining_procedure then
           begin
             CreateUsedAsmSymbolList;
             localfixup:=aktprocdef.localst.address_fixup;
             parafixup:=aktprocdef.parast.address_fixup;
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
                       taicpu(hp2).ResetPass1;
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
             UsedAsmSymbolListResetAltSym;
             DestroyUsedAsmSymbolList;
           end
         else
           begin
             { if the routine is an inline routine, then we must hold a copy
               because it can be necessary for inlining later }
             if (aktprocdef.proccalloption=pocall_inline) then
               exprasmList.concatlistcopy(p_asm)
             else
               exprasmList.concatlist(p_asm);
           end;
         if not (nf_object_preserved in flags) then
           maybe_loadself;
       end;


{*****************************************************************************
                             TBLOCKNODE
*****************************************************************************}

    procedure tcgblocknode.pass_2;
      begin
        { do second pass on left node }
        if assigned(left) then
         begin
           secondpass(left);
           { Compiler inserted blocks can return values }
           location_copy(location,left.location);
         end;
      end;

{*****************************************************************************
                          TTEMPCREATENODE
*****************************************************************************}

    procedure tcgtempcreatenode.pass_2;
      begin
        { if we're secondpassing the same tcgtempcreatenode twice, we have a bug }
        if tempinfo^.valid then
          internalerror(200108222);

        { get a (persistent) temp }
        if persistent then
          tg.gettempofsizereferencepersistant(exprasmlist,size,tempinfo^.ref)
        else
          tg.gettempofsizereference(exprasmlist,size,tempinfo^.ref);
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
        if release_to_normal then
          tg.persistanttemptonormal(tempinfo^.ref.offset)
        else
          tg.ungetpersistanttempreference(exprasmlist,tempinfo^.ref);
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
  Revision 1.14  2002-04-23 19:16:34  peter
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

  Revision 1.10  2001/12/31 16:54:14  peter
    * fixed inline crash with assembler routines

  Revision 1.9  2001/11/02 22:58:01  peter
    * procsym definition rewrite

  Revision 1.8  2001/10/25 21:22:35  peter
    * calling convention rewrite

  Revision 1.7  2001/08/26 13:36:39  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.6  2001/08/24 13:47:27  jonas
    * moved "reverseparameters" from ninl.pas to ncal.pas
    + support for non-persistent temps in ttempcreatenode.create, for use
      with typeconversion nodes

  Revision 1.5  2001/08/23 14:28:35  jonas
    + tempcreate/ref/delete nodes (allows the use of temps in the
      resulttype and first pass)
    * made handling of read(ln)/write(ln) processor independent
    * moved processor independent handling for str and reset/rewrite-typed
      from firstpass to resulttype pass
    * changed names of helpers in text.inc to be generic for use as
      compilerprocs + added "iocheck" directive for most of them
    * reading of ordinals is done by procedures instead of functions
      because otherwise FPC_IOCHECK overwrote the result before it could
      be stored elsewhere (range checking still works)
    * compilerprocs can now be used in the system unit before they are
      implemented
    * added note to errore.msg that booleans can't be read using read/readln

  Revision 1.4  2001/06/02 19:22:15  peter
    * refs count for relabeled asmsymbols fixed

  Revision 1.3  2001/05/18 22:31:06  peter
    * tasmnode.pass_2 is independent of cpu, moved to ncgbas
    * include ncgbas for independent nodes

  Revision 1.2  2001/04/13 01:22:08  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.1  2000/10/14 10:14:50  peter
    * moehrendorf oct 2000 rewrite

}
