{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit handles the codegeneration pass

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
unit n386bas;

{$i defines.inc}

  interface

    uses
       node,nbas;

    type
       ti386statementnode = class(tstatementnode)
          procedure pass_2;override;
       end;

       ti386blocknode = class(tblocknode)
          procedure pass_2;override;
       end;

       ti386asmnode = class(tasmnode)
          procedure pass_2;override;
       end;

  implementation

    uses
       globals,
       aasm,cpubase,cpuasm,
       symconst,symsym,
       pass_2,tgcpu,
       cgai386;

    procedure ti386asmnode.pass_2;

      procedure ReLabel(var p:pasmsymbol);
        begin
          if p^.proclocal then
           begin
             if not assigned(p^.altsymbol) then
              begin
                p^.GenerateAltSymbol;
                UsedAsmSymbolListInsert(p);
              end;
             p:=p^.altsymbol;
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
             localfixup:=aktprocsym^.definition^.localst^.address_fixup;
             parafixup:=aktprocsym^.definition^.parast^.address_fixup;
             hp:=tai(p_asm.first);
             while assigned(hp) do
              begin
                hp2:=tai(hp.getcopy);
                skipnode:=false;
                case hp2.typ of
                  ait_label :
                     begin
                       { regenerate the labels by setting altsymbol }
                       ReLabel(pasmsymbol(tai_label(hp2).l));
                     end;
                  ait_const_rva,
                  ait_const_symbol :
                     begin
                       ReLabel(tai_const_symbol(hp2).sym);
                     end;
                  ait_instruction :
                     begin
{$ifdef i386}
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
{$endif i386}
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
             if (pocall_inline in aktprocsym^.definition^.proccalloptions) then
               exprasmList.concatlistcopy(p_asm)
             else
               exprasmList.concatlist(p_asm);
           end;
         if not (nf_object_preserved in flags) then
          begin
{$ifdef i386}
            maybe_loadesi;
{$endif}
{$ifdef m68k}
            maybe_loada5;
{$endif}
          end;
       end;

    procedure ti386statementnode.pass_2;

      var
         hp : tnode;

      begin
         hp:=self;
         while assigned(hp) do
          begin
            if assigned(tstatementnode(hp).right) then
             begin
               cleartempgen;
               {!!!!!!
               oldrl:=temptoremove;
               temptoremove:=new(TLinkedList,init);
               }
               secondpass(tstatementnode(hp).right);
               { !!!!!!!
                 some temporary data which can't be released elsewhere
               removetemps(exprasmlist,temptoremove);
               dispose(temptoremove,done);
               temptoremove:=oldrl;
               }
             end;
            hp:=tstatementnode(hp).left;
          end;
      end;


    procedure ti386blocknode.pass_2;
      begin
      { do second pass on left node }
        if assigned(left) then
         secondpass(left);
      end;


begin
   cstatementnode:=ti386statementnode;
   cblocknode:=ti386blocknode;
   casmnode:=ti386asmnode;
end.
{
  $Log$
  Revision 1.5  2000-12-25 00:07:32  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.4  2000/11/29 00:30:46  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.3  2000/11/04 14:25:23  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.2  2000/10/31 22:02:56  peter
    * symtable splitted, no real code changes

  Revision 1.1  2000/10/15 09:33:31  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.1  2000/10/14 10:14:48  peter
    * moehrendorf oct 2000 rewrite

}
