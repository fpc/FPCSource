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
       symtable,symconst,
       pass_2,tgeni386,
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
        hp,hp2 : pai;
        localfixup,parafixup,
        i : longint;
        skipnode : boolean;
      begin
         if inlining_procedure then
           begin
             InitUsedAsmSymbolList;
             localfixup:=aktprocsym^.definition^.localst^.address_fixup;
             parafixup:=aktprocsym^.definition^.parast^.address_fixup;
             hp:=pai(p_asm^.first);
             while assigned(hp) do
              begin
                hp2:=pai(hp^.getcopy);
                skipnode:=false;
                case hp2^.typ of
                  ait_label :
                     begin
                       { regenerate the labels by setting altsymbol }
                       ReLabel(pasmsymbol(pai_label(hp2)^.l));
                     end;
                  ait_const_rva,
                  ait_const_symbol :
                     begin
                       ReLabel(pai_const_symbol(hp2)^.sym);
                     end;
                  ait_instruction :
                     begin
{$ifdef i386}
                       { fixup the references }
                       for i:=1 to paicpu(hp2)^.ops do
                        begin
                          with paicpu(hp2)^.oper[i-1] do
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
                       if (pai_marker(hp2)^.kind in [AsmBlockStart, AsmBlockEnd]) then
                        skipnode:=true;
                     end;
                   else
                end;
                if not skipnode then
                 exprasmlist^.concat(hp2)
                else
                 dispose(hp2,done);
                hp:=pai(hp^.next);
              end;
             { restore used symbols }
             UsedAsmSymbolListResetAltSym;
             DoneUsedAsmSymbolList;
           end
         else
           begin
             { if the routine is an inline routine, then we must hold a copy
               becuase it can be necessary for inlining later }
             if (pocall_inline in aktprocsym^.definition^.proccalloptions) then
               exprasmlist^.concatlistcopy(p_asm)
             else
               exprasmlist^.concatlist(p_asm);
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
               temptoremove:=new(plinkedlist,init);
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
  Revision 1.1  2000-10-14 10:14:48  peter
    * moehrendorf oct 2000 rewrite

}