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
unit nbas;

{$i defines.inc}

interface

    uses
       aasm,node;

    type
       tnothingnode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
          procedure pass_2;override;
       end;

       terrornode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

       tasmnode = class(tnode)
          p_asm : paasmoutput;
          constructor create(p : paasmoutput);virtual;
          destructor destroy;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
       end;

       tstatementnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
{$ifdef extdebug}
          procedure dowrite;override;
{$endif extdebug}
       end;

       tblocknode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
       end;

    var
       cnothingnode : class of tnothingnode;
       cerrornode : class of terrornode;
       casmnode : class of tasmnode;
       cstatementnode : class of tstatementnode;
       cblocknode : class of tblocknode;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symtype,symdef,types,
      pass_1,
      nflw,tgcpu,hcodegen
{$ifdef newcg}
      ,cgbase
{$endif}
      ;

{*****************************************************************************
                             TFIRSTNOTHING
*****************************************************************************}

    constructor tnothingnode.create;

      begin
         inherited create(nothingn);
      end;

    function tnothingnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         resulttype:=voiddef;
      end;

    procedure tnothingnode.pass_2;

      begin
         { avoid an abstract rte }
      end;


{*****************************************************************************
                             TFIRSTERROR
*****************************************************************************}

    constructor terrornode.create;

      begin
         inherited create(errorn);
      end;

    function terrornode.pass_1 : tnode;
      begin
         pass_1:=nil;
         include(flags,nf_error);
         codegenerror:=true;
         resulttype:=generrordef;
      end;

{*****************************************************************************
                            TSTATEMENTNODE
*****************************************************************************}

    constructor tstatementnode.create(l,r : tnode);

      begin
         inherited create(statementn,l,r);
      end;

    function tstatementnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         { left is the next statement in the list }
         resulttype:=voiddef;
         { no temps over several statements }
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         { right is the statement itself calln assignn or a complex one }
         {must_be_valid:=true; obsolete PM }
         firstpass(right);
         if (not (cs_extsyntax in aktmoduleswitches)) and
            assigned(right.resulttype) and
            (right.resulttype<>pdef(voiddef)) then
           CGMessage(cg_e_illegal_expression);
         if codegenerror then
           exit;
         registers32:=right.registers32;
         registersfpu:=right.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=right.registersmmx;
{$endif SUPPORT_MMX}
         { left is the next in the list }
         firstpass(left);
         if codegenerror then
           exit;
         if right.registers32>registers32 then
           registers32:=right.registers32;
         if right.registersfpu>registersfpu then
           registersfpu:=right.registersfpu;
{$ifdef SUPPORT_MMX}
         if right.registersmmx>registersmmx then
           registersmmx:=right.registersmmx;
{$endif}
      end;

{$ifdef extdebug}
    procedure tstatementnode.dowrite;

      begin
         { can't use inherited dowrite, because that will use the
           binary which we don't want for statements }
         dowritenodetype;
         writeln(',');
         { write the statement }
         writenodeindention:=writenodeindention+'    ';
         writenode(right);
         writeln(')');
         delete(writenodeindention,1,4);
         { go on with the next statement }
         writenode(left);
      end;
{$endif}

{*****************************************************************************
                             TBLOCKNODE
*****************************************************************************}

    constructor tblocknode.create(l : tnode);

      begin
         inherited create(blockn,l);
      end;

    function tblocknode.pass_1 : tnode;
      var
         hp : tstatementnode;
         count : longint;
      begin
         pass_1:=nil;
         count:=0;
         hp:=tstatementnode(left);
         while assigned(hp) do
           begin
              if cs_regalloc in aktglobalswitches then
                begin
                   { node transformations }

                   { concat function result to exit }
                   { this is wrong for string or other complex
                     result types !!! }
                   if ret_in_acc(procinfo^.returntype.def) and
                      assigned(hp.left) and
                      assigned(tstatementnode(hp.left).right) and
                      (tstatementnode(hp.left).right.nodetype=exitn) and
                      (hp.right.nodetype=assignn) and
                      { !!!! this tbinarynode should be tassignmentnode }
                      (tbinarynode(hp.right).left.nodetype=funcretn) then
                      begin
                         if assigned(texitnode(tstatementnode(hp.left).right).left) then
                           CGMessage(cg_n_inefficient_code)
                         else
                           begin
                              texitnode(tstatementnode(hp.left).right).left:=tstatementnode(hp.right).right;
                              tstatementnode(hp.right).right:=nil;
                              hp.right.free;
                              hp.right:=nil;
                           end;
                      end
                   { warning if unreachable code occurs and elimate this }
                   else if (hp.right.nodetype in
                     [exitn,breakn,continuen,goton]) and
                     { statement node (JM) }
                     assigned(hp.left) and
                     { kind of statement! (JM) }
                     assigned(tstatementnode(hp.left).right) and
                     (tstatementnode(hp.left).right.nodetype<>labeln) then
                     begin
                        { use correct line number }
                        aktfilepos:=hp.left.fileinfo;
                        hp.left.free;
                        hp.left:=nil;
                        CGMessage(cg_w_unreachable_code);
                        { old lines }
                        aktfilepos:=hp.right.fileinfo;
                     end;
                end;
              if assigned(hp.right) then
                begin
{$ifdef newcg}
                   tg.cleartempgen;
{$else newcg}
                   cleartempgen;
{$endif newcg}
                   codegenerror:=false;
                   firstpass(hp.right);
                   if (not (cs_extsyntax in aktmoduleswitches)) and
                      assigned(hp.right.resulttype) and
                      (hp.right.resulttype<>pdef(voiddef)) then
                     CGMessage(cg_e_illegal_expression);
                   {if codegenerror then
                     exit;}
                   hp.registers32:=hp.right.registers32;
                   hp.registersfpu:=hp.right.registersfpu;
{$ifdef SUPPORT_MMX}
                   hp.registersmmx:=hp.right.registersmmx;
{$endif SUPPORT_MMX}
                end
              else
                hp.registers32:=0;

              if hp.registers32>registers32 then
                registers32:=hp.registers32;
              if hp.registersfpu>registersfpu then
                registersfpu:=hp.registersfpu;
{$ifdef SUPPORT_MMX}
              if hp.registersmmx>registersmmx then
                registersmmx:=hp.registersmmx;
{$endif}
              inc(count);
              hp:=tstatementnode(hp.left);
           end;
      end;


{*****************************************************************************
                             TASMNODE
*****************************************************************************}

    constructor tasmnode.create(p : paasmoutput);

      begin
         inherited create(asmn);
         p_asm:=p;
      end;

    destructor tasmnode.destroy;
      begin
        if assigned(p_asm) then
         dispose(p_asm,done);
        inherited destroy;
      end;

    function tasmnode.getcopy: tnode;
      var
        n: tasmnode;
      begin
        n := tasmnode(inherited getcopy);
        if assigned(p_asm) then
          begin
            new(n.p_asm,init);
            n.p_asm^.concatlistcopy(p_asm);
          end
        else n.p_asm := nil;
        getcopy := n;
      end;

    function tasmnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         procinfo^.flags:=procinfo^.flags or pi_uses_asm;
      end;

begin
   cnothingnode:=tnothingnode;
   cerrornode:=terrornode;
   casmnode:=tasmnode;
   cstatementnode:=tstatementnode;
   cblocknode:=tblocknode;
end.
{
  $Log$
  Revision 1.5  2000-11-29 00:30:31  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.4  2000/10/31 22:02:47  peter
    * symtable splitted, no real code changes

  Revision 1.3  2000/10/27 14:57:16  jonas
    + implementation for tasmnode.getcopy

  Revision 1.2  2000/10/14 21:52:54  peter
    * fixed memory leaks

  Revision 1.1  2000/10/14 10:14:50  peter
    * moehrendorf oct 2000 rewrite

}
