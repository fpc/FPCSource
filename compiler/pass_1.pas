{$ifndef cg11}
{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements the first pass of the code generator

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
unit pass_1;

{$i defines.inc}

interface

    uses
       tree;

    procedure firstpass(var p : ptree);
    function  do_firstpass(var p : ptree) : boolean;


implementation

    uses
      globtype,systems,
      cutils,cobjects,verbose,globals,
      aasm,symtable,types,
      htypechk,
      tcadd,tccal,tccnv,tccon,tcflw,
      tcinl,tcld,tcmat,tcmem,tcset,cpubase,cpuasm
{$ifdef newcg}
      ,cgbase
      ,tgcpu
{$else newcg}
      ,hcodegen
{$ifdef i386}
      ,tgeni386
{$endif}
{$ifdef m68k}
      ,tgen68k
{$endif}
{$endif}
      ;

{*****************************************************************************
                              FirstPass
*****************************************************************************}

    type
       firstpassproc = procedure(var p : ptree);

    procedure firstnothing(var p : ptree);
      begin
         p^.resulttype:=voiddef;
      end;


    procedure firsterror(var p : ptree);
      begin
         p^.error:=true;
         codegenerror:=true;
         p^.resulttype:=generrordef;
      end;


    procedure firststatement(var p : ptree);
      begin
         { left is the next statement in the list }
         p^.resulttype:=voiddef;
         { no temps over several statements }
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         { right is the statement itself calln assignn or a complex one }
         {must_be_valid:=true; obsolete PM }
         firstpass(p^.right);
         if (not (cs_extsyntax in aktmoduleswitches)) and
            assigned(p^.right^.resulttype) and
            (p^.right^.resulttype<>pdef(voiddef)) then
           CGMessage(cg_e_illegal_expression);
         if codegenerror then
           exit;
         p^.registers32:=p^.right^.registers32;
         p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.right^.registersmmx;
{$endif SUPPORT_MMX}
         { left is the next in the list }
         firstpass(p^.left);
         if codegenerror then
           exit;
         if p^.right^.registers32>p^.registers32 then
           p^.registers32:=p^.right^.registers32;
         if p^.right^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.right^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.right^.registersmmx;
{$endif}
      end;


    procedure firstblock(var p : ptree);
      var
         hp : ptree;
         count : longint;
      begin
         count:=0;
         hp:=p^.left;
         while assigned(hp) do
           begin
              if cs_regalloc in aktglobalswitches then
                begin
                   { Codeumstellungen }

                   { Funktionsresultate an exit anh„ngen }
                   { this is wrong for string or other complex
                     result types !!! }
                   if ret_in_acc(procinfo^.returntype.def) and
                      assigned(hp^.left) and
                      assigned(hp^.left^.right) and
                      (hp^.left^.right^.treetype=exitn) and
                      (hp^.right^.treetype=assignn) and
                      (hp^.right^.left^.treetype=funcretn) then
                      begin
                         if assigned(hp^.left^.right^.left) then
                           CGMessage(cg_n_inefficient_code)
                         else
                           begin
                              hp^.left^.right^.left:=hp^.right^.right;
                              hp^.right^.right:=nil;
                              disposetree(hp^.right);
                              hp^.right:=nil;
                           end;
                      end
                   { warning if unreachable code occurs and elimate this }
                   else if (hp^.right^.treetype in
                     [exitn,breakn,continuen,goton]) and
                     { statement node (JM) }
                     assigned(hp^.left) and
                     { kind of statement! (JM) }
                     assigned(hp^.left^.right) and
                     (hp^.left^.right^.treetype<>labeln) then
                     begin
                        { use correct line number }
                        aktfilepos:=hp^.left^.fileinfo;
                        disposetree(hp^.left);
                        hp^.left:=nil;
                        CGMessage(cg_w_unreachable_code);
                        { old lines }
                        aktfilepos:=hp^.right^.fileinfo;
                     end;
                end;
              if assigned(hp^.right) then
                begin
{$ifdef newcg}
                   tg.cleartempgen;
{$else newcg}
                   cleartempgen;
{$endif newcg}
                   codegenerror:=false;
                   firstpass(hp^.right);
                   if (not (cs_extsyntax in aktmoduleswitches)) and
                      assigned(hp^.right^.resulttype) and
                      (hp^.right^.resulttype<>pdef(voiddef)) then
                     CGMessage(cg_e_illegal_expression);
                   {if codegenerror then
                     exit;}
                   hp^.registers32:=hp^.right^.registers32;
                   hp^.registersfpu:=hp^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
                   hp^.registersmmx:=hp^.right^.registersmmx;
{$endif SUPPORT_MMX}
                end
              else
                hp^.registers32:=0;

              if hp^.registers32>p^.registers32 then
                p^.registers32:=hp^.registers32;
              if hp^.registersfpu>p^.registersfpu then
                p^.registersfpu:=hp^.registersfpu;
{$ifdef SUPPORT_MMX}
              if hp^.registersmmx>p^.registersmmx then
                p^.registersmmx:=hp^.registersmmx;
{$endif}
              inc(count);
              hp:=hp^.left;
           end;
      end;



    procedure firstasm(var p : ptree);
      begin
        procinfo^.flags:=procinfo^.flags or pi_uses_asm;
      end;



    procedure firstpass(var p : ptree);
      const
         procedures : array[ttreetyp] of firstpassproc =
            (firstadd,   {addn}
             firstadd,   {muln}
             firstadd,   {subn}
             firstmoddiv,      {divn}
             firstadd,   {symdifn}
             firstmoddiv,      {modn}
             firstassignment,  {assignn}
             firstload, {loadn}
             firstrange,       {range}
             firstadd,   {ltn}
             firstadd,   {lten}
             firstadd,   {gtn}
             firstadd,   {gten}
             firstadd,   {equaln}
             firstadd,   {unequaln}
             firstin,     {inn}
             firstadd,   {orn}
             firstadd,   {xorn}
             firstshlshr,      {shrn}
             firstshlshr,      {shln}
             firstadd,   {slashn}
             firstadd,   {andn}
             firstsubscript,   {subscriptn}
             firstderef,       {derefn}
             firstaddr, {addrn}
             firstdoubleaddr,  {doubleaddrn}
             firstordconst,    {ordconstn}
             firsttypeconv,    {typeconvn}
             firstcalln,       {calln}
             firstnothing,     {callparan}
             firstrealconst,   {realconstn}
             firstfixconst,    {fixconstn}
             firstunaryminus,  {unaryminusn}
             firstasm,         {asmn}
             firstvec,         {vecn}
             firstpointerconst,{pointerconstn}
             firststringconst, {stringconstn}
             firstfuncret,     {funcretn}
             firstself, {selfn}
             firstnot,   {notn}
             firstinline,      {inlinen}
             firstniln, {niln}
             firsterror,       {errorn}
             firsttype, {typen}
             firsthnew, {hnewn}
             firsthdispose,    {hdisposen}
             firstnew,   {newn}
             firstsimplenewdispose, {simpledisposen}
             firstsetelement,  {setelementn}
             firstsetconst,    {setconstn}
             firstblock,       {blockn}
             firststatement,   {statementn}
             firstnothing,     {loopn}
             firstif,     {ifn}
             firstnothing,     {breakn}
             firstnothing,     {continuen}
             first_while_repeat, {repeatn}
             first_while_repeat, {whilen}
             firstfor,   {forn}
             firstexit, {exitn}
             firstwith, {withn}
             firstcase, {casen}
             firstlabel,       {labeln}
             firstgoto, {goton}
             firstsimplenewdispose, {simplenewn}
             firsttryexcept,   {tryexceptn}
             firstraise,       {raisen}
             firstnothing,     {switchesn}
             firsttryfinally,  {tryfinallyn}
             firston,     {onn}
             firstis,     {isn}
             firstas,     {asn}
             firsterror,       {caretn}
             firstnothing,     {failn}
             firstadd,   {starstarn}
             firstprocinline,  {procinlinen}
             firstarrayconstruct, {arrayconstructn}
             firstarrayconstructrange, {arrayconstructrangen}
             firstnothing,     {nothingn}
             firstloadvmt      {loadvmtn}
             );
      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos    : tfileposinfo;
{$ifdef extdebug}
         str1,str2 : string;
         oldp      : ptree;
         not_first : boolean;
{$endif extdebug}
      begin
{$ifdef extdebug}
         inc(total_of_firstpass);
         if (p^.firstpasscount>0) and only_one_pass then
           exit;
{$endif extdebug}
         oldcodegenerror:=codegenerror;
         oldpos:=aktfilepos;
         oldlocalswitches:=aktlocalswitches;
{$ifdef extdebug}
         if p^.firstpasscount>0 then
           begin
              move(p^,str1[1],sizeof(ttree));
              str1[0]:=char(sizeof(ttree));
              new(oldp);
              oldp^:=p^;
              not_first:=true;
              inc(firstpass_several);
           end
         else
           not_first:=false;
{$endif extdebug}

         if not p^.error then
           begin
              codegenerror:=false;
              aktfilepos:=p^.fileinfo;
              aktlocalswitches:=p^.localswitches;
              procedures[p^.treetype](p);
              aktlocalswitches:=oldlocalswitches;
              aktfilepos:=oldpos;
              p^.error:=codegenerror;
              codegenerror:=codegenerror or oldcodegenerror;
           end
         else
           codegenerror:=true;
{$ifdef extdebug}
         if not_first then
           begin
              { dirty trick to compare two ttree's (PM) }
              move(p^,str2[1],sizeof(ttree));
              str2[0]:=char(sizeof(ttree));
              if str1<>str2 then
                begin
                   comment(v_debug,'tree changed after first counting pass '
                     +tostr(longint(p^.treetype)));
                   compare_trees(oldp,p);
                end;
              dispose(oldp);
           end;
         if count_ref then
           inc(p^.firstpasscount);
{$endif extdebug}
      end;


    function do_firstpass(var p : ptree) : boolean;
      begin
         aktexceptblock:=nil;
         codegenerror:=false;
         firstpass(p);
         do_firstpass:=codegenerror;
      end;


end.
{$else cg11}
unit pass_1;

{$i defines.inc}

interface

    uses
       node;

    procedure firstpass(var p : tnode);
    function  do_firstpass(var p : tnode) : boolean;

    type
       tnothingnode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

       terrornode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

       tasmnode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

       tstatementnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tblocknode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
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
      cutils,cobjects,verbose,globals,
      aasm,symtable,types,
      htypechk,
      cpubase,cpuasm
{$ifdef newcg}
      ,cgbase
      ,tgcpu
{$else newcg}
      ,hcodegen
{$ifdef i386}
      ,tgeni386
{$endif}
{$ifdef m68k}
      ,tgen68k
{$endif}
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


{*****************************************************************************
                             TBLOCKNODE
*****************************************************************************}

    constructor tblocknode.create(l,r : tnode);

      begin
         inherited create(blockn,l,r);
      end;

    function tblocknode.pass_1 : tnode;
      var
         hp : tnode;
         count : longint;
      begin
         pass_1:=nil;
         count:=0;
         hp:=left;
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
                      assigned(hp.left.right) and
                      (hp.left.right.treetype=exitn) and
                      (hp.right.treetype=assignn) and
                      (hp.right.left.treetype=funcretn) then
                      begin
                         if assigned(hp.left.right.left) then
                           CGMessage(cg_n_inefficient_code)
                         else
                           begin
                              hp.left.right.left:=hp.right.right;
                              hp.right.right:=nil;
                              disposetree(hp.right);
                              hp.right:=nil;
                           end;
                      end
                   { warning if unreachable code occurs and elimate this }
                   else if (hp.right.treetype in
                     [exitn,breakn,continuen,goton]) and
                     { statement node (JM) }
                     assigned(hp.left) and
                     { kind of statement! (JM) }
                     assigned(hp.left.right) and
                     (hp.left.right.treetype<>labeln) then
                     begin
                        { use correct line number }
                        aktfilepos:=hp.left.fileinfo;
                        disposetree(hp.left);
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

              if hp.registers32>p^.registers32 then
                registers32:=hp.registers32;
              if hp.registersfpu>registersfpu then
                registersfpu:=hp.registersfpu;
{$ifdef SUPPORT_MMX}
              if hp.registersmmx>registersmmx then
                registersmmx:=hp.registersmmx;
{$endif}
              inc(count);
              hp:=hp.left;
           end;
      end;


{*****************************************************************************
                             TASMNODE
*****************************************************************************}

    function tasmnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         procinfo^.flags:=procinfo^.flags or pi_uses_asm;
      end;

{*****************************************************************************
                            Global procedures
*****************************************************************************}

    procedure firstpass(var p : pnode);

      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos    : tfileposinfo;
         hp : tnode;
{$ifdef extdebug}
         str1,str2 : string;
         oldp      : tnode;
         not_first : boolean;
{$endif extdebug}
      begin
{$ifdef extdebug}
         inc(total_of_firstpass);
         if (p^.firstpasscount>0) and only_one_pass then
           exit;
{$endif extdebug}
         oldcodegenerror:=codegenerror;
         oldpos:=aktfilepos;
         oldlocalswitches:=aktlocalswitches;
{$ifdef extdebug}
         if p^.firstpasscount>0 then
           begin
              move(p^,str1[1],sizeof(ttree));
              str1[0]:=char(sizeof(ttree));
              new(oldp);
              oldp^:=p^;
              not_first:=true;
              inc(firstpass_several);
           end
         else
           not_first:=false;
{$endif extdebug}

         if not nf_error in p.flags then
           begin
              codegenerror:=false;
              aktfilepos:=p.fileinfo;
              aktlocalswitches:=p.localswitches;
              hp:=p.pass_1;
              { should the node be replaced? }
              if assigned(hp) then
                begin
                   p.free;
                   p:=hp;
                end;
              aktlocalswitches:=oldlocalswitches;
              aktfilepos:=oldpos;
              if codegenerror then
                include(p.flags,nf_error);
              codegenerror:=codegenerror or oldcodegenerror;
           end
         else
           codegenerror:=true;
{$ifdef extdebug}
         if not_first then
           begin
              { dirty trick to compare two ttree's (PM) }
              move(p^,str2[1],sizeof(ttree));
              str2[0]:=char(sizeof(ttree));
              if str1<>str2 then
                begin
                   comment(v_debug,'tree changed after first counting pass '
                     +tostr(longint(p^.treetype)));
                   compare_trees(oldp,p);
                end;
              dispose(oldp);
           end;
         if count_ref then
           inc(p^.firstpasscount);
{$endif extdebug}
      end;


    function do_firstpass(var p : tnode) : boolean;
      begin
         aktexceptblock:=nil;
         codegenerror:=false;
         firstpass(p);
         do_firstpass:=codegenerror;
      end;

begin
   cnothingnode:=tnothingnode;
   cerrornode:=terrornode;
   casmnode:=tasmnode;
   cstatementnode:=tstatementnode;
   cblocknode:=tblocknode;
end.
{$endif cg11}
{
  $Log$
  Revision 1.4  2000-09-24 15:06:21  peter
    * use defines.inc

  Revision 1.3  2000/09/19 23:09:07  pierre
   * problems wih extdebug cond. solved

  Revision 1.2  2000/07/13 11:32:44  michael
  + removed logs

}
