{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Type checking and register allocation for nodes that influence
    the flow

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
unit nflw;

{$i fpcdefs.inc}

interface

    uses
       node,cpubase,
       aasmbase,aasmtai,aasmcpu,symnot,
       symppu,symtype,symbase,symdef,symsym;

    type
       { flags used by loop nodes }
       tloopflags = (
         { set if it is a for ... downto ... do loop }
         lnf_backward,
         { Do we need to parse childs to set var state? }
         lnf_varstate,
         { Do a test at the begin of the loop?}
         lnf_testatbegin,
         { Negate the loop test? }
         lnf_checknegate,
         { Should the value of the loop variable on exit be correct. }
         lnf_dont_mind_loopvar_on_exit);
    const
         { loop flags which must match to consider loop nodes equal regarding the flags }
         loopflagsequal = [lnf_backward];

    type
       tloopnode = class(tbinarynode)
          t1,t2 : tnode;
          loopflags : set of tloopflags;
          constructor create(tt : tnodetype;l,r,_t1,_t2 : tnode);virtual;
          destructor destroy;override;
          function getcopy : tnode;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          procedure insertintolist(l : tnodelist);override;
{$ifdef extdebug}
          procedure _dowrite;override;
{$endif extdebug}
          function docompare(p: tnode): boolean; override;
       end;

       twhilerepeatnode = class(tloopnode)
          constructor create(l,r,_t1:Tnode;tab,cn:boolean);virtual;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
{$ifdef state_tracking}
          function track_state_pass(exec_known:boolean):boolean;override;
{$endif}
       end;
       twhilerepeatnodeclass = class of twhilerepeatnode;

       tifnode = class(tloopnode)
          constructor create(l,r,_t1 : tnode);virtual;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
       end;
       tifnodeclass = class of tifnode;

       tfornode = class(tloopnode)
          loopvar_notid:cardinal;
          constructor create(l,r,_t1,_t2 : tnode;back : boolean);virtual;
          procedure loop_var_access(not_type:Tnotification_flag;symbol:Tsym);
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
       end;
       tfornodeclass = class of tfornode;

       texitnode = class(tunarynode)
          onlyassign : boolean;
          constructor create(l:tnode);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
       end;
       texitnodeclass = class of texitnode;

       tbreaknode = class(tnode)
          constructor create;virtual;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
       end;
       tbreaknodeclass = class of tbreaknode;

       tcontinuenode = class(tnode)
          constructor create;virtual;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
       end;
       tcontinuenodeclass = class of tcontinuenode;

       tgotonode = class(tnode)
          labsym : tlabelsym;
          exceptionblock : integer;
          constructor create(p : tlabelsym);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          function getcopy : tnode;override;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       tgotonodeclass = class of tgotonode;

       tlabelnode = class(tunarynode)
          labelnr : tasmlabel;
          labsym : tlabelsym;
          exceptionblock : integer;
          constructor createcase(p : tasmlabel;l:tnode);virtual;
          constructor create(p : tlabelsym;l:tnode);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          function getcopy : tnode;override;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       tlabelnodeclass = class of tlabelnode;

       traisenode = class(tbinarynode)
          frametree : tnode;
          constructor create(l,taddr,tframe:tnode);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          function getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       traisenodeclass = class of traisenode;

       ttryexceptnode = class(tloopnode)
          constructor create(l,r,_t1 : tnode);virtual;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
       end;
       ttryexceptnodeclass = class of ttryexceptnode;

       ttryfinallynode = class(tbinarynode)
          constructor create(l,r:tnode);virtual;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
       end;
       ttryfinallynodeclass = class of ttryfinallynode;

       tonnode = class(tbinarynode)
          exceptsymtable : tsymtable;
          excepttype : tobjectdef;
          constructor create(l,r:tnode);virtual;
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
          function getcopy : tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       tonnodeclass = class of tonnode;

       tfailnode = class(tnode)
          constructor create;virtual;
          function det_resulttype:tnode;override;
          function pass_1: tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       tfailnodeclass = class of tfailnode;

    { for compatibilty }
    function genloopnode(t : tnodetype;l,r,n1 : tnode;back : boolean) : tnode;

    var
       cwhilerepeatnode : twhilerepeatnodeclass;
       cifnode : tifnodeclass;
       cfornode : tfornodeclass;
       cexitnode : texitnodeclass;
       cbreaknode : tbreaknodeclass;
       ccontinuenode : tcontinuenodeclass;
       cgotonode : tgotonodeclass;
       clabelnode : tlabelnodeclass;
       craisenode : traisenodeclass;
       ctryexceptnode : ttryexceptnodeclass;
       ctryfinallynode : ttryfinallynodeclass;
       connode : tonnodeclass;
       cfailnode : tfailnodeclass;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symtable,paramgr,defutil,htypechk,pass_1,
      ncon,nmem,nld,ncnv,nbas,rgobj,
    {$ifdef state_tracking}
      nstate,
    {$endif}
      cginfo,cgbase
      ;

    function genloopnode(t : tnodetype;l,r,n1 : tnode;back : boolean) : tnode;

      var
         p : tnode;

      begin
         case t of
            ifn:
               p:=cifnode.create(l,r,n1);
            whilerepeatn:
               if back then
                  {Repeat until.}
                  p:=cwhilerepeatnode.create(l,r,n1,false,true)
               else
                  {While do.}
                  p:=cwhilerepeatnode.create(l,r,n1,true,false);
            forn:
               p:=cfornode.create(l,r,n1,nil,back);
         end;
         genloopnode:=p;
      end;

{****************************************************************************
                                 TLOOPNODE
*****************************************************************************}

    constructor tloopnode.create(tt : tnodetype;l,r,_t1,_t2 : tnode);

      begin
         inherited create(tt,l,r);
         t1:=_t1;
         t2:=_t2;
         set_file_line(l);
      end;

    destructor tloopnode.destroy;

      begin
         t1.free;
         t2.free;
         inherited destroy;
      end;


    constructor tloopnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        t1:=ppuloadnode(ppufile);
        t2:=ppuloadnode(ppufile);
      end;


    procedure tloopnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppuwritenode(ppufile,t1);
        ppuwritenode(ppufile,t2);
      end;


    procedure tloopnode.derefimpl;
      begin
        inherited derefimpl;
        if assigned(t1) then
          t1.derefimpl;
        if assigned(t2) then
          t2.derefimpl;
      end;


    function tloopnode.getcopy : tnode;

      var
         p : tloopnode;

      begin
         p:=tloopnode(inherited getcopy);
         if assigned(t1) then
           p.t1:=t1.getcopy
         else
           p.t1:=nil;
         if assigned(t2) then
           p.t2:=t2.getcopy
         else
           p.t2:=nil;
         getcopy:=p;
      end;

    procedure tloopnode.insertintolist(l : tnodelist);

      begin
      end;
{$ifdef extdebug}
    procedure tloopnode._dowrite;
      begin
        inherited _dowrite;
        writenodeindention:=writenodeindention+'    ';
        writenode(t1);
        writenode(t2);
        delete(writenodeindention,1,4);
      end;
{$endif extdebug}

    function tloopnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (loopflags*loopflagsequal=tloopnode(p).loopflags*loopflagsequal) and
          t1.isequal(tloopnode(p).t1) and
          t2.isequal(tloopnode(p).t2);
      end;

{****************************************************************************
                               TWHILEREPEATNODE
*****************************************************************************}

    constructor Twhilerepeatnode.create(l,r,_t1:Tnode;tab,cn:boolean);

    begin
        inherited create(whilerepeatn,l,r,_t1,nil);
        if tab then
            include(loopflags, lnf_testatbegin);
        if cn then
            include(loopflags,lnf_checknegate);
    end;

    function twhilerepeatnode.det_resulttype:tnode;
      var
         t:Tunarynode;
      begin
         result:=nil;
         resulttype:=voidtype;

         resulttypepass(left);
         {A not node can be removed.}
         if left.nodetype=notn then
            begin
                t:=Tunarynode(left);
                left:=Tunarynode(left).left;
                t.left:=nil;
                t.destroy;
{$ifdef Delphi}
                { How can this be handled in Delphi ? }
                RunError(255);
{$else}
                {Symdif operator, in case you are wondering:}
                loopflags:=loopflags >< [lnf_checknegate];
{$endif}
            end;
         { loop instruction }
         if assigned(right) then
           resulttypepass(right);
         set_varstate(left,true);
         if codegenerror then
           exit;
         if not is_boolean(left.resulttype.def) then
           begin
             CGMessage(type_e_mismatch);
             exit;
           end;
      end;


    function twhilerepeatnode.pass_1 : tnode;
      var
         old_t_times : longint;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         old_t_times:=rg.t_times;

         { calc register weight }
         if not(cs_littlesize in aktglobalswitches ) then
           rg.t_times:=rg.t_times*8;
         rg.cleartempgen;

         firstpass(left);
         if codegenerror then
           exit;
         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         { loop instruction }
         if assigned(right) then
           begin
              rg.cleartempgen;
              firstpass(right);
              if codegenerror then
                exit;

              if registers32<right.registers32 then
                registers32:=right.registers32;
              if registersfpu<right.registersfpu then
                registersfpu:=right.registersfpu;
{$ifdef SUPPORT_MMX}
              if registersmmx<right.registersmmx then
                registersmmx:=right.registersmmx;
{$endif SUPPORT_MMX}
           end;

         rg.t_times:=old_t_times;
      end;

{$ifdef state_tracking}
    function Twhilerepeatnode.track_state_pass(exec_known:boolean):boolean;

    var condition:Tnode;
        code:Tnode;
        done:boolean;
        value:boolean;
        change:boolean;
        firsttest:boolean;
        factval:Tnode;

    begin
        track_state_pass:=false;
        done:=false;
        firsttest:=true;
        {For repeat until statements, first do a pass through the code.}
        if not(lnf_testatbegin in flags) then
            begin
                code:=right.getcopy;
                if code.track_state_pass(exec_known) then
                    track_state_pass:=true;
                code.destroy;
            end;
        repeat
            condition:=left.getcopy;
            code:=right.getcopy;
            change:=condition.track_state_pass(exec_known);
            factval:=aktstate.find_fact(left);
            if factval<>nil then
                begin
                    condition.destroy;
                    condition:=factval.getcopy;
                    change:=true;
                end;
            if change then
                begin
                    track_state_pass:=true;
                    {Force new resulttype pass.}
                    condition.resulttype.def:=nil;
                    do_resulttypepass(condition);
                end;
            if is_constboolnode(condition) then
                begin
                    {Try to turn a while loop into a repeat loop.}
                    if firsttest then
                        exclude(flags,testatbegin);
                    value:=(Tordconstnode(condition).value<>0) xor checknegate;
                    if value then
                        begin
                            if code.track_state_pass(exec_known) then
                                track_state_pass:=true;
                        end
                    else
                        done:=true;
                end
            else
                begin
                    {Remove any modified variables from the state.}
                    code.track_state_pass(false);
                    done:=true;
                end;
            code.destroy;
            condition.destroy;
            firsttest:=false;
        until done;
        {The loop condition is also known, for example:
         while i<10 do
            begin
                ...
            end;

         When the loop is done, we do know that i<10 = false.
        }
        condition:=left.getcopy;
        if condition.track_state_pass(exec_known) then
            begin
                track_state_pass:=true;
                {Force new resulttype pass.}
                condition.resulttype.def:=nil;
                do_resulttypepass(condition);
            end;
        if not is_constboolnode(condition) then
            aktstate.store_fact(condition,
             cordconstnode.create(byte(checknegate),booltype,true))
        else
            condition.destroy;
    end;
{$endif}

{*****************************************************************************
                               TIFNODE
*****************************************************************************}

    constructor tifnode.create(l,r,_t1 : tnode);
      begin
         inherited create(ifn,l,r,_t1,nil);
      end;


    function tifnode.det_resulttype:tnode;
      begin
         result:=nil;
         resulttype:=voidtype;

         resulttypepass(left);
         { if path }
         if assigned(right) then
           resulttypepass(right);
         { else path }
         if assigned(t1) then
           resulttypepass(t1);
         set_varstate(left,true);
         if codegenerror then
           exit;

         if not is_boolean(left.resulttype.def) then
           Message1(type_e_boolean_expr_expected,left.resulttype.def.typename);
      end;


    function tifnode.pass_1 : tnode;
      var
         old_t_times : longint;
         hp : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         old_t_times:=rg.t_times;
         rg.cleartempgen;
         firstpass(left);
         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         { determines registers weigths }
         if not(cs_littlesize in aktglobalswitches) then
           rg.t_times:=rg.t_times div 2;
         if rg.t_times=0 then
           rg.t_times:=1;

         { if path }
         if assigned(right) then
           begin
              rg.cleartempgen;
              firstpass(right);

              if registers32<right.registers32 then
                registers32:=right.registers32;
              if registersfpu<right.registersfpu then
                registersfpu:=right.registersfpu;
{$ifdef SUPPORT_MMX}
              if registersmmx<right.registersmmx then
                registersmmx:=right.registersmmx;
{$endif SUPPORT_MMX}
           end;

         { else path }
         if assigned(t1) then
           begin
              rg.cleartempgen;
              firstpass(t1);

              if registers32<t1.registers32 then
                registers32:=t1.registers32;
              if registersfpu<t1.registersfpu then
                registersfpu:=t1.registersfpu;
{$ifdef SUPPORT_MMX}
              if registersmmx<t1.registersmmx then
                registersmmx:=t1.registersmmx;
{$endif SUPPORT_MMX}
           end;

         { leave if we've got an error in one of the paths }

         if codegenerror then
           exit;

         if left.nodetype=ordconstn then
           begin
              { optimize }
              if tordconstnode(left).value=1 then
                begin
                   hp:=right;
                   right:=nil;
                   { we cannot set p to nil !!! }
                   if assigned(hp) then
                     result:=hp
                   else
                     result:=cnothingnode.create;
                end
              else
                begin
                   hp:=t1;
                   t1:=nil;
                   { we cannot set p to nil !!! }
                   if assigned(hp) then
                     result:=hp
                   else
                     result:=cnothingnode.create;
                end;
           end;

         rg.t_times:=old_t_times;
      end;


{*****************************************************************************
                              TFORNODE
*****************************************************************************}

    constructor tfornode.create(l,r,_t1,_t2 : tnode;back : boolean);

      begin
         inherited create(forn,l,r,_t1,_t2);
         if back then
           include(loopflags,lnf_backward);
         include(loopflags,lnf_testatbegin);
      end;

    procedure Tfornode.loop_var_access(not_type:Tnotification_flag;
                                       symbol:Tsym);

    begin
      {If there is a read access, the value of the loop counter is important;
       at the end of the loop the loop variable should contain the value it
       had in the last iteration.}
      if not_type=vn_onwrite then
        begin
          writeln('Loopvar does not matter on exit');
        end
      else
        begin
          exclude(loopflags,lnf_dont_mind_loopvar_on_exit);
          writeln('Loopvar does matter on exit');
        end;
      Tvarsym(symbol).unregister_notification(loopvar_notid);
    end;

    function tfornode.det_resulttype:tnode;
      var
        hp : tnode;
      begin
         result:=nil;
         resulttype:=voidtype;


         if left.nodetype<>assignn then
           begin
              CGMessage(cg_e_illegal_expression);
              exit;
           end;

         {Can we spare the first comparision?}
         if (right.nodetype=ordconstn) and (Tassignmentnode(left).right.nodetype=ordconstn) then
            if (
                (lnf_backward in loopflags) and
                (Tordconstnode(Tassignmentnode(left).right).value>=Tordconstnode(right).value)
               )
             or not(
                    (lnf_backward in loopflags) and
                    (Tordconstnode(Tassignmentnode(left).right).value<=Tordconstnode(right).value)
                   ) then
                exclude(loopflags,lnf_testatbegin);

         { save counter var }
         t2:=tassignmentnode(left).left.getcopy;

         resulttypepass(left);
         set_varstate(left,false);

         if assigned(t1) then
           begin
             resulttypepass(t1);
             if codegenerror then
               exit;
           end;

         { process count var }
         resulttypepass(t2);
         set_varstate(t2,true);
         if codegenerror then
           exit;

         { Check count var, record fields are also allowed in tp7 }
         hp:=t2;
         while (hp.nodetype=subscriptn) or
               ((hp.nodetype=vecn) and
                is_constintnode(tvecnode(hp).right)) do
           hp:=tunarynode(hp).left;
         { we need a simple loadn, but the load must be in a global symtable or
           in the same lexlevel }
         if (hp.nodetype=funcretn) or
            (
             (hp.nodetype=loadn) and
             (
              (tloadnode(hp).symtable.symtablelevel<=1) or
              (tloadnode(hp).symtable.symtablelevel=lexlevel)
             ) and
             not(
                 (tloadnode(hp).symtableentry.typ=varsym) and
                 ((tvarsym(tloadnode(hp).symtableentry).varspez in [vs_var,vs_out]) or
                  (vo_is_thread_var in tvarsym(tloadnode(hp).symtableentry).varoptions))
                )
            ) then
          begin
            if (hp.nodetype=loadn) and
               (tloadnode(hp).symtableentry.typ=varsym) then
              tvarsym(tloadnode(hp).symtableentry).varstate:=vs_used;
            if (not(is_ordinal(t2.resulttype.def)) or is_64bitint(t2.resulttype.def)) then
              CGMessagePos(hp.fileinfo,type_e_ordinal_expr_expected);
          end
         else
           CGMessagePos(hp.fileinfo,cg_e_illegal_count_var);

         resulttypepass(right);
         set_varstate(right,true);
         inserttypeconv(right,t2.resulttype);
      end;


    function tfornode.pass_1 : tnode;
      var
         old_t_times : longint;
      {$ifdef loopvar_dont_mind}
         hp : Tnode;
      {$endif loopvar_dont_mind}
     begin
         result:=nil;
         expectloc:=LOC_VOID;
         { Calc register weight }
         old_t_times:=rg.t_times;
         if not(cs_littlesize in aktglobalswitches) then
           rg.t_times:=rg.t_times*8;

         rg.cleartempgen;
         firstpass(left);

         rg.cleartempgen;
         if assigned(t1) then
          begin
            firstpass(t1);
            if codegenerror then
             exit;
          end;
         registers32:=t1.registers32;
         registersfpu:=t1.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         if left.registers32>registers32 then
           registers32:=left.registers32;
         if left.registersfpu>registersfpu then
           registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         if left.registersmmx>registersmmx then
           registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         { process count var }
         rg.cleartempgen;
         firstpass(t2);
         if codegenerror then
          exit;
         if t2.registers32>registers32 then
           registers32:=t2.registers32;
         if t2.registersfpu>registersfpu then
           registersfpu:=t2.registersfpu;
{$ifdef SUPPORT_MMX}
         if t2.registersmmx>registersmmx then
           registersmmx:=t2.registersmmx;
{$endif SUPPORT_MMX}

         rg.cleartempgen;
         firstpass(right);
      {$ifdef loopvar_dont_mind}
         { Check count var, record fields are also allowed in tp7 }
         include(loopflags,lnf_dont_mind_loopvar_on_exit);
         hp:=t2;
         while (hp.nodetype=subscriptn) or
               ((hp.nodetype=vecn) and
                is_constintnode(tvecnode(hp).right)) do
           hp:=tunarynode(hp).left;
         if (hp.nodetype=loadn) and (Tloadnode(hp).symtableentry.typ=varsym) then
            loopvar_notid:=Tvarsym(Tloadnode(hp).symtableentry).
             register_notification([vn_onread,vn_onwrite],@loop_var_access);
      {$endif}
         if right.registers32>registers32 then
           registers32:=right.registers32;
         if right.registersfpu>registersfpu then
           registersfpu:=right.registersfpu;
{$ifdef SUPPORT_MMX}
         if right.registersmmx>registersmmx then
           registersmmx:=right.registersmmx;
{$endif SUPPORT_MMX}
         { we need at least one register for comparisons PM }
         if registers32=0 then
           inc(registers32);
         rg.t_times:=old_t_times;
      end;


{*****************************************************************************
                             TEXITNODE
*****************************************************************************}

    constructor texitnode.create(l:tnode);
      begin
        inherited create(exitn,l);
        onlyassign:=false;
      end;


    constructor texitnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        onlyassign:=boolean(ppufile.getbyte);
      end;


    procedure texitnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putbyte(byte(onlyassign));
      end;


    function texitnode.det_resulttype:tnode;
      var
         pt : tnode;
      begin
        result:=nil;
        { Check the 2 types }
        if not inlining_procedure then
         begin
           if assigned(left) then
            begin
              inserttypeconv(left,aktprocdef.rettype);
              if paramanager.ret_in_param(aktprocdef.rettype.def,aktprocdef.proccalloption) or
                 (procinfo.no_fast_exit) or
                 ((procinfo.flags and pi_uses_exceptions)<>0) then
               begin
                 pt:=cfuncretnode.create(aktprocdef.funcretsym);
                 left:=cassignmentnode.create(pt,left);
                 onlyassign:=true;
               end;
              tfuncretsym(aktprocdef.funcretsym).funcretstate:=vs_assigned;
            end;
         end;
        if assigned(left) then
         begin
           resulttypepass(left);
           set_varstate(left,true);
         end;
        resulttype:=voidtype;
      end;


    function texitnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         if assigned(left) then
           begin
              firstpass(left);
              if codegenerror then
               exit;
              registers32:=left.registers32;
              registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
              registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
           end;
      end;


{*****************************************************************************
                             TBREAKNODE
*****************************************************************************}

    constructor tbreaknode.create;

      begin
        inherited create(breakn);
      end;


    function tbreaknode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=voidtype;
      end;


    function tbreaknode.pass_1 : tnode;
      begin
        result:=nil;
        expectloc:=LOC_VOID;
      end;


{*****************************************************************************
                             TCONTINUENODE
*****************************************************************************}

    constructor tcontinuenode.create;
      begin
        inherited create(continuen);
      end;


    function tcontinuenode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=voidtype;
      end;


    function tcontinuenode.pass_1 : tnode;
      begin
        result:=nil;
        expectloc:=LOC_VOID;
      end;


{*****************************************************************************
                             TGOTONODE
*****************************************************************************}

    constructor tgotonode.create(p : tlabelsym);
      begin
        inherited create(goton);
        exceptionblock:=aktexceptblock;
        labsym:=p;
      end;


    constructor tgotonode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        labsym:=tlabelsym(ppufile.getderef);
        exceptionblock:=ppufile.getbyte;
      end;


    procedure tgotonode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(labsym);
        ppufile.putbyte(exceptionblock);
      end;


    procedure tgotonode.derefimpl;
      begin
        inherited derefimpl;
        resolvesym(pointer(labsym));
      end;


    function tgotonode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=voidtype;
      end;


    function tgotonode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         { check if }
         if assigned(labsym) and
            assigned(labsym.code) and
            (exceptionblock<>tlabelnode(labsym.code).exceptionblock) then
           begin
             writeln('goto exceptblock: ',exceptionblock);
             writeln('label exceptblock: ',tlabelnode(labsym.code).exceptionblock);
             CGMessage(cg_e_goto_inout_of_exception_block);
           end;
      end;


   function tgotonode.getcopy : tnode;
     var
        p : tgotonode;
     begin
        p:=tgotonode(inherited getcopy);
        p.labsym:=labsym;
        p.exceptionblock:=exceptionblock;
        result:=p;
     end;


    function tgotonode.docompare(p: tnode): boolean;
      begin
        docompare := false;
      end;


{*****************************************************************************
                             TLABELNODE
*****************************************************************************}

    constructor tlabelnode.createcase(p : tasmlabel;l:tnode);
      begin
        inherited create(labeln,l);
        { it shouldn't be possible to jump to case labels using goto }
        exceptionblock:=-1;
        labsym:=nil;
        labelnr:=p;
      end;


    constructor tlabelnode.create(p : tlabelsym;l:tnode);
      begin
        inherited create(labeln,l);
        exceptionblock:=aktexceptblock;
        labsym:=p;
        labelnr:=p.lab;
        { save the current labelnode in the labelsym }
        p.code:=self;
      end;


    constructor tlabelnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        labsym:=tlabelsym(ppufile.getderef);
        labelnr:=tasmlabel(ppufile.getasmsymbol);
        exceptionblock:=ppufile.getbyte;
      end;


    procedure tlabelnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(labsym);
        ppufile.putasmsymbol(labelnr);
        ppufile.putbyte(exceptionblock);
      end;


    procedure tlabelnode.derefimpl;
      begin
        inherited derefimpl;
        resolvesym(pointer(labsym));
        objectlibrary.derefasmsymbol(tasmsymbol(labelnr));
      end;


    function tlabelnode.det_resulttype:tnode;
      begin
        result:=nil;
        { left could still be unassigned }
        if assigned(left) then
         resulttypepass(left);
        resulttype:=voidtype;
      end;


    function tlabelnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         if assigned(left) then
          begin
            rg.cleartempgen;
            firstpass(left);
            registers32:=left.registers32;
            registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
            registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
          end;
      end;


   function tlabelnode.getcopy : tnode;
     var
        p : tlabelnode;
     begin
        p:=tlabelnode(inherited getcopy);
        p.labelnr:=labelnr;
        p.exceptionblock:=exceptionblock;
        p.labsym:=labsym;
        result:=p;
     end;


    function tlabelnode.docompare(p: tnode): boolean;
      begin
        docompare := false;
      end;


{*****************************************************************************
                            TRAISENODE
*****************************************************************************}

    constructor traisenode.create(l,taddr,tframe:tnode);
      begin
         inherited create(raisen,l,taddr);
         frametree:=tframe;
      end;


    constructor traisenode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        frametree:=ppuloadnode(ppufile);
      end;


    procedure traisenode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppuwritenode(ppufile,frametree);
      end;


    procedure traisenode.derefimpl;
      begin
        inherited derefimpl;
        if assigned(frametree) then
          frametree.derefimpl;
      end;


    function traisenode.getcopy : tnode;
      var
         n : traisenode;
      begin
         n:=traisenode(inherited getcopy);
         if assigned(frametree) then
           n.frametree:=frametree.getcopy
         else
           n.frametree:=nil;
         getcopy:=n;
      end;


    procedure traisenode.insertintolist(l : tnodelist);
      begin
      end;


    function traisenode.det_resulttype:tnode;
      begin
         result:=nil;
         resulttype:=voidtype;
         if assigned(left) then
           begin
              { first para must be a _class_ }
              resulttypepass(left);
              set_varstate(left,true);
              if codegenerror then
               exit;
              if not(is_class(left.resulttype.def)) then
                CGMessage(type_e_mismatch);
              { insert needed typeconvs for addr,frame }
              if assigned(right) then
               begin
                 { addr }
                 resulttypepass(right);
                 inserttypeconv(right,voidpointertype);
                 { frame }
                 if assigned(frametree) then
                  begin
                    resulttypepass(frametree);
                    inserttypeconv(frametree,voidpointertype);
                  end;
               end;
           end;
      end;


    function traisenode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         if assigned(left) then
           begin
              { first para must be a _class_ }
              firstpass(left);
              { insert needed typeconvs for addr,frame }
              if assigned(right) then
               begin
                 { addr }
                 firstpass(right);
                 { frame }
                 if assigned(frametree) then
                  firstpass(frametree);
               end;
              left_right_max;
           end;
      end;


    function traisenode.docompare(p: tnode): boolean;
      begin
        docompare := false;
      end;


{*****************************************************************************
                             TTRYEXCEPTNODE
*****************************************************************************}

    constructor ttryexceptnode.create(l,r,_t1 : tnode);
      begin
         inherited create(tryexceptn,l,r,_t1,nil);
      end;


    function ttryexceptnode.det_resulttype:tnode;
      begin
         result:=nil;
         resulttypepass(left);
         { on statements }
         if assigned(right) then
           resulttypepass(right);
         { else block }
         if assigned(t1) then
           resulttypepass(t1);
         resulttype:=voidtype;
      end;


    function ttryexceptnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         rg.cleartempgen;
         firstpass(left);
         { on statements }
         if assigned(right) then
           begin
              rg.cleartempgen;
              firstpass(right);
              registers32:=max(registers32,right.registers32);
              registersfpu:=max(registersfpu,right.registersfpu);
{$ifdef SUPPORT_MMX}
              registersmmx:=max(registersmmx,right.registersmmx);
{$endif SUPPORT_MMX}
           end;
         { else block }
         if assigned(t1) then
           begin
              firstpass(t1);
              registers32:=max(registers32,t1.registers32);
              registersfpu:=max(registersfpu,t1.registersfpu);
{$ifdef SUPPORT_MMX}
              registersmmx:=max(registersmmx,t1.registersmmx);
{$endif SUPPORT_MMX}
           end;
      end;


{*****************************************************************************
                           TTRYFINALLYNODE
*****************************************************************************}

    constructor ttryfinallynode.create(l,r:tnode);
      begin
        inherited create(tryfinallyn,l,r);
      end;


    function ttryfinallynode.det_resulttype:tnode;
      begin
         result:=nil;
         resulttype:=voidtype;

         resulttypepass(left);
         set_varstate(left,true);

         resulttypepass(right);
         set_varstate(right,true);
      end;


    function ttryfinallynode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         rg.cleartempgen;
         firstpass(left);

         rg.cleartempgen;
         firstpass(right);
         left_right_max;
      end;


{*****************************************************************************
                                TONNODE
*****************************************************************************}

    constructor tonnode.create(l,r:tnode);
      begin
         inherited create(onn,l,r);
         exceptsymtable:=nil;
         excepttype:=nil;
      end;


    destructor tonnode.destroy;
      begin
        if assigned(exceptsymtable) then
         exceptsymtable.free;
        inherited destroy;
      end;


    constructor tonnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        exceptsymtable:=nil;
        excepttype:=nil;
      end;


    function tonnode.getcopy : tnode;
      var
         n : tonnode;
      begin
         n:=tonnode(inherited getcopy);
         n.exceptsymtable:=exceptsymtable;
         n.excepttype:=excepttype;
         result:=n;
      end;


    function tonnode.det_resulttype:tnode;
      begin
         result:=nil;
         resulttype:=voidtype;
         if not(is_class(excepttype)) then
           CGMessage(type_e_mismatch);
         if assigned(left) then
           resulttypepass(left);
         if assigned(right) then
           resulttypepass(right);
      end;


    function tonnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         rg.cleartempgen;
         registers32:=0;
         registersfpu:=0;
{$ifdef SUPPORT_MMX}
         registersmmx:=0;
{$endif SUPPORT_MMX}
         if assigned(left) then
           begin
              firstpass(left);
              registers32:=left.registers32;
              registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
              registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
           end;

         rg.cleartempgen;
         if assigned(right) then
           begin
              firstpass(right);
              registers32:=max(registers32,right.registers32);
              registersfpu:=max(registersfpu,right.registersfpu);
{$ifdef SUPPORT_MMX}
              registersmmx:=max(registersmmx,right.registersmmx);
{$endif SUPPORT_MMX}
           end;
      end;


    function tonnode.docompare(p: tnode): boolean;
      begin
        docompare := false;
      end;


{*****************************************************************************
                                TFAILNODE
*****************************************************************************}


    constructor tfailnode.create;
      begin
         inherited create(failn);
      end;


    function tfailnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=voidtype;
      end;


    function tfailnode.pass_1 : tnode;
      begin
        result:=nil;
        expectloc:=LOC_VOID;
      end;


    function tfailnode.docompare(p: tnode): boolean;
      begin
        docompare := false;
      end;



begin
   cwhilerepeatnode:=twhilerepeatnode;
   cifnode:=tifnode;
   cfornode:=tfornode;
   cexitnode:=texitnode;
   cgotonode:=tgotonode;
   clabelnode:=tlabelnode;
   craisenode:=traisenode;
   ctryexceptnode:=ttryexceptnode;
   ctryfinallynode:=ttryfinallynode;
   connode:=tonnode;
   cfailnode:=tfailnode;
end.
{
  $Log$
  Revision 1.66  2003-04-22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.65  2003/03/20 15:54:46  peter
    * don't allow var and out parameters as for loop counter

  Revision 1.64  2003/01/09 21:52:37  peter
    * merged some verbosity options.
    * V_LineInfo is a verbosity flag to include line info

  Revision 1.63  2003/01/04 08:08:47  daniel
    * Readded missing variable

  Revision 1.62  2003/01/03 17:16:57  peter
    * fixed warning about unset funcret

  Revision 1.61  2003/01/03 12:15:56  daniel
    * Removed ifdefs around notifications
      ifdefs around for loop optimizations remain

  Revision 1.60  2002/12/31 09:55:58  daniel
   + Notification implementation complete
   + Add for loop code optimization using notifications
     results in 1.5-1.9% speed improvement in nestloop benchmark
     Optimization incomplete, compiler does not cycle yet with
     notifications enabled.

  Revision 1.59  2002/12/30 22:44:53  daniel
  * Some work on notifications

  Revision 1.58  2002/12/27 15:25:40  peter
    * do not allow threadvar as loop counter

  Revision 1.57  2002/11/28 11:17:02  florian
    * loop node flags from node flags splitted

  Revision 1.56  2002/11/25 17:43:18  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.55  2002/11/18 17:31:56  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.54  2002/10/20 15:31:49  peter
    * set funcret state for exit(0)

  Revision 1.53  2002/10/05 12:43:25  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.52  2002/09/07 15:25:03  peter
    * old logs removed and tabs fixed

  Revision 1.51  2002/09/07 12:16:04  carl
    * second part bug report 1996 fix, testrange in cordconstnode
      only called if option is set (also make parsing a tiny faster)

  Revision 1.50  2002/09/01 18:47:00  peter
    * assignn check in exitnode changed to use a separate boolean as the
      assignn can be changed to a calln

  Revision 1.49  2002/09/01 08:01:16  daniel
   * Removed sets from Tcallnode.det_resulttype
   + Added read/write notifications of variables. These will be usefull
     for providing information for several optimizations. For example
     the value of the loop variable of a for loop does matter is the
     variable is read after the for loop, but if it's no longer used
     or written, it doesn't matter and this can be used to optimize
     the loop code generation.

  Revision 1.48  2002/08/22 15:15:20  daniel
   * Fixed the detection wether the first check of a for loop can be skipped

  Revision 1.47  2002/08/19 19:36:43  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.46  2002/08/17 22:09:46  florian
    * result type handling in tcgcal.pass_2 overhauled
    * better tnode.dowrite
    * some ppc stuff fixed

  Revision 1.45  2002/08/17 09:23:37  florian
    * first part of procinfo rewrite

  Revision 1.44  2002/07/21 06:58:49  daniel
  * Changed booleans into flags

  Revision 1.43  2002/07/20 11:57:54  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.42  2002/07/20 11:18:18  daniel
  * Small mistake fixed; the skip test was done before we know the for node
    is correct.

  Revision 1.40  2002/07/20 08:19:31  daniel
  * State tracker automatically changes while loops into repeat loops

  Revision 1.39  2002/07/19 12:55:27  daniel
  * Further developed state tracking in whilerepeatn

  Revision 1.38  2002/07/19 11:41:35  daniel
  * State tracker work
  * The whilen and repeatn are now completely unified into whilerepeatn. This
    allows the state tracker to change while nodes automatically into
    repeat nodes.
  * Resulttypepass improvements to the notn. 'not not a' is optimized away and
    'not(a>b)' is optimized into 'a<=b'.
  * Resulttypepass improvements to the whilerepeatn. 'while not a' is optimized
    by removing the notn and later switchting the true and falselabels. The
    same is done with 'repeat until not a'.

  Revision 1.37  2002/07/16 13:57:02  florian
    * raise takes now a void pointer as at and frame address
      instead of a longint

  Revision 1.36  2002/07/15 18:03:15  florian
    * readded removed changes

  Revision 1.35  2002/07/14 18:00:44  daniel
  + Added the beginning of a state tracker. This will track the values of
    variables through procedures and optimize things away.

  Revision 1.34  2002/07/11 14:41:28  florian
    * start of the new generic parameter handling

  Revision 1.33  2002/07/01 18:46:23  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.32  2002/05/18 13:34:10  peter
    * readded missing revisions

  Revision 1.31  2002/05/16 19:46:38  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

}
