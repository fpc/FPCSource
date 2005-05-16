{
    $Id: nflw.pas,v 1.112 2005/03/25 22:20:19 peter Exp $
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
       symtype,symbase,symdef,symsym;

    type
       { flags used by loop nodes }
       tloopflag = (
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
       tloopflags = set of tloopflag;

    const
         { loop flags which must match to consider loop nodes equal regarding the flags }
         loopflagsequal = [lnf_backward];

    type
       tloopnode = class(tbinarynode)
          t1,t2 : tnode;
          loopflags : tloopflags;
          constructor create(tt : tnodetype;l,r,_t1,_t2 : tnode);virtual;
          destructor destroy;override;
          function getcopy : tnode;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          procedure insertintolist(l : tnodelist);override;
          procedure printnodetree(var t:text);override;
          function docompare(p: tnode): boolean; override;
       end;

       twhilerepeatnode = class(tloopnode)
          constructor create(l,r:Tnode;tab,cn:boolean);virtual;
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
          labsymderef : tderef;
          exceptionblock : integer;
{          internlab : tinterngotolabel;}
          constructor create(p : tlabelsym);virtual;
{          constructor createintern(g:tinterngotolabel);}
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
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
          labsymderef : tderef;
          exceptionblock : integer;
          constructor createcase(p : tasmlabel;l:tnode);virtual;
          constructor create(p : tlabelsym;l:tnode);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
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
          procedure buildderefimpl;override;
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

       ttryfinallynode = class(tloopnode)
          implicitframe : boolean;
          constructor create(l,r:tnode);virtual;
          constructor create_implicit(l,r,_t1:tnode);virtual;
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


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,paramgr,defcmp,defutil,htypechk,pass_1,
      ncal,nadd,ncon,nmem,nld,ncnv,nbas,cgobj,
    {$ifdef state_tracking}
      nstate,
    {$endif}
      cgbase,procinfo
      ;


{****************************************************************************
                                 TLOOPNODE
*****************************************************************************}

    constructor tloopnode.create(tt : tnodetype;l,r,_t1,_t2 : tnode);

      begin
         inherited create(tt,l,r);
         t1:=_t1;
         t2:=_t2;
         fileinfo:=l.fileinfo;
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


    procedure tloopnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        if assigned(t1) then
          t1.buildderefimpl;
        if assigned(t2) then
          t2.buildderefimpl;
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
         p.loopflags:=loopflags;
         getcopy:=p;
      end;

    procedure tloopnode.insertintolist(l : tnodelist);

      begin
      end;


    procedure tloopnode.printnodetree(var t:text);
      begin
        write(t,printnodeindention,'(');
        printnodeindent;
        printnodeinfo(t);
        writeln(t);
        printnode(t,left);
        printnode(t,right);
        printnode(t,t1);
        printnode(t,t2);
        printnodeunindent;
        writeln(t,printnodeindention,')');
      end;


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

    constructor Twhilerepeatnode.create(l,r:Tnode;tab,cn:boolean);
      begin
          inherited create(whilerepeatn,l,r,nil,nil);
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
                {Symdif operator, in case you are wondering:}
                loopflags:=loopflags >< [lnf_checknegate];
            end;
         { loop instruction }
         if assigned(right) then
           resulttypepass(right);
         set_varstate(left,vs_used,[vsf_must_be_valid]);
         if codegenerror then
           exit;

         if not is_boolean(left.resulttype.def) then
           begin
             if left.resulttype.def.deftype=variantdef then
               inserttypeconv(left,booltype)
             else
               CGMessage1(type_e_boolean_expr_expected,left.resulttype.def.typename);
           end;

         { Give warnings for code that will never be executed for
           while false do }
         if (lnf_testatbegin in loopflags) and
            (left.nodetype=ordconstn) and
            (tordconstnode(left).value=0) and
            assigned(right) then
           CGMessagePos(right.fileinfo,cg_w_unreachable_code);
      end;


    function twhilerepeatnode.pass_1 : tnode;
      var
         old_t_times : longint;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         old_t_times:=cg.t_times;

         { calc register weight }
         if not(cs_littlesize in aktglobalswitches ) then
           cg.t_times:=cg.t_times*8;

         firstpass(left);
         if codegenerror then
           exit;
         registersint:=left.registersint;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         { loop instruction }
         if assigned(right) then
           begin
              firstpass(right);
              if codegenerror then
                exit;

              if registersint<right.registersint then
                registersint:=right.registersint;
              if registersfpu<right.registersfpu then
                registersfpu:=right.registersfpu;
{$ifdef SUPPORT_MMX}
              if registersmmx<right.registersmmx then
                registersmmx:=right.registersmmx;
{$endif SUPPORT_MMX}
           end;

         cg.t_times:=old_t_times;
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
         set_varstate(left,vs_used,[vsf_must_be_valid]);
         if codegenerror then
           exit;

         if not is_boolean(left.resulttype.def) then
           begin
             if left.resulttype.def.deftype=variantdef then
               inserttypeconv(left,booltype)
             else
               Message1(type_e_boolean_expr_expected,left.resulttype.def.typename);
           end;

         { optimize constant expressions }
         if left.nodetype=ordconstn then
           begin
              if tordconstnode(left).value=1 then
                begin
                   if assigned(right) then
                     result:=right
                   else
                     result:=cnothingnode.create;
                   right:=nil;
                   if assigned(t1) then
                     CGMessagePos(t1.fileinfo,cg_w_unreachable_code);
                end
              else
                begin
                   if assigned(t1) then
                     result:=t1
                   else
                     result:=cnothingnode.create;
                   t1:=nil;
                   if assigned(right) then
                     CGMessagePos(right.fileinfo,cg_w_unreachable_code);
                end;
           end;
      end;


    function tifnode.pass_1 : tnode;
      var
         old_t_times : longint;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         old_t_times:=cg.t_times;
         firstpass(left);
         registersint:=left.registersint;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         { determines registers weigths }
         if not(cs_littlesize in aktglobalswitches) then
           cg.t_times:=cg.t_times div 2;
         if cg.t_times=0 then
           cg.t_times:=1;

         { if path }
         if assigned(right) then
           begin
              firstpass(right);

              if registersint<right.registersint then
                registersint:=right.registersint;
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
              firstpass(t1);

              if registersint<t1.registersint then
                registersint:=t1.registersint;
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

         cg.t_times:=old_t_times;
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
      Tabstractvarsym(symbol).unregister_notification(loopvar_notid);
    end;

    function tfornode.det_resulttype:tnode;
      begin
         result:=nil;
         resulttype:=voidtype;

         { process the loopvar, from and to, varstates are already set }
         resulttypepass(left);
         resulttypepass(right);
         resulttypepass(t1);

         {Can we spare the first comparision?}
         if (t1.nodetype=ordconstn) and
            (right.nodetype=ordconstn) and
            (
             (
              (lnf_backward in loopflags) and
              (Tordconstnode(right).value>=Tordconstnode(t1).value)
             ) or
             (
               not(lnf_backward in loopflags) and
               (Tordconstnode(right).value<=Tordconstnode(t1).value)
             )
            ) then
           exclude(loopflags,lnf_testatbegin);

         { Make sure that the loop var and the
           from and to values are compatible types }
         inserttypeconv(right,left.resulttype);
         inserttypeconv(t1,left.resulttype);

         if assigned(t2) then
           resulttypepass(t2);
      end;


    function tfornode.pass_1 : tnode;
      var
         old_t_times : longint;
     begin
         result:=nil;
         expectloc:=LOC_VOID;

         firstpass(left);
         if left.registersint>registersint then
           registersint:=left.registersint;
         if left.registersfpu>registersfpu then
           registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         if left.registersmmx>registersmmx then
           registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         firstpass(right);
         if right.registersint>registersint then
           registersint:=right.registersint;
         if right.registersfpu>registersfpu then
           registersfpu:=right.registersfpu;
{$ifdef SUPPORT_MMX}
         if right.registersmmx>registersmmx then
           registersmmx:=right.registersmmx;
{$endif SUPPORT_MMX}

         firstpass(t1);
         if t1.registersint>registersint then
           registersint:=t1.registersint;
         if t1.registersfpu>registersfpu then
           registersfpu:=t1.registersfpu;
{$ifdef SUPPORT_MMX}
         if t1.registersmmx>registersmmx then
           registersmmx:=t1.registersmmx;
{$endif SUPPORT_MMX}

         if assigned(t2) then
          begin
            { Calc register weight }
            old_t_times:=cg.t_times;
            if not(cs_littlesize in aktglobalswitches) then
              cg.t_times:=cg.t_times*8;
            firstpass(t2);
            if codegenerror then
             exit;
            if t2.registersint>registersint then
              registersint:=t2.registersint;
            if t2.registersfpu>registersfpu then
              registersfpu:=t2.registersfpu;
{$ifdef SUPPORT_MMX}
            if t2.registersmmx>registersmmx then
              registersmmx:=t2.registersmmx;
{$endif SUPPORT_MMX}
            cg.t_times:=old_t_times;
          end;

         { we need at least one register for comparisons PM }
         if registersint=0 then
           inc(registersint);
      end;


{*****************************************************************************
                             TEXITNODE
*****************************************************************************}

    constructor texitnode.create(l:tnode);
      begin
        inherited create(exitn,l);
      end;


    constructor texitnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
      end;


    procedure texitnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
      end;


    function texitnode.det_resulttype:tnode;
      begin
        result:=nil;
        if assigned(left) then
          begin
            { add assignment to funcretsym }
            inserttypeconv(left,current_procinfo.procdef.rettype);
            left:=cassignmentnode.create(
                cloadnode.create(current_procinfo.procdef.funcretsym,current_procinfo.procdef.funcretsym.owner),
                left);
            resulttypepass(left);
            set_varstate(left,vs_used,[vsf_must_be_valid]);
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
              registersint:=left.registersint;
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
        ppufile.getderef(labsymderef);
        exceptionblock:=ppufile.getbyte;
      end;


    procedure tgotonode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(labsymderef);
        ppufile.putbyte(exceptionblock);
      end;


    procedure tgotonode.buildderefimpl;
      begin
        inherited buildderefimpl;
        labsymderef.build(labsym);
      end;


    procedure tgotonode.derefimpl;
      begin
        inherited derefimpl;
        labsym:=tlabelsym(labsymderef.resolve);
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
           CGMessage(cg_e_goto_inout_of_exception_block);
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
        ppufile.getderef(labsymderef);
        labelnr:=tasmlabel(ppufile.getasmsymbol);
        exceptionblock:=ppufile.getbyte;
      end;


    procedure tlabelnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(labsymderef);
        ppufile.putasmsymbol(labelnr);
        ppufile.putbyte(exceptionblock);
      end;


    procedure tlabelnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        labsymderef.build(labsym);
      end;


    procedure tlabelnode.derefimpl;
      begin
        inherited derefimpl;
        labsym:=tlabelsym(labsymderef.resolve);
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
            firstpass(left);
            registersint:=left.registersint;
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


    procedure traisenode.buildderefimpl;
      begin
        inherited buildderefimpl;
        if assigned(frametree) then
          frametree.buildderefimpl;
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
              set_varstate(left,vs_used,[vsf_must_be_valid]);
              if codegenerror then
               exit;
              if not(is_class(left.resulttype.def)) then
                CGMessage1(type_e_class_type_expected,left.resulttype.def.typename);
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
         include(current_procinfo.flags,pi_do_call);
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
         include(current_procinfo.flags,pi_do_call);
         expectloc:=LOC_VOID;
         firstpass(left);
         { on statements }
         if assigned(right) then
           begin
              firstpass(right);
              registersint:=max(registersint,right.registersint);
              registersfpu:=max(registersfpu,right.registersfpu);
{$ifdef SUPPORT_MMX}
              registersmmx:=max(registersmmx,right.registersmmx);
{$endif SUPPORT_MMX}
           end;
         { else block }
         if assigned(t1) then
           begin
              firstpass(t1);
              registersint:=max(registersint,t1.registersint);
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
        inherited create(tryfinallyn,l,r,nil,nil);
        implicitframe:=false;
      end;


    constructor ttryfinallynode.create_implicit(l,r,_t1:tnode);
      begin
        inherited create(tryfinallyn,l,r,_t1,nil);
        implicitframe:=true;
      end;


    function ttryfinallynode.det_resulttype:tnode;
      begin
         result:=nil;
         include(current_procinfo.flags,pi_do_call);
         resulttype:=voidtype;

         resulttypepass(left);
         set_varstate(left,vs_used,[vsf_must_be_valid]);

         resulttypepass(right);
         set_varstate(right,vs_used,[vsf_must_be_valid]);

         { special finally block only executed when there was an exception }
         if assigned(t1) then
           begin
             resulttypepass(t1);
             set_varstate(t1,vs_used,[vsf_must_be_valid]);
           end;
      end;


    function ttryfinallynode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         firstpass(left);

         firstpass(right);
         left_right_max;

         if assigned(t1) then
           begin
             firstpass(t1);
             registersint:=max(registersint,t1.registersint);
             registersfpu:=max(registersfpu,t1.registersfpu);
{$ifdef SUPPORT_MMX}
             registersmmx:=max(registersmmx,t1.registersmmx);
{$endif SUPPORT_MMX}
           end;
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
        { copied nodes don't need to release the symtable }
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
         n.exceptsymtable:=exceptsymtable.getcopy;
         n.excepttype:=excepttype;
         result:=n;
      end;


    function tonnode.det_resulttype:tnode;
      begin
         result:=nil;
         resulttype:=voidtype;
         if not(is_class(excepttype)) then
           CGMessage1(type_e_class_type_expected,excepttype.typename);
         if assigned(left) then
           resulttypepass(left);
         if assigned(right) then
           resulttypepass(right);
      end;


    function tonnode.pass_1 : tnode;
      begin
         result:=nil;
         include(current_procinfo.flags,pi_do_call);
         expectloc:=LOC_VOID;
         registersint:=0;
         registersfpu:=0;
{$ifdef SUPPORT_MMX}
         registersmmx:=0;
{$endif SUPPORT_MMX}
         if assigned(left) then
           begin
              firstpass(left);
              registersint:=left.registersint;
              registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
              registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
           end;

         if assigned(right) then
           begin
              firstpass(right);
              registersint:=max(registersint,right.registersint);
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
end.
{
  $Log: nflw.pas,v $
  Revision 1.112  2005/03/25 22:20:19  peter
    * add hint when passing an uninitialized variable to a var parameter

  Revision 1.111  2005/03/24 23:06:43  peter
    * don't remove repeat until node in repeat until true;

  Revision 1.110  2005/02/23 20:38:09  florian
    + variants can be used as cond. expr. in if, while, repeat ... until statements

  Revision 1.109  2005/02/14 17:13:06  peter
    * truncate log

  Revision 1.108  2005/01/31 20:23:53  peter
    * set varstate before parsing the instruction block in for statements

  Revision 1.107  2005/01/31 16:16:21  peter
    * for-node cleanup, checking for uninitialzed from and to values
      is now supported

  Revision 1.106  2005/01/16 14:44:03  peter
    * fix unreachable code check for repeat loop

  Revision 1.105  2005/01/16 10:50:32  peter
    * give warning for unreachable code in while/if statements

  Revision 1.104  2005/01/03 17:55:57  florian
    + first batch of patches to support tdef.getcopy fully

}
