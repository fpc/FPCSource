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
       node,aasmbase,aasmtai,aasmcpu,cpubase,
       symbase,symdef,symsym;

    type
       tloopnode = class(tbinarynode)
          t1,t2 : tnode;
          constructor create(tt : tnodetype;l,r,_t1,_t2 : tnode);virtual;
          destructor destroy;override;
          function getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
{$ifdef extdebug}
          procedure dowrite;override;
{$endif extdebug}
          function docompare(p: tnode): boolean; override;
       end;

       twhilerepeatnode = class(tloopnode)
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
{$ifdef state_tracking}
	  procedure track_state_pass(exec_known:boolean);override;
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
          constructor create(l,r,_t1,_t2 : tnode;back : boolean);virtual;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
       end;
       tfornodeclass = class of tfornode;

       texitnode = class(tunarynode)
          constructor create(l:tnode);virtual;
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
          labelnr : tasmlabel;
          labsym : tlabelsym;
          exceptionblock : integer;
          constructor create(p : tlabelsym);virtual;
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
          function getcopy : tnode;override;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       tlabelnodeclass = class of tlabelnode;

       traisenode = class(tbinarynode)
          frametree : tnode;
          constructor create(l,taddr,tframe:tnode);virtual;
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
      symconst,symtable,types,htypechk,pass_1,
      ncon,nmem,nld,ncnv,nbas,rgobj,
    {$ifdef state_tracking}
      nstate,
    {$endif}
      cgbase
      ;

    function genloopnode(t : tnodetype;l,r,n1 : tnode;back : boolean) : tnode;

      var
         p : tnode;

      begin
         case t of
            ifn:
               p:=cifnode.create(l,r,n1);
            repeatn:
               p:=cwhilerepeatnode.create(repeatn,l,r,n1,nil);
            whilen:
               p:=cwhilerepeatnode.create(whilen,l,r,n1,nil);
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
    procedure tloopnode.dowrite;
      begin
        inherited dowrite;
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
          t1.isequal(tloopnode(p).t1) and
          t2.isequal(tloopnode(p).t2);
      end;

{****************************************************************************
                               TWHILEREPEATNODE
*****************************************************************************}

    function twhilerepeatnode.det_resulttype:tnode;
      begin
         result:=nil;
         resulttype:=voidtype;

         resulttypepass(left);
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
    procedure Twhilerepeatnode.track_state_pass(exec_known:boolean);
    
    var condition:Tnode;
	code:Tnode;
	done:boolean;
	value:boolean;
    
    begin
	done:=false;
	repeat
	    condition:=left.getcopy;
	    condition.track_state_pass(exec_known);
	    {Force new resulttype pass.}
	    condition.resulttype.def:=nil;
	    do_resulttypepass(condition);
	    code:=right.getcopy;
	    if is_constboolnode(condition) then
		begin
		    value:=Tordconstnode(condition).value<>0;
		    if value then
			code.track_state_pass(exec_known)
		    else
		        done:=true;
		end
	    else
		{Remove any modified variables from the state.}
		code.track_state_pass(false);
	    code.destroy;
	    condition.destroy;
	until done;
	{The loop condition is also known, for example:
	 while i<10 do
	    begin
	        ...
	    end;
	 
	 When the loop is done, we do know that i<10 = false.
	}
	condition:=left.getcopy;
        condition.track_state_pass(exec_known);
	{Force new resulttype pass.}
        condition.resulttype.def:=nil;
	do_resulttypepass(condition);
	aktstate.store_fact(condition,cordconstnode.create(0,booltype));
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
           include(flags,nf_backward);
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
            ((hp.nodetype=loadn) and
             ((tloadnode(hp).symtable.symtablelevel<=1) or
              (tloadnode(hp).symtable.symtablelevel=lexlevel))) then
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
     begin
         result:=nil;
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
              if ret_in_param(aktprocdef.rettype.def) or
                 (procinfo^.no_fast_exit) or
                 ((procinfo^.flags and pi_uses_exceptions)<>0) then
               begin
                 pt:=cfuncretnode.create(aktprocdef.funcretsym);
                 left:=cassignmentnode.create(pt,left);
               end;
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
      end;


{*****************************************************************************
                             TGOTONODE
*****************************************************************************}

    constructor tgotonode.create(p : tlabelsym);
      begin
        inherited create(goton);
        exceptionblock:=aktexceptblock;
        labsym:=p;
        labelnr:=p.lab;
      end;


    function tgotonode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=voidtype;
      end;


    function tgotonode.pass_1 : tnode;
      begin
         result:=nil;
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
        p.labelnr:=labelnr;
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
                 inserttypeconv(right,s32bittype);
                 { frame }
                 if assigned(frametree) then
                  begin
                    resulttypepass(frametree);
                    inserttypeconv(frametree,s32bittype);
                  end;
               end;
           end;
      end;


    function traisenode.pass_1 : tnode;
      begin
         result:=nil;
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
  Revision 1.35  2002-07-14 18:00:44  daniel
  + Added the beginning of a state tracker. This will track the values of
    variables through procedures and optimize things away.

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

  Revision 1.29  2002/05/12 16:53:07  peter
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

  Revision 1.28  2002/03/31 20:26:34  jonas
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
