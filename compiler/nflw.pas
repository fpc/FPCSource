{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

interface

    uses
       node,aasm,cpubase,
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
       end;

       twhilerepeatnode = class(tloopnode)
          function pass_1 : tnode;override;
       end;

       tifnode = class(tloopnode)
          constructor create(l,r,_t1 : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tfornode = class(tloopnode)
          constructor create(l,r,_t1,_t2 : tnode;back : boolean);virtual;
          function pass_1 : tnode;override;
       end;

       texitnode = class(tunarynode)
          constructor create(l:tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tbreaknode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

       tcontinuenode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

       tgotonode = class(tnode)
          labelnr : pasmlabel;
          labsym : plabelsym;
          constructor create(p : pasmlabel);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
       end;

       tlabelnode = class(tunarynode)
          labelnr : pasmlabel;
          exceptionblock : tnode;
          labsym : plabelsym;
          constructor create(p : pasmlabel;l:tnode);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
       end;

       traisenode = class(tbinarynode)
          frametree : tnode;
          constructor create(l,taddr,tframe:tnode);virtual;
          function getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          function pass_1 : tnode;override;
       end;

       ttryexceptnode = class(tloopnode)
          constructor create(l,r,_t1 : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       ttryfinallynode = class(tbinarynode)
          constructor create(l,r:tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tonnode = class(tbinarynode)
          exceptsymtable : psymtable;
          excepttype : pobjectdef;
          constructor create(l,r:tnode);virtual;
          destructor destroy;override;
          function pass_1 : tnode;override;
          function getcopy : tnode;override;
       end;

       tfailnode = class(tnode)
          constructor create;virtual;
          function pass_1: tnode;override;
       end;

    { for compatibilty }
    function genloopnode(t : tnodetype;l,r,n1 : tnode;back : boolean) : tnode;

    var
       cwhilerepeatnode : class of twhilerepeatnode;
       cifnode : class of tifnode;
       cfornode : class of tfornode;
       cexitnode : class of texitnode;
       cbreaknode : class of tbreaknode;
       ccontinuenode : class of tcontinuenode;
       cgotonode : class of tgotonode;
       clabelnode : class of tlabelnode;
       craisenode : class of traisenode;
       ctryexceptnode : class of ttryexceptnode;
       ctryfinallynode : class of ttryfinallynode;
       connode : class of tonnode;
       cfailnode : class of tfailnode;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symtable,types,htypechk,pass_1,
      ncon,nmem,nld,ncnv,nbas,tgcpu,hcodegen
{$ifdef newcg}
      ,tgobj
      ,cgbase
{$else newcg}
      ,temp_gen
{$endif newcg}
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


{****************************************************************************
                               TWHILEREPEATNODE
*****************************************************************************}

    function twhilerepeatnode.pass_1 : tnode;

      var
         old_t_times : longint;
      begin
         pass_1:=nil;
         old_t_times:=t_times;

         { calc register weight }
         if not(cs_littlesize in aktglobalswitches ) then
           t_times:=t_times*8;
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}

         firstpass(left);
         set_varstate(left,true);
         if codegenerror then
           exit;
         if not is_boolean(left.resulttype) then
           begin
             CGMessage(type_e_mismatch);
             exit;
           end;

         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         { loop instruction }
         if assigned(right) then
           begin
{$ifdef newcg}
              tg.cleartempgen;
{$else newcg}
              cleartempgen;
{$endif newcg}
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

         t_times:=old_t_times;
      end;


{*****************************************************************************
                               TIFNODE
*****************************************************************************}

    constructor tifnode.create(l,r,_t1 : tnode);

      begin
         inherited create(ifn,l,r,_t1,nil);
      end;

    function tifnode.pass_1 : tnode;
      var
         old_t_times : longint;
         hp : tnode;
      begin
         pass_1:=nil;
         old_t_times:=t_times;
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         firstpass(left);
         set_varstate(left,true);

         { Only check type if no error, we can't leave here because
           the right also needs to be firstpassed }
         if not codegenerror then
          begin
            if not is_boolean(left.resulttype) then
              Message1(type_e_boolean_expr_expected,left.resulttype^.typename);
          end;

         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         { determines registers weigths }
         if not(cs_littlesize in aktglobalswitches) then
           t_times:=t_times div 2;
         if t_times=0 then
           t_times:=1;

         { if path }
         if assigned(right) then
           begin
{$ifdef newcg}
              tg.cleartempgen;
{$else newcg}
              cleartempgen;
{$endif newcg}
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
{$ifdef newcg}
              tg.cleartempgen;
{$else newcg}
              cleartempgen;
{$endif newcg}
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
                     pass_1:=hp
                   else
                     pass_1:=cnothingnode.create;
                end
              else
                begin
                   hp:=t1;
                   t1:=nil;
                   { we cannot set p to nil !!! }
                   if assigned(hp) then
                     pass_1:=hp
                   else
                     pass_1:=cnothingnode.create;
                end;
           end;

         t_times:=old_t_times;
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

    function tfornode.pass_1 : tnode;

      var
         old_t_times : longint;
         hp : tnode;
      begin
         result:=nil;
         { Calc register weight }
         old_t_times:=t_times;
         if not(cs_littlesize in aktglobalswitches) then
           t_times:=t_times*8;

         if left.nodetype<>assignn then
           begin
              CGMessage(cg_e_illegal_expression);
              exit;
           end;
         { save counter var }
         t2:=tassignmentnode(left).left.getcopy;

{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         firstpass(left);
         set_varstate(left,false);

{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
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
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         firstpass(t2);
         set_varstate(t2,true);
         if codegenerror then
          exit;

         { Check count var, record fields are also allowed in tp7 }
         hp:=t2;
         while (hp.nodetype=subscriptn) do
          hp:=tsubscriptnode(hp).left;
         { we need a simple loadn, but the load must be in a global symtable or
           in the same lexlevel }
         if (hp.nodetype=funcretn) or
            ((hp.nodetype=loadn) and
             ((tloadnode(hp).symtable^.symtablelevel<=1) or
              (tloadnode(hp).symtable^.symtablelevel=lexlevel))) then
          begin
            if tloadnode(hp).symtableentry^.typ=varsym then
              pvarsym(tloadnode(hp).symtableentry)^.varstate:=vs_used;
            if (not(is_ordinal(t2.resulttype)) or is_64bitint(t2.resulttype)) then
              CGMessagePos(hp.fileinfo,type_e_ordinal_expr_expected);
          end
         else
          CGMessagePos(hp.fileinfo,cg_e_illegal_count_var);

         if t2.registers32>registers32 then
           registers32:=t2.registers32;
         if t2.registersfpu>registersfpu then
           registersfpu:=t2.registersfpu;
{$ifdef SUPPORT_MMX}
         if t2.registersmmx>registersmmx then
           registersmmx:=t2.registersmmx;
{$endif SUPPORT_MMX}

{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         firstpass(right);
         set_varstate(right,true);
         if right.nodetype<>ordconstn then
           begin
              right:=gentypeconvnode(right,t2.resulttype);
{$ifdef newcg}
              tg.cleartempgen;
{$else newcg}
              cleartempgen;
{$endif newcg}
              firstpass(right);
           end;

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
         t_times:=old_t_times;
      end;


{*****************************************************************************
                             TEXITNODE
*****************************************************************************}

    constructor texitnode.create(l:tnode);

      begin
        inherited create(exitn,l);
      end;

    function texitnode.pass_1 : tnode;
      var
         pt : tfuncretnode;
      begin
         pass_1:=nil;
         resulttype:=voiddef;
         if assigned(left) then
           begin
              firstpass(left);
              procinfo^.funcret_state:=vs_assigned;
              if codegenerror then
               exit;
              { Check the 2 types }
              left:=gentypeconvnode(left,procinfo^.returntype.def);
              firstpass(left);
              if ret_in_param(procinfo^.returntype.def) or procinfo^.no_fast_exit then
                begin
                  pt:=cfuncretnode.create;
                  pt.rettype.setdef(procinfo^.returntype.def);
                  pt.funcretprocinfo:=procinfo;
                  left:=cassignmentnode.create(pt,left);
                  firstpass(left);
                end;
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

    function tcontinuenode.pass_1 : tnode;
      begin
        result:=nil;
      end;


{*****************************************************************************
                             TGOTONODE
*****************************************************************************}

    constructor tgotonode.create(p : pasmlabel);

      begin
        inherited create(goton);
        labelnr:=p;
      end;


    function tgotonode.pass_1 : tnode;
      begin
         pass_1:=nil;
         resulttype:=voiddef;
      end;

   function tgotonode.getcopy : tnode;

     var
        p : tgotonode;

     begin
        p:=tgotonode(inherited getcopy);
        p.labelnr:=labelnr;
        p.labsym:=labsym;
        result:=p;
     end;

{*****************************************************************************
                             TLABELNODE
*****************************************************************************}

    constructor tlabelnode.create(p : pasmlabel;l:tnode);

      begin
        inherited create(labeln,l);
        labelnr:=p;
        exceptionblock:=nil;
        labsym:=nil;
      end;


    function tlabelnode.pass_1 : tnode;

      begin
         pass_1:=nil;
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         exceptionblock:=aktexceptblock;
         firstpass(left);
         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         resulttype:=voiddef;
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

    function traisenode.pass_1 : tnode;
      begin
         pass_1:=nil;
         resulttype:=voiddef;
         if assigned(left) then
           begin
              { first para must be a _class_ }
              firstpass(left);
              if assigned(left.resulttype) and
                 not(is_class(left.resulttype)) then
                CGMessage(type_e_mismatch);
              set_varstate(left,true);
              if codegenerror then
               exit;
              { insert needed typeconvs for addr,frame }
              if assigned(right) then
               begin
                 { addr }
                 firstpass(right);
                 right:=gentypeconvnode(right,s32bitdef);
                 firstpass(right);
                 if codegenerror then
                  exit;
                 { frame }
                 if assigned(frametree) then
                  begin
                    firstpass(frametree);
                    frametree:=gentypeconvnode(frametree,s32bitdef);
                    firstpass(frametree);
                    if codegenerror then
                     exit;
                  end;
               end;
              left_right_max;
           end;
      end;


{*****************************************************************************
                             TTRYEXCEPTNODE
*****************************************************************************}

    constructor ttryexceptnode.create(l,r,_t1 : tnode);

      begin
         inherited create(tryexceptn,l,r,_t1,nil);
      end;

    function ttryexceptnode.pass_1 : tnode;

      var
         oldexceptblock : tnode;

      begin
         pass_1:=nil;
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         oldexceptblock:=aktexceptblock;
         aktexceptblock:=left;
         firstpass(left);
         aktexceptblock:=oldexceptblock;
         { on statements }
         if assigned(right) then
           begin
{$ifdef newcg}
              tg.cleartempgen;
{$else newcg}
              cleartempgen;
{$endif newcg}
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=right;
              firstpass(right);
              aktexceptblock:=oldexceptblock;
              registers32:=max(registers32,right.registers32);
              registersfpu:=max(registersfpu,right.registersfpu);
{$ifdef SUPPORT_MMX}
              registersmmx:=max(registersmmx,right.registersmmx);
{$endif SUPPORT_MMX}
           end;
         { else block }
         if assigned(t1) then
           begin
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=t1;
              firstpass(t1);
              aktexceptblock:=oldexceptblock;
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

    function ttryfinallynode.pass_1 : tnode;

      var
         oldexceptblock : tnode;

      begin
         pass_1:=nil;
         resulttype:=voiddef;
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         oldexceptblock:=aktexceptblock;
         aktexceptblock:=left;
         firstpass(left);
         aktexceptblock:=oldexceptblock;
         set_varstate(left,true);
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         oldexceptblock:=aktexceptblock;
         aktexceptblock:=right;
         firstpass(right);
         aktexceptblock:=oldexceptblock;
         set_varstate(right,true);
         if codegenerror then
           exit;
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
         dispose(exceptsymtable,done);
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

    function tonnode.pass_1 : tnode;

      var
         oldexceptblock : tnode;

      begin
         pass_1:=nil;
         { that's really an example procedure for a firstpass :) }
         if not(is_class(excepttype)) then
           CGMessage(type_e_mismatch);
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         resulttype:=voiddef;
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

{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         if assigned(right) then
           begin
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=right;
              firstpass(right);
              aktexceptblock:=oldexceptblock;
              registers32:=max(registers32,right.registers32);
              registersfpu:=max(registersfpu,right.registersfpu);
{$ifdef SUPPORT_MMX}
              registersmmx:=max(registersmmx,right.registersmmx);
{$endif SUPPORT_MMX}
           end;
      end;

{*****************************************************************************
                                TONNODE
*****************************************************************************}


    constructor tfailnode.create;

      begin
         inherited create(failn);
      end;

    function tfailnode.pass_1 : tnode;

      begin
         pass_1:=nil;
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
  Revision 1.11  2000-11-29 00:30:33  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.10  2000/11/04 14:25:20  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.9  2000/10/31 22:02:48  peter
    * symtable splitted, no real code changes

  Revision 1.8  2000/10/21 18:16:11  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.7  2000/10/14 21:52:55  peter
    * fixed memory leaks

  Revision 1.6  2000/10/14 10:14:50  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.5  2000/10/01 19:48:24  peter
    * lot of compile updates for cg11

  Revision 1.4  2000/09/28 19:49:52  florian
  *** empty log message ***

  Revision 1.3  2000/09/24 21:15:34  florian
    * some errors fix to get more stuff compilable

  Revision 1.2  2000/09/24 15:06:19  peter
    * use defines.inc

  Revision 1.1  2000/09/22 22:46:03  florian
    + initial revision

}
