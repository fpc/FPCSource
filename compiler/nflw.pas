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
interface

    uses
       node,aasm,cpubase,symtable;

    type
       tloopnode = class(tbinarynode)
          t1,t2 : tnode;
          constructor create(tt : tnodetype;l,r,_t1,_t2 : tnode);virtual;
          destructor destroy;override;
          function getcopy : tnode;override;
       end;

       tlabelednode = class(tnode)
          labelnr : pasmlabel;
          exceptionblock : tnode;
          labsym : plabelsym;
          destructor destroy;override;
          function getcopy : tnode;override;
       end;

       twhilerepeatnode = class(tloopnode)
          function pass_1 : tnode;override;
       end;

       tifnode = class(tloopnode)
          constructor create(l,r,_t1 : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tfornode = class(tloopnode)
          constructor create(l,r,_t1,_t2 : tnode;back : boolean);
          function pass_1 : tnode;override;
       end;

       texitnode = class(tunarynode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

       tgotonode = class(tlabelednode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

       tlabelnode = class(tlabelednode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

       traisenode = class(tbinarynode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

       ttryexceptnode = class(tloopnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

       ttryfinallynode = class(tbinarynode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

       tonnode = class(tbinarynode)
          exceptsymtable : psymtable;
          excepttype : pobjectdef;
          constructor create;virtual;
          function pass_1 : tnode;override;
          destructor destroy;override;
          function getcopy : tnode;override;
       end;

    { for compatibilty }
    function genloopnode(t : tnodetype;l,r,n1 : tnode;back : boolean) : tnode;

    var
       cwhilerepeatnode : class of twhilerepeatnode;
       cifnode : class of tifnode;
       cfornode : class of tfornode;
       cexitnode : class of texitnode;
       cgotonode : class of tgotonode;
       clabelnode : class of tlabelnode;
       craisenode : class of traisenode;
       ctryexceptnode : class of ttryexceptnode;
       ctryfinallynode : class of ttryfinallynode;
       connode : class of tonnode;
       { the block node of the current exception block to check gotos }
       aktexceptblock : tnode;


implementation

    uses
      globtype,systems,
      cutils,cobjects,verbose,globals,
      symconst,types,htypechk,pass_1
{$ifdef newcg}
      ,tgobj
      ,tgcpu
      ,cgbase
{$else newcg}
      ,hcodegen
      ,temp_gen
{$ifdef i386}
      ,tgeni386
{$endif}
{$ifdef m68k}
      ,tgen68k
{$endif m68k}
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
         p.t1:=t1.getcopy;
         p.t2:=t2.getcopy;
         getcopy:=p;
      end;

{****************************************************************************
                                 TTLABELDNODE
*****************************************************************************}

   destructor tlabelednode.destroy;

     begin
     end;

   function tlabelednode.getcopy : tnode;

     var
        p : tlabelednode;

     begin
        p:=tlabelednode(inherited getcopy);
        p.labelnr:=labelnr;
        p.exceptionblock:=exceptionblock;
        p.labsym:=labsym;
     end;

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
         left.set_varstate(true);
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
         left.set_varstate(true);

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
              if left.value=1 then
                begin
                   left.free;
                   hp:=right;
                   right:=nil;
                   t1.free;
                   { we cannot set p to nil !!! }
                   if assigned(hp) then
                     pass_1:=hp
                   else
                     pass_1:=cnothingnode.create;
                end
              else
                begin
                   left.free;
                   hp:=t1;
                   t1:=nil;
                   right.free;
                   { we cannot set p to nil !!! }
                   if assigned(hp) then
                     pass_1:=hp;
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
         inherited create(forn,l,r,_t1_,t2);
         if back then
           include(flags,nf_backward);
      end;

    function tfornode.pass_1 : tnode;

      var
         old_t_times : longint;
         hp : tnode;
      begin
         pass_1:=nil;
         { Calc register weight }
         old_t_times:=t_times;
         if not(cs_littlesize in aktglobalswitches) then
           t_times:=t_times*8;
         { save counter var }
         t2:=left.left.getcopy;

         if left.treetype<>assignn then
           CGMessage(cg_e_illegal_expression);

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

         registers32:=t1^.registers32;
         registersfpu:=t1^.registersfpu;
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
         while (hp.treetype=subscriptn) do
          hp:=hp.left;
         { we need a simple loadn, but the load must be in a global symtable or
           in the same lexlevel }
         if (hp.treetype=funcretn) or
            ((hp.treetype=loadn) and
             ((hp.symtable^.symtablelevel<=1) or
              (hp.symtable^.symtablelevel=lexlevel))) then
          begin
            if hp.symtableentry^.typ=varsym then
              pvarsym(hp.symtableentry)^.varstate:=vs_used;
            if (not(is_ordinal(t2^.resulttype)) or is_64bitint(t2^.resulttype)) then
              CGMessagePos(hp.fileinfo,type_e_ordinal_expr_expected);
          end
         else
          CGMessagePos(hp.fileinfo,cg_e_illegal_count_var);

         if t2^.registers32>registers32 then
           registers32:=t2^.registers32;
         if t2^.registersfpu>registersfpu then
           registersfpu:=t2^.registersfpu;
{$ifdef SUPPORT_MMX}
         if t2^.registersmmx>registersmmx then
           registersmmx:=t2^.registersmmx;
{$endif SUPPORT_MMX}

{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         firstpass(right);
         set_varstate(right,true);
         if right.treetype<>ordconstn then
           begin
              right:=gentypeconvnode(right,t2^.resulttype);
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

    constructor texitnode.create;

      begin
      end;

    function texitnode.pass_1 : tnode;
      var
         pt : tnode;
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
                  pt:=genzeronode(funcretn);
                  pt^.rettype.setdef(procinfo^.returntype.def);
                  pt^.funcretprocinfo:=procinfo;
                  left:=gennode(assignn,pt,left);
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
                             TGOTONODE
*****************************************************************************}

    constructor tgotonode.create;

      begin
      end;

    function tgotonode.pass_1 : tnode;
      begin
         pass_1:=nil;
         resulttype:=voiddef;
      end;

{*****************************************************************************
                             TLABELNODE
*****************************************************************************}

    constructor tlabelnode.create;

      begin
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


{*****************************************************************************
                            TRAISENODE
*****************************************************************************}

    constructor traisenode.create;

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
                 ((left.resulttype^.deftype<>objectdef) or
                  not(pobjectdef(left.resulttype)^.is_class)) then
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
              left_right_max(p);
           end;
      end;


{*****************************************************************************
                             TTRYEXCEPTNODE
*****************************************************************************}

    constructor ttryexceptnode.create;

      begin
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
              registers32:=max(registers32,t1^.registers32);
              registersfpu:=max(registersfpu,t1^.registersfpu);
{$ifdef SUPPORT_MMX}
              registersmmx:=max(registersmmx,t1^.registersmmx);
{$endif SUPPORT_MMX}
           end;
      end;


{*****************************************************************************
                           TTRYFINALLYNODE
*****************************************************************************}

    constructor ttryfinallynode.create;

      begin
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
         left_right_max(p);
      end;


{*****************************************************************************
                                TONNODE
*****************************************************************************}

    constructor tonnode.create;

      begin
      end;

    function tonnode.pass_1 : tnode;

      var
         oldexceptblock : tnode;

      begin
         pass_1:=nil;
         { that's really an example procedure for a firstpass :) }
         if (excepttype^.deftype<>objectdef) or
           not(pobjectdef(excepttype)^.is_class) then
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
  $Log$
  Revision 1.1  2000-09-22 22:46:03  florian
    + initial revision

}