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
unit tcflw;
interface

    uses
      tree;

    procedure first_while_repeat(var p : ptree);
    procedure firstif(var p : ptree);
    procedure firstfor(var p : ptree);
    procedure firstexit(var p : ptree);
    procedure firstgoto(var p : ptree);
    procedure firstlabel(var p : ptree);
    procedure firstraise(var p : ptree);
    procedure firsttryexcept(var p : ptree);
    procedure firsttryfinally(var p : ptree);
    procedure firston(var p : ptree);

var
   { the block node of the current exception block to check gotos }
   aktexceptblock : ptree;


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,htypechk,pass_1,cpubase
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

{*****************************************************************************
                         First_While_RepeatN
*****************************************************************************}

    procedure first_while_repeat(var p : ptree);
      var
         old_t_times : longint;
      begin
         old_t_times:=t_times;

         { calc register weight }
         if not(cs_littlesize in aktglobalswitches ) then
           t_times:=t_times*8;
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}

         firstpass(p^.left);
         set_varstate(p^.left,true);
         if codegenerror then
           exit;
         if not is_boolean(p^.left^.resulttype) then
           begin
             CGMessage(type_e_mismatch);
             exit;
           end;

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         { loop instruction }
         if assigned(p^.right) then
           begin
{$ifdef newcg}
              tg.cleartempgen;
{$else newcg}
              cleartempgen;
{$endif newcg}
              firstpass(p^.right);
              if codegenerror then
                exit;

              if p^.registers32<p^.right^.registers32 then
                p^.registers32:=p^.right^.registers32;
              if p^.registersfpu<p^.right^.registersfpu then
                p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
              if p^.registersmmx<p^.right^.registersmmx then
                p^.registersmmx:=p^.right^.registersmmx;
{$endif SUPPORT_MMX}
           end;

         t_times:=old_t_times;
      end;


{*****************************************************************************
                               FirstIfN
*****************************************************************************}

    procedure firstif(var p : ptree);
      var
         old_t_times : longint;
         hp : ptree;
      begin
         old_t_times:=t_times;
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         firstpass(p^.left);
         set_varstate(p^.left,true);

         { Only check type if no error, we can't leave here because
           the p^.right also needs to be firstpassed }
         if not codegenerror then
          begin
            if not is_boolean(p^.left^.resulttype) then
              Message1(type_e_boolean_expr_expected,p^.left^.resulttype^.typename);
          end;

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         { determines registers weigths }
         if not(cs_littlesize in aktglobalswitches) then
           t_times:=t_times div 2;
         if t_times=0 then
           t_times:=1;

         { if path }
         if assigned(p^.right) then
           begin
{$ifdef newcg}
              tg.cleartempgen;
{$else newcg}
              cleartempgen;
{$endif newcg}
              firstpass(p^.right);

              if p^.registers32<p^.right^.registers32 then
                p^.registers32:=p^.right^.registers32;
              if p^.registersfpu<p^.right^.registersfpu then
                p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
              if p^.registersmmx<p^.right^.registersmmx then
                p^.registersmmx:=p^.right^.registersmmx;
{$endif SUPPORT_MMX}
           end;

         { else path }
         if assigned(p^.t1) then
           begin
{$ifdef newcg}
              tg.cleartempgen;
{$else newcg}
              cleartempgen;
{$endif newcg}
              firstpass(p^.t1);

              if p^.registers32<p^.t1^.registers32 then
                p^.registers32:=p^.t1^.registers32;
              if p^.registersfpu<p^.t1^.registersfpu then
                p^.registersfpu:=p^.t1^.registersfpu;
{$ifdef SUPPORT_MMX}
              if p^.registersmmx<p^.t1^.registersmmx then
                p^.registersmmx:=p^.t1^.registersmmx;
{$endif SUPPORT_MMX}
           end;

         { leave if we've got an error in one of the paths }

         if codegenerror then
           exit;

         if p^.left^.treetype=ordconstn then
           begin
              { optimize }
              if p^.left^.value=1 then
                begin
                   disposetree(p^.left);
                   hp:=p^.right;
                   disposetree(p^.t1);
                   { we cannot set p to nil !!! }
                   if assigned(hp) then
                     begin
                        putnode(p);
                        p:=hp;
                     end
                   else
                     begin
                        p^.left:=nil;
                        p^.t1:=nil;
                        p^.treetype:=nothingn;
                     end;
                end
              else
                begin
                   disposetree(p^.left);
                   hp:=p^.t1;
                   disposetree(p^.right);
                   { we cannot set p to nil !!! }
                   if assigned(hp) then
                     begin
                        putnode(p);
                        p:=hp;
                     end
                   else
                     begin
                        p^.left:=nil;
                        p^.right:=nil;
                        p^.treetype:=nothingn;
                     end;
                end;
           end;

         t_times:=old_t_times;
      end;


{*****************************************************************************
                               FirstFor
*****************************************************************************}

    procedure firstfor(var p : ptree);

      var
         old_t_times : longint;
         hp : ptree;
      begin
         { Calc register weight }
         old_t_times:=t_times;
         if not(cs_littlesize in aktglobalswitches) then
           t_times:=t_times*8;
         { save counter var }
         p^.t2:=getcopy(p^.left^.left);

         if p^.left^.treetype<>assignn then
           CGMessage(cg_e_illegal_expression);

{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         firstpass(p^.left);
         set_varstate(p^.left,false);

{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         if assigned(p^.t1) then
          begin
            firstpass(p^.t1);
            if codegenerror then
             exit;
          end;

         p^.registers32:=p^.t1^.registers32;
         p^.registersfpu:=p^.t1^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         if p^.left^.registers32>p^.registers32 then
           p^.registers32:=p^.left^.registers32;
         if p^.left^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.left^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         { process count var }
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         firstpass(p^.t2);
         set_varstate(p^.t2,true);
         if codegenerror then
          exit;

         { Check count var, record fields are also allowed in tp7 }
         hp:=p^.t2;
         while (hp^.treetype=subscriptn) do
          hp:=hp^.left;
         { we need a simple loadn, but the load must be in a global symtable or
           in the same lexlevel }
         if not(hp^.treetype in [loadn,funcretn]) or
            ((hp^.symtable^.symtablelevel>1) and (hp^.symtable^.symtablelevel<>lexlevel)) then
          CGMessagePos(hp^.fileinfo,cg_e_illegal_count_var)
         else
          begin
            if hp^.symtableentry^.typ=varsym then
              pvarsym(hp^.symtableentry)^.varstate:=vs_used;
            if (not(is_ordinal(p^.t2^.resulttype)) or is_64bitint(p^.t2^.resulttype)) then
              CGMessagePos(hp^.fileinfo,type_e_ordinal_expr_expected);
          end;

         if p^.t2^.registers32>p^.registers32 then
           p^.registers32:=p^.t2^.registers32;
         if p^.t2^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.t2^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.t2^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.t2^.registersmmx;
{$endif SUPPORT_MMX}

{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         firstpass(p^.right);
         set_varstate(p^.right,true);
         if p^.right^.treetype<>ordconstn then
           begin
              p^.right:=gentypeconvnode(p^.right,p^.t2^.resulttype);
{$ifdef newcg}
              tg.cleartempgen;
{$else newcg}
              cleartempgen;
{$endif newcg}
              firstpass(p^.right);
           end;

         if p^.right^.registers32>p^.registers32 then
           p^.registers32:=p^.right^.registers32;
         if p^.right^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.right^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.right^.registersmmx;
{$endif SUPPORT_MMX}
         { we need at least one register for comparisons PM }
         if p^.registers32=0 then
           inc(p^.registers32);
         t_times:=old_t_times;
      end;


{*****************************************************************************
                              FirstExit
*****************************************************************************}

    procedure firstexit(var p : ptree);
      var
         pt : ptree;
      begin
         if assigned(p^.left) then
           begin
              firstpass(p^.left);
              procinfo^.funcret_state:=vs_assigned;
              if codegenerror then
               exit;
              { Check the 2 types }
              p^.left:=gentypeconvnode(p^.left,p^.resulttype);
              firstpass(p^.left);
              if ret_in_param(p^.resulttype) or procinfo^.no_fast_exit then
                begin
                  pt:=genzeronode(funcretn);
                  pt^.rettype.setdef(p^.resulttype);
                  pt^.funcretprocinfo:=procinfo;
                  p^.left:=gennode(assignn,pt,p^.left);
                  firstpass(p^.left);
                end;
              p^.registers32:=p^.left^.registers32;
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
           end;
      end;


{*****************************************************************************
                             FirstGoto
*****************************************************************************}

    procedure firstgoto(var p : ptree);
      begin
         p^.resulttype:=voiddef;
      end;


{*****************************************************************************
                             FirstLabel
*****************************************************************************}

    procedure firstlabel(var p : ptree);
      begin
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         p^.exceptionblock:=aktexceptblock;
         firstpass(p^.left);
         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         p^.resulttype:=voiddef;
      end;


{*****************************************************************************
                             FirstRaise
*****************************************************************************}

    procedure firstraise(var p : ptree);
      begin
         p^.resulttype:=voiddef;
         if assigned(p^.left) then
           begin
              { first para must be a _class_ }
              firstpass(p^.left);
              if assigned(p^.left^.resulttype) and
                 ((p^.left^.resulttype^.deftype<>objectdef) or
                  not(pobjectdef(p^.left^.resulttype)^.is_class)) then
                CGMessage(type_e_mismatch);
              set_varstate(p^.left,true);
              if codegenerror then
               exit;
              { insert needed typeconvs for addr,frame }
              if assigned(p^.right) then
               begin
                 { addr }
                 firstpass(p^.right);
                 p^.right:=gentypeconvnode(p^.right,s32bitdef);
                 firstpass(p^.right);
                 if codegenerror then
                  exit;
                 { frame }
                 if assigned(p^.frametree) then
                  begin
                    firstpass(p^.frametree);
                    p^.frametree:=gentypeconvnode(p^.frametree,s32bitdef);
                    firstpass(p^.frametree);
                    if codegenerror then
                     exit;
                  end;
               end;
              left_right_max(p);
           end;
      end;


{*****************************************************************************
                             FirstTryExcept
*****************************************************************************}

    procedure firsttryexcept(var p : ptree);

      var
         oldexceptblock : ptree;

      begin
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         oldexceptblock:=aktexceptblock;
         aktexceptblock:=p^.left;
         firstpass(p^.left);
         aktexceptblock:=oldexceptblock;
         { on statements }
         if assigned(p^.right) then
           begin
{$ifdef newcg}
              tg.cleartempgen;
{$else newcg}
              cleartempgen;
{$endif newcg}
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=p^.right;
              firstpass(p^.right);
              aktexceptblock:=oldexceptblock;
              p^.registers32:=max(p^.registers32,p^.right^.registers32);
              p^.registersfpu:=max(p^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
           end;
         { else block }
         if assigned(p^.t1) then
           begin
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=p^.t1;
              firstpass(p^.t1);
              aktexceptblock:=oldexceptblock;
              p^.registers32:=max(p^.registers32,p^.t1^.registers32);
              p^.registersfpu:=max(p^.registersfpu,p^.t1^.registersfpu);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.registersmmx,p^.t1^.registersmmx);
{$endif SUPPORT_MMX}
           end;
      end;


{*****************************************************************************
                             FirstTryFinally
*****************************************************************************}

    procedure firsttryfinally(var p : ptree);

      var
         oldexceptblock : ptree;

      begin
         p^.resulttype:=voiddef;
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         oldexceptblock:=aktexceptblock;
         aktexceptblock:=p^.left;
         firstpass(p^.left);
         aktexceptblock:=oldexceptblock;
         set_varstate(p^.left,true);
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         oldexceptblock:=aktexceptblock;
         aktexceptblock:=p^.right;
         firstpass(p^.right);
         aktexceptblock:=oldexceptblock;
         set_varstate(p^.right,true);
         if codegenerror then
           exit;
         left_right_max(p);
      end;


{*****************************************************************************
                                 FirstOn
*****************************************************************************}

    procedure firston(var p : ptree);

      var
         oldexceptblock : ptree;

      begin
         { that's really an example procedure for a firstpass :) }
         if (p^.excepttype^.deftype<>objectdef) or
           not(pobjectdef(p^.excepttype)^.is_class) then
           CGMessage(type_e_mismatch);
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         p^.resulttype:=voiddef;
         p^.registers32:=0;
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         if assigned(p^.left) then
           begin
              firstpass(p^.left);
              p^.registers32:=p^.left^.registers32;
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
           end;

{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         if assigned(p^.right) then
           begin
              oldexceptblock:=aktexceptblock;
              aktexceptblock:=p^.right;
              firstpass(p^.right);
              aktexceptblock:=oldexceptblock;
              p^.registers32:=max(p^.registers32,p^.right^.registers32);
              p^.registersfpu:=max(p^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
           end;
      end;


end.
{
  $Log$
  Revision 1.3  2000-08-02 07:04:56  jonas
    * fixed crash when an undeclared identifier is used in a raise statement
      (merged from fixes branch)

  Revision 1.2  2000/07/13 11:32:51  michael
  + removed logs

}
