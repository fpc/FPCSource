{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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


implementation

    uses
      cobjects,verbose,globals,systems,
      symtable,aasm,types,
      hcodegen,htypechk,temp_gen,pass_1
{$ifdef i386}
      ,i386,tgeni386
{$endif}
{$ifdef m68k}
      ,m68k,tgeni386
{$endif}
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

         cleartempgen;
         must_be_valid:=true;
         firstpass(p^.left);
         if codegenerror then
           exit;
         if not((p^.left^.resulttype^.deftype=orddef) and
            (porddef(p^.left^.resulttype)^.typ in [bool8bit,bool16bit,bool32bit])) then
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
              cleartempgen;
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
         cleartempgen;
         must_be_valid:=true;
         firstpass(p^.left);
         if codegenerror then
           exit;
         if not((p^.left^.resulttype^.deftype=orddef) and
            (porddef(p^.left^.resulttype)^.typ in [bool8bit,bool16bit,bool32bit])) then
            begin
               CGMessage(type_e_mismatch);
               exit;
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
              cleartempgen;
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

         { else path }
         if assigned(p^.t1) then
           begin
              cleartempgen;
              firstpass(p^.t1);
              if codegenerror then
                exit;

              if p^.registers32<p^.t1^.registers32 then
                p^.registers32:=p^.t1^.registers32;
              if p^.registersfpu<p^.t1^.registersfpu then
                p^.registersfpu:=p^.t1^.registersfpu;
{$ifdef SUPPORT_MMX}
              if p^.registersmmx<p^.t1^.registersmmx then
                p^.registersmmx:=p^.t1^.registersmmx;
{$endif SUPPORT_MMX}
           end;
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

      begin
         { Calc register weight }
         old_t_times:=t_times;
         if not(cs_littlesize in aktglobalswitches) then
           t_times:=t_times*8;

         cleartempgen;
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

         if p^.left^.treetype<>assignn then
           CGMessage(cg_e_illegal_expression);

         { Laufvariable retten }
         p^.t2:=getcopy(p^.left^.left);

         { Check count var }
         if (p^.t2^.treetype<>loadn) then
          CGMessage(cg_e_illegal_count_var);

         if (not(is_ordinal(p^.t2^.resulttype))) then
          CGMessage(type_e_ordinal_expr_expected);

         cleartempgen;
         must_be_valid:=false;
         firstpass(p^.left);
         must_be_valid:=true;
         if p^.left^.registers32>p^.registers32 then
           p^.registers32:=p^.left^.registers32;
         if p^.left^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.left^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         cleartempgen;
         firstpass(p^.t2);
         if p^.t2^.registers32>p^.registers32 then
           p^.registers32:=p^.t2^.registers32;
         if p^.t2^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.t2^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.t2^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.t2^.registersmmx;
{$endif SUPPORT_MMX}

         cleartempgen;
         firstpass(p^.right);
         if p^.right^.treetype<>ordconstn then
           begin
              p^.right:=gentypeconvnode(p^.right,p^.t2^.resulttype);
              cleartempgen;
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
         t_times:=old_t_times;
      end;


{*****************************************************************************
                              FirstExit
*****************************************************************************}

    procedure firstexit(var p : ptree);
      begin
         if assigned(p^.left) then
           begin
              firstpass(p^.left);
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
         cleartempgen;
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
         {
         p^.registersfpu:=0;
         p^.registers32:=0;
         }
         if assigned(p^.left) then
           begin
              firstpass(p^.left);

              { this must be a _class_ }
              if (p^.left^.resulttype^.deftype<>objectdef) or
                ((pobjectdef(p^.left^.resulttype)^.options and oois_class)=0) then
                CGMessage(type_e_mismatch);

              p^.registersfpu:=p^.left^.registersfpu;
              p^.registers32:=p^.left^.registers32;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
              if assigned(p^.right) then
                begin
                   firstpass(p^.right);
                   p^.right:=gentypeconvnode(p^.right,s32bitdef);
                   firstpass(p^.right);
                   left_right_max(p);
                end;
           end;
      end;


{*****************************************************************************
                             FirstTryExcept
*****************************************************************************}

    procedure firsttryexcept(var p : ptree);
      begin
         cleartempgen;
         firstpass(p^.left);

         { on statements }
         if assigned(p^.right) then
           begin
              cleartempgen;
              firstpass(p^.right);
              p^.registers32:=max(p^.registers32,p^.right^.registers32);
              p^.registersfpu:=max(p^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
           end;
         { else block }
         if assigned(p^.t1) then
           begin
              firstpass(p^.t1);
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
      begin
         p^.resulttype:=voiddef;
         cleartempgen;
         must_be_valid:=true;
         firstpass(p^.left);

         cleartempgen;
         must_be_valid:=true;
         firstpass(p^.right);
         if codegenerror then
           exit;
         left_right_max(p);
      end;


{*****************************************************************************
                                 FirstOn
*****************************************************************************}

    procedure firston(var p : ptree);
      begin
         { that's really an example procedure for a firstpass :) }
         cleartempgen;
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

         cleartempgen;
         if assigned(p^.right) then
           begin
              firstpass(p^.right);
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
  Revision 1.1  1998-09-23 20:42:24  peter
    * splitted pass_1

}

