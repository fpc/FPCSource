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
      globtype,systems,
      cobjects,verbose,globals,
      symtable,aasm,types,
      hcodegen,htypechk,temp_gen,pass_1,cpubase
{$ifdef i386}
      ,tgeni386
{$endif}
{$ifdef m68k}
      ,tgen68k
{$endif m68k}
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
         if not is_boolean(p^.left^.resulttype) then
          begin
            Message1(type_e_boolean_expr_expected,p^.left^.resulttype^.typename);
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
         hp : ptree;
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
         { save counter var }
         p^.t2:=getcopy(p^.left^.left);

         p^.registers32:=p^.t1^.registers32;
         p^.registersfpu:=p^.t1^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         if p^.left^.treetype<>assignn then
           CGMessage(cg_e_illegal_expression);

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

         { process count var }
         cleartempgen;
         firstpass(p^.t2);
         if codegenerror then
          exit;

         { Check count var, record fields are also allowed in tp7 }
         hp:=p^.t2;
         while (hp^.treetype=subscriptn) do
          hp:=hp^.left;
         if (hp^.treetype<>loadn) then
          CGMessage(cg_e_illegal_count_var)
         else
          if (not(is_ordinal(p^.t2^.resulttype)) or is_64bitint(p^.t2^.resulttype)) then
           CGMessage(type_e_ordinal_expr_expected);

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
         { we need at least one register for comparisons PM }
         if p^.registers32=0 then
           inc(p^.registers32);
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
              procinfo.funcret_is_valid:=true;
              if codegenerror then
               exit;
              { Check the 2 types }
              p^.left:=gentypeconvnode(p^.left,p^.resulttype);
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
                 not(pobjectdef(p^.left^.resulttype)^.is_class) then
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
  Revision 1.19  1999-09-16 23:05:56  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

  Revision 1.18  1999/09/16 10:44:30  pierre
   * firstexit must now set procinfo.funcret_is_valid

  Revision 1.17  1999/08/23 23:41:45  pierre
   * for reg allocation corrected

  Revision 1.16  1999/08/05 16:53:20  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.15  1999/08/04 00:23:39  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.14  1999/08/03 22:03:30  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.13  1999/08/01 18:28:15  florian
    * modifications for the new code generator

  Revision 1.12  1999/06/30 22:16:25  florian
    * use of is_ordinal checked: often a qword/int64 isn't allowed (case/for ...)
    * small qword problems fixed

  Revision 1.11  1999/06/13 22:41:07  peter
    * merged from fixes

  Revision 1.10.2.1  1999/06/13 22:38:54  peter
    * better error message when type is wrong with if statement

  Revision 1.10  1999/05/27 19:45:18  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.9  1999/05/01 13:24:52  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.8  1999/03/24 23:17:36  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.7  1999/03/09 19:24:42  peter
    * type check the exit()

  Revision 1.6  1999/02/22 02:15:48  peter
    * updates for ag386bin

  Revision 1.5  1999/01/13 12:01:43  peter
    * fixed crash with counter var

  Revision 1.4  1998/12/11 00:03:55  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.3  1998/10/19 08:55:10  pierre
    * wrong stabs info corrected once again !!
    + variable vmt offset with vmt field only if required
      implemented now !!!

  Revision 1.2  1998/10/06 20:49:10  peter
    * m68k compiler compiles again

  Revision 1.1  1998/09/23 20:42:24  peter
    * splitted pass_1

}
