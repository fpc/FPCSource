{
    $Id$
    Copyright (c) 1996-98 by Florian Klaempfl

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
{$ifdef tp}
  {$F+}
{$endif tp}
unit pass_1;
interface

    uses
       tree;

    procedure firstpass(p : pnode);
    function  do_firstpass(var p : ptree) : boolean;
    function  do_firstpassnode(var p : pnode) : boolean;

implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      aasm,symtable,types,
      cgbase
      { not yet converted:
      htypechk,tcadd,tccal,tccnv,tccon,tcflw,
      tcinl,tcld,tcmat,tcmem,tcset
      }
{$ifdef i386}
      ,i386,tgeni386
{$endif}
{$ifdef m68k}
      ,m68k,tgen68k
{$endif}
      ;

{*****************************************************************************
                              FirstPass
*****************************************************************************}

{$ifdef dummy}
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
         cleartempgen;
         { right is the statement itself calln assignn or a complex one }
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
                   if ret_in_acc(procinfo.retdef) and
                      assigned(hp^.left) and
                      (hp^.left^.right^.treetype=exitn) and
                      (hp^.right^.treetype=assignn) and
                      (hp^.right^.left^.treetype=funcretn) then
                      begin
                         if assigned(hp^.left^.right^.left) then
                           CGMessage(cg_n_inefficient_code)
                         else
                           begin
                              hp^.left^.right^.left:=getcopy(hp^.right^.right);
                              disposetree(hp^.right);
                              hp^.right:=nil;
                           end;
                      end
                   { warning if unreachable code occurs and elimate this }
                   else if (hp^.right^.treetype in
                     [exitn,breakn,continuen,goton]) and
                     assigned(hp^.left) and
                     (hp^.left^.treetype<>labeln) then
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
                   cleartempgen;
                   firstpass(hp^.right);
                   if (not (cs_extsyntax in aktmoduleswitches)) and
                      assigned(hp^.right^.resulttype) and
                      (hp^.right^.resulttype<>pdef(voiddef)) then
                     CGMessage(cg_e_illegal_expression);
                   if codegenerror then
                     exit;
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
        procinfo.flags:=procinfo.flags or pi_uses_asm;
      end;

{$endif dummy}

    procedure firstpass(p : pnode);

      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos           : tfileposinfo;
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
       {$ifndef TP}
         {$ifopt H+}
           SetLength(str1,sizeof(ttree));
         {$else}
              str1[0]:=char(sizeof(ttree));
         {$endif}
       {$else}
              str1[0]:=char(sizeof(ttree));
       {$endif}
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
              p^.pass_1;
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
       {$ifndef TP}
         {$ifopt H+}
           SetLength(str2,sizeof(ttree));
         {$else}
              str2[0]:=char(sizeof(ttree));
         {$endif}
       {$else}
              str2[0]:=char(sizeof(ttree));
       {$endif}
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
         codegenerror:=false;
         do_firstpass:=codegenerror;
      end;

    function do_firstpassnode(var p : pnode) : boolean;

      begin
         codegenerror:=false;
         firstpass(p);
         do_firstpassnode:=codegenerror;
      end;


end.
{
  $Log$
  Revision 1.1  1998-12-26 15:20:31  florian
    + more changes for the new version

}
