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
{$ifdef tp}
  {$F+}
{$endif tp}
unit pass_1;
interface

    uses
       tree;

    procedure firstpass(p : ptree);
    procedure firstpassnode(p : pnode);
    function  do_firstpass(var p : ptree) : boolean;
    function  do_firstpassnode(var p : pnode) : boolean;

implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      aasm,symtable,types,
      cgbase,cpuasm,cpubase
      { not yet converted:
      htypechk,tcadd,tccal,tccnv,tccon,tcflw,
      tcinl,tcld,tcmat,tcmem,tcset
      }
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


    procedure firstasm(var p : ptree);

      begin
        procinfo.flags:=procinfo^.flags or pi_uses_asm;
      end;

{$endif dummy}

    procedure firstpassnode(p : pnode);

      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos           : tfileposinfo;
{$ifdef extdebug}
         str1,str2 : string;
         oldp      : pnode;
         not_first : boolean;
{$endif extdebug}
      begin
         oldcodegenerror:=codegenerror;
         oldpos:=aktfilepos;
         oldlocalswitches:=aktlocalswitches;

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
      end;


    function do_firstpass(var p : ptree) : boolean;

      begin
         codegenerror:=false;
         do_firstpass:=codegenerror;
      end;

    procedure firstpass(p : ptree);

      begin
         codegenerror:=false;
      end;

    function do_firstpassnode(var p : pnode) : boolean;

      begin
         codegenerror:=false;
         firstpassnode(p);
         do_firstpassnode:=codegenerror;
      end;


end.
{
  $Log$
  Revision 1.7  2000-01-07 01:14:54  peter
    * updated copyright to 2000

  Revision 1.6  1999/10/12 21:20:47  florian
    * new codegenerator compiles again

  Revision 1.5  1999/08/04 00:23:57  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.4  1999/08/01 18:22:36  florian
   * made it again compilable

  Revision 1.3  1999/01/23 23:29:48  florian
    * first running version of the new code generator
    * when compiling exceptions under Linux fixed

  Revision 1.2  1999/01/13 22:52:37  florian
    + YES, finally the new code generator is compilable, but it doesn't run yet :(

  Revision 1.1  1998/12/26 15:20:31  florian
    + more changes for the new version

}
