{
    $Id$
    Copyright (c) 1998 by Jonas Maebe

    This unit calls the optimization procedures to optimize the assembler
    code for i386+

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
Unit aopt386;

Interface

Uses
  aasm;

Procedure Optimize(AsmL: PAasmOutput);

Implementation

Uses
  globtype,
  globals,i386,DAOpt386,POpt386,CSOpt386;


Procedure Optimize(AsmL: PAasmOutput);
Var BlockEnd: Pai;
Begin
{setup labeltable, always necessary}
  DFAPass1(AsmL);
{peephole optimizations}
  PeepHoleOptPass1(AsmL);
  PeepHoleOptPass1(AsmL);
{data flow analyzer}
  If (cs_slowoptimize in aktglobalswitches) Then
    Begin
      BlockEnd := DFAPass2(AsmL);
      If BlockEnd <> Nil Then
{common subexpression elimination}
        CSE(AsmL, Pai(AsmL^.First), BlockEnd);
    End;
{more peephole optimizations}
  PeepHoleOptPass2(AsmL);
{dispose labeltabel}
  ShutDownDFA;
End;

End.

{
 $Log$
 Revision 1.23  1998-12-11 00:02:43  peter
   + globtype,tokens,version unit splitted from globals

 Revision 1.22  1998/08/19 16:07:57  jonas
   * changed optimizer switches + cleanup of DestroyRefs in daopt386.pas

 Revision 1.21  1998/08/06 19:40:29  jonas
   * removed $ before and after Log in comment

 Revision 1.20  1998/08/05 16:00:08  florian
   * some fixes for ansi strings
   * log to Log changed

}
