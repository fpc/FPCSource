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

Uses aasm;

Procedure Optimize(AsmL: PAasmOutput);

Implementation

Uses i386, DAOpt386, POpt386, CSOpt386;

Procedure Optimize(AsmL: PAasmOutput);
Var BlockEnd: Pai;
Begin
  DFAPass1(AsmL);
  PeepHoleOptPass1(AsmL);
  PeepHoleOptPass1(AsmL);
  BlockEnd := DFAPass2(AsmL);
  If BlockEnd <> Nil Then
    CSE(AsmL, Pai(AsmL^.First), BlockEnd);
  PeepHoleOptPass2(AsmL);
  ShutDownDFA;
End;

End.

{
 $log $
}
