 {
    $Id: aoptcpub.pas,v 1.8 2005/02/26 01:27:00 jonas Exp $
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains several types and constants necessary for the
    optimizer to work on the 80x86 architecture

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
Unit aoptcpub; { Assembler OPTimizer CPU specific Base }

{$i fpcdefs.inc}

{ enable the following define if memory references can have both a base and }
{ index register in 1 operand                                               }

{$define RefsHaveIndexReg}

{ enable the following define if memory references can have a scaled index }

{ define RefsHaveScale}

{ enable the following define if memory references can have a segment }
{ override                                                            }

{ define RefsHaveSegment}

Interface

Uses
  aasmcpu,AOptBase, cpubase;

Type

{ type of a normal instruction }
  TInstr = Taicpu;
  PInstr = ^TInstr;

{ ************************************************************************* }
{ **************************** TCondRegs ********************************** }
{ ************************************************************************* }
{ Info about the conditional registers                                      }
  TCondRegs = Object
    Constructor Init;
    Destructor Done;
  End;

{ ************************************************************************* }
{ **************************** TAoptBaseCpu ******************************* }
{ ************************************************************************* }

  TAoptBaseCpu = class(TAoptBase)
  End;


{ ************************************************************************* }
{ ******************************* Constants ******************************* }
{ ************************************************************************* }
Const

{ the maximum number of things (registers, memory, ...) a single instruction }
{ changes                                                                    }

  MaxCh = 3;

{ the maximum number of operands an instruction has }

  MaxOps = 5;

{Oper index of operand that contains the source (reference) with a load }
{instruction                                                            }

  LoadSrc = 1;

{Oper index of operand that contains the destination (register) with a load }
{instruction                                                                }

  LoadDst = 0;

{Oper index of operand that contains the source (register) with a store }
{instruction                                                            }

  StoreSrc = 0;

{Oper index of operand that contains the destination (reference) with a load }
{instruction                                                                 }

  StoreDst = 1;


  aopt_uncondjmp = A_B;
  aopt_condjmp = A_BC;

Implementation

{ ************************************************************************* }
{ **************************** TCondRegs ********************************** }
{ ************************************************************************* }
Constructor TCondRegs.init;
Begin
End;

Destructor TCondRegs.Done; {$ifdef inl} inline; {$endif inl}
Begin
End;

End.

{
 $Log: aoptcpub.pas,v $
 Revision 1.8  2005/02/26 01:27:00  jonas
   * fixed generic jumps optimizer and enabled it for ppc (the label table
     was not being initialised -> getfinaldestination always failed, which
     caused wrong optimizations in some cases)
   * changed the inverse_cond into a function, because tasmcond is a record
     on ppc
   + added a compare_conditions() function for the same reason

 Revision 1.7  2005/02/14 17:13:10  peter
   * truncate log

}
