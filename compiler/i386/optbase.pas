{
    $Id$
    Copyright (c) 1998-2002 by the Free Pascal development team
    
    This routine contains the basic tables and information
    for the generic optimizers and cpu specific optimizations.

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
{# This unit should define cpu specific information which is required
   for the optimizers.
}   
unit optbase;

interface

uses cpuinfo, cpubase;


{*****************************************************************************
                   Opcode propeties (needed for optimizer)
*****************************************************************************}

{$ifndef NOOPT}
Type
{What an instruction can change}
  TInsChange = (Ch_None,
     {Read from a register}
     Ch_REAX, Ch_RECX, Ch_REDX, Ch_REBX, Ch_RESP, Ch_REBP, Ch_RESI, Ch_REDI,
     {write from a register}
     Ch_WEAX, Ch_WECX, Ch_WEDX, Ch_WEBX, Ch_WESP, Ch_WEBP, Ch_WESI, Ch_WEDI,
     {read and write from/to a register}
     Ch_RWEAX, Ch_RWECX, Ch_RWEDX, Ch_RWEBX, Ch_RWESP, Ch_RWEBP, Ch_RWESI, Ch_RWEDI,
     {modify the contents of a register with the purpose of using
      this changed content afterwards (add/sub/..., but e.g. not rep
      or movsd)}
     Ch_MEAX, Ch_MECX, Ch_MEDX, Ch_MEBX, Ch_MESP, Ch_MEBP, Ch_MESI, Ch_MEDI,
     Ch_CDirFlag {clear direction flag}, Ch_SDirFlag {set dir flag},
     Ch_RFlags, Ch_WFlags, Ch_RWFlags, Ch_FPU,
     Ch_Rop1, Ch_Wop1, Ch_RWop1,Ch_Mop1,
     Ch_Rop2, Ch_Wop2, Ch_RWop2,Ch_Mop2,
     Ch_Rop3, Ch_WOp3, Ch_RWOp3,Ch_Mop3,
     Ch_WMemEDI,
     Ch_All
  );


const
  MaxCh = 3; { Max things a instruction can change }
type
  TInsProp = packed record
    Ch : Array[1..MaxCh] of TInsChange;
  end;

const
  InsProp : array[tasmop] of TInsProp =
{$i i386prop.inc}

{$endif NOOPT}

implementation


end.

{
  $Log$
  Revision 1.1  2002-04-20 21:50:14  carl
  + optimization cpu specific information base file

}  
