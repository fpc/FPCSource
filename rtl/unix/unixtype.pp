{
   This file is part of the Free Pascal run time library.
   (c) 2004 by Marco van de Voort
   member of the Free Pascal development team.

   THIS UNIT IS NOT FOR USE BY ENDUSERS. IT IS USED TO AVOID CERTAIN
   CIRCULAR REFERENCE PROBLEMS.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
unit unixtype;

Interface

{$i ptypes.inc}

Type
  TTime = time_t;
Implementation

End.
