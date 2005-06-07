{
   This file is part of the Free Pascal run time library.
   (c) 2000-2003 by Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   Headers for i386 specific functions on FreeBSD

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{**************************
     Port IO functions
***************************}

Function  IOperm (From,Num : Cardinal; Value : Longint) : boolean;
{$ifndef BSD}
Function  IoPL(Level : longint) : Boolean;
{$endif}

