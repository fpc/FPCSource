{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Carl-Eric Codere,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef FPC_UNIT_HAS_STRPCOPY}
   function strpcopy(d : pchar;const s : string) : pchar;
   var
    counter : byte;
  Begin
    counter := 0;
   { if empty pascal string  }
   { then setup and exit now }
   if s = '' then
   Begin
     D[0] := #0;
     StrPCopy := D;
     exit;
   end;
   for counter:=1 to length(S) do
     D[counter-1] := S[counter];
   { terminate the string }
   D[counter] := #0;
   StrPCopy:=D;
 end;
{$endif FPC_UNIT_HAS_STRPCOPY}

{
  $Log$
  Revision 1.2  2004-11-21 15:35:23  peter
    * float routines all use internproc and compilerproc helpers

  Revision 1.1  2003/07/07 20:22:05  peter
    * generic string routines added

}
