{
   $Id$
   This file is part of the Free Pascal run time library.
   (c) 2002 by Marco van de Voort
   members of the Free Pascal development team.
   
   Generic POSIX signal functions draft. Based on a few constants.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

function fpfdaddset(var nset : TFDSet;fdno:cint): cint;

Begin
   if (fdno<=0) or (fdno > FD_MAXFDSET) Then
       exit(-1);
   nset[(fdno-1) shr ln2bitsinword]:=nset[(fdno-1) shr ln2bitsinword] OR (1 shl ((fdno-1) and ln2bitmask));
   fpfdaddset:=0;
End;   

function fpfddelset(var nset : TFDSet;fdno:cint): cint;

Begin
   if (fdno<=0) or (fdno >  FD_MAXFDSET) Then
       exit(-1);
   nset[(fdno-1) shr ln2bitsinword]:=nset[(fdno-1) shr ln2bitsinword] AND NOT (1 shl ((fdno-1) and ln2bitmask));
   fpfddelset:=0;
End;

function fpfdemptyset(var nset : TFDSet):cint;

var i :longint;

Begin
  for i:=0 to wordsinsigset-1 DO nset[i]:=0;
  fpfdemptyset:=0;
End;

function fpfdfillset(var nset : TFDSet):cint;

var i :longint;

Begin
  for i:=0 to wordsinsigset DO nset[i]:=NOT 0;
  fpfdfillset:=0;
End;

function fpfdismember(const nset : TFDSet;fdno:cint): cint;

Begin
   if (fdno<=0) or (fdno >  FD_MAXFDSET) Then
       exit(-1);
    if ((nset[(fdno-1) shr ln2bitsinword]) and (1 shl ((fdno-1) and ln2bitmask)))>0 Then
     fpfdismember:=1
    else 
     fpfdismember:=0;
End;      

{
   $Log$
   Revision 1.1  2003-09-14 20:16:48  marco
    * new files unixreform

   Revision 1.3  2003/06/01 16:28:41  marco
    * Enhancements to make the compiler baseunix using.

   Revision 1.2  2002/12/18 16:50:39  marco
    * Unix RTL generic parts. Linux working, *BSD will follow shortly

   Revision 1.1  2002/11/14 12:20:30  marco
    * initial version, taken from bsdfunc.inc, since both linux and bsd use it


}