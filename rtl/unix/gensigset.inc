{
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

function fpsigaddset(var nset : tsigset;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       fpseterrno(ESysEINVAL);
       exit(-1);
     End;
   nset[(signo-1) shr ln2bitsinword]:=nset[(signo-1) shr ln2bitsinword] OR (1 shl ((signo-1) and ln2bitmask));
   fpsigaddset:=0;
End;

function fpsigdelset(var nset : tsigset;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       fpseterrno(ESysEINVAL);
       exit(-1);
     End;
   nset[(signo-1) shr ln2bitsinword]:=nset[(signo-1) shr ln2bitsinword] AND NOT (1 shl ((signo-1) and ln2bitmask));
   fpsigdelset:=0;
End;

function fpsigemptyset(var nset : tsigset):cint;

var i :longint;

Begin
  for i:=0 to wordsinsigset-1 DO nset[i]:=0;
  fpsigemptyset:=0;
End;

function fpsigfillset(var nset : tsigset):cint;

var i :longint;

Begin
  for i:=0 to wordsinsigset-1 DO nset[i]:=high(nset[i]);
  fpsigfillset:=0;
End;

function fpsigismember(const nset : tsigset;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       fpseterrno(ESysEINVAL);
       exit(-1);
     End;
    if ((nset[(signo-1) shr ln2bitsinword]) and (1 shl ((signo-1) and ln2bitmask)))>0 Then
     fpsigismember:=1
    else
     fpsigismember:=0;
End;

