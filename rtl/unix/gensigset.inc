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

function sys_sigaddset(var _set : sigset_t;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       seterrno(sys_EINVAL);
       exit(-1);
     End;
   _set[(signo-1) shr ln2bitsinword]:=_set[(signo-1) shr ln2bitsinword] OR (1 shl ((signo-1) and ln2bitmask));
   sys_sigaddset:=0;
End;   

function sys_sigdelset(var _set : sigset_t;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       seterrno(sys_EINVAL);
       exit(-1);
     End;
   _set[(signo-1) shr ln2bitsinword]:=_set[(signo-1) shr ln2bitsinword] AND NOT (1 shl ((signo-1) and ln2bitmask));
   sys_sigdelset:=0;
End;

function sys_sigemptyset(var _set : sigset_t):cint;

var i :longint;

Begin
  for i:=0 to wordsinsigset-1 DO _set[i]:=0;
  sys_sigemptyset:=0;
End;

function sys_sigfillset(var _set : sigset_t):cint;

var i :longint;

Begin
  for i:=0 to wordsinsigset DO _set[i]:=NOT 0;
  sys_sigfillset:=0;
End;

function sys_sigismember(const _set : sigset_t;signo:cint): cint;

Begin
   if (signo<=0) or (signo > SIG_MAXSIG) Then
     Begin
       seterrno(sys_EINVAL);
       exit(-1);
     End;
    if ((_set[(signo-1) shr ln2bitsinword]) and (1 shl ((signo-1) and ln2bitmask)))>0 Then
     sys_sigismember:=1
    else 
     sys_sigismember:=0;
End;      

{
   $Log$
   Revision 1.1  2002-11-14 12:20:30  marco
    * initial version, taken from bsdfunc.inc, since both linux and bsd use it


}