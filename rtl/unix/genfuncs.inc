{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Marco van de Voort.

    A few general purpose routines. General purpose enough for *BSD
     and Linux at least.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function InternalCreateShellArgV(cmd:pChar; len:longint):ppchar;
{
  Create an argv which executes a command in a shell using /bin/sh -c
}
const   Shell   = '/bin/sh'#0'-c'#0;
var
  pp,p : ppchar;
//  temp : string; !! Never pass a local var back!!
begin
  getmem(pp,4*sizeof(pointer));
  p:=pp;
  p^:=@Shell[1];
  inc(p);
  p^:=@Shell[9];
  inc(p);
  getmem(p^,len+1);
  move(cmd^,p^^,len);
  pchar(p^)[len]:=#0;
  inc(p);
  p^:=Nil;
  InternalCreateShellArgV:=pp;
end;

function CreateShellArgV(const prog:string):ppchar;
begin
  CreateShellArgV:=InternalCreateShellArgV(@prog[1],length(prog));
end;

function CreateShellArgV(const prog:Ansistring):ppchar;
{
  Create an argv which executes a command in a shell using /bin/sh -c
  using a AnsiString;
}
begin
  CreateShellArgV:=InternalCreateShellArgV(@prog[1],length(prog)); // if ppc works like delphi this also work when @prog[1] is invalid (len=0)
end;


procedure FreeShellArgV(p:ppchar);
begin
  if (p<>nil) then begin
    freemem(p[2]);
    freemem(p);
   end;
end;

{$ifndef FPC_USE_LIBC}
Function fpgetenv(name:pchar):pchar;

var
  p     : ppchar;
  found : boolean;
  np,cp : pchar;
  len,i : longint;
Begin
  if (name=nil) or (envp=NIL) Then
   exit(NIL);
  np:=name;
  while (np^<>#0) and (np^<>'=') DO
   inc(np);
  len:=np-name;
  p:=envp;
  while (p^<>NIL) DO
   Begin
    cp:=p^;
    np:=name;
    i:=len;
    while (i<>0) and (cp^<>#0) DO
      Begin
        if cp^<>np^ Then
          Begin
           inc(cp); inc(np);
           break;
         End;
        inc(cp); inc(np);
        dec(i)
      End;
    if (i=0) and (cp^='=') Then
        exit(cp+1);
   inc(p);
  end;
 fpgetenv:=nil;
End;
{$ENDIF}

Function fpgetenv(name:string):Pchar; [public, alias : 'FPC_SYSC_FPGETENV'];
{
  Searches the environment for a string with name p and
  returns a pchar to it's value.
  A pchar is used to accomodate for strings of length > 255
}

Begin
{$ifndef FPC_USE_LIBC}
  name:=name+'=';            {Else HOST will also find HOSTNAME, etc}
{$else}
  name:=name+#0;
{$endif}
  fpgetenv:=fpgetenv(@name[1]);
end;

