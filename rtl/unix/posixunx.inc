{
    $Id$
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

Function sys_getenv(const name:pchar):pchar;

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
End;

Function sys_getenv(name:string):Pchar;
{
  Searches the environment for a string with name p and
  returns a pchar to it's value.
  A pchar is used to accomodate for strings of length > 255
}


Begin
  name:=name+'=';            {Else HOST will also find HOSTNAME, etc}
  sys_getenv:=sys_getenv(@name[1]);
end;

{
  $Log$
  Revision 1.1  2002-10-27 13:16:54  marco
   * Routines that certainly will be shared between Linux and *BSD


}