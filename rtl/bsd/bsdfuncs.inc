{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    An *BSD implementation of Uname and in the future some more
     calls.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


function sys_uname(var name:utsname):cint;
Var
        mib  : array[0..1] of cint;
        rval : cint;
        len  : size_t;
        i    : longint;
        oerrno : cint;

procedure Doone(pz:pchar;pzsize:cint;val1,val2:cint);

Begin
        mib[0] := val1;
        mib[1] := val2;
        len    := pzsize;
        oerrno := geterrno;

        if (sys_sysctl(@mib, 2, pz, @len, NIL, 0) = -1) Then
           Begin
                if (geterrno = sys_ENOMEM) Then
                        seterrno(oerrno)
                else
                        rval := -1;
           End;
         pz[pzsize- 1] := #0;
End;

Begin
        rval := 0;
        DoOne(@name.sysname,sizeof(name.sysname),CTL_KERN,KERN_OSTYPE);
        DoOne(@name.nodename,sizeof(name.nodename),CTL_KERN,KERN_HOSTNAME);
        DoOne(@name.release,sizeof(name.release),CTL_KERN,KERN_OSRELEASE);
        { The version may have newlines in it, turn them into spaces. }
        DoOne(@name.version,sizeof(name.version),CTL_KERN,KERN_VERSION);

        For I:=0 to sizeof(name.sysname)-2 Do
          If (name.version[i]=#13) or (name.version[i]=#9) Then
            name.version[i]:=' ';
        DoOne(@name.machine,sizeof(name.machine),CTL_HW,HW_MACHINE);
        sys_Uname:=rval;
end;

function GetDomainName(Name:PChar; NameLen:Cint):cint;

Const Mib_GetDomainname : array[0..1] of cint=(CTL_KERN,KERN_NISDOMAINNAME);

VAR
	tsize : size_t;
begin
	tsize := namelen;
	if (sys_sysctl(@Mib_GetDomainname, 2, name, @tsize, NIL, 0) = -1) Then
          GetDomainName:=-1
        Else
          GetDomainName:=0;
end;          

function GetHostName(Name:PChar; NameLen:Cint):cint;

Const Mib_GetHostName : array[0..1] of cint=(CTL_KERN,KERN_HOSTNAME);

Var
	tsize : size_t;
begin
	tsize := namelen;
	if (sys_sysctl(@Mib_GetHostName, 2, name, @tsize, NIL, 0) = -1) Then
	  GetHostName:=-1
	Else
	  GetHostName:=0;
End;

{
 $Log$
 Revision 1.3  2002-09-08 16:11:59  marco
  * Added GetDomainName and that other one ..

 Revision 1.2  2002/09/07 16:01:17  peter
   * old logs removed and tabs fixed

 Revision 1.1  2002/08/21 07:03:16  marco
  * Fixes from Tuesday.

 Revision 1.1  2002/08/08 11:39:30  marco
  * Initial versions, to allow support for uname in posix.pp



}



