{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    An *BSD implementation of Uname based on the SYSCTL call.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


function uname(var name:utsname):cint;
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
        oerrno := errno;
        if (sysctl(@mib, 2, pz, @len, NIL, 0) = -1) Then
           Begin
                if (errno = sys_ENOMEM) Then
                        errno := oerrno
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
        Uname:=rval;
end;

{
 $Log$
 Revision 1.2  2002-09-07 16:01:17  peter
   * old logs removed and tabs fixed

 Revision 1.1  2002/08/08 11:39:30  marco
  * Initial versions, to allow support for uname in posix.pp



}



