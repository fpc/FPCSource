{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function SysOSAlloc(size: ptrint): pointer;
begin
  result:=Fpmmap(nil,Size,3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0);
  if result=pointer(-1) then
    result:=nil
  else
    seterrno(0);
end;

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptrint);
begin
  fpmunmap(p, size);
end;



{
   $Log: sysheap.inc,v $
   Revision 1.1  2005/02/07 22:04:55  peter
     * moved to unix

   Revision 1.1  2005/02/06 16:57:18  peter
     * threads for go32v2,os,emx,netware

   Revision 1.1  2005/02/06 13:06:20  peter
     * moved file and dir functions to sysfile/sysdir
     * win32 thread in systemunit

}

