{
    $Id: sysheap.inc,v 1.1 2005/02/07 21:30:12 peter Exp $
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

{*****************************************************************************
      OS Memory allocation / deallocation
 ****************************************************************************}

function SysOSAlloc(size: ptrint): pointer;
begin
  result:=AllocPooled(MOS_heapPool,size);
end;

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptrint);
begin
  FreePooled(MOS_heapPool,p,size);
end;


{
   $Log: sysheap.inc,v $
   Revision 1.1  2005/02/07 21:30:12  peter
     * system unit updated

   Revision 1.1  2005/02/06 16:57:18  peter
     * threads for go32v2,os,emx,netware

   Revision 1.1  2005/02/06 13:06:20  peter
     * moved file and dir functions to sysfile/sysdir
     * win32 thread in systemunit

}

