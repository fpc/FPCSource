{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001-2005 by Free Pascal development team

    Low level heap functions for MacOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
      OS Memory allocation / deallocation
 ****************************************************************************}

{ function to allocate size bytes more for the program }
{ must return the first address of new data space or nil if failed }
function SysOSAlloc(size: ptrint): pointer;
begin
  result := NewPtr(size);
end;

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptrint);
begin
  DisposePtr(p);
end;



{
   $Log: sysheap.inc,v $
   Revision 1.2  2005/05/14 11:52:37  olle
     * Updated header comments

   Revision 1.1  2005/02/07 21:30:12  peter
     * system unit updated

   Revision 1.1  2005/02/06 16:57:18  peter
     * threads for go32v2,os,emx,netware

   Revision 1.1  2005/02/06 13:06:20  peter
     * moved file and dir functions to sysfile/sysdir
     * win32 thread in systemunit

}

