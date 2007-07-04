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
function SysOSAlloc(size: ptruint): pointer;
begin
  result := NewPtr(size);
end;

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptruint);
begin
  DisposePtr(p);
end;

