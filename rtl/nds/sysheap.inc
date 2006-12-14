{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Free Pascal development team

    Low level memory functions

    Heap functions unit for Nintendo DS
    Copyright (c) 2006 by Francesco Lombardi

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
  result := nil;//pointer($02000000);
end;

{ $define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptrint);
begin
  
end;
