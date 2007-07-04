{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by contributors of the Free Pascal Compiler

    Basic heap handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
      OS Memory allocation / deallocation
 ****************************************************************************}

 
function SysOSAlloc(size: ptruint): pointer;
begin

end;

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptruint);
begin

end;
