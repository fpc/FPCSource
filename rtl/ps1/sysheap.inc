{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Free Pascal development team

    Low level memory functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                              Heap Management
*****************************************************************************}

function SysOSAlloc (size: ptruint): pointer;
begin
  SysOSAlloc:=_malloc(size);
end;

procedure SysOSFree(p: pointer; size: ptruint);
begin
  _free(p);
end;
