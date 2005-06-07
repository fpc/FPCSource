{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implements OS dependent part for loading of dynamic libraries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$ifdef readinterface}

{ ---------------------------------------------------------------------
    Interface declarations
  ---------------------------------------------------------------------}

type
 TLibHandle = longint;

const
 NilHandle = 0;

{$else}

{ ---------------------------------------------------------------------
    Implementation section
  ---------------------------------------------------------------------}

uses
 DosCalls;

function LoadLibrary (Name: AnsiString): TLibHandle;
var
 ErrPath: array [0..259] of char;
 Handle: longint;
begin
 if DosLoadModule (@ErrPath, SizeOf (ErrPath), PChar (Name), Handle) = 0
                                then Result := Handle else Result := NilHandle;
end;

function GetProcedureAddress (Lib: TLibHandle; ProcName: AnsiString): pointer;
var
 P: pointer;
begin
 if DosQueryProcAddr (Lib, 0, PChar (ProcName), P) = 0 then Result := P
                                                            else Result := nil;
end;

function UnloadLibrary (Lib: TLibHandle): boolean;
begin
 Result := DosFreeModule (Lib) = 0;
end;

{$endif}

