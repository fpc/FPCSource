{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2011 by the Free Pascal development team

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

Type
  TLibHandle = System.THandle;

Const
  NilHandle = 0;
// these are for easier crossplatform construction of dll names in dynloading libs.
  SharedSuffix = 'nlm';

{$else}

{ ---------------------------------------------------------------------
    Implementation section
  ---------------------------------------------------------------------}

Uses nwserv;

Function DoLoadLibrary(const Name : RawByteString) : TlibHandle;
var args : array[0..1] of PAnsiChar;
begin
  args[0] := PAnsiChar(Name);
  args[1] := nil;
  Result:=spawnvp(P_NOWAIT,@args,nil);
end;

Function GetProcedureAddress(Lib : TLibHandle; const ProcName : AnsiString) : Pointer;

begin
  Result:=ImportSymbol(GetNlmHandle, pchar(ProcName));
end;

Function UnloadLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=false;
end;


Function GetLoadErrorStr: string;

begin  
  Result:='';
end;
{$endif}

