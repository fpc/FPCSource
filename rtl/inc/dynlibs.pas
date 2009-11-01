{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implements OS-independent loading of dynamic libraries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFDEF FPC}
{$MODE OBJFPC}
{$ENDIF}

unit dynlibs;

interface

uses
  SysUtils, RtlConsts, SysConst;

{ ---------------------------------------------------------------------
  Read OS-dependent interface declarations.
  ---------------------------------------------------------------------}

{$define readinterface}
{$i dynlibs.inc}
{$undef  readinterface}

{ ---------------------------------------------------------------------
  OS - Independent declarations.
  ---------------------------------------------------------------------}


Function SafeLoadLibrary(Name : AnsiString) : TLibHandle;
Function LoadLibrary(Name : AnsiString) : TLibHandle;
Function GetProcedureAddress(Lib : TlibHandle; ProcName : AnsiString) : Pointer;
Function UnloadLibrary(Lib : TLibHandle) : Boolean;


// Kylix/Delphi compability

Type
  HModule = TLibHandle;

Function FreeLibrary(Lib : TLibHandle) : Boolean;
Function GetProcAddress(Lib : TlibHandle; ProcName : AnsiString) : Pointer;


// Dynamic Library Manager

{ Note: If you look for some code that uses this library handler, take a look at
    sqlite3.inc of sqlite package (simple) or
    mysql.inc of mysql package (advanced)
}

type
  PLibHandler = ^TLibHandler;

  TLibEventLoading = function(User: Pointer; Handler: PLibHandler): Boolean;
  TLibEventUnloading = procedure(Handler: PLibHandler);

  PPLibSymbol = ^PLibSymbol;
  PLibSymbol = ^TLibSymbol;
  TLibSymbol = record
    pvar: PPointer;  { pointer to Symbol variable }
    name: String;    { name of the Symbol }
    weak: Boolean;   { weak }
  end;

  TLibHandler = record
    InterfaceName: String;                { abstract name of the library }
    Defaults     : array of String;       { list of default library filenames }
    Filename     : String;                { handle of the current loaded library }
    Handle       : TLibHandle;            { filename of the current loaded library }
    Loading      : TLibEventLoading;      { loading event, called after the unit is loaded }
    Unloading    : TLibEventUnloading;    { unloading event, called before the unit is unloaded }
    SymCount     : Integer;               { number of symbols }
    Symbols      : PLibSymbol;            { symbol address- and namelist }
    ErrorMsg     : String;                { last error message }
    RefCount     : Integer;               { reference counter }
  end;


{ handler definition }
function LibraryHandler(const InterfaceName: String; const DefaultLibraries: array of String;
  const Symbols: PLibSymbol; const SymCount: Integer; const AfterLoading: TLibEventLoading = nil;
  const BeforeUnloading: TLibEventUnloading = nil): TLibHandler;

{ initialization/finalization }
function TryInitializeLibrary(var Handler: TLibHandler; const LibraryNames: array of String;
  const User: Pointer = nil; const NoSymbolErrors: Boolean = False): Integer;
function TryInitializeLibrary(var Handler: TLibHandler; const LibraryName: String = '';
  const User: Pointer = nil; const NoSymbolErrors: Boolean = False): Integer;
function InitializeLibrary(var Handler: TLibHandler; const LibraryNames: array of String;
  const User: Pointer = nil; const NoSymbolErrors: Boolean = False): Integer;
function InitializeLibrary(var Handler: TLibHandler; const LibraryName: String = '';
  const User: Pointer = nil; const NoSymbolErrors: Boolean = False): Integer;
function ReleaseLibrary(var Handler: TLibHandler): Integer;

{ errors }
procedure AppendLibraryError(var Handler: TLibHandler; const Msg: String);
function GetLastLibraryError(var Handler: TLibHandler): String;
procedure RaiseLibraryException(var Handler: TLibHandler);

{ symbol load/clear }
function LoadLibrarySymbols(const Lib: TLibHandle; const Symbols: PLibSymbol; const Count: Integer;
  const ErrorSym: PPLibSymbol = nil): Boolean;
procedure ClearLibrarySymbols(const Symbols: PLibSymbol; const Count: Integer);


// these are for easier crossplatform construction of dll names in dynloading libs.
Const
 {$ifdef Windows}
  SharedSuffix  = 'dll';
 {$else}
   {$ifdef Darwin}
     SharedSuffix = 'dylib';
   {$else}
     {$ifdef OS2}
       SharedSuffix = 'dll';
     {$else}
       SharedSuffix = 'so';
     {$endif}
   {$endif}
 {$endif}

Implementation

{ ---------------------------------------------------------------------
  OS - Independent declarations.
  ---------------------------------------------------------------------}

{$i dynlibs.inc}

Function FreeLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=UnloadLibrary(lib);
end;

Function GetProcAddress(Lib : TlibHandle; ProcName : AnsiString) : Pointer;

begin
  Result:=GetProcedureAddress(Lib,Procname);
end;

Function SafeLoadLibrary(Name : AnsiString) : TLibHandle;

{$ifdef i386}
 var w : word;
{$endif}

Begin
{$ifdef i386}
  w:=get8087cw;
{$endif}
 result:=loadlibrary(name);

{$ifdef i386}
  set8087cw(w);
{$endif}
End;

function LibraryHandler(const InterfaceName: String; const DefaultLibraries: array of String;
  const Symbols: PLibSymbol; const SymCount: Integer; const AfterLoading: TLibEventLoading;
  const BeforeUnloading: TLibEventUnloading): TLibHandler;
var
  I: Integer;
begin
  Result.InterfaceName := InterfaceName;
  Result.Filename      := '';
  Result.Handle        := NilHandle;
  Result.Loading       := AfterLoading;
  Result.Unloading     := BeforeUnloading;
  Result.SymCount      := SymCount;
  Result.Symbols       := Symbols;
  Result.ErrorMsg      := '';
  Result.RefCount      := 0;

  SetLength(Result.Defaults, Length(DefaultLibraries));
  for I := 0 to High(DefaultLibraries) do
    Result.Defaults[I] := DefaultLibraries[I];
end;

function TryInitializeLibraryInternal(var Handler: TLibHandler; const LibraryName: String;
  const User: Pointer; const NoSymbolErrors: Boolean): Integer;
var
  ErrSym: PLibSymbol;
begin
  if (Handler.Filename <> '') and (Handler.Filename <> LibraryName) then
  begin
    AppendLibraryError(Handler, Format(SLibraryAlreadyLoaded, [Handler.InterfaceName, Handler.Filename]));
    Result := -1;
    Exit;
  end;

  Result := InterlockedIncrement(Handler.RefCount);
  if Result = 1 then
  begin
    Handler.Handle := LoadLibrary(LibraryName);
    if Handler.Handle = NilHandle then
    begin
      AppendLibraryError(Handler, Format(SLibraryNotLoaded, [Handler.InterfaceName, LibraryName]));
      Handler.RefCount := 0;
      Result := -1;
      Exit;
    end;

    Handler.Filename := LibraryName;

    if not LoadLibrarySymbols(Handler.Handle, Handler.Symbols, Handler.SymCount, @ErrSym) and not NoSymbolErrors then
    begin
      AppendLibraryError(Handler, Format(SLibraryUnknownSym, [ErrSym^.name, Handler.InterfaceName, LibraryName]));
      UnloadLibrary(Handler.Handle);
      Handler.Handle := NilHandle;
      Handler.Filename := '';
      Handler.RefCount := 0;
      Result := -1;
      Exit;
    end;

    if Assigned(Handler.Loading) and not Handler.Loading(User, @Handler) then
    begin
      UnloadLibrary(Handler.Handle);
      Handler.Handle := NilHandle;
      Handler.Filename := '';
      Handler.RefCount := 0;
      Result := -1;
      Exit;
    end;
  end;
end;

function TryInitializeLibrary(var Handler: TLibHandler; const LibraryName: String;
  const User: Pointer; const NoSymbolErrors: Boolean): Integer;
begin
  if LibraryName <> '' then
  begin
    Handler.ErrorMsg := '';
    Result := TryInitializeLibraryInternal(Handler, LibraryName, User, NoSymbolErrors);
  end else
    Result := TryInitializeLibrary(Handler, Handler.Defaults, User, NoSymbolErrors);
end;

function TryInitializeLibrary(var Handler: TLibHandler; const LibraryNames: array of String;
  const User: Pointer; const NoSymbolErrors: Boolean): Integer;
var
  I: Integer;
begin
  Handler.ErrorMsg := '';

  if Length(LibraryNames) <= 0 then
  begin
    if Length(Handler.Defaults) > 0 then
    begin
      Result := TryInitializeLibrary(Handler, Handler.Defaults, User, NoSymbolErrors);
      Exit;
    end;

    AppendLibraryError(Handler, SVarInvalid);
    Result := -1;
    Exit;
  end;

  for I := 0 to High(LibraryNames) do
  begin
    Result := TryInitializeLibraryInternal(Handler, LibraryNames[I], User, NoSymbolErrors);
    if Result > 0 then
    begin
      Handler.ErrorMsg := '';
      Exit;
    end;
  end;
end;

function InitializeLibrary(var Handler: TLibHandler; const LibraryNames: array of String;
  const User: Pointer; const NoSymbolErrors: Boolean): Integer;
begin
  Result := TryInitializeLibrary(Handler, LibraryNames, User, NoSymbolErrors);
  if Result < 0 then
    RaiseLibraryException(Handler);
end;

function InitializeLibrary(var Handler: TLibHandler; const LibraryName: String;
  const User: Pointer; const NoSymbolErrors: Boolean): Integer;
begin
  Result := TryInitializeLibrary(Handler, LibraryName, User, NoSymbolErrors);
  if Result < 0 then
    RaiseLibraryException(Handler);
end;

function ReleaseLibrary(var Handler: TLibHandler): Integer;
begin
  Handler.ErrorMsg := '';

  Result := InterlockedDecrement(Handler.RefCount);
  if Result = 0 then
  begin
    if Assigned(Handler.Unloading) then
      Handler.Unloading(@Handler);
    ClearLibrarySymbols(Handler.Symbols, Handler.SymCount);
    UnloadLibrary(Handler.Handle);
    Handler.Handle := NilHandle;
    Handler.Filename := '';
  end else
    if Result < 0 then
      Handler.RefCount := 0;
end;

procedure AppendLibraryError(var Handler: TLibHandler; const Msg: String);
begin
  if Handler.ErrorMsg <> '' then
    Handler.ErrorMsg := Handler.ErrorMsg + LineEnding + Msg
  else
    Handler.ErrorMsg := Msg;
end;

function GetLastLibraryError(var Handler: TLibHandler): String;
begin
  Result := Handler.ErrorMsg;
  Handler.ErrorMsg := '';
end;

procedure RaiseLibraryException(var Handler: TLibHandler);
var
  Msg: String;
begin
  Msg := GetLastLibraryError(Handler);
  if Msg <> '' then
    raise EInOutError.Create(Msg)
  else
    raise EInOutError.Create(SUnknown);
end;

function LoadLibrarySymbols(const Lib: TLibHandle; const Symbols: PLibSymbol; const Count: Integer;
  const ErrorSym: PPLibSymbol): Boolean;
var
  P,L: PLibSymbol;
begin
  P := Symbols;
  L := @Symbols[Count];
  while P < L do
  begin
    P^.pvar^ := GetProcedureAddress(Lib, P^.name);
    if not Assigned(P^.pvar^) and not P^.weak then
    begin
      if Assigned(ErrorSym) then
        ErrorSym^ := P;
      Result := False;
      Exit;
    end;
    Inc(P);
  end;
  Result := True;
end;

procedure ClearLibrarySymbols(const Symbols: PLibSymbol; const Count: Integer);
var
  P,L: PLibSymbol;
begin
  P := Symbols;
  L := @Symbols[Count];
  while P < L do
  begin
    P^.pvar^ := nil;
    Inc(P);
  end;
end;

end.
