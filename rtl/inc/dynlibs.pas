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

Type
  PLibHandler = ^TLibHandler;

  TLibEventLoading = function(User: Pointer; Handler: PLibHandler; out ErrorMsg: String): Boolean;
  TLibEventUnloading = procedure(User: Pointer; Handler: PLibHandler);

  PPLibSymbol = ^PLibSymbol;
  PLibSymbol = ^TLibSymbol;
  TLibSymbol = record
    pvar: PPointer;  { pointer to Symbol variable }
    name: String;    { name of the Symbol }
    weak: Boolean;   { weak }
  end;

  TLibHandler = record
    AbstractName : String;
    Handle       : TLibHandle;
    Filename     : String;
    Loading      : TLibEventLoading;
    Unloading    : TLibEventUnloading;
    SymCount     : Integer;
    Symbols      : PLibSymbol;
    ErrorMsg     : String;
    RefCount     : Integer;
  end;


function LibraryHandler(const AbstractName: String; const Symbols: PLibSymbol; const SymCount: Integer;
  const AfterLoading: TLibEventLoading = nil; const BeforeUnloading: TLibEventUnloading = nil): TLibHandler;
function TryInitializeLibrary(var Handler: TLibHandler; const Filenames: array of String;
  const User: Pointer = nil; const Weak: Boolean = False): Integer;
function TryInitializeLibrary(var Handler: TLibHandler; const Filename: String;
  const User: Pointer = nil; const Weak: Boolean = False): Integer;
function InitializeLibrary(var Handler: TLibHandler; const Filenames: array of String;
  const User: Pointer = nil; const Weak: Boolean = False): Integer;
function InitializeLibrary(var Handler: TLibHandler; const Filename: String;
  const User: Pointer = nil; const Weak: Boolean = False): Integer;
function ReleaseLibrary(var Handler: TLibHandler; User: Pointer = nil): Integer;
function GetLastLibraryError(var Handler: TLibHandler): String;
procedure RaiseLibraryException(var Handler: TLibHandler);

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

function LibraryHandler(const AbstractName: String; const Symbols: PLibSymbol; const SymCount: Integer;
  const AfterLoading: TLibEventLoading; const BeforeUnloading: TLibEventUnloading): TLibHandler;
begin
  Result.AbstractName := AbstractName;
  Result.Handle       := NilHandle;
  Result.Filename     := '';
  Result.Loading      := AfterLoading;
  Result.Unloading    := BeforeUnloading;
  Result.SymCount     := SymCount;
  Result.Symbols      := Symbols;
  Result.ErrorMsg     := '';
  Result.RefCount     := 0;
end;

function TryInitializeLibrary(var Handler: TLibHandler; const Filenames: array of String;
  const User: Pointer; const Weak: Boolean): Integer;
var
  I: Integer;
begin
  if Length(Filenames) <= 0 then
  begin
    Handler.ErrorMsg := SVarInvalid;
    Result := -1;
    Exit;
  end;

  for I := 0 to High(Filenames) do
  begin
    Result := TryInitializeLibrary(Handler, Filenames[I], User, Weak);
    if Result > 0 then
      Exit;
  end;
end;

function TryInitializeLibrary(var Handler: TLibHandler; const Filename: String;
  const User: Pointer; const Weak: Boolean): Integer;
var
  ErrSym: PLibSymbol;
begin
  Handler.ErrorMsg := '';

  if (Handler.Filename <> '') and (Handler.Filename <> Filename) then
  begin
    Handler.ErrorMsg := Format(SLibraryAlreadyLoaded, [Handler.AbstractName, Handler.Filename]);
    Result := -1;
    Exit;
  end;

  Result := InterlockedIncrement(Handler.RefCount);
  if Result = 1 then
  begin
    Handler.Handle := LoadLibrary(Filename);
    if Handler.Handle = NilHandle then
    begin
      Handler.ErrorMsg := Format(SLibraryNotLoaded, [Handler.AbstractName, Filename]);
      Handler.RefCount := 0;
      Result := -1;
      Exit;
    end;

    Handler.Filename := Filename;

    if not LoadLibrarySymbols(Handler.Handle, Handler.Symbols, Handler.SymCount, @ErrSym) and not Weak then
    begin
      UnloadLibrary(Handler.Handle);
      Handler.Handle := NilHandle;
      Handler.Filename := '';
      Handler.ErrorMsg := Format(SLibraryUnknownSym, [ErrSym^.name, Handler.AbstractName, Filename]);
      Handler.RefCount := 0;
      Result := -1;
      Exit;
    end;

    if Assigned(Handler.Loading) and not Handler.Loading(User, @Handler, Handler.ErrorMsg) then
    begin
      UnloadLibrary(Handler.Handle);
      Handler.Handle := NilHandle;
      Handler.Filename := '';
      Handler.RefCount := 0;
      Result := -1;
      Exit;
    end else
      Handler.ErrorMsg := '';
  end;
end;

function InitializeLibrary(var Handler: TLibHandler; const Filenames: array of String;
  const User: Pointer; const Weak: Boolean): Integer;
begin
  Result := TryInitializeLibrary(Handler, Filenames, User, Weak);
  RaiseLibraryException(Handler);
end;

function InitializeLibrary(var Handler: TLibHandler; const Filename: String;
  const User: Pointer; const Weak: Boolean): Integer;
begin
  Result := TryInitializeLibrary(Handler, Filename, User, Weak);
  RaiseLibraryException(Handler);
end;

function ReleaseLibrary(var Handler: TLibHandler; User: Pointer): Integer;
begin
  Handler.ErrorMsg := '';

  Result := InterlockedDecrement(Handler.RefCount);
  if Result = 0 then
  begin
    if Assigned(Handler.Unloading) then
      Handler.Unloading(User, @Handler);
    ClearLibrarySymbols(Handler.Symbols, Handler.SymCount);
    UnloadLibrary(Handler.Handle);
    Handler.Handle := NilHandle;
    Handler.Filename := '';
  end else
    if Result < 0 then
      Handler.RefCount := 0;
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
    raise EInOutError.Create(Msg);
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
