{ *********************************************************************
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Bart Broersma.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE

 **********************************************************************}

unit win9xwsmanager;

{**********************************************************************
 Objective: to provide minimal WideString Upper- and LowerCase
 functionality under Win9x systems.
 This is achievd by dynamically linking shlwapi.dll functions.
 If this fails a fallback mechanism is provided so that at least all
 lower ASCII characters in a WideString are Upper/LowerCased

 Without this library UTF8UpperCase/UTF8LowerCase will fail
 on win9x systems (which makes many Lazarus LCL controls that
 handle strings behave wrong).

 You can use this unit in your uses clause independent of yout target OS.
 No code will be linked in if yout target is not Windows.
 On true Unicode Windows (WinCE, WinNT-based) no additional libraries
 will be linked in (see: InitWin9xWSManager).

 Currently only LowerWideStringProc and UpperWideStringProc are
 replaced on win9x.
 Possibly other functions might need replacin too.

***********************************************************************}

{$mode objfpc}{$H+}
interface

{$IFDEF WINDOWS}
{ $define DEBUG_WIN9X_WSMANAGER} 


uses
  Windows, SysUtils;

{$endif WINDOWS}

implementation

{$ifdef WINDOWS}

type
  TShlwapiFunc = function(lpsz: LPWSTR; ccLength: DWORD): DWORD; stdcall;

var
  CharUpperBuffWrapW: TShlwapifunc = nil;
  CharLowerBuffWrapW: TShlwapifunc = nil;
  ShlwapiHandle: THandle = 0;
  SavedUnicodeStringManager: TUnicodeStringManager;


// Win9x**Simple functions do essentially Upper/LowerCase on
// lower ASCII characters in the string

function Win9xWideUpperSimple(const S: WideString): WideString;
const
  diff = Ord('a') - Ord('A');
var
  W: WideChar;
  i: Integer;
begin
  Result := S;
  for i := 1 to length(Result) do
  begin
    W := Result[i];
    if (Ord(W) in [Ord(Char('a'))..Ord(Char('z'))]) then
    begin
      Word(W) := Word(W) - diff;
      Result[i] := W;
    end;
  end;
end;

function Win9xWideLowerSimple(const S: WideString): WideString;
const
  diff = Ord('a') - Ord('A');
var
  W: WideChar;
  i: Integer;
begin
  Result := S;
  for i := 1 to length(Result) do
  begin
    W := Result[i];
    if (Ord(W) in [Ord(Char('A'))..Ord(Char('Z'))]) then
    begin
      Word(W) := Word(W) + diff;
      Result[i] := W;
    end;
  end;
end;


function Win9xWideUpper(const S: WideString): WideString;
begin
  Result := S;
  CharUpperBuffWrapW(PWChar(Result), Length(Result));
end;

function Win9xWideLower(const S: WideString): WideString;
begin
  Result := S;
  CharLowerBuffWrapW(PWChar(Result), Length(Result));
end;


procedure FreeDll;
begin
  {$ifdef DEBUG_WIN9X_WSMANAGER}
  if IsConsole then writeln('FreeDLL');
  {$endif}
  if ShlwapiHandle <> 0 then
  begin
    FreeLibrary(ShlwapiHandle);
    ShlwapiHandle := 0;
  end;
end;


procedure InitDll;
var
  PU,PL: Pointer;
begin
  ShlwapiHandle := LoadLibrary('shlwapi.dll');
  if (ShlwapiHandle <> 0) then
  begin
    //shlwapi functions cannot be loaded by name, only by index!
    PU := GetProcAddress(ShlwapiHandle,PChar(44));
    if (PU <> nil) then CharUpperBuffWrapW := TShlwapiFunc(PU);
    PL := GetProcAddress(ShlwapiHandle,PChar(39));
    if (PL <> nil) then CharLowerBuffWrapW := TShlwapiFunc(PL);
    {$ifdef DEBUG_WIN9X_WSMANAGER}
    {$PUSH}{$HINTS OFF}  //suppress hints on Pointer to PtrUInt tyecasting
    if IsConsole then
    begin
      writeln('Successfully loaded shlwapi.dll');
      if (PU <> nil) then
        writeln('Assigning CharUpperBuffWrapW @: ',HexStr(PtrUInt(PU),2*sizeof(PtrInt)))
      else
        writeln('Unable to load CharUpperBuffWrapW');
      if (PL <> nil) then
        writeln('Assigning CharLowerBuffWrapW @: ',HexStr(PtrUInt(PL),2*sizeof(PtrInt)))
      else
        writeln('Unable to load CharLowerBuffWrapW');
    end;
    {$POP}
    {$endif DEBUG_WIN9X_WSMANAGER}
    if (PU = nil) and (PL = nil) then
    begin
      FreeDLL;
    end;
  end
  else
  begin
    {$ifdef DEBUG_WIN9X_WSMANAGER}
    writeln('Unable to load shlwapi.dll');
    {$endif}
  end;
end;


procedure InitWin9xWSManager;
var
  WS: WideString;
begin
  SavedUnicodeStringManager := WideStringManager;
  WS := 'abc';
  if WideUpperCase(WS) <> 'ABC' then
  begin
    InitDLL;
    if Assigned(CharUpperBuffWrapW) then
    begin
      WideStringManager.UpperWideStringProc := @Win9xWideUpper;
      WS := 'abc';
      if WideUpperCase(WS) <> 'ABC' then WideStringManager.UpperWideStringProc := @Win9xWideUpperSimple;
    end
    else
    begin
      WideStringManager.UpperWideStringProc := @Win9xWideUpperSimple;
    end;
    if Assigned(CharLowerBuffWrapW) then
    begin
      WideStringmanager.LowerWideStringProc := @Win9xWideLower;
      WS := 'ABC';
      if WideLowerCase(WS) <> 'abc' then WideStringManager.LowerWideStringProc := @Win9xWideLowerSimple;
    end
    else
    begin
      WideStringManager.LowerWideStringProc := @Win9xWideLowerSimple;
    end;
  end;
end;

initialization
  InitWin9xWSManager;

finalization
  WideStringManager := SavedUnicodeStringManager;
  if (ShlwapiHandle <> 0) then FreeDll;

{$endif}
end.

