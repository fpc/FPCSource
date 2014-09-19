{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Free Pascal development team

    Platform specific debug functions for Amiga-like systems

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Basic system-specific debug facility. Logs to the primary log source, which is 
  usually the serial port or where the serial log is redirected to (eg. RamDebug
  on MorphOS) We could also use some barely-documented RawDoFmt() blackmagic here
  specifying "1" as PutChProc pointer, but it doesn't really matter, because
  RawDoFmt() will just call RawPutChar internally in that case (KB) }

procedure SysDebug(const s: RawByteString); platform;
var
  i: LongInt;
begin
  if s <> '' then
    for i:=1 to Length(s) do
      RawPutChar(s[i]);
end;

procedure SysDebugLn(const s: RawByteString); platform;
var
  i: LongInt;
begin
  if s <> '' then
    for i:=1 to Length(s) do
      RawPutChar(s[i]);
  RawPutChar(#10);
end;

procedure SysDebugLn; {$IFDEF SYSTEMINLINE}inline;{$ENDIF} platform;
begin
  RawPutChar(#10);
end;
