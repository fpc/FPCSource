{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Tomas Hajny,
    member of the Free Pascal development team.

    Common implementations of functions for unit Dos
    (including dummy implementation of some functions for platforms
    missing real implementation).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure DoDirSeparators(var p:shortstring);
var
  i : longint;
begin
  for i:=1 to length(p) do
    if p[i] in AllowDirectorySeparators then
      p[i]:=DirectorySeparator;
end;

procedure DoDirSeparators(p:Pchar);
var
  i : longint;
begin
  for i:=0 to strlen(p) do
    if p[i] in AllowDirectorySeparators then
      p[i]:=DirectorySeparator;
end;

{$IFNDEF HAS_DOSEXITCODE}
threadvar
  LastDosExitCode: longint;

function DosExitCode: word;
begin
  if LastDosExitCode > high (word) then
    DosExitCode := high (word)
  else
    DosExitCode := LastDosExitCode and $FFFF;
end;
{$ENDIF HAS_DOSEXITCODE}


{$IFNDEF HAS_GETMSCOUNT}
 {$WARNING Real GetMsCount implementation missing, dummy version used}
{Dummy implementation of GetMsCount for platforms missing anything better.}
function GetMsCount: int64;
var
  Y, Mo, D, WD, H, Mi, S, S100: word;
const
  DayTable: array[Boolean, 1..12] of longint =
      ((0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
       (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335));

 function Leap: boolean;
 begin
  if (Y mod 400) = 0 then
   Leap := true
  else
   if ((Y mod 100) = 0) or ((Y mod 4) <> 0) then
    Leap := false
   else
    Leap := true;
 end;

begin
  GetDate (Y, Mo, D, WD);
  GetTime (H, Mi, S, S100);
  GetMsCount := S100 * 10 + S * 1000 + cardinal (Mi) * 60*1000
       + int64 (H) * 60*60*1000
        + int64 (D + DayTable [Leap, Mo]
          + (Y div 400) * 97 + ((Y mod 400) div 100) * 24 + (Y mod 100) div 4)
                                                                * 24*60*60*1000
         + int64 (Y) * 365*24*60*60*1000;
end;
{$ENDIF HAS_GETMSCOUNT}


{$IFNDEF HAS_GETCBREAK}
procedure GetCBreak (var BreakValue: boolean);
begin
  BreakValue := true;
end;
{$ENDIF HAS_GETCBREAK}


{$IFNDEF HAS_SETCBREAK}
procedure SetCBreak (BreakValue: boolean);
begin
end;
{$ENDIF HAS_SETCBREAK}


{$IFNDEF HAS_GETVERIFY}
var
  VerifyValue: boolean;

procedure GetVerify (var Verify: boolean);
begin
  Verify := VerifyValue;
end;
{$ENDIF HAS_GETVERIFY}


{$IFNDEF HAS_SETVERIFY}
 {$IFDEF HAS_GETVERIFY}
var
  VerifyValue: boolean;
 {$ENDIF HAS_GETVERIFY}

procedure SetVerify (Verify: boolean);
begin
  VerifyValue := Verify;
end;
{$ENDIF HAS_SETVERIFY}


{$IFDEF CPUI386}
 {$IFNDEF HAS_INTR}
procedure Intr (IntNo: byte; var Regs: Registers);
begin
end;
 {$ENDIF HAS_INTR}


 {$IFNDEF HAS_MSDOS}
procedure MSDos (var Regs: Registers);
begin
  Intr ($21, Regs);
end;
 {$ENDIF HAS_MSDOS}
{$ENDIF CPUI386}


{$IFNDEF HAS_SWAPVECTORS}
procedure SwapVectors;
begin
end;
{$ENDIF HAS_SWAPVECTORS}


{$IFNDEF HAS_GETINTVEC}
procedure GetIntVec (IntNo: byte; var Vector: pointer);
begin
  Vector := nil;
end;
{$ENDIF HAS_GETINTVEC}


{$IFNDEF HAS_SETINTVEC}
procedure SetIntVec (IntNo: byte; Vector: pointer);
begin
end;
{$ENDIF HAS_SETINTVEC}


{$IFNDEF HAS_KEEP}
procedure Keep (ExitCode: word);
begin
end;
{$ENDIF HAS_KEEP}


{$IFNDEF HAS_GETSHORTNAME}
function GetShortName (var P: String): boolean;
begin
  GetShortName := true;
end;
{$ENDIF HAS_GETSHORTNAME}


{$IFNDEF HAS_GETLONGNAME}
function GetLongName (var P: String): boolean;
begin
  GetLongName := true;
end;
{$ENDIF HAS_GETLONGNAME}


{PackTime is platform independent}
procedure PackTime (var T: DateTime; var P: longint);

var zs:longint;

begin
    p:=-1980;
    p:=p+t.year and 127;
    p:=p shl 4;
    p:=p+t.month;
    p:=p shl 5;
    p:=p+t.day;
    p:=p shl 16;
    zs:=t.hour;
    zs:=zs shl 6;
    zs:=zs+t.min;
    zs:=zs shl 5;
    zs:=zs+t.sec div 2;
    p:=p+(zs and $ffff);
end;

{UnpackTime is platform-independent}
procedure UnpackTime (P: longint; var T: DateTime);

begin
    t.sec:=(p and 31) * 2;
    p:=p shr 5;
    t.min:=p and 63;
    p:=p shr 6;
    t.hour:=p and 31;
    p:=p shr 5;
    t.day:=p and 31;
    p:=p shr 5;
    t.month:=p and 15;
    p:=p shr 4;
    t.year:=p+1980;
end;


{****************************************************************************
               A platform independent implementation of FSplit
****************************************************************************}

{$IFNDEF HAS_FSPLIT}
{$warnings off}
Procedure FSplit (Path: PathStr; var Dir: DirStr; var Name: NameStr; var Ext: ExtStr);
var
  DirEnd, ExtStart: Longint;
begin
  { allow slash and backslash }
  DoDirSeparators(Path);
{ Find the first DirectorySeparator or DriveSeparator from the end. }
  DirEnd := Length (Path);
{ Avoid problems with platforms having DriveSeparator = DirectorySeparator. }
  if DirectorySeparator = DriveSeparator then
   while (DirEnd > 0) and (Path [DirEnd] <> DirectorySeparator) do
    Dec (DirEnd)
  else
   while (DirEnd > 0) and
         (Path [DirEnd] <> DirectorySeparator) and
         (Path [DirEnd] <> DriveSeparator) do
    Dec (DirEnd);

{ The first "extension" should be returned if LFN }
{ support not available, the last one otherwise.  }
  if LFNSupport then
    begin
      ExtStart := Length (Path);
      while (ExtStart > DirEnd) and (Path [ExtStart] <> ExtensionSeparator) do
        Dec (ExtStart);
      if ExtStart = 0 then
        ExtStart := Length (Path) + 1
      else
        if Path [ExtStart] <> ExtensionSeparator then
          ExtStart := Length (Path) + 1;
    end
  else
    begin
      ExtStart := DirEnd + 1;
      while (ExtStart <= Length (Path)) and (Path [ExtStart] <> ExtensionSeparator) do
        Inc (ExtStart);
    end;

  Dir := Copy (Path, 1, DirEnd);
  Name := Copy (Path, DirEnd + 1, ExtStart - DirEnd - 1);
  Ext := Copy (Path, ExtStart, Length (Path) - ExtStart + 1);
end;
{$warnings on}
{$ENDIF HAS_FSPLIT}


{****************************************************************************
               A platform independent implementation of FExpand
****************************************************************************}

{$IFNDEF HAS_FEXPAND}

(* FExpand maintained in standalone include file for easier maintenance. *)
{$I fexpand.inc}

{$ENDIF HAS_FEXPAND}

