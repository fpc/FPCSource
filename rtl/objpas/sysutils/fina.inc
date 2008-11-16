{
    *********************************************************************
    Copyright (C) 1997, 1998 Gertjan Schouten

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
    *********************************************************************

    System Utilities For Free Pascal
}

function ChangeFileExt(const FileName, Extension: RtlString): RtlString;
var
  i : longint;
  EndSep : Set of Char;
begin
  i := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators+[ExtensionSeparator];
  while (I > 0) and not(FileName[I] in EndSep) do
    Dec(I);
  if (I = 0) or (FileName[I] <> ExtensionSeparator) then
    I := Length(FileName)+1;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;

function ExtractFilePath(const FileName: RtlString): RtlString;
var
  i : longint;
  EndSep : Set of Char;
begin
  i := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators;
  while (i > 0) and not (FileName[i] in EndSep) do
    Dec(i);
  If I>0 then
    Result := Copy(FileName, 1, i)
  else
    Result:='';
end;

function ExtractFileDir(const FileName: RtlString): RtlString;
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators;
  while (I > 0) and not (FileName[I] in EndSep) do
    Dec(I);
  if (I > 1) and (FileName[I] in AllowDirectorySeparators) and
     not (FileName[I - 1] in EndSep) then
    Dec(I);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileDrive(const FileName: RtlString): RtlString;

var
  i,l: longint;

begin
  Result := '';
  l:=Length(FileName);
  if (L<2) then
    exit;
  If (FileName[2] in AllowDriveSeparators) then
    result:=Copy(FileName,1,2)
  else if (FileName[1] in AllowDirectorySeparators) and
          (FileName[2] in AllowDirectorySeparators) then
    begin
      i := 2;
      While (i<L) and Not (Filename[i+1] in AllowDirectorySeparators) do
        inc(i);
      Result:=Copy(FileName,1,i);
    end;
end;

function ExtractFileName(const FileName: RtlString): RtlString;
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators;
  while (I > 0) and not (FileName[I] in EndSep) do
    Dec(I);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function ExtractFileExt(const FileName: RtlString): RtlString;
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators+[ExtensionSeparator];
  while (I > 0) and not (FileName[I] in EndSep) do
    Dec(I);
  if (I > 0) and (FileName[I] = ExtensionSeparator) then
    Result := Copy(FileName, I, MaxInt)
  else
    Result := '';
end;

function ExtractShortPathName(Const FileName : RtlString) : RtlString;
{$ifdef MSWINDOWS}
var
  res: UnicodeString;
{$endif}
begin
{$ifdef MSWINDOWS}
  SetLength(res,Max_Path);
  SetLength(res,GetShortPathNameW(_W(FileName), PWideChar(res),Length(res)));
  Result:=res;
{$else}
  Result:=FileName;
{$endif}
end;

type
  PathStr=RtlString;

{$DEFINE FPC_FEXPAND_SYSUTILS}

{$I fexpand.inc}


function ExpandFileName (Const FileName : RtlString): RtlString;

Var S : RtlString;

Begin
 S:=FileName;
 DoDirSeparators(S);
 Result:=Fexpand(S);
end;


{$ifndef HASEXPANDUNCFILENAME}
function ExpandUNCFileName (Const FileName : RtlString): RtlString;
begin
  Result:=ExpandFileName (FileName);
  //!! Here should follow code to replace the drive: part with UNC...
end;
{$endif HASEXPANDUNCFILENAME}


Const
  MaxDirs = 129;

function ExtractRelativepath (Const BaseName,DestName : RtlString): RtlString;

Var Source, Dest : RtlString;
    Sc,Dc,I,J    : Longint;
    SD,DD        : Array[1..MaxDirs] of PRtlChar;

Const OneLevelBack = '..'+DirectorySeparator;

begin
  If Uppercase(ExtractFileDrive(BaseName))<>Uppercase(ExtractFileDrive(DestName)) Then
    begin
    Result:=DestName;
    exit;
    end;
  Source:=ExcludeTrailingPathDelimiter(ExtractFilePath(BaseName));
  Dest:=ExcludeTrailingPathDelimiter(ExtractFilePath(DestName));
  SC:=GetDirs (Source,SD);
  DC:=GetDirs (Dest,DD);
  I:=1;
  While (I<=DC) and (I<=SC) do
    begin
    If AnsiCompareText(DD[i],SD[i])=0 then
      Inc(i)
    else
      Break;
    end;
  Result:='';
  For J:=I to SC do Result:=Result+OneLevelBack;
  For J:=I to DC do Result:=Result+DD[J]+DirectorySeparator;
  Result:=Result+ExtractFileName(DestNAme);
end;

Procedure DoDirSeparators (Var FileName : RtlString);

VAr I : longint;

begin
  For I:=1 to Length(FileName) do
    If FileName[I] in AllowDirectorySeparators then
      FileName[i]:=DirectorySeparator;
end;


Function SetDirSeparators (Const FileName : RtlString) : RtlString;

begin
  Result:=FileName;
  DoDirSeparators (Result);
end;

{
  DirName is split in a #0 separated list of directory names,
  Dirs is an array of pchars, pointing to these directory names.
  The function returns the number of directories found, or -1
  if none were found.
}

Function GetDirs (Var DirName : RtlString; Var Dirs : Array of PRtlChar) : Longint;

Var I : Longint;

begin
  I:=1;
  Result:=-1;
  While I<=Length(DirName) do
    begin
    If (DirName[i] in AllowDirectorySeparators) and
       { avoid error in case last char=pathdelim }
       (length(dirname)>i) then
      begin
        DirName[i]:=#0;
        Inc(Result);
        Dirs[Result]:=@DirName[I+1];
      end;
    Inc(I);
    end;
  If Result>-1 then inc(Result);
end;

function IncludeTrailingPathDelimiter(Const Path : RtlString) : RtlString;

Var
  l : Integer;

begin
  Result:=Path;
  l:=Length(Result);
  If (L=0) or not(Result[l] in AllowDirectorySeparators) then
    Result:=Result+DirectorySeparator;
end;

function IncludeTrailingBackslash(Const Path : RtlString) : RtlString;

begin
  Result:=IncludeTrailingPathDelimiter(Path);
end;

function ExcludeTrailingBackslash(Const Path: RtlString): RtlString;

begin
  Result:=ExcludeTrailingPathDelimiter(Path);
end;

function ExcludeTrailingPathDelimiter(Const Path: RtlString): RtlString;

Var
  L : Integer;

begin
  L:=Length(Path);
  If (L>0) and (Path[L] in AllowDirectorySeparators) then
    Dec(L);
  Result:=Copy(Path,1,L);
end;

function IsPathDelimiter(Const Path: RtlString; Index: Integer): Boolean;

begin
  Result:=(Index>0) and (Index<=Length(Path)) and (Path[Index] in AllowDirectorySeparators);
end;

Function GetFileHandle(var f : File):Longint;

begin
  result:=filerec(f).handle;
end;

Function GetFileHandle(var f : Text):Longint;
begin
  result:=textrec(f).handle;
end;

