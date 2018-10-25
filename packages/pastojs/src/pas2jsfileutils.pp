{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018  Mattias Gaertner  mattias@freepascal.org

    Pascal to Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Abstract:
    Low level file path handling.
}
unit Pas2jsFileUtils;

{$mode objfpc}{$H+}

{$i pas2js_defines.inc}

interface

uses
  {$IFDEF Unix}
  BaseUnix,
  {$ENDIF}
  {$IFDEF Pas2JS}
  NodeJSFS,
  {$ENDIF}
  SysUtils, Classes;

function FilenameIsAbsolute(const aFilename: string):boolean;
function FilenameIsWinAbsolute(const aFilename: string):boolean;
function FilenameIsUnixAbsolute(const aFilename: string):boolean;
function FileIsInPath(const Filename, Path: string): boolean;
function ChompPathDelim(const Path: string): string;
function ExpandFileNamePJ(const FileName: string; {const} BaseDir: string = ''): string;
function ExpandDirectory(const aDirectory: string): string;
function TryCreateRelativePath(const Filename, BaseDirectory: String;
  UsePointDirectory: boolean; out RelPath: String): Boolean;
function ResolveDots(const AFilename: string): string;
procedure ForcePathDelims(Var FileName: string);
function GetForcedPathDelims(Const FileName: string): String;
function ExtractFilenameOnly(const aFilename: string): string;
function GetCurrentDirPJ: String;
function CompareFilenames(const File1, File2: string): integer;

function GetPhysicalFilename(const Filename: string;
        ExceptionOnError: boolean): string;
function ResolveSymLinks(const Filename: string;
                 {%H-}ExceptionOnError: boolean): string; // if a link is broken returns ''
function MatchGlobbing(Mask, Name: string): boolean;
function FileIsWritable(const AFilename: string): boolean;

function GetEnvironmentVariableCountPJ: Integer;
function GetEnvironmentStringPJ(Index: Integer): string;
function GetEnvironmentVariablePJ(const EnvVar: string): String;

function GetNextDelimitedItem(const List: string; Delimiter: char;
                              var Position: integer): string;

type TChangeStamp = SizeInt;
const InvalidChangeStamp = low(TChangeStamp);
procedure IncreaseChangeStamp(var Stamp: TChangeStamp);

{$IFDEF FPC_HAS_CPSTRING}
const
  UTF8BOM = #$EF#$BB#$BF;
  EncodingUTF8 = 'UTF-8';
  EncodingSystem = 'System';
function NormalizeEncoding(const Encoding: string): string;
function IsNonUTF8System: boolean;// true if system encoding is not UTF-8
function UTF8CharacterStrictLength(P: PChar): integer;
function GetDefaultTextEncoding: string;
function GetConsoleTextEncoding: string;
{$IFDEF Windows}
// AConsole - If false, it is the general system encoding,
//            if true, it is the console encoding
function GetWindowsEncoding(AConsole: Boolean = False): string;
{$ENDIF}
{$IF defined(Unix) and not defined(Darwin)}
function GetUnixEncoding: string;
{$ENDIF}
function IsASCII(const s: string): boolean; inline;

function UTF8ToUTF16(const s: string): UnicodeString;
function UTF16ToUTF8(const s: UnicodeString): string;

function UTF8ToSystemCP(const s: string): string;
function SystemCPToUTF8(const s: string): string;

function ConsoleToUTF8(const s: string): string;
// converts UTF8 string to console encoding (used by Write, WriteLn)
function UTF8ToConsole(const s: string): string;
{$ENDIF FPC_HAS_CPSTRING}

implementation

{$IFDEF Windows}
uses Windows;
{$ENDIF}

{$IFDEF FPC_HAS_CPSTRING}
var
  EncodingValid: boolean = false;
  DefaultTextEncoding: string = EncodingSystem;
  {$IFDEF Unix}
  {$IFNDEF Darwin}
  Lang: string = '';
  {$ENDIF}
  {$ENDIF}
  NonUTF8System: boolean = false;
{$ENDIF}

function FilenameIsWinAbsolute(const aFilename: string): boolean;
begin
  Result:=((length(aFilename)>=3) and
           (aFilename[1] in ['A'..'Z','a'..'z']) and (aFilename[2]=':') and (aFilename[3]in AllowDirectorySeparators))
      or ((length(aFilename)>=2) and (aFilename[1] in AllowDirectorySeparators) and (aFilename[2] in AllowDirectorySeparators));
end;

function FilenameIsUnixAbsolute(const aFilename: string): boolean;
begin
  Result:=(aFilename<>'') and (aFilename[1]='/');
end;

function FileIsInPath(const Filename, Path: string): boolean;
var
  ExpFile: String;
  ExpPath: String;
  l: integer;
begin
  if Path='' then
  begin
    Result:=false;
    exit;
  end;
  ExpFile:=Filename;
  ExpPath:=IncludeTrailingPathDelimiter(Path);
  l:=length(ExpPath);
  Result:=(l>0) and (length(ExpFile)>l) and (ExpFile[l]=PathDelim)
          and (CompareFileNames(ExpPath,LeftStr(ExpFile,l))=0);
end;

function ChompPathDelim(const Path: string): string;
var
  Len, MinLen: Integer;
begin
  Result:=Path;
  if Path = '' then
    exit;
  Len:=length(Result);
  if (Result[1] in AllowDirectorySeparators) then
  begin
    MinLen := 1;
    {$IFDEF HasUNCPaths}
    if (Len >= 2) and (Result[2] in AllowDirectorySeparators) then
      MinLen := 2; // keep UNC '\\', chomp 'a\' to 'a'
    {$ENDIF}
  end
  else begin
    MinLen := 0;
    {$IFdef MSWindows}
    if (Len >= 3) and (Result[1] in ['a'..'z', 'A'..'Z'])  and
       (Result[2] = ':') and (Result[3] in AllowDirectorySeparators)
    then
      MinLen := 3;
    {$ENDIF}
  end;

  while (Len > MinLen) and (Result[Len] in AllowDirectorySeparators) do dec(Len);
  if Len<length(Result) then
    SetLength(Result,Len);
end;

function ExpandDirectory(const aDirectory: string): string;
begin
  Result:=aDirectory;
  if Result='' then exit;
  Result:=ExpandFileNamePJ(Result);
  if Result='' then exit;
  Result:=IncludeTrailingPathDelimiter(Result);
end;

function TryCreateRelativePath(const Filename, BaseDirectory: String;
  UsePointDirectory: boolean; out RelPath: String): Boolean;
{
  Returns True if it is possible to create a relative path from Source to Dest
  Function must be thread safe, so no expanding of filenames is done, since this
  is not threadsafe (at least on Windows platform)

  - Dest and Source must either be both absolute filenames, or relative
  - Dest and Source cannot contain '..' since no expanding is done by design
  - Dest and Source must be on same drive or UNC path (Windows)
  - if both Dest and Source are relative they must at least share their base directory
  - Double PathDelims are ignored (unless they are part of the UNC convention)

  - if UsePointDirectory is True and Result is True then if RelPath is Empty string, RelPath becomes '.'
  - if AlwaysRequireSharedBaseFolder is False then Absolute filenames need not share a basefolder

  - if the function succeeds RelPath contains the relative path from Source to Dest,
    no PathDelimiter is appended to the end of RelPath

  Examples:
  - Filename = /foo/bar BaseDirectory = /foo Result = True RelPath = bar
  - Filename = /foo///bar BaseDirectory = /foo// Result = True RelPath = bar
  - Filename = /foo BaseDirectory = /foo/bar Result = True RelPath = ../
  - Filename = /foo/bar BaseDirectory = /bar Result = False (no shared base directory)
  - Filename = foo/bar BaseDirectory = foo/foo Result = True RelPath = ../bar
  - Filename = foo/bar BaseDirectory = bar/foo Result = False (no shared base directory)
  - Filename = /foo BaseDirectory = bar Result = False (mixed absolute and relative)
}
begin
  Result:=false;
  RelPath:=Filename;
  if (BaseDirectory='') or (Filename='') then exit;
  writeln('TryCreateRelativePath ToDo: ',Filename,' Base=',BaseDirectory,' UsePointDirectory=',UsePointDirectory);
end;

function ResolveDots(const AFilename: string): string;
//trim double path delims and expand special dirs like .. and .
//on Windows change also '/' to '\' except for filenames starting with '\\?\'
var
  Len: Integer;
begin
  Len:=length(AFilename);
  if Len=0 then exit('');

  Result:=AFilename;
  writeln('ResolveDots ToDo ',AFilename);
end;

procedure ForcePathDelims(Var FileName: string);
begin
  Filename:=GetForcedPathDelims(Filename);
end;

function GetForcedPathDelims(const FileName: string): String;
var
  i: Integer;
  c: Char;
begin
  Result:=Filename;
  if PathDelim='/' then
    c:='\'
  else
    c:='/';
  for i:=1 to length(Result) do
    if Result[i]=c then
      Result[i]:=PathDelim;
end;

function ExtractFilenameOnly(const aFilename: string): string;
var
  StartPos: Integer;
  ExtPos: Integer;
begin
  StartPos:=length(AFilename)+1;
  while (StartPos>1)
  and not (AFilename[StartPos-1] in AllowDirectorySeparators)
  {$IFDEF Windows}and (AFilename[StartPos-1]<>':'){$ENDIF}
  do
    dec(StartPos);
  ExtPos:=length(AFilename);
  while (ExtPos>=StartPos) and (AFilename[ExtPos]<>'.') do
    dec(ExtPos);
  if (ExtPos<StartPos) then ExtPos:=length(AFilename)+1;
  Result:=copy(AFilename,StartPos,ExtPos-StartPos);
end;

function CompareFilenames(const File1, File2: string): integer;
begin
  writeln('CompareFilenames ToDo ',File1,' ',File2);
  Result:=0;
end;

function MatchGlobbing(Mask, Name: string): boolean;
// match * and ?
begin
  if Mask='' then exit(Name='');
  writeln('MatchGlobbing ToDo ',Mask,' Name=',Name);
  Result:=false;
end;

function GetNextDelimitedItem(const List: string; Delimiter: char;
  var Position: integer): string;
var
  StartPos: Integer;
begin
  StartPos:=Position;
  while (Position<=length(List)) and (List[Position]<>Delimiter) do
    inc(Position);
  Result:=copy(List,StartPos,Position-StartPos);
  if Position<=length(List) then inc(Position); // skip Delimiter
end;

procedure IncreaseChangeStamp(var Stamp: TChangeStamp);
begin
  if Stamp<High(TChangeStamp) then
    inc(Stamp)
  else
    Stamp:=InvalidChangeStamp+1;
end;

{$IFDEF FPC_HAS_CPSTRING}
function IsNonUTF8System: boolean;
begin
  Result:=NonUTF8System;
end;

function UTF8CharacterStrictLength(P: PChar): integer;
begin
  if p=nil then exit(0);
  if ord(p^)<%10000000 then
  begin
    // regular single byte character
    exit(1);
  end
  else if ord(p^)<%11000000 then
  begin
    // invalid single byte character
    exit(0);
  end
  else if ((ord(p^) and %11100000) = %11000000) then
  begin
    // should be 2 byte character
    if (ord(p[1]) and %11000000) = %10000000 then
      exit(2)
    else
      exit(0);
  end
  else if ((ord(p^) and %11110000) = %11100000) then
  begin
    // should be 3 byte character
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000) then
      exit(3)
    else
      exit(0);
  end
  else if ((ord(p^) and %11111000) = %11110000) then
  begin
    // should be 4 byte character
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000)
    and ((ord(p[3]) and %11000000) = %10000000) then
      exit(4)
    else
      exit(0);
  end else
    exit(0);
end;

function GetDefaultTextEncoding: string;
begin
  if EncodingValid then
  begin
    Result:=DefaultTextEncoding;
    exit;
  end;

  {$IFDEF Windows}
  Result:=GetWindowsEncoding;
  {$ELSE}
  {$IFDEF Darwin}
  Result:=EncodingUTF8;
  {$ELSE}
  Lang := GetEnvironmentVariable('LC_ALL');
  if Lang='' then
  begin
    Lang := GetEnvironmentVariable('LC_MESSAGES');
    if Lang='' then
      Lang := GetEnvironmentVariable('LANG');
  end;
  Result:=GetUnixEncoding;
  {$ENDIF}
  {$ENDIF}
  Result:=NormalizeEncoding(Result);

  DefaultTextEncoding:=Result;
  EncodingValid:=true;
end;

function NormalizeEncoding(const Encoding: string): string;
var
  i: Integer;
begin
  Result:=LowerCase(Encoding);
  for i:=length(Result) downto 1 do
    if Result[i]='-' then Delete(Result,i,1);
end;

function IsASCII(const s: string): boolean; inline;
var
  p: PChar;
begin
  if s='' then exit(true);
  p:=PChar(s);
  repeat
    case p^ of
    #0: if p-PChar(s)=length(s) then exit(true);
    #128..#255: exit(false);
    end;
    inc(p);
  until false;
end;

function UTF8ToUTF16(const s: string): UnicodeString;
begin
  Result:=UTF8Decode(s);
end;

function UTF16ToUTF8(const s: UnicodeString): string;
begin
  if s='' then exit('');
  Result:=UTF8Encode(s);
  // prevent UTF8 codepage appear in the strings - we don't need codepage
  // conversion magic
  SetCodePage(RawByteString(Result), CP_ACP, False);
end;
{$ENDIF}

{$IFDEF Unix}
  {$I pas2jsfileutilsunix.inc}
{$ENDIF}
{$IFDEF Windows}
  {$I pas2jsfileutilswin.inc}
{$ENDIF}
{$IFDEF NodeJS}
  {$I pas2jsfileutilsnodejs.inc}
{$ENDIF}

procedure InternalInit;
begin
  {$IFDEF FPC_HAS_CPSTRING}
  SetMultiByteConversionCodePage(CP_UTF8);
  // SetMultiByteFileSystemCodePage(CP_UTF8); not needed, this is the default under Windows
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);

  GetDefaultTextEncoding;
  {$IFDEF Windows}
  NonUTF8System:=true;
  {$ELSE}
  NonUTF8System:=SysUtils.CompareText(DefaultTextEncoding,'UTF8')<>0;
  {$ENDIF}
  {$ENDIF}

  InitPlatform;
end;

initialization
  InternalInit;
{$IFDEF FPC}
finalization
  FinalizePlatform;
{$ENDIF}
end.

