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
  SysUtils, Classes;

function FilenameIsAbsolute(const aFilename: string):boolean;
function FilenameIsWinAbsolute(const aFilename: string):boolean;
function FilenameIsUnixAbsolute(const aFilename: string):boolean;
function FileIsInPath(const Filename, Path: string): boolean;
function ChompPathDelim(const Path: string): string;
function ExpandFileNameUTF8(const FileName: string; {const} BaseDir: string = ''): string;
function ExpandDirectory(const aDirectory: string): string;
function TryCreateRelativePath(const Filename, BaseDirectory: String;
  UsePointDirectory: boolean; out RelPath: String): Boolean;
function ResolveDots(const AFilename: string): string;
procedure ForcePathDelims(Var FileName: string);
function GetForcedPathDelims(Const FileName: string): String;
function ExtractFilenameOnly(const aFilename: string): string;
function GetCurrentDirUTF8: String;
function CompareFilenames(const File1, File2: string): integer;

function GetPhysicalFilename(const Filename: string;
        ExceptionOnError: boolean): string;
function ResolveSymLinks(const Filename: string;
                 {%H-}ExceptionOnError: boolean): string; // if a link is broken returns ''
function MatchGlobbing(Mask, Name: string): boolean;
function FileIsWritable(const AFilename: string): boolean;

function GetEnvironmentVariableCountUTF8: Integer;
function GetEnvironmentStringUTF8(Index: Integer): string;
function GetEnvironmentVariableUTF8(const EnvVar: string): String;

function GetNextDelimitedItem(const List: string; Delimiter: char;
                              var Position: integer): string;

type TChangeStamp = SizeInt;
const InvalidChangeStamp = low(TChangeStamp);
procedure IncreaseChangeStamp(var Stamp: TChangeStamp);

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

implementation

{$IFDEF Windows}
uses Windows;
{$ENDIF}

var
  EncodingValid: boolean = false;
  DefaultTextEncoding: string = EncodingSystem;
  {$IFDEF Unix}
  {$IFNDEF Darwin}
  Lang: string = '';
  {$ENDIF}
  {$ENDIF}
  NonUTF8System: boolean = false;

function FilenameIsWinAbsolute(const aFilename: string): boolean;
begin
  Result:=((length(aFilename)>=3) and
           (aFilename[1] in ['A'..'Z','a'..'z']) and (aFilename[2]=':')  and (aFilename[3]in AllowDirectorySeparators))
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
          and (AnsiCompareFileName(ExpPath,LeftStr(ExpFile,l))=0);
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
  Result:=ExpandFileNameUTF8(Result);
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

  function IsNameChar(c: char): boolean; inline;
  begin
    Result:=(c<>#0) and not (c in AllowDirectorySeparators);
  end;

var
  UpDirCount: Integer;
  ResultPos: Integer;
  i: Integer;
  FileNameRestLen, SharedDirs: Integer;
  FileP, BaseP, FileEndP, BaseEndP: PChar;
begin
  Result:=false;
  RelPath:=Filename;
  if (BaseDirectory='') or (Filename='') then exit;
  // check for different windows file drives
  if (CompareText(ExtractFileDrive(Filename),
                     ExtractFileDrive(BaseDirectory))<>0)
  then
    exit;

  FileP:=PChar(Filename);
  BaseP:=PChar(BaseDirectory);

  //writeln('TryCreateRelativePath START File="',FileP,'" Base="',BaseP,'"');

  // skip matching directories
  SharedDirs:=0;
  if FileP^ in AllowDirectorySeparators then
  begin
    if not (BaseP^ in AllowDirectorySeparators) then exit;
    repeat
      while FileP^ in AllowDirectorySeparators do inc(FileP);
      while BaseP^ in AllowDirectorySeparators do inc(BaseP);
      if (FileP^=#0) or (BaseP^=#0) then break;
      //writeln('TryCreateRelativePath check match .. File="',FileP,'" Base="',BaseP,'"');
      FileEndP:=FileP;
      BaseEndP:=BaseP;
      while IsNameChar(FileEndP^) do inc(FileEndP);
      while IsNameChar(BaseEndP^) do inc(BaseEndP);
      if CompareFilenames(copy(Filename,FileP-PChar(Filename)+1,FileEndP-FileP),
        copy(BaseDirectory,BaseP-PChar(BaseDirectory)+1,BaseEndP-BaseP))<>0
      then
        break;
      FileP:=FileEndP;
      BaseP:=BaseEndP;
      inc(SharedDirs);
    until false;
  end else if (BaseP^ in AllowDirectorySeparators) then
    exit;

  //writeln('TryCreateRelativePath skipped matches File="',FileP,'" Base="',BaseP,'"');
  if SharedDirs=0 then exit;

  // calculate needed '../'
  UpDirCount:=0;
  BaseEndP:=BaseP;
  while IsNameChar(BaseEndP^) do begin
    inc(UpDirCount);
    while IsNameChar(BaseEndP^) do inc(BaseEndP);
    while BaseEndP^ in AllowDirectorySeparators do inc(BaseEndP);
  end;

  //writeln('TryCreateRelativePath UpDirCount=',UpDirCount,' File="',FileP,'" Base="',BaseP,'"');
  // create relative filename
  if (FileP^=#0) and (UpDirCount=0) then
  begin
    // Filename is the BaseDirectory
    if UsePointDirectory then
      RelPath:='.'
    else
      RelPath:='';
    exit(true);
  end;

  FileNameRestLen:=length(Filename)-(FileP-PChar(Filename));
  SetLength(RelPath,3*UpDirCount+FileNameRestLen);
  ResultPos:=1;
  for i:=1 to UpDirCount do begin
    RelPath[ResultPos]:='.';
    RelPath[ResultPos+1]:='.';
    RelPath[ResultPos+2]:=PathDelim;
    inc(ResultPos,3);
  end;
  if FileNameRestLen>0 then
    Move(FileP^,RelPath[ResultPos],FileNameRestLen);
  Result:=true;
end;

function ResolveDots(const AFilename: string): string;
//trim double path delims and expand special dirs like .. and .
//on Windows change also '/' to '\' except for filenames starting with '\\?\'

  {$ifdef windows}
  function IsDriveDelim(const Path: string; p: integer): boolean; inline;
  begin
    Result:=(p=2) and (Path[2]=DriveDelim) and (Path[1] in ['a'..'z','A'..'Z']);
  end;
  {$endif}

  function IsPathDelim(const Path: string; p: integer): boolean;
  begin
    if (p<=0) or (Path[p]=PathDelim) then exit(true);
    {$ifdef windows}
    if IsDriveDelim(Path,p) then
      exit(true);
    {$endif}
    Result:=false;
  end;

var SrcPos, DestPos, Len, DirStart: integer;
  c: char;
  MacroPos: LongInt;
begin
  Len:=length(AFilename);
  if Len=0 then exit('');

  Result:=AFilename;

  {$ifdef windows}
  //Special case: everything is literal after this, even dots (this does not apply to '//?/')
  if (length(AFilename)>=4) and (AFilename[1]='\') and (AFilename[2]='\')
  and (AFilename[3]='?') and (AFilename[4]='\') then
    exit;
  {$endif}

  SrcPos:=1;
  DestPos:=1;

  // trim double path delimiters and special dirs . and ..
  while (SrcPos<=Len) do begin
    c:=AFilename[SrcPos];
    {$ifdef windows}
    //change / to \. The WinApi accepts both, but it leads to strange effects in other places
    if (c in AllowDirectorySeparators) then c := PathDelim;
    {$endif}
    // check for duplicate path delims
    if (c=PathDelim) then
    begin
      inc(SrcPos);
      {$IFDEF Windows}
      if (DestPos>2)
      {$ELSE}
      if (DestPos>1)
      {$ENDIF}
      and (Result[DestPos-1]=PathDelim) then
      begin
        // skip duplicate PathDelim
        continue;
      end;
      Result[DestPos]:=c;
      inc(DestPos);
      continue;
    end;
    // check for special dirs . and ..
    if (c='.') then
    begin
      if (SrcPos<Len) then
      begin
        if (AFilename[SrcPos+1] in AllowDirectorySeparators)
        and IsPathDelim(Result,DestPos-1) then
        begin
          // special dir ./ or */./
          // -> skip
          inc(SrcPos,2);
          while (SrcPos<=Len) and (AFilename[SrcPos] in AllowDirectorySeparators) do
            inc(SrcPos);
          continue;
        end else if (AFilename[SrcPos+1]='.')
        and ((SrcPos+1=Len) or (AFilename[SrcPos+2] in AllowDirectorySeparators)) then
        begin
          // special dir ..
          //  1. ..      -> copy
          //  2. /..     -> skip .., keep /
          //  3. C:..    -> copy
          //  4. C:\..   -> skip .., keep C:\
          //  5. \\..    -> skip .., keep \\
          //  6. ../..   -> copy because if the first '..' was not resolved, the next can't neither
          //  7. dir/..  -> trim dir and ..
          //  8. dir$macro/..  -> copy
          if DestPos=1 then
          begin
            //  1. .. or ../  -> copy
          end else if (DestPos=2) and (Result[1]=PathDelim) then
          begin
            //  2. /..     -> skip .., keep /
            inc(SrcPos,2);
            continue;
          {$IFDEF Windows}
          end else if (DestPos=3) and IsDriveDelim(Result,2) then
          begin
            //  3. C:..    -> copy
          end else if (DestPos=4) and (Result[3]=PathDelim)
          and IsDriveDelim(Result,2) then
          begin
            //  4. C:\..   -> skip .., keep C:\
            inc(SrcPos,2);
            continue;
          end else if (DestPos=3) and (Result[1]=PathDelim)
          and (Result[2]=PathDelim) then
          begin
            //  5. \\..    -> skip .., keep \\
            inc(SrcPos,2);
            continue;
          {$ENDIF}
          end else if (DestPos>1) and (Result[DestPos-1]=PathDelim) then
          begin
            // */.
            if (DestPos>3)
            and (Result[DestPos-2]='.') and (Result[DestPos-3]='.')
            and IsPathDelim(Result,DestPos-4) then
            begin
              //  6. ../..   -> copy because if the first '..' was not resolved, the next can't neither
            end else begin
              //  7. xxxdir/..  -> trim dir and skip ..
              DirStart:=DestPos-2;
              while (DirStart>1) and (Result[DirStart-1]<>PathDelim) do
                dec(DirStart);
              {$ifdef windows}
              if (DirStart=1) and IsDriveDelim(Result,2) then
                inc(DirStart,2);
              {$endif}
              MacroPos:=DirStart;
              while MacroPos<DestPos do begin
                if (Result[MacroPos]='$')
                and (Result[MacroPos+1] in ['(','a'..'z','A'..'Z']) then
                begin
                  // 8. directory contains a macro -> keep
                  break;
                end;
                inc(MacroPos);
              end;
              if MacroPos=DestPos then
              begin
                // previous directory does not contain a macro -> remove dir/..
                DestPos:=DirStart;
                inc(SrcPos,2);
                //writeln('ResolveDots ',DestPos,' SrcPos=',SrcPos,' File="',AFilename,'" Result="',copy(Result,1,DestPos-1),'"');
                if SrcPos>Len then
                begin
                  // '..' at end of filename
                  if (DestPos>1) and (Result[DestPos-1]=PathDelim) then
                  begin
                    // foo/dir/.. -> foo
                    dec(DestPos);
                  end else if (DestPos=1) then
                  begin
                    // foo/.. -> .
                    Result[1]:='.';
                    DestPos:=2;
                  end;
                end else if DestPos=1 then
                begin
                  // e.g. 'foo/../'
                  while (SrcPos<=Len) and (AFilename[SrcPos] in AllowDirectorySeparators) do
                    inc(SrcPos);
                end;
                continue;
              end;
            end;
          end;
        end;
      end else begin
        // special dir . at end of filename
        if DestPos=1 then
        begin
          Result:='.';
          exit;
        end;
        if (DestPos>2) and (Result[DestPos-1]=PathDelim)
        {$ifdef windows}
        and not IsDriveDelim(Result,DestPos-2)
        {$endif}
        then begin
          // foo/. -> foo
          // C:foo\. -> C:foo
          // C:\. -> C:\
          dec(DestPos);
        end;
        break;
      end;
    end;
    // copy directory
    repeat
      Result[DestPos]:=c;
      inc(DestPos);
      inc(SrcPos);
      if (SrcPos>Len) then break;
      c:=AFilename[SrcPos];
      {$ifdef windows}
      //change / to \. The WinApi accepts both, but it leads to strange effects in other places
      if (c in AllowDirectorySeparators) then c := PathDelim;
      {$endif}
      if c=PathDelim then break;
    until false;
  end;
  // trim result
  if DestPos<=length(AFilename) then
    if (DestPos=1) then
      Result:='.'
    else
      SetLength(Result,DestPos-1);
end;

procedure ForcePathDelims(Var FileName: string);
var
  i: Integer;
begin
  for i:=1 to length(FileName) do
    {$IFDEF Windows}
    if Filename[i]='/' then
      Filename[i]:='\';
    {$ELSE}
    if Filename[i]='\' then
      Filename[i]:='/';
    {$ENDIF}
end;

function GetForcedPathDelims(const FileName: string): String;
begin
  Result:=FileName;
  ForcePathDelims(Result);
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
  Result:=AnsiCompareFileName(File1,File2);
end;

function MatchGlobbing(Mask, Name: string): boolean;
// match * and ?

  function IsNameEnd(NameP: PChar): boolean; inline;
  begin
    Result:=(NameP^=#0) and (NameP-PChar(Name)=length(Name));
  end;

  function Check(MaskP, NameP: PChar): boolean;
  var
    c: Integer;
  begin
    repeat
      case MaskP^ of
      #0:
        exit(IsNameEnd(NameP));
      '?':
        if not IsNameEnd(NameP) then
        begin
          inc(MaskP);
          c:=UTF8CharacterStrictLength(NameP);
          if c<1 then c:=1;
          inc(NameP,c);
        end else
          exit(false);
      '*':
        begin
          repeat
            inc(MaskP);
          until MaskP^<>'*';
          if MaskP=#0 then exit(true);
          while not IsNameEnd(NameP) do begin
            inc(NameP);
            if Check(MaskP,NameP) then exit(true);
          end;
          exit(false);
        end;
      else
        if NameP^<>MaskP^ then exit(false);
        c:=UTF8CharacterStrictLength(MaskP);
        if c<1 then c:=1;
        inc(MaskP);
        c:=UTF8CharacterStrictLength(NameP);
        if c<1 then c:=1;
        inc(NameP,c);
      end;
    until false;
  end;

var
  MaskP: PChar;
begin
  if Mask='' then exit(Name='');
  {$IFDEF CaseInsensitiveFilenames}
  Mask:=AnsiLowerCase(Mask);
  Name:=AnsiLowerCase(Name);
  {$ENDIF}
  MaskP:=PChar(Mask);
  while (MaskP^='*') and (MaskP[1]='*') do inc(MaskP);
  if (MaskP^='*') and (MaskP[1]=#0) then
    exit(true); // the * mask fits all, even the empty string
  if Name='' then
    exit(false);
  Result:=Check(MaskP,PChar(Name));
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

{$IFDEF Unix}
  {$I pas2jsfileutilsunix.inc}
{$ENDIF}
{$IFDEF Windows}
  {$I pas2jsfileutilswin.inc}
{$ENDIF}

procedure InternalInit;
begin
  SetMultiByteConversionCodePage(CP_UTF8);
  // SetMultiByteFileSystemCodePage(CP_UTF8); not needed, this is the default under Windows
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);

  GetDefaultTextEncoding;
  {$IFDEF Windows}
  NonUTF8System:=true;
  {$ELSE}
  NonUTF8System:=SysUtils.CompareText(DefaultTextEncoding,'UTF8')<>0;
  {$ENDIF}
  InitPlatform;
end;

initialization
  InternalInit;
finalization
  FinalizePlatform;
end.

