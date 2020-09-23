unit Pas2JSUtils;
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
    Utility routines that do not need a filesystem or OS functionality.
    Filesystem-specific things should go to pas2jsfileutils instead.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ChompPathDelim(const Path: string): string;
function GetNextDelimitedItem(const List: string; Delimiter: char;
                              var Position: integer): string;
type
   TChangeStamp = SizeInt;

const
  InvalidChangeStamp = low(TChangeStamp);

Function IncreaseChangeStamp(Stamp: TChangeStamp) : TChangeStamp;
const
  EncodingUTF8 = 'UTF-8';
  EncodingSystem = 'System';

function NormalizeEncoding(const Encoding: string): string;
function IsASCII(const s: string): boolean; inline;
{$IFDEF FPC_HAS_CPSTRING}
const
  UTF8BOM = #$EF#$BB#$BF;
function UTF8CharacterStrictLength(P: PChar): integer;

function UTF8ToUTF16(const s: string): UnicodeString;
function UTF16ToUTF8(const s: UnicodeString): string;

{$ENDIF FPC_HAS_CPSTRING}

function IsNonUTF8System: boolean;// true if system encoding is not UTF-8
{$IFDEF Windows}
// AConsole - If false, it is the general system encoding,
//            if true, it is the console encoding
function GetWindowsEncoding(AConsole: Boolean = False): string;
{$ENDIF}
{$IF defined(Unix) and not defined(Darwin)}
function GetUnixEncoding: string;
{$ENDIF}

Function NonUTF8System: boolean;
function GetDefaultTextEncoding: string;

procedure SplitCmdLineParams(const Params: string; ParamList: TStrings;
                             ReadBackslash: boolean = false);

implementation

{$IFDEF Windows}
uses Windows;
{$ENDIF}

Var
  {$IFDEF Unix}
  {$IFNDEF Darwin}
  Lang: string = '';
  {$ENDIF}
  {$ENDIF}
  EncodingValid: boolean = false;
  DefaultTextEncoding: string = EncodingSystem;
  gNonUTF8System : Boolean = {$IFDEF FPC_HAS_CPSTRING}false{$ELSE}true{$ENDIF};

Function NonUTF8System: boolean;

begin
  Result:=gNonUTF8System;
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

function IncreaseChangeStamp(Stamp: TChangeStamp): TChangeStamp;
begin
  if Stamp<High(TChangeStamp) then
    Result:=Stamp+1
  else
    Result:=InvalidChangeStamp+1;
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
    {$IFDEF Pas2js}
    if (Len >= 2) and (Result[2]=Result[1]) and (PathDelim='\') then
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
    {$IFdef Pas2js}
    if (PathDelim='\')
        and (Len >= 3) and (Result[1] in ['a'..'z', 'A'..'Z'])
        and (Result[2] = ':') and (Result[3] in AllowDirectorySeparators)
    then
      MinLen := 3;
    {$ENDIF}
  end;

  while (Len > MinLen) and (Result[Len] in AllowDirectorySeparators) do dec(Len);
  if Len<length(Result) then
    SetLength(Result,Len);
end;

function NormalizeEncoding(const Encoding: string): string;
var
  i: Integer;
begin
  Result:=LowerCase(Encoding);
  for i:=length(Result) downto 1 do
    if Result[i]='-' then Delete(Result,i,1);
end;

{$IFDEF WINDOWS}
function GetWindowsEncoding(AConsole: Boolean = False): string;
var
  cp : UINT;
{$IFDEF WinCE}
// CP_UTF8 is missing in the windows unit of the Windows CE RTL
const
  CP_UTF8 = 65001;
{$ENDIF}
begin
  if AConsole then cp := GetOEMCP
  else cp := GetACP;

  case cp of
    CP_UTF8: Result := EncodingUTF8;
  else
    Result:='cp'+IntToStr(cp);
  end;
end;
{$ENDIF}

function IsASCII(const s: string): boolean; inline;
{$IFDEF Pas2js}
var
  i: Integer;
begin
  for i:=1 to length(s) do
    if s[i]>#127 then exit(false);
  Result:=true;
end;
{$ELSE}
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
{$ENDIF}

{$IFDEF FPC_HAS_CPSTRING}
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

function IsNonUTF8System: boolean;
begin
  Result:=NonUTF8System;
end;

{$IFDEF UNIX}
{$IFNDEF Darwin}
function GetUnixEncoding: string;
var
  i: integer;
begin
  Result:=EncodingSystem;
  i:=pos('.',Lang);
  if (i>0) and (i<=length(Lang)) then
    Result:=copy(Lang,i+1,length(Lang)-i);
end;
{$ENDIF}
{$ENDIF}

function GetDefaultTextEncoding: string;


begin
  if EncodingValid then
  begin
    Result:=DefaultTextEncoding;
    exit;
  end;

  {$IFDEF Pas2js}
  Result:=EncodingUTF8;
  {$ELSE}
    {$IFDEF Windows}
    Result:=GetWindowsEncoding;
    {$ELSE}
      {$IFDEF Darwin}
      Result:=EncodingUTF8;
      {$ELSE}
      // unix
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
  {$ENDIF}
  Result:=NormalizeEncoding(Result);

  DefaultTextEncoding:=Result;
  EncodingValid:=true;
end;

procedure InternalInit;
begin
  {$IFDEF FPC_HAS_CPSTRING}
  SetMultiByteConversionCodePage(CP_UTF8);
  // SetMultiByteFileSystemCodePage(CP_UTF8); not needed, this is the default under Windows
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);

  GetDefaultTextEncoding;
  {$IFDEF Windows}
  gNonUTF8System:=true;
  {$ELSE}
  gNonUTF8System:=SysUtils.CompareText(DefaultTextEncoding,'UTF8')<>0;
  {$ENDIF}
  {$ENDIF}
end;
procedure SplitCmdLineParams(const Params: string; ParamList: TStrings;
                             ReadBackslash: boolean = false);
// split spaces, quotes are parsed as single parameter
// if ReadBackslash=true then \" is replaced to " and not treated as quote
// #0 is always end
type
  TMode = (mNormal,mApostrophe,mQuote);
var
  p: Integer;
  Mode: TMode;
  Param: String;
begin
  p:=1;
  while p<=length(Params) do
  begin
    // skip whitespace
    while (p<=length(Params)) and (Params[p] in [' ',#9,#10,#13]) do inc(p);
    if (p>length(Params)) or (Params[p]=#0) then
      break;
    // read param
    Param:='';
    Mode:=mNormal;
    while p<=length(Params) do
    begin
      case Params[p] of
      #0:
        break;
      '\':
        begin
          inc(p);
          if ReadBackslash then
            begin
            // treat next character as normal character
            if (p>length(Params)) or (Params[p]=#0) then
              break;
            if ord(Params[p])<128 then
            begin
              Param+=Params[p];
              inc(p);
            end else begin
              // next character is already a normal character
            end;
          end else begin
            // treat backslash as normal character
            Param+='\';
          end;
        end;
      '''':
        begin
          inc(p);
          case Mode of
          mNormal:
            Mode:=mApostrophe;
          mApostrophe:
            Mode:=mNormal;
          mQuote:
            Param+='''';
          end;
        end;
      '"':
        begin
          inc(p);
          case Mode of
          mNormal:
            Mode:=mQuote;
          mApostrophe:
            Param+='"';
          mQuote:
            Mode:=mNormal;
          end;
        end;
      ' ',#9,#10,#13:
        begin
          if Mode=mNormal then break;
          Param+=Params[p];
          inc(p);
        end;
      else
        Param+=Params[p];
        inc(p);
      end;
    end;
    //writeln('SplitCmdLineParams Param=#'+Param+'#');
    ParamList.Add(Param);
  end;
end;


initialization
  InternalInit;
end.

