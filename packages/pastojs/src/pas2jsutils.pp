{$IFNDEF FPC_DOTTEDUNITS}
unit Pas2JSUtils;
{$ENDIF FPC_DOTTEDUNITS}
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

// Check whether we need  the LANG variable
{$IFDEF Unix}
{$IFNDEF Darwin}
{$DEFINE NEEDLANG}
{$ENDIF}
{$ENDIF}
{$IFDEF WASM32}
{$DEFINE NEEDLANG}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

function ChompPathDelim(const Path: string): string;
function GetNextDelimitedItem(const List: string; Delimiter: Char;
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
function UTF8CharacterStrictLength(P: PAnsiChar): integer;

function UTF8ToUTF16(const s: AnsiString): UnicodeString;
function UTF16ToUTF8(const s: UnicodeString): AnsiString;

function UTF8ToSystemCP(const s: ansistring): ansistring;
function SystemCPToUTF8(const s: ansistring): ansistring;

function ConsoleToUTF8(const s: ansistring): ansistring;
// converts UTF8 string to console encoding (used by Write, WriteLn)
function UTF8ToConsole(const s: ansistring): ansistring;
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
function IsEncodingValid : Boolean;

{$IFDEF NEEDLANG}
function GetLang: string;
{$ENDIF}

function GetConsoleTextEncoding: string;

procedure SplitCmdLineParams(const Params: string; ParamList: TStrings;
                             ReadBackslash: boolean = false);

implementation

{$IFDEF Windows}
{$IFDEF FPC_DOTTEDUNITS}
uses WinApi.Windows;
{$ELSE FPC_DOTTEDUNITS}
uses Windows;
{$ENDIF FPC_DOTTEDUNITS}
{$ENDIF}


Var
{$IFDEF NEEDLANG}
  Lang: string = '';
{$ENDIF}
  EncodingValid: boolean = false;
  DefaultTextEncoding: string = EncodingSystem;
  gNonUTF8System : Boolean = {$IFDEF FPC_HAS_CPSTRING}false{$ELSE}true{$ENDIF};

Function NonUTF8System: boolean;

begin
  Result:=gNonUTF8System;
end;

function IsEncodingValid : Boolean;

begin
  Result:=EncodingValid;
end;

{$IFDEF NEEDLANG}
function GetLang: string;

begin
  Result:=Lang;
end;
{$ENDIF}

function GetNextDelimitedItem(const List: string; Delimiter: Char;
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
  p: PAnsiChar;
begin
  if s='' then exit(true);
  p:=PAnsiChar(s);
  repeat
    case p^ of
    #0: if p-PAnsiChar(s)=length(s) then exit(true);
    #128..#255: exit(false);
    end;
    inc(p);
  until false;
end;
{$ENDIF}

{$IFDEF FPC_HAS_CPSTRING}
function UTF8CharacterStrictLength(P: PAnsiChar): integer;
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

function UTF8ToUTF16(const s: AnsiString): UnicodeString;
begin
  Result:=UTF8Decode(s);
end;

function UTF16ToUTF8(const s: UnicodeString): ansistring;
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
      // unix & wasm
      Lang := GetEnvironmentVariable('LC_ALL');
      if Lang='' then
      begin
        Lang := GetEnvironmentVariable('LC_MESSAGES');
        if Lang='' then
          Lang := GetEnvironmentVariable('LANG');
      end;
      {$IFNDEF CPUWASM}
      Result:=GetUnixEncoding;
      {$ELSE} 
      // wasm
      Result:='UTF8'; // some choice needs to be made
      {$ENDIF}
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
  gNonUTF8System:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.CompareText(DefaultTextEncoding,'UTF8')<>0;
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

function GetConsoleTextEncoding: string;
begin
{$IFDEF WINDOWS}
  Result:=GetWindowsEncoding(True);
{$ELSE} 
  Result:=GetDefaultTextEncoding;
{$ENDIF}  
end;

{$IFDEF WINDOWS}

{$ifdef WinCe}
function UTF8ToSystemCP(const s: ansistring): ansistring; inline;
begin
  Result := s;
end;
{$else}
function UTF8ToSystemCP(const s: ansistring): ansistring;
// result has codepage CP_ACP
var
  src: UnicodeString;
  len: LongInt;
begin
  Result:=s;
  if IsASCII(Result) then
  begin
    // prevent codepage conversion magic
    SetCodePage(RawByteString(Result), CP_ACP, False);
    exit;
  end;
  src:=UTF8Decode(s);
  if src='' then
    exit;
  len:=WideCharToMultiByte(CP_ACP,0,PUnicodeChar(src),length(src),nil,0,nil,nil);
  SetLength(Result,len);
  if len>0 then
  begin
    WideCharToMultiByte(CP_ACP,0,PUnicodeChar(src),length(src),@Result[1],length(Result),nil,nil);
    // prevent codepage conversion magic
    SetCodePage(RawByteString(Result), CP_ACP, False);
  end;
end;
{$endif not wince}

{$ifdef WinCE}
function SystemCPToUTF8(const s: ansistring): ansistring; inline;
begin
  Result := SysToUtf8(s);
end;
{$else}
// for all Windows supporting 8bit codepages (e.g. not WinCE)
function SystemCPToUTF8(const s: ansistring): ansistring;
// result has codepage CP_ACP
var
  UTF16WordCnt: SizeInt;
  UTF16Str: UnicodeString;
begin
  Result:=s;
  if IsASCII(Result) then
  begin
    // prevent codepage conversion magic
    SetCodePage(RawByteString(Result), CP_ACP, False);
    exit;
  end;
  UTF16WordCnt:=MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, Pointer(s), length(s), nil, 0);
  // this will null-terminate
  if UTF16WordCnt>0 then
  begin
    setlength(UTF16Str{%H-}, UTF16WordCnt);
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, Pointer(s), length(s), @UTF16Str[1], UTF16WordCnt);
    Result:=UTF16ToUTF8(UTF16Str);
  end;
end;
{$endif not wince}

{$ifdef WinCe}
function UTF8ToConsole(const s: ansistring): ansistring; // converts UTF8 to console string (used by Write, WriteLn)
begin
  Result := UTF8ToSystemCP(s);
end;
{$else}
function UTF8ToConsole(const s: ansistring): ansistring; // converts UTF8 to console string (used by Write, WriteLn)
var
  Dst: PAnsiChar;
begin
  {$ifndef NO_CP_RTL}
  Result := UTF8ToSystemCP(s);
  {$else NO_CP_RTL}
  Result := s; // Kept for compatibility
  {$endif NO_CP_RTL}
  Dst := AllocMem((Length(Result) + 1) * SizeOf(AnsiChar));
  if CharToOEM(PAnsiChar(Result), Dst) then
    Result := StrPas(Dst);
  FreeMem(Dst);
  {$ifndef NO_CP_RTL}
  SetCodePage(RawByteString(Result), CP_OEMCP, False);
  {$endif NO_CP_RTL}
end;
{$endif not WinCE}

{$ifdef WinCE}
function ConsoleToUTF8(const s: ansistring): ansistring;// converts console encoding to UTF8
begin
  Result := SysToUTF8(s);
end;
{$else}
function ConsoleToUTF8(const s: ansistring): ansistring;// converts console encoding to UTF8
var
  Dst: PAnsiChar;
begin
  Dst := AllocMem((Length(s) + 1) * SizeOf(AnsiChar));
  if OemToChar(PAnsiChar(s), Dst) then
    Result := StrPas(Dst)
  else
    Result := s;
  FreeMem(Dst);
  Result := SystemCPToUTF8(Result);
end;
{$endif not wince}

{$ENDIF WINDOWS}

{$IFDEF UNIX}
function UTF8ToSystemCP(const s: Ansistring): Ansistring;
begin
  if NonUTF8System and not IsASCII(s) then
  begin
    Result:=UTF8ToAnsi(s);
    // prevent UTF8 codepage appear in the strings - we don't need codepage
    // conversion magic
    SetCodePage(RawByteString(Result), StringCodePage(s), False);
  end
  else
    Result:=s;
end;

function SystemCPToUTF8(const s: ansistring): ansistring;
begin
  if NonUTF8System and not IsASCII(s) then
  begin
    Result:=AnsiToUTF8(s);
    // prevent UTF8 codepage appear in the strings - we don't need codepage
    // conversion magic
    SetCodePage(RawByteString(Result), StringCodePage(s), False);
  end
  else
    Result:=s;
end;

function ConsoleToUTF8(const s: ansistring): ansistring;
begin
  Result:=SystemCPToUTF8(s);
end;

function UTF8ToConsole(const s: ansistring): ansistring;
begin
  Result:=UTF8ToSystemCP(s);
end;
{$ENDIF UNIX}

{$IF NOT DEFINED(UNIX) AND NOT DEFINED(WINDOWS)}
function UTF8ToSystemCP(const s: Ansistring): Ansistring;
begin
  if NonUTF8System and not IsASCII(s) then
  begin
    Result:=UTF8ToAnsi(s);
    // prevent UTF8 codepage appear in the strings - we don't need codepage
    // conversion magic
    SetCodePage(RawByteString(Result), StringCodePage(s), False);
  end
  else
    Result:=s;
end;

function SystemCPToUTF8(const s: ansistring): ansistring;
begin
  if NonUTF8System and not IsASCII(s) then
  begin
    Result:=AnsiToUTF8(s);
    // prevent UTF8 codepage appear in the strings - we don't need codepage
    // conversion magic
    SetCodePage(RawByteString(Result), StringCodePage(s), False);
  end
  else
    Result:=s;
end;

function ConsoleToUTF8(const s: ansistring): ansistring;
begin
  Result:=SystemCPToUTF8(s);
end;

function UTF8ToConsole(const s: ansistring): ansistring;
begin
  Result:=UTF8ToSystemCP(s);
end;
{$ENDIF}

initialization
  InternalInit;
end.

