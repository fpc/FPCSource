{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by the Free Pascal development team

    Gettext interface to resourcestrings.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

{$IFNDEF FPC_DOTTEDUNITS}
unit gettext;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes;
{$ELSE FPC_DOTTEDUNITS}
uses SysUtils, Classes;
{$ENDIF FPC_DOTTEDUNITS}

const
  MOFileHeaderMagic = $950412DE;

type
  TMOFileHeader = packed record
    magic: LongWord;             // MOFileHeaderMagic
    revision: LongWord;          // 0
    nstrings: LongWord;          // Number of string pairs
    OrigTabOffset: LongWord;     // Offset of original string offset table
    TransTabOffset: LongWord;    // Offset of translated string offset table
    HashTabSize: LongWord;       // Size of hashing table
    HashTabOffset: LongWord;     // Offset of first hashing table entry
  end;

  TMOStringInfo = packed record
    length: LongWord;
    offset: LongWord;
  end;

  TMOStringTable = array[0..(1 shl 30) div SizeOf(TMOStringInfo)] of TMOStringInfo;
  PMOStringTable = ^TMOStringTable;


  TLongWordArray = array[0..(1 shl 30) div SizeOf(LongWord)] of LongWord;
  PLongWordArray = ^TLongWordArray;

  TPCharArray = array[0..(1 shl 30) div SizeOf(PAnsiChar)] of PAnsiChar;
  PPCharArray = ^TPCharArray;

  TMOFile = class
  protected
    StringCount, HashTableSize: LongWord;
    HashTable: PLongWordArray;
    OrigTable, TranslTable: PMOStringTable;
    OrigStrings, TranslStrings: PPCharArray;
  public
    constructor Create(const AFilename: String);
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    function Translate(AOrig: PAnsiChar; ALen: Integer; AHash: LongWord): RTLString;
    function Translate(const AOrig: RTLString; AHash: LongWord): RTLString;
    function Translate(const AOrig: RTLString): RTLString;
  end;

  EMOFileError = class(Exception);


  procedure GetLanguageIDs(var Lang, FallbackLang: AnsiString);
  procedure TranslateResourceStrings(AFile: TMOFile);
  procedure TranslateUnitResourceStrings(const AUnitName:AnsiString; AFile: TMOFile);
  procedure TranslateResourceStrings(const AFilename: AnsiString);
  procedure TranslateUnitResourceStrings(const AUnitName:AnsiString; const AFilename: AnsiString);

Type
  TTranslationErrorHandler = Procedure (const aFileName, aUnitName : String; aError : Exception; Out ReRaise : Boolean);

Var
  OnTranslationError : TTranslationErrorHandler = Nil;

implementation

{$ifdef Windows}

{$IFDEF FPC_DOTTEDUNITS}
uses
   WinApi.Windows;
{$ELSE FPC_DOTTEDUNITS}
uses
   windows;
{$ENDIF FPC_DOTTEDUNITS}

{$endif}


procedure Endianfixmotable(p:PMOStringTable;n:integer);
var I:integer;
begin
  if n>0 then
    for i:=0 to n-1 do
      begin 
        p^[i].length:=swapendian(p^[i].length);
        p^[i].offset:=swapendian(p^[i].offset);
      end;
end;

procedure Endianfixhashtable(p:PLongwordArray;n:integer);
var I:integer;
begin
  if n>0 then
    for i:=0 to n-1 do
      begin 
        p^[i]:=swapendian(p^[i]);
      end;
end;

constructor TMOFile.Create(AStream: TStream);
var
  header: TMOFileHeader;
  i: Integer;
  endianswap : boolean;

begin
  inherited Create;

  AStream.Read(header, Sizeof(header));

  if (header.magic <> MOFileHeaderMagic) and (swapendian(header.magic)<>MOFileHeaderMagic) then
    raise EMOFileError.Create('Invalid magic - not a MO file?');

  endianswap:=header.magic<>MOFileHeaderMagic;
  If EndianSwap then 
    begin
     with header do
       begin 
          revision	:=SwapEndian(revision);
          nstrings	:=SwapEndian(nstrings);
          OrigTabOffset :=SwapEndian(OrigTabOffset);
          TransTabOffset:=SwapEndian(TransTabOffset);
          HashTabSize   :=SwapEndian(HashTabSize);
          HashTabOffset :=SwapEndian(HashTabOffset);
       end;
    end;

  GetMem(OrigTable, header.nstrings * SizeOf(TMOStringInfo));
  GetMem(TranslTable, header.nstrings * SizeOf(TMOStringInfo));
  GetMem(OrigStrings, header.nstrings * SizeOf(PAnsiChar));
  GetMem(TranslStrings, header.nstrings * SizeOf(PAnsiChar));


  AStream.Position := header.OrigTabOffset;
  AStream.Read(OrigTable^, header.nstrings * SizeOf(TMOStringInfo));
  if EndianSwap then 
    EndianFixmotable(OrigTable,Header.NStrings);

  AStream.Position := header.TransTabOffset;
  AStream.Read(TranslTable^, header.nstrings * SizeOf(TMOStringInfo));
  if EndianSwap then 
    EndianFixmotable(TranslTable,Header.NStrings);

  StringCount := header.nstrings;

  // Read strings
  for i := 0 to StringCount - 1 do
  begin
    AStream.Position := OrigTable^[i].offset;
{    SetLength(s, OrigTable^[i].length);
    AStream.Read(s[1], OrigTable^[i].length);
    OrigStrings^[i] := StrNew(PAnsiChar(s));}
    GetMem(OrigStrings^[i], OrigTable^[i].length + 1);
    AStream.Read(OrigStrings^[i]^, OrigTable^[i].length);
    OrigStrings^[i][OrigTable^[i].length] := #0;
  end;

  for i := 0 to StringCount - 1 do
  begin
    AStream.Position := TranslTable^[i].offset;
{    SetLength(s, TranslTable^[i].length);
    AStream.Read(s[1], TranslTable^[i].length);
    TranslStrings^[i] := StrNew(PAnsiChar(s));}
    GetMem(TranslStrings^[i], TranslTable^[i].length+1);
    AStream.Read(TranslStrings^[i]^, TranslTable^[i].length);
    TranslStrings^[i][TranslTable^[i].length] := #0;
  end;

  // Read hashing table
  HashTableSize := header.HashTabSize;
  GetMem(HashTable, 4 * HashTableSize);
  AStream.Position := header.HashTabOffset;
  AStream.Read(HashTable^, 4 * HashTableSize);
  if EndianSwap then 
    EndianFixHashTable(hashtable,hashtablesize);
end;

constructor TMOFile.Create(const AFilename: String);
var
  f: TStream;
begin
  f := TFileStream.Create(AFilename, fmOpenRead);
  try
    Self.Create(f);
  finally
    f.Free;
  end;
end;

destructor TMOFile.Destroy;
var
  i: Integer;
begin
  for i := 0 to StringCount - 1 do
  begin
    FreeMem(OrigStrings^[i]);
    FreeMem(TranslStrings^[i]);
  end;
  FreeMem(OrigTable);
  FreeMem(TranslTable);
  FreeMem(OrigStrings);
  FreeMem(TranslStrings);
  FreeMem(HashTable);
  inherited Destroy;
end;

function TMOFile.Translate(AOrig: PAnsiChar; ALen: Integer; AHash: LongWord):RTLString;
var
  idx, incr, nstr: LongWord;
begin
  Result := '';
  if AHash = $FFFFFFFF then
    exit;
  idx := AHash mod HashTableSize;
  incr := 1 + (AHash mod (HashTableSize - 2));
  while True do
  begin
    nstr := HashTable^[idx];
    if (nstr = 0) or (nstr > StringCount) then
      Break;
    if (OrigTable^[nstr - 1].length = LongWord(ALen)) and
       (StrComp(OrigStrings^[nstr - 1], AOrig) = 0) then
    begin
      Result := TranslStrings^[nstr - 1];
      Break;
    end;
    if idx >= HashTableSize - incr then
      Dec(idx, HashTableSize - incr)
    else
      Inc(idx, incr);
  end;
  if Result<>'' then
    exit;
end;

function TMOFile.Translate(const AOrig:RTLString ; AHash: LongWord): RTLString;

Var
  SOrig : UTF8String;

begin
  SOrig:=UTF8Encode(aOrig);
  Result := Translate(PAnsiChar(SOrig), Length(AOrig), AHash);
end;

function TMOFile.Translate(const AOrig:RTLString ):RTLString;

begin
  Result := Translate(AOrig, Hash(AOrig));
end;


// -------------------------------------------------------
//   Resourcestring translation procedures
// -------------------------------------------------------


function Translate (Name : AnsiString; Value : RTLString; Hash : Longint; arg:pointer) : RTLString;
var contextempty : boolean;
begin
  contextempty:=name='';
  Result:='';
  if not contextempty then
    Result:=TMOFile(arg).Translate(Name+#4+Value);
  if contextempty or (Result='') then
    Result:=TMOFile(arg).Translate(Value,Hash);
end;


procedure TranslateResourceStrings(AFile: TMOFile);
begin
  SetResourceStrings(@Translate,AFile);
end;


procedure TranslateUnitResourceStrings(const AUnitName:AnsiString; AFile: TMOFile);
begin
  SetUnitResourceStrings(AUnitName,@Translate,AFile);
end;


{$ifdef windows}
procedure GetLanguageIDs(var Lang, FallbackLang:AnsiString );
var
  Buffer: array[1..4] of {$ifdef Wince}WideChar{$else}AnsiChar{$endif};
  Country: AnsiString;
  UserLCID: LCID;
begin
  //defaults
  Lang := '';
  FallbackLang:='';
  UserLCID := GetUserDefaultLCID;
  if GetLocaleInfo(UserLCID, LOCALE_SABBREVLANGNAME, @Buffer[1], 4)<>0 then begin
    FallbackLang := lowercase(copy(Buffer,1,2));

    // Chinese abbreviation should be zh instead of ch
    if (Copy(Buffer,1,3)='CHS') or (Copy(Buffer,1,3)='CHT') then FallbackLang:='zh';
  end;
  if GetLocaleInfo(UserLCID, LOCALE_SABBREVCTRYNAME, @Buffer[1], 4)<>0 then begin
    Country := copy(Buffer,1,2);

    // some 2 letter codes are not the first two letters of the 3 letter code
    // there are probably more, but first let us see if there are translations
    if (Buffer='PRT') then Country:='PT';
    
    if (Copy(Buffer,1,3)='CHN') then Country:='CN';  

    Lang := FallbackLang+'_'+Country;
  end;
end;

{$else}

procedure GetLanguageIDs(var Lang, FallbackLang: AnsiString);
begin
  FallbackLang:='';
  lang := GetEnvironmentVariable('LC_ALL');
  if Length(lang) = 0 then
  begin
    lang := GetEnvironmentVariable('LC_MESSAGES');
    if Length(lang) = 0 then
    begin
      lang := GetEnvironmentVariable('LANG');
      if Length(lang) = 0 then
        exit;   // no language defined via environment variables
    end;
  end;
  FallbackLang := Copy(lang, 1, 2);
end;
{$endif}

Function DoReRaise(const aFileName, aUnitName : String; E : Exception) : boolean;

begin
  Result:=False;
  if Assigned(OnTranslationError) then
    OnTranslationError(aFileName,aUnitName,E,Result);
end;

procedure TranslateResourceStrings(const AFilename: AnsiString);

  
var
  mo: TMOFile;
  lang, FallbackLang: AnsiString;
  fn: AnsiString;
begin
  GetLanguageIDs(Lang, FallbackLang);
  fn:=Format(AFilename, [FallbackLang]);

  if fileexists(fn) then
    begin
      try
        mo := TMOFile.Create(fn);
        try
          TranslateResourceStrings(mo);
        finally
          mo.Free;
        end;
      except
        on e: Exception do 
          if DoReRaise(FN,'',E) then
            Raise ;
      end;
    end;
  lang := Copy(lang, 1, 5);
  fn:=Format(AFilename, [lang]);
  if fileexists(fn) then
    begin
      try
        mo := TMOFile.Create(Format(AFilename, [lang]));
        try
          TranslateResourceStrings(mo);
        finally
          mo.Free;
        end;
      except
        on e: Exception do
          if DoReRaise(FN,'',E) then
            Raise ;
      end;
    end;
end;


procedure TranslateUnitResourceStrings(const AUnitName:AnsiString; const AFilename: AnsiString);
var
  mo: TMOFile;
  FN : String;
  lang, FallbackLang: AnsiString;
begin
  GetLanguageIDs(Lang, FallbackLang);
  try
    FN := Format(AFilename, [FallbackLang]);
    mo := TMOFile.Create(FN);
    try
      TranslateUnitResourceStrings(AUnitName,mo);
    finally
      mo.Free;
    end;
  except
    on e: Exception do
      if DoReRaise(FN,aUnitName,E) then
        Raise ;
  end;

  lang := Copy(lang, 1, 5);
  try
    FN := Format(AFilename, [FallbackLang]);
    mo := TMOFile.Create(FN);
    try
      TranslateUnitResourceStrings(AUnitName,mo);
    finally
      mo.Free;
    end;
  except
    on e: Exception do
      if DoReRaise(FN,aUnitName,E) then
        Raise ;

  end;
end;

finalization
  finalizeresourcetables;
end.
