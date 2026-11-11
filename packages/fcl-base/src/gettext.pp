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
unit GetText;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Types;
{$ELSE FPC_DOTTEDUNITS}
uses SysUtils, Classes, Types;
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


  // Returns locale aLocale without codeset and modifier, e.g. 'es_ES' for 'es_ES.UTF-8'; empty for C and POSIX.
  function NormalizeLanguageID(const aLocale: AnsiString): AnsiString;
  // Returns the language part of language ID aID, e.g. 'ast' for 'ast_ES'.
  function LanguageIDLanguage(const aID: AnsiString): AnsiString;
  // Returns the language IDs for the values of LANGUAGE, LC_ALL, LC_MESSAGES and LANG, most preferred first.
  function GetLanguageIDList(const aLanguage, aLCAll, aLCMessages, aLang: AnsiString): TStringDynArray;
  // Returns the language IDs of the current user, most preferred first.
  function GetLanguageIDList: TStringDynArray;
  // Returns the most preferred language ID of the current user and its language part.
  procedure GetLanguageIDs(var Lang, FallbackLang: AnsiString);
{$ifdef windows}
  // Returns the ISO 639 language and ISO 3166 country codes of Windows locale aLocale, e.g. 'sv_SE' and 'sv'.
  procedure GetLanguageIDs(aLocale: LongWord; var aLang, aFallbackLang: AnsiString);
{$endif}
  procedure TranslateResourceStrings(AFile: TMOFile);
  procedure TranslateUnitResourceStrings(const AUnitName:AnsiString; AFile: TMOFile);
  procedure TranslateResourceStrings(const AFilename: AnsiString);
  procedure TranslateUnitResourceStrings(const AUnitName:AnsiString; const AFilename: AnsiString);
  // Applies the existing files Format(AFilename,[ID]) for aLanguageIDs, the first (most preferred) ID last.
  procedure TranslateResourceStrings(const AFilename: AnsiString; const aLanguageIDs: TStringDynArray);
  // Applies the existing files Format(AFilename,[ID]) for aLanguageIDs to unit AUnitName, the first ID last.
  procedure TranslateUnitResourceStrings(const AUnitName, AFilename: AnsiString; const aLanguageIDs: TStringDynArray);

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
    Result:=TMOFile(arg).Translate(Value,LongWord(Hash));
end;


procedure TranslateResourceStrings(AFile: TMOFile);
begin
  SetResourceStrings(@Translate,AFile);
end;


procedure TranslateUnitResourceStrings(const AUnitName:AnsiString; AFile: TMOFile);
begin
  SetUnitResourceStrings(AUnitName,@Translate,AFile);
end;


function NormalizeLanguageID(const aLocale: AnsiString): AnsiString;

var
  lPos: Integer;

begin
  Result:=Trim(aLocale);
  lPos:=Pos('.',Result);
  if lPos>0 then
    SetLength(Result,lPos-1);
  lPos:=Pos('@',Result);
  if lPos>0 then
    SetLength(Result,lPos-1);
  if (Result='C') or (Result='POSIX') then
    Result:='';
end;


function LanguageIDLanguage(const aID: AnsiString): AnsiString;

var
  lPos: Integer;

begin
  Result:=aID;
  lPos:=Pos('_',Result);
  if lPos>0 then
    SetLength(Result,lPos-1);
end;


// Appends aID to aList unless it is empty or already present.
procedure AddLanguageID(var aList: TStringDynArray; const aID: AnsiString);

var
  i: Integer;

begin
  if aID='' then
    exit;
  for i:=0 to Length(aList)-1 do
    if aList[i]=aID then
      exit;
  i:=Length(aList);
  SetLength(aList,i+1);
  aList[i]:=aID;
end;


function GetLanguageIDList(const aLanguage, aLCAll, aLCMessages, aLang: AnsiString): TStringDynArray;

var
  lLocale, lEntries, lID: AnsiString;
  lPos: Integer;

begin
  Result:=nil;
  lLocale:=aLCAll;
  if lLocale='' then
    lLocale:=aLCMessages;
  if lLocale='' then
    lLocale:=aLang;
  if NormalizeLanguageID(lLocale)='' then
    exit;
  if aLanguage<>'' then
    lEntries:=aLanguage
  else
    lEntries:=lLocale;
  while lEntries<>'' do
    begin
    lPos:=Pos(':',lEntries);
    if lPos=0 then
      lPos:=Length(lEntries)+1;
    lID:=NormalizeLanguageID(Copy(lEntries,1,lPos-1));
    Delete(lEntries,1,lPos);
    AddLanguageID(Result,lID);
    AddLanguageID(Result,LanguageIDLanguage(lID));
    end;
end;


{$ifdef windows}
// Returns locale information aType of locale aLocale, or an empty string.
function GetLocaleString(aLocale: LCID; aType: LCTYPE): AnsiString;

var
  lBuffer: array[0..8] of {$ifdef wince}WideChar{$else}AnsiChar{$endif};
  lLen: Integer;
{$ifdef wince}
  lValue: UnicodeString;
{$endif}

begin
  Result:='';
  lLen:=GetLocaleInfo(aLocale,aType,@lBuffer[0],Length(lBuffer));
  if lLen<=1 then
    exit;
{$ifdef wince}
  SetString(lValue,PWideChar(@lBuffer[0]),lLen-1);
  Result:=AnsiString(lValue);
{$else}
  SetString(Result,PAnsiChar(@lBuffer[0]),lLen-1);
{$endif}
end;


procedure GetLanguageIDs(aLocale: LongWord; var aLang, aFallbackLang: AnsiString);

var
  lCountry: AnsiString;

begin
  aLang:='';
  aFallbackLang:=GetLocaleString(aLocale,LOCALE_SISO639LANGNAME);
  lCountry:=GetLocaleString(aLocale,LOCALE_SISO3166CTRYNAME);
  if (aFallbackLang<>'') and (lCountry<>'') then
    aLang:=aFallbackLang+'_'+lCountry;
end;


procedure GetLanguageIDs(var Lang, FallbackLang:AnsiString );
begin
  GetLanguageIDs(GetUserDefaultLCID,Lang,FallbackLang);
end;


function GetLanguageIDList: TStringDynArray;

var
  lLang, lFallbackLang: AnsiString;

begin
  Result:=nil;
  GetLanguageIDs(lLang,lFallbackLang);
  AddLanguageID(Result,lLang);
  AddLanguageID(Result,lFallbackLang);
end;

{$else}

function GetLanguageIDList: TStringDynArray;

begin
  Result:=GetLanguageIDList(GetEnvironmentVariable('LANGUAGE'),GetEnvironmentVariable('LC_ALL'),
                            GetEnvironmentVariable('LC_MESSAGES'),GetEnvironmentVariable('LANG'));
end;


procedure GetLanguageIDs(var Lang, FallbackLang: AnsiString);

var
  lList: TStringDynArray;

begin
  Lang:='';
  FallbackLang:='';
  lList:=GetLanguageIDList;
  if Length(lList)=0 then
    exit;
  Lang:=lList[0];
  FallbackLang:=LanguageIDLanguage(Lang);
end;
{$endif}

Function DoReRaise(const aFileName, aUnitName : String; E : Exception) : boolean;

begin
  Result:=False;
  if Assigned(OnTranslationError) then
    OnTranslationError(aFileName,aUnitName,E,Result);
end;

// Applies the existing files Format(aFilename,[ID]) for aLanguageIDs, last ID first; all units if aUnitName is empty.
procedure TranslateFromFiles(const aUnitName, aFilename: AnsiString; const aLanguageIDs: TStringDynArray);

var
  mo: TMOFile;
  fn: AnsiString;
  i: Integer;

begin
  for i:=Length(aLanguageIDs)-1 downto 0 do
    begin
    fn:=Format(aFilename,[aLanguageIDs[i]]);
    if not FileExists(fn) then
      continue;
    try
      mo:=TMOFile.Create(fn);
      try
        if aUnitName='' then
          TranslateResourceStrings(mo)
        else
          TranslateUnitResourceStrings(aUnitName,mo);
      finally
        mo.Free;
      end;
    except
      on e: Exception do
        if DoReRaise(fn,aUnitName,E) then
          Raise;
    end;
    end;
end;


procedure TranslateResourceStrings(const AFilename: AnsiString; const aLanguageIDs: TStringDynArray);

begin
  TranslateFromFiles('',AFilename,aLanguageIDs);
end;


procedure TranslateUnitResourceStrings(const AUnitName, AFilename: AnsiString; const aLanguageIDs: TStringDynArray);

begin
  if AUnitName<>'' then
    TranslateFromFiles(AUnitName,AFilename,aLanguageIDs);
end;


procedure TranslateResourceStrings(const AFilename: AnsiString);

begin
  TranslateFromFiles('',AFilename,GetLanguageIDList);
end;


procedure TranslateUnitResourceStrings(const AUnitName:AnsiString; const AFilename: AnsiString);

begin
  TranslateUnitResourceStrings(AUnitName,AFilename,GetLanguageIDList);
end;

end.
