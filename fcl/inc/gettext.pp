{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by the Free Pascal development team

    Gettext interface to resourcestrings.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit gettext;

interface

uses sysutils, classes;

const
  MOFileHeaderMagic = $950412de;

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

  TMOStringTable = array[LongWord] of TMOStringInfo;
  PMOStringTable = ^TMOStringTable;


  TLongWordArray = array[LongWord] of LongWord;
  PLongWordArray = ^TLongWordArray;

  TPCharArray = array[LongWord] of PChar;
  PPCharArray = ^TPCharArray;

  TMOFile = class
  protected
    HashTableSize: LongWord;
    HashTable: PLongWordArray;
    OrigTable, TranslTable: PMOStringTable;
    OrigStrings, TranslStrings: PPCharArray;
  public
    constructor Create(AFilename: String);
    constructor Create(AStream: TStream);
    function Translate(AOrig: PChar; ALen: Integer; AHash: LongWord): String;
    function Translate(AOrig: String; AHash: LongWord): String;
    function Translate(AOrig: String): String;
  end;

  EMOFileError = class(Exception)
  end;


  procedure TranslateResourceStrings(AFile: TMOFile);
  procedure TranslateResourceStrings(AFilename: String);

implementation

uses dos;

constructor TMOFile.Create(AStream: TStream);
var
  header: TMOFileHeader;
  i: Integer;
  s: String;
begin
  inherited Create;

  AStream.Read(header, Sizeof(header));

  if header.magic <> MOFileHeaderMagic then
    raise EMOFileError.Create('Invalid magic - not a MO file?');

{  WriteLn('Revision: ', header.revision);
  WriteLn('# of strings: ', header.nstrings);
  WriteLn('OrigTabOffset: ', header.OrigTabOffset);
  WriteLn('TransTabOffset: ', header.TransTabOffset);
  WriteLn('# of hashcodes: ', header.HashTabSize);
  WriteLn('HashTabOffset: ', header.HashTabOffset);
}
  GetMem(OrigTable, header.nstrings * SizeOf(TMOStringInfo));
  GetMem(TranslTable, header.nstrings * SizeOf(TMOStringInfo));
  GetMem(OrigStrings, header.nstrings * SizeOf(PChar));
  GetMem(TranslStrings, header.nstrings * SizeOf(PChar));


  AStream.Position := header.OrigTabOffset;
  AStream.Read(OrigTable^, header.nstrings * SizeOf(TMOStringInfo));

  AStream.Position := header.TransTabOffset;
  AStream.Read(TranslTable^, header.nstrings * SizeOf(TMOStringInfo));


  // Read strings
  for i := 0 to header.nstrings - 1 do begin
    AStream.Position := OrigTable^[i].offset;
    SetLength(s, OrigTable^[i].length);
    AStream.Read(s[1], OrigTable^[i].length);
    OrigStrings^[i] := StrNew(PChar(s));
  end;

  for i := 0 to header.nstrings - 1 do begin
    AStream.Position := TranslTable^[i].offset;
    SetLength(s, TranslTable^[i].length);
    AStream.Read(s[1], TranslTable^[i].length);
    TranslStrings^[i] := StrNew(PChar(s));
  end;

  // Read hashing table
  HashTableSize := header.HashTabSize;
  GetMem(HashTable, 4 * HashTableSize);
  AStream.Position := header.HashTabOffset;
  AStream.Read(HashTable^, 4 * HashTableSize);
end;

constructor TMOFile.Create(AFilename: String);
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


function TMOFile.Translate(AOrig: PChar; ALen: Integer; AHash: LongWord): String;
var
  idx, incr, nstr: LongWord;
begin
  idx := AHash mod HashTableSize;
  incr := 1 + (AHash mod (HashTableSize - 2));
  while True do begin
    nstr := HashTable^[idx];
    if nstr = 0 then begin
      Result := '';
      exit;
    end;
    if (OrigTable^[nstr - 1].length = ALen) and
       (StrComp(OrigStrings^[nstr - 1], AOrig) = 0) then begin
      Result := TranslStrings^[nstr - 1];
      exit;
    end;
    if idx >= HashTableSize - incr then
      Dec(idx, HashTableSize - incr)
    else
      Inc(idx, incr);
  end;
end;

function TMOFile.Translate(AOrig: String; AHash: LongWord): String;
begin
  Result := Translate(PChar(AOrig), Length(AOrig), AHash);
end;

function TMOFile.Translate(AOrig: String): String;
begin
  Result := Translate(AOrig, Hash(AOrig));
end;


// -------------------------------------------------------
//   Resourcestring translation procedures
// -------------------------------------------------------

{
  Define USEITERATOR if you want to translate the strings using
  the SetResourceStrings call. This is not recommended for this
  particular iplementation, since we must pass through a global 
  variable TheFile : TMOFile. However that works too.
}

{$ifdef USEITERATOR}
Var 
  Thefile : TMOFile;

Function Translate (Name,Value : AnsiString; Hash : Longint) : AnsiString;

begin
  Result:=TheFile.Translate(Value,Hash);
end;

procedure TranslateResourceStrings(AFile: TMOFile);
var
  i,j : Integer;
  s : String;
begin
  TheFile:=AFile;
  SetResourceStrings(@Translate);
end;
{$else}

procedure TranslateResourceStrings(AFile: TMOFile);
var
  i,j,count : Integer;
  s : String;
begin
  For I:=0 to ResourceStringTableCount-1 do
    begin
    Count:=ResourceStringCount(I);
    For J:=0 to Count-1 do
      begin
      S:=AFile.Translate(GetResourceStringDefaultValue(I,J),
                         GetResourceStringHash(I,J));
      if S <> '' then
        SetResourceStringValue(I,J,S);
      end;
    end;
end;
{$endif}

procedure TranslateResourceStrings(AFilename: String);
var
  mo: TMOFile;
  lang: String;
begin
  lang := Copy(GetEnv('LANG'), 1, 2);
  try
    mo := TMOFile.Create(Format(AFilename, [lang]));
    TranslateResourceStrings(mo);
    mo.Free;
  except
    on e: Exception do;
  end;
end;

end.


{
  $Log$
  Revision 1.7  2000-01-06 01:20:33  peter
    * moved out of packages/ back to topdir

  Revision 1.1  2000/01/03 19:33:07  peter
    * moved to packages dir

  Revision 1.5  1999/10/15 19:42:18  michael
  hash is available in tables

  Revision 1.4  1999/08/28 13:35:16  michael
  * Uses now hash function of objpas

  Revision 1.3  1999/08/27 15:53:36  michael
  + Adapted to new resourcestring mechanism. Uses objpas interface only

  Revision 1.2  1999/08/26 11:05:15  peter
    * updated for new resourcestrings

  Revision 1.1  1999/08/04 11:31:09  michael
  * Added gettext

  Revision 1.1  1999/07/25 16:23:31  michael
  + Initial implementation from Sebastian Guenther

}
