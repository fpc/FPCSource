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


  function CalcHash(s: String): LongWord;

  procedure TranslateResourceStrings(AFile: TMOFile);
  procedure TranslateResourceStrings(AFilename: String);

implementation

uses dos;


function CalcHash(s: String): LongWord;
var
  g, i : LongWord;
begin
  Result := 0;
  for i := 1 to Length(s) do begin
    Result := Result shl 4 + Ord(s[i]);
    g := Result and ($f shl 28);
    if g <> 0 then
      Result := (Result xor (g shr 24)) xor g;
  end;
  if Result = 0 then Result := not 0;
end;


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
  Result := Translate(AOrig, CalcHash(AOrig));
end;


// -------------------------------------------------------
//   Resourcestring translation procedures
// -------------------------------------------------------

type
  PResourceStringRecord = ^TResourceStringRecord;
  TResourceStringRecord = Packed Record
     DefaultValue,
     CurrentValue : AnsiString;
     HashValue : longint;
     Name : AnsiString;
   end;

   TResourceStringTable = Packed Record
     Count : longint;
     Resrec : Array[Word] of TResourceStringRecord;
   end;
   PResourceStringTable = ^TResourceStringTable;

   TResourceTableList = Packed Record
     Count : longint;
     Tables : Array[Word] of PResourceStringTable;
   end;

Var
  ResourceStringTable : TResourceTablelist; External Name 'FPC_RESOURCESTRINGTABLES';


procedure TranslateResourceStrings(AFile: TMOFile);
var
  i,j : Integer;
  s : String;
begin
  With ResourceStringTable do
    For I:=0 to Count-1 do
      With Tables[I]^ do
         For J:=0 to Count-1 do
           With ResRec[J] do
            begin
              // WriteLn(j, ': ', DefaultValue, ' / ', CurrentValue, ' / ', HashValue);
              s := AFile.Translate(DefaultValue);
              if s <> '' then
               CurrentValue := s;
            end;
end;

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
  Revision 1.2  1999-08-26 11:05:15  peter
    * updated for new resourcestrings

  Revision 1.1  1999/08/04 11:31:09  michael
  * Added gettext

  Revision 1.1  1999/07/25 16:23:31  michael
  + Initial implementation from Sebastian Guenther

}
