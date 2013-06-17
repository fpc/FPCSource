{   UnicodeSet implementation.

    Copyright (c) 2013 by Inoussa OUEDRAOGO

    The source code is distributed under the Library GNU
    General Public License with the following modification:

        - object files and libraries linked into an application may be
          distributed without source code.

    If you didn't receive a copy of the file COPYING, contact:
          Free Software Foundation
          675 Mass Ave
          Cambridge, MA  02139
          USA

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit unicodeset;

{$mode delphi}{$H+}
{$scopedenums on}

interface

uses
  SysUtils,
  grbtree, helper;

type

  EUnicodeSetException = class(Exception)
  end;

  TUnicodeSet = class;

  TPatternParser = class
  private
    FBufferStr : UnicodeString;
    FBuffer : PUnicodeChar;
    FBufferLength : Integer;
    FSet : TUnicodeSet;
    FPosition : Integer;
  private
    procedure Error(const AMsg : string; const AArgs : array of const);overload;inline;
    procedure Error(const AMsg : string);overload;inline;
    procedure SetBuffer(const APattern : PUnicodeChar; const ALength : Integer);
    procedure CheckEOF();inline;overload;
    procedure CheckEOF(ALength : Integer);overload;inline;
    procedure UnexpectedEOF();inline;
    function IsThis(AItem : UnicodeString; const APosition : Integer) : Boolean;overload;
    function IsThis(AItem : UnicodeString) : Boolean;overload;inline;
    procedure Expect(AItem : UnicodeString; const APosition : Integer);overload;inline;
    procedure Expect(AItem : UnicodeString);overload;inline;
    procedure SkipSpaces();inline;
    function NextChar() : TUnicodeCodePoint;
    procedure ParseItem();
    procedure DoParse();
  public
    procedure Parse(const APattern : PUnicodeChar; const ALength : Integer);overload;
    procedure Parse(const APattern : UnicodeString);overload;inline;
    property CurrentSet : TUnicodeSet read FSet write FSet;
  end;

  TUnicodeCodePointArrayComparator = class
  public
    // Return
    //    * if A>B then  1
    //    * if A=B then  0
    //    * if A<B then -1
    class function Compare(const A, B : TUnicodeCodePointArray) : Integer;static;inline;
  end;

  TUnicodeSet = class
  private type
      TItem = TUnicodeCodePointArray;
      TTree = TRBTree<TItem,TUnicodeCodePointArrayComparator>;
  public type
    TIterator = TTree.TIterator;
  private
    FTree : TTree;
    FParser : TPatternParser;
  private
    procedure CreateParser();inline;
  public
    constructor Create();
    destructor Destroy;override;
    procedure Add(AChar : TUnicodeCodePoint);inline;overload;
    procedure Add(AString : TUnicodeCodePointArray);inline;overload;
    procedure AddRange(const AStart, AEnd : TUnicodeCodePoint);inline;
    procedure AddPattern(const APattern : UnicodeString);inline;
    function CreateIterator() : TIterator;
    function Contains(const AString : array of TUnicodeCodePoint) : Boolean;overload;
    function Contains(const AChar : TUnicodeCodePoint) : Boolean;inline;overload;
    function Contains(const AChar : UnicodeChar) : Boolean;inline;overload;
    function Contains(const AChar : AnsiChar) : Boolean;inline;overload;
  end;

resourcestring
  SInvalidLength = 'Invalid length value : "%d".';
  SInvalidPosition = 'Invalid position : "%d".';
  SInvalidRangeLimits = 'Invalid range limits : ["%x" , "%x"].';
  SExpectedBut = 'Expects "%s" but got "%s..." .';
  SUnexpectedEOF = 'Unexpected end of file.';

implementation
uses
  unicodedata;

function ToArray(const AItem : TUnicodeCodePoint) : TUnicodeCodePointArray;inline;
begin
  SetLength(Result,1);
  Result[Low(Result)] := AItem;
end;

function CompareItem(const Item1, Item2 : TUnicodeCodePointArray): Integer;
var
  a, b : ^TUnicodeCodePoint;
  i, ha, hb : Integer;
begin
  if (Pointer(Item1) = Pointer(Item2)) then
    exit(0);
  if (Item1 = nil) then
    exit(-1);
  if (Item2 = nil) then
    exit(1);
  a := @Item1[0];
  b := @Item2[0];
  Result := 1;
  ha := Length(Item1) - 1;
  hb := Length(Item2) - 1;
  for i := 0 to ha do begin
    if (i > hb) then
      exit;
    if (a^ < b^) then
      exit(-1);
    if (a^ > b^) then
      exit(1);
    Inc(a);
    Inc(b);
  end;
  if (ha = hb) then
    exit(0);
  exit(-1);
end;

{ TUnicodeCodePointArrayComparator }

class function TUnicodeCodePointArrayComparator.Compare(const A, B : TUnicodeCodePointArray): Integer;
begin
  Result := CompareItem(A,B);
end;

{ TPatternParser }

procedure TPatternParser.Error(const AMsg: string; const AArgs: array of const);
begin
  raise EUnicodeSetException.CreateFmt(AMsg,AArgs);
end;

procedure TPatternParser.Error(const AMsg: string);
begin
  raise EUnicodeSetException.Create(AMsg);
end;

procedure TPatternParser.SetBuffer(
  const APattern : PUnicodeChar;
  const ALength  : Integer
);
begin
  FPosition := 0;
  if (ALength <= 1) then begin
    FBufferStr := '';
    FBuffer := nil;
    FBufferLength := 0;
    exit;
  end;
  FBufferLength := ALength;
  SetLength(FBufferStr,FBufferLength);
  FBuffer := @FBufferStr[1];
  Move(APattern^,FBuffer^,(FBufferLength*SizeOf(FBuffer^)));
end;

procedure TPatternParser.CheckEOF();
begin
  CheckEOF(0);
end;

procedure TPatternParser.CheckEOF(ALength : Integer);
begin
  if (ALength < 0) then
    Error(SInvalidLength,[ALength]);
  if ((FPosition+ALength) >= FBufferLength) then
    UnexpectedEOF();
end;

procedure TPatternParser.UnexpectedEOF();
begin
  Error(SUnexpectedEOF);
end;

function TPatternParser.IsThis(AItem: UnicodeString; const APosition: Integer): Boolean;
var
  i, k, c : Integer;
begin
  if (APosition < 0) then
    Error(SInvalidPosition,[APosition]);
  Result := False;
  c := Length(AItem);
  if (c = 0) then
    exit;
  i := APosition;
  k := i + c;
  if (k >= FBufferLength) then
    exit;
  if CompareMem(@AItem[1], @FBuffer[APosition],c) then
    Result := True;
end;

function TPatternParser.IsThis(AItem : UnicodeString) : Boolean;
begin
  Result := IsThis(AItem,FPosition);
end;

procedure TPatternParser.Expect(AItem: UnicodeString; const APosition: Integer);
begin
  if not IsThis(AItem,APosition) then
    Error(SExpectedBut,[AItem,Copy(FBuffer,APosition,Length(AItem))]);
end;

procedure TPatternParser.Expect(AItem: UnicodeString);
begin
  Expect(AItem,FPosition);
end;

procedure TPatternParser.SkipSpaces();
begin
  while (FPosition < FBufferLength) do begin
    if (FBuffer[FPosition] <> ' ') then
      Break;
    Inc(FPosition);
  end;
end;

function TPatternParser.NextChar(): TUnicodeCodePoint;
var
  i : Integer;
  c : UnicodeChar;
  cp : TUnicodeCodePoint;
  s : UnicodeString;
begin
  SkipSpaces();
  CheckEOF();
  c := FBuffer[FPosition];
  cp := Ord(c);
  Inc(FPosition);
  if (c = '\') and (FPosition < FBufferLength) then begin
    if IsThis('\') then begin
      Inc(FPosition);
      CheckEOF();
      cp := Ord(FBuffer[FPosition]);
      Inc(FPosition);
    end else if IsThis('u') then begin
      Inc(FPosition);
      CheckEOF(4);
      s := Copy(FBufferStr,(FPosition+1),4);
      Inc(FPosition,4);
      if not TryStrToInt('$'+s,i) then
        Error(SExpectedBut,['\uXXXX',s]);
      cp := i;
    end;
  end;
  if (cp <= MAX_WORD) and UnicodeIsLowSurrogate(UnicodeChar(Word(cp))) then begin
    SkipSpaces();
    CheckEOF();
    c := UnicodeChar(Word(cp));
    if UnicodeIsSurrogatePair(c,FBuffer[FPosition]) then begin
      cp := ToUCS4(c,FBuffer[FPosition]);
      Inc(FPosition);
    end;
  end;
  Result := cp;
end;

function CompareTo(const A : TUnicodeCodePoint; const B : UnicodeChar) : Boolean;inline;
begin
  Result := (A = Ord(B));
end;

procedure TPatternParser.ParseItem();
var
  cp, lastCp : TUnicodeCodePoint;
  charCount : Integer;
begin
  SkipSpaces();
  Expect('[');
  charCount := 0;
  Inc(FPosition);
  cp:=0;
  while (FPosition < FBufferLength) do begin
    lastCp := cp;
    cp := NextChar();
    if CompareTo(cp,']') then
      Break;
    if CompareTo(cp,'-') then begin
      if (charCount = 0) then
        Error(SExpectedBut,['<char>','-']);
      cp := NextChar();
      FSet.AddRange(lastCp,cp);
    end else begin
      FSet.Add(cp);
    end;
    Inc(charCount);
  end;
end;

procedure TPatternParser.DoParse();
begin
  SkipSpaces();
  while (FPosition < FBufferLength) do begin
    ParseItem();
    SkipSpaces();
  end;
end;

procedure TPatternParser.Parse(const APattern: PUnicodeChar; const ALength: Integer);
begin
  if (ALength < 2) then
    exit;
  SetBuffer(APattern,ALength);
  DoParse();
end;

procedure TPatternParser.Parse(const APattern : UnicodeString);
begin
  Parse(@APattern[1],Length(APattern));
end;

{ TUnicodeSet }

procedure TUnicodeSet.CreateParser();
begin
  if (FParser = nil) then begin
    FParser := TPatternParser.Create();
    FParser.CurrentSet := Self;
  end;
end;

constructor TUnicodeSet.Create;
begin
  FTree := TTree.Create();
end;

destructor TUnicodeSet.Destroy;
begin
  FParser.Free();
  FTree.Free();
  inherited Destroy;
end;

procedure TUnicodeSet.Add(AChar: TUnicodeCodePoint);
begin
  FTree.Insert(ToArray(AChar));
end;

procedure TUnicodeSet.Add(AString: TUnicodeCodePointArray);
begin
  if (AString <> nil) then
    FTree.Insert(AString);
end;

procedure TUnicodeSet.AddRange(const AStart, AEnd : TUnicodeCodePoint);
var
  i : Integer;
begin
  if (AStart > AEnd) then
    raise EUnicodeSetException.CreateFmt(SInvalidRangeLimits,[AStart,AEnd]);
  for i := AStart to AEnd do
    Add(i);
end;

procedure TUnicodeSet.AddPattern(const APattern : UnicodeString);
begin
  CreateParser();
  FParser.Parse(APattern);
end;

function TUnicodeSet.CreateIterator() : TIterator;
begin
  Result := FTree.CreateForwardIterator();
end;

function TUnicodeSet.Contains(const AString : array of TUnicodeCodePoint) : Boolean;
var
  c : Integer;
  x : TUnicodeCodePointArray;
begin
  Result := False;
  c := Length(AString);
  if (c = 0) then
    exit;
  SetLength(x,c);
  Move(AString[Low(AString)],x[Low(x)],(c*SizeOf(x[0])));
  if (FTree.FindNode(x) <> nil) then
    Result := True;
end;

function TUnicodeSet.Contains(const AChar : TUnicodeCodePoint) : Boolean;
begin
  Result := Contains([AChar]);
end;

function TUnicodeSet.Contains(const AChar : UnicodeChar) : Boolean;
begin
  Result := Contains(TUnicodeCodePoint(Ord(AChar)));
end;

function TUnicodeSet.Contains(const AChar : AnsiChar) : Boolean;
begin
  Result := Contains(TUnicodeCodePoint(Ord(AChar)));
end;

end.

