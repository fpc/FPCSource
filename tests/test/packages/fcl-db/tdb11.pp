{ %skiptarget=msdos,go32v2 }
program tdb11;

{ A TWideStringField of type ftFixedWideChar stores and returns its text as
  one of type ftWideString does, truncated to the field and with
  TDataSet.DataConvert writing no more than its DataSize bytes, for a field
  that fits the fixed conversion buffer and for one that does not }

{$mode objfpc}{$H+}

uses db, memds;

const
  Extra = 16;
  Fill = $5A;

procedure CheckConvert(DSet: TDataSet; F: TField; ToNative: Boolean; Code: Integer);
var
  Src, Back: UnicodeString;
  Dest: array of Byte;
  I: SizeInt;
begin
  Src:=StringOfChar(WideChar('y'),F.Size+Extra);
  SetLength(Dest,F.DataSize+2*Extra);
  FillChar(Dest[0],Length(Dest),Fill);
  DSet.DataConvert(F,PWideChar(Src),@Dest[0],ToNative);
  for I:=F.DataSize to High(Dest) do
    if Dest[I]<>Fill then
      Halt(Code);
  Back:=PWideChar(@Dest[0]);
  if Back<>Copy(Src,1,F.Size) then
    Halt(Code+1);
end;

procedure CheckStore(F: TField; Code: Integer);
var
  Value: UnicodeString;
begin
  if not (F is TWideStringField) or (F.DataType<>ftFixedWideChar) then
    Halt(Code);
  F.AsUnicodeString:='abc';
  if F.AsUnicodeString<>'abc' then
    Halt(Code+1);
  Value:=StringOfChar(WideChar('z'),F.Size+50);
  F.AsUnicodeString:=Value;
  if F.AsUnicodeString<>Copy(Value,1,F.Size) then
    Halt(Code+2);
end;

var
  DSet: TMemDataset;
  S, L: TField;

begin
  DSet:=TMemDataset.Create(nil);
  DSet.FieldDefs.Add('S',ftFixedWideChar,10);
  DSet.FieldDefs.Add('L',ftFixedWideChar,9000);
  DSet.CreateTable;
  DSet.Open;
  S:=DSet.FieldByName('S');
  L:=DSet.FieldByName('L');
  if (S.DataSize>dsMaxStringSize) or (L.DataSize<=dsMaxStringSize) then
    Halt(1);
  DSet.Append;
  CheckStore(S,2);
  CheckStore(L,5);
  DSet.Post;
  if S.AsUnicodeString<>StringOfChar(WideChar('z'),S.Size) then
    Halt(8);
  if L.AsUnicodeString<>StringOfChar(WideChar('z'),L.Size) then
    Halt(9);
  CheckConvert(DSet,S,True,10);
  CheckConvert(DSet,S,False,12);
  CheckConvert(DSet,L,True,14);
  CheckConvert(DSet,L,False,16);
  DSet.Close;
  DSet.Free;
end.
