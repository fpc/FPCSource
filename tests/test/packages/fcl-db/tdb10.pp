{ %skiptarget=msdos,go32v2 }
program tdb10;

{ TDataSet.DataConvert copies a TWideStringField's text, in either
  direction, as at most the field's Size characters and a terminator, so
  never more than DataSize bytes, however long the text it is given; and a
  value longer than the field is stored truncated to it }

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
  Value:=StringOfChar(WideChar('z'),F.Size+5000);
  F.AsUnicodeString:=Value;
  if F.AsUnicodeString<>Copy(Value,1,F.Size) then
    Halt(Code);
end;

var
  DSet: TMemDataset;
  S, L: TField;

begin
  DSet:=TMemDataset.Create(nil);
  DSet.FieldDefs.Add('S',ftWideString,10);
  DSet.FieldDefs.Add('L',ftWideString,9000);
  DSet.CreateTable;
  DSet.Open;
  S:=DSet.FieldByName('S');
  L:=DSet.FieldByName('L');
  if (S.DataSize>dsMaxStringSize) or (L.DataSize<=dsMaxStringSize) then
    Halt(1);
  CheckConvert(DSet,S,True,2);
  CheckConvert(DSet,S,False,4);
  CheckConvert(DSet,L,True,6);
  CheckConvert(DSet,L,False,8);
  DSet.Append;
  CheckStore(S,10);
  CheckStore(L,11);
  DSet.Post;
  if S.AsUnicodeString<>StringOfChar(WideChar('z'),S.Size) then
    Halt(12);
  if L.AsUnicodeString<>StringOfChar(WideChar('z'),L.Size) then
    Halt(13);
  DSet.Close;
  DSet.Free;
end.
