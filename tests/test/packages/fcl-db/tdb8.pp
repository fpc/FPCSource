{ %skiptarget=msdos,go32v2 }
program tdb8;

{ A TStringField with Transliterate set hands TDataSet.Translate at most
  DataSize-1 characters, however long the value assigned to it, and a source
  that is not its destination, both for a field that fits the fixed buffer and
  for one that does not }

{$mode objfpc}{$H+}

uses db, memds;

type
  TTestDataset = class(TMemDataset)
  public
    function Translate(Src, Dest: PAnsiChar; ToOem: Boolean): Integer; override;
  end;

var
  Longest, DestSize: SizeInt;
  SameBuffer: Boolean;

{ copies at most DestSize-1 characters, so that the test itself never writes
  past the field's buffer }
function TTestDataset.Translate(Src, Dest: PAnsiChar; ToOem: Boolean): Integer;
begin
  if ToOem and (Src=Dest) then
    SameBuffer:=True;
  Result:=StrLen(Src);
  if Result>Longest then
    Longest:=Result;
  if Result>DestSize-1 then
    Result:=DestSize-1;
  if Src<>Dest then
    Move(Src^,Dest^,Result);
  Dest[Result]:=#0;
end;

procedure Check(DSet: TDataSet; const Name: String; Code: Integer);
var
  F: TStringField;
  Value, Back: AnsiString;
begin
  F:=DSet.FieldByName(Name) as TStringField;
  F.Transliterate:=True;
  Value:=StringOfChar('a',F.DataSize*2);
  DestSize:=F.DataSize;
  Longest:=0;
  SameBuffer:=False;
  F.AsString:=Value;
  if Longest>F.DataSize-1 then
    Halt(Code);
  if SameBuffer then
    Halt(Code+4);
  Back:=F.AsString;
  if Back<>Copy(Value,1,F.DataSize-1) then
    Halt(Code+1);
end;

var
  DSet: TTestDataset;

begin
  DSet:=TTestDataset.Create(nil);
  DSet.FieldDefs.Add('S',ftString,5);
  DSet.FieldDefs.Add('L',ftString,dsMaxStringSize+10);
  DSet.CreateTable;
  DSet.Open;
  DSet.Append;
  Check(DSet,'S',1);
  Check(DSet,'L',3);
  DSet.Post;
  DSet.Close;
  DSet.Free;
end.
