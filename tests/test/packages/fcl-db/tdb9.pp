{ %skiptarget=msdos,go32v2 }
program tdb9;

{ A TWideStringField larger than dsMaxStringSize bytes is converted into a
  buffer of its own size when it is set: its whole value is stored, and an
  OnValidate handler reading the field sees that value }

{$mode objfpc}{$H+}

uses db, memds;

type
  TValidator = class
    Seen: UnicodeString;
    procedure Validate(Sender: TField);
  end;

procedure TValidator.Validate(Sender: TField);
begin
  Seen:=Sender.AsUnicodeString;
end;

procedure Check(F: TField; V: TValidator; const Value: UnicodeString; Code: Integer);
begin
  V.Seen:='';
  F.AsUnicodeString:=Value;
  if V.Seen<>Value then
    Halt(Code);
  if F.AsUnicodeString<>Value then
    Halt(Code+1);
end;

var
  DSet: TMemDataset;
  F: TField;
  V: TValidator;

begin
  DSet:=TMemDataset.Create(nil);
  DSet.FieldDefs.Add('L',ftWideString,9000);
  DSet.CreateTable;
  DSet.Open;
  F:=DSet.FieldByName('L');
  if F.DataSize<=dsMaxStringSize then
    Halt(1);
  V:=TValidator.Create;
  F.OnValidate:=@V.Validate;
  DSet.Append;
  Check(F,V,'abc',2);
  Check(F,V,StringOfChar(WideChar('x'),F.Size),4);
  DSet.Post;
  if F.AsUnicodeString<>StringOfChar(WideChar('x'),F.Size) then
    Halt(6);
  DSet.Close;
  V.Free;
  DSet.Free;
end.
