{ %skiptarget=msdos,go32v2 }
program tdb7;

{ AsAnsiString of a TWideStringField converts between the AnsiString and the
  field's UTF-16 text, in both directions, also for an AnsiString assigned
  through SetFields }

{$mode objfpc}{$H+}

uses db, memds;

var
  DSet: TMemDataset;
  F: TField;
  S: AnsiString;

begin
  DSet:=TMemDataset.Create(nil);
  DSet.FieldDefs.Add('W',ftWideString,10);
  DSet.CreateTable;
  DSet.Open;
  DSet.Append;
  F:=DSet.FieldByName('W');
  if not (F is TWideStringField) then
    Halt(1);

  F.AsAnsiString:='abc';
  if F.AsUnicodeString<>'abc' then
    Halt(2);

  F.AsUnicodeString:='xyz';
  if F.AsAnsiString<>'xyz' then
    Halt(3);

  S:='uvw';
  DSet.SetFields([S]);
  if F.AsUnicodeString<>'uvw' then
    Halt(4);

  DSet.Post;
  if F.AsAnsiString<>'uvw' then
    Halt(5);
  DSet.Close;
  DSet.Free;
end.
