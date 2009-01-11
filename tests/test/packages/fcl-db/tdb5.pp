program LookupIsNull;

uses db, memds, variants;

var
  DSet:TMemDataset;
  tmpVariant:Variant;

begin
  DSet:=TMemDataset.Create(nil);
  DSet.FieldDefs.Add('NAME',ftString,20);
  DSet.CreateTable;
  DSet.Open;

  tmpVariant:=DSet.Lookup('NAME','aaaa','NAME');
  if not (VarIsNull(tmpVariant)) then
    Halt(1);
  DSet.Close;
  DSet.Free;
end.
