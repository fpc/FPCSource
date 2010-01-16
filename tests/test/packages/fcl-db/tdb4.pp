program TestMemDs;

{$mode objfpc}{$H+}

uses
  memds,db;

var
   DSet:TMemDataset;

begin
   DSet:=TMemDataset.Create(nil);
   DSet.FieldDefs.Add('NAME',ftString,20);
   DSet.CreateTable;
   DSet.Open;

   DSet.Append;
   DSet.Edit;
   DSet.FieldByName('NAME').Value:='aaa';
   DSet.Post;

   DSet.Append;
   DSet.Edit;
   DSet.FieldByName('NAME').Value:='bbb';
   DSet.Post;

   if Dset.RecordCount<>2 then
     halt(1);
   DSet.Free;
end.
