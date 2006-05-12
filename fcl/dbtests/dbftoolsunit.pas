unit DBFToolsUnit;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, toolsunit,
  db,
  Dbf, dbf_fields;


type
     { TDBFDBConnector }

     TDBFDBConnector = class(TDBConnector)
     private
     protected
       Function CreateNDataset(n : integer) : TDataset; override;
       Procedure FreeNDataset(var ds : TDataset); override;
     public
       destructor Destroy; override;
     end;

implementation

destructor TDBFDBConnector.Destroy;
begin
  inherited Destroy;
end;

function TDBFDBConnector.CreateNDataset(n: integer): TDataset;
var countID : integer;
begin
  with TDbf.Create(nil) do
    begin
    FilePath := dbname;
    TableName := 'fpdev_'+inttostr(n)+'.db';
    FieldDefs.Add('ID',ftInteger);
    FieldDefs.Add('NAME',ftString,50);
    CreateTable;
    Open;
    for countId := 1 to n do
      begin
      Append;
      FieldByName('ID').AsInteger := countID;
      FieldByName('NAME').AsString := 'TestName'+inttostr(countID);
      end;
    if state = dsinsert then
      Post;
    Close;
    Free;
    end;
// A dataset that has been opened and closed can't be used. Or else the tests
// for a newly generated dataset can't work properly.
  Result := TDbf.Create(nil);
  with (result as TDbf) do
    begin
    FilePath := dbname;
    TableName := 'fpdev_'+inttostr(n)+'.db';
    end;
end;

procedure TDBFDBConnector.FreeNDataset(var ds: TDataset);
begin
  if ds.Active then ds.close;
  FreeAndNil(ds);
end;

end.

