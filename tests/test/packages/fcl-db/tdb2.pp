program TTestDBBasics_TestSetFieldValues;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  Assertions,
  ToolsUnit,
  dbftoolsunit,
  variants,
  db;

var PassException : boolean;

begin
  InitialiseDBConnector;
  with DBConnector.GetNDataset(true,11) do
    begin
    open;
    first;
    edit;
    FieldValues['id']:=5;
    post;
    AssertEquals('TestName1',FieldByName('name').AsString);
    AssertEquals(5,FieldByName('id').AsInteger);
    edit;
    FieldValues['name']:='FieldValuesTestName';
    post;
    AssertEquals('FieldValuesTestName',FieldByName('name').AsString);
    AssertEquals(5,FieldByName('id').AsInteger);
    edit;
    FieldValues['id;name']:= VarArrayOf([243,'ValuesTestName']);
    post;
    AssertEquals('ValuesTestName',FieldByName('name').AsString);
    AssertEquals(243,FieldByName('id').AsInteger);
    
    PassException:=false;
    try
      edit;
      FieldValues['id;name;fake']:= VarArrayOf([243,'ValuesTestName',4]);
    except
      on E: EDatabaseError do PassException := True;
    end;
    post;
    AssertTrue(PassException);
    end;
end.
