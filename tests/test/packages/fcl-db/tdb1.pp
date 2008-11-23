program TTestDBBasics_TestGetFieldValues;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  Assertions,
  ToolsUnit,
  dbftoolsunit,
  db;

var AVar          : Variant;
    PassException : boolean;

begin
  InitialiseDBConnector;
  with DBConnector.GetNDataset(true,14) do
    begin
    open;
    AVar:=FieldValues['id'];
    AssertEquals(AVar,1);

    AVar:=FieldValues['name'];
    AssertEquals(AVar,'TestName1');

    AVar:=FieldValues['id;name'];
    AssertEquals(AVar[0],1);
    AssertEquals(AVar[1],'TestName1');

    AVar:=FieldValues['name;id;'];
    AssertEquals(AVar[1],1);
    AssertEquals(AVar[0],'TestName1');

    PassException:=false;
    try
      AVar:=FieldValues['name;id;fake'];
    except
      on E: EDatabaseError do PassException := True;
    end;
    AssertTrue(PassException);

    end;
end.

