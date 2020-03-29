program project1;
{$mode objfpc}{$H+}

uses
  Classes, FGL;

type
  TBaseClass = class (TObject)
  end;

  generic TFPGObjectListEx<T: TBaseClass> = class (specialize TFPGObjectList<T>)
    function GetItemByID(AID: Integer): T;
  end;

function TFPGObjectListEx.GetItemByID(AID: Integer): T;
begin
  Result:=nil; //T(nil);
end;

begin
end.
