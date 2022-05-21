{ %OPT=-gw -godwarfproperties }
program tdwarfproperties1;

{$mode objfpc}{$H+}

type
  ITestIntf = interface(IUnknown)
    function GetCurrent: TObject;
  end;

  ITestIntf2 = interface(ITestIntf)
    property Current2: TObject read GetCurrent;
  end;

  { TGenEvaluationIdentifierList }

  TGenEvaluationIdentifierList = class(TInterfacedObject, ITestIntf2)
    function GetCurrent: TObject;
    property Current2: TObject read GetCurrent;
  end;

function TGenEvaluationIdentifierList.GetCurrent: TObject;
begin

end;

var
  s: ITestIntf;

begin
  s := ITestIntf(nil);
end.

