{ %fail }

program bug9;

{$ifdef fpc}
{$mode delphi}
{$endif}


type
  ITest = interface(IInterface)
    ['{FE6B16A6-A898-4B09-A46E-0AAC5E0A4E14}']
    function Parent: ITest;
    function GetChild: ITest;
  end;

  ITestEx = interface(ITest)
    ['{82449E91-76BE-4F4A-B873-1865042D5CAF}']
  end;

  TTest = class(TInterfacedObject, ITest)
    function ITest.Parent = ParentEx;
    { ITestEx }
    function ParentEx: ITestEx;
    function GetChild: ITest;
    procedure RemoveChild;
  end;
    { ITest }

    { ITestEx }

function TTest.ParentEx: ITest;
begin;
Result := nil
end;



function TTest.GetChild: ITest;
begin;
WriteLn('TTest.GetChild');
Result := nil
end;

procedure TTest.RemoveChild;
begin;
WriteLn('TTest.RemoveChild');
end;


var E: ITest;
begin
  E := TTest.Create;
  WriteLn('Calling GetChild');
  E.GetChild();
  WriteLn('Stop');
end.
