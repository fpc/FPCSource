program bug9;

{$ifdef fpc}
{$mode delphi}
{$endif}


type
  ttesttype = (testgetchild,testparent,testparentex);

  ITest = interface(IInterface)
    ['{FE6B16A6-A898-4B09-A46E-0AAC5E0A4E14}']
    function Parent: ITest;
  end;

  ITestEx = interface(ITest)
    ['{82449E91-76BE-4F4A-B873-1865042D5CAF}']
    function Parent: ITestEx;
    function GetChild: ITestEx;
    procedure RemoveChild;
  end;

  TTest = class(TInterfacedObject, ITestEx)
    function ITestEx.Parent = ParentEx;
    { ITest }
    function Parent: ITest;
    { ITestEx }
    function ParentEx: ITestEx;
    function GetChild: ITestEx;
    procedure RemoveChild;
  end;
    { ITest }
var
  test: ttesttype;

function TTest.Parent: ITest;
begin;
writeln('ttest.parent');
Result := nil;
if (test<>testparent) then
  halt(1);
end;



    { ITestEx }

function TTest.ParentEx: ITestEx;
begin;
writeln('ttest.parentex');
Result := nil;
if (test<>testparentex) then
  halt(1);
end;



function TTest.GetChild: ITestEx;
begin;
WriteLn('TTest.GetChild');
Result := nil;
if (test<>testgetchild) then
  halt(1);
end;

procedure TTest.RemoveChild;
begin;
WriteLn('TTest.RemoveChild');
halt(1);
end;


var E: ITestEx;
    e2: itest;
begin
  E := TTest.Create;
  WriteLn('Calling GetChild');
  test:=testgetchild;
  E.GetChild();
  test:=testparentex;
  e.parent;
  test:=testparent;
  e2:=e;
  e2.parent;
  WriteLn('Stop');
end.
