{ %VERSION=1.1 }

{$ifdef fpc}
{$mode objfpc}
{$endif}

type
  ITest = interface(IUnknown)
    procedure DoSomething;
  end;


  TMyClass = class(TInterfacedObject, ITest)
    procedure DoSomething;
  end;

var
   i : longint;

procedure TMyClass.DoSomething;
begin
  inc(i);
end;


procedure DoTest(const ATest: ITest);
begin
  ATest.DoSomething;
end;


procedure DoTest2(ATest: ITest);
begin
  ATest.DoSomething;
end;


var
  c: ITest;
begin
  i:=0;
  c := TMyClass.Create;
  DoTest(c);
  DoTest2(c);
  if i<>2 then
    begin
       writeln('Problem with passing interfaces as parameters');
       halt(1);
    end;
end.
