{ %version=1.1 }
{$mode objfpc}
Type
  IMyInterface = Interface
    Function MyFunc : Integer;
  end;

  TMyClass = Class(TInterfacedObject,IMyInterface)
    Function MyOtherFunction : Integer;
    // The following fails in FPC.
    Function IMyInterface.MyFunc = MyOtherFunction;
  end;

Function TMyClass.MyOtherFunction : Integer;

begin
  Result:=23;
end;

Var
  A : TMyClass;
  M : IMyInterface;
  I : Integer;

begin
  A:=TMyClass.Create;
  M:=A;
  I:=M.MyFunc;
  If (I<>23) then
    begin
    Writeln('Error calling interface');
    Halt(1);
    end;
end.
