{ Source provided for Free Pascal Bug Report 14019 }
{ Submitted by "hennymcc" on  2009-06-21 }

program tw14019;

{$mode objfpc}

type
  ITest = interface
    function SomeMethod(): ITest;
    function GetValue(): Integer;
  end;

  TTest = class(TInterfacedObject, ITest)
  public
    procedure FreeInstance; override;
    function SomeMethod(): ITest;
    function GetValue(): Integer;
  end;

procedure TTest.FreeInstance;
begin
  FillChar(Pointer(Self)^, InstanceSize, 0);
  inherited FreeInstance;
end;

function TTest.SomeMethod(): ITest;
begin
  Result := TTest.Create();
end;

function TTest.GetValue(): Integer;
begin
  Result := 0;
end;

var
  t: ITest;
begin
  t := TTest.Create();
  t.SomeMethod().SomeMethod().GetValue();
end.

