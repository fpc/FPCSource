program tfuncref48;

{$mode delphi}{$H+}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{same as tfuncref26 but with mode delphi}

type
  TTestObject = class(TInterfacedObject, IInterface)
    destructor Destroy; override;
  end;

  TTestFunc = reference to procedure;

var
  destroyed: Boolean;

destructor TTestObject.Destroy;
begin
  destroyed := True;
  inherited;
end;

{ use out parameter to avoid the usage of a temp }
procedure DoTest(out res: TTestFunc);
var
  intf: IInterface;

  procedure TestSub;
  begin
    intf._AddRef;
    intf._Release;
  end;

begin
  intf := TTestObject.Create;
  res := TestSub;
end;

procedure DoTest2(out res: TTestFunc);
var
  intf: IInterface;

  procedure TestSub(out res: TTestFunc);
  begin
    res := procedure
           begin
             intf._AddRef;
             intf._Release;
           end;
  end;

begin
  intf := TTestObject.Create;
  TestSub(res);
end;

var
  f: TTestFunc;
begin
  DoTest(f);
  if destroyed then
    Halt(1);
  f();
  f := Nil;
  if not destroyed then
    Halt(2);

  destroyed := False;

  DoTest2(f);
  if destroyed then
    Halt(3);
  f();
  f := Nil;
  if not destroyed then
    Halt(4);
end.
