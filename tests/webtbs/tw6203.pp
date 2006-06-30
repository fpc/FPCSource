{$mode delphi}

uses
  uw6203;

type
  TDerived = class(TTest)
  private
    procedure CMTest(var Msg: TMessage); message CM_TEST;
  end;

procedure TDerived.CMTest(var Msg: TMessage);
begin
  inherited;
  WriteLn('TDerived.CMTest');
end;

var
  Test: TTest;
  Msg: TMessage;
begin
  err:=true;
  Test := TDerived.Create;
  Msg.Msg := CM_TEST;
  Test.Dispatch(Msg);
  if err then
    halt(1);
end.
