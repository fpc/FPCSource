{ %NORUN }

program tw39902b;

{$mode objfpc}

uses Classes;

type TTest = class(TObject)
 FEvent: TNotifyEvent;
 procedure SetEvent(aValue: TNotifyEvent);
 procedure SomeEvent (Sender: NativeInt); overload;
 procedure SomeEvent (Sender: TObject); overload;
 property Event1: TNotifyEvent read FEvent write FEvent;
 property Event2: TNotifyEvent read FEvent write SetEvent;
end;

procedure TTest.SetEvent(aValue: TNotifyEvent);
begin
  FEvent:=aValue;
end;

procedure TTest.SomeEvent (Sender: TObject);
begin
end;

procedure TTest.SomeEvent (Sender: NativeInt);
begin
end;

procedure Foo(aArg: TNotifyEvent);
begin

end;

var
 x: TTest;
 //y: TStringList;
 m: TNotifyEvent;
begin
 x := TTest.Create;
 //y := TStringList.Create;
 //y.OnChange := x.SomeEvent;
 x.Event1 := @x.SomeEvent;
 x.Event2 := @x.SomeEvent;
 m := @x.SomeEvent;
 Foo(@x.someEvent);
end.
