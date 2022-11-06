program tw39978;

{$IFDEF FPC}
{$mode delphi}
{$ModeSwitch functionreferences}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

type
  TRefProc = reference to procedure(Sender: TObject);

procedure Test(P: TRefProc);
begin
  P(nil);
end;

type
  TMyObj = class(TObject)
  public
    procedure MyEvent(Sender: TObject);
    procedure MyTest;
  end;

var
  Obj: TMyObj;

{ TMyObj }
procedure TMyObj.MyEvent(Sender: TObject);
begin
  if (Self<>Obj) then // solved with ObjFpc mode and Test(@MyEvent); using Self.MyEvent doesn't help either
    Halt(1);
end;
procedure TMyObj.MyTest;
begin
  Test(MyEvent);
end;

begin
  Obj := TMyObj.Create;
  Obj.MyTest;
end.

