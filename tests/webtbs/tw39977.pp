{ %NORUN }

program tw39977;

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
  Writeln('test');
end;

type
  TMyObjA = class(TObject)
  strict protected
    procedure MyEvent(Sender: TObject);
  end;

  TMyObjB = class(TMyObjA)
  public
    procedure MyTest;
  end;

{ TMyObjA }
procedure TMyObjA.MyEvent(Sender: TObject);
begin
  Writeln('hello');
end;

{ TMyObjB }
procedure TMyObjB.MyTest;
begin
  Test(MyEvent); // solved with ObjFpc mode and Test(@MyEvent); using Self.MyEvent or TRefProc(MyEvent) doesn't help
end;

var
  O: TMyObjB;
begin
  O := TMyObjB.Create;
  O.MyTest;
end.

