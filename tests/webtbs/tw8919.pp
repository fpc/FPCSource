
{$mode delphi}

type
  TOnProc = procedure of object;

  PMyObj = ^TMyObj;
  TMyObj = object
  private
    FOnProc: TOnProc;

    s: ansistring;
  public
    property OnProc: TOnProc read FOnProc write FOnProc;
    procedure Proc;
  end;

procedure TMyObj.Proc;
begin
end;

var
  obj: PMyObj;

begin
  New(obj);
  obj^.OnProc:=obj^.Proc;
  if TMethod(obj^.OnProc).Data <> obj then begin
    writeln('Test FAILED!');
    Halt(1);
  end;
  writeln('Test OK!');
end.
