{$mode delphi}

type
  TProc = procedure of object;

  TTest = class
  public
    proc: TProc;
    constructor Create;
    procedure foo;
    procedure bar;
  end;

constructor TTest.Create;
begin
  inherited;
  proc := nil;
end;

procedure TTest.foo;
begin
  writeln('foo');
end;

procedure TTest.bar;
begin
  if @proc <> nil then proc;
end;

var
  t: TTest;

begin
  t := TTest.Create;
  t.proc := t.foo;
  t.bar;
  t.Free;
end.
