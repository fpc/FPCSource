program StaticClassProc;

{$MODE OBJFPC}

type
  TTest = class
  public type
    TProcedure = procedure;
  private
    class procedure MyProc; static;
  public
    constructor Create;
  end;

{ TTest }

constructor TTest.Create;
var
  aProc: TProcedure;
begin
  aProc := @MyProc;
  aProc;
end;

class procedure TTest.MyProc;
begin
  Writeln('OK');
end;

begin
  TTest.Create;
end.

