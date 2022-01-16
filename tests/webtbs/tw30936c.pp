program StaticClassProc;

{$MODE DELPHI}

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
begin
end;

class procedure TTest.MyProc;
begin
  Writeln('OK');
end;

var
  aProc: TProcedure;
begin
  aProc := TTest.MyProc;
  aProc;
end.

