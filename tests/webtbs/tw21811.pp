{$mode objfpc}
type
  TC1 = class
  strict protected
    procedure P;
  end;

  TH1 = class helper for TC1
  public
    procedure Q;
  end;

var
  b: boolean;

procedure TC1.P;
  begin
    b:=true;
  end;

procedure TH1.Q;
  begin
    inherited P;
  end;

var
  c: tc1;
begin
  b:=false;
  c:=tc1.create;
  c.q;
  c.free;
  if not b then
    halt(1);
end.
