{$mode objfpc}

var
  err : boolean;

type
  TA = class
  end;
  TB = class(TA)
  end;
  TC = class(TB)
  end;

procedure Test(const A: TA); overload;
begin
end;

procedure Test(const B: TB); overload;
begin
  writeln('ok');
  err:=false;
end;

var
  X : TC;

begin
  err:=true;
  Test(X);
  if err then
    halt(1);
end.
