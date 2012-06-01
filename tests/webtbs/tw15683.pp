program tw15683;

{$mode objfpc}

type
  generic GSomething<TSomeType> = class
    procedure Method(i :Integer);
    procedure Method(s :TSomeType);
  end;

procedure GSomething.Method(i: Integer);
begin
end;

procedure GSomething.Method(s: TSomeType);
begin
end;

begin
end.
