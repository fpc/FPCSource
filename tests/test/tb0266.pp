{ %fail }

{$mode objfpc}
{$interfaces corba}

type
  tc = class
   strict private
    procedure test;
  end;

  tintf = interface
    procedure test;
  end;

  tc2 = class(tc, tintf)
  end;

procedure tc.test;
begin
end;

begin
end.
