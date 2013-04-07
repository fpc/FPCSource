{ %fail }
{$mode objfpc}

type
  ti = interface;

  tc = class(tinterfacedobject, ti)
  end;

  ti = interface
    procedure test;
  end;

begin
end.

