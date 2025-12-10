{ %NORUN }

program tw41504b;

{$mode objfpc}

type
  TTest = class
    procedure Test; virtual; abstract;
  end;

  TTest2 = type TTest;

begin

end.
