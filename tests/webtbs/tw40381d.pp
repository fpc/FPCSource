{ %FAIL }

program tw40381d;

{$mode objfpc}
{$warn 5043 error}

type
  TFoo = 1..9 deprecated;

  TBar = class
    //F1: TFoo;
    function M1:TFoo;
  end;

{var
  a: TFoo;}

{procedure b(x:TFoo);
begin
  writeln(x);
end;}

function TBar.M1:TFoo;
begin
//  writeln(x);
end;

begin
end.

