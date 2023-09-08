{ %FAIL }

program tw40381c;

{$mode objfpc}
{$warn 5043 error}

type
  TFoo = 1..9 deprecated;

  TBar = class
    //F1: TFoo;
    //procedure M1(x:TFoo);
  end;

{var
  a: TFoo;}

procedure b(x:array of TFoo);
begin
  //writeln(x);
end;

{procedure TBar.M1(x: TFoo);
begin
  writeln(x);
end;}

begin
end.

