program tw40979;
{$mode ObjFPC}

procedure AddGroup(out aArg: Integer);

  generic procedure Add<T>(const X : T; const Y : T; out Z : T);
  begin
    Z:=X+Y;
  end;

  var
    R : integer;

  begin
    specialize Add<integer>(4,5,R);
    aArg := R;
  end;

var
   r : integer;
begin
   AddGroup(r);
   if r <> 9 then
     Halt(1);
end.

