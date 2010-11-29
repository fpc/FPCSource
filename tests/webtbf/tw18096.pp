{ %fail }
{$mode objfpc}
type
  generic tc1<T> = class
  public
    x : T;
  end;
  
  generic tc2<T> = class
  type tc2a = specialize tc1<T>;
  var x : tc2a;
  end;
  
  tc2_Integer = specialize tc2<Integer>;

var
  a : tc2_Integer;
begin
  a := tc2_Integer.Create;
  a.x := tc2.tc2a.Create; // this is not allowed, user must use specialization of tc2
  a.x.x := 99;
  if (a.x.x <> 99) then
    Halt(1);
end.
