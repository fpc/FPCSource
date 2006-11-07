type
  tp1 = function(var i : longint) : longint;

  tp2 = type tp1;

procedure p(f : tp1);
  begin
  end;


procedure p(f : tp2);
  begin
  end;

var
  f : tp1;

begin
  f:=nil;
  if assigned(f) then
    p(f);
end.
