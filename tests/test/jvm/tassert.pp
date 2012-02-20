program tassert;

{$mode objfpc}

var
  b: byte;
  caught: boolean;
begin
  caught:=false;
  try
    assert(b=1,'yow');
  except
    caught:=true;
  end;
  if not caught then
    halt(1);
end.
