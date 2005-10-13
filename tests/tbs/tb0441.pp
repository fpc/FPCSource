{$mode fpc}

operator :=(x:ShortInt)RESULT:ShortString;
  begin
    Val(RESULT,x);
  end;

var
  s:ShortString;
begin
  s:=12;
end.
