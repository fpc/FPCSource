{$mode fpc}

operator :=(x:LongInt)RESULT:ShortString;
  begin
    Val(RESULT,x);
  end;

var
  s:ShortString;
begin
  s:=12;
end.
