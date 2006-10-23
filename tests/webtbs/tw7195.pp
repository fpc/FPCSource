{$mode delphi}

Function Test(S : single) : single;
begin
  Result := sqrt(s);
end;

Function Test2(S : single) : single;
begin
  Result := sqr(s);
end;

begin
 writeln(Test(0.123445));
 writeln(Test2(0.123445));
end.

