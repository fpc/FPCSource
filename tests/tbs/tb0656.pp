{ %NORUN }

program tb0656;

{$mode objfpc}

procedure Test1(const aArg);
begin
end;

procedure Test2(const aArg);
begin
  Test1(aArg);
end;

procedure Test3(constref aArg);
begin
end;

procedure Test4(constref aArg);
begin
  Test3(aArg);
end;

begin

end.
