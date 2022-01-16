{ %FAIL }

program tover8;

{$mode objfpc}

generic procedure Test<T>(aArg: T);
begin
end;

generic function Test<T>(aArg: T): T;
begin
end;

begin

end.
