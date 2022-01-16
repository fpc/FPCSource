{$mode objfpc}

program test;

generic procedure DoThis<T>(msg: T);
begin
end;

generic procedure DoThis<T>(a: array of T);
begin
end;

begin
end.
