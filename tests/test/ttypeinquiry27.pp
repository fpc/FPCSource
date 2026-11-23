{ %FAIL }
{ "type of" with a result type that has no name cannot be a generic parameter }
program ttypeinquiry27;

{$mode objfpc}

type
  generic TGen<T> = record
    v: T;
  end;

procedure kek;
begin
end;

type
  TKekGen = specialize TGen<type of @kek>;

begin
end.
