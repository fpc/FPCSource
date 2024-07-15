{ %fail }
{$mode objfpc}

program test;

type
  TMyRecord = record
  end;

function printf(format: PChar): Integer; external; varargs;

var
  r: TMyRecord;
begin
  printf('test', r); // Fatal: Internal error 2004102303
end.
