{ %fail }
{$mode fpc}

function GetROVar:longint;
begin
  GetROVar:=3;
end;

property
  ROVar:longint read GetROVar;

begin
  ROVar:=1;
end.
