{$mode delphi}

program Project1;

function add(a, b : Integer) : Int64;
begin
 result := Int64(a) * $FFFFFFFF + b;
end;

begin

 writeLn(add(200210, 1)); // Linux x86_64: 859895402131951
                          // Windows i386: -200209
 if add(200210, 1)<>859895402131951 then
   halt(1);
end.
