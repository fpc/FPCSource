{$mode objfpc}

function test: longint;

  var
    l: longint;

  procedure nest;
    begin
      if result<>111 then
        halt(1);
      if l<>222 then
        halt(2);
      l:=1231;
      result:=555;
    end;

begin
  result:=111;
  l:=222;
  nest;
  if l<>1231 then
    halt(3);
  if result=555 then
    exit;
  result:=666;
end;

begin
  if test<>555 then
    halt(4);
end.
