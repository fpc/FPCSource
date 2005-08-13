{ %target=linux }
{$mode objfpc}

{$linklib libuw3964}

function testfunc : longint;public name 'testfunc';
begin
  result:=1234;
end;

function f : longint;external name 'f';

exports
  testfunc name 'testfunc';

begin
  writeln(f);
end.
