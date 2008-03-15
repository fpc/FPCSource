{ %needlibrary }
{ %target=linux }
{$mode objfpc}

{$linklib tw3964a}

function testfunc : longint;
begin
  result:=1234;
end;

function f : longint;external name 'f';

exports
  testfunc name 'testfunc';

begin
  writeln(f);
end.
