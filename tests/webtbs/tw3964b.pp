{ %needlibrary }
{ %target=linux,haiku }
{$mode objfpc}

{$if (FPC_FULLVERSION<=30301) and defined(linux)}
  uses
    initc;
{$endif (FPC_FULLVERSION<=30301) and defined(linux)}

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
