{ %target=linux }
{$mode objfpc}
library uw3964;

  function testfunc : longint;external name 'testfunc';

  function f : longint;public;
    begin
      result:=testfunc;
    end;

  exports
    f;

begin
end.
