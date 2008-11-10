{ %norun }

{ %target=darwin }
{ %opt=-olibtweaklib1.dylib }

{$PIC+}

library tweaklib;

var
  _myvar: cardinal; export name '_MYVAR';

function _test: cardinal;
begin
  writeln('this is a test');
  _test:=$facef00d;
  _myvar:=$12345678;
end;

exports
  _test name '_TEST',
  _myvar name '_MYVAR';

end.
