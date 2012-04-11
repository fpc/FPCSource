{ %norun }

{$ifdef CPUX86_64}
{$ifndef WINDOWS}
{$PIC+}
{$endif WINDOWS}
{$endif CPUX86_64}

{$ifdef mswindows}
 {$define supported}
{$endif win32}
{$if defined(Unix) and not defined(aix)}
 {$define supported}
{$endif Unix}
{$ifndef fpc}
   {$define supported}
{$endif}

{$ifdef supported}
library tweaklib1;


{ On darwin, you always have to link with a (usually stub) library containing
  all symbols, weak or not. Only at run time the weak symbols may be missing.
  The real test is done there by tweaklib2/tweaklib3.
}

var
  _myvar: cardinal; export name '_MYVAR';
{$ifdef darwin}
  _myvar2: cardinal; export name '_MYVAR2';
{$endif darwin}

function _test: cardinal;
begin
  writeln('this is a test');
  _test:=$facef00d;
  _myvar:=$12345678;
end;

{$ifdef darwin}
procedure _test2;
begin
end;
{$endif darwin}

exports
{$ifdef darwin}
  _test2 name '_TEST2',
  _myvar2,
{$endif darwin}
  _test name '_TEST',
  _myvar;

{$endif supported}
end.
