{ %NEEDLIBRARY }

{$ifdef mswindows}
 {$define supported}
{$endif mswindows}
{$ifdef Unix}
 {$define supported}
{$endif Unix}
{$ifndef fpc}
   {$define supported}
{$endif}

{$ifdef supported}

const
  {$ifdef windows}
    libname='tweaklib1.dll';
  {$else}
    libname='tweaklib1';
    {$linklib tweaklib1}
  {$endif}

var
{$ifndef darwin}
  _myvar: cardinal; weakexternal libname name '_MYVAR';
  _myvar2: cardinal; weakexternal libname name '_MYVAR2';
{$else darwin}
  { needs to be fixed in the compiler }
  _myvar: cardinal; weakexternal libname name 'MYVAR';
  _myvar2: cardinal; weakexternal libname name 'MYVAR2';
{$endif darwin}

function _test: cardinal; weakexternal libname name '_TEST';

procedure _test2; weakexternal libname name '_TEST2';

begin
{$ifdef darwin}
  if (paramcount=0) then
    halt(0);
{$endif}
  if not assigned(@_test) then
    halt(1);
  if assigned(@_test2) then
    halt(2);
  if _test<>$facef00d then
    halt(3);
  if not assigned(@_myvar) then
    halt(4);
  if assigned(@_myvar2) then
    halt(5);
  if (_myvar<>$12345678) then
    halt(6);
end.
{$else supported}
begin
  { no library support }
end.
{$endif supported}
