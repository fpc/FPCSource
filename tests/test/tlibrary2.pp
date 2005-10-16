{ %NEEDLIBRARY }

{ Test program to test linking to fpc library }

{$ifdef win32}
 {$define supported}
{$endif win32}
{$ifdef Unix}
 {$define supported}
{$endif Unix}
{$ifndef fpc}
   {$define supported}
{$endif}

{$ifdef supported}

const
{$ifdef win32}
  libname='tlibrary1.dll';
{$else}
  libname='tlibrary1';
  {$linklib tlibrary1}
{$endif}

procedure test;external libname name 'TestName';

begin
  test;
end.
{$else not supported}
begin
  Writeln('Dummy test because target does not support libraries');
end.
{$endif not supported}
