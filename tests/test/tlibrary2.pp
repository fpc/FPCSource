{ %OPT=-Fl. }
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
  libname='libtlibrary1.so';
{$endif}

procedure test;external libname name 'TestName';

begin
  test;
end.
{$endif supported}
