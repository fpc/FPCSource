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

procedure test;external 'libtlibrary.so' name 'TestName';

begin
  test;
end.
{$endif supported}
