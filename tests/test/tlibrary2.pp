{ %skiptarget=go32v2 }
{ %NEEDLIBRARY }
{ %delfiles=tlibrary1 }

{ Test program to test linking to fpc library }

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
  libname='tlibrary1.dll';
{$else}
  libname='tlibrary1';
  {$linklib tlibrary1}
{$endif}

procedure test;external libname name 'TestName';

begin
  if islibrary then
    halt(3);
  if moduleislib then
    halt(4);
  test;
end.
{$else not supported}
begin
  Writeln('Dummy test because target does not support libraries');
end.
{$endif not supported}
