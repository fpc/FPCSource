{ %CPU=i386 }
{ %NOTE=This test requires an installed nasm assembler }

{$define dummy}
{$ifdef win32}
{$output_format asw}
{$undef dummy}
{$endif win32}

{$ifdef go32v2}
{$output_format as}
{$undef dummy}
{$endif go32v2}

{$ifdef Unix}
{$output_format as}
{$undef dummy}
{$endif unix}

{$ifdef dummy}
const
  x = ' this is a dummy test';
{$else}
uses utasout;
{$endif}

begin
  Writeln('x = ',x);
end.
