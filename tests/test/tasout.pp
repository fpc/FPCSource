{ %CPU=i386 }

{$define dummy}
{$ifdef win32}
{$output_format asw}
{$undef dummy}
{$endif win32}

{$ifdef go32v2}
{$output_format as}
{$undef dummy}
{$endif go32v2}

{$ifdef linux}
{$output_format as}
{$undef dummy}
{$endif linux}

{$ifdef dummy}
const
  x = ' this is a dummy test';
{$else}
uses utasout;
{$endif}

begin
  Writeln('x = ',x);
end.
