{ requires nasm }

{$ifdef win32}
{$output_format nasmwin32}
{$endif win32}

{$ifdef go32v2}
{$output_format nasmcoff}
{$endif go32v2}

{$ifdef unix}
{$output_format nasmelf}
{$endif unix}


unit utasout;

interface

var
  x : longint;

implementation

begin
  x:=2;
end.
