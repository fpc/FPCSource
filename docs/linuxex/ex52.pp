Program Example52;

{ Program to demonstrate the GetFileHandle (was : GetFS) function.
  In 1.9.x it has been generalised over all platforms }

Uses SysUtils;

begin
  Writeln ('File descriptor of input  ',getFileHandle(input));
  Writeln ('File descriptor of output ',getFileHandle(output));
  Writeln ('File descriptor of stderr ',getFileHandle(stderr));
end.
