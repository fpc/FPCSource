Program Example52;

{ Program to demonstrate the GetFS function. }

Uses oldlinux;

begin
  Writeln ('File descriptor of input  ',getfs(input));
  Writeln ('File descriptor of output ',getfs(output));
  Writeln ('File descriptor of stderr ',getfs(stderr));
end.
