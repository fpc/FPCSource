Program Example13;

{ Program to demonstrate the Shell function. }

Uses oldlinux;

begin
  { This will send the output of 'ls -l' to the file ls.out }
  { thanks to the shell's redirection functionality }
  Shell ('ls -l >ls.out')
end.
