Program Example10;

{ Program to demonstrate the Execl function. }

Uses oldlinux, strings;

begin
  { Execute 'ls -l', with current environment. }
  { 'ls' is NOT looked for in PATH environment variable.}
  Execl ('/bin/ls -l');
end.
