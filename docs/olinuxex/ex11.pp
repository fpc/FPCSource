Program Example11;

{ Program to demonstrate the Execle function. }

Uses oldlinux, strings;

begin
  { Execute 'ls -l', with current environment. }
  { 'ls' is NOT looked for in PATH environment variable.}
  { envp is defined in the system unit.}
  Execle ('/bin/ls -l',envp);
end.
