Program Example12;

{ Program to demonstrate the Execlp function. }

Uses oldlinux, strings;

begin
  { Execute 'ls -l', with current environment. }
  { 'ls' is looked for in PATH environment variable.}
  { envp is defined in the system unit.}
  Execlp ('ls -l',envp);
end.
