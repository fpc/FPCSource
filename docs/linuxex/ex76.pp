Program Example76;

{ Program to demonstrate the FpExeclp function. }

Uses Unix, strings;

begin
  { Execute 'ls -l', with current environment. }
  { 'ls' is looked for in PATH environment variable.}
  { envp is defined in the system unit.}
  FpExeclp ('ls',['-l']);
end.
