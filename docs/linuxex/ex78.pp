Program Example77;

{ Program to demonstrate the FPExecle function. }

Uses Unix, strings;

begin
  { Execute 'ls -l', with current environment. }
  { 'ls' is NOT looked for in PATH environment variable.}
  { envp is defined in the system unit.}
  FpExecLE ('/bin/ls',['-l'],envp);
end.
