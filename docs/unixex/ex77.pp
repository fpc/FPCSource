Program Example77;

{ Program to demonstrate the FPExecL function. }

Uses Unix, strings;

begin
  { Execute 'ls -l', with current environment. }
  { 'ls' is NOT looked for in PATH environment variable.}
  FpExecL ('/bin/ls',['-l']);
end.
