Program Example9;

{ Program to demonstrate the Execvp function. }

Uses oldlinux, strings;

Const Arg0 : PChar = 'ls';
      Arg1 : Pchar = '-l';

Var PP : PPchar;


begin
  GetMem (PP,3*SizeOf(Pchar));
  PP[0]:=Arg0;
  PP[1]:=Arg1;
  PP[3]:=Nil;
  { Execute 'ls -l', with current environment. }
  { 'ls' is looked for in PATH environment variable.}
  { Envp is defined in the system unit. }
  Execvp ('ls',pp,envp);
end.
