Program Example79;

{ Program to demonstrate the FpExecVP function. }

Uses Unix, strings;

Const Arg0 : PChar = 'ls';
      Arg1 : Pchar = '-l';

Var PP : PPchar;


begin
  GetMem (PP,3*SizeOf(Pchar));
  PP[0]:=Arg0;
  PP[1]:=Arg1;
  PP[2]:=Nil;
  { Execute 'ls -l', with current environment. }
  { 'ls' is looked for in PATH environment variable.}
  fpExecvp ('ls',pp);
end.
