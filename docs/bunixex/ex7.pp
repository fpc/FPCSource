Program Example7;

{ Program to demonstrate the Execve function. }

Uses BaseUnix, strings;

Const Arg0 : PChar = '/bin/ls';
      Arg1 : Pchar = '-l';

Var PP : PPchar;


begin
  GetMem (PP,3*SizeOf(Pchar));
  PP[0]:=Arg0;
  PP[1]:=Arg1;
  PP[3]:=Nil;
  { Execute '/bin/ls -l', with current environment }
  { Envp is defined in system.inc }
  fpExecVe ('/bin/ls',pp,envp);
end.
