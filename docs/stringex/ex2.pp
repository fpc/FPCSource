Program Example2;

Uses strings;

{ Program to demonstrate the StrPCopy function. }

Const S = 'This is a normal string.';

Var P : Pchar;

begin
  p:=StrAlloc (length(S)+1);
  if StrPCopy (P,S)<>P then
    Writeln ('This is impossible !!')
  else
    writeln (P);
end.
