Program Example4;

Uses strings;

{ Program to demonstrate the StrCopy function. }

Const P : PCHar = 'This is a PCHAR string.';

var PP : PChar;

begin
  PP:=StrAlloc(Strlen(P)+1);
  STrCopy (PP,P);
  If StrComp (PP,P)<>0 then
    Writeln ('Oh-oh problems...')
  else
    Writeln ('All is well : PP=',PP);
end.
