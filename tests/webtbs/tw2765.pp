{ Source provided for Free Pascal Bug Report 2765 }
{ Submitted by "Michael Van Canneyt" on  2003-11-04 }
{ e-mail: michael.vancanneyt‚wisa.be }
program testw;

var
  P : PWideChar;
  PA : PChar;
  S,T : AnsiString;

begin
  S:='SomeThing';
  P:=PWideChar(S);
  PA:=PChar(Pointer(P));
  Writeln('P : ',PA);
  T:=WideCharToString(P);
  Writeln('T : ',T);
end.
