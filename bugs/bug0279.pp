{$H+}
Program AnsiTest;

Type
   PS=^String;

var
  P:PS;
Begin
  New(P);
  P^:='';
  P^:=P^+'BLAH';
  P^:=P^+' '+P^;
  Writeln(P^);
  Dispose(P);
end.

