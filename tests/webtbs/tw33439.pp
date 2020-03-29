Var Cur : Currency ;

Begin
  Cur:=100000000000;
  Cur:=Cur * 7 ;
  if Cur<>700000000000 then
    halt(1);
  writeln('ok');
End.
