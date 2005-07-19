
Var
  S: ansistring;
  SS: shortstring;
Begin
  SS := 'find';
  SetLength(S, 300);
  S := S + SS;
  Writeln(Pos(SS, S)); // This will not find the occurance of 'find'
  if pos(ss,s)<>301 then
    halt(1);
End.
