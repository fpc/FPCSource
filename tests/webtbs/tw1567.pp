procedure hallo(a:array of word);
begin
   writeln(a[0],' ',a[1],' ',a[2]);
   if (a[0]<>999) or
      (a[1]<>999) or
      (a[2]<>999) then
    halt(1);
end;

begin
   hallo([999,999,999]);
end.
