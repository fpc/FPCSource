uses Dos;
var
  f : file;
  l : longint;
  dt : datetime;
begin
  assign(f,'tb0432.tmp');
  rewrite(f);
  close(f);

  { Set Invalid date }
  dt.year:=2001;
  dt.month:=2;
  dt.day:=30;
  packtime(dt,l);

  SetFTime(f,l);
  writeln(doserror);

  if doserror<>13 then
   begin
     Writeln('Wrong doserror');
     halt(1);
   end;

end.
