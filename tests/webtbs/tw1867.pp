uses SysUtils;

var
  s : string;
begin
  s:=formatdatetime ('hh:nn:ss.zzz', encodetime (12, 30, 44, 4));
  writeln(s);
  if s<>'12:30:44.004' then
   begin
     writeln('ERROR!');
     halt(1);
   end;
end.
