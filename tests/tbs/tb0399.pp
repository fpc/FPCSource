procedure error;
  begin
     writeln('Problem with octal constants');
     halt(1);
  end;

begin
   if 8<>&10 then
     error;
   if 1<>&1 then
     error;
   if 64<>&100 then
     error;
   if 33<>&41 then
     error;
   if 33<>&41 then
     error;
   if 12345678<>&57060516 then
     error;
end.
