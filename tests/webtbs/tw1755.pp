var
  err : boolean;

begin
  err:=false;

  write('assigned(nil): ');
  if assigned(nil) then
   err:=true
  else
   writeln('nil');
  write('assigned(pointer(0)): ');
  if assigned(pointer(0)) then
   err:=true
  else
   writeln('nil');
  write('assigned(pointer(10000)): ');
  if assigned(pointer(10000)) then
   writeln('assigned')
  else
   err:=true;

  if err then
   begin
     writeln('err');
     halt(1);
   end;

end.
