var
 e:boolean;
 a:integer;
begin
 e:=true;
 a:=3;
 while (a<5) and e do begin
  e:=false;
  write('*');
  continue;
 end;
end.