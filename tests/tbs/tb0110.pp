{ Old file: tbs0129.pp }
{ endless loop with while/continue                      OK 0.99.6 (FK) }

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
