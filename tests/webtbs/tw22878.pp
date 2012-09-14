program gr;
{$mode objfpc}
    type
        t0= record
            p: pointer;
        end;
        t1= packed record
            u16: word;
            data: t0;
        end;

        td= class
            function return: t1;
        end;
            function td.return: t1;
            begin
              return.u16:=1;
              return.data.p:=pointer(2);
            end;
var
  c: td;
  r: t1;
begin
  c:=td.create;
  r:=c.return;
  if r.u16<>1 then
    halt(1);
  if r.data.p<>pointer(2) then
    halt(2);
end.

