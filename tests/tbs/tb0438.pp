{$ifdef fpc}{$mode objfpc}{$endif}

procedure p(a : array of const);
  var
    i : integer;
  begin
    for i:=low(a) to high(a) do
     begin
       write(i,': ');
       if (a[i].vtype=vtpchar) then
        begin
          writeln('"',a[i].vpchar,'"');
          if (a[i].vpchar<>'test') then
           begin
             writeln('Wrong string content');
             halt(1);
           end;
        end
       else
        begin
          writeln('No string type (',a[i].vtype,')');
          halt(1);
        end;
     end;
  end;

var
   a : array[0..25] of char;

begin
   a:='test';
   p([a,a]);
end.
