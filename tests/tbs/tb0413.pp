{$mode fpc}

var
   s : ansistring;
   ss : shortstring;
   as : ansistring;
   c : char;

begin
   as:='ansistring';
   ss:='shortstring';
   c:='C';
   s:=s+as;
   s:=s+c;
   s:=s+ss;
   s:=s+s;
   if s<>'ansistringCshortstringansistringCshortstring' then
     begin
        writeln('Problem with ansistring appending');
        halt(1);
     end;
end.
