{$mode objfpc}
{$h+}

var
  s : rawByteString;
begin
 s:='REGISTER'+'DATA';
 writeln(stringcodepage(s));
 if stringcodepage(s)<>0 then
   halt(1);
 s:=s+'NAME';
 writeln(stringcodepage(s));
 if (stringcodepage(s)<>0) and
    (stringcodepage(s)<>DefaultSystemCodePage) then
   halt(2);
end.
