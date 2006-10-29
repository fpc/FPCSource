var i:integer;
    H:single;
    s:string;

begin
  H:= 10;
  for i:=1 to 25 do
    begin
      str(H:8,s);
      if (s[1] <> ' ') or
         (length(s) <> 8) then
        begin
          writeln(s);
          halt(1);
        end;
      H:= H / 10.0;
    end;
end.

