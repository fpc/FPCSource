{ %version=1.1 }

var
  err : boolean;

procedure lowercase(c:char);overload;
begin
  writeln('char');
end;
procedure lowercase(c:shortstring);overload;
begin
  writeln('short');
  err:=false;
end;
procedure lowercase(c:ansistring);overload;
begin
  writeln('ansi');
end;

var
  w : widestring;
begin
  err:=true;
  { this should choosse the shortstring version }
  lowercase(w);
  if err then
   begin
     writeln('ERROR!');
     halt(1);
   end;
end.

