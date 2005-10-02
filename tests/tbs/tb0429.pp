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
end;
procedure lowercase(c:ansistring);overload;
begin
  writeln('ansi');
  err:=false;
end;

var
  w : widestring;
  s : ansistring;
  i : longint;
begin
  err:=true;
  { this should choosse the ansistring version }
  w:='';
  for i:=1 to 300 do w:=w+'.';
  lowercase(w);
  if err then
   begin
     writeln('Wrong lowercase Error!');
     halt(1);
   end;

   { check if ansistring pos() call is not broken }
   s:='';
   for i:=1 to 300 do s:=s+'.';
   s:=s+'test';
   if pos('test',s)<>301 then
    begin
      writeln('Pos(ansistring) Error!');
      halt(1);
    end;

end.
