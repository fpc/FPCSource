type
 fontdataty = record
   name: ansistring;
   int: longint;
 end;
 
var
  r1: fontdataty;
  r2: fontdataty;
begin
  fillchar(r1,sizeof(r1),0);
  r1.int:= 12345;
  writeln(r1.int);
  r2:=r1;
  writeln(r2.int);
  if r1.int <> r2.int then begin
    writeln('Test failed!');
    Halt(1);
  end;
  writeln('Test OK.');
end.
