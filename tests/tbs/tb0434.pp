
function dummy : longint;
begin
  dummy:=1;
end;

var
 x:function:longint;
 x2:function:longint;
 y:pointer absolute x;
 y2:pointer absolute x2;
begin
  x2:=@dummy;
  if (y<>nil) or (y2<>pointer(@dummy)) then
   begin
     Writeln('Wrong code generated for absolute to procvarsmy');
     halt(1);
   end;
end.
