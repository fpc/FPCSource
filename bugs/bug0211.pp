var
   a,b : boolean;
   c : byte;
   i : longint;

procedure Error;
begin
   Writeln('Error in bug0211');
   Halt(1);
end;

begin
  c:=5;
  a:=boolean(c);
  if a and not a then
    Begin
       Writeln('FPC is crazy !!');
       Error;
    End;
  i:=256;
  a:=boolean(i);
  Writeln('boolean(256) =',a);
end.
  
