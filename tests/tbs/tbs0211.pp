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
  { the value here is less trivial }
  { BP returns false here !!       }
  { the problem is the converting wordbool to boolean }
  { if wordbool is 256 should not convert true to false !! }

  Writeln('boolean(256) =',a);
end.

