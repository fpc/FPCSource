Program Example23;

{ Program to demonstrate the FilePos function. }

Var F : File of Longint;
    L,FP : longint;

begin
  { Fill a file with data :
    Each position contains the position ! }
  Assign (F,'test.tmp');
  Rewrite (F);
  For L:=0 to 100 do
    begin
    FP:=FilePos(F);
    Write (F,FP);
    end;
  Close (F);
  Reset (F);
  { If all goes well, nothing is displayed here. }
  While not (Eof(F)) do
    begin
    FP:=FilePos (F);
    Read (F,L);
    if L<>FP then
      Writeln ('Something wrong: Got ',l,' on pos ',FP);
    end;
  Close (F);
  Erase (f);
end.
