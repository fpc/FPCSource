Program Example23;

{ Program to demonstrate the FilePos function. }

Var F : File of Longint;
    L,FP : longint;
    
begin
  { Fill a file with data : 
    Each position contains the position ! }
  Assign (F,'test.dat');
  Rewrite (F);
  For L:=0 to 100 do
    begin
    FP:=FilePos(F);
    Write (F,FP);
    end;
  Close (F);
  Reset (F);
  { If ll goes well, nothing is displayed here. }
  While not (Eof(F)) do
    begin
    FP:=FilePos (F);
    Read (F,L);
    if L<>FP then 
      Writeln ('Something is wrong here ! : Got ',l,' on pos ',FP);
    end;
  Close (F);
  Erase (f);
end.
