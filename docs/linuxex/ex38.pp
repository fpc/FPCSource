Program Example38;

{ Program to demonstrate the AssignStream function. }

Uses linux;

Var Si,So : Text;
    S : String;
    i : longint;
        
begin
  if not (paramstr(1)='-son') then
    begin
    Writeln ('Calling son');
    Assignstream (Si,So,'./ex38a -son');
    if linuxerror<>0 then writeln ('AssignStream failed !');
{$i-}
{    rewrite (so);
    if ioresult<>0 then writeln ('Error !!');
    reset(si);
    if ioresult<>0 then writeln ('Error !!');
 }   Writeln ('Speaking to son');
    For i:=1 to 10 do writeln (so,'Hello son !');
    For i:=1 to 3 do writeln (so,'Hello chap !');
    flush (so);
    while not eof(si) do
      begin
      readln (si,s);
      writeln ('Father: Son said : ',S);
      end;
    Writeln ('Stopped conversation');
    Close (Si);
    Close (so);
    Writeln ('Put down phone');
    end
  Else
    begin
    Writeln ('This is the son ');
    While not eof (input) do 
      begin
      readln (s);
      if pos ('Hello son !',S)<>0 then
         Writeln ('Hello Dad !')
      else 
         writeln ('Who are you ?');
      end
    end 
end.
