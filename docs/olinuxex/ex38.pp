Program Example38;

{ Program to demonstrate the AssignStream function. }

Uses oldlinux;

Var Si,So : Text;
    S : String;
    i : longint;

begin
  if not (paramstr(1)='-son') then
    begin
    Writeln ('Calling son');
    Assignstream (Si,So,'./ex38 -son');
    if linuxerror<>0 then
      begin
      writeln ('AssignStream failed !');
      halt(1);
      end;
    Writeln ('Speaking to son');
    For i:=1 to 10 do
      begin
      writeln (so,'Hello son !');
      if ioresult<>0 then writeln ('Can''t speak to son...');
      end;
    For i:=1 to 3 do writeln (so,'Hello chap !');
    close (so);
    while not eof(si) do
      begin
      readln (si,s);
      writeln ('Father: Son said : ',S);
      end;
    Writeln ('Stopped conversation');
    Close (Si);
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
      end;
    close (output);
    end
end.
