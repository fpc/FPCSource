Program Example75;

{ Program to demonstrate the Write(ln) function. }

Var
  F : File of Longint;
  L : Longint;

begin
  Write ('This is on the first line ! '); { No CR/LF pair! }
  Writeln ('And this too...');
  Writeln ('But this is already on the second line...');
  Assign (f,'test.tmp');
  Rewrite (f);
  For L:=1 to 10 do
    write (F,L); { No writeln allowed here ! }
  Close (f);
end.
