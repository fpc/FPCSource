Program Example52;

{ Program to demonstrate the Rewrite function. }

Var F : File;
    I : longint;

begin
  Assign (F,'Test.tmp');
  { Create the file. Recordsize is 4 }
  Rewrite (F,Sizeof(I));
  For I:=1 to 10 do
    BlockWrite (F,I,1);
  close (f);
  { F contains now a binary representation of
    10 longints going from 1 to 10 }
end.
