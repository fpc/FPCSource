Program Example16;
uses Crt;

{ Program to demonstrate the Sound and NoSound function. }

var
  i : longint;
begin
  WriteLn('You will hear some tones from your speaker');
  while (i<15000) do
   begin
     inc(i,500);
     Sound(i);
     Delay(100);
   end;
  WriteLn('Quiet now!');
  NoSound; {Stop noise}
end.
