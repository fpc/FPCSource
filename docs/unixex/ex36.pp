Program Example36;

{ Program to demonstrate the AssignPipe function. }

Uses BaseUnix,Unix;

Var pipi,pipo : Text;
    s : String;

begin
  Writeln ('Assigning Pipes.');
  If assignpipe(pipi,pipo)<>0 then
    Writeln('Error assigning pipes !',fpgeterrno);
  Writeln ('Writing to pipe, and flushing.');
  Writeln (pipo,'This is a textstring');close(pipo);
  Writeln ('Reading from pipe.');
  While not eof(pipi) do
    begin
    Readln (pipi,s);
    Writeln ('Read from pipe : ',s);
    end;
  close (pipi);
  writeln ('Closed pipes.');
  writeln
end.