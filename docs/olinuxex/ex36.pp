Program Example36;

{ Program to demonstrate the AssignPipe function. }

Uses oldlinux;

Var pipi,pipo : Text;
    s : String;

begin
  Writeln ('Assigning Pipes.');
  If Not assignpipe(pipi,pipo) then
    Writeln('Error assigning pipes !',LinuxError);
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