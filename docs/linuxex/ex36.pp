Program Example36;

{ Program to demonstrate the AssignPipe function. }

Uses linux;

Var pipi,pipo : Text;
    s : String;
    
begin
  Writeln ('Assigning Pipes.');
  assignpipe(pipi,pipo);
  if linuxerror<>0 then 
    Writeln('Error assigning pipes !');
  Writeln ('Rewrite Output pipe');
  rewrite (pipo);
  Writeln ('Reset Input pipe ');
  reset (pipi);
  Writeln ('Writing to pipe, and flushing.');
  Writeln (pipo,'This is a textstring');flush(pipo);
  Writeln ('Reading from pipe.');
  Readln (pipi,s);
  Writeln ('Read from pipe : ',s);
  close (pipo);
  close (pipi);
  writeln ('Closed pipes.');
  writeln
end.