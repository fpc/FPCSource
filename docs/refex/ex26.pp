Program Example26;

{ Program to demonstrate the Flush function. }

Var F : Text;

begin
  { Assign F to standard output }
  Assign (F,'');
  Rewrite (F);
  Writeln (F,'This line is written first, but appears later !');
  { At this point the text is in the internal pascal buffer,
    and not yet written to standard output }
  Writeln ('This line appears first, but is written later !');
  { A writeln to 'output' always causes a flush - so this text is
    written to screen }
  Flush (f);
  { At this point, the text written to F is written to screen. }
  Write (F,'Finishing ');
  Close (f);  { Closing a file always causes a flush first }
  Writeln ('off.');
end.
