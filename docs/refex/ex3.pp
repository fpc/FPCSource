Program Example3;

{ Program to demonstrate the Append function. }

Var f : text;

begin
  Assign (f,'test.txt');
  Rewrite (f);            { file is opened for write, and emptied }
  Writeln (F,'This is the first line of text.txt');
  close (f);
  Append(f);              { file is opened for write, but NOT emptied.
                            any text written to it is appended.}
  Writeln (f,'This is the second line of text.txt');
  close (f);
end.
