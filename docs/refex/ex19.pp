Program Example19;

{ Program to demonstrate the Eoln function. }

begin
  { This program waits for keyboard input. }
  { It will print True when an empty line is put in,
    and false when you type a non-empty line.
    It will only stop when you press enter.}
  While not Eoln do
    Writeln (eoln);
end.
