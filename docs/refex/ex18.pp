Program Example18;

{ Program to demonstrate the Eof function. }

Var T1,T2 : text;
    C : Char;

begin
  { Set file to read from. Empty means from standard input.}
  assign (t1,paramstr(1));
  reset (t1);
  { Set file to write to. Empty means to standard output. }
  assign (t2,paramstr(2));
  rewrite (t2);
  While not eof(t1) do
    begin
    read (t1,C);
    write (t2,C);
    end;
  Close (t1);
  Close (t2);
end.
