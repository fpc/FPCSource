Program Example61;

{ Program to demonstrate the SetTextBuf function. }

Var
  Fin,Fout : Text;
  Ch : Char;
  Bufin,Bufout : Array[1..10000] of byte;

begin
  Assign (Fin,paramstr(1));
  Reset (Fin);
  Assign (Fout,paramstr(2));
  Rewrite (Fout);
  { This is harmless before IO has begun }
  { Try this program again on a big file,
    after commenting out the following 2
    lines and recompiling it. }
  SetTextBuf (Fin,Bufin);
  SetTextBuf (Fout,Bufout);
  While not eof(Fin) do
    begin
    Read (Fin,ch);
    write (Fout,ch);
    end;
  Close (Fin);
  Close (Fout);
end.
