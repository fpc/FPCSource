Program Example6;

{ Program to demonstrate the BlockRead and BlockWrite functions. }

Var Fin, fout : File;
    NumRead,NumWritten : Word;
    Buf : Array[1..2048] of byte;
    Total : Longint;

begin
  Assign (Fin, Paramstr(1));
  Assign (Fout,Paramstr(2));
  Reset (Fin,1);
  Rewrite (Fout,1);
  Total:=0;
  Repeat
    BlockRead (Fin,buf,Sizeof(buf),NumRead);
    BlockWrite (Fout,Buf,NumRead,NumWritten);
    inc(Total,NumWritten);
  Until (NumRead=0) or (NumWritten<>NumRead);
  Write ('Copied ',Total,' bytes from file ',paramstr(1));
  Writeln (' to file ',paramstr(2));
  close(fin);
  close(fout);
end.
