{
  LINES.PP

  Program that counts number of Lines in a file

  Copyright (c) 1992,95 by FP Kl„mpfl
  Translated By Eric Molitor (emolitor@freenet.fsu.edu)

  History:
      29.10.1992       Version 1.0
      3.3.1995         an FPKPascal angepaát
}

program count_lines;

  uses
     dos,crt;

  type
     td = array[1..10000] of byte;

  var
     lines : longint;
     s : searchrec;
     f : file;
     d : ^td;
{$ifdef tp}
     count : word;
     i,z : integer;
{$else}
     count,i,z : longint;
{$endif}

  begin
     lines:=0;
     new(d);
     if paramcount<1 then
       begin
          writeln('Usage: LINES FILENAME.EXT [FILENAME.EXT] ...');
          writeln('  Multiple File Names and Wild Cards Allowed:');
          writeln('  z.B  LINES *.CPP STDIO.H *.ASM');
          halt(1);
       end;
     for i:=1 to paramcount do
       begin
          findfirst(paramstr(i),archive,s);
          while (doserror=0) do
            begin
               gotoxy(1,wherey);
               write('                               ');
               gotoxy(1,wherey);
               write('Scanning: ',s.name);
               assign(f,s.name);
               reset(f,1);
               while not(eof(f)) do
                 begin
                    blockread(f,d^,10000,count);
                    for z:=1 to count do
                      if d^[z]=10 then inc(lines);
                 end;
               close(f);
               findnext(s);
            end;
       end;
     dispose(d);
     gotoxy(1,wherey);
     write('                               ');
     gotoxy(1,wherey);
     if lines=1 then writeln('1 Line') else writeln(lines,' Lines');
  end.
