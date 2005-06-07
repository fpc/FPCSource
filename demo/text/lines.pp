{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-98 by Florian Klaempfl

    Line Counter Example

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program count_lines;
{
  Program that counts number of Lines in a file
}

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
     i,z   : integer;
{$else}
     count,i,z : longint;
{$endif}

  begin
     lines:=0;
     new(d);
     if paramcount<1 then
       begin
          writeln('Usage: ',paramstr(0),' filename.ext [filename.ext] ...');
          writeln('  Multiple File Names and Wild Cards Allowed:');
          writeln('  Example: lines *.cpp stdio.h *.asm');
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
