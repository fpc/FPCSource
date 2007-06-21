{ Source provided for Free Pascal Bug Report 1021 }
{ Submitted by "Oliver Puetz" on  2000-07-03 }
{ e-mail: Oliver.Puetz@gmx.de }
{
  Free Pascal Compiler version 0.99.15 [2000/03/30] for i386
  Copyright (c) 1993-2000 by Florian Klaempfl
  Win NT 4.0 Fixpak 2

  With TFloat = EXTENDED Writeln resumes 0.0   0.0   1
  With TFloat = DOUBLE   Writeln resumes 0.0   1.0   1

  Thus only the write-command seems not to transfer the extended a equals 1
  to a string like '1'
}

type  tfloat  = extended;

var   a, b    : tfloat;
      i       : INTEGER;
      f : text;
begin
  case sizeof(tfloat) of
    4:  writeln('single');
    8:  writeln('double');
   10:  writeln('extended');
   else writeln(sizeof(tfloat));
  end;
  a := 0;
  b := 1 - a;
  i := Round(b);
  writeln(a:30:20, b:30:20, i:10);
  assign(f,'tbug1021.tmp');
  rewrite(f);
  writeln(f,a:30:20,' ',b:30:20,' ',i:10);
  close(f);
  reset(f);
  read(f,a);
  read(f,b);
  read(f,i);
  if (a<>0.0) then
    begin
      Writeln('Error reading A value, should be zero');
      Halt(1);
    end;
  if (b<>1.0) then
    begin
      Writeln('Error reading B value, should be one');
      Halt(1);
    end;
  if (i<>1) then
    begin
      Writeln('Error reading I value, should be one');
      Halt(1);
    end;
  close(f);
  erase(f);
end.
