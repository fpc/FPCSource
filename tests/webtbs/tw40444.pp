{$mode iso}
program tw40444;
{
  Test eof and eoln in ISO mode on a text file whose last line has no line end.
}
var
  f: text;
  c: char;
  i: integer;
  n: integer;

procedure check(cond: boolean; code: integer);
begin
  if not cond then
    begin
      writeln('failed: ', code);
      halt(code);
    end;
end;

procedure makefile(s: integer);
begin
  rewrite(f);
  case s of
    1: write(f,'abc');
    2: write(f,'12');
    3: writeln(f,'ab');
    4: ;
    5: begin writeln(f,'l1'); write(f,'l2'); end;
  end;
  close(f);
  reset(f);
end;

begin
  assign(f,'tw40444.tmp');

  makefile(1);
  read(f,c); check(c='a',1);
  read(f,c); check(c='b',2);
  read(f,c); check(c='c',3);
  check(eoln(f),4);
  check(not eof(f),5);
  read(f,c); check(c=' ',6);
  check(eof(f),7);
  close(f);

  makefile(1);
  readln(f);
  check(eof(f),10);
  close(f);

  makefile(2);
  read(f,i); check(i=12,20);
  check(eoln(f),21);
  check(not eof(f),22);
  readln(f);
  check(eof(f),23);
  close(f);

  makefile(3);
  read(f,c); read(f,c); check(c='b',30);
  check(eoln(f),31);
  check(not eof(f),32);
  readln(f);
  check(eof(f),33);
  close(f);

  makefile(4);
  check(eof(f),40);
  close(f);

  makefile(5);
  n:=0;
  while not eof(f) do
    begin
      readln(f);
      n:=n+1;
    end;
  check(n=2,50);
  close(f);

  makefile(5);
  n:=0;
  while not eof(f) do
    begin
      while not eoln(f) do
        read(f,c);
      readln(f);
      n:=n+1;
    end;
  check(n=2,60);
  close(f);

  rewrite(f);
  write(f,'xy');
  reset(f);
  read(f,c); check(c='x',70);
  read(f,c); check(c='y',71);
  check(eoln(f),72);
  check(not eof(f),73);
  read(f,c); check(c=' ',74);
  check(eof(f),75);
  close(f);

  erase(f);
  writeln('ok');
end.
