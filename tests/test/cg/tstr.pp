procedure test_shortstr;
type
  tlocalstring = shortstring;
var
  l: longint;
  c: cardinal;
  f: real;
  i: int64;
  q: qword;
  s: tlocalstring;
  len: byte;
  frac: word;

  procedure check(const ss: tlocalstring);
  begin
    if s <> ss then
      begin
        writeln('error!');
        halt(1);
      end;
  end;

begin
  writeln('testing str(<value>,shortstring)...');

  l := -1;
  str(l,s);
  check('-1');
  str(l:0,s);
  check('-1');
  str(l:1,s);
  check('-1');
  str(l:2,s);
  check('-1');
  str(l:3,s);
  check(' -1');
  len := 4;
  str(l:len,s);
  check('  -1');

  c := 10;
  str(c,s);
  check('10');
  str(c:0,s);
  check('10');
  str(c:1,s);
  check('10');
  str(c:2,s);
  check('10');
  str(c:3,s);
  check(' 10');

  { for more in-depth tests of str_real, see ../tstreal[1,2].pp }
  f := -1.12345;
  str(f,s);
  check('-1.123450000000000E+000');
  str(f:0,s);
  check('-1.1E+000');
  str(f:1,s);
  check('-1.1E+000');
  str(f:2,s);
  check('-1.1E+000');
  str(f:3,s);
  check('-1.1E+000');
  str(f:4,s);
  check('-1.1E+000');
  str(f:0:0,s);
  check('-1');
  str(f:0:1,s);
  check('-1.1');
  str(f:0:2,s);
  check('-1.12');
  str(f:1:0,s);
  check('-1');
  str(f:1:1,s);
  check('-1.1');
  str(f:5:0,s);
  check('   -1');
  str(f:5:1,s);
  check(' -1.1');
  str(f:5:2,s);
  check('-1.12');
  len := 6;
  frac := 2;
  str(f:len:frac,s);
  check(' -1.12');

  i := -1;
  str(i,s);
  check('-1');
  str(i:0,s);
  check('-1');
  str(i:1,s);
  check('-1');
  str(i:2,s);
  check('-1');
  str(i:3,s);
  check(' -1');

  q := 10;
  str(q,s);
  check('10');
  str(q:0,s);
  check('10');
  str(q:1,s);
  check('10');
  str(q:2,s);
  check('10');
  str(q:3,s);
  check(' 10');
end;

procedure test_ansistr;
type
  tlocalstring = ansistring;
var
  l: longint;
  c: cardinal;
  f: real;
  i: int64;
  q: qword;
  s: tlocalstring;
  len: shortint;
  frac: smallint;

  procedure check(const ss: tlocalstring);
  begin
    if s <> ss then
      begin
        writeln('error!');
        halt(1);
      end;
  end;

begin
  writeln('testing str(<value>,ansistring)...');

  l := -1;
  str(l,s);
  check('-1');
  str(l:0,s);
  check('-1');
  str(l:1,s);
  check('-1');
  str(l:2,s);
  check('-1');
  str(l:3,s);
  check(' -1');
  len := 4;
  str(l:len,s);
  check('  -1');

  c := 10;
  str(c,s);
  check('10');
  str(c:0,s);
  check('10');
  str(c:1,s);
  check('10');
  str(c:2,s);
  check('10');
  str(c:3,s);
  check(' 10');

  { for more in-depth tests of str_real, see ../tstreal[1,2].pp }
  f := -1.12345;
  str(f,s);
  check('-1.123450000000000E+000');
  str(f:0,s);
  check('-1.1E+000');
  str(f:1,s);
  check('-1.1E+000');
  str(f:2,s);
  check('-1.1E+000');
  str(f:3,s);
  check('-1.1E+000');
  str(f:4,s);
  check('-1.1E+000');
  str(f:0:0,s);
  check('-1');
  str(f:0:1,s);
  check('-1.1');
  str(f:0:2,s);
  check('-1.12');
  str(f:1:0,s);
  check('-1');
  str(f:1:1,s);
  check('-1.1');
  str(f:5:0,s);
  check('   -1');
  str(f:5:1,s);
  check(' -1.1');
  str(f:5:2,s);
  check('-1.12');
  len := 6;
  frac := 2;
  str(f:len:frac,s);
  check(' -1.12');

  i := -1;
  str(i,s);
  check('-1');
  str(i:0,s);
  check('-1');
  str(i:1,s);
  check('-1');
  str(i:2,s);
  check('-1');
  str(i:3,s);
  check(' -1');

  q := 10;
  str(q,s);
  check('10');
  str(q:0,s);
  check('10');
  str(q:1,s);
  check('10');
  str(q:2,s);
  check('10');
  str(q:3,s);
  check(' 10');
end;

{$ifdef haswidestring}
procedure test_widestr;
type
  tlocalstring = widestring;
var
  l: longint;
  c: cardinal;
  f: real;
  i: int64;
  q: qword;
  s: tlocalstring;
  len: longint;
  frac: cardinal;

  procedure check(const ss: tlocalstring);
  begin
    if s <> ss then
      begin
        writeln('error!');
        halt(1);
      end;
  end;

begin
  writeln('testing str(<value>,widestring)...');

  l := -1;
  str(l,s);
  check('-1');
  str(l:0,s);
  check('-1');
  str(l:1,s);
  check('-1');
  str(l:2,s);
  check('-1');
  str(l:3,s);
  check(' -1');
  len := 4;
  str(l:len,s);
  check('  -1');

  c := 10;
  str(c,s);
  check('10');
  str(c:0,s);
  check('10');
  str(c:1,s);
  check('10');
  str(c:2,s);
  check('10');
  str(c:3,s);
  check(' 10');

  { for more in-depth tests of str_real, see ../tstreal[1,2].pp }
  f := -1.12345;
  str(f,s);
  check('-1.123450000000000E+000');
  str(f:0,s);
  check('-1.1E+000');
  str(f:1,s);
  check('-1.1E+000');
  str(f:2,s);
  check('-1.1E+000');
  str(f:3,s);
  check('-1.1E+000');
  str(f:4,s);
  check('-1.1E+000');
  str(f:0:0,s);
  check('-1');
  str(f:0:1,s);
  check('-1.1');
  str(f:0:2,s);
  check('-1.12');
  str(f:1:0,s);
  check('-1');
  str(f:1:1,s);
  check('-1.1');
  str(f:5:0,s);
  check('   -1');
  str(f:5:1,s);
  check(' -1.1');
  str(f:5:2,s);
  check('-1.12');
  len := 6;
  frac := 2;
  str(f:len:frac,s);
  check(' -1.12');

  i := -1;
  str(i,s);
  check('-1');
  str(i:0,s);
  check('-1');
  str(i:1,s);
  check('-1');
  str(i:2,s);
  check('-1');
  str(i:3,s);
  check(' -1');

  q := 10;
  str(q,s);
  check('10');
  str(q:0,s);
  check('10');
  str(q:1,s);
  check('10');
  str(q:2,s);
  check('10');
  str(q:3,s);
  check(' 10');
end;
{$endif haswidestring}

begin
  test_shortstr;
  test_ansistr;
{$ifdef haswidestring}
  test_widestr;
{$endif haswidestring}
  writeln('str tests successful!');
end.
