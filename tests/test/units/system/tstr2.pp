
{$ifdef fpc}
  {$ifndef ver1_0}
    {$define haswidestring}
  {$endif}
{$else}
  {$ifndef ver70}
    {$define haswidestring}
  {$endif}
{$endif}

procedure check_shortstr(const test: shortstring; const ref: shortstring);
begin
  if test <> ref then
    begin
      writeln('error!');
      halt(1);
    end;
end;

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
  longval : longint;

begin
  writeln('testing str(<value>,shortstring)...');

  l := -1;
  str(l,s);
  check_shortstr(s,'-1');
  str(l:0,s);
  check_shortstr(s,'-1');
  str(l:1,s);
  check_shortstr(s,'-1');
  str(l:2,s);
  check_shortstr(s,'-1');
  str(l:3,s);
  check_shortstr(s,' -1');
  len := 4;
  str(l:len,s);
  check_shortstr(s,'  -1');

  c := 10;
  str(c,s);
  check_shortstr(s,'10');
  str(c:0,s);
  check_shortstr(s,'10');
  str(c:1,s);
  check_shortstr(s,'10');
  str(c:2,s);
  check_shortstr(s,'10');
  str(c:3,s);
  check_shortstr(s,' 10');

  { for more in-depth tests of str_real, see ../tstreal[1,2].pp }
  f := -1.12345;
{$IFOPT E-}
  str(f:22,s);
  if (sizeof(extended) = 10) or
     (sizeof(extended) = 12) then
    check_shortstr(s,'-1.12345000000000E+000')
  else if sizeof(extended) = 8 then
    check_shortstr(s,'-1.12345000000000E+000')
  else
    check_shortstr(s,'error, not yet implemented!!!!');
{$endif}
  { the number of exponents depends on the mapping of the real type }
  if sizeof(real) = 8 then
    begin
      str(f:0,s);
      check_shortstr(s,'-1.1E+000');
      str(f:1,s);
      check_shortstr(s,'-1.1E+000');
      str(f:2,s);
      check_shortstr(s,'-1.1E+000');
      str(f:3,s);
      check_shortstr(s,'-1.1E+000');
      str(f:4,s);
      check_shortstr(s,'-1.1E+000');
    end
  else
    begin
      str(f:0,s);
      check_shortstr(s,'-1.1E+00');
      str(f:1,s);
      check_shortstr(s,'-1.1E+00');
      str(f:2,s);
      check_shortstr(s,'-1.1E+00');
      str(f:3,s);
      check_shortstr(s,'-1.1E+00');
      str(f:4,s);
      check_shortstr(s,'-1.1E+00');
    end;
  str(f:0:0,s);
  check_shortstr(s,'-1');
  str(f:0:1,s);
  check_shortstr(s,'-1.1');
  str(f:0:2,s);
  check_shortstr(s,'-1.12');
  str(f:1:0,s);
  check_shortstr(s,'-1');
  str(f:1:1,s);
  check_shortstr(s,'-1.1');
  str(f:5:0,s);
  check_shortstr(s,'   -1');
  str(f:5:1,s);
  check_shortstr(s,' -1.1');
  str(f:5:2,s);
  check_shortstr(s,'-1.12');
  len := 6;
  frac := 2;
  str(f:len:frac,s);
  check_shortstr(s,' -1.12');

  i := -1;
  str(i,s);
  check_shortstr(s,'-1');
  str(i:0,s);
  check_shortstr(s,'-1');
  str(i:1,s);
  check_shortstr(s,'-1');
  str(i:2,s);
  check_shortstr(s,'-1');
  str(i:3,s);
  check_shortstr(s,' -1');
  i:=655536;
  str(i,s);
  check_shortstr(s,'655536');
  str(i:0,s);
  check_shortstr(s,'655536');
  str(i:1,s);
  check_shortstr(s,'655536');
  str(i:2,s);
  check_shortstr(s,'655536');
  str(i:3,s);
  check_shortstr(s,'655536');
  longval:=1;
  i:=int64(longval) shl 33;
  str(i,s);
  check_shortstr(s,'8589934592');
  str(i:0,s);
  check_shortstr(s,'8589934592');
  str(i:1,s);
  check_shortstr(s,'8589934592');
  str(i:2,s);
  check_shortstr(s,'8589934592');
  str(i:3,s);
  check_shortstr(s,'8589934592');

  q := 10;
  str(q,s);
  check_shortstr(s,'10');
  str(q:0,s);
  check_shortstr(s,'10');
  str(q:1,s);
  check_shortstr(s,'10');
  str(q:2,s);
  check_shortstr(s,'10');
  str(q:3,s);
  check_shortstr(s,' 10');
  q:=655536;
  str(q,s);
  check_shortstr(s,'655536');
  str(q:0,s);
  check_shortstr(s,'655536');
  str(q:1,s);
  check_shortstr(s,'655536');
  str(q:2,s);
  check_shortstr(s,'655536');
  str(q:3,s);
  check_shortstr(s,'655536');
  longval:=1;
  q:=qword(longval) shl 33;
  str(q,s);
  check_shortstr(s,'8589934592');
  str(q:0,s);
  check_shortstr(s,'8589934592');
  str(q:1,s);
  check_shortstr(s,'8589934592');
  str(q:2,s);
  check_shortstr(s,'8589934592');
  str(q:3,s);
  check_shortstr(s,'8589934592');
end;

procedure check_ansistr(const test: ansistring; const ref: ansistring);
begin
  if test <> ref then
    begin
      writeln('error!');
      halt(1);
    end;
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
  longval : longint;

begin
  writeln('testing str(<value>,ansistring)...');

  l := -1;
  str(l,s);
  check_ansistr(s,'-1');
  str(l:0,s);
  check_ansistr(s,'-1');
  str(l:1,s);
  check_ansistr(s,'-1');
  str(l:2,s);
  check_ansistr(s,'-1');
  str(l:3,s);
  check_ansistr(s,' -1');
  len := 4;
  str(l:len,s);
  check_ansistr(s,'  -1');

  c := 10;
  str(c,s);
  check_ansistr(s,'10');
  str(c:0,s);
  check_ansistr(s,'10');
  str(c:1,s);
  check_ansistr(s,'10');
  str(c:2,s);
  check_ansistr(s,'10');
  str(c:3,s);
  check_ansistr(s,' 10');

  { for more in-depth tests of str_real, see ../tstreal[1,2].pp }
  f := -1.12345;
{$IFOPT E-}
  str(f:22,s);
  if (sizeof(extended) = 10) or
     (sizeof(extended) = 12) then
    check_ansistr(s,'-1.12345000000000E+000')
  else if sizeof(extended) = 8 then
    check_ansistr(s,'-1.12345000000000E+000')
  else
    check_ansistr(s,'error, not yet implemented!!!!');
{$endif}
  { the number of exponents depends on the mapping of the real type }
  if sizeof(real) = 8 then
    begin
      str(f:0,s);
      check_ansistr(s,'-1.1E+000');
      str(f:1,s);
      check_ansistr(s,'-1.1E+000');
      str(f:2,s);
      check_ansistr(s,'-1.1E+000');
      str(f:3,s);
      check_ansistr(s,'-1.1E+000');
      str(f:4,s);
      check_ansistr(s,'-1.1E+000');
    end
  else
    begin
      str(f:0,s);
      check_ansistr(s,'-1.1E+00');
      str(f:1,s);
      check_ansistr(s,'-1.1E+00');
      str(f:2,s);
      check_ansistr(s,'-1.1E+00');
      str(f:3,s);
      check_ansistr(s,'-1.1E+00');
      str(f:4,s);
      check_ansistr(s,'-1.1E+00');
    end;
  str(f:0:0,s);
  check_ansistr(s,'-1');
  str(f:0:1,s);
  check_ansistr(s,'-1.1');
  str(f:0:2,s);
  check_ansistr(s,'-1.12');
  str(f:1:0,s);
  check_ansistr(s,'-1');
  str(f:1:1,s);
  check_ansistr(s,'-1.1');
  str(f:5:0,s);
  check_ansistr(s,'   -1');
  str(f:5:1,s);
  check_ansistr(s,' -1.1');
  str(f:5:2,s);
  check_ansistr(s,'-1.12');
  len := 6;
  frac := 2;
  str(f:len:frac,s);
  check_ansistr(s,' -1.12');

  i := -1;
  str(i,s);
  check_ansistr(s,'-1');
  str(i:0,s);
  check_ansistr(s,'-1');
  str(i:1,s);
  check_ansistr(s,'-1');
  str(i:2,s);
  check_ansistr(s,'-1');
  str(i:3,s);
  check_ansistr(s,' -1');
  i:=655536;
  str(i,s);
  check_ansistr(s,'655536');
  str(i:0,s);
  check_ansistr(s,'655536');
  str(i:1,s);
  check_ansistr(s,'655536');
  str(i:2,s);
  check_ansistr(s,'655536');
  str(i:3,s);
  check_ansistr(s,'655536');
  longval:=1;
  i:=int64(longval) shl 33;
  str(i,s);
  check_ansistr(s,'8589934592');
  str(i:0,s);
  check_ansistr(s,'8589934592');
  str(i:1,s);
  check_ansistr(s,'8589934592');
  str(i:2,s);
  check_ansistr(s,'8589934592');
  str(i:3,s);
  check_ansistr(s,'8589934592');

  q := 10;
  str(q,s);
  check_ansistr(s,'10');
  str(q:0,s);
  check_ansistr(s,'10');
  str(q:1,s);
  check_ansistr(s,'10');
  str(q:2,s);
  check_ansistr(s,'10');
  str(q:3,s);
  check_ansistr(s,' 10');
  q:=655536;
  str(q,s);
  check_ansistr(s,'655536');
  str(q:0,s);
  check_ansistr(s,'655536');
  str(q:1,s);
  check_ansistr(s,'655536');
  str(q:2,s);
  check_ansistr(s,'655536');
  str(q:3,s);
  check_ansistr(s,'655536');
  longval:=1;
  q:=qword(longval) shl 33;
  str(q,s);
  check_ansistr(s,'8589934592');
  str(q:0,s);
  check_ansistr(s,'8589934592');
  str(q:1,s);
  check_ansistr(s,'8589934592');
  str(q:2,s);
  check_ansistr(s,'8589934592');
  str(q:3,s);
  check_ansistr(s,'8589934592');
end;

{$ifdef haswidestring}
procedure check_widestr(const test: widestring; const ref: widestring);
begin
  if test <> ref then
    begin
      writeln('error!');
      halt(1);
    end;
end;

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
  longval : longint;

begin
  writeln('testing str(<value>,widestring)...');

  l := -1;
  str(l,s);
  check_widestr(s,'-1');
  str(l:0,s);
  check_widestr(s,'-1');
  str(l:1,s);
  check_widestr(s,'-1');
  str(l:2,s);
  check_widestr(s,'-1');
  str(l:3,s);
  check_widestr(s,' -1');
  len := 4;
  str(l:len,s);
  check_widestr(s,'  -1');

  c := 10;
  str(c,s);
  check_widestr(s,'10');
  str(c:0,s);
  check_widestr(s,'10');
  str(c:1,s);
  check_widestr(s,'10');
  str(c:2,s);
  check_widestr(s,'10');
  str(c:3,s);
  check_widestr(s,' 10');

  { for more in-depth tests of str_real, see ../tstreal[1,2].pp }
  f := -1.12345;
{$IFOPT E-}
  str(f:22,s);
  if sizeof(extended) = 10 then
    check_widestr(s,'-1.12345000000000E+000')
  else if sizeof(extended) = 8 then
    check_widestr(s,'-1.12345000000000E+000')
  else
    check_widestr(s,'error, not yet implemented!!!!');
{$endif}
  { the number of exponents depends on the mapping of the real type }
  if sizeof(real) = 8 then
    begin
      str(f:0,s);
      check_widestr(s,'-1.1E+000');
      str(f:1,s);
      check_widestr(s,'-1.1E+000');
      str(f:2,s);
      check_widestr(s,'-1.1E+000');
      str(f:3,s);
      check_widestr(s,'-1.1E+000');
      str(f:4,s);
      check_widestr(s,'-1.1E+000');
    end
  else
    begin
      str(f:0,s);
      check_widestr(s,'-1.1E+00');
      str(f:1,s);
      check_widestr(s,'-1.1E+00');
      str(f:2,s);
      check_widestr(s,'-1.1E+00');
      str(f:3,s);
      check_widestr(s,'-1.1E+00');
      str(f:4,s);
      check_widestr(s,'-1.1E+00');
    end;
  str(f:0:0,s);
  check_widestr(s,'-1');
  str(f:0:1,s);
  check_widestr(s,'-1.1');
  str(f:0:2,s);
  check_widestr(s,'-1.12');
  str(f:1:0,s);
  check_widestr(s,'-1');
  str(f:1:1,s);
  check_widestr(s,'-1.1');
  str(f:5:0,s);
  check_widestr(s,'   -1');
  str(f:5:1,s);
  check_widestr(s,' -1.1');
  str(f:5:2,s);
  check_widestr(s,'-1.12');
  len := 6;
  frac := 2;
  str(f:len:frac,s);
  check_widestr(s,' -1.12');

  i := -1;
  str(i,s);
  check_widestr(s,'-1');
  str(i:0,s);
  check_widestr(s,'-1');
  str(i:1,s);
  check_widestr(s,'-1');
  str(i:2,s);
  check_widestr(s,'-1');
  str(i:3,s);
  check_widestr(s,' -1');
  i:=655536;
  str(i,s);
  check_widestr(s,'655536');
  str(i:0,s);
  check_widestr(s,'655536');
  str(i:1,s);
  check_widestr(s,'655536');
  str(i:2,s);
  check_widestr(s,'655536');
  str(i:3,s);
  check_widestr(s,'655536');
  longval:=1;
  i:=int64(longval) shl 33;
  str(i,s);
  check_widestr(s,'8589934592');
  str(i:0,s);
  check_widestr(s,'8589934592');
  str(i:1,s);
  check_widestr(s,'8589934592');
  str(i:2,s);
  check_widestr(s,'8589934592');
  str(i:3,s);
  check_widestr(s,'8589934592');

  q := 10;
  str(q,s);
  check_widestr(s,'10');
  str(q:0,s);
  check_widestr(s,'10');
  str(q:1,s);
  check_widestr(s,'10');
  str(q:2,s);
  check_widestr(s,'10');
  str(q:3,s);
  check_widestr(s,' 10');
  q:=655536;
  str(q,s);
  check_widestr(s,'655536');
  str(q:0,s);
  check_widestr(s,'655536');
  str(q:1,s);
  check_widestr(s,'655536');
  str(q:2,s);
  check_widestr(s,'655536');
  str(q:3,s);
  check_widestr(s,'655536');
  longval:=1;
  q:=qword(longval) shl 33;
  str(q,s);
  check_widestr(s,'8589934592');
  str(q:0,s);
  check_widestr(s,'8589934592');
  str(q:1,s);
  check_widestr(s,'8589934592');
  str(q:2,s);
  check_widestr(s,'8589934592');
  str(q:3,s);
  check_widestr(s,'8589934592');
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
