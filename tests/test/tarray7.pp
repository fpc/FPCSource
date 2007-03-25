program array7;

{This test checks for open array functionality.}

function average(const row:array of integer):real;

var i:longint;
    temp:real;

begin
  temp:=Row[0];
  for i:=1 to high(row) do
    temp:=temp+row[i];
  average:=temp/(high(row)+1);
end;

procedure uppercase(var u:array of char);

var i:longint;

begin
  for i:=low(u) to high(u) do
    u[i]:=upcase(u[i]);
end;

var a:array[-1000..1000] of integer;
    b:Pinteger;
    c:array of integer;
    d:array[1..10] of char;
    e:Pchar;
    f:string;
    g:ansistring;
    i:longint;
    s:string[31];

begin
  {Integer stuff.}

  {First try it with a static array.}
  for i:=low(a) to high(a) do
    a[i]:=i xor 99;
  str(average(a):4:3,s);
  if s<>'-0.046' then
    halt(1);
  str(average(a[-1000..0]):4:3,s);
  if s<>'-502.203' then
    halt(2);
  str(average(a[0..1000]):4:3,s);
  if s<>'502.209' then
    halt(3);

  {Now try it with a heap block.}
  getmem(b,2001*sizeof(integer));
  for i:=-1000 to 1000 do
    b[i+1000]:=i xor 99;
  str(average(b[0..2000]):4:3,s);
  if s<>'-0.046' then
    halt(4);
  dispose(b);

  {And now try it with a dynamic array.}
  setlength(c,2001);
  for i:=-1000 to 1000 do
    c[i+1000]:=i xor 99;
  str(average(c):4:3,s);
  if s<>'-0.046' then
    halt(5);
  str(average(c[0..1000]):4:3,s);
  if s<>'-502.203' then
    halt(6);
  str(average(c[1000..2000]):4:3,s);
  if s<>'502.209' then
    halt(7);
  setlength(c,0);

  {Character stuff.}

  {First with a static array.}
  d:='abcdefghij';
  uppercase(d);
  if d<>'ABCDEFGHIJ' then
    halt(8);

  {Now with a heap block.}
  getmem(e,10);
  move(d,e^,10);
  uppercase(e[0..9]);
  move(e^,d,10);
  if d<>'ABCDEFGHIJ' then
    halt(9);
  dispose(e);

  {Then a shortstring.}
  f:='abcdefghij';
  uppercase(f[1..10]);
  if f<>'ABCDEFGHIJ' then
    halt(10);

  {And finish with an ansistring.}
  g:='abcdefghij';
  uppercase(g[1..10]);
  if g<>'ABCDEFGHIJ' then
    halt(11);
end.
