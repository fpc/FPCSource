{$i+}

{ Widestring is not supported in 1.0.x }
{$ifndef VER1_0}
  {$define HASWIDESTR}
{$endif VER1_0}

function getint64_1 : int64;
var
 value : longint;
begin
 value:=1;
 getint64_1 := int64(value) shl 40;
end;

function getint64_2 : int64;
var
 value : longint;
begin
 value:=65535;
 getint64_2 := value;
end;

procedure test_rwtext;
var
  t: text;
  s: shortstring;
  a: ansistring;
{$ifdef HASWIDESTR}
  wc : widechar;
  w : widestring;
{$endif HASWIDESTR}
  l: longint;
  card: cardinal;
  b: byte; bool: boolean;
  c: char;
  arr: array[1..10] of char;
  p: pchar;
  r: real;
  vl : int64;
  vl1 : int64;
  tmplong : longint;
begin
  bool := true;
  writeln('ShortString const test');
  writeln('ShortString const test with const len':70);
  b := 60;
  writeln('ShortString const test with var len':b);
  s := 'ShortStr var test';
  writeln(s);
  s := s+ ' with const len';
  writeln(s:40);
  s := 'ShortStr var test with var len';
  writeln(s:b);

  l := -1; c := 'y'; card := 18; r := 5.1234;
  writeln('A combo test: ',b,' ',l,' ',c,' ',card,' ',bool:10);
  writeln('floats: ',r,' ',r:1,' ',r:8,' ',r:10:2);
  arr := 'arrofchars';
  writeln('array of char: ',arr:38);

  arr[10] := #0;
  p := @arr;
  writeln('pchar test: ',p);

  a := 'this is an ansistring';
  writeln(a);

  vl:=getint64_1;
  vl1:=getint64_2;
  writeln('int64 test : ',vl, ' ',vl1);

{$ifdef HASWIDESTR}
  wc := 'y';
  writeln('widechar: ',wc);
  w := 'this is a widestring';
  writeln(w);
{$endif HASWIDESTR}

  write('no new line now...',l,c,b);

  write;
  read;

  assign(t,'treadwrt.txt');
  rewrite(t);
  writeln('testing text file functionality...');
  writeln(t,'this is a string');
  writeln(t,l);
  writeln(t,c);
  writeln(t,b);
  writeln(t,vl);
  writeln(t,vl1);
  l := 0;
  c := #32;
  b := 5;
  vl:=1;
  vl1 := 2;
  close(t);
  reset(t);
  readln(t,s);
  if s <> 'this is a string' then
    halt(1);
  readln(t,l);
  if l <> -1 then
    halt(1);
  readln(t,c);
  if c <> 'y' then
    halt(1);
  readln(t,b);
  if b <> 60 then
    halt(1);
  { 64-bit read testing }
  readln(t,vl);
  if vl <> getint64_1 then
    halt(1);
  readln(t,vl1);
  if vl1 <> getint64_2 then
    halt(1);
  close(t);
  erase(t);
  writeln('write/read text passed...');
end;

procedure test_rwtyped;
var
  f: file of cardinal;
  c: cardinal;
begin
  assign(f,'treadwrt.dat');
  rewrite(f);
  c := 8;
  write(f,c);
  write(f,cardinal(10));
  close(f);
  reset(f);
  read(f,c);
  if c <> 8 then
    halt(1);
  read(f,c);
  if c <> 10 then
    halt(1);
  close(f);
  erase(f);
  writeln('write/read typed passed...');
end;

begin
  test_rwtext;
  test_rwtyped;
end.
