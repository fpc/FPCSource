{ %norun }
{$x-}
uses
  variants;

type
  tobj = object
    constructor init;
    destructor done;
  end;
   
 constructor tobj.init;
   begin
   end;

 destructor tobj.done;
   begin
   end;

procedure testcomplexassignments;
var
  s1,s2: shortstring;
  a1,a2: ansistring;
  w1,w2: widestring;
  u1,u2: unicodestring;
  v1,v2: variant;
  arr: array[1..4] of char;
  c: char;
  wc: widechar;
  p: pchar;
  pw: pwidechar;
  darr: array of char;
begin
  s1:=s2;
  a1:=a2;
  w1:=w2;
  u1:=u2;
  v1:=v2;
  s1:=arr;
  a1:=arr;
  w1:=arr;
  u1:=arr;
  arr:=s1;
  arr:=a1;
  arr:=w1;
  arr:=u1;
  s1:=c;
  a1:=c;
  w1:=c;
  u1:=c;
  s1:=wc;
  a1:=wc;
  w1:=wc;
  u1:=wc;
  s1:=p;
  a1:=p;
  w1:=p;
  u1:=p;
  s1:=pw;
  a1:=pw;
  w1:=pw;
  u1:=pw;
  v1:=darr;
end;


procedure testval;
var
  ss: shortstring;
  b: byte;
  w: word;
  c: cardinal;
  q: qword;
  si: shortint;
  i: smallint;
  l: longint;
  ii: int64;
begin
  val(ss,b,w);
  val(ss,c,b);
{$ifdef cpu64}
  val(ss,q,ii);
{$endif}
  val(ss,q,si);
  val(ss,si,i);
  val(ss,i,l);
{$ifdef cpu64}
  val(ss,l,ii);
{$endif}
  val(ss,ii,l);
end;

var
  o: tobj;
  po: ^tobj;
begin
  o.init;
  new(po,init);
  dispose(po,done);
end.
