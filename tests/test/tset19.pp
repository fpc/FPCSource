{ in with left operands whose type is larger than the set range or signed:
  on 8 bit CPUs the left operand and the set elements must not be truncated }
program tset19;

type
  tbig = set of 1000..1100;

procedure error(nr: longint);
  begin
    writeln('error ',nr);
    halt(1);
  end;

var
  s: set of byte;
  sb: tbig;
  w: word;
  b: byte;
  si: shortint;
  l: longint;

begin
  s:=[1,3,255];
  sb:=[1000,1018,1050,1100];

  { word left operand, constant set of small values }
  w:=257;
  if w in [1,3] then
    error(1);
  w:=3;
  if not(w in [1,3]) then
    error(2);

  { word left operand, variable set of byte }
  w:=257;
  if w in s then
    error(3);
  w:=511;
  if w in s then
    error(4);
  w:=255;
  if not(w in s) then
    error(5);

  { signed left operand }
  si:=-1;
  if si in s then
    error(6);
  if si in [1,3,255] then
    error(7);
  si:=3;
  if not(si in s) then
    error(8);

  { longint left operand }
  l:=65536+3;
  if l in s then
    error(9);
  if l in [1,3] then
    error(10);
  l:=-253;
  if l in s then
    error(11);

  { set with elements >255 and a set base <>0 }
  w:=1050+256;
  if w in sb then
    error(20);
  w:=1050-256;
  if w in sb then
    error(21);
  w:=1050;
  if not(w in sb) then
    error(22);
  w:=1050+256;
  if w in [1000,1050,1100] then
    error(23);
  w:=1050;
  if not(w in [1000,1050,1100]) then
    error(24);

  { byte left operand: never in a set of 1000..1100 }
  b:=250;
  if b in sb then
    error(30);
  b:=26;
  if b in sb then
    error(31);
  if b in [1000,1050] then
    error(32);

  writeln('ok');
end.
