{ %OPT=-O3 }
{ tests cse of expressions with swapped operands of commutative operators }
{$mode objfpc}
type
  tset = set of 0..31;
  prec = ^trec;
  trec = record
    x : longint;
  end;

function fint(a,b,c,d : longint) : longint; noinline;
begin
  result:=(a*b+c)*(b*a+d)+(a+b)*(b+a)+ord(a=b)+ord(b=a)+(a xor b)-(b xor a);
end;


function fint64(a,b : int64) : int64; noinline;
begin
  result:=(a*b)-(b*a)+(a and b)+(b and a)+(a or b)-(b or a);
end;


function fdouble(a,b : double) : double; noinline;
begin
  result:=(a*b+1.0)*(b*a+2.0)+(a+b)/(b+a);
end;


function fset(a,b : tset) : tset; noinline;
begin
  result:=((a+b)*(b+a))+((a*b)-(b*a))+((a><b)*(b><a));
end;


function fbool(a,b : boolean) : boolean; noinline;
begin
{$push}
{$B+}
  result:=(a and b) or (b and a);
{$pop}
end;


{ the operands of short boolean evaluated expressions must not be swapped }
function fshortbool(p : prec) : boolean; noinline;
begin
  result:=((p<>nil) and (p^.x>0)) or ((p<>nil) and (p^.x<-10));
end;


function fstring(const a,b : shortstring) : boolean; noinline;
begin
  { string concatenation is not commutative }
  result:=(a+b)=(b+a);
end;


var
  r : trec;
begin
  if fint(3,5,7,11)<>(22*26+64) then
    halt(1);
  if fint(4,4,0,0)<>(16*16+64+2) then
    halt(2);
  if fint64(6,10)<>(2+2+14-14) then
    halt(3);
  if fdouble(2.0,3.0)<>(7.0*8.0+1.0) then
    halt(4);
  if fset([1,2,3],[3,4])<>[1,2,3,4] then
    halt(5);
  if fset([1,2],[3,4])<>[1,2,3,4] then
    halt(6);
  if not(fbool(true,true)) or fbool(true,false) or fbool(false,true) then
    halt(7);
  if fshortbool(nil) then
    halt(8);
  r.x:=1;
  if not(fshortbool(@r)) then
    halt(9);
  r.x:=-20;
  if not(fshortbool(@r)) then
    halt(10);
  r.x:=-5;
  if fshortbool(@r) then
    halt(11);
  if fstring('ab','cd') then
    halt(12);
  if not(fstring('ab','ab')) then
    halt(13);
  writeln('ok');
end.
