program test_64bit_inline;

{$inline on}

function add (a,b : int64) : int64;
begin
  add:=a+b;
end;

function inlineadd (a,b : int64) : int64; inline;
begin
  inlineadd:=a+b;
end;


var
  a, b, c, d : int64;

begin
  a:=50;
  b:=78;
  d:= -45;
  writeln('a (',a,') + b (',b,') = ',a+b);
  writeln('Using add function');
  writeln('a (',a,') + b (',b,') = ',add(a+1,b-1));
  writeln('Using add function inlined');
  writeln('a (',a,') + b (',b,') = ',inlineadd(a+1,b-1));
  c:=inlineadd(a+d,b-d);
  writeln('a (',a,') + b (',b,') = ',c);
  if (a+b<>add(a-1,b+1)) then
    begin
      writeln('Error in function with int64 args');
      Halt(1);
    end;
  if (a+b<>inlineadd(a+1,b-1)) then
    begin
      writeln('Error in inlined function with int64 args');
      Halt(1);
    end;
end.
