{ %OPT=-O3 -CriotR }
program tw39713;

{ Internal Error 200203272 would get triggered when align was inlined }

function align(i,a:longint):longint; inline;
{
  return value <i> aligned <a> boundary. <a> must be power of two.
}
  begin
    { One-line formula for i >= 0 is
      >>> (i + a - 1) and not (a - 1),
      and for i < 0 is
      >>> i and not (a - 1). }

    if a>0 then
      a:=a-1; { 'a' is decremented beforehand, this also allows a=0 as a synonym for a=1. }
    if i>=0 then
      i:=i+a;
    align:=i and not a;
  end;
  
procedure IncVar(Input: LongInt; var IncrementVar: LongInt); noinline;
  begin
    Inc(IncrementVar, align(Input,4));
  end;

const
  Expected: array[0..5] of LongInt = (0, 5, 6, 7, 8, 13);
 var
  X, Y, IncrementVar: LongInt;
begin
  IncrementVar := 0;
  
  for X := 0 to 5 do
    begin
      IncrementVar := X;
      IncVar(X, IncrementVar);
      if IncrementVar <> Expected[X] then
        begin
          WriteLn('FAILED on X = ', X, '; expected ', Expected[X], ' got ', Y);
          Halt(1);
        end;
    end;

  Writeln('ok');
end.
