{ Old file: tbs0091.pp }
{ missing standard functions in constant expressions    OK 0.99.7 (PFV) }

{ Page 22 of The Language Guide of Turbo Pascal }
var
 t: byte;
const
  a = Trunc(1.3);
  b = Round(1.6);
  c = abs(-5);
  ErrStr = 'Hello!';
  d = Length(ErrStr);
  e = Lo($1234);
  f = Hi($1234);
  g = Chr(34);
  h = Odd(1);
  i = Ord('3');
  j = Pred(34);
  l = Sizeof(t);
  m = Succ(9);
  n = Swap($1234);
  o = ptr(0,0);
Begin
end.
