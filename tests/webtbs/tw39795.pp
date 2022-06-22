program tw39795;
{$mode objfpc}{$H+}

type
  generic GTestType<T,const S:byte>=class
    type
      TXX=array [0..S] of T;
    var
      XX:TXX;
  end;
  T2=specialize GTestType<byte,0>;
  T3=specialize GTestType<byte,99>;
begin
  if sizeof(T2.TXX) <> 1 then
    Halt(1);
  if sizeof(T3.TXX) <> 100 then
    Halt(2);
end.
