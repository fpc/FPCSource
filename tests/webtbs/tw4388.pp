{ %opt=-O2r }
{ %cpu=i386 }
{$mode delphi}

type
  TBigInt=record
    Digits : PInt64;
  end;
  PBigInt=^TBigInt;
var
  a : PBigInt;
  t : int64;
  m : longint;
begin
  new(a);
  with a^ do
    begin
      new(Digits);
      m:=10;
      asm
          mov   eax, m
          mul   eax
          mov   t[0], eax
          mov   t[4], edx
      end ['EAX','EDX'];
      Digits^:=t;
      writeln(Digits^);
      if Digits^<>100 then
        halt(1);
    end;
end.
