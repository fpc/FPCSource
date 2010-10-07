function TailRecFibonacci(const n: Byte): QWord;

  function InnerFibo(const n: Byte; const r1,r2: QWord): QWord; inline;
  begin
    case n of
      0: InnerFibo := r1;
      1: InnerFibo := r2;
      else InnerFibo := InnerFibo(n - 1,r2,r1 + r2);
    end;
  end;

begin
  TailRecFibonacci := InnerFibo(n,0,1);
end;

begin
  if TailRecFibonacci(10)<>55 then
    halt(1);
end.
