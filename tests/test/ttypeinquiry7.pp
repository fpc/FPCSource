{ "type of (S+T)" inside generics: the operand is parsed when the generic is
  specialized, so it can use the type parameters. }
program ttypeinquiry7;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

type
  generic TSum<S,T> = record
    Sum: type of (S+T);
  end;

generic function Add<S,T>(a: S; b: T): type of (S+T);
begin
  Result:=a+b;
end;

procedure Check(Ok: boolean; Id: longint);
begin
  if not Ok then
    begin
      writeln('failed: ',Id);
      halt(Id);
    end;
end;

var
  x: longint = 1;
  aByte: byte = 0;
  aWord: word = 0;
  aDouble: Double = 0;
  aSingle: Single = 0;
  aByteWord: specialize TSum<byte,word>;
  aDoubleSingle: specialize TSum<Double,Single>;
begin
  x:=specialize Add<smallint,longint>(-1000,-100000);
  Check(x=-101000,1);

  { "type of (S+T)" gives the same type as the expression itself, so byte+word
    is promoted the same way as in any other expression }
  Check(SizeOf(aByteWord.Sum)=SizeOf(aByte+aWord),2);

  Check(SizeOf(aDoubleSingle.Sum)=SizeOf(aDouble+aSingle),3);
  Check(SizeOf(aDoubleSingle.Sum)=8,4);

  writeln('ok');
end.
