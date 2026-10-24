{ CompareByte/Word/DWord: the first difference decides, later differences in the same word are ignored }

{$mode objfpc}

const
  cMaxLen = 72;

var
  gA, gB: array[0..4*cMaxLen+15] of byte;


procedure Fail(const aWhat: string; aOfsA, aOfsB, aLen, aPos: longint; aCode: longint);

begin
  writeln(aWhat,' wrong for ofsA=',aOfsA,' ofsB=',aOfsB,' len=',aLen,' pos=',aPos);
  halt(aCode);
end;


procedure Prepare(aSize, aOfsB, aLen, aPos: longint);

var
  i: longint;

begin
  fillchar(gA,sizeof(gA),$55);
  fillchar(gB,sizeof(gB),$55);
  { element aPos of B is higher, every later element is lower }
  gB[aOfsB+aPos*aSize+aSize-1]:=$65;
  gB[aOfsB+aPos*aSize]:=$65;
  for i:=(aPos+1)*aSize to aLen*aSize-1 do
    gB[aOfsB+i]:=$45;
end;


procedure TestCompare;

var
  lOfsA, lOfsB, lLen, lPos: longint;

begin
  for lOfsA:=0 to 7 do
    for lOfsB:=0 to 7 do
      for lLen:=1 to cMaxLen do
        for lPos:=0 to lLen-1 do
          begin
            Prepare(1,lOfsB,lLen,lPos);
            if CompareByte(gA[lOfsA],gB[lOfsB],lLen)>=0 then
              Fail('CompareByte',lOfsA,lOfsB,lLen,lPos,1);
            if CompareByte(gB[lOfsB],gA[lOfsA],lLen)<=0 then
              Fail('CompareByte reversed',lOfsA,lOfsB,lLen,lPos,2);
            Prepare(2,lOfsB,lLen,lPos);
            if CompareWord(gA[lOfsA],gB[lOfsB],lLen)>=0 then
              Fail('CompareWord',lOfsA,lOfsB,lLen,lPos,3);
            if CompareWord(gB[lOfsB],gA[lOfsA],lLen)<=0 then
              Fail('CompareWord reversed',lOfsA,lOfsB,lLen,lPos,4);
            Prepare(4,lOfsB,lLen,lPos);
            if CompareDWord(gA[lOfsA],gB[lOfsB],lLen)>=0 then
              Fail('CompareDWord',lOfsA,lOfsB,lLen,lPos,5);
            if CompareDWord(gB[lOfsB],gA[lOfsA],lLen)<=0 then
              Fail('CompareDWord reversed',lOfsA,lOfsB,lLen,lPos,6);
          end;
end;


begin
  TestCompare;
  writeln('ok');
end.
