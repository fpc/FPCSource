{ %OPT=-O2 }
{ a zero-extended byte or word that goes negative through a subtraction and is
  then compared unsigned: the narrowed comparison must give the result of the
  full-size one for every value }
program tmovzxcmp2;

{$mode objfpc}{$R-}{$Q-}

var
  gErrors: longint;

function IsLower(c: char): boolean; noinline;
  begin
    result:=c in ['a'..'z'];
  end;

function IsDigit(c: char): boolean; noinline;
  begin
    result:=c in ['0'..'9'];
  end;

function LowerBit(c: char): longint; noinline;
  begin
    result:=ord(c in ['a'..'z']) shl 5;
  end;

function BitSix(b: byte): boolean; noinline;
  begin
    result:=((b shr 6) and 1)<>0;
  end;

function Is64(b: byte): boolean; noinline;
  begin
    result:=(b xor 64)=0;
  end;

function Below200(b: byte): boolean; noinline;
  begin
    result:=longword(longint(b)-128)<200;
  end;

function Below155(b: byte): boolean; noinline;
  begin
    result:=longword(longint(b)-100)<155;
  end;

function Below156(b: byte): boolean; noinline;
  begin
    result:=longword(longint(b)-100)<156;
  end;

function AtMost155(b: byte): boolean; noinline;
  begin
    result:=longword(longint(b)-100)<=155;
  end;

function Is155(b: byte): boolean; noinline;
  begin
    result:=longint(b)-100=155;
  end;

function TwoSteps(b: byte): boolean; noinline;
  begin
    result:=longword(longint(b)-50-50)<10;
  end;

function Masked(b: byte): boolean; noinline;
  begin
    result:=longword((longint(b)-97) and $1FF)<26;
  end;

function WordBelow5000(w: word): boolean; noinline;
  begin
    result:=longword(longint(w)-1000)<5000;
  end;

function WordBelow35536(w: word): boolean; noinline;
  begin
    result:=longword(longint(w)-30000)<35536;
  end;

procedure Check(const aName: string; aArg: longint; aGot, aExpected: boolean);
  begin
    if aGot<>aExpected then
      begin
        writeln('error: ',aName,'(',aArg,') = ',aGot,', expected ',aExpected);
        inc(gErrors);
      end;
  end;

function UnsignedBelow(aValue: int64; aLimit: int64): boolean;
  begin
    result:=(aValue>=0) and (aValue<aLimit);
  end;

var
  i: longint;
  v: int64;
begin
  gErrors:=0;
  for i:=0 to 255 do
    begin
      v:=i;
      Check('IsLower',i,IsLower(chr(i)),UnsignedBelow(v-97,26));
      Check('IsDigit',i,IsDigit(chr(i)),UnsignedBelow(v-48,10));
      Check('LowerBit',i,LowerBit(chr(i))=32,UnsignedBelow(v-97,26));
      Check('BitSix',i,BitSix(i),(v and 64)<>0);
      Check('Is64',i,Is64(i),v=64);
      Check('Below200',i,Below200(i),UnsignedBelow(v-128,200));
      Check('Below155',i,Below155(i),UnsignedBelow(v-100,155));
      Check('Below156',i,Below156(i),UnsignedBelow(v-100,156));
      Check('AtMost155',i,AtMost155(i),UnsignedBelow(v-100,156));
      Check('Is155',i,Is155(i),v-100=155);
      Check('TwoSteps',i,TwoSteps(i),UnsignedBelow(v-100,10));
      Check('Masked',i,Masked(i),((v-97) and $1FF)<26);
    end;
  for i:=0 to 65535 do
    begin
      v:=i;
      Check('WordBelow5000',i,WordBelow5000(i),UnsignedBelow(v-1000,5000));
      Check('WordBelow35536',i,WordBelow35536(i),UnsignedBelow(v-30000,35536));
    end;
  if gErrors<>0 then
    halt(1);
  writeln('ok');
end.
