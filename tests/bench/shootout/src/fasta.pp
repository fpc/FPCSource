{ The Computer Language Shootout
  http://shootout.alioth.debian.org

  contributed by Ian Osgood
  modified by Vincent Snijders
}
{$mode objfpc}{$inline on}{$I-}

program fasta;

uses Math;

const ALU : AnsiString =
  'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG' +
  'GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA' +
  'CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT' +
  'ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA' +
  'GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG' +
  'AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC' +
  'AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA';

const codes = 'acgtBDHKMNRSVWY';

const IUB : array[0..14] of double = ( 0.27, 0.12, 0.12, 0.27,
  0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02 );

const HomoSap : array[0..3] of double = (
  0.3029549426680, 0.1979883004921, 0.1975473066391,  0.3015094502008 );

const LineLen = 60;

type
  TGene=record
    prob: double;
    code: char;
    dummy: array[1..7] of char;
  end;
  PGene = ^TGene;

var
  n : longint;
  Genes: array of TGene;
  TextBuf: array[0..$FFF] of byte;

procedure fastaRepeat(n : integer);
var
  sourceALU: ansistring;
  line, wrapALU : pchar;
  nulled : char;
  lenALU : integer;
begin
  sourceALU := ALU + copy(ALU, 1, LineLen);
  line := PChar(sourceALU);
  lenALU := length(ALU);
  wrapALU := @sourceALU[lenALU];
  repeat
    nulled := line[LineLen];
    line[LineLen] := #0;
    writeln(line);
    inc(line, LineLen);
    line^ := nulled;
    if line>wrapALU then
      dec(line, lenALU);
    n := n - LineLen;
  until n <= LineLen;
  line[n] := #0;
  writeln(line);
end;

function genRandom(limit : integer): double;
const
  seed : integer = 42;
  IM = 139968;
  IA = 3877;
  IC = 29573;
begin
  seed := (seed * IA + IC) mod IM;
  genRandom := limit * seed * (1 / IM);
end;

procedure InitGenes(const probs: array of double);
var
  i : integer;
  SumProb: double;
begin
  SetLength(Genes, length(probs));
  SumProb := 0;
  for i := low(probs) to high(probs) do begin
    SumProb := SumProb + probs[i];
    Genes[i].prob := SumProb;
    Genes[i].code := codes[i-low(probs)+1];
  end;

end;

procedure fastaRandom(n : integer; const probs: array of double);
var
  line : string;
  p : pchar;

  function chooseCode : char; inline;
  var r : double;
      Gene: PGene;
  begin
    r := genRandom(1);

    Gene := @Genes[low(Genes)];
    while (r>=Gene^.prob) do
      inc(Gene);
   result := Gene^.Code;
  end;

begin
  { make gene array}
  InitGenes(probs);

  SetLength(line,lineLen);
  while n > lineLen do
  begin
    p := @line[1];
    while (p<=@line[lineLen]) do begin
      p^ := chooseCode;
      inc(p);
    end;
    writeln(line);
    n := n - lineLen;
  end;

  SetLength(line,n);
  p := @line[1];
  while (p<=@line[n]) do begin
    p^ := chooseCode;
    inc(p);
  end;
  writeln(line);
end;

begin
  SetTextBuf(output, TextBuf, sizeof(TextBuf));
  val(paramstr(1), n);

  writeln('>ONE Homo sapiens alu');
  fastaRepeat(n*2);

  writeln('>TWO IUB ambiguity codes');
  fastaRandom(n*3, IUB);

  writeln('>THREE Homo sapiens frequency');
  fastaRandom(n*5, HomoSap);
end.

