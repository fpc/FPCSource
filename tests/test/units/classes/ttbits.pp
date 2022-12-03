program ttbits;

{$MODE objfpc}{$H+} {$coperators on} {$typedaddress on} {$modeswitch advancedrecords} {$modeswitch duplicatelocals}

uses
  Classes, SysUtils, Math;

var
  somethingFailed: boolean = false;

const
  Bool01: array[boolean] of char = ('0', '1');

procedure Fail(const msg: string = 'Err!');
begin
  Writeln(msg, LineEnding);
  somethingFailed := true;
end;

procedure FillWithRandom(b: TBits);
var
  I: Integer;
begin
  for I := 0 to b.Size - 1 do
    b[I] := Random(2) <> 0;
end;

procedure TestCopyBits;
const
  NumTests = 100;
  MaxBits = 200;
var
  b1: TBits = nil;
  b2: TBits = nil;
  I: Integer;
begin
  try
    b1 := TBits.Create;
    b2 := TBits.Create;
    for I := 1 to NumTests do
    begin
      b1.Size := Random(MaxBits);
      FillWithRandom(b1);
      b2.CopyBits(b1);
      if not b1.Equals(b2) then
        Fail;
      if not b2.Equals(b1) then
        Fail;
    end;
  finally
    b1.Free;
    b2.Free;
  end;
end;

function CreateBits(const src: string): TBits;
var
  i: SizeInt;
begin
  result := TBits.Create(length(src));
  for i := 1 to length(src) do
    result[i - 1] := src[i] = '1';
end;

function Dump(b: TBits): string;
var
  i: SizeInt;
begin
  SetLength((@result)^, b.Size);
  for i := 0 to b.Size - 1 do
    result[1 + i] := Bool01[b[i]];
end;

type
  BinOpEnum = (op_And, op_Or, op_Xor, op_Not);

function ReferenceBinOpResult(const aSrc, bSrc: string; op: BinOpEnum): string;
var
  i: SizeInt;
  va, vb, vr: boolean;
begin
  case op of
    op_Or, op_Xor: SetLength((@result)^, max(length(aSrc), length(bSrc)));
    op_And, op_Not: SetLength(result, length(aSrc));
  end;

  for i := 1 to length(result) do
  begin
    va := (i <= length(aSrc)) and (aSrc[i] = '1');
    vb := (i <= length(bSrc)) and (bSrc[i] = '1');
    case op of
      op_And: vr := va and vb;
      op_Or: vr := va or vb;
      op_Xor, op_Not: vr := va xor vb;
    end;
    result[i] := Bool01[vr];
  end;
end;

procedure TestBinOp(const aSrc, bSrc: string; op: BinOpEnum; const expect: string);
var
  a, b: TBits;
  msg: string;
begin
  a := nil; b := nil;
  try
    a := CreateBits(aSrc);
    b := CreateBits(bSrc);

    case op of
      op_And: a.AndBits(b);
      op_Or: a.OrBits(b);
      op_Xor: a.XorBits(b);
      op_Not: a.NotBits(b);
    end;

    if Dump(a) <> expect then
    begin
      WriteStr(msg,
        op, ' failed:', LineEnding,
        'a        = ', aSrc, LineEnding,
        'b        = ', bSrc, LineEnding,
        'expected = ', expect, LineEnding,
        'got      = ', Dump(a));
      Fail(msg);
    end;
  finally
    a.Free; b.Free;
  end;
end;

procedure TestBinOps;
const
  Srcs: array[0 .. 11] of string =
  (
    '', '0', '1', '10101',
    // 1 zero, 1 one, 2 zeros, 2 ones, ..., 11 zeros, 11 ones
    '010011000111000011110000011111000000111111000000011111110000000011111111000000000111111111000000000011111111110000000000011111111111',
    // 1 one, 1 zero, 2 ones, 2 zeros, ..., 11 ones, 11 zeros
    '101100111000111100001111100000111111000000111111100000001111111100000000111111111000000000111111111100000000001111111111100000000000',
    // 11 zeros, 11 ones, 10 zeros, 10 ones, ..., 1 zero, 1 one
    '000000000001111111111100000000001111111111000000000111111111000000001111111100000001111111000000111111000001111100001111000111001101',
    // 11 ones, 11 zeros, 10 ones, 10 zeros, ..., 1 one, 1 zero
    '111111111110000000000011111111110000000000111111111000000000111111110000000011111110000000111111000000111110000011110000111000110010',

    // 1 zero, 1 one, 2 zeros, 2 ones, ..., 23 zeros, 23 ones
    '010011000111000011110000011111000000111111000000011111110000000011111111000000000111111111000000000011111111110000000000011111111111' +
    '000000000000111111111111000000000000011111111111110000000000000011111111111111000000000000000111111111111111000000000000000011111111' +
    '111111110000000000000000011111111111111111000000000000000000111111111111111111000000000000000000011111111111111111110000000000000000' +
    '000011111111111111111111000000000000000000000111111111111111111111000000000000000000000011111111111111111111110000000000000000000000' +
    '011111111111111111111111',

    // 1 one, 1 zero, 2 ones, 2 zeros, ..., 23 ones, 23 zeros
    '101100111000111100001111100000111111000000111111100000001111111100000000111111111000000000111111111100000000001111111111100000000000' +
    '111111111111000000000000111111111111100000000000001111111111111100000000000000111111111111111000000000000000111111111111111100000000' +
    '000000001111111111111111100000000000000000111111111111111111000000000000000000111111111111111111100000000000000000001111111111111111' +
    '111100000000000000000000111111111111111111111000000000000000000000111111111111111111111100000000000000000000001111111111111111111111' +
    '100000000000000000000000',

    // 23 zeros, 23 ones, 22 zeros, 22 ones, ..., 1 zero, 1 one
    '000000000000000000000001111111111111111111111100000000000000000000001111111111111111111111000000000000000000000111111111111111111111' +
    '000000000000000000001111111111111111111100000000000000000001111111111111111111000000000000000000111111111111111111000000000000000001' +
    '111111111111111100000000000000001111111111111111000000000000000111111111111111000000000000001111111111111100000000000001111111111111' +
    '000000000000111111111111000000000001111111111100000000001111111111000000000111111111000000001111111100000001111111000000111111000001' +
    '111100001111000111001101',

    // 23 ones, 23 zeros, 22 ones, 22 zeros, ..., 1 one, 1 zero
    '111111111111111111111110000000000000000000000011111111111111111111110000000000000000000000111111111111111111111000000000000000000000' +
    '111111111111111111110000000000000000000011111111111111111110000000000000000000111111111111111111000000000000000000111111111111111110' +
    '000000000000000011111111111111110000000000000000111111111111111000000000000000111111111111110000000000000011111111111110000000000000' +
    '111111111111000000000000111111111110000000000011111111110000000000111111111000000000111111110000000011111110000000111111000000111110' +
    '000011110000111000110010'
  );
var
  op: BinOpEnum;
  iA, iB: SizeInt;
begin
  TestBinOp('1011', '111001', op_Xor, '010101');
  for iA := 0 to High(Srcs) do
    for iB := 0 to High(Srcs) do
      for op in BinOpEnum do
        TestBinOp(Srcs[iA], Srcs[iB], op, ReferenceBinOpResult(Srcs[iA], Srcs[iB], op));
end;

procedure TestFinds(const src: string; state: boolean; const positions: array of int16);
type
  DirectionEnum = (LeftToRight, RightToLeft);
var
  b: TBits;
  direction: DirectionEnum;
  found, iPos, expected, foundRO: SizeInt;
  msg: string;
begin
  b := nil;
  try
    b := CreateBits(src);
    for direction in DirectionEnum do
    begin
      case direction of
        LeftToRight:
          begin
            found := b.FindFirstBit(state);
            foundRO := b.Find(state, 0);
            iPos := 0;
          end;
        RightToLeft:
          begin
            // Emulate non-existing (for now) FindFirstBitRev that searches from the end.
            b.FindFirstBit(state);
            if b.Size = 0 then
              found := -1
            else
            begin
              b.SetIndex(b.Size - 1);
              if b[b.Size - 1] = state then found := b.Size - 1 else found := b.FindPrevBit;
            end;
            foundRO := b.FindRev(state, b.Size - 1);
            iPos := High(positions);
          end;
      end;

      repeat
        if (iPos >= 0) and (iPos < length(positions)) then expected := positions[iPos] else expected := -1;
        if found <> expected then
        begin
          WriteStr(msg, 'FindFirst/NextBit failed:' + LineEnding +
            'src = ' + src + LineEnding +
            'state = ', Bool01[state], ', dir = ', direction, ', iPos = ', iPos, ', found = ', found, ', expected = ', expected);
          Fail(msg);
          break;
        end;
        if foundRO <> expected then
        begin
          WriteStr(msg, 'Find0/1 failed:' + LineEnding +
            'src = ' + src + LineEnding +
            'state = ', Bool01[state], ', dir = ', direction, ', iPos = ', iPos, ', foundRO = ', foundRO, ', expected = ', expected);
          Fail(msg);
          break;
        end;
        if expected < 0 then break;
        case direction of
          LeftToRight:
            begin
              iPos += 1;
              found := b.FindNextBit;
              foundRO := b.Find(state, foundRO + 1);
            end;
          RightToLeft:
            begin
              iPos -= 1;
              found := b.FindPrevBit;
              foundRO := b.FindRev(state, foundRO - 1);
            end;
        end;
      until false;
    end;
  finally
    b.Free;
  end;
end;

procedure TestFinds;
var
  state: boolean;
  c0, c1: char;
  positions: array of int16;
  i, iPos: SizeInt;
  iRandomTest: int32;
  r: string;
begin
  for state in boolean do
  begin
    c0 := Bool01[not state]; c1 := Bool01[state];
    TestFinds('', state, []);
    TestFinds(
      StringOfChar(c0, 30) + c1 + StringOfChar(c0, 39) + c1 + StringOfChar(c0, 49) + c1 + StringOfChar(c0, 59) + c1,
      state,
      [30, 70, 120, 180]);

    SetLength((@positions)^, 499);
    for i := 0 to High(positions) do positions[i] := i + ord(i >= 250);
    TestFinds(StringOfChar(c1, 250) + c0 + StringOfChar(c1, 249), state, positions);

    TestFinds(c1 + StringOfChar(c0, 254) + c1, state, [0, 255]);
    TestFinds(c1 + StringOfChar(c0, 255) + c1, state, [0, 256]);

    SetLength(positions, 150);
    for iRandomTest := 1 to 1000 do
    begin
      SetLength((@r)^, random(length(positions)));
      iPos := 0;
      for i := 0 to length(r) - 1 do
        if random(2) = 0 then
          pChar(pointer(r))[i] := c0
        else
        begin
          pChar(pointer(r))[i] := c1;
          positions[iPos] := i;
          iPos += 1;
        end;
      TestFinds(r, state, Slice(positions, iPos));
    end;
  end;
end;

procedure TestZeroUpper;
var
  b, b2: TBits;
  expected: string;
begin
  b := nil; b2 := nil;
  try
    expected := '10111001011';
    b := CreateBits(expected);

    b.Size := b.Size - 2;
    SetLength(expected, length(expected) - 2);
    if Dump(b) <> expected then
      Fail(
        'ZeroUpper failed after truncation:' + LineEnding +
        'expected = ' + expected + LineEnding +
        'got      = ' + Dump(b));

    b.Size := b.Size + 3;
    expected += '000';
    if Dump(b) <> expected then
      Fail(
        'ZeroUpper failed after widening:' + LineEnding +
        'expected = ' + expected + LineEnding +
        'got      = ' + Dump(b));

    FreeAndNil(b);
    b := CreateBits('101');
    b2 := CreateBits('11111');
    b.NotBits(b2);
    b.Size := 6;
    expected := '010000';
    if Dump(b) <> expected then
      Fail(
        'ZeroUpper / NotBits failed:' + LineEnding +
        'expected = ' + expected + LineEnding +
        'got      = ' + Dump(b));
  finally
    b.Free; b2.Free;
  end;
end;

type
  BitfieldRangeTest = record
    procedure PerformOnSrc;
    procedure PrepareScratch;
    function ExpectMatchesResb: boolean;
    function SrcBitfieldPiece: string;
    function ResbAndExpectedPiece: string;
    class function BytesToString(b: pByte; n: SizeUint): string; static;
    class function BitsToString(b: TBits; n: SizeUint): string; static;

  const
    BaseBits = bitsizeof(TBitsBase);
    Size = 8 * BaseBits;

    PositionBases: array[0 .. 3] of uint16 =
    (
      BaseBits, BaseBits + BaseBits div 2, Size div 2, Size div 2 + BaseBits div 2
    );

    CountBases: array[0 .. 3] of uint16 =
    (
      0, BaseBits, 4 * BaseBits, 6 * BaseBits
    );

    Fuzz: array[0 .. 4] of int8 =
    (
      -5, -2, 0, +2, +5
    );

  var
    src, expect: array[0 .. Size - 1] of byte;
    srcb, resb: TBits;
  end;

procedure BitfieldRangeTest.PerformOnSrc;
var
  srcBase, srcFuzz, dstBase, dstFuzz, countBase, countFuzz, src, dst, count: SizeInt;
begin
  for srcBase in PositionBases do
    for srcFuzz in Fuzz do
    begin
      src := srcBase + srcFuzz;
      if not ((src >= 0) and (src <= Size)) then continue;

      for dstBase in PositionBases do
        for dstFuzz in Fuzz do
        begin
          dst := dstBase + dstFuzz;
          if not ((dst >= 0) and (dst <= Size)) then continue;

          for countBase in CountBases do
            for countFuzz in Fuzz do
            begin
              count := countBase + countFuzz;
              if not ((count >= 0) and (count <= Size - max(src, dst))) then continue;

              PrepareScratch;
              Move(pByte(expect)[src], pByte(expect)[dst], count * sizeof(byte));
              resb.MoveRange(src, dst, count);
              if not ExpectMatchesResb then
                Fail(SrcBitfieldPiece + LineEnding +
                  'MoveRange(' + src.ToString + ', ' + dst.ToString + ', ' + count.ToString + ') = ' + LineEnding +
                  ResbAndExpectedPiece + '.');
            end;
        end;
    end;
end;

procedure BitfieldRangeTest.PrepareScratch;
begin
  Move(src, expect, sizeof(src));
  resb.CopyBits(srcb);
end;

function BitfieldRangeTest.ExpectMatchesResb: boolean;
var
  i: SizeInt;
begin
  for i := 0 to Size - 1 do
    if expect[i] <> ord(resb[i]) then
      exit(false);
  result := true;
end;

function BitfieldRangeTest.SrcBitfieldPiece: string;
begin
  result :=
    'Source bitfield:' + LineEnding +
    BytesToString(src, Size);
end;

function BitfieldRangeTest.ResbAndExpectedPiece: string;
begin
  result :=
    BitsToString(resb, Size) + LineEnding +
    'expected ' + LineEnding +
    BytesToString(expect, Size);
end;

class function BitfieldRangeTest.BytesToString(b: pByte; n: SizeUint): string;
var
  i: SizeInt;
begin
  result := '';
  for i := 0 to SizeInt(n) - 1 do
  begin
    if (i > 0) and (SizeUint(i) mod bitsizeof(TBitsBase) = 0) then
      result += LineEnding;
    result += IntToStr(b[i]);
  end;
end;

class function BitfieldRangeTest.BitsToString(b: TBits; n: SizeUint): string;
var
  i: SizeInt;
begin
  result := '';
  for i := 0 to SizeInt(n) - 1 do
  begin
    if (i > 0) and (SizeUint(i) mod bitsizeof(TBitsBase) = 0) then
      result += LineEnding;
    result += Bool01[b[i]];
  end;
end;

procedure TestRanges;
var
  iBitfieldSeed, i: SizeInt;
  br: BitfieldRangeTest;
begin
  br.srcb := nil; br.resb := nil;
  try
    br.srcb := TBits.Create(br.Size);
    br.resb := TBits.Create(br.Size);
    for iBitfieldSeed := 1 to 10 do
    begin
      RandSeed := iBitfieldSeed;
      for i := 0 to br.Size - 1 do
      begin
        br.src[i] := random(2);
        br.srcb[i] := br.src[i] <> 0;
      end;
      br.PerformOnSrc;
    end;
  finally
    br.srcb.Free; br.resb.Free;
  end;
end;

procedure TestGetSetUint;
type
  NumRec = record
    value, count, pos: cardinal;
  end;
var
  iBitfieldSeed, i, iSwap, len: SizeInt;
  b: TBits;
  n: array of NumRec;
  nSwap: NumRec;
  msg: string;
  got: cardinal;
begin
  for iBitfieldSeed := 1 to 10 do
  begin
    RandSeed := iBitfieldSeed;

    // Fill 'n' with random numbers of different bit lengths.
    SetLength((@n)^, 200);
    len := 0;
    for i := 0 to High(n) do
    begin
      n[i].count := 1 + i * bitsizeof(cardinal) div length(n);
      n[i].value := random(High(cardinal) + 1);
      if n[i].count < bitsizeof(n[i].value) then
        n[i].value := n[i].value and (cardinal(1) shl n[i].count - 1);
      n[i].pos := len;
      len += SizeInt(n[i].count);
    end;

    // Shuffle 'n'.
    for i := High(n) downto 1 do
    begin
      iSwap := random(i + 1);
      nSwap := n[iSwap];
      n[iSwap] := n[i];
      n[i] := nSwap;
    end;

    b := TBits.Create;
    try
      for i := 0 to High(n) do
        b.SetUint(n[i].pos, n[i].count, n[i].value);
      for i := 0 to High(n) do
      begin
        got := b.GetUint(n[i].pos, n[i].count);
        if got <> n[i].value then
        begin
          WriteStr(msg,
            'Get/SetUint failed:', LineEnding,
            Dump(b), LineEnding,
            'pos = ', n[i].pos, ', count = ', n[i].count, ', expected = ', n[i].value, ', got = ', got);
          Fail(msg);
          break;
        end;
      end;
    finally
      b.Free;
    end;
  end;
end;

begin
  TestCopyBits;
  TestBinOps;
  TestFinds;
  TestZeroUpper;
  TestRanges;
  TestGetSetUint;
  if somethingFailed then
    Halt(1);
  Writeln('Ok!');
end.
