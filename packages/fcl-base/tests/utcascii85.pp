unit utcascii85;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, punit, ascii85;

procedure RegisterTests;

implementation

function EncodeBytes(const aData: TBytes; aWidth: Integer = 72; aBoundary: Boolean = False): String;

var
  Dest: TStringStream;
  Enc: TASCII85EncoderStream;

begin
  Dest := TStringStream.Create('');
  try
    Enc := TASCII85EncoderStream.Create(Dest, aWidth, aBoundary);
    try
      if Length(aData) > 0 then
        Enc.WriteBuffer(aData[0], Length(aData));
    finally
      Enc.Free; // flushes
    end;
    Result := Dest.DataString;
  finally
    Dest.Free;
  end;
end;

function EncodeStr(const aData: String; aWidth: Integer = 72; aBoundary: Boolean = False): String;

var
  B: TBytes;

begin
  SetLength(B, Length(aData));
  if Length(aData) > 0 then
    Move(aData[1], B[0], Length(aData));
  Result := EncodeBytes(B, aWidth, aBoundary);
end;

function DecodeStr(const aEncoded: String): TBytes;

var
  Src: TStringStream;
  Dec: TASCII85DecoderStream;
  Buf: array[0..255] of Byte;
  N, Len: Integer;

begin
  Result := nil;
  Len := 0;
  Src := TStringStream.Create(aEncoded);
  try
    Dec := TASCII85DecoderStream.Create(Src);
    try
      repeat
        N := Dec.Read(Buf, SizeOf(Buf));
        if N > 0 then
          begin
          SetLength(Result, Len + N);
          Move(Buf, Result[Len], N);
          Inc(Len, N);
          end;
      until N <= 0;
    finally
      Dec.Free;
    end;
  finally
    Src.Free;
  end;
end;

function ZeroBytes(aCount: Integer): TBytes;

begin
  Result := nil;
  SetLength(Result, aCount);
  if aCount > 0 then
    FillChar(Result[0], aCount, 0);
end;

function BytesToStr(const aBytes: TBytes): String;

var
  I: Integer;

begin
  Result := '';
  for I := 0 to Length(aBytes) - 1 do
    begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + IntToStr(aBytes[I]);
    end;
  Result := '[' + Result + ']';
end;

function AssertBytesEqual(const Msg: String; const aExpected, aActual: TBytes): Boolean;

var
  Cmp: Boolean;

begin
  Cmp := (Length(aActual) = Length(aExpected)) and
         ((Length(aActual) = 0) or CompareMem(@aActual[0], @aExpected[0], Length(aActual)));
  Result := AssertTrue(Msg + '. Expected: ' + BytesToStr(aExpected) + ', Actual: ' + BytesToStr(aActual), Cmp);
end;

function StripLineBreaks(const S: String): String;

begin
  Result := StringReplace(S, #13, '', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);
end;

function TestEncodeEmpty: TTestString;

begin
  Result := '';
  AssertEquals('Empty input', '', EncodeStr(''));
end;

function TestEncodeKnownVectors: TTestString;

begin
  Result := '';
  AssertEquals('"Man "', '9jqo^', EncodeStr('Man '));
  AssertEquals('"Man is d"', '9jqo^BlbD-', EncodeStr('Man is d'));
  AssertEquals('"M"', '9`', EncodeStr('M'));
  AssertEquals('"Ma"', '9jn', EncodeStr('Ma'));
  AssertEquals('"Man"', '9jqo', EncodeStr('Man'));
end;

function TestEncodePartialGroups: TTestString;

begin
  Result := '';
  // Partial groups of N bytes must produce N+1 characters
  AssertEquals('1 byte', '#6', EncodeBytes([7]));
  AssertEquals('2 bytes', '!!`', EncodeBytes([0, 7]));
  AssertEquals('3 bytes', '!!!6', EncodeBytes([0, 0, 7]));
  AssertEquals('4 bytes', '!!!!(', EncodeBytes([0, 0, 0, 7]));
  AssertEquals('5 bytes', '!!!!(#6', EncodeBytes([0, 0, 0, 7, 7]));
  AssertEquals('High bytes 1', 'rr', EncodeBytes([255]));
  AssertEquals('High bytes 3', 'rrC+', EncodeBytes([255, 0, 200]));
end;

function TestEncodeZeroShortcut: TTestString;

begin
  Result := '';
  AssertEquals('4 zero bytes', 'z', EncodeBytes(ZeroBytes(4)));
  AssertEquals('8 zero bytes', 'zz', EncodeBytes(ZeroBytes(8)));
  AssertEquals('12 zero bytes', 'zzz', EncodeBytes(ZeroBytes(12)));
end;

function TestEncodeZeroPartialGroup: TTestString;

begin
  Result := '';
  // 'z' stands for a full group of 4 zero bytes only
  AssertEquals('1 zero byte', '!!', EncodeBytes(ZeroBytes(1)));
  AssertEquals('2 zero bytes', '!!!', EncodeBytes(ZeroBytes(2)));
  AssertEquals('3 zero bytes', '!!!!', EncodeBytes(ZeroBytes(3)));
  AssertEquals('5 zero bytes', 'z!!', EncodeBytes(ZeroBytes(5)));
  AssertEquals('6 zero bytes', 'z!!!', EncodeBytes(ZeroBytes(6)));
  AssertEquals('7 zero bytes', 'z!!!!', EncodeBytes(ZeroBytes(7)));
  AssertEquals('9 zero bytes', 'zz!!', EncodeBytes(ZeroBytes(9)));
end;

function TestEncodeMultipleWrites: TTestString;

var
  Dest: TStringStream;
  Enc: TASCII85EncoderStream;
  B: Byte;
  I: Integer;

begin
  Result := '';
  Dest := TStringStream.Create('');
  try
    Enc := TASCII85EncoderStream.Create(Dest, 72, False);
    try
      // One byte at a time, groups must be carried across Write calls
      for I := 1 to 8 do
        begin
        B := Ord('A') + I;
        Enc.WriteBuffer(B, 1);
        end;
    finally
      Enc.Free;
    end;
    AssertEquals('Byte-wise writes equal single write', EncodeStr('BCDEFGHI'), Dest.DataString);
  finally
    Dest.Free;
  end;
end;

function TestEncodeBoundary: TTestString;

begin
  Result := '';
  AssertEquals('Boundary, full group', '<~9jqo^~>' + sLineBreak, EncodeStr('Man ', 72, True));
  AssertEquals('Boundary, partial group', '<~9jqo~>' + sLineBreak, EncodeStr('Man', 72, True));
  AssertEquals('Boundary, empty', '<~~>' + sLineBreak, EncodeStr('', 72, True));
end;

function TestEncodeWidth: TTestString;

var
  Data, S, Line: String;
  Lines: TStringList;
  I: Integer;

begin
  Result := '';
  Data := 'The quick brown fox jumps over the lazy dog';
  S := EncodeStr(Data, 10);
  AssertEquals('Content unchanged by wrapping', EncodeStr(Data, 1000), StripLineBreaks(S));
  Lines := TStringList.Create;
  try
    Lines.Text := S;
    AssertTrue('Output is wrapped', Lines.Count > 1);
    for I := 0 to Lines.Count - 1 do
      begin
      Line := Lines[I];
      AssertTrue('Line ' + IntToStr(I) + ' not longer than width', Length(Line) <= 10);
      end;
  finally
    Lines.Free;
  end;
end;

function TestDecodeKnownVectors: TTestString;

begin
  Result := '';
  AssertBytesEqual('Empty', [], DecodeStr(''));
  AssertBytesEqual('"Man "', [77, 97, 110, 32], DecodeStr('9jqo^'));
  AssertBytesEqual('"M"', [77], DecodeStr('9`'));
  AssertBytesEqual('"Ma"', [77, 97], DecodeStr('9jn'));
  AssertBytesEqual('"Man"', [77, 97, 110], DecodeStr('9jqo'));
  AssertBytesEqual('z', [0, 0, 0, 0], DecodeStr('z'));
  AssertBytesEqual('z + partial', [0, 0, 0, 0, 0], DecodeStr('z!!'));
  AssertBytesEqual('Boundaries', [77, 97, 110, 32], DecodeStr('<~9jqo^~>'));
  AssertBytesEqual('Whitespace ignored', [77, 97, 110, 32], DecodeStr('9j'#10'qo'#13#10'^'));
end;

function TestRoundTrip: TTestString;

var
  N, Pattern, I: Integer;
  B: TBytes;

begin
  Result := '';
  B := nil;
  for N := 0 to 40 do
    for Pattern := 0 to 3 do
      begin
      SetLength(B, N);
      for I := 0 to N - 1 do
        case Pattern of
          0: B[I] := 0;
          1: B[I] := 255;
          2: B[I] := I * 7 + 1;
          3: if (I div 4) mod 2 = 0 then B[I] := 0 else B[I] := I;
        end;
      AssertBytesEqual('Round trip N=' + IntToStr(N) + ' pattern=' + IntToStr(Pattern),
                       B, DecodeStr(EncodeBytes(B)));
      AssertBytesEqual('Round trip with boundary and wrap N=' + IntToStr(N) + ' pattern=' + IntToStr(Pattern),
                       B, DecodeStr(EncodeBytes(B, 8, True)));
      end;
end;

procedure RegisterTests;

begin
  AddSuite('TASCII85Tests');
  AddTest('TestEncodeEmpty', @TestEncodeEmpty, 'TASCII85Tests');
  AddTest('TestEncodeKnownVectors', @TestEncodeKnownVectors, 'TASCII85Tests');
  AddTest('TestEncodePartialGroups', @TestEncodePartialGroups, 'TASCII85Tests');
  AddTest('TestEncodeZeroShortcut', @TestEncodeZeroShortcut, 'TASCII85Tests');
  AddTest('TestEncodeZeroPartialGroup', @TestEncodeZeroPartialGroup, 'TASCII85Tests');
  AddTest('TestEncodeMultipleWrites', @TestEncodeMultipleWrites, 'TASCII85Tests');
  AddTest('TestEncodeBoundary', @TestEncodeBoundary, 'TASCII85Tests');
  AddTest('TestEncodeWidth', @TestEncodeWidth, 'TASCII85Tests');
  AddTest('TestDecodeKnownVectors', @TestDecodeKnownVectors, 'TASCII85Tests');
  AddTest('TestRoundTrip', @TestRoundTrip, 'TASCII85Tests');
end;

end.
