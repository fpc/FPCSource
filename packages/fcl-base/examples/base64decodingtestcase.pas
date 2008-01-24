(******************************************************************************
 *                                                                            *
 *  (c) 2007 Hexis BV                                                         *
 *                                                                            *
 *  File:        Base64DecodingTestCase.pas                                   *
 *  Author:      Bram Kuijvenhoven (bkuijvenhoven@hexis.nl)                   *
 *  Description: FPCUnit tests for Base64.TBase64DecodingStream               *
 *                                                                            *
 ******************************************************************************)

unit Base64DecodingTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, base64;

type

  { TBase64DecodingStreamTestCase }

  TBase64DecodingStreamTestCase = class(TTestCase)
  private
    Base64Stream: TBase64DecodingStream;
    SourceStream: TStream;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    // utility
    procedure WriteToSourceStream(s:string);
    function NoiseCharString:string; // all non-base64 alphabet characters
    function RandomNoise(l:integer):string; // l random non-base64 alphabet characters
    // test templates
    procedure TestGetSize(mode:TBase64DecodingMode; s:string; expectedSize:Int64);
    procedure TestDecode (mode:TBase64DecodingMode; s, expected:string);
    procedure TestReset  (mode:TBase64DecodingMode; s:string; start:array of Int64; expected:array of string);
  published
    procedure TestGetSizeStrict1;
    procedure TestGetSizeStrict2;
    procedure TestGetSizeStrict3;
    procedure TestGetSizeStrict4;
    procedure TestGetSizeStrict5;
    procedure TestGetSizeMIME01;
    procedure TestGetSizeMIME02;
    procedure TestGetSizeMIME03;
    procedure TestGetSizeMIME04;
    procedure TestGetSizeMIME05;
    procedure TestGetSizeMIME06;
    procedure TestGetSizeMIME07;
    procedure TestGetSizeMIME08;
    procedure TestGetSizeMIME09;
    procedure TestGetSizeMIME10;
    procedure TestGetSizeMIME11;
    procedure TestGetSizeMIME12;
    procedure TestGetSizeMIME13;
    procedure TestGetSizeMIME14;
    procedure TestGetSizeMIME15;
    procedure TestGetSizeMIME16;
    procedure TestGetSizeMIME17;
    procedure TestGetSizeMIME18;
    procedure TestGetSizeMIME19;
    procedure TestGetSizeMIME20;
    procedure TestGetSizeMIME21;
    procedure TestGetSizeMIME22;
    procedure TestDecodeStrict01;
    procedure TestDecodeStrict02;
    procedure TestDecodeStrict03;
    procedure TestDecodeStrict04;
    procedure TestDecodeStrict05;
    procedure TestDecodeStrict06;
    procedure TestDecodeStrict07;
    procedure TestDecodeStrict08;
    procedure TestDecodeStrict09;
    procedure TestDecodeStrict10;
    procedure TestDecodeMIME1;
    procedure TestDecodeMIME2;
    procedure TestDecodeMIME3;
    procedure TestDecodeMIME4;
    procedure TestDecodeMIME5;
    procedure TestDecodeMIME6;
    procedure TestDecodeMIME7;
    procedure TestDecodeMIME8;
    procedure TestDecodeMIME9;
    procedure TestResetStrict1;
    procedure TestResetStrict2;
    procedure TestResetStrict3;
    procedure TestResetMIME1;
    procedure TestResetMIME2;
    procedure TestResetMIME3;
  end;
  
implementation

uses
  Math;

// utility routines
type
  TChars = set of char;
  
function SetToString(chars:TChars):string;
var
  pos: Integer;
  c: Char;
begin
  pos:=0;
  SetLength(Result,256);
  for c:=#0 to #255 do
    if c in chars then begin
      Inc(pos);
      Result[pos]:=c;
    end;
  SetLength(Result,pos);
end;

function Shuffle(s:string):string;
var
  i: Integer;
  randomPos: Integer;
  c: Char;
begin
  UniqueString(s);
  for i:=1 to Length(s) do begin
    randomPos:=Random(Length(s))+1;
    c:=s[randomPos];
    s[randomPos]:=s[i];
    s[i]:=c;
  end;
  Result:=s;
end;

// order preserving shuffle
function Merge(s,t:string):string;
var
  si: Integer;
  ti: Integer;
  sLeft: Integer;
  tLeft: Integer;
  chooseS: Boolean;
  ri: Integer;
  count: LongInt;
  i: Integer;
begin
  si := 1;
  ti := 1;
  ri := 1;
  sLeft := Length(s);
  tLeft := Length(t);
  SetLength(Result, sLeft + tLeft);
  while (sLeft>0) or (tLeft>0) do begin
    chooseS := Random(sLeft + tLeft) < sLeft;
    if chooseS then begin
      count := Min(Random(7)+1, sLeft);
      for i := 0 to count - 1 do
        Result[ri + i] := s[si + i];
      Inc(ri, count);
      Inc(si, count);
      Dec(sLeft, count);
    end else begin
      count := Min(Random(7)+1, tLeft);
      for i := 0 to count - 1 do
        Result[ri + i] := t[ti + i];
      Inc(ri, count);
      Inc(ti, count);
      Dec(tLeft, count);
    end;
  end;
end;

procedure TBase64DecodingStreamTestCase.SetUp;
begin
  SourceStream := TMemoryStream.Create;
  Base64Stream := TBase64DecodingStream.Create(SourceStream);
end;

procedure TBase64DecodingStreamTestCase.TearDown;
begin
  FreeAndNil(Base64Stream);
  FreeAndNil(SourceStream);
end;

procedure TBase64DecodingStreamTestCase.WriteToSourceStream(s: string);
begin
  SourceStream.Write(s[1],Length(s));
  SourceStream.Position:=0;
end;

function TBase64DecodingStreamTestCase.NoiseCharString: string;
begin
  Result:=SetToString([#0..#255]-['a'..'z','A'..'Z','0'..'9','+','/','=']);
end;

function TBase64DecodingStreamTestCase.RandomNoise(l: integer): string;
var
  i: Integer;
  noiseChars: String;
begin
  noiseChars:=NoiseCharString; // our pool
  SetLength(Result,l);
  for i:=1 to l do
    Result[i]:=noiseChars[Random(Length(noiseChars))+1];
end;

procedure TBase64DecodingStreamTestCase.TestGetSize(mode:TBase64DecodingMode; s: string; expectedSize: Int64);
var
  Size: Int64;
begin
  Base64Stream.Mode := mode;
  WriteToSourceStream(s);
  Size := Base64Stream.Size;
  AssertEquals('Correct size calculated by Size getter', Size, expectedSize);
end;

procedure TBase64DecodingStreamTestCase.TestDecode(mode: TBase64DecodingMode; s, expected: string);
var
  Buf: array[0..63] of Char;
  si: Integer;
  i: Integer;
  count: LongInt;
begin
  writeln(Length(s),'->',Length(expected));
  Base64Stream.Mode := mode;
  WriteToSourceStream(s);
  si := 1;  // index into expected
  repeat
    count := Base64Stream.Read(Buf, Random(SizeOf(Buf))+1);
    AssertTrue('Not too many bytes decoded', si + count - 1 <= Length(expected));
    for i := 0 to count-1 do
      AssertEquals('Correctly decoded byte', expected[si + i], Buf[i]);
    Inc(si, count);
    AssertEquals('Returned size is correct', Length(expected), Base64Stream.Size);
  until Base64Stream.EOF;
  AssertEquals('Correct decoded length', Length(expected), si-1);
end;

procedure TBase64DecodingStreamTestCase.TestReset(mode: TBase64DecodingMode; s: string; start: array of Int64; expected: array of string);
var
  Buf: array[0..63] of Char;
  len: Integer;
  i: Integer;
  count: LongInt;
  startI: Integer;
begin
  Base64Stream.Mode := mode;
  WriteToSourceStream(s);
  AssertEquals('start and expected arrays have same length',Length(start),Length(expected));
  for startI := 0 to High(start) do begin
    SourceStream.Position := start[startI];
    Base64Stream.Reset;
    // test decoding
    len := 0;
    repeat
      count := Base64Stream.Read(Buf, Random(SizeOf(Buf))+1);
      AssertTrue('Not too many bytes decoded', len + count <= Length(expected[startI]));
      for i := 0 to count-1 do
        AssertEquals(Format('Correctly decoded byte at %d',[len+i]), expected[startI][len + i + 1], Buf[i]);
      Inc(len, count);
      AssertEquals('Returned size is correct', Length(expected[startI]), Base64Stream.Size);
    until Base64Stream.EOF;
    AssertEquals('Correct decoded length', Length(expected[startI]), len);
  end;
end;

procedure TBase64DecodingStreamTestCase.TestGetSizeStrict1; begin TestGetSize(bdmStrict,'',0); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeStrict2; begin TestGetSize(bdmStrict,'aAzZ09+/',6); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeStrict3; begin TestGetSize(bdmStrict,'aAzZ09+=',5); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeStrict4; begin TestGetSize(bdmStrict,'aAzZ09==',4); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeStrict5; begin TestGetSize(bdmStrict,'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+/',48); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME01; begin TestGetSize(bdmMIME,'',0); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME02; begin TestGetSize(bdmMIME,NoiseCharString,0); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME03; begin TestGetSize(bdmMIME,Shuffle(NoiseCharString),0); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME04; begin TestGetSize(bdmMIME,Shuffle(RandomNoise(12))+'=',0); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME05; begin TestGetSize(bdmMIME,Shuffle(NoiseCharString)+'==',0); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME06; begin TestGetSize(bdmMIME,Shuffle(RandomNoise(13))+'===',0); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME07; begin TestGetSize(bdmMIME,Shuffle(NoiseCharString)+'====',0); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME08; begin TestGetSize(bdmMIME,Shuffle(RandomNoise(14))+'a=',0); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME09; begin TestGetSize(bdmMIME,Shuffle(NoiseCharString)+'ab==',1); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME10; begin TestGetSize(bdmMIME,Shuffle(RandomNoise(15))+'abc=',2); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME11; begin TestGetSize(bdmMIME,Shuffle(NoiseCharString)+'abcd',3); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME12; begin TestGetSize(bdmMIME,Shuffle(RandomNoise(16)+'abcd'),3); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME13; begin TestGetSize(bdmMIME,Shuffle(NoiseCharString+'abcd')+'=',3); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME14; begin TestGetSize(bdmMIME,Shuffle(RandomNoise(17)+'abcd')+'01=',4); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME15; begin TestGetSize(bdmMIME,Shuffle(NoiseCharString+'abcd')+'01==',4); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME16; begin TestGetSize(bdmMIME,Shuffle(RandomNoise(18)+'abcd')+'012==',5); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME17; begin TestGetSize(bdmMIME,Shuffle(NoiseCharString+'abcd')+'0123=',6); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME18; begin TestGetSize(bdmMIME,Shuffle(RandomNoise(19)+'abcd')+'012345=',7); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME19; begin TestGetSize(bdmMIME,Shuffle(NoiseCharString+'abcd')+'=01234=',3); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME20; begin TestGetSize(bdmMIME,Shuffle(RandomNoise(20)+'abcd')+'0=1234=',3); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME21; begin TestGetSize(bdmMIME,Shuffle(NoiseCharString+'abcd')+'01=234=',4); end;
procedure TBase64DecodingStreamTestCase.TestGetSizeMIME22; begin TestGetSize(bdmMIME,Shuffle(RandomNoise(99)+'abcd')+'012=34=',5); end;

procedure TBase64DecodingStreamTestCase.TestDecodeStrict01; begin TestDecode(bdmStrict,'',''); end;
procedure TBase64DecodingStreamTestCase.TestDecodeStrict02; begin TestDecode(bdmStrict,'AA==',#0); end;
procedure TBase64DecodingStreamTestCase.TestDecodeStrict03; begin TestDecode(bdmStrict,'AAA=',#0#0); end;
procedure TBase64DecodingStreamTestCase.TestDecodeStrict04; begin TestDecode(bdmStrict,'AAAA',#0#0#0); end;
procedure TBase64DecodingStreamTestCase.TestDecodeStrict05; begin TestDecode(bdmStrict,'AAAAAA==',#0#0#0#0); end;
procedure TBase64DecodingStreamTestCase.TestDecodeStrict06; begin TestDecode(bdmStrict,'AAAAAAA=',#0#0#0#0#0); end;
procedure TBase64DecodingStreamTestCase.TestDecodeStrict07; begin TestDecode(bdmStrict,'AAAAAAAA',#0#0#0#0#0#0); end;
procedure TBase64DecodingStreamTestCase.TestDecodeStrict08; begin TestDecode(bdmStrict,'AAAAAAAA',#0#0#0#0#0#0); end;
procedure TBase64DecodingStreamTestCase.TestDecodeStrict09; begin TestDecode(bdmStrict, 'TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IGxhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbmltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIGxhYm9yaXM'+'gbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdWF0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbiB2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdCBudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaWRhdGF0IG5vbiBwcm9pZGVudC'+'wgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYSBkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4K',
                                                                                        'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor '+'in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.'#10); end;
procedure TBase64DecodingStreamTestCase.TestDecodeStrict10; begin TestDecode(bdmStrict, 'QmFzZTY0IERlY29kaW5nIFN1Y2NlZWRlZCE=','Base64 Decoding Succeeded!'); end;
procedure TBase64DecodingStreamTestCase.TestDecodeMIME1; begin TestDecode(bdmMIME,Merge('QmFzZTY0IERlY29kaW5nIFN1Y2NlZWRlZCE=',RandomNoise(12  )),'Base64 Decoding Succeeded!'); end;
procedure TBase64DecodingStreamTestCase.TestDecodeMIME2; begin TestDecode(bdmMIME,Merge('QmFzZTY0IERlY29kaW5nIFN1Y2NlZWRlZCE=',RandomNoise(1200)),'Base64 Decoding Succeeded!'); end;
procedure TBase64DecodingStreamTestCase.TestDecodeMIME3; begin TestDecode(bdmMIME,      'QmFzZTY0'                   ,'Base64'); end;
procedure TBase64DecodingStreamTestCase.TestDecodeMIME4; begin TestDecode(bdmMIME,      'QmFzZTY0X='                 ,'Base64'); end;
procedure TBase64DecodingStreamTestCase.TestDecodeMIME5; begin TestDecode(bdmMIME,Merge('QmFzZTY0',RandomNoise(1200)),'Base64'); end;
procedure TBase64DecodingStreamTestCase.TestDecodeMIME6; begin TestDecode(bdmMIME,      'TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IGxhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbmltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIGxhYm9yaXM'+'gbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdWF0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbiB2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdCBudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaWRhdGF0IG5vbiBwcm9pZGVudC'+'wgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYSBkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4K',
                                                                                        'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor '+'in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.'#10); end;
procedure TBase64DecodingStreamTestCase.TestDecodeMIME7; begin TestDecode(bdmMIME,Merge('TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IGxhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbmltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIGxhYm9yaXM'+'gbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdWF0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbiB2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdCBudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaWRhdGF0IG5vbiBwcm9pZGVudC'+'wgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYSBkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4K',RandomNoise(1200)),
                                                                                        'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor '+'in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.'#10); end;
procedure TBase64DecodingStreamTestCase.TestDecodeMIME8; begin TestDecode(bdmMIME,Merge('TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IGxhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbmltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIGxhYm9yaXM'+'gbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdWF0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbiB2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdCBudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaWRhdGF0IG5vbiBwcm9pZGVudC'+'wgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYSBkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4K=',RandomNoise(1200)),
                                                                                        'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor '+'in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.'#10); end;
procedure TBase64DecodingStreamTestCase.TestDecodeMIME9; begin TestDecode(bdmMIME,Merge('TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IGxhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbmltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIGxhYm9yaXM'+'gbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdWF0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbiB2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdCBudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaWRhdGF0IG5vbiBwcm9pZGVudC'+'wgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYSBkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4KX=',RandomNoise(1200)),
                                                                                        'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor '+'in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.'#10); end;

procedure TBase64DecodingStreamTestCase.TestResetStrict1; begin TestReset(bdmStrict,'',[0],['']); end;
procedure TBase64DecodingStreamTestCase.TestResetStrict2; begin TestReset(bdmStrict,'AA==',[0,4],[#0,'']); end;
procedure TBase64DecodingStreamTestCase.TestResetStrict3; begin TestReset(bdmStrict,'AAAAAAA=',[0,4,8],[#0#0#0#0#0,#0#0,'']); end;
procedure TBase64DecodingStreamTestCase.TestResetMIME1; begin TestReset(bdmMIME,'QmFzZTY0IERlY29kaW5nIFN1Y2NlZWRlZCE=',[0],['Base64 Decoding Succeeded!']); end;
procedure TBase64DecodingStreamTestCase.TestResetMIME2; begin TestReset(bdmMIME,'QmFzZTY0IERlY29kaW5nIFN1Y2NlZWRlZCE=',[0,4,8],['Base64 Decoding Succeeded!','e64 Decoding Succeeded!',' Decoding Succeeded!']); end;
procedure TBase64DecodingStreamTestCase.TestResetMIME3; begin TestReset(bdmMIME,'Qm!@#$%^&*()F!@#$%^&*()z!@#$%^&*()ZTY0IERlY29kaW5nIFN1Y2NlZWRlZCE=',[0,24,34,38,62,66],['Base64 Decoding Succeeded!','e64 Decoding Succeeded!','e64 Decoding Succeeded!',' Decoding Succeeded!','d!','']); end;

initialization

  RegisterTest(TBase64DecodingStreamTestCase);
  
end.

