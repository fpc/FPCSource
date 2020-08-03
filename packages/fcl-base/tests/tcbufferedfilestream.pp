unit tcbufferedfilestream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, bufstream;

type

  { TTestBufferedFileStream }

  TTestBufferedFileStream= class(TTestCase)
  private
  const
    TEST_RANDOM_READS=10000;
    TEST_SEQUENTIAL_READS=1000000;
    TEST_FILENAME='testfile.bin';
    TEST_WRITEC_FILE='testwritecache.bin';
    TEST_WRITEF_FILE='testwritedirec.bin';
  private
    function CompareStreams(const aStream1: TStream; const aStream2: TStream): Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCacheRead;
    procedure TestCacheWrite;
    procedure TestCacheSeek;
  end;

implementation

procedure TTestBufferedFileStream.TestCacheRead;
var
  lBufferedStream: TBufferedFileStream;
  lStream: TFileStream;
  b: array [0..10000-1] of char;
  j,k: integer;
  lBytesToRead: integer;
  lEffectiveRead: integer;
  {$IFDEF CHECK_AGAINST_FILE}
  lEffectiveRead2: integer;
  {$ENDIF}
  lReadPosition: int64;
  lCheckInitV: integer;
  lTick: QWord;
begin
  b[0]:=#0; // Avoid initalization hint
  lBufferedStream:=TBufferedFileStream.Create(TEST_FILENAME,fmOpenRead or fmShareDenyWrite);
  lStream:=TFileStream.Create(TEST_FILENAME,fmOpenRead or fmShareDenyWrite);
  try
    RandSeed:=1;
    Randomize;
    lTick:=GetTickCount64;
    for j := 0 to Pred(TEST_RANDOM_READS) do begin
      lBytesToRead:=Random(10000);
      lReadPosition:=Random(lBufferedStream.Size);
      lBufferedStream.Position:=lReadPosition;

      lEffectiveRead:=lBufferedStream.Read(b,lBytesToRead);

      {$IFDEF CHECK_AGAINST_FILE}
      // Now read without cache
      lStream.Position:=lReadPosition;
      lEffectiveRead2:=lStream.Read(b2,lBytesToRead);
      if lEffectiveRead<>lEffectiveRead2 then begin
        FAIL('Read length mismatch');
      end;
      if not CompareMem(@b[0],@b2[0],lEffectiveRead) then begin
        FAIL('Compare buffer data error');
      end;
      F.Position:=0;
      {$ELSE}
      lCheckInitV:=lReadPosition mod 10;
      for k := 0 to Pred(lEffectiveRead) do begin
        if b[k]<>char(ord('0')+lCheckInitV mod 10) then begin
          FAIL('Expected data error');
        end;
        inc(lCheckInitV);
      end;
      {$ENDIF}
    end;
    // Writeln('CACHE ',TEST_RANDOM_READS,' random reads in ',GetTickCount64-lTick,' ms.');

    RandSeed:=1;
    Randomize;

    // Writeln('Same operation without cache');
    lTick:=GetTickCount64;
    for j := 0 to Pred(TEST_RANDOM_READS) do begin
      lBytesToRead:=Random(10000);
      lReadPosition:=Random(lBufferedStream.Size);

      lStream.Position:=lReadPosition;
      lEffectiveRead:=lStream.Read(b,lBytesToRead);

      lCheckInitV:=lReadPosition mod 10;
      for k := 0 to Pred(lEffectiveRead) do begin
        if b[k]<>char(ord('0')+lCheckInitV mod 10) then begin
          FAIL('Expected data error');
        end;
        inc(lCheckInitV);
      end;
    end;
    // Writeln('FILE ',TEST_RANDOM_READS,' random reads in ',GetTickCount64-lTick,' ms.');

    // Writeln('Check sequential read');

    RandSeed:=1;
    Randomize;
    lTick:=GetTickCount64;
    lBytesToRead:=1;
    lReadPosition:=0;
    lBufferedStream.Position:=lReadPosition;
    lStream.Position:=lReadPosition;
    for j := 0 to Pred(TEST_SEQUENTIAL_READS) do begin

      lEffectiveRead:=lBufferedStream.Read(b,lBytesToRead);

      {$IFDEF CHECK_AGAINST_FILE}
      // Now read without cache
      lEffectiveRead2:=lStream.Read(b2,lBytesToRead);
      if lEffectiveRead<>lEffectiveRead2 then begin
        FAIL('Read length mismatch');
      end;
      if not CompareMem(@b[0],@b2[0],lEffectiveRead) then begin
        FAIL('Compare buffer data error');
      end;
      F.Position:=0;
      {$ELSE}
      lCheckInitV:=lReadPosition mod 10;
      for k := 0 to Pred(lEffectiveRead) do begin
        if b[k]<>char(ord('0')+lCheckInitV mod 10) then begin
          FAIL('Expected data error');
        end;
        inc(lCheckInitV);
      end;
      {$ENDIF}
      inc(lReadPosition,lBytesToRead);
    end;
    // Writeln('CACHE ',TEST_SEQUENTIAL_READS,' byte sequential reads in ',GetTickCount64-lTick,' ms.');

    RandSeed:=1;
    Randomize;
    lTick:=GetTickCount64;
    lBytesToRead:=1;
    lReadPosition:=0;
    lStream.Position:=lReadPosition;
    for j := 0 to Pred(TEST_SEQUENTIAL_READS) do begin

      lEffectiveRead:=lStream.Read(b,lBytesToRead);

      lCheckInitV:=lReadPosition mod 10;
      for k := 0 to Pred(lEffectiveRead) do begin
        if b[k]<>char(ord('0')+lCheckInitV mod 10) then begin
          FAIL('Expected data error');
        end;
        inc(lCheckInitV);
      end;
      inc(lReadPosition,lBytesToRead);
    end;
    // Writeln('FILE ',TEST_SEQUENTIAL_READS,' byte sequential reads in ',GetTickCount64-lTick,' ms.');

    // Writeln('CACHE Trying read beyond limits');
    lBufferedStream.Position:=lBufferedStream.Size-1;
    lEffectiveRead:=lBufferedStream.Read(b,2);
    if lEffectiveRead<>1 then begin
      FAIL('Read beyond limits, returned bytes: '+inttostr(lEffectiveRead));
    end else begin
      // Writeln('CACHE OK, read beyond limits returns 0 bytes.');
    end;
  finally
    lBufferedStream.Free;
    lStream.Free;
  end;
end;

procedure TTestBufferedFileStream.TestCacheWrite;
const
  EXPECTED_SIZE=10000000;
  TEST_ROUNDS=100000;
var
  lBufferedStream: TBufferedFileStream;
  lStream: TFileStream;
  lVerifyStream1,lVerifyStream2: TFileStream;
  b: array [0..10000-1] of char;
  j: integer;
  lBytesToWrite: integer;
  lWritePosition: int64;
begin
  // Writeln('Testing write cache');
  // All test should return the same random sequence
  RandSeed:=1;
  Randomize;
  for j := 0 to Pred(10000) do begin
    b[j]:='0';
  end;
  lBufferedStream:=TBufferedFileStream.Create(TEST_WRITEC_FILE,fmCreate);
  lStream:=TFileStream.Create(TEST_WRITEF_FILE,fmCreate);
  try
    for j := 0 to Pred(EXPECTED_SIZE div Sizeof(b)) do begin
      lBufferedStream.Write(b,sizeof(b));
      lStream.Write(b,sizeof(b));
    end;
    for j := 0 to Pred(Sizeof(b)) do begin
      b[j]:=char(ord('0')+j mod 10);
    end;
  finally
    lBufferedStream.Free;
    lStream.Free;
  end;
  lBufferedStream:=TBufferedFileStream.Create(TEST_WRITEC_FILE,fmOpenReadWrite);
  lStream:=TFileStream.Create(TEST_WRITEF_FILE,fmOpenWrite);
  try
    for j := 0 to Pred(TEST_ROUNDS) do begin
      if lStream.Size<>lBufferedStream.Size then begin
        FAIL('Mismatched lengths');
      end;
      lWritePosition:=Random(EXPECTED_SIZE);
      lBytesToWrite:=Random(sizeof(b));
      lBufferedStream.Position:=lWritePosition;
      lStream.Position:=lWritePosition;
      lBufferedStream.Write(b,lBytesToWrite);
      lStream.Write(b,lBytesToWrite);
      // if j mod 1273 = 0 then write(j,' / ',TEST_ROUNDS,#13);
    end;
    // Writeln(TEST_ROUNDS,' / ',TEST_ROUNDS);
    if lStream.Size<>lBufferedStream.Size then begin
      FAIL('Mismatched lengths');
    end;
  finally
    lBufferedStream.Free;
    lStream.Free;
  end;

  // Verify both generated files are identical.
  lVerifyStream1:=TFileStream.Create(TEST_WRITEC_FILE,fmOpenRead or fmShareDenyWrite);
  lVerifyStream2:=TFileStream.Create(TEST_WRITEF_FILE,fmOpenRead or fmShareDenyWrite);
  try
    if not CompareStreams(lVerifyStream1,lVerifyStream2) then begin
      FAIL('Streams are different!!');
    end else begin
      // Writeln('Streams are identical. OK.');
    end;
  finally
    lVerifyStream1.Free;
    lVerifyStream2.Free;
  end;
end;

procedure TTestBufferedFileStream.TestCacheSeek;
var
  lBufferedStream: TBufferedFileStream;
  lStream: TFileStream;
  bBuffered: array [0..10000] of BYTE;
  bStream: array [0..10000] of BYTE;
  bread : Integer;

begin
  bBuffered[0]:=0; // Avoid initalization hint
  bStream[0]:=0; // Avoid initalization hint
  lBufferedStream:=TBufferedFileStream.Create(TEST_FILENAME,fmOpenRead or fmShareDenyWrite);
  lStream:=TFileStream.Create(TEST_FILENAME,fmOpenRead or fmShareDenyWrite);
  try
    // Writeln('Set position=-1');
    lStream.Position:=-1;
    // Writeln('TFileStream position=',lStream.Position);
    lBufferedStream.Position:=-1;
    // Writeln('Buffered    position=',lBufferedStream.Position);
    if lStream.Position<>lBufferedStream.Position then begin
      FAIL('Positions are not the same.');
    end else begin
      // Writeln('Positions are the same.');
    end;

    // Writeln('Read data when position=-1');
    bread:=lStream.Read(bBuffered[0],10);
     // Writeln('TFileStream read bytes  : ',bread);
     // Writeln('TFileStream end position: ',lStream.Position);
    bread:=lBufferedStream.Read(bStream[0],10);
     // Writeln('Buffered      read bytes: ',bread);
     // Writeln('Buffered    end position: ',lBufferedStream.Position);
    if (not CompareMem(@bBuffered[0],@bStream[0],10)) or (lStream.Position<>lBufferedStream.Position) then begin
      FAIL('Read data or positions are not the same.');
    end else begin
      // Writeln('Read data at -1 is the same.');
    end;

    // Writeln('Testing Seek operations');
    // Writeln('Seek -1 from beginning');
    bread:=lStream.Seek(-1,soBeginning);
    // Writeln('Stream seek result  : ',bread);
    bread:=lBufferedStream.Seek(-1,soBeginning);
    // Writeln('Buffered seek result: ',);

    // Writeln('Read data when Seek -1');
    bread:=lStream.Read(bBuffered[0],10);
    // Writeln('TFileStream read bytes  : ',bread);
    // Writeln('TFileStream end position: ',lStream.Position);
    bread:=lBufferedStream.Read(bStream[0],10);
    // Writeln('Buffered      read bytes: ',bread);
    // Writeln('Buffered    end position: ',lBufferedStream.Position);
    if (not CompareMem(@bBuffered[0],@bStream[0],10)) or (lStream.Position<>lBufferedStream.Position) then begin
      FAIL('Read data or positions are not the same.');
    end else begin
      // Writeln('Read data at -1 is the same.');
    end;

    // Writeln('Seek -current*2 from current');
    bread:=lStream.Seek(lStream.Position*-2,soCurrent);
    // Writeln('Stream seek result  : ',bread);
    bread:=lBufferedStream.Seek(lBufferedStream.Position*-2,soCurrent);
    // Writeln('Buffered seek result: ',bread);
    // Writeln('Read data when Seek from current -current*2');
    bread:=lStream.Read(bBuffered[0],10);
    // Writeln('TFileStream read bytes  : ',bread);
    // Writeln('TFileStream end position: ',lStream.Position);
    bread:=lBufferedStream.Read(bStream[0],10);
    // Writeln('Buffered      read bytes: ',);
    // Writeln('Buffered    end position: ',lBufferedStream.Position);
    if (not CompareMem(@bBuffered[0],@bStream[0],10)) or (lStream.Position<>lBufferedStream.Position) then begin
      FAIL('Read data or positions are not the same.');
    end else begin
      // Writeln('Read data at -current*2 is the same.');
    end;
  finally
    lBufferedStream.Free;
    lStream.Free;
  end;
end;

procedure TTestBufferedFileStream.SetUp;
var
  F: TFileStream;
  b: array [0..10000-1] of char;
  j: integer;
begin
  for j := 0 to Pred(10000) do begin
    b[j]:=char(ord('0')+j mod 10);
  end;
  F:=TFileStream.Create(TEST_FILENAME,fmCreate);
  for j := 0 to Pred(1000) do begin
    F.Write(b,sizeof(b));
  end;
  F.Free;
end;

procedure TTestBufferedFileStream.TearDown;
begin
  DeleteFile(TEST_FILENAME);
  DeleteFile(TEST_WRITEC_FILE);
  DeleteFile(TEST_WRITEF_FILE);
end;

function TTestBufferedFileStream.CompareStreams(const aStream1: TStream;
  const aStream2: TStream): Boolean;
const
  BUFFER_SIZE=5213; // Odd number
var
  b1: array [0..BUFFER_SIZE-1] of BYTE;
  b2: array [0..BUFFER_SIZE-1] of BYTE;
  lReadBytes: integer;
  lAvailable: integer;
  lEffectiveRead1: integer;
  lEffectiveRead2: integer;
begin
  b1[0]:=0; // Avoid initalization hint
  b2[0]:=0; // Avoid initalization hint
  Result:=false;
  if aStream1.Size<>aStream2.Size then exit;
  aStream1.Position:=0;
  aStream2.Position:=0;
  while aStream1.Position<aStream1.Size do begin
    lAvailable:=aStream1.Size-aStream1.Position;
    if lAvailable>=BUFFER_SIZE then begin
      lReadBytes:=BUFFER_SIZE;
    end else begin
      lReadBytes:=aStream1.Size-aStream1.Position;
    end;
    lEffectiveRead1:=aStream1.Read(b1[0],lReadBytes);
    lEffectiveRead2:=aStream2.Read(b2[0],lReadBytes);
    if lEffectiveRead1<>lEffectiveRead2 then exit;
    if not CompareMem(@b1[0],@b2[0],lEffectiveRead1) then exit;
  end;
  Result:=true;
end;

initialization
  RegisterTest(TTestBufferedFileStream);
end.

