unit utcchainstream;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, chainstream;

Type

  { TTestChainStream }

  TTestChainStream = Class (TTestCase)
  private
    FStreams : Array of TStream;
    FStream: TChainedStream;
    procedure ClearStreams;
  Public
    Function CreateStream(aOffset,aCount : Word) : TStream;
    Procedure CreateChainedStream(Sizes : Array of Word; aOffset : Word = 0);
    Procedure Setup; override;
    Procedure TearDown; override;
    Property Stream : TChainedStream Read FStream;
  Published
    Procedure TestEmpty;
    Procedure TestCreateStream;
    Procedure TestCreateStreams;
    Procedure TestCreateStreams2;
    Procedure TestOneStreamRead;
    Procedure TestTwoStreamsRead;
    Procedure TestTwoStreamsReadCrossBuffer;
    Procedure TestTwoStreamsWrite;
    Procedure TestTwoStreamsWriteCrossBuffer;
    Procedure TestStreamsSeekFromEnd1;
    Procedure TestStreamsSeekFromEnd2;
    Procedure TestStreamsSeekFromBeginning1;
    Procedure TestStreamsSeekFromBeginning2;
    Procedure TestStreamsSeekFromCurrent1;
    Procedure TestStreamsSeekFromCurrent2;
  end;

implementation

{ TTestChainStream }

function TTestChainStream.CreateStream(aOffset, aCount: Word): TStream;

Var
  I : integer;

begin
  Result:=TMemoryStream.Create;
  For I:=0 to aCount-1 do
    begin
    Result.WriteBuffer(aOffset,SizeOf(Word));
    Inc(aOffset);
    end;
  Result.Position:=0;
end;

procedure TTestChainStream.CreateChainedStream(Sizes: array of Word; aOffset : Word = 0);

Var
  I : integer;
  aSize : Word;

begin
  ClearStreams;
  SetLength(FStreams,Length(Sizes));
  For I:=0 to Length(FStreams)-1 do
    begin
    aSize:=Sizes[i];
    FStreams[i]:=CreateStream(aOffset,aSize);
    Inc(aOffset,aSize);
    end;
  FStream:=TChainedStream.Create(FStreams,False);
end;

procedure TTestChainStream.Setup;
begin

  inherited Setup;
end;

procedure TTestChainStream.ClearStreams;

var
  I : Integer;

begin
  if Assigned(FStream) then
    begin
    if FStream.OwnsStreams then
      FStreams:=[];
    FStream.Free;
    end;
  For I:=0 to Length(FStreams)-1 do
    FStreams[i].Free;
end;

procedure TTestChainStream.TearDown;
begin
  ClearStreams;
  inherited TearDown;
end;

procedure TTestChainStream.TestEmpty;
begin
  AssertNull('No stream',FStream);
  AssertEquals('No streams',0,Length(FStreams));
end;

procedure TTestChainStream.TestCreateStream;

var
  S : TStream;
  I,W : Word;

begin
  S:=CreateStream(10,3);
  try
    AssertEquals('Stream position',0,S.Position);
    AssertEquals('Stream size',6,S.Size);
    For I:=10 to 12 do
      begin
      S.ReadBuffer(W,SizeOf(Word));
      AssertEquals('Correct byte read',I,W);
      end;
  finally
    S.Free;
  end;
end;

procedure TTestChainStream.TestCreateStreams;

begin
  CreateChainedStream([10]);
  AssertEquals('Correct stream count',1,Length(FStreams));
  AssertEquals('Count',1,Stream.StreamCount);
  AssertSame('Stream',FStreams[0],Stream.Streams[0]);
  AssertEquals('Total size',20,Stream.Size);
end;

procedure TTestChainStream.TestCreateStreams2;
begin
  CreateChainedStream([10,10]);
  AssertEquals('Correct stream count',2,Length(FStreams));
  AssertEquals('Count',2,Stream.StreamCount);
  AssertSame('Stream 0',FStreams[0],Stream.Streams[0]);
  AssertSame('Stream 1',FStreams[1],Stream.Streams[1]);
  AssertEquals('Total size',40,Stream.Size);
end;

procedure TTestChainStream.TestOneStreamRead;

Var
  I : Integer;
  W : Word;

begin
  CreateChainedStream([10]);
  For I:=0 to 9 do
    begin
    Stream.ReadBuffer(W,SizeOf(W));
    AssertEquals('Correct bytes read',I,W)
    end;
end;

procedure TTestChainStream.TestTwoStreamsRead;

Var
  I : Integer;
  W : Word;

begin
  CreateChainedStream([10,10]);
  For I:=0 to 19 do
    begin
    Stream.ReadBuffer(W,SizeOf(W));
    AssertEquals('Correct bytes read',I,W)
    end;
end;

procedure TTestChainStream.TestTwoStreamsReadCrossBuffer;
Var
  I : Integer;
  W : Array of Word;

begin
  SetLength(W,20);
  CreateChainedStream([10,10]);
  Stream.ReadBuffer(W[0],Length(W)*SizeOf(Word));
  For I:=0 to 19 do
    begin
    AssertEquals('Correct bytes read',I,W[i])
    end;
end;

procedure TTestChainStream.TestTwoStreamsWrite;

Var
  I : Integer;
  W : Word;

begin
  CreateChainedStream([10,10]);
  For I:=100 to 119 do
    begin
    W:=I;
    Stream.WriteBuffer(W,SizeOf(W));
    end;
  Stream.Position:=0;
  For I:=100 to 119 do
    begin
    Stream.ReadBuffer(W,SizeOf(W));
    AssertEquals('Correct bytes read',I,W)
    end;
end;

procedure TTestChainStream.TestTwoStreamsWriteCrossBuffer;
Var
  I : Integer;
  W : Array of Word;
  WW : Word;

begin
  SetLength(W,20);
  For I:=0 to 19 do
    W[i]:=I;
  CreateChainedStream([10,10]);
  Stream.WriteBuffer(W[0],Length(W)*SizeOf(Word));
  FStreams[0].Position:=0;
  For I:=0 to 9 do
    begin
    FStreams[0].ReadBuffer(WW,SizeOf(WW));
    AssertEquals('Correct bytes read',I,WW)
    end;
  FStreams[1].Position:=0;
  For I:=10 to 19 do
    begin
    FStreams[1].ReadBuffer(WW,SizeOf(WW));
    AssertEquals('Correct bytes read',I,WW)
    end;
end;

procedure TTestChainStream.TestStreamsSeekFromEnd1;

Var
  W : Word;

begin
  CreateChainedStream([10,10]);
  AssertEquals('Seek',38,Stream.Seek(-2,soEnd));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read',19,W);
  AssertEquals('Seek 2',20,Stream.Seek(-20,soEnd));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read 2',10,W);
  AssertEquals('Seek 3',18,Stream.Seek(-22,soEnd));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read 2',9,W);
end;

procedure TTestChainStream.TestStreamsSeekFromEnd2;

Var
  W : Word;

begin
  CreateChainedStream([10,10,10]);
  AssertEquals('Seek',0,Stream.Seek(-60,soEnd));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read ',0,W);
  AssertEquals('Seek',30,Stream.Seek(-30,soEnd));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read ',15,W);
end;

procedure TTestChainStream.TestStreamsSeekFromBeginning1;

Var
  W : Word;

begin
  CreateChainedStream([10,10]);
  AssertEquals('Seek',38,Stream.Seek(38,soBeginning));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read',19,W);
  AssertEquals('Seek 2',20,Stream.Seek(20,soBeginning));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read 2',10,W);
  AssertEquals('Seek 3',18,Stream.Seek(18,soBeginning));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read 2',9,W);
end;

procedure TTestChainStream.TestStreamsSeekFromBeginning2;

Var
  W : Word;

begin
  CreateChainedStream([10,10,10]);
  AssertEquals('Seek',0,Stream.Seek(0,soBeginning));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read ',0,W);
  AssertEquals('Seek',30,Stream.Seek(30,soBeginning));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read ',15,W);
  AssertEquals('Seek',50,Stream.Seek(50,soBeginning));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read ',25,W);
end;

procedure TTestChainStream.TestStreamsSeekFromCurrent1;
Var
  W : Word;

begin
  CreateChainedStream([10,10]);
  AssertEquals('Seek',4,Stream.Seek(4,soCurrent));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read 1',2,W);
  AssertEquals('Position after read 1',6,Stream.Position);
  AssertEquals('Seek 2',26,Stream.Seek(20,soCurrent));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read 2',13,W);
  AssertEquals('Position after read 2',28,Stream.Position);
  AssertEquals('Seek 3',32,Stream.Seek(4,soCurrent));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read 3',16,W);
  AssertEquals('Position after read 2',34,Stream.Position);
end;

procedure TTestChainStream.TestStreamsSeekFromCurrent2;
Var
  W : Word;
  I : Integer;

begin
  CreateChainedStream([10,10]);
  For I:=1 to 15 do
    Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Position after reading data',30,Stream.Position);
  AssertEquals('Seek',26,Stream.Seek(-4,soCurrent));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read 1',13,W);
  AssertEquals('Position after read 1',28,Stream.Position);
  AssertEquals('Seek 2',8,Stream.Seek(-20,soCurrent));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read 2',4,W);
  AssertEquals('Position after read 2',10,Stream.Position);
  AssertEquals('Seek 3',6,Stream.Seek(-4,soCurrent));
  Stream.ReadBuffer(W,SizeOf(Word));
  AssertEquals('Correct read 3',3,W);
  AssertEquals('Position after read 2',8,Stream.Position);
end;

initialization
  RegisterTest(TTestChainStream);
end.

