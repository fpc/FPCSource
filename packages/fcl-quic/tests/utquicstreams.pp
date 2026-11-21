{
  This file is part of the Free Component Library.
  Tests for FpQuic.Streams.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit utquicstreams;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry, FpQuic.Consts, FpQuic.Buffers, FpQuic.TransportParams, FpQuic.Streams;

type
  TTestQuicReassembly = class(TTestCase)
  private
    FBuffer: TQuicReassembly;
    function ReadString: String;
    procedure ExpectFinalSizeError(aOffset: QWord; const aData: String; aFin: Boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInOrder;
    procedure TestOutOfOrder;
    procedure TestOverlapAndDuplicate;
    procedure TestFin;
    procedure TestFinalSizeErrors;
    procedure TestSetFinalSize;
    procedure TestPartialRead;
    procedure TestDuplicatesStoredOnce;
    procedure TestTooManyRanges;
  end;

  TTestQuicSendBuffer = class(TTestCase)
  private
    FBuffer: TQuicSendBuffer;
    function Chunk(aMaxLength: Integer; aMaxOffset: QWord; out aOffset: QWord; out aFin: Boolean): String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNewData;
    procedure TestFlowControlLimit;
    procedure TestFinWithData;
    procedure TestFinOnly;
    procedure TestRetransmitLost;
    procedure TestLostFinOnly;
    procedure TestLostDataHasFin;
    procedure TestAcknowledgements;
    procedure TestLostBelowBase;
  end;

  TTestQuicStream = class(TTestCase)
  published
    procedure TestReceiveFlowControl;
    procedure TestReset;
    procedure TestReadWindowUpdate;
    procedure TestLocalResetAndStop;
    procedure TestIsClosed;
  end;

  TTestQuicStreamMap = class(TTestCase)
  private
    function Params(aBidi, aUni: QWord): TQuicTransportParams;
  published
    procedure TestOpenLocalClient;
    procedure TestOpenLocalServer;
    procedure TestInitialLimits;
    procedure TestRemoteStreams;
    procedure TestFrameErrors;
    procedure TestRemoveClosed;
  end;

implementation

function Bytes(const aText: String): TBytes;

begin
  Result:=TEncoding.UTF8.GetBytes(aText);
end;


{ TTestQuicReassembly }

procedure TTestQuicReassembly.SetUp;

begin
  FBuffer:=TQuicReassembly.Create;
end;


procedure TTestQuicReassembly.TearDown;

begin
  FBuffer.Free;
end;


function TTestQuicReassembly.ReadString: String;

begin
  Result:=TEncoding.UTF8.GetString(FBuffer.ReadAll);
end;


procedure TTestQuicReassembly.ExpectFinalSizeError(aOffset: QWord; const aData: String; aFin: Boolean);

begin
  try
    FBuffer.Insert(aOffset,Bytes(aData),aFin);
    Fail('Expected FINAL_SIZE_ERROR');
  except
    on E: EQuicError do
      AssertEquals('Error code',QUIC_FINAL_SIZE_ERROR,E.ErrorCode);
  end;
end;


procedure TTestQuicReassembly.TestInOrder;

begin
  FBuffer.Insert(0,Bytes('abc'),False);
  FBuffer.Insert(3,Bytes('def'),False);
  AssertEquals('Available',6,FBuffer.Available);
  AssertEquals('Data','abcdef',ReadString);
  AssertEquals('Read offset',6,FBuffer.ReadOffset);
  AssertEquals('Highest',6,FBuffer.HighestOffset);
  AssertFalse('Not finished',FBuffer.IsFinished);
end;


procedure TTestQuicReassembly.TestOutOfOrder;

begin
  FBuffer.Insert(6,Bytes('ghi'),False);
  FBuffer.Insert(3,Bytes('def'),False);
  AssertEquals('Nothing in order yet',0,FBuffer.Available);
  FBuffer.Insert(0,Bytes('abc'),False);
  AssertEquals('Merged','abcdefghi',ReadString);
end;


procedure TTestQuicReassembly.TestOverlapAndDuplicate;

begin
  FBuffer.Insert(0,Bytes('abcd'),False);
  FBuffer.Insert(2,Bytes('cdef'),False);
  FBuffer.Insert(0,Bytes('ab'),False);
  FBuffer.Insert(8,Bytes('ij'),False);
  FBuffer.Insert(7,Bytes('hij'),False);
  FBuffer.Insert(5,Bytes('fgh'),False);
  AssertEquals('Overlaps merged','abcdefghij',ReadString);
  FBuffer.Insert(3,Bytes('d'),False);
  AssertEquals('Old duplicate ignored',0,FBuffer.Available);
  FBuffer.Insert(10,nil,False);
  AssertEquals('Empty frame',0,FBuffer.Available);
end;


procedure TTestQuicReassembly.TestFin;

begin
  FBuffer.Insert(3,Bytes('def'),True);
  AssertTrue('Final size known',FBuffer.HasFinalSize);
  AssertEquals('Final size',6,FBuffer.FinalSize);
  FBuffer.Insert(0,Bytes('abc'),False);
  AssertEquals('Data','abcdef',ReadString);
  AssertTrue('Finished',FBuffer.IsFinished);
  FBuffer.Insert(0,Bytes('abcdef'),True);
end;


procedure TTestQuicReassembly.TestFinalSizeErrors;

begin
  FBuffer.Insert(0,Bytes('abcdef'),False);
  ExpectFinalSizeError(0,'abc',True);
  FBuffer.Insert(6,nil,True);
  ExpectFinalSizeError(6,'x',False);
  ExpectFinalSizeError(0,'abcde',True);
end;


procedure TTestQuicReassembly.TestSetFinalSize;

begin
  FBuffer.Insert(0,Bytes('ab'),False);
  FBuffer.SetFinalSize(10);
  AssertEquals('Final size',10,FBuffer.FinalSize);
  AssertEquals('Highest',10,FBuffer.HighestOffset);
  try
    FBuffer.SetFinalSize(11);
    Fail('Changing the final size must fail');
  except
    on E: EQuicError do
      AssertEquals('Error code',QUIC_FINAL_SIZE_ERROR,E.ErrorCode);
  end;
end;


procedure TTestQuicReassembly.TestPartialRead;

var
  lBuf: array[0..9] of Byte;

begin
  lBuf[0]:=0;
  AssertEquals('Read nothing',0,FBuffer.Read(@lBuf[0],10));
  FBuffer.Insert(0,Bytes('abcdef'),False);
  AssertEquals('Read limited',4,FBuffer.Read(@lBuf[0],4));
  AssertEquals('First byte',Ord('a'),lBuf[0]);
  AssertEquals('Rest','ef',ReadString);
  AssertEquals('Read zero count',0,FBuffer.Read(@lBuf[0],0));
end;


procedure TTestQuicReassembly.TestDuplicatesStoredOnce;

var
  I: Integer;

begin
  for I:=1 to 1000 do
    FBuffer.Insert(100,Bytes('0123456789'),False);
  AssertEquals('Duplicates use no extra memory',110,FBuffer.Buffered);
  AssertEquals('Gap blocks reading',0,FBuffer.Available);
  FBuffer.Insert(0,Bytes(StringOfChar('x',100)),False);
  AssertEquals('Gap filled',110,FBuffer.Available);
  AssertEquals('Data after the gap','0123456789',Copy(ReadString,101,10));
  AssertEquals('Buffer empty after reading',0,FBuffer.Buffered);
  FBuffer.Insert(105,Bytes('56789abc'),False);
  AssertEquals('Overlap with read data is trimmed','abc',ReadString);
end;


procedure TTestQuicReassembly.TestTooManyRanges;

var
  lOld: Integer;

begin
  lOld:=QuicMaxReceiveRanges;
  QuicMaxReceiveRanges:=3;
  try
    FBuffer.Insert(2,Bytes('a'),False);
    FBuffer.Insert(4,Bytes('b'),False);
    FBuffer.Insert(6,Bytes('c'),False);
    FBuffer.Insert(4,Bytes('b'),False);
    try
      FBuffer.Insert(8,Bytes('d'),False);
      Fail('A fourth range must fail');
    except
      on E: EQuicError do
        AssertEquals('Error code',QUIC_INTERNAL_ERROR,E.ErrorCode);
    end;
  finally
    QuicMaxReceiveRanges:=lOld;
  end;
end;


{ TTestQuicSendBuffer }

procedure TTestQuicSendBuffer.SetUp;

begin
  FBuffer:=TQuicSendBuffer.Create;
end;


procedure TTestQuicSendBuffer.TearDown;

begin
  FBuffer.Free;
end;


function TTestQuicSendBuffer.Chunk(aMaxLength: Integer; aMaxOffset: QWord; out aOffset: QWord;
  out aFin: Boolean): String;

var
  lData: TBytes;

begin
  if FBuffer.NextChunk(aMaxLength,aMaxOffset,aOffset,lData,aFin) then
    Result:=TEncoding.UTF8.GetString(lData)
  else
    Result:='<none>';
end;


procedure TTestQuicSendBuffer.TestNewData;

var
  lOffset: QWord;
  lFin: Boolean;

begin
  AssertFalse('Nothing pending',FBuffer.HasPending);
  AssertEquals('Empty','<none>',Chunk(100,1000,lOffset,lFin));
  FBuffer.Write(Bytes('hello world'));
  AssertTrue('Pending',FBuffer.HasPending);
  AssertEquals('Zero length','<none>',Chunk(0,1000,lOffset,lFin));
  AssertEquals('First chunk','hello',Chunk(5,1000,lOffset,lFin));
  AssertEquals('First offset',0,lOffset);
  AssertEquals('Second chunk',' world',Chunk(100,1000,lOffset,lFin));
  AssertEquals('Second offset',5,lOffset);
  AssertFalse('No fin',lFin);
  AssertEquals('Next offset',11,FBuffer.NextOffset);
  AssertEquals('Buffered',11,FBuffer.Buffered);
  AssertFalse('All sent',FBuffer.HasPending);
end;


procedure TTestQuicSendBuffer.TestFlowControlLimit;

var
  lOffset: QWord;
  lFin: Boolean;

begin
  FBuffer.Write(Bytes('abcdef'));
  FBuffer.Finish;
  AssertEquals('Limited by flow control','abcd',Chunk(100,4,lOffset,lFin));
  AssertFalse('No fin while blocked',lFin);
  AssertEquals('Blocked','<none>',Chunk(100,4,lOffset,lFin));
  AssertTrue('Still pending',FBuffer.HasPending);
  AssertEquals('Rest with fin','ef',Chunk(100,6,lOffset,lFin));
  AssertTrue('Fin',lFin);
end;


procedure TTestQuicSendBuffer.TestFinWithData;

var
  lOffset: QWord;
  lFin: Boolean;

begin
  FBuffer.Write(Bytes('abc'));
  FBuffer.Finish;
  AssertTrue('Finished',FBuffer.Finished);
  AssertEquals('Data','abc',Chunk(100,100,lOffset,lFin));
  AssertTrue('Fin with data',lFin);
  AssertFalse('Nothing more',FBuffer.HasPending);
  FBuffer.OnAcked(0,3,True);
  AssertTrue('All acked',FBuffer.AllAcked);
end;


procedure TTestQuicSendBuffer.TestFinOnly;

var
  lOffset: QWord;
  lFin: Boolean;

begin
  FBuffer.Write(Bytes('abc'));
  AssertEquals('Data','abc',Chunk(100,100,lOffset,lFin));
  FBuffer.Finish;
  AssertTrue('Fin pending',FBuffer.HasPending);
  AssertEquals('Fin only','',Chunk(100,100,lOffset,lFin));
  AssertTrue('Fin',lFin);
  AssertEquals('Fin offset',3,lOffset);
end;


procedure TTestQuicSendBuffer.TestRetransmitLost;

var
  lOffset: QWord;
  lFin: Boolean;

begin
  FBuffer.Write(Bytes('0123456789'));
  Chunk(4,100,lOffset,lFin);
  Chunk(4,100,lOffset,lFin);
  FBuffer.OnLost(0,4,False);
  FBuffer.OnAcked(4,4,False);
  AssertTrue('Lost pending',FBuffer.HasPending);
  AssertEquals('Lost part 1','01',Chunk(2,100,lOffset,lFin));
  AssertEquals('Lost part 1 offset',0,lOffset);
  AssertEquals('Lost part 2','23',Chunk(10,100,lOffset,lFin));
  AssertEquals('Then new data','89',Chunk(10,100,lOffset,lFin));
  FBuffer.OnLost(4,4,False);
  AssertEquals('Acked data not resent','<none>',Chunk(10,100,lOffset,lFin));
end;


procedure TTestQuicSendBuffer.TestLostFinOnly;

var
  lOffset: QWord;
  lFin: Boolean;

begin
  FBuffer.Write(Bytes('ab'));
  FBuffer.Finish;
  Chunk(10,100,lOffset,lFin);
  FBuffer.OnAcked(0,2,False);
  FBuffer.OnLost(0,2,True);
  AssertTrue('Fin lost',FBuffer.HasPending);
  AssertEquals('Fin resent alone','',Chunk(10,100,lOffset,lFin));
  AssertTrue('Fin',lFin);
  AssertEquals('Fin offset',2,lOffset);
  FBuffer.OnAcked(2,0,True);
  AssertTrue('All acked',FBuffer.AllAcked);
  FBuffer.OnLost(2,0,True);
  AssertFalse('Acked fin not resent',FBuffer.HasPending);
end;


procedure TTestQuicSendBuffer.TestLostDataHasFin;

var
  lOffset: QWord;
  lFin: Boolean;

begin
  FBuffer.Write(Bytes('abcd'));
  FBuffer.Finish;
  Chunk(10,100,lOffset,lFin);
  FBuffer.OnLost(0,4,True);
  AssertEquals('Retransmission','abcd',Chunk(10,100,lOffset,lFin));
  AssertTrue('Fin in retransmission',lFin);
  AssertFalse('No separate fin',FBuffer.HasPending);
end;


procedure TTestQuicSendBuffer.TestAcknowledgements;

var
  lOffset: QWord;
  lFin: Boolean;

begin
  FBuffer.Write(Bytes('0123456789'));
  Chunk(10,100,lOffset,lFin);
  FBuffer.OnAcked(5,3,False);
  AssertEquals('Gap keeps data',10,FBuffer.Buffered);
  FBuffer.OnAcked(0,5,False);
  AssertEquals('Prefix freed',2,FBuffer.Buffered);
  FBuffer.OnAcked(0,9,False);
  AssertEquals('Overlapping ack',1,FBuffer.Buffered);
  FBuffer.OnAcked(0,2,False);
  AssertEquals('Old ack',1,FBuffer.Buffered);
  AssertFalse('No fin yet',FBuffer.AllAcked);
  AssertEquals('End offset',10,FBuffer.EndOffset);
end;


procedure TTestQuicSendBuffer.TestLostBelowBase;

var
  lOffset: QWord;
  lFin: Boolean;

begin
  FBuffer.Write(Bytes('0123456789'));
  Chunk(10,100,lOffset,lFin);
  FBuffer.OnAcked(0,4,False);
  FBuffer.OnLost(0,2,False);
  AssertFalse('Fully acked range ignored',FBuffer.HasPending);
  FBuffer.OnLost(0,0,False);
  FBuffer.OnLost(2,4,False);
  AssertEquals('Only unacked part resent','45',Chunk(10,100,lOffset,lFin));
  AssertEquals('Offset',4,lOffset);
  FBuffer.OnLost(6,2,False);
  FBuffer.OnAcked(0,8,False);
  AssertEquals('Lost range acked meanwhile','<none>',Chunk(10,100,lOffset,lFin));
end;


{ TTestQuicStream }

procedure TTestQuicStream.TestReceiveFlowControl;

var
  lStream: TQuicStream;

begin
  lStream:=TQuicStream.Create(0,True,True,100,10);
  try
    AssertEquals('Increase',4,lStream.OnData(0,Bytes('abcd'),False));
    AssertEquals('No increase for duplicate',0,lStream.OnData(0,Bytes('ab'),False));
    try
      lStream.OnData(8,Bytes('abc'),False);
      Fail('Beyond the limit');
    except
      on E: EQuicError do
        AssertEquals('Flow control error',QUIC_FLOW_CONTROL_ERROR,E.ErrorCode);
    end;
  finally
    lStream.Free;
  end;
end;


procedure TTestQuicStream.TestReset;

var
  lStream: TQuicStream;

begin
  lStream:=TQuicStream.Create(0,True,True,100,10);
  try
    lStream.OnData(0,Bytes('ab'),False);
    AssertEquals('Reset increase',3,lStream.OnReset(7,5));
    AssertTrue('Reset received',lStream.ResetReceived);
    AssertEquals('Reset code',7,lStream.ResetErrorCode);
    AssertEquals('Late data within final size',0,lStream.OnData(2,Bytes('cde'),False));
    try
      lStream.OnData(4,Bytes('xy'),False);
      Fail('Data beyond the final size');
    except
      on E: EQuicError do
        AssertEquals('Final size error',QUIC_FINAL_SIZE_ERROR,E.ErrorCode);
    end;
    try
      lStream.OnReset(7,11);
      Fail('Final size beyond the limit');
    except
      on E: EQuicError do
        AssertEquals('Flow control error',QUIC_FLOW_CONTROL_ERROR,E.ErrorCode);
    end;
  finally
    lStream.Free;
  end;
end;


procedure TTestQuicStream.TestReadWindowUpdate;

var
  lStream: TQuicStream;
  lBuf: array[0..15] of Byte;

begin
  lBuf[0]:=0;
  lStream:=TQuicStream.Create(0,True,True,100,10);
  try
    lStream.OnData(0,Bytes('abcd'),False);
    AssertEquals('Read',4,lStream.Read(@lBuf[0],16));
    AssertFalse('Window still large',lStream.MaxStreamDataPending);
    lStream.OnData(4,Bytes('ef'),False);
    lStream.Read(@lBuf[0],16);
    AssertTrue('Window update due',lStream.MaxStreamDataPending);
    AssertEquals('New limit',16,lStream.RecvMaxData);
    lStream.MaxStreamDataPending:=False;
    lStream.OnData(6,Bytes('ghijkl'),True);
    lStream.Read(@lBuf[0],16);
    AssertFalse('No update after the final size',lStream.MaxStreamDataPending);
  finally
    lStream.Free;
  end;
end;


procedure TTestQuicStream.TestLocalResetAndStop;

var
  lStream: TQuicStream;

begin
  lStream:=TQuicStream.Create(0,True,True,100,10);
  try
    lStream.Send.Write(Bytes('abc'));
    AssertTrue('Wants to send',lStream.WantsToSend);
    lStream.Reset(5);
    AssertTrue('Reset pending',lStream.ResetPending);
    AssertEquals('Reset code',5,lStream.LocalResetCode);
    AssertFalse('Reset stream sends no data',lStream.WantsToSend);
    lStream.ResetPending:=False;
    lStream.Reset(6);
    AssertFalse('Second reset ignored',lStream.ResetPending);
    lStream.StopSending(9);
    AssertTrue('Stop pending',lStream.StopSendingPending);
    AssertEquals('Stop code',9,lStream.LocalStopCode);
    lStream.StopSendingPending:=False;
    lStream.StopSending(10);
    AssertFalse('Second stop ignored',lStream.StopSendingPending);
  finally
    lStream.Free;
  end;
end;


procedure TTestQuicStream.TestIsClosed;

var
  lStream: TQuicStream;
  lOffset: QWord;
  lData: TBytes;
  lFin: Boolean;

begin
  lStream:=TQuicStream.Create(2,True,False,100,10);
  try
    AssertTrue('Send only',lStream.CanSend and not lStream.CanReceive);
    AssertEquals('Id',2,lStream.Id);
    lStream.Send.Finish;
    lStream.Send.NextChunk(10,100,lOffset,lData,lFin);
    AssertFalse('Fin not acked',lStream.IsClosed);
    lStream.Send.OnAcked(0,0,True);
    AssertTrue('Closed',lStream.IsClosed);
    lStream.Reset(1);
    AssertFalse('No reset after completion',lStream.LocalReset);
  finally
    lStream.Free;
  end;
  lStream:=TQuicStream.Create(3,False,True,100,10);
  try
    lStream.OnReset(0,0);
    AssertFalse('Open until the reset is delivered',lStream.IsClosed);
    lStream.ResetDelivered:=True;
    AssertTrue('Closed by reset',lStream.IsClosed);
    lStream.StopSending(1);
    AssertFalse('No stop after reset',lStream.LocalStopSending);
  finally
    lStream.Free;
  end;
end;


{ TTestQuicStreamMap }

function TTestQuicStreamMap.Params(aBidi, aUni: QWord): TQuicTransportParams;

begin
  Result:=QuicDefaultTransportParams;
  Result.InitialMaxStreamsBidi:=aBidi;
  Result.InitialMaxStreamsUni:=aUni;
  Result.InitialMaxStreamDataBidiLocal:=100;
  Result.InitialMaxStreamDataBidiRemote:=200;
  Result.InitialMaxStreamDataUni:=300;
end;


procedure TTestQuicStreamMap.TestOpenLocalClient;

var
  lMap: TQuicStreamMap;

begin
  lMap:=TQuicStreamMap.Create(False,Params(10,10));
  try
    AssertNull('No peer limit yet',lMap.OpenLocal(True));
    lMap.SetPeerParams(Params(2,1));
    AssertEquals('Peer bidi limit',2,lMap.PeerMaxStreams(True));
    AssertEquals('First bidi',0,lMap.OpenLocal(True).Id);
    AssertEquals('Second bidi',4,lMap.OpenLocal(True).Id);
    AssertNull('Bidi limit',lMap.OpenLocal(True));
    AssertEquals('First uni',2,lMap.OpenLocal(False).Id);
    AssertNull('Uni limit',lMap.OpenLocal(False));
    lMap.OnMaxStreams(False,3);
    lMap.OnMaxStreams(False,2);
    AssertEquals('Raised uni limit',3,lMap.PeerMaxStreams(False));
    AssertEquals('Second uni',6,lMap.OpenLocal(False).Id);
    AssertEquals('Count',4,lMap.Count);
    AssertEquals('Find',4,lMap.Find(4).Id);
    AssertNull('Find missing',lMap.Find(8));
  finally
    lMap.Free;
  end;
end;


procedure TTestQuicStreamMap.TestOpenLocalServer;

var
  lMap: TQuicStreamMap;

begin
  lMap:=TQuicStreamMap.Create(True,Params(10,10));
  try
    lMap.SetPeerParams(Params(1,1));
    AssertEquals('Server bidi',1,lMap.OpenLocal(True).Id);
    AssertEquals('Server uni',3,lMap.OpenLocal(False).Id);
  finally
    lMap.Free;
  end;
end;


procedure TTestQuicStreamMap.TestInitialLimits;

var
  lMap: TQuicStreamMap;
  lStream: TQuicStream;
  lPeer: TQuicTransportParams;

begin
  lMap:=TQuicStreamMap.Create(False,Params(10,10));
  try
    lPeer:=Params(10,10);
    lPeer.InitialMaxStreamDataBidiLocal:=1000;
    lPeer.InitialMaxStreamDataBidiRemote:=2000;
    lPeer.InitialMaxStreamDataUni:=3000;
    lMap.SetPeerParams(lPeer);
    lStream:=lMap.OpenLocal(True);
    AssertEquals('Local bidi send',2000,lStream.SendMaxData);
    AssertEquals('Local bidi recv',100,lStream.RecvMaxData);
    lStream:=lMap.OpenLocal(False);
    AssertEquals('Local uni send',3000,lStream.SendMaxData);
    AssertFalse('Local uni cannot receive',lStream.CanReceive);
    lStream:=lMap.GetForFrame(1,False);
    AssertEquals('Remote bidi send',1000,lStream.SendMaxData);
    AssertEquals('Remote bidi recv',200,lStream.RecvMaxData);
    lStream:=lMap.GetForFrame(3,False);
    AssertEquals('Remote uni recv',300,lStream.RecvMaxData);
    AssertFalse('Remote uni cannot send',lStream.CanSend);
  finally
    lMap.Free;
  end;
end;


procedure TTestQuicStreamMap.TestRemoteStreams;

var
  lMap: TQuicStreamMap;
  lId: QWord;

begin
  lMap:=TQuicStreamMap.Create(True,Params(10,10));
  try
    AssertEquals('Limit',10,lMap.LocalMaxStreams(True));
    AssertFalse('Nothing to accept',lMap.Accept(lId));
    AssertEquals('Implicit open',8,lMap.GetForFrame(8,False).Id);
    AssertEquals('Three streams',3,lMap.Count);
    AssertTrue('Accept 0',lMap.Accept(lId));
    AssertEquals('First accepted',0,lId);
    AssertTrue('Accept 4',lMap.Accept(lId));
    AssertEquals('Second accepted',4,lId);
    AssertTrue('Accept 8',lMap.Accept(lId));
    AssertEquals('Existing stream',4,lMap.GetForFrame(4,True).Id);
  finally
    lMap.Free;
  end;
end;


procedure TTestQuicStreamMap.TestFrameErrors;

var
  lMap: TQuicStreamMap;

  procedure Expect(aId: QWord; aSending: Boolean; aCode: QWord);

  begin
    try
      lMap.GetForFrame(aId,aSending);
      Fail('Expected error for stream '+IntToStr(aId));
    except
      on E: EQuicError do
        AssertEquals('Error code for stream '+IntToStr(aId),aCode,E.ErrorCode);
    end;
  end;

begin
  lMap:=TQuicStreamMap.Create(False,Params(2,2));
  try
    lMap.SetPeerParams(Params(10,10));
    lMap.OpenLocal(False);
    Expect(2,False,QUIC_STREAM_STATE_ERROR);
    Expect(3,True,QUIC_STREAM_STATE_ERROR);
    Expect(0,False,QUIC_STREAM_STATE_ERROR);
    Expect(9,False,QUIC_STREAM_LIMIT_ERROR);
    AssertNotNull('Local uni accepts MAX_STREAM_DATA',lMap.GetForFrame(2,True));
  finally
    lMap.Free;
  end;
end;


procedure TTestQuicStreamMap.TestRemoveClosed;

var
  lMap: TQuicStreamMap;
  lStream: TQuicStream;

begin
  lMap:=TQuicStreamMap.Create(False,Params(1,1));
  try
    lMap.SetPeerParams(Params(1,1));
    lStream:=lMap.GetForFrame(3,False);
    lStream.OnData(0,Bytes('x'),True);
    lStream:=lMap.GetForFrame(1,False);
    lStream.OnReset(0,0);
    lStream.ResetDelivered:=True;
    lStream.Reset(0);
    lStream.ResetAcked:=True;
    lStream:=lMap.OpenLocal(True);
    lMap.RemoveClosed;
    AssertEquals('Uni stream not read yet',2,lMap.Count);
    lMap.Find(3).Recv.ReadAll;
    lMap.RemoveClosed;
    AssertEquals('Only the open local stream remains',1,lMap.Count);
    AssertEquals('Remaining stream',0,lMap[0].Id);
    AssertTrue('Bidi credit',lMap.MaxStreamsPending[True]);
    AssertTrue('Uni credit',lMap.MaxStreamsPending[False]);
    AssertEquals('New bidi limit',2,lMap.LocalMaxStreams(True));
    AssertNull('Closed stream',lMap.GetForFrame(1,False));
  finally
    lMap.Free;
  end;
end;


initialization
  RegisterTests([TTestQuicReassembly,TTestQuicSendBuffer,TTestQuicStream,TTestQuicStreamMap]);
end.
