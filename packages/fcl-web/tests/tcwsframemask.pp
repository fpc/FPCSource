{
  Tests for the masks of outgoing WebSocket frames.
}
unit tcwsframemask;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpwebsocket;

type
  // Transport that keeps every written buffer and reads nothing.
  TRecordingTransport = class(TObject, IWSTransport)
  private
    FWrites: array of TBytes;
    function GetWrite(aIndex: Integer): TBytes;
    function GetWriteCount: Integer;
  public
    // Return False: there is no incoming data.
    function CanRead(aTimeOut: Integer): Boolean;
    // Raise an exception: there is no incoming data.
    procedure ReadBuffer(aBytes: TBytes);
    // Return 0: there is no incoming data.
    function ReadBytes(var aBytes: TBytes; aCount: Integer): Integer;
    // Record the first aCount bytes of aBytes.
    function WriteBytes(aBytes: TBytes; aCount: Integer): Integer;
    // Record aBytes.
    procedure WriteBuffer(aBytes: TBytes);
    // Return an empty line.
    function ReadLn: String;
    // Return 127.0.0.1.
    function PeerIP: string;
    // Return 0.
    function PeerPort: word;
    // Buffers written so far, oldest first.
    property Writes[aIndex: Integer]: TBytes read GetWrite;
    // Number of buffers written.
    property WriteCount: Integer read GetWriteCount;
  end;

  // Client connection that writes to a TRecordingTransport.
  TRecordingConnection = class(TWSClientConnection)
  private
    FRecorder: TRecordingTransport;
  protected
    function GetTransport: IWSTransport; override;
    procedure DoDisconnect; override;
  public
    // Create a connection with its own recording transport.
    constructor Create; reintroduce;
    destructor Destroy; override;
    // Transport that holds the written frames.
    property Recorder: TRecordingTransport read FRecorder;
  end;

  TTestWSFrameMask = class(TTestCase)
  private
    FConnection: TRecordingConnection;
    function FrameMask(aIndex: Integer): Cardinal;
    function FramePayload(aIndex: Integer): String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUnmaskedWhenMaskIsZero;
    procedure TestMaskedWhenMaskIsSet;
    procedure TestEachFrameHasNewMask;
    procedure TestConfiguredMaskValueNotUsed;
    procedure TestControlFramesHaveNewMask;
    procedure TestNextFrameMask;
  end;

implementation

function TRecordingTransport.GetWrite(aIndex: Integer): TBytes;

begin
  Result:=FWrites[aIndex];
end;


function TRecordingTransport.GetWriteCount: Integer;

begin
  Result:=Length(FWrites);
end;


function TRecordingTransport.CanRead(aTimeOut: Integer): Boolean;

begin
  Result:=False;
end;


procedure TRecordingTransport.ReadBuffer(aBytes: TBytes);

begin
  raise Exception.Create('No incoming data');
end;


function TRecordingTransport.ReadBytes(var aBytes: TBytes; aCount: Integer): Integer;

begin
  Result:=0;
end;


function TRecordingTransport.WriteBytes(aBytes: TBytes; aCount: Integer): Integer;

begin
  WriteBuffer(Copy(aBytes,0,aCount));
  Result:=aCount;
end;


procedure TRecordingTransport.WriteBuffer(aBytes: TBytes);

begin
  SetLength(FWrites,Length(FWrites)+1);
  FWrites[High(FWrites)]:=Copy(aBytes);
end;


function TRecordingTransport.ReadLn: String;

begin
  Result:='';
end;


function TRecordingTransport.PeerIP: string;

begin
  Result:='127.0.0.1';
end;


function TRecordingTransport.PeerPort: word;

begin
  Result:=0;
end;


constructor TRecordingConnection.Create;

begin
  inherited Create(nil,nil,[]);
  FRecorder:=TRecordingTransport.Create;
end;


destructor TRecordingConnection.Destroy;

begin
  inherited Destroy;
  FRecorder.Free;
end;


function TRecordingConnection.GetTransport: IWSTransport;

begin
  Result:=FRecorder;
end;


procedure TRecordingConnection.DoDisconnect;

begin
end;


procedure TTestWSFrameMask.SetUp;

begin
  FConnection:=TRecordingConnection.Create;
end;


procedure TTestWSFrameMask.TearDown;

begin
  FreeAndNil(FConnection);
end;


// Return the mask key of written frame aIndex in the byte order TWSFrame uses; the payload is below 126 bytes.
function TTestWSFrameMask.FrameMask(aIndex: Integer): Cardinal;

var
  lFrame: TBytes;

begin
  lFrame:=FConnection.Recorder.Writes[aIndex];
  AssertTrue('Frame has the mask bit', (lFrame[1] and $80)<>0);
  Result:=0;
  Move(lFrame[2],Result,SizeOf(Result));
end;


// Return the unmasked payload of written frame aIndex, which has a payload below 126 bytes.
function TTestWSFrameMask.FramePayload(aIndex: Integer): String;

var
  lFrame: TBytes;
  lLen, i: Integer;

begin
  lFrame:=FConnection.Recorder.Writes[aIndex];
  lLen:=lFrame[1] and $7F;
  Result:='';
  SetLength(Result,lLen);
  for i:=0 to lLen-1 do
    Result[i+1]:=Chr(lFrame[6+i] xor lFrame[2+(i mod 4)]);
end;


procedure TTestWSFrameMask.TestUnmaskedWhenMaskIsZero;

begin
  FConnection.OutgoingFrameMask:=0;
  FConnection.Send('abc');
  AssertEquals('One frame written', 1, FConnection.Recorder.WriteCount);
  AssertEquals('Frame has no mask bit', 0, FConnection.Recorder.Writes[0][1] and $80);
end;


procedure TTestWSFrameMask.TestMaskedWhenMaskIsSet;

begin
  FConnection.OutgoingFrameMask:=1;
  FConnection.Send('abc');
  AssertEquals('One frame written', 1, FConnection.Recorder.WriteCount);
  AssertTrue('Mask key is not zero', FrameMask(0)<>0);
  AssertEquals('Payload unmasks to the message', 'abc', FramePayload(0));
end;


procedure TTestWSFrameMask.TestEachFrameHasNewMask;

var
  i: Integer;

begin
  FConnection.OutgoingFrameMask:=1;
  for i:=1 to 3 do
    FConnection.Send('abc');
  AssertEquals('Three frames written', 3, FConnection.Recorder.WriteCount);
  AssertTrue('Second frame has another mask than the first', FrameMask(1)<>FrameMask(0));
  AssertTrue('Third frame has another mask than the second', FrameMask(2)<>FrameMask(1));
  for i:=0 to 2 do
    AssertEquals('Payload of each frame unmasks to the message', 'abc', FramePayload(i));
end;


procedure TTestWSFrameMask.TestConfiguredMaskValueNotUsed;

begin
  FConnection.OutgoingFrameMask:=$12345678;
  FConnection.Send('abc');
  AssertTrue('Mask key is not the configured value', FrameMask(0)<>$12345678);
end;


procedure TTestWSFrameMask.TestControlFramesHaveNewMask;

begin
  FConnection.OutgoingFrameMask:=1;
  FConnection.Send(ftPing,BytesOf('abc'));
  FConnection.Send(ftPing,BytesOf('abc'));
  AssertEquals('Two frames written', 2, FConnection.Recorder.WriteCount);
  AssertTrue('Second ping has another mask than the first', FrameMask(1)<>FrameMask(0));
  AssertEquals('Ping payload unmasks to the data', 'abc', FramePayload(1));
end;


procedure TTestWSFrameMask.TestNextFrameMask;

begin
  FConnection.OutgoingFrameMask:=0;
  AssertEquals('No mask when OutgoingFrameMask is 0', 0, FConnection.NextFrameMask);
  FConnection.OutgoingFrameMask:=1;
  AssertTrue('Nonzero mask when OutgoingFrameMask is set', FConnection.NextFrameMask<>0);
end;

initialization
  RegisterTest(TTestWSFrameMask);
end.
