{
  This file is part of the Free Component Library.
  Tests for FpQuic.Udp: segmented sends over the loopback interface.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit utquicudp;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, fpsockets, FpQuic.Udp;

type
  TTestQuicUdp = class(TTestCase)
  private
    FSender: TFPSocket;
    FReceiver: TFPSocket;
    function ReceiveSize: Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSegments;
    procedure TestDisabled;
    procedure TestTooManySegments;
  end;

implementation

procedure TTestQuicUdp.SetUp;

begin
  FSender:=UDPSocket(stIPv4);
  FReceiver:=UDPSocket(stIPv4);
  Bind(FReceiver,'127.0.0.1',0);
end;


procedure TTestQuicUdp.TearDown;

begin
  QuicGsoEnabled:=True;
  CloseSocket(FSender);
  CloseSocket(FReceiver);
end;


function TTestQuicUdp.ReceiveSize: Integer;

var
  lBuffer: array[0..65535] of Byte;

begin
  Result:=-1;
  if DataAvailable(FReceiver,1000) then
    Result:=Receive(FReceiver,@lBuffer[0],SizeOf(lBuffer));
end;


procedure TTestQuicUdp.TestSegments;

var
  lData: TBytes;

begin
  {$IFDEF LINUX}
  AssertTrue('Available on Linux',QuicGsoAvailable(FSender));
  {$ENDIF}
  if not QuicGsoAvailable(FSender) then
    Exit;
  lData:=nil;
  SetLength(lData,2900);
  AssertTrue('Sent with one call',QuicSendSegments(FSender,'127.0.0.1',LocalEndpoint(FReceiver).Second,
    PByte(lData),Length(lData),1200));
  AssertEquals('First datagram',1200,ReceiveSize);
  AssertEquals('Second datagram',1200,ReceiveSize);
  AssertEquals('Last datagram',500,ReceiveSize);
end;


procedure TTestQuicUdp.TestDisabled;

var
  lData: TBytes;

begin
  QuicGsoEnabled:=False;
  AssertFalse('Not available when disabled',QuicGsoAvailable(FSender));
  lData:=nil;
  SetLength(lData,2400);
  AssertFalse('Nothing sent when disabled',QuicSendSegments(FSender,'127.0.0.1',LocalEndpoint(FReceiver).Second,
    PByte(lData),Length(lData),1200));
  AssertEquals('No datagram',-1,ReceiveSize);
end;


procedure TTestQuicUdp.TestTooManySegments;

var
  lData: TBytes;

begin
  lData:=nil;
  SetLength(lData,10*300);
  {$IFDEF LINUX}
  AssertFalse('Rejected by the Linux kernel',QuicSendSegments(FSender,'127.0.0.1',LocalEndpoint(FReceiver).Second,
    PByte(lData),Length(lData),10));
  {$ENDIF}
end;


initialization
  RegisterTest(TTestQuicUdp);
end.
