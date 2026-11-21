{
  This file is part of the Free Component Library.
  UDP segmentation offload for QUIC: several datagrams in one system call (Linux UDP_SEGMENT, Windows USO).
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
unit FpQuic.Udp;

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Net.FPSockets;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, fpsockets;
{$ENDIF FPC_DOTTEDUNITS}

const
  // Largest number of bytes sent with one call.
  QuicMaxSegmentBytes = 65000;

var
  // Allows segmentation offload; False sends every datagram with its own system call.
  QuicGsoEnabled: Boolean = True;
  // Largest number of datagrams sent with one call, at most 64.
  QuicMaxSegments: Integer = 64;

// True when aSocket can send several datagrams with one call on this platform.
function QuicGsoAvailable(const aSocket: TFPSocket): Boolean;

// Sends the aLength bytes at aData to aAddress:aPort as datagrams of aSegmentSize bytes, the last one possibly
// shorter, with one system call. Returns False when segmentation offload is disabled or not supported;
// nothing was sent then. Transient send errors count as sent.
function QuicSendSegments(const aSocket: TFPSocket; const aAddress: TNetworkAddress; aPort: Word; aData: PByte;
  aLength, aSegmentSize: Integer): Boolean;

implementation

{$IFDEF LINUX}
{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes, System.InitC, UnixApi.Base, UnixApi.Sockets;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes, initc, baseunix, unixsockets;
{$ENDIF FPC_DOTTEDUNITS}

const
  SOL_UDP = 17;
  UDP_SEGMENT = 103;

type
  TControlBuffer = array[0..3] of QWord;

function QuicGsoAvailable(const aSocket: TFPSocket): Boolean;

begin
  Result:=QuicGsoEnabled;
end;


function QuicSendSegments(const aSocket: TFPSocket; const aAddress: TNetworkAddress; aPort: Word; aData: PByte;
  aLength, aSegmentSize: Integer): Boolean;

var
  lAddr: TAddressUnion;
  lIov: tiovec;
  lMsg: msghdr;
  lControl: TControlBuffer;
  lCmsg: Pcmsghdr;
  lError: cint;

begin
  if not QuicGsoEnabled then
    Exit(False);
  lAddr:=CreateAddr(aAddress,aPort,aSocket.SocketType=stIPDualStack);
  lIov.iov_base:=aData;
  lIov.iov_len:=aLength;
  lMsg:=Default(msghdr);
  lControl:=Default(TControlBuffer);
  lMsg.msg_name:=lAddr.GetAddr;
  lMsg.msg_namelen:=lAddr.GetAddrSize;
  lMsg.msg_iov:=@lIov;
  lMsg.msg_iovlen:=1;
  lMsg.msg_control:=@lControl;
  lMsg.msg_controllen:=CMSG_SPACE(SizeOf(Word));
  lCmsg:=CMSG_FIRSTHDR(@lMsg);
  lCmsg^.cmsg_level:=SOL_UDP;
  lCmsg^.cmsg_type:=UDP_SEGMENT;
  lCmsg^.cmsg_len:=CMSG_LEN(SizeOf(Word));
  PWord(CMSG_DATA(lCmsg))^:=aSegmentSize;
  if sendmsg(aSocket.FD,@lMsg,0)>=0 then
    Exit(True);
  lError:=fpgetCerrno;
  Result:=(lError<>ESysEIO) and (lError<>ESysEINVAL) and (lError<>ESysENOPROTOOPT) and (lError<>ESysEOPNOTSUPP);
end;
{$ENDIF LINUX}

{$IF DEFINED(WINDOWS) AND NOT DEFINED(WINCE)}
{$IFDEF FPC_DOTTEDUNITS}
uses
  WinApi.Windows, WinApi.Winsock2;
{$ELSE FPC_DOTTEDUNITS}
uses
  windows, winsock2;
{$ENDIF FPC_DOTTEDUNITS}

const
  UDP_SEND_MSG_SIZE = 2;

type
  TControlBuffer = array[0..3] of QWord;
  TQuicWSABuf = record
    Len: ULONG;
    Buf: PAnsiChar;
  end;

  TQuicWSAMsg = record
    Name: PSockAddr;
    NameLen: Integer;
    Buffers: ^TQuicWSABuf;
    BufferCount: DWORD;
    Control: TQuicWSABuf;
    Flags: DWORD;
  end;

  TQuicWSACMsgHdr = record
    Len: SIZE_T;
    Level: Integer;
    MsgType: Integer;
  end;

  TWSASendMsg = function(s: TSocket; aMsg: Pointer; aFlags: DWORD; aSent: PDWORD; aOverlapped: Pointer;
    aCompletion: Pointer): Integer; stdcall;

var
  WSASendMsgFunc: TWSASendMsg;

function QuicGsoAvailable(const aSocket: TFPSocket): Boolean;

var
  lValue: DWORD;
  lLength: Integer;

begin
  lValue:=0;
  lLength:=SizeOf(lValue);
  Result:=QuicGsoEnabled and Assigned(WSASendMsgFunc)
          and (getsockopt(aSocket.FD,IPPROTO_UDP,UDP_SEND_MSG_SIZE,PAnsiChar(@lValue),lLength)=0);
end;


function QuicSendSegments(const aSocket: TFPSocket; const aAddress: TNetworkAddress; aPort: Word; aData: PByte;
  aLength, aSegmentSize: Integer): Boolean;

var
  lAddr: TAddressUnion;
  lBuffer: TQuicWSABuf;
  lMsg: TQuicWSAMsg;
  lControl: TControlBuffer;
  lHeader: ^TQuicWSACMsgHdr;
  lSent: DWORD;

begin
  if not QuicGsoEnabled or not Assigned(WSASendMsgFunc) then
    Exit(False);
  lAddr:=CreateAddr(aAddress,aPort,aSocket.SocketType=stIPDualStack);
  lBuffer.Len:=aLength;
  lBuffer.Buf:=PAnsiChar(aData);
  lControl:=Default(TControlBuffer);
  lHeader:=@lControl;
  lHeader^.Len:=SizeOf(TQuicWSACMsgHdr)+SizeOf(DWORD);
  lHeader^.Level:=IPPROTO_UDP;
  lHeader^.MsgType:=UDP_SEND_MSG_SIZE;
  PDWORD(PByte(lHeader)+SizeOf(TQuicWSACMsgHdr))^:=aSegmentSize;
  lMsg:=Default(TQuicWSAMsg);
  lMsg.Name:=PSockAddr(lAddr.GetAddr);
  lMsg.NameLen:=lAddr.GetAddrSize;
  lMsg.Buffers:=@lBuffer;
  lMsg.BufferCount:=1;
  lMsg.Control.Len:=SizeOf(TQuicWSACMsgHdr)+((SizeOf(DWORD)+SizeOf(Pointer)-1) div SizeOf(Pointer))*SizeOf(Pointer);
  lMsg.Control.Buf:=PAnsiChar(lHeader);
  lSent:=0;
  Result:=(WSASendMsgFunc(aSocket.FD,@lMsg,0,@lSent,nil,nil)=0) or (WSAGetLastError<>WSAEINVAL);
end;
{$ENDIF}

{$IF NOT DEFINED(LINUX) AND (NOT DEFINED(WINDOWS) OR DEFINED(WINCE))}
function QuicGsoAvailable(const aSocket: TFPSocket): Boolean;

begin
  Result:=False;
end;


function QuicSendSegments(const aSocket: TFPSocket; const aAddress: TNetworkAddress; aPort: Word; aData: PByte;
  aLength, aSegmentSize: Integer): Boolean;

begin
  Result:=False;
end;
{$ENDIF}

{$IF DEFINED(WINDOWS) AND NOT DEFINED(WINCE)}
initialization
  Pointer(WSASendMsgFunc):=GetProcAddress(GetModuleHandle('ws2_32.dll'),'WSASendMsg');
{$ENDIF}
end.
