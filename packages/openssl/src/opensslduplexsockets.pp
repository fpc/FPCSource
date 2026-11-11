{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by the Free Pascal development team

    SSL socket handler class that handles duplex read/write.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit opensslduplexsockets;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Net.Sockets, System.Net.Ssockets, System.Net.Sslsockets, Api.Openssl,
  System.Net.Fpopenssl, System.Net.Opensslsockets;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, sockets, ssockets, sslsockets, openssl, fpopenssl, opensslsockets;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TOpenSSLDuplexSocketHandler
    Using this unit makes TOpenSSLDuplexSocketHandler the default SSL handler.
    After the handshake OpenSSL reads from the socket and writes to a memory BIO
    whose contents are sent to the socket outside the SSL lock. 
    Supports one thread reading while another writes, including TLS 1.3 KeyUpdate exchanges.
    Close and Shutdown wait for a writer that is blocked on a full socket. }

  TOpenSSLDuplexSocketHandler = Class(TOpenSSLSocketHandler)
  Private
    FWriteLock : TRTLCriticalSection;
    FOutBio : PBIO;
    FOutBuf : TBytes;
    FOutStart : Integer;
    FOutEnd : Integer;
    function FlushOutput(aWait: Boolean; aDeadline: QWord; out aBlocked: Boolean): Boolean;
    function OutputQueued: Boolean;
    procedure SetFlushError(aError: Integer);
  Protected
    procedure HandshakeDone; override;
    function DoneContext: Boolean; override;
  Public
    // Creates the handler and its write lock
    Constructor Create; override;
    // Frees the handler and its write lock
    Destructor Destroy; override;
    // Sends close_notify (and waits for the peer's when BiDirectional), then frees the SSL session
    function Shutdown(BiDirectional : Boolean): boolean; override;
    // Encrypts Buffer and sends it; returns Count, 0 when the session is closed, -1 on error
    function Send(Const Buffer; Count: Integer): Integer; override;
    // Reads decrypted data; returns the byte count, 0 on close or IOTimeout, -1 on error
    function Recv(Const Buffer; Count: Integer): Integer; override;
  end;

implementation

const
  cOutBufSize = 32768;

{ TOpenSSLDuplexSocketHandler }

constructor TOpenSSLDuplexSocketHandler.Create;

begin
  inherited Create;
  InitCriticalSection(FWriteLock);
end;


destructor TOpenSSLDuplexSocketHandler.Destroy;

begin
  inherited Destroy;
  DoneCriticalSection(FWriteLock);
end;


procedure TOpenSSLDuplexSocketHandler.HandshakeDone;

var
  lReadBio, lWriteBio: PBIO;

begin
  inherited HandshakeDone;
  lReadBio:=BioNewSocket(Socket.Handle,BIO_NOCLOSE);
  if lReadBio=nil then
    Exit;
  lWriteBio:=BioNew(BioSMem);
  if lWriteBio=nil then
    begin
    BioFreeAll(lReadBio);
    Exit;
    end;
  SslSetBio(SSL.SSL,lReadBio,lWriteBio);
  FOutBio:=lWriteBio;
  SetLength(FOutBuf,cOutBufSize);
  FOutStart:=0;
  FOutEnd:=0;
end;


function TOpenSSLDuplexSocketHandler.DoneContext: Boolean;

begin
  FOutBio:=nil;
  FOutStart:=0;
  FOutEnd:=0;
  Result:=inherited DoneContext;
end;


procedure TOpenSSLDuplexSocketHandler.SetFlushError(aError: Integer);

begin
  SetIOError(SSL_ERROR_SYSCALL);
  FLastError:=aError;
end;


function TOpenSSLDuplexSocketHandler.OutputQueued: Boolean;

begin
  LockSSL;
  try
    Result:=Assigned(SSL) and (FOutBio<>nil) and (BioCtrlPending(FOutBio)>0);
  finally
    UnlockSSL;
  end;
end;


function TOpenSSLDuplexSocketHandler.FlushOutput(aWait: Boolean; aDeadline: QWord; out aBlocked: Boolean): Boolean;

var
  lLen, lErr: Integer;

begin
  Result:=True;
  aBlocked:=False;
  if aWait then
    EnterCriticalSection(FWriteLock)
  else if TryEnterCriticalSection(FWriteLock)=0 then
    Exit;
  try
    repeat
      if FOutStart=FOutEnd then
        begin
        LockSSL;
        try
          if Assigned(SSL) and (FOutBio<>nil) and (BioCtrlPending(FOutBio)>0) then
            lLen:=BioRead(FOutBio,FOutBuf,Length(FOutBuf))
          else
            lLen:=0;
        finally
          UnlockSSL;
        end;
        if lLen<=0 then
          Break;
        FOutStart:=0;
        FOutEnd:=lLen;
        end;
      lLen:=fpSend(Socket.Handle,@FOutBuf[FOutStart],FOutEnd-FOutStart,Socket.WriteFlags);
      if lLen>0 then
        Inc(FOutStart,lLen)
      else
        begin
        lErr:=SocketError;
        if lErr=EsockEINTR then
          Continue;
        if lErr<>EsockEWOULDBLOCK then
          begin
          SetFlushError(lErr);
          Exit(False);
          end;
        if not aWait then
          begin
          aBlocked:=True;
          Exit;
          end;
        if not WaitForSocket([sosCanWrite],aDeadline) then
          begin
          SetFlushError(EsockEWOULDBLOCK);
          Exit(False);
          end;
        end;
    until False;
  finally
    LeaveCriticalSection(FWriteLock);
  end;
  // Sends output another thread queued while this call held the write lock
  if aWait and OutputQueued then
    Result:=FlushOutput(True,aDeadline,aBlocked);
end;


function TOpenSSLDuplexSocketHandler.Shutdown(BiDirectional : Boolean): boolean;

var
  r : integer;
  lBlocked : Boolean;

begin
  if FOutBio=nil then
    Exit(inherited Shutdown(BiDirectional));
  EnterCriticalSection(FWriteLock);
  LockSSL;
  try
    // Shutdown runs on a blocking socket
    SetNonBlockingIO(False);
    Result:=Assigned(SSL);
    if Result then
      begin
      r:=SSL.Shutdown;
      FlushOutput(True,0,lBlocked);
      if (not BiDirectional) or (r<>0) then
        Result:=CheckSSL(r)
      else
        begin
        Result:=fpShutdown(Socket.Handle,1)=0;
        if Result then
          Result:=CheckSSL(SSL.Shutdown);
        end;
      end;
    if Result then
      Result:=DoneContext;
  finally
    UnlockSSL;
    LeaveCriticalSection(FWriteLock);
  end;
end;


function TOpenSSLDuplexSocketHandler.Send(Const Buffer; Count: Integer): Integer;

var
  e: integer;
  lDeadline: QWord;
  lBlocked: Boolean;

begin
  if FOutBio=nil then
    Exit(inherited Send(Buffer,Count));
  lDeadline:=DeadlineFromTimeout;
  repeat
    LockSSL;
    try
      if not Assigned(SSL) then
        Exit(-1);
      Result:=SSL.Write(@Buffer,Count);
      e:=SSL.GetError(Result);
      if not (e in [SSL_ERROR_WANT_READ,SSL_ERROR_WANT_WRITE]) then
        begin
        SetIOError(e);
        if (e=SSL_ERROR_ZERO_RETURN) then
          Result:=0;
        end;
    finally
      UnlockSSL;
    end;
    if not FlushOutput(True,lDeadline,lBlocked) then
      Exit(-1);
    // SSL_write waits for peer data during a TLS 1.2 renegotiation
    if (e=SSL_ERROR_WANT_READ) and not WaitForSocket([sosCanRead],lDeadline) then
      begin
      SetFlushError(EsockEWOULDBLOCK);
      Exit(-1);
      end;
  until Not (e in [SSL_ERROR_WANT_READ,SSL_ERROR_WANT_WRITE]);
end;


function TOpenSSLDuplexSocketHandler.Recv(Const Buffer; Count: Integer): Integer;

var
  e: integer;
  lDeadline: QWord;
  lBlocked: Boolean;
  lCheck: TSocketStates;

begin
  if FOutBio=nil then
    Exit(inherited Recv(Buffer,Count));
  lDeadline:=DeadlineFromTimeout;
  repeat
    LockSSL;
    try
      if not Assigned(SSL) then
        Exit(0);
      Result:=SSL.Read(@Buffer,Count);
      e:=SSL.GetError(Result);
      if not (e in [SSL_ERROR_WANT_READ,SSL_ERROR_WANT_WRITE]) then
        begin
        SetIOError(e);
        if (e=SSL_ERROR_ZERO_RETURN) then
          Result:=0;
        end;
    finally
      UnlockSSL;
    end;
    // Sends what SSL_read produced (KeyUpdate answers, alerts) without blocking
    FlushOutput(False,0,lBlocked);
    if e in [SSL_ERROR_WANT_READ,SSL_ERROR_WANT_WRITE] then
      begin
      if lBlocked then
        lCheck:=[sosCanRead,sosCanWrite]
      else
        lCheck:=[sosCanRead];
      if not WaitForSocket(lCheck,lDeadline) then
        Exit(0);
      end;
  until Not (e in [SSL_ERROR_WANT_READ,SSL_ERROR_WANT_WRITE]);
end;


initialization
  TSSLSocketHandler.SetDefaultHandlerClass(TOpenSSLDuplexSocketHandler);
end.
