{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    OpenSSL 3.5+ backend for QUIC: packet protection ciphers and the QUIC TLS handshake API.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpQuic.OpenSSL;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes, System.CTypes, Api.Openssl, FpQuic.Consts, FpQuic.Buffers,
  FpQuic.Crypto, FpQuic.TransportParams, FpQuic.TLS;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes, ctypes, openssl, FpQuic.Consts, FpQuic.Buffers, FpQuic.Crypto, FpQuic.TransportParams, FpQuic.TLS;
{$ENDIF FPC_DOTTEDUNITS}

type
  // Error raised when OpenSSL fails or lacks the required functionality.
  EQuicOpenSSL = class(Exception);

  // AEAD cipher implemented with an OpenSSL EVP cipher context.
  TQuicOpenSSLAead = class(TQuicAead)
  private
    FCipher: PEVP_CIPHER;
    FKey: TBytes;
    FCtx: PEVP_CIPHER_CTX;
  public
    // Creates the AEAD of aSuite with key aKey.
    constructor Create(aSuite: TQuicCipherSuite; const aKey: TBytes);
    destructor Destroy; override;
    // Encrypts aData into aOut followed by the tag.
    procedure Seal(const aNonce: TQuicNonce; aAD: PByte; aADLen: Integer; aData: PByte; aDataLen: Integer;
      aOut: PByte); override;
    // Decrypts and authenticates aData (ciphertext and tag) into aOut.
    function Open(const aNonce: TQuicNonce; aAD: PByte; aADLen: Integer; aData: PByte; aDataLen: Integer;
      aOut: PByte): Boolean; override;
  end;

  // Header protection implemented with AES-ECB or ChaCha20 from OpenSSL.
  TQuicOpenSSLHeaderProtection = class(TQuicHeaderProtection)
  private
    FCipher: PEVP_CIPHER;
    FKey: TBytes;
    FChaCha: Boolean;
    FCtx: PEVP_CIPHER_CTX;
  public
    // Creates the header protection cipher of aSuite with key aKey.
    constructor Create(aSuite: TQuicCipherSuite; const aKey: TBytes);
    destructor Destroy; override;
    // Computes the 5-byte mask from a 16-byte sample.
    procedure Mask(aSample: PByte; out aMask: TQuicHPMask); override;
  end;

  // Cipher factory creating OpenSSL based cipher objects.
  TQuicOpenSSLCipherFactory = class(TObject, IQuicCipherFactory)
  public
    // Creates an OpenSSL AEAD.
    function CreateAead(aSuite: TQuicCipherSuite; const aKey: TBytes): TQuicAead;
    // Creates an OpenSSL header protection cipher.
    function CreateHeaderProtection(aSuite: TQuicCipherSuite; const aKey: TBytes): TQuicHeaderProtection;
  end;

  // TLS settings of a QUIC endpoint.
  TQuicTLSConfig = record
    // Application protocols: offered by a client, accepted by a server in order of preference.
    Alpn: array of AnsiString;
    // Client: server name sent with SNI and checked against the certificate when VerifyPeer is set.
    ServerName: AnsiString;
    // Server: PEM certificate chain file and private key file.
    CertificateFile: String;
    PrivateKeyFile: String;
    // Client: verify the server certificate and ServerName against CAFile and CAPath (or the default store when
    // both are empty). False by default: the server certificate is then not checked.
    VerifyPeer: Boolean;
    CAFile: String;
    CAPath: String;
    // TLS 1.3 cipher suites in OpenSSL syntax; empty for the OpenSSL default.
    CipherSuites: AnsiString;
    // File receiving the TLS secrets in NSS key log format; empty for none.
    KeyLogFile: String;
    // Server: PEM certificate chain and private key held in memory, used instead of the files when not empty.
    CertificateData: TBytes;
    PrivateKeyData: TBytes;
    // Accept (server) or send (client) 0-RTT data on resumed sessions. An attacker can replay 0-RTT data to
    // another server process or after a restart; accept it only for requests that are safe to repeat.
    EnableEarlyData: Boolean;
  end;

  // OpenSSL SSL_CTX shared by all TLS sessions of one endpoint role.
  TQuicOpenSSLContext = class
  private
    FCtx: PSSL_CTX;
    FIsServer: Boolean;
    FConfig: TQuicTLSConfig;
    FAlpnWire: TBytes;
    function LoadCertificate: Boolean;
  public
    // Creates the context; raises EQuicOpenSSL when certificates cannot be loaded.
    constructor Create(aIsServer: Boolean; const aConfig: TQuicTLSConfig);
    destructor Destroy; override;
    // True for a server context.
    property IsServer: Boolean read FIsServer;
  end;

  // TLS 1.3 session using the OpenSSL 3.5 QUIC TLS callbacks.
  TQuicOpenSSLTLS = class(TObject, IQuicTLS)
  private
    FContext: TQuicOpenSSLContext;
    FSSL: PSSL;
    FEvents: IQuicTLSEvents;
    FParams: TBytes;
    FPeerParams: TBytes;
    FRecv: array[TQuicEncryptionLevel] of TBytes;
    FReadLevel: TQuicEncryptionLevel;
    FWriteLevel: TQuicEncryptionLevel;
    FComplete: Boolean;
    FError: TObject;
    procedure Advance;
    function Fail: cint;
  public
    // Creates a session using aContext, which must outlive the session.
    constructor Create(aContext: TQuicOpenSSLContext);
    destructor Destroy; override;
    // Starts the handshake; raises the first exception raised by an event handler or the key log.
    procedure Start(const aEvents: IQuicTLSEvents; const aTransportParams: TBytes);
    // Passes received CRYPTO data to OpenSSL; raises the first exception raised by an event handler or the key log.
    procedure ProvideData(aLevel: TQuicEncryptionLevel; const aData: TBytes);
    // True once the handshake is complete.
    function IsComplete: Boolean;
    // Negotiated application protocol.
    function NegotiatedAlpn: AnsiString;
    // Resumes the session of a ticket received with TLSSessionTicket; True when 0-RTT is possible.
    function Resume(const aTicket: TBytes; out aPeerParams: TBytes): Boolean;
    // True when the server accepted 0-RTT data.
    function EarlyDataAccepted: Boolean;
  end;

// Loads OpenSSL from the given library files (empty names: default search) and checks for the QUIC TLS API.
function QuicInitOpenSSL(const aSSLLibrary: String = ''; const aCryptoLibrary: String = ''): Boolean;

implementation

const
  EVP_CTRL_AEAD_GET_TAG = $10;
  EVP_CTRL_AEAD_SET_TAG = $11;
  AeadNames: array[TQuicCipherSuite] of AnsiString = ('aes-128-gcm','aes-256-gcm','chacha20-poly1305');
  HPNames: array[TQuicCipherSuite] of AnsiString = ('aes-128-ecb','aes-256-ecb','chacha20');

// Raises EQuicOpenSSL with aMessage when aResult is not positive.
procedure CheckSSL(aResult: cint; const aMessage: String);

begin
  if aResult<=0 then
    raise EQuicOpenSSL.Create(aMessage);
end;


{ TQuicOpenSSLAead }

constructor TQuicOpenSSLAead.Create(aSuite: TQuicCipherSuite; const aKey: TBytes);

begin
  FCipher:=EVP_get_cipherbyname(PAnsiChar(AeadNames[aSuite]));
  FKey:=Copy(aKey);
  FCtx:=EVP_CIPHER_CTX_new();
end;


destructor TQuicOpenSSLAead.Destroy;

begin
  EVP_CIPHER_CTX_free(FCtx);
  QuicWipe(FKey);
  inherited Destroy;
end;


procedure TQuicOpenSSLAead.Seal(const aNonce: TQuicNonce; aAD: PByte; aADLen: Integer; aData: PByte;
  aDataLen: Integer; aOut: PByte);

var
  lLength: cint;

begin
  lLength:=0;
  CheckSSL(EVP_EncryptInit(FCtx,FCipher,PByte(FKey),@aNonce[0]),'AEAD init failed');
  CheckSSL(EVP_EncryptUpdate(FCtx,nil,@lLength,aAD,aADLen),'AEAD associated data failed');
  if aDataLen>0 then
    CheckSSL(EVP_EncryptUpdate(FCtx,aOut,@lLength,aData,aDataLen),'AEAD encryption failed');
  CheckSSL(EVP_EncryptFinal(FCtx,aOut+aDataLen,@lLength),'AEAD final failed');
  CheckSSL(EVP_CIPHER_CTX_ctrl(FCtx,EVP_CTRL_AEAD_GET_TAG,QUIC_AEAD_TAG_LENGTH,aOut+aDataLen),'AEAD tag failed');
end;


function TQuicOpenSSLAead.Open(const aNonce: TQuicNonce; aAD: PByte; aADLen: Integer; aData: PByte;
  aDataLen: Integer; aOut: PByte): Boolean;

var
  lLength, lPlain: cint;

begin
  lLength:=0;
  lPlain:=aDataLen-QUIC_AEAD_TAG_LENGTH;
  CheckSSL(EVP_DecryptInit(FCtx,FCipher,PByte(FKey),@aNonce[0]),'AEAD init failed');
  CheckSSL(EVP_DecryptUpdate(FCtx,nil,@lLength,aAD,aADLen),'AEAD associated data failed');
  if lPlain>0 then
    CheckSSL(EVP_DecryptUpdate(FCtx,aOut,@lLength,aData,lPlain),'AEAD decryption failed');
  CheckSSL(EVP_CIPHER_CTX_ctrl(FCtx,EVP_CTRL_AEAD_SET_TAG,QUIC_AEAD_TAG_LENGTH,aData+lPlain),'AEAD tag failed');
  Result:=EVP_DecryptFinal(FCtx,aOut+lPlain,@lLength)>0;
end;


{ TQuicOpenSSLHeaderProtection }

constructor TQuicOpenSSLHeaderProtection.Create(aSuite: TQuicCipherSuite; const aKey: TBytes);

begin
  FCipher:=EVP_get_cipherbyname(PAnsiChar(HPNames[aSuite]));
  FKey:=Copy(aKey);
  FChaCha:=aSuite=qcsChaCha20Poly1305;
  FCtx:=EVP_CIPHER_CTX_new();
end;


destructor TQuicOpenSSLHeaderProtection.Destroy;

begin
  EVP_CIPHER_CTX_free(FCtx);
  QuicWipe(FKey);
  inherited Destroy;
end;


procedure TQuicOpenSSLHeaderProtection.Mask(aSample: PByte; out aMask: TQuicHPMask);

var
  lBlock: array[0..15] of Byte;
  lLength: cint;

begin
  lLength:=0;
  FillChar(lBlock,SizeOf(lBlock),0);
  if FChaCha then
    begin
    CheckSSL(EVP_EncryptInit(FCtx,FCipher,PByte(FKey),aSample),'Header protection init failed');
    CheckSSL(EVP_EncryptUpdate(FCtx,@lBlock[0],@lLength,@lBlock[0],5),'Header protection failed');
    end
  else
    begin
    CheckSSL(EVP_EncryptInit(FCtx,FCipher,PByte(FKey),nil),'Header protection init failed');
    CheckSSL(EVP_EncryptUpdate(FCtx,@lBlock[0],@lLength,aSample,16),'Header protection failed');
    end;
  Move(lBlock[0],aMask[0],5);
end;


{ TQuicOpenSSLCipherFactory }

function TQuicOpenSSLCipherFactory.CreateAead(aSuite: TQuicCipherSuite; const aKey: TBytes): TQuicAead;

begin
  Result:=TQuicOpenSSLAead.Create(aSuite,aKey);
end;


function TQuicOpenSSLCipherFactory.CreateHeaderProtection(aSuite: TQuicCipherSuite;
  const aKey: TBytes): TQuicHeaderProtection;

begin
  Result:=TQuicOpenSSLHeaderProtection.Create(aSuite,aKey);
end;


type
  TOsslDispatch = record
    FunctionId: cint;
    Func: Pointer;
  end;

  TSSLSetQuicTlsCbs = function(s: PSSL; qtdis: Pointer; arg: Pointer): cint; cdecl;
  TSSLSetQuicTlsTransportParams = function(s: PSSL; params: PByte; params_len: csize_t): cint; cdecl;
  TSSLFunc = function(s: PSSL): cint; cdecl;
  TSSLGetPendingCipher = function(s: PSSL): SslPtr; cdecl;
  TSSLCtxUsePrivateKeyFile = function(ctx: PSSL_CTX; aFile: PAnsiChar; aType: cint): cint; cdecl;
  TSSLCtxSetCipherSuites = function(ctx: PSSL_CTX; aList: PAnsiChar): cint; cdecl;
  TSSLSetExData = function(s: PSSL; aIndex: cint; aData: Pointer): cint; cdecl;
  TSSLGetExData = function(s: PSSL; aIndex: cint): Pointer; cdecl;
  TSSLKeyLogCallback = procedure(s: PSSL; aLine: PAnsiChar); cdecl;
  TSSLCtxSetKeyLogCallback = procedure(ctx: PSSL_CTX; aCallback: TSSLKeyLogCallback); cdecl;
  TSSLSetEnabled = function(s: PSSL; aEnabled: cint): cint; cdecl;
  TSSLNewSessionCallback = function(s: PSSL; aSession: Pointer): cint; cdecl;
  TSSLCtxSessSetNewCb = procedure(ctx: PSSL_CTX; aCallback: TSSLNewSessionCallback); cdecl;
  TI2DSSLSession = function(aSession: Pointer; aOut: PPByte): cint; cdecl;
  TD2ISSLSession = function(aSession: PPointer; aIn: PPByte; aLength: clong): Pointer; cdecl;
  TSSLSessionFree = procedure(aSession: Pointer); cdecl;
  TSSLSetSession = function(s: PSSL; aSession: Pointer): cint; cdecl;
  TSSLCtxClearOptions = function(ctx: PSSL_CTX; aOptions: cuint64): cuint64; cdecl;
  TSSLSetShutdown = procedure(s: PSSL; aMode: cint); cdecl;
  TSSLGetSession = function(s: PSSL): Pointer; cdecl;
  TSSLSessionGet0Cipher = function(aSession: Pointer): SslPtr; cdecl;
  TSSLSessionSet1TicketAppData = function(aSession: Pointer; aData: Pointer; aLength: csize_t): cint; cdecl;
  TSSLSessionGet0TicketAppData = function(aSession: Pointer; aData: PPointer; aLength: pcsize_t): cint; cdecl;
  TSSLTicketGenCallback = function(s: PSSL; aArg: Pointer): cint; cdecl;
  TSSLCtxSetSessionTicketCb = function(ctx: PSSL_CTX; aGen: TSSLTicketGenCallback; aDec: Pointer;
    aArg: Pointer): cint; cdecl;
  TSSLAllowEarlyDataCallback = function(s: PSSL; aArg: Pointer): cint; cdecl;
  TSSLCtxSetAllowEarlyDataCb = procedure(ctx: PSSL_CTX; aCallback: TSSLAllowEarlyDataCallback; aArg: Pointer); cdecl;

const
  TLS1_3_VERSION = $0304;
  OSSL_FUNC_SSL_QUIC_TLS_CRYPTO_SEND = 2001;
  OSSL_FUNC_SSL_QUIC_TLS_CRYPTO_RECV_RCD = 2002;
  OSSL_FUNC_SSL_QUIC_TLS_CRYPTO_RELEASE_RCD = 2003;
  OSSL_FUNC_SSL_QUIC_TLS_YIELD_SECRET = 2004;
  OSSL_FUNC_SSL_QUIC_TLS_GOT_TRANSPORT_PARAMS = 2005;
  OSSL_FUNC_SSL_QUIC_TLS_ALERT = 2006;
  OSSL_RECORD_PROTECTION_LEVEL_EARLY = 1;
  OSSL_RECORD_PROTECTION_LEVEL_HANDSHAKE = 2;
  SSL_CTRL_EXTRA_CHAIN_CERT = 14;
  SSL_CTRL_SET_SESS_CACHE_MODE = 44;
  SSL_SESS_CACHE_CLIENT_ONLY = $301;
  SSL_OP_ENABLE_MIDDLEBOX_COMPAT = $00100000;
  SSL_SHUTDOWN_DONE = 3;
  SSL_EARLY_DATA_ACCEPTED = 2;

var
  SSL_set_quic_tls_cbs: TSSLSetQuicTlsCbs;
  SSL_set_quic_tls_transport_params: TSSLSetQuicTlsTransportParams;
  SSL_do_handshake: TSSLFunc;
  SSL_set_connect_state: TSSLFunc;
  SSL_set_accept_state: TSSLFunc;
  SSL_get_pending_cipher: TSSLGetPendingCipher;
  SSL_CTX_use_PrivateKey_file: TSSLCtxUsePrivateKeyFile;
  SSL_CTX_set_ciphersuites: TSSLCtxSetCipherSuites;
  SSL_set_ex_data: TSSLSetExData;
  SSL_get_ex_data: TSSLGetExData;
  SSL_CTX_set_keylog_callback: TSSLCtxSetKeyLogCallback;
  SSL_set_quic_tls_early_data_enabled: TSSLSetEnabled;
  SSL_CTX_sess_set_new_cb: TSSLCtxSessSetNewCb;
  i2d_SSL_SESSION: TI2DSSLSession;
  d2i_SSL_SESSION: TD2ISSLSession;
  SSL_SESSION_free: TSSLSessionFree;
  SSL_set_session: TSSLSetSession;
  SSL_CTX_clear_options: TSSLCtxClearOptions;
  SSL_set_shutdown: TSSLSetShutdown;
  SSL_get_session: TSSLGetSession;
  SSL_SESSION_get0_cipher: TSSLSessionGet0Cipher;
  SSL_SESSION_set1_ticket_appdata: TSSLSessionSet1TicketAppData;
  SSL_SESSION_get0_ticket_appdata: TSSLSessionGet0TicketAppData;
  SSL_CTX_set_session_ticket_cb: TSSLCtxSetSessionTicketCb;
  SSL_CTX_set_allow_early_data_cb: TSSLCtxSetAllowEarlyDataCb;
  SSL_get_early_data_status: TSSLFunc;
  SSL_is_init_finished: TSSLFunc;
  QuicTlsDispatch: array[0..6] of TOsslDispatch;

// Resolves the libssl functions missing from the openssl unit; False without the QUIC TLS API.
function LoadQuicFunctions: Boolean;

begin
  Pointer(SSL_set_quic_tls_cbs):=GetProcAddress(SSLLibHandle,'SSL_set_quic_tls_cbs');
  Pointer(SSL_set_quic_tls_transport_params):=GetProcAddress(SSLLibHandle,'SSL_set_quic_tls_transport_params');
  Pointer(SSL_do_handshake):=GetProcAddress(SSLLibHandle,'SSL_do_handshake');
  Pointer(SSL_set_connect_state):=GetProcAddress(SSLLibHandle,'SSL_set_connect_state');
  Pointer(SSL_set_accept_state):=GetProcAddress(SSLLibHandle,'SSL_set_accept_state');
  Pointer(SSL_get_pending_cipher):=GetProcAddress(SSLLibHandle,'SSL_get_pending_cipher');
  Pointer(SSL_CTX_use_PrivateKey_file):=GetProcAddress(SSLLibHandle,'SSL_CTX_use_PrivateKey_file');
  Pointer(SSL_CTX_set_ciphersuites):=GetProcAddress(SSLLibHandle,'SSL_CTX_set_ciphersuites');
  Pointer(SSL_set_ex_data):=GetProcAddress(SSLLibHandle,'SSL_set_ex_data');
  Pointer(SSL_get_ex_data):=GetProcAddress(SSLLibHandle,'SSL_get_ex_data');
  Pointer(SSL_CTX_set_keylog_callback):=GetProcAddress(SSLLibHandle,'SSL_CTX_set_keylog_callback');
  Pointer(SSL_set_quic_tls_early_data_enabled):=GetProcAddress(SSLLibHandle,'SSL_set_quic_tls_early_data_enabled');
  Pointer(SSL_CTX_sess_set_new_cb):=GetProcAddress(SSLLibHandle,'SSL_CTX_sess_set_new_cb');
  Pointer(i2d_SSL_SESSION):=GetProcAddress(SSLLibHandle,'i2d_SSL_SESSION');
  Pointer(d2i_SSL_SESSION):=GetProcAddress(SSLLibHandle,'d2i_SSL_SESSION');
  Pointer(SSL_SESSION_free):=GetProcAddress(SSLLibHandle,'SSL_SESSION_free');
  Pointer(SSL_set_session):=GetProcAddress(SSLLibHandle,'SSL_set_session');
  Pointer(SSL_CTX_clear_options):=GetProcAddress(SSLLibHandle,'SSL_CTX_clear_options');
  Pointer(SSL_set_shutdown):=GetProcAddress(SSLLibHandle,'SSL_set_shutdown');
  Pointer(SSL_get_session):=GetProcAddress(SSLLibHandle,'SSL_get_session');
  Pointer(SSL_SESSION_get0_cipher):=GetProcAddress(SSLLibHandle,'SSL_SESSION_get0_cipher');
  Pointer(SSL_SESSION_set1_ticket_appdata):=GetProcAddress(SSLLibHandle,'SSL_SESSION_set1_ticket_appdata');
  Pointer(SSL_SESSION_get0_ticket_appdata):=GetProcAddress(SSLLibHandle,'SSL_SESSION_get0_ticket_appdata');
  Pointer(SSL_CTX_set_session_ticket_cb):=GetProcAddress(SSLLibHandle,'SSL_CTX_set_session_ticket_cb');
  Pointer(SSL_CTX_set_allow_early_data_cb):=GetProcAddress(SSLLibHandle,'SSL_CTX_set_allow_early_data_cb');
  Pointer(SSL_get_early_data_status):=GetProcAddress(SSLLibHandle,'SSL_get_early_data_status');
  Pointer(SSL_is_init_finished):=GetProcAddress(SSLLibHandle,'SSL_is_init_finished');
  Result:=Assigned(SSL_set_quic_tls_cbs);
end;


function QuicInitOpenSSL(const aSSLLibrary: String; const aCryptoLibrary: String): Boolean;

begin
  Result:=InitSSLInterface(aSSLLibrary,aCryptoLibrary) and LoadQuicFunctions;
end;


// Maps an OpenSSL record protection level to a QUIC encryption level.
function LevelOf(aProtLevel: cuint32): TQuicEncryptionLevel;

begin
  if aProtLevel=OSSL_RECORD_PROTECTION_LEVEL_HANDSHAKE then
    Result:=qelHandshake
  else
    Result:=qelApplication;
end;


// Maps a TLS 1.3 cipher suite name to a QUIC cipher suite; raises EQuicOpenSSL for other suites.
function SuiteOf(const aName: AnsiString): TQuicCipherSuite;

begin
  if aName='TLS_AES_128_GCM_SHA256' then
    Result:=qcsAes128Gcm
  else if aName='TLS_AES_256_GCM_SHA384' then
    Result:=qcsAes256Gcm
  else if aName='TLS_CHACHA20_POLY1305_SHA256' then
    Result:=qcsChaCha20Poly1305
  else
    raise EQuicOpenSSL.Create('Cipher suite '+aName+' is not supported by QUIC');
end;


// OpenSSL callback: handshake bytes to send at the current write level.
function CbCryptoSend(s: PSSL; aBuf: PByte; aLen: csize_t; aConsumed: pcsize_t; aArg: Pointer): cint; cdecl;

var
  lTLS: TQuicOpenSSLTLS;

begin
  lTLS:=TQuicOpenSSLTLS(aArg);
  try
    lTLS.FEvents.TLSSendCrypto(lTLS.FWriteLevel,QuicBytesOf(aBuf,aLen));
    aConsumed^:=aLen;
    Result:=1;
  except
    Result:=lTLS.Fail;
  end;
end;


// OpenSSL callback: returns the buffered handshake bytes of the current read level.
function CbCryptoRecvRcd(s: PSSL; aBuf: PPByte; aBytesRead: pcsize_t; aArg: Pointer): cint; cdecl;

var
  lTLS: TQuicOpenSSLTLS;

begin
  lTLS:=TQuicOpenSSLTLS(aArg);
  aBuf^:=PByte(lTLS.FRecv[lTLS.FReadLevel]);
  aBytesRead^:=Length(lTLS.FRecv[lTLS.FReadLevel]);
  Result:=1;
end;


// OpenSSL callback: drops handshake bytes OpenSSL has consumed.
function CbCryptoReleaseRcd(s: PSSL; aBytesRead: csize_t; aArg: Pointer): cint; cdecl;

var
  lTLS: TQuicOpenSSLTLS;

begin
  lTLS:=TQuicOpenSSLTLS(aArg);
  Delete(lTLS.FRecv[lTLS.FReadLevel],0,aBytesRead);
  Result:=1;
end;


// OpenSSL callback: a new traffic secret; direction 1 is the write direction.
function CbYieldSecret(s: PSSL; aProtLevel: cuint32; aDirection: cint; aSecret: PByte; aSecretLen: csize_t;
  aArg: Pointer): cint; cdecl;

var
  lTLS: TQuicOpenSSLTLS;
  lLevel: TQuicEncryptionLevel;
  lCipher: SslPtr;

begin
  lTLS:=TQuicOpenSSLTLS(aArg);
  Result:=1;
  try
    lCipher:=SSL_get_pending_cipher(s);
    if lCipher=nil then
      lCipher:=SSL_SESSION_get0_cipher(SSL_get_session(s));
    if aProtLevel=OSSL_RECORD_PROTECTION_LEVEL_EARLY then
      begin
      lTLS.FEvents.TLSEarlySecret(aDirection=1,SuiteOf(SSLCipherGetName(lCipher)),QuicBytesOf(aSecret,aSecretLen));
      Exit;
      end;
    lLevel:=LevelOf(aProtLevel);
    if aDirection=1 then
      lTLS.FWriteLevel:=lLevel
    else
      lTLS.FReadLevel:=lLevel;
    lTLS.FEvents.TLSSecret(lLevel,aDirection=1,SuiteOf(SSLCipherGetName(lCipher)),QuicBytesOf(aSecret,aSecretLen));
  except
    Result:=lTLS.Fail;
  end;
end;


// OpenSSL callback: transport parameters received from the peer.
function CbGotTransportParams(s: PSSL; aParams: PByte; aLen: csize_t; aArg: Pointer): cint; cdecl;

var
  lTLS: TQuicOpenSSLTLS;

begin
  lTLS:=TQuicOpenSSLTLS(aArg);
  try
    lTLS.FPeerParams:=QuicBytesOf(aParams,aLen);
    lTLS.FEvents.TLSPeerTransportParams(lTLS.FPeerParams);
    Result:=1;
  except
    Result:=lTLS.Fail;
  end;
end;


// OpenSSL client callback: a new session; passes it with the server's transport parameters as a ticket.
function CbNewSession(s: PSSL; aSession: Pointer): cint; cdecl;

var
  lTLS: TQuicOpenSSLTLS;
  lDer: TBytes;
  lOut: PByte;
  lWriter: TQuicWriter;

begin
  lTLS:=TQuicOpenSSLTLS(SSL_get_ex_data(s,0));
  Result:=0;
  try
    lDer:=nil;
    SetLength(lDer,i2d_SSL_SESSION(aSession,nil));
    lOut:=PByte(lDer);
    i2d_SSL_SESSION(aSession,@lOut);
    lWriter.Init;
    lWriter.WriteVarBytes(lDer);
    lWriter.WriteVarBytes(lTLS.FPeerParams);
    lTLS.FEvents.TLSSessionTicket(lWriter.ToBytes);
  except
    lTLS.Fail;
  end;
end;


// OpenSSL server callback: stores the local transport parameters in a new ticket.
function CbGenerateTicket(s: PSSL; aArg: Pointer): cint; cdecl;

var
  lTLS: TQuicOpenSSLTLS;

begin
  lTLS:=TQuicOpenSSLTLS(SSL_get_ex_data(s,0));
  Result:=SSL_SESSION_set1_ticket_appdata(SSL_get_session(s),PByte(lTLS.FParams),Length(lTLS.FParams));
end;


// OpenSSL server callback: accepts 0-RTT only when the ticket's transport parameters are still honoured.
function CbAllowEarlyData(s: PSSL; aArg: Pointer): cint; cdecl;

var
  lTLS: TQuicOpenSSLTLS;
  lData: Pointer;
  lLength: csize_t;
  lRemembered, lCurrent: TQuicTransportParams;

begin
  lTLS:=TQuicOpenSSLTLS(SSL_get_ex_data(s,0));
  lData:=nil;
  lLength:=0;
  SSL_SESSION_get0_ticket_appdata(SSL_get_session(s),@lData,@lLength);
  lRemembered:=QuicDecodeTransportParams(QuicBytesOf(lData,lLength),True);
  lCurrent:=QuicDecodeTransportParams(lTLS.FParams,True);
  Result:=Ord(QuicEarlyParamsCompatible(lRemembered,lCurrent));
end;


// OpenSSL callback: a TLS alert to send.
function CbAlert(s: PSSL; aAlert: Byte; aArg: Pointer): cint; cdecl;

var
  lTLS: TQuicOpenSSLTLS;

begin
  lTLS:=TQuicOpenSSLTLS(aArg);
  try
    lTLS.FEvents.TLSAlert(aAlert);
    Result:=1;
  except
    Result:=lTLS.Fail;
  end;
end;


// OpenSSL ALPN selection callback of a server context.
function CbAlpnSelect(s: PSSL; aOut: PPByte; aOutLen: PByte; aIn: PByte; aInLen: cuint; aArg: Pointer): cint; cdecl;

var
  lContext: TQuicOpenSSLContext;

begin
  lContext:=TQuicOpenSSLContext(aArg);
  if SslSelectNextProto(aOut,aOutLen,PByte(lContext.FAlpnWire),Length(lContext.FAlpnWire),aIn,aInLen)=1 then
    Result:=SSL_TLSEXT_ERR_OK
  else
    Result:=SSL_TLSEXT_ERR_ALERT_FATAL;
end;


// OpenSSL key log callback: appends aLine to the key log file of the session's context.
procedure CbKeyLog(s: PSSL; aLine: PAnsiChar); cdecl;

var
  lTLS: TQuicOpenSSLTLS;
  lFileName: String;
  lFile: TFileStream;
  lText: AnsiString;

begin
  lTLS:=TQuicOpenSSLTLS(SSL_get_ex_data(s,0));
  lFileName:=lTLS.FContext.FConfig.KeyLogFile;
  try
    if FileExists(lFileName) then
      begin
      lFile:=TFileStream.Create(lFileName,fmOpenWrite or fmShareDenyNone);
      lFile.Seek(0,soEnd);
      end
    else
      lFile:=TFileStream.Create(lFileName,fmCreate);
    try
      lText:=AnsiString(aLine)+#10;
      lFile.WriteBuffer(lText[1],Length(lText));
    finally
      lFile.Free;
    end;
  except
    lTLS.Fail;
  end;
end;


{ TQuicOpenSSLContext }

constructor TQuicOpenSSLContext.Create(aIsServer: Boolean; const aConfig: TQuicTLSConfig);

var
  lProto: AnsiString;
  lWriter: TQuicWriter;

begin
  FIsServer:=aIsServer;
  FConfig:=aConfig;
  lWriter.Init;
  for lProto in aConfig.Alpn do
    begin
    lWriter.WriteByte(Length(lProto));
    lWriter.WriteBuffer(PByte(lProto),Length(lProto));
    end;
  FAlpnWire:=lWriter.ToBytes;
  FCtx:=SslCtxNew(SslTLSMethod);
  SslCtxSetMinProtoVersion(FCtx,TLS1_3_VERSION);
  if aConfig.CipherSuites<>'' then
    CheckSSL(SSL_CTX_set_ciphersuites(FCtx,PAnsiChar(aConfig.CipherSuites)),'Invalid cipher suites');
  if aConfig.KeyLogFile<>'' then
    SSL_CTX_set_keylog_callback(FCtx,@CbKeyLog);
  if aIsServer then
    begin
    SslCtxSetAlpnSelectCb(FCtx,@CbAlpnSelect,Self);
    if not LoadCertificate then
      raise EQuicOpenSSL.Create('Cannot load certificate or private key');
    if aConfig.EnableEarlyData then
      begin
      SSL_CTX_set_session_ticket_cb(FCtx,@CbGenerateTicket,nil,nil);
      SSL_CTX_set_allow_early_data_cb(FCtx,@CbAllowEarlyData,nil);
      end;
    end
  else
    begin
    SSL_CTX_clear_options(FCtx,SSL_OP_ENABLE_MIDDLEBOX_COMPAT);
    SslCTXCtrl(FCtx,SSL_CTRL_SET_SESS_CACHE_MODE,SSL_SESS_CACHE_CLIENT_ONLY,nil);
    SSL_CTX_sess_set_new_cb(FCtx,@CbNewSession);
    SslCtxSetAlpnProtos(FCtx,PByte(FAlpnWire),Length(FAlpnWire));
    if aConfig.VerifyPeer then
      begin
      if aConfig.ServerName='' then
        raise EQuicOpenSSL.Create('VerifyPeer needs a ServerName');
      SslCtxSetVerify(FCtx,SSL_VERIFY_PEER,nil);
      if (aConfig.CAFile<>'') or (aConfig.CAPath<>'') then
        CheckSSL(SslCtxLoadVerifyLocations(FCtx,aConfig.CAFile,aConfig.CAPath),'Cannot load the CA certificates')
      else
        CheckSSL(SslCtxSetDefaultVerifyPaths(FCtx),'Cannot load the default CA certificates');
      end;
    end;
end;


function TQuicOpenSSLContext.LoadCertificate: Boolean;

var
  lBio: PBIO;
  lCert: PX509;
  lKey: PEVP_PKEY;

begin
  if Length(FConfig.CertificateData)=0 then
    Result:=SslCtxUseCertificateChainFile(FCtx,FConfig.CertificateFile)=1
  else
    begin
    lBio:=BIO_new_mem_buf(PByte(FConfig.CertificateData),Length(FConfig.CertificateData));
    try
      lCert:=PEMReadBioX509(lBio,nil,nil,nil);
      Result:=(lCert<>nil) and (SslCtxUseCertificate(FCtx,lCert)=1);
      X509Free(lCert);
      lCert:=PEMReadBioX509(lBio,nil,nil,nil);
      while lCert<>nil do
        begin
        SslCTXCtrl(FCtx,SSL_CTRL_EXTRA_CHAIN_CERT,0,lCert);
        lCert:=PEMReadBioX509(lBio,nil,nil,nil);
        end;
    finally
      BioFreeAll(lBio);
    end;
    end;
  if Length(FConfig.PrivateKeyData)=0 then
    Exit(Result and (SSL_CTX_use_PrivateKey_file(FCtx,PAnsiChar(AnsiString(FConfig.PrivateKeyFile)),
      SSL_FILETYPE_PEM)=1));
  lBio:=BIO_new_mem_buf(PByte(FConfig.PrivateKeyData),Length(FConfig.PrivateKeyData));
  try
    lKey:=PEM_read_bio_PrivateKey(lBio,nil,nil,nil);
    Result:=Result and (lKey<>nil) and (SslCtxUsePrivateKey(FCtx,lKey)=1);
    EvpPkeyFree(lKey);
  finally
    BioFreeAll(lBio);
  end;
end;


destructor TQuicOpenSSLContext.Destroy;

begin
  SslCtxFree(FCtx);
  inherited Destroy;
end;


{ TQuicOpenSSLTLS }

constructor TQuicOpenSSLTLS.Create(aContext: TQuicOpenSSLContext);

begin
  FContext:=aContext;
  FSSL:=SslNew(aContext.FCtx);
  SSL_set_ex_data(FSSL,0,Self);
  SSL_set_quic_tls_cbs(FSSL,@QuicTlsDispatch[0],Self);
  if aContext.IsServer then
    begin
    SSL_set_accept_state(FSSL);
    if aContext.FConfig.EnableEarlyData then
      SSL_set_quic_tls_early_data_enabled(FSSL,1);
    end
  else
    SSL_set_connect_state(FSSL);
end;


destructor TQuicOpenSSLTLS.Destroy;

begin
  if FComplete then
    SSL_set_shutdown(FSSL,SSL_SHUTDOWN_DONE);
  SslFree(FSSL);
  FError.Free;
  inherited Destroy;
end;


procedure TQuicOpenSSLTLS.Advance;

var
  lDummy: Byte;
  lError: TObject;

begin
  if FComplete then
    SslRead(FSSL,@lDummy,1)
  else
    FComplete:=(SSL_do_handshake(FSSL)=1) and (SSL_is_init_finished(FSSL)=1);
  if FError=nil then
    Exit;
  lError:=FError;
  FError:=nil;
  raise lError;
end;


// Keeps the exception being handled for Advance to raise once OpenSSL returns; returns 0.
function TQuicOpenSSLTLS.Fail: cint;

begin
  if FError=nil then
    FError:=TObject(AcquireExceptionObject);
  Result:=0;
end;


procedure TQuicOpenSSLTLS.Start(const aEvents: IQuicTLSEvents; const aTransportParams: TBytes);

begin
  FEvents:=aEvents;
  FParams:=aTransportParams;
  SSL_set_quic_tls_transport_params(FSSL,PByte(FParams),Length(FParams));
  if not FContext.IsServer then
    begin
    if FContext.FConfig.ServerName<>'' then
      SslSetTlsextHostName(FSSL,PAnsiChar(FContext.FConfig.ServerName));
    if FContext.FConfig.VerifyPeer then
      CheckSSL(SslSet1Host(FSSL,FContext.FConfig.ServerName),'Invalid server name');
    Advance;
    end;
end;


procedure TQuicOpenSSLTLS.ProvideData(aLevel: TQuicEncryptionLevel; const aData: TBytes);

begin
  FRecv[aLevel]:=Concat(FRecv[aLevel],aData);
  Advance;
end;


function TQuicOpenSSLTLS.IsComplete: Boolean;

begin
  Result:=FComplete;
end;


function TQuicOpenSSLTLS.NegotiatedAlpn: AnsiString;

var
  lData: PByte;
  lLength: cuint;

begin
  lData:=nil;
  lLength:=0;
  SslGet0AlpnSelected(FSSL,@lData,@lLength);
  Result:='';
  SetString(Result,PAnsiChar(lData),lLength);
end;


function TQuicOpenSSLTLS.Resume(const aTicket: TBytes; out aPeerParams: TBytes): Boolean;

var
  lReader: TQuicReader;
  lDer, lParams: TBytes;
  lIn: PByte;
  lSession: Pointer;

begin
  aPeerParams:=nil;
  try
    lReader.InitBytes(aTicket);
    lDer:=lReader.ReadVarBytes;
    lParams:=lReader.ReadVarBytes;
  except
    on EQuicError do
      Exit(False);
  end;
  lIn:=PByte(lDer);
  lSession:=d2i_SSL_SESSION(nil,@lIn,Length(lDer));
  if lSession=nil then
    Exit(False);
  SSL_set_session(FSSL,lSession);
  SSL_SESSION_free(lSession);
  Result:=FContext.FConfig.EnableEarlyData and (SSL_set_quic_tls_early_data_enabled(FSSL,1)=1);
  if Result then
    aPeerParams:=lParams;
end;


function TQuicOpenSSLTLS.EarlyDataAccepted: Boolean;

begin
  Result:=SSL_get_early_data_status(FSSL)=SSL_EARLY_DATA_ACCEPTED;
end;


initialization
  QuicTlsDispatch[0].FunctionId:=OSSL_FUNC_SSL_QUIC_TLS_CRYPTO_SEND;
  QuicTlsDispatch[0].Func:=@CbCryptoSend;
  QuicTlsDispatch[1].FunctionId:=OSSL_FUNC_SSL_QUIC_TLS_CRYPTO_RECV_RCD;
  QuicTlsDispatch[1].Func:=@CbCryptoRecvRcd;
  QuicTlsDispatch[2].FunctionId:=OSSL_FUNC_SSL_QUIC_TLS_CRYPTO_RELEASE_RCD;
  QuicTlsDispatch[2].Func:=@CbCryptoReleaseRcd;
  QuicTlsDispatch[3].FunctionId:=OSSL_FUNC_SSL_QUIC_TLS_YIELD_SECRET;
  QuicTlsDispatch[3].Func:=@CbYieldSecret;
  QuicTlsDispatch[4].FunctionId:=OSSL_FUNC_SSL_QUIC_TLS_GOT_TRANSPORT_PARAMS;
  QuicTlsDispatch[4].Func:=@CbGotTransportParams;
  QuicTlsDispatch[5].FunctionId:=OSSL_FUNC_SSL_QUIC_TLS_ALERT;
  QuicTlsDispatch[5].Func:=@CbAlert;


end.
