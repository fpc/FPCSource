{
    This file is part of the Free Component Library (FCL).
    Minimal Windows SSPI/Schannel declarations used by schannelsslsockets.
    See COPYING.FPC for details about the copyright.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit schannelsspi;
{$ENDIF}
{$mode objfpc}{$H+}
{$packrecords c}
interface
{$IFNDEF WINDOWS}{$fatal Schannel is available on Windows only}{$ENDIF}
type
  TSecurityStatus = LongInt;
  TSecHandle = record
    Lower, Upper: PtrUInt;
  end;
  PSecHandle = ^TSecHandle;
  TSecBuffer = record
    Size, BufferType: Cardinal;
    Data: Pointer;
  end;
  PSecBuffer = ^TSecBuffer;
  TSecBufferDesc = record
    Version, Count: Cardinal;
    Buffers: PSecBuffer;
  end;
  PSecBufferDesc = ^TSecBufferDesc;
  TSecStreamSizes = record
    Header, Trailer, MaximumMessage, Buffers, BlockSize: Cardinal;
  end;
  TSchannelCred = record
    Version, CredCount: Cardinal;
    Creds, RootStore: Pointer;
    MapperCount: Cardinal;
    Mappers: Pointer;
    AlgorithmCount: Cardinal;
    Algorithms: Pointer;
    EnabledProtocols, MinCipherStrength, MaxCipherStrength,
      SessionLifespan, Flags, CredFormat: Cardinal;
  end;
  TSchCredentials = record
    Version, CredFormat, CredCount: Cardinal;
    Creds, RootStore: Pointer;
    MapperCount: Cardinal;
    Mappers: Pointer;
    SessionLifespan, Flags, TLSParameterCount: Cardinal;
    TLSParameters: Pointer;
  end;

const
  SEC_E_OK = 0;
  SEC_I_CONTINUE_NEEDED = $00090312;
  SEC_I_CONTEXT_EXPIRED = $00090317;
  SEC_I_RENEGOTIATE = $00090321;
  SEC_E_UNSUPPORTED_FUNCTION = LongInt($80090302);
  SEC_E_INTERNAL_ERROR = LongInt($80090304);
  SEC_E_INVALID_TOKEN = LongInt($80090308);
  SEC_E_UNKNOWN_CREDENTIALS = LongInt($8009030D);
  SEC_E_INCOMPLETE_MESSAGE = LongInt($80090318);
  SEC_E_ILLEGAL_MESSAGE = LongInt($80090326);
  SECBUFFER_EMPTY = 0;
  SECBUFFER_DATA = 1;
  SECBUFFER_TOKEN = 2;
  SECBUFFER_EXTRA = 5;
  SECBUFFER_STREAM_TRAILER = 6;
  SECBUFFER_STREAM_HEADER = 7;
  SECPKG_ATTR_STREAM_SIZES = 4;
  SECPKG_CRED_OUTBOUND = 2;
  ISC_REQ_REPLAY_DETECT = $00000004;
  ISC_REQ_SEQUENCE_DETECT = $00000008;
  ISC_REQ_CONFIDENTIALITY = $00000010;
  ISC_REQ_ALLOCATE_MEMORY = $00000100;
  ISC_REQ_EXTENDED_ERROR = $00004000;
  ISC_REQ_STREAM = $00008000;
  SCH_CRED_NO_DEFAULT_CREDS = $00000010;
  SCH_CRED_AUTO_CRED_VALIDATION = $00000020;
  SCH_CRED_MANUAL_CRED_VALIDATION = $00000008;
  SCH_CRED_NO_SERVERNAME_CHECK = $00000004;
  SCH_USE_STRONG_CRYPTO = $00400000;
  SCHANNEL_SHUTDOWN = 1;
  SCHANNEL_CRED_VERSION = 4;
  SCH_CREDENTIALS_VERSION = 5;
  SP_PROT_TLS1_2_CLIENT = $00000800;

function AcquireCredentialsHandleW(Principal, PackageName: PWideChar;
  Use: Cardinal; LogonID, AuthData, GetKey, GetKeyArg: Pointer;
  Credential: PSecHandle; Expiry: Pointer): TSecurityStatus; stdcall;
  external 'secur32.dll' name 'AcquireCredentialsHandleW';
function InitializeSecurityContextW(Credential, Context: PSecHandle;
  Target: PWideChar; Requirements, Reserved1, DataRep: Cardinal;
  Input: PSecBufferDesc; Reserved2: Cardinal; NewContext: PSecHandle;
  Output: PSecBufferDesc; Attributes: PCardinal; Expiry: Pointer): TSecurityStatus; stdcall;
  external 'secur32.dll' name 'InitializeSecurityContextW';
function QueryContextAttributesW(Context: PSecHandle; Attribute: Cardinal;
  Buffer: Pointer): TSecurityStatus; stdcall;
  external 'secur32.dll' name 'QueryContextAttributesW';
function EncryptMessage(Context: PSecHandle; QOP: Cardinal;
  Message: PSecBufferDesc; Sequence: Cardinal): TSecurityStatus; stdcall;
  external 'secur32.dll' name 'EncryptMessage';
function DecryptMessage(Context: PSecHandle; Message: PSecBufferDesc;
  Sequence: Cardinal; QOP: PCardinal): TSecurityStatus; stdcall;
  external 'secur32.dll' name 'DecryptMessage';
function ApplyControlToken(Context: PSecHandle; Input: PSecBufferDesc): TSecurityStatus; stdcall;
  external 'secur32.dll' name 'ApplyControlToken';
function FreeContextBuffer(Buffer: Pointer): TSecurityStatus; stdcall;
  external 'secur32.dll' name 'FreeContextBuffer';
function DeleteSecurityContext(Context: PSecHandle): TSecurityStatus; stdcall;
  external 'secur32.dll' name 'DeleteSecurityContext';
function FreeCredentialsHandle(Credential: PSecHandle): TSecurityStatus; stdcall;
  external 'secur32.dll' name 'FreeCredentialsHandle';
implementation
end.
