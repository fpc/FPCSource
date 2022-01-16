{ Parsed from IOBluetooth.framework OBEX.h }

{$ifdef TYPES}
type
  OBEXError = cint32;
  OBEXErrorPtr = ^OBEXError;

const
  kOBEXErrorRangeMin = -21850;
  kOBEXErrorRangeMax = -21899;
  kOBEXSuccess = 0;
  kOBEXGeneralError = -21850;
  kOBEXNoResourcesError = -21851;
  kOBEXUnsupportedError = -21852;
  kOBEXInternalError = -21853;
  kOBEXBadArgumentError = -21854;
  kOBEXTimeoutError = -21855;
  kOBEXBadRequestError = -21856;
  kOBEXCancelledError = -21857;
  kOBEXForbiddenError = -21858;
  kOBEXUnauthorizedError = -21859;
  kOBEXNotAcceptableError = -21860;
  kOBEXConflictError = -21861;
  kOBEXMethodNotAllowedError = -21862;
  kOBEXNotFoundError = -21863;
  kOBEXNotImplementedError = -21864;
  kOBEXPreconditionFailedError = -21865;
  kOBEXSessionBusyError = -21875;
  kOBEXSessionNotConnectedError = -21876;
  kOBEXSessionBadRequestError = -21877;
  kOBEXSessionBadResponseError = -21878;
  kOBEXSessionNoTransportError = -21879;
  kOBEXSessionTransportDiedError = -21880;
  kOBEXSessionTimeoutError = -21881;
  kOBEXSessionAlreadyConnectedError = -21882;
{$endif}

{$ifdef TYPES}
const
  kOBEXHeaderIDName = $01;
  kOBEXHeaderIDDescription = $05;
  kOBEXHeaderIDReservedRangeStart = $10;
  kOBEXHeaderIDReservedRangeEnd = $2F;
  kOBEXHeaderIDUserDefinedRangeStart = $30;
  kOBEXHeaderIDUserDefinedRangeEnd = $3F;
  kOBEXHeaderIDType = $42;
  kOBEXHeaderIDTimeISO = $44;
  kOBEXHeaderIDTarget = $46;
  kOBEXHeaderIDHTTP = $47;
  kOBEXHeaderIDBody = $48;
  kOBEXHeaderIDEndOfBody = $49;
  kOBEXHeaderIDWho = $4A;
  kOBEXHeaderIDAppParameters = $4C;
  kOBEXHeaderIDAuthorizationChallenge = $4D;
  kOBEXHeaderIDAuthorizationResponse = $4E;
  kOBEXHeaderIDObjectClass = $4F;
  kOBEXHeaderIDCount = $C0;
  kOBEXHeaderIDLength = $C3;
  kOBEXHeaderIDTime4Byte = $C4;
  kOBEXHeaderIDConnectionID = $CB;
  kOBEXHeaderIDOBEX13WANUUID = $50;
  kOBEXHeaderIDOBEX13ObjectClass = $51;
  kOBEXHeaderIDOBEX13SessionParameters = $52;
  kOBEXHeaderIDOBEX13SessionSequenceNumber = $93;
  kOBEXHeaderIDOBEX13CreatorID = $CF;
{$endif}

{$ifdef TYPES}
const
  kOBEXResponseCodeReservedRangeStart = $00;
  kOBEXResponseCodeReservedRangeEnd = $0F;
  kOBEXResponseCodeContinue = $10;
  kOBEXResponseCodeContinueWithFinalBit = $90;
  kOBEXResponseCodeSuccess = $20;
  kOBEXResponseCodeSuccessWithFinalBit = $A0;
  kOBEXResponseCodeCreated = $21;
  kOBEXResponseCodeCreatedWithFinalBit = $A1;
  kOBEXResponseCodeAccepted = $22;
  kOBEXResponseCodeAcceptedWithFinalBit = $A2;
  kOBEXResponseCodeNonAuthoritativeInfo = $23;
  kOBEXResponseCodeNonAuthoritativeInfoWithFinalBit = $A3;
  kOBEXResponseCodeNoContent = $24;
  kOBEXResponseCodeNoContentWithFinalBit = $A4;
  kOBEXResponseCodeResetContent = $25;
  kOBEXResponseCodeResetContentWithFinalBit = $A5;
  kOBEXResponseCodePartialContent = $26;
  kOBEXResponseCodePartialContentWithFinalBit = $A6;
  kOBEXResponseCodeMultipleChoices = $30;
  kOBEXResponseCodeMultipleChoicesWithFinalBit = $B0;
  kOBEXResponseCodeMovedPermanently = $31;
  kOBEXResponseCodeMovedPermanentlyWithFinalBit = $B1;
  kOBEXResponseCodeMovedTemporarily = $32;
  kOBEXResponseCodeMovedTemporarilyWithFinalBit = $B2;
  kOBEXResponseCodeSeeOther = $33;
  kOBEXResponseCodeSeeOtherWithFinalBit = $B3;
  kOBEXResponseCodeNotModified = $34;
  kOBEXResponseCodeNotModifiedWithFinalBit = $B4;
  kOBEXResponseCodeUseProxy = $35;
  kOBEXResponseCodeUseProxyWithFinalBit = $B5;
  kOBEXResponseCodeBadRequest = $40;
  kOBEXResponseCodeBadRequestWithFinalBit = $C0;
  kOBEXResponseCodeUnauthorized = $41;
  kOBEXResponseCodeUnauthorizedWithFinalBit = $C1;
  kOBEXResponseCodePaymentRequired = $42;
  kOBEXResponseCodePaymentRequiredWithFinalBit = $C2;
  kOBEXResponseCodeForbidden = $43;
  kOBEXResponseCodeForbiddenWithFinalBit = $C3;
  kOBEXResponseCodeNotFound = $44;
  kOBEXResponseCodeNotFoundWithFinalBit = $C4;
  kOBEXResponseCodeMethodNotAllowed = $45;
  kOBEXResponseCodeMethodNotAllowedWithFinalBit = $C5;
  kOBEXResponseCodeNotAcceptable = $46;
  kOBEXResponseCodeNotAcceptableWithFinalBit = $C6;
  kOBEXResponseCodeProxyAuthenticationRequired = $47;
  kOBEXResponseCodeProxyAuthenticationRequiredWithFinalBit = $C7;
  kOBEXResponseCodeRequestTimeOut = $48;
  kOBEXResponseCodeRequestTimeOutWithFinalBit = $C8;
  kOBEXResponseCodeConflict = $49;
  kOBEXResponseCodeConflictWithFinalBit = $C9;
  kOBEXResponseCodeGone = $4A;
  kOBEXResponseCodeGoneWithFinalBit = $CA;
  kOBEXResponseCodeLengthRequired = $4B;
  kOBEXResponseCodeLengthRequiredFinalBit = $CB;
  kOBEXResponseCodePreconditionFailed = $4C;
  kOBEXResponseCodePreconditionFailedWithFinalBit = $CC;
  kOBEXResponseCodeRequestedEntityTooLarge = $4D;
  kOBEXResponseCodeRequestedEntityTooLargeWithFinalBit = $CD;
  kOBEXResponseCodeRequestURLTooLarge = $4E;
  kOBEXResponseCodeRequestURLTooLargeWithFinalBit = $CE;
  kOBEXResponseCodeUnsupportedMediaType = $4F;
  kOBEXResponseCodeUnsupportedMediaTypeWithFinalBit = $CF;
  kOBEXResponseCodeInternalServerError = $50;
  kOBEXResponseCodeInternalServerErrorWithFinalBit = $D0;
  kOBEXResponseCodeNotImplemented = $51;
  kOBEXResponseCodeNotImplementedWithFinalBit = $D1;
  kOBEXResponseCodeBadGateway = $52;
  kOBEXResponseCodeBadGatewayWithFinalBit = $D2;
  kOBEXResponseCodeServiceUnavailable = $53;
  kOBEXResponseCodeServiceUnavailableWithFinalBit = $D3;
  kOBEXResponseCodeGatewayTimeout = $54;
  kOBEXResponseCodeGatewayTimeoutWithFinalBit = $D4;
  kOBEXResponseCodeHTTPVersionNotSupported = $55;
  kOBEXResponseCodeHTTPVersionNotSupportedWithFinalBit = $D5;
  kOBEXResponseCodeDatabaseFull = $60;
  kOBEXResponseCodeDatabaseFullWithFinalBit = $E0;
  kOBEXResponseCodeDatabaseLocked = $61;
  kOBEXResponseCodeDatabaseLockedWithFinalBit = $E1;
{$endif}

{$ifdef TYPES}
const
  kOBEXOpCodeReserved = $04;
  kOBEXOpCodeConnect = $80;
  kOBEXOpCodeDisconnect = $81;
  kOBEXOpCodePut = $02;
  kOBEXOpCodePutWithHighBitSet = $82;
  kOBEXOpCodeGet = $03;
  kOBEXOpCodeGetWithHighBitSet = $83;
  kOBEXOpCodeReservedWithHighBitSet = $84;
  kOBEXOpCodeSetPath = $85;
  kOBEXOpCodeAbort = $FF;
  kOBEXOpCodeReservedRangeStart = $06;
  kOBEXOpCodeReservedRangeEnd = $0F;
  kOBEXOpCodeUserDefinedStart = $10;
  kOBEXOpCodeUserDefinedEnd = $1F;
{$endif}

{$ifdef TYPES}
const
  kOBEXConnectFlagNone = 0 shl 0;
  kOBEXConnectFlagSupportMultipleItLMPConnections = 1 shl 0;
  kOBEXConnectFlag1Reserved = 1 shl 1;
  kOBEXConnectFlag2Reserved = 1 shl 2;
  kOBEXConnectFlag3Reserved = 1 shl 3;
  kOBEXConnectFlag4Reserved = 1 shl 4;
  kOBEXConnectFlag5Reserved = 1 shl 5;
  kOBEXConnectFlag6Reserved = 1 shl 6;
  kOBEXConnectFlag7Reserved = 1 shl 7;
{$endif}

{$ifdef TYPES}
const
  kOBEXPutFlagNone = 0 shl 0;
  kOBEXPutFlagGoToParentDirFirst = 1 shl 0;
  kOBEXPutFlagDontCreateDirectory = 1 shl 1;
  kOBEXPutFlag2Reserved = 1 shl 2;
  kOBEXPutFlag3Reserved = 1 shl 3;
  kOBEXPutFlag4Reserved = 1 shl 4;
  kOBEXPutFlag5Reserved = 1 shl 5;
  kOBEXPutFlag6Reserved = 1 shl 6;
  kOBEXPutFlag7Reserved = 1 shl 7;
{$endif}

{$ifdef TYPES}
const
  kOBEXNonceFlagNone = 0 shl 0;
  kOBEXNonceFlagSendUserIDInResponse = 1 shl 0;
  kOBEXNonceFlagAccessModeReadOnly = 1 shl 1;
  kOBEXNonceFlag2Reserved = 1 shl 2;
  kOBEXNonceFlag3Reserved = 1 shl 3;
  kOBEXNonceFlag4Reserved = 1 shl 4;
  kOBEXNonceFlag5Reserved = 1 shl 5;
  kOBEXNonceFlag6Reserved = 1 shl 6;
  kOBEXNonceFlag7Reserved = 1 shl 7;
{$endif}

{$ifdef TYPES}
const
  kOBEXRealmASCII = $00;
  kOBEXRealmISO88591 = $01;
  kOBEXRealmISO88592 = $02;
  kOBEXRealmISO88593 = $03;
  kOBEXRealmISO88594 = $04;
  kOBEXRealmISO88595 = $05;
  kOBEXRealmISO88596 = $06;
  kOBEXRealmISO88597 = $07;
  kOBEXRealmISO88598 = $08;
  kOBEXRealmISO88599 = $09;
  kOBEXRealmUNICODE = $FF;
{$endif}

{$ifdef TYPES}
const
  kOBEXOpCodeCreateSession = $00;
  kOBEXOpCodeCloseSession = $01;
  kOBEXOpCodeSuspendSession = $02;
  kOBEXOpCodeResumeSession = $03;
  kOBEXOpCodeSetTimeout = $04;
{$endif}

{$ifdef TYPES}
const
  kOBEXSessionParameterTagDeviceAddress = $00;
  kOBEXSessionParameterTagNonce = $01;
  kOBEXSessionParameterTagSessionID = $02;
  kOBEXSessionParameterTagNextSequenceNumber = $03;
  kOBEXSessionParameterTagTimeout = $04;
  kOBEXSessionParameterTagSessionOpcode = $05;
{$endif}

{$ifdef TYPES}
const
  kOBEXVersion10 = $10;
{$endif}

{$ifdef TYPES}
type
  OBEXHeaderIdentifier = cuint8;
  OBEXHeaderIdentifierPtr = ^OBEXHeaderIdentifier;
  OBEXVersion = cuint8;
  OBEXVersionPtr = ^OBEXVersion;
  OBEXFlags = cuint8;
  OBEXFlagsPtr = ^OBEXFlags;
  OBEXOpCode = cuint8;
  OBEXOpCodePtr = ^OBEXOpCode;
  OBEXConstants = cuint8;
  OBEXConstantsPtr = ^OBEXConstants;
  OBEXMaxPacketLength = cuint16;
  OBEXMaxPacketLengthPtr = ^OBEXMaxPacketLength;
  OBEXSessionRef = OpaqueType;
{$endif}

{$ifdef TYPES}
type
  OBEXConnectCommandResponseData = record
    serverResponseOpCode: OBEXOpCode;
    headerDataPtr: pointer;
    headerDataLength: size_t;
    maxPacketSize: OBEXMaxPacketLength;
    version: OBEXVersion;
    flags: OBEXFlags;
  end;
type
  OBEXConnectCommandResponseDataPtr = ^OBEXConnectCommandResponseData;
type
  OBEXDisconnectCommandResponseData = record
    serverResponseOpCode: OBEXOpCode;
    headerDataPtr: pointer;
    headerDataLength: size_t;
  end;
type
  OBEXDisconnectCommandResponseDataPtr = ^OBEXDisconnectCommandResponseData;
type
  OBEXPutCommandResponseData = record
    serverResponseOpCode: OBEXOpCode;
    headerDataPtr: pointer;
    headerDataLength: size_t;
  end;
type
  OBEXPutCommandResponseDataPtr = ^OBEXPutCommandResponseData;
type
  OBEXGetCommandResponseData = record
    serverResponseOpCode: OBEXOpCode;
    headerDataPtr: pointer;
    headerDataLength: size_t;
  end;
type
  OBEXGetCommandResponseDataPtr = ^OBEXGetCommandResponseData;
type
  OBEXSetPathCommandResponseData = record
    serverResponseOpCode: OBEXOpCode;
    headerDataPtr: pointer;
    headerDataLength: size_t;
    flags: OBEXFlags;
    constants: OBEXConstants;
  end;
type
  OBEXSetPathCommandResponseDataPtr = ^OBEXSetPathCommandResponseData;
type
  OBEXAbortCommandResponseData = record
    serverResponseOpCode: OBEXOpCode;
    headerDataPtr: pointer;
    headerDataLength: size_t;
  end;
type
  OBEXAbortCommandResponseDataPtr = ^OBEXAbortCommandResponseData;
{$endif}

{$ifdef TYPES}
type
  OBEXConnectCommandData = record
    headerDataPtr: pointer;
    headerDataLength: size_t;
    maxPacketSize: OBEXMaxPacketLength;
    version: OBEXVersion;
    flags: OBEXFlags;
  end;
type
  OBEXConnectCommandDataPtr = ^OBEXConnectCommandData;
type
  OBEXDisconnectCommandData = record
    headerDataPtr: pointer;
    headerDataLength: size_t;
  end;
type
  OBEXDisconnectCommandDataPtr = ^OBEXDisconnectCommandData;
type
  OBEXPutCommandData = record
    headerDataPtr: pointer;
    headerDataLength: size_t;
    bodyDataLeftToSend: size_t;
  end;
type
  OBEXPutCommandDataPtr = ^OBEXPutCommandData;
type
  OBEXGetCommandData = record
    headerDataPtr: pointer;
    headerDataLength: size_t;
  end;
type
  OBEXGetCommandDataPtr = ^OBEXGetCommandData;
type
  OBEXSetPathCommandData = record
    headerDataPtr: pointer;
    headerDataLength: size_t;
    flags: OBEXFlags;
    constants: OBEXConstants;
  end;
type
  OBEXSetPathCommandDataPtr = ^OBEXSetPathCommandData;
type
  OBEXAbortCommandData = record
    headerDataPtr: pointer;
    headerDataLength: size_t;
  end;
type
  OBEXAbortCommandDataPtr = ^OBEXAbortCommandData;
type
  OBEXErrorData = record
    error: OBEXError;
    dataPtr: pointer;
    dataLength: size_t;
  end;
type
  OBEXErrorDataPtr = ^OBEXErrorData;
{$endif}

{$ifdef TYPES}
const
  kOBEXSessionEventTypeConnectCommandResponseReceived = 'OCEC';
  kOBEXSessionEventTypeDisconnectCommandResponseReceived = 'OCED';
  kOBEXSessionEventTypePutCommandResponseReceived = 'OCEP';
  kOBEXSessionEventTypeGetCommandResponseReceived = 'OCEG';
  kOBEXSessionEventTypeSetPathCommandResponseReceived = 'OCES';
  kOBEXSessionEventTypeAbortCommandResponseReceived = 'OCEA';
  kOBEXSessionEventTypeConnectCommandReceived = 'OSEC';
  kOBEXSessionEventTypeDisconnectCommandReceived = 'OSED';
  kOBEXSessionEventTypePutCommandReceived = 'OSEP';
  kOBEXSessionEventTypeGetCommandReceived = 'OSEG';
  kOBEXSessionEventTypeSetPathCommandReceived = 'OSES';
  kOBEXSessionEventTypeAbortCommandReceived = 'OSEA';
  kOBEXSessionEventTypeError = 'OGEE';

type
  OBEXSessionEventType = cuint32;
  OBEXSessionEventTypePtr = ^OBEXSessionEventType;

type
  OBEXSessionEvent = record
    type_: OBEXSessionEventType;
    session: OBEXSessionRef;
    refCon: pointer;
    isEndOfEventData: Boolean;
    reserved1: pointer;
    reserved2: pointer;
    u: record
      case longint of
        0: (connectCommandResponseData: OBEXConnectCommandResponseData);
        1: (disconnectCommandResponseData: OBEXDisconnectCommandResponseData);
        2: (putCommandResponseData: OBEXPutCommandResponseData);
        3: (getCommandResponseData: OBEXGetCommandResponseData);
        4: (setPathCommandResponseData: OBEXSetPathCommandResponseData);
        5: (abortCommandResponseData: OBEXAbortCommandResponseData);
        6: (connectCommandData: OBEXConnectCommandData);
        7: (disconnectCommandData: OBEXDisconnectCommandData);
        8: (putCommandData: OBEXPutCommandData);
        9: (getCommandData: OBEXGetCommandData);
        10: (setPathCommandData: OBEXSetPathCommandData);
        11: (abortCommandData: OBEXAbortCommandData);
        12: (errorData: OBEXErrorData);
    end;
  end;
type
  OBEXSessionEventPtr = ^OBEXSessionEvent;

type
  OBEXSessionEventCallback = procedure (inEvent: OBEXSessionEventPtr); cdecl;
{$endif}

{$ifdef TYPES}
const
  kCharsetStringISO88591 = 'CHARSET=ISO-8859-1';
  kCharsetStringUTF8 = 'UTF-8';
  kEncodingStringQuotedPrintable = 'QUOTED-PRINTABLE';
  kEncodingStringBase64 = 'BASE-64';
  kEncodingString8Bit = '8BIT';
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kOBEXHeaderIDKeyName: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyType: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyDescription: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyTimeISO: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyTime4Byte: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyTarget: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyHTTP: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyBody: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyEndOfBody: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyWho: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyAppParameters: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyAuthorizationChallenge: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyAuthorizationResponse: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyObjectClass: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyCount: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyLength: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyConnectionID: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyByteSequence: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyUnknownUnicodeText: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyUnknownByteSequence: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyUnknown1ByteQuantity: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyUnknown4ByteQuantity: CFStringRef; cvar; external;
  kOBEXHeaderIDKeyUserDefined: CFStringRef; cvar; external;
{$endif}

