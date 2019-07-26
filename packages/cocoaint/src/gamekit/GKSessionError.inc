{ Parsed from GameKit.framework GKSessionError.h }

{$ifdef EXTERNAL_SYMBOLS}
var
  GKSessionErrorDomain: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
const
  GKSessionInvalidParameterError = 30500;
  GKSessionPeerNotFoundError = 30501;
  GKSessionDeclinedError = 30502;
  GKSessionTimedOutError = 30503;
  GKSessionCancelledError = 30504;
  GKSessionConnectionFailedError = 30505;
  GKSessionConnectionClosedError = 30506;
  GKSessionDataTooBigError = 30507;
  GKSessionNotConnectedError = 30508;
  GKSessionCannotEnableError = 30509;
  GKSessionInProgressError = 30510;
  GKSessionConnectivityError = 30201;
  GKSessionTransportError = 30202;
  GKSessionInternalError = 30203;
  GKSessionUnknownError = 30204;
  GKSessionSystemError = 30205;
type
  GKSessionError = clong;
{$endif}

