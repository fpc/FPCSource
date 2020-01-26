{ Parsed from SyncServices.framework SyncServicesErrors.h }

{$ifdef EXTERNAL_SYMBOLS}
var
  ISyncErrorDomain: NSString deprecated 'in 10_5, 10_7'; cvar; external;
{$endif}

{$ifdef TYPES}
const
  ISyncSessionClientAlreadySyncingError = 100;
  ISyncSessionUserCanceledSessionError = 101;
  ISyncSessionDriverRegistrationError = 200;
  ISyncSessionDriverPullFailureError = 201;
  ISyncSessionDriverFatalError = 300;
const
  ISyncServerDisabledReasonNone = 1000;
  ISyncServerDisabledReasonByPreference = 1001;
  ISyncServerDisabledReasonSharedNetworkHome = 1002;
  ISyncServerDisabledReasonUnresponsive = 1003;
  ISyncServerDisabledReasonUnknown = 1004;
type
  ISyncServerDisabledReason = clong;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  ISyncInvalidSchemaException: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncInvalidArgumentsException: NSString deprecated 'in 10_4, 10_7'; cvar; external;
{$endif}

