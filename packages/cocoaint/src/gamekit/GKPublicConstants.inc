{ Parsed from GameKit.framework GKPublicConstants.h }

{$ifdef TYPES}
const
  GKSendDataReliable = 0;
  GKSendDataUnreliable = 1;
type
  GKSendDataMode = clong;
const
  GKSessionModeServer = 0;
  GKSessionModeClient = 1;
  GKSessionModePeer = 2;
type
  GKSessionMode = clong;
const
  GKPeerStateAvailable = 0;
  GKPeerStateUnavailable = 1;
  GKPeerStateConnected = 2;
  GKPeerStateDisconnected = 3;
  GKPeerStateConnecting = 4;
type
  GKPeerConnectionState = clong;
const
  GKVoiceChatServiceInternalError = 32000;
  GKVoiceChatServiceNoRemotePacketsError = 32001;
  GKVoiceChatServiceUnableToConnectError = 32002;
  GKVoiceChatServiceRemoteParticipantHangupError = 32003;
  GKVoiceChatServiceInvalidCallIDError = 32004;
  GKVoiceChatServiceAudioUnavailableError = 32005;
  GKVoiceChatServiceUninitializedClientError = 32006;
  GKVoiceChatServiceClientMissingRequiredMethodsError = 32007;
  GKVoiceChatServiceRemoteParticipantBusyError = 32008;
  GKVoiceChatServiceRemoteParticipantCancelledError = 32009;
  GKVoiceChatServiceRemoteParticipantResponseInvalidError = 32010;
  GKVoiceChatServiceRemoteParticipantDeclinedInviteError = 32011;
  GKVoiceChatServiceMethodCurrentlyInvalidError = 32012;
  GKVoiceChatServiceNetworkConfigurationError = 32013;
  GKVoiceChatServiceUnsupportedRemoteVersionError = 32014;
  GKVoiceChatServiceOutOfMemoryError = 32015;
  GKVoiceChatServiceInvalidParameterError = 32016;
type
  GKVoiceChatServiceError = clong;
{$endif}

