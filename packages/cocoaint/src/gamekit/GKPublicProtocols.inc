{ Parsed from GameKit.framework GKPublicProtocols.h }


{$ifdef TYPES}
type
  GKSessionDelegateProtocolPtr = ^GKSessionDelegateProtocol;
  GKVoiceChatClientProtocolPtr = ^GKVoiceChatClientProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  GKSessionDelegateProtocol = objcprotocol external name 'GKSessionDelegate' (NSObjectProtocol)
  optional
    procedure session_peer_didChangeState (session: GKSession; peerID: NSString; state: GKPeerConnectionState); message 'session:peer:didChangeState:';
    procedure session_didReceiveConnectionRequestFromPeer (session: GKSession; peerID: NSString); message 'session:didReceiveConnectionRequestFromPeer:';
    procedure session_connectionWithPeerFailed_withError (session: GKSession; peerID: NSString; error: NSError); message 'session:connectionWithPeerFailed:withError:';
    procedure session_didFailWithError (session: GKSession; error: NSError); message 'session:didFailWithError:';
  end;

type
  GKVoiceChatClientProtocol = objcprotocol external name 'GKVoiceChatClient' (NSObjectProtocol)
  required
    procedure voiceChatService_sendData_toParticipantID (voiceChatService: GKVoiceChatService; data: NSData; participantID: NSString); message 'voiceChatService:sendData:toParticipantID:';
    function participantID: NSString; message 'participantID';
  optional
    procedure voiceChatService_sendRealTimeData_toParticipantID (voiceChatService: GKVoiceChatService; data: NSData; participantID_: NSString); message 'voiceChatService:sendRealTimeData:toParticipantID:';
    procedure voiceChatService_didStartWithParticipantID (voiceChatService: GKVoiceChatService; participantID_: NSString); message 'voiceChatService:didStartWithParticipantID:';
    procedure voiceChatService_didNotStartWithParticipantID_error (voiceChatService: GKVoiceChatService; participantID_: NSString; error: NSError); message 'voiceChatService:didNotStartWithParticipantID:error:';
    procedure voiceChatService_didStopWithParticipantID_error (voiceChatService: GKVoiceChatService; participantID_: NSString; error: NSError); message 'voiceChatService:didStopWithParticipantID:error:';
    procedure voiceChatService_didReceiveInvitationFromParticipantID_callID (voiceChatService: GKVoiceChatService; participantID_: NSString; callID: NSInteger); message 'voiceChatService:didReceiveInvitationFromParticipantID:callID:';
  end;
{$endif}

