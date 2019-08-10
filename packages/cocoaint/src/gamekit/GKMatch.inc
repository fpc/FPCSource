{ Parsed from GameKit.framework GKMatch.h }


{$ifdef TYPES}
type
  GKMatchPtr = ^GKMatch;
  GKMatchDelegateProtocolPtr = ^GKMatchDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  GKMatchSendDataMode = NSInteger;
  GKMatchSendDataModePtr = ^GKMatchSendDataMode;

const
  GKMatchSendDataReliable = 0;
  GKMatchSendDataUnreliable = 1;

type
  GKPlayerConnectionState = NSInteger;
  GKPlayerConnectionStatePtr = ^GKPlayerConnectionState;

const
  GKPlayerStateUnknown = 0;
  GKPlayerStateConnected = 1;
  GKPlayerStateDisconnected = 2;
{$endif}

{$ifdef CLASSES}

type
  GKMatch = objcclass external (NSObject)
  public
    function players: NSArray; message 'players';
    procedure setDelegate(newValue: GKMatchDelegateProtocol); message 'setDelegate:';
    function delegate: GKMatchDelegateProtocol; message 'delegate';
    function expectedPlayerCount: NSUInteger; message 'expectedPlayerCount';
    function sendData_toPlayers_dataMode_error (data: NSData; players_: NSArray; mode: GKMatchSendDataMode; error: NSErrorPtr): ObjCBOOL; message 'sendData:toPlayers:dataMode:error:'; { available in 10_10, 8_0 }
    function sendDataToAllPlayers_withDataMode_error (data: NSData; mode: GKMatchSendDataMode; error: NSErrorPtr): ObjCBOOL; message 'sendDataToAllPlayers:withDataMode:error:';
    procedure disconnect; message 'disconnect';
    function voiceChatWithName (name: NSString): GKVoiceChat; message 'voiceChatWithName:';
    procedure chooseBestHostingPlayerWithCompletionHandler (completionHandler: OpaqueCBlock); message 'chooseBestHostingPlayerWithCompletionHandler:'; { available in 10_10, 8_0 }
    procedure rematchWithCompletionHandler (completionHandler: OpaqueCBlock); message 'rematchWithCompletionHandler:'; { available in 10_9, 6_0 }
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  GKMatchDelegateProtocol = objcprotocol external name 'GKMatchDelegate' (NSObjectProtocol)
  optional
    procedure match_didReceiveData_fromRemotePlayer (match: GKMatch; data: NSData; player: GKPlayer); message 'match:didReceiveData:fromRemotePlayer:'; { available in 10_10, 8_0 }
    procedure match_didReceiveData_fromPlayer (match: GKMatch; data: NSData; playerID: NSString); message 'match:didReceiveData:fromPlayer:'; deprecated 'in 10_8, 10_10, 4_1, 8_0, "use match:didReceiveData:fromRemotePlayer:"';
    procedure match_player_didChangeConnectionState (match: GKMatch; player: GKPlayer; state: GKPlayerConnectionState); message 'match:player:didChangeConnectionState:'; { available in 10_8, 4_1 }
    procedure match_player_didChangeState (match: GKMatch; playerID: NSString; state: GKPlayerConnectionState); message 'match:player:didChangeState:'; deprecated 'in 10_8, 10_8, 4_1, 8_0, "use match:player:didChangeConnectionState:"';
    procedure match_didFailWithError (match: GKMatch; error: NSError); message 'match:didFailWithError:'; { available in 10_8, 4_1 }
    function match_shouldReinviteDisconnectedPlayer (match: GKMatch; player: GKPlayer): ObjCBOOL; message 'match:shouldReinviteDisconnectedPlayer:'; { available in 10_10, 8_0 }
    function match_shouldReinvitePlayer (match: GKMatch; playerID: NSString): ObjCBOOL; message 'match:shouldReinvitePlayer:'; deprecated 'in 10_8, 10_10, 5_0, 8_0, "use shouldReinviteDisconnectedPlayer:"';
  end;
{$endif}

{$ifdef CLASSES}

type
  GKMatch_Deprecated = objccategory external name 'Deprecated' (GKMatch)
    procedure chooseBestHostPlayerWithCompletionHandler (completionHandler: OpaqueCBlock); message 'chooseBestHostPlayerWithCompletionHandler:'; deprecated 'in 10_9, 10_10, 6_0, 8_0, "use chooseBestHostingPlayerWithCompletionHandler:"';
    function sendData_toPlayers_withDataMode_error (data: NSData; playerIDs: NSArray; mode: GKMatchSendDataMode; error: NSErrorPtr): ObjCBOOL; message 'sendData:toPlayers:withDataMode:error:'; deprecated 'in 10_8, 10_10, 4_1, 8_0, "use sendData:toPlayers:dataMode:error:"';
    function playerIDs: NSArray; message 'playerIDs';
  end;
{$endif}

