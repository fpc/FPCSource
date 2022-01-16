{ Parsed from GameKit.framework GKLocalPlayer.h }


{ Types from GKLocalPlayer }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  GKLocalPlayerPtr = ^GKLocalPlayer;
  GKLocalPlayerListenerProtocolPtr = ^GKLocalPlayerListenerProtocol;
{$endif}

{$ifdef CLASSES}

type
  GKLocalPlayer = objcclass external (GKPlayer)
  public
    class function localPlayer: GKLocalPlayer; message 'localPlayer';
    function isAuthenticated: ObjCBOOL; message 'isAuthenticated';
    function isUnderage: ObjCBOOL; message 'isUnderage';
    {$if defined(TARGET_OS_IPHONE)}
    procedure setAuthenticateHandler(newValue: OpaqueCBlock); message 'setAuthenticateHandler:';
    function authenticateHandler: OpaqueCBlock; message 'authenticateHandler';
    {$else}
    procedure setAuthenticateHandler_(newValue: OpaqueCBlock); message 'setAuthenticateHandler:';
    function authenticateHandler_: OpaqueCBlock; message 'authenticateHandler';
    {$endif}
    procedure loadFriendPlayersWithCompletionHandler (completionHandler: OpaqueCBlock); message 'loadFriendPlayersWithCompletionHandler:'; { available in 10_10, 8_0 }
    procedure setDefaultLeaderboardIdentifier_completionHandler (leaderboardIdentifier: NSString; completionHandler: OpaqueCBlock); message 'setDefaultLeaderboardIdentifier:completionHandler:'; { available in 10_10, 7_0 }
    procedure loadDefaultLeaderboardIdentifierWithCompletionHandler (completionHandler: OpaqueCBlock); message 'loadDefaultLeaderboardIdentifierWithCompletionHandler:'; { available in 10_10, 7_0 }
    procedure generateIdentityVerificationSignatureWithCompletionHandler (completionHandler: OpaqueCBlock); message 'generateIdentityVerificationSignatureWithCompletionHandler:'; { available in 10_10, 7_0 }
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  GKLocalPlayerListenerProtocol = objcprotocol external name 'GKLocalPlayerListener' (GKChallengeListenerProtocol, GKInviteEventListenerProtocol, GKTurnBasedEventListenerProtocol, GKSavedGameListenerProtocol)
  end;
{$endif}

{$ifdef CLASSES}

type
  GKLocalPlayerEvents = objccategory external (GKLocalPlayer)
    procedure registerListener (listener: GKLocalPlayerListenerProtocol); message 'registerListener:'; { available in 10_10, 7_0 }
    procedure unregisterListener (listener: GKLocalPlayerListenerProtocol); message 'unregisterListener:'; { available in 10_10, 7_0 }
    procedure unregisterAllListeners; message 'unregisterAllListeners'; { available in 10_10, 7_0 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  GKPlayerAuthenticationDidChangeNotificationName: NSString { available in 10_8, 4_1 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  GKLocalPlayer_Deprecated = objccategory external name 'Deprecated' (GKLocalPlayer)
    procedure setDefaultLeaderboardCategoryID_completionHandler (categoryID: NSString; completionHandler: OpaqueCBlock); message 'setDefaultLeaderboardCategoryID:completionHandler:'; deprecated 'in 10_8, 10_10, 6_0, 7_0,"Use setDefaultLeaderboardIdentifier:completionHandler: instead"';
    procedure loadDefaultLeaderboardCategoryIDWithCompletionHandler (completionHandler: OpaqueCBlock); message 'loadDefaultLeaderboardCategoryIDWithCompletionHandler:'; deprecated 'in 10_8, 10_10, 6_0, 7_0,"Use loadDefaultLeaderboardIdentifierWithCompletionHandler: instead"';
    procedure loadFriendsWithCompletionHandler (completionHandler: OpaqueCBlock); message 'loadFriendsWithCompletionHandler:'; deprecated 'in 10_8, 10_10, 4_1, 8_0, "use loadFriendPlayersWithCompletionHandler: instead"';
    procedure authenticateWithCompletionHandler (completionHandler: OpaqueCBlock); message 'authenticateWithCompletionHandler:'; deprecated 'in 10_8, 10_8, 4_1, 6_0, "Set the authenticateHandler instead"';
    function friends: NSArray; message 'friends';
  end;
{$endif}

