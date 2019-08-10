{ Parsed from GameKit.framework GKMatchmakerViewController.h }


{ Types from GKMatchmakerViewController }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  GKMatchmakerViewControllerPtr = ^GKMatchmakerViewController;
  GKMatchmakerViewControllerDelegateProtocolPtr = ^GKMatchmakerViewControllerDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  GKMatchmakerViewController = objcclass external (NSViewController, GKViewControllerProtocol)
  private
    _remoteViewController: id;
    _matchmakerDelegateWeak: GKMatchmakerViewControllerDelegateProtocol;
    _matchRequest: GKMatchRequest;
    _acceptedInvite: GKInvite;
    _match: GKMatch;
    _hosted: ObjCBOOL;
    _finished: ObjCBOOL;
    _hostedPlayers: NSMutableArray;
  public
    procedure setMatchmakerDelegate(newValue: GKMatchmakerViewControllerDelegateProtocol); message 'setMatchmakerDelegate:';
    function matchmakerDelegate: GKMatchmakerViewControllerDelegateProtocol; message 'matchmakerDelegate';
    function matchRequest: GKMatchRequest; message 'matchRequest';
    procedure setHosted(newValue: ObjCBOOL); message 'setHosted:';
    function isHosted: ObjCBOOL; message 'isHosted';
    function initWithMatchRequest (request: GKMatchRequest): id; message 'initWithMatchRequest:';
    function initWithInvite (invite: GKInvite): id; message 'initWithInvite:';
    procedure addPlayersToMatch (match: GKMatch); message 'addPlayersToMatch:'; { available in 10_8, 5_0 }
    procedure setHostedPlayer_connected (playerID: NSString; connected: ObjCBOOL); message 'setHostedPlayer:connected:'; deprecated 'in 10_8, 10_10, 5_0, 8_0, "use setHostedPlayer:didConnect:"';
    procedure setHostedPlayer_didConnect (player: GKPlayer; connected: ObjCBOOL); message 'setHostedPlayer:didConnect:'; { available in 10_10, 8_0 }
    {$if defined(TARGET_OS_IPHONE)}
    procedure setHostedPlayerReady (playerID: NSString); message 'setHostedPlayerReady:'; deprecated 'in NA, NA, 4_1, 5_0';
    {$endif}
    procedure setDefaultInvitationMessage(newValue: NSString); message 'setDefaultInvitationMessage:';
    function defaultInvitationMessage: NSString; message 'defaultInvitationMessage';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  GKMatchmakerViewControllerDelegateProtocol = objcprotocol external name 'GKMatchmakerViewControllerDelegate' (NSObjectProtocol)
  required
    procedure matchmakerViewControllerWasCancelled (viewController: GKMatchmakerViewController); message 'matchmakerViewControllerWasCancelled:'; { available in 10_8, 4_1 }
    procedure matchmakerViewController_didFailWithError (viewController: GKMatchmakerViewController; error: NSError); message 'matchmakerViewController:didFailWithError:'; { available in 10_8, 4_1 }
  optional
    procedure matchmakerViewController_didFindMatch (viewController: GKMatchmakerViewController; match: GKMatch); message 'matchmakerViewController:didFindMatch:'; { available in 10_8, 4_1 }
    procedure matchmakerViewController_didFindHostedPlayers (viewController: GKMatchmakerViewController; players: NSArray); message 'matchmakerViewController:didFindHostedPlayers:'; { available in 10_10, 8_0 }
    procedure matchmakerViewController_didFindPlayers (viewController: GKMatchmakerViewController; playerIDs: NSArray); message 'matchmakerViewController:didFindPlayers:'; deprecated 'in 10_8, 10_10, 4_1, 8_0, "use matchmakerViewController:didFindHostedPlayers:"';
    procedure matchmakerViewController_hostedPlayerDidAccept (viewController: GKMatchmakerViewController; player: GKPlayer); message 'matchmakerViewController:hostedPlayerDidAccept:'; { available in 10_10, 8_0 }
    procedure matchmakerViewController_didReceiveAcceptFromHostedPlayer (viewController: GKMatchmakerViewController; playerID: NSString); message 'matchmakerViewController:didReceiveAcceptFromHostedPlayer:'; deprecated 'in 10_8, 10_10, 5_0, 8_0, "use matchmakerViewController:hostedPlayerDidAccept:"';
  end;
{$endif}

