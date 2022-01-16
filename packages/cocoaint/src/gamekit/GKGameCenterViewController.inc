{ Parsed from GameKit.framework GKGameCenterViewController.h }


{$ifdef TYPES}
type
  GKGameCenterViewControllerPtr = ^GKGameCenterViewController;
  GKGameCenterControllerDelegateProtocolPtr = ^GKGameCenterControllerDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  GKGameCenterViewControllerState = NSInteger;
  GKGameCenterViewControllerStatePtr = ^GKGameCenterViewControllerState;

const
  GKGameCenterViewControllerStateDefault = -1;
  GKGameCenterViewControllerStateLeaderboards = 0;
  GKGameCenterViewControllerStateAchievements = 1;
  GKGameCenterViewControllerStateChallenges = 2;
{$endif}

{$ifdef CLASSES}
type
  GKGameCenterViewController = objcclass external (NSViewController, GKViewControllerProtocol)
  private
    _internal1, _internal2, _internal3: id;
    _viewState: GKGameCenterViewControllerState;
    _leaderboardIdentifier: NSString;
    _leaderboardCategory: NSString;
    _leaderboardTimeScope: GKLeaderboardTimeScope;
  public
    procedure setGameCenterDelegate(newValue: GKGameCenterControllerDelegateProtocol); message 'setGameCenterDelegate:';
    function gameCenterDelegate: GKGameCenterControllerDelegateProtocol; message 'gameCenterDelegate';
    procedure setViewState(newValue: GKGameCenterViewControllerState); message 'setViewState:';
    function viewState: GKGameCenterViewControllerState; message 'viewState';
  end;
{$endif}

{$ifdef CLASSES}
type
  Leaderboards = objccategory external (GKGameCenterViewController)
    procedure setLeaderboardTimeScope(newValue: GKLeaderboardTimeScope); message 'setLeaderboardTimeScope:';
    function leaderboardTimeScope: GKLeaderboardTimeScope; message 'leaderboardTimeScope';
    procedure setLeaderboardIdentifier(newValue: NSString); message 'setLeaderboardIdentifier:';
    function leaderboardIdentifier: NSString; message 'leaderboardIdentifier';
    procedure setLeaderboardCategory(newValue: NSString); message 'setLeaderboardCategory:';
    function leaderboardCategory: NSString; message 'leaderboardCategory';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  GKGameCenterControllerDelegateProtocol = objcprotocol external name 'GKGameCenterControllerDelegate' (NSObjectProtocol)
    procedure gameCenterViewControllerDidFinish (gameCenterViewController: GKGameCenterViewController); message 'gameCenterViewControllerDidFinish:'; { available in 10_9, 6_0 }
  end;
{$endif}

