{ Parsed from GameKit.framework GKTurnBasedMatchmakerViewController.h }


{$ifdef TYPES}
type
  GKTurnBasedMatchmakerViewControllerPtr = ^GKTurnBasedMatchmakerViewController;
  GKTurnBasedMatchmakerViewControllerDelegateProtocolPtr = ^GKTurnBasedMatchmakerViewControllerDelegateProtocol;
{$endif}

{$ifdef CLASSES}
type
  GKTurnBasedMatchmakerViewController = objcclass external (NSViewController, GKViewControllerProtocol)
  private
    _remoteViewController: id;
    _turnBasedMatchmakerDelegateWeak: GKTurnBasedMatchmakerViewControllerDelegateProtocol;
    _matchRequest: GKMatchRequest;
    _showExistingMatches: ObjCBOOL;
  public
    procedure setTurnBasedMatchmakerDelegate(newValue: GKTurnBasedMatchmakerViewControllerDelegateProtocol); message 'setTurnBasedMatchmakerDelegate:';
    function turnBasedMatchmakerDelegate: GKTurnBasedMatchmakerViewControllerDelegateProtocol; message 'turnBasedMatchmakerDelegate';
    procedure setShowExistingMatches(newValue: ObjCBOOL); message 'setShowExistingMatches:';
    function showExistingMatches: ObjCBOOL; message 'showExistingMatches';
    function initWithMatchRequest (request: GKMatchRequest): id; message 'initWithMatchRequest:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  GKTurnBasedMatchmakerViewControllerDelegateProtocol = objcprotocol external name 'GKTurnBasedMatchmakerViewControllerDelegate' (NSObjectProtocol)
  required
    procedure turnBasedMatchmakerViewControllerWasCancelled (viewController: GKTurnBasedMatchmakerViewController); message 'turnBasedMatchmakerViewControllerWasCancelled:'; { available in 10_8, 5_0 }
    procedure turnBasedMatchmakerViewController_didFailWithError (viewController: GKTurnBasedMatchmakerViewController; error: NSError); message 'turnBasedMatchmakerViewController:didFailWithError:'; { available in 10_8, 5_0 }
    procedure turnBasedMatchmakerViewController_didFindMatch (viewController: GKTurnBasedMatchmakerViewController; match: GKTurnBasedMatch); message 'turnBasedMatchmakerViewController:didFindMatch:'; { available in 10_8, 5_0 }
    procedure turnBasedMatchmakerViewController_playerQuitForMatch (viewController: GKTurnBasedMatchmakerViewController; match: GKTurnBasedMatch); message 'turnBasedMatchmakerViewController:playerQuitForMatch:'; { available in 10_8, 5_0 }
  end;
{$endif}

