{ Parsed from GameKit.framework GKChallengesViewController.h }


{$ifdef TYPES}
type
  GKChallengesViewControllerPtr = ^GKChallengesViewController;
  GKChallengesViewControllerDelegateProtocolPtr = ^GKChallengesViewControllerDelegateProtocol;
{$endif}

{$ifdef CLASSES}
type
  GKChallengesViewController = objcclass external (NSViewController, GKViewControllerProtocol)
  private
    _internal1, _internal2, _internal3, _internal4: id;
  public
    procedure setChallengeDelegate(newValue: GKChallengesViewControllerDelegateProtocol); message 'setChallengeDelegate:';
    function challengeDelegate: GKChallengesViewControllerDelegateProtocol; message 'challengeDelegate';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  GKChallengesViewControllerDelegateProtocol = objcprotocol external name 'GKChallengesViewControllerDelegate'
  required
    procedure challengesViewControllerDidFinish (viewController: GKChallengesViewController); message 'challengesViewControllerDidFinish:';
  end;
{$endif}
