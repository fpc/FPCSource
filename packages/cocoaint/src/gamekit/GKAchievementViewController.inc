{ Parsed from GameKit.framework GKAchievementViewController.h }


{$ifdef TYPES}
type
  GKAchievementViewControllerPtr = ^GKAchievementViewController;
  GKAchievementViewControllerDelegateProtocolPtr = ^GKAchievementViewControllerDelegateProtocol;
{$endif}

{$ifdef CLASSES}
type
  GKAchievementViewController = objcclass external (GKGameCenterViewController)
  private
    _achievementDelegate: GKAchievementViewControllerDelegateProtocol;
  public
    procedure setAchievementDelegate(newValue: GKAchievementViewControllerDelegateProtocol); message 'setAchievementDelegate:';
    function achievementDelegate: GKAchievementViewControllerDelegateProtocol; message 'achievementDelegate';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  GKAchievementViewControllerDelegateProtocol = objcprotocol external name 'GKAchievementViewControllerDelegate' (NSObjectProtocol)
  required
    procedure achievementViewControllerDidFinish (viewController: GKAchievementViewController); message 'achievementViewControllerDidFinish:';
  end;
{$endif}

