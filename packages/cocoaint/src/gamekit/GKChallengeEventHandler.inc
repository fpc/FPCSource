{ Parsed from GameKit.framework GKChallengeEventHandler.h }


{$ifdef TYPES}
type
  GKChallengeEventHandlerPtr = ^GKChallengeEventHandler;
  GKChallengeEventHandlerDelegateProtocolPtr = ^GKChallengeEventHandlerDelegateProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  GKChallengeEventHandlerDelegateProtocol = objcprotocol external name 'GKChallengeEventHandlerDelegate' (NSObjectProtocol)
  optional
    procedure localPlayerDidSelectChallenge (challenge: GKChallenge); message 'localPlayerDidSelectChallenge:';
    function shouldShowBannerForLocallyReceivedChallenge (challenge: GKChallenge): ObjCBOOL; message 'shouldShowBannerForLocallyReceivedChallenge:';
    procedure localPlayerDidReceiveChallenge (challenge: GKChallenge); message 'localPlayerDidReceiveChallenge:';
    function shouldShowBannerForLocallyCompletedChallenge (challenge: GKChallenge): ObjCBOOL; message 'shouldShowBannerForLocallyCompletedChallenge:';
    procedure localPlayerDidCompleteChallenge (challenge: GKChallenge); message 'localPlayerDidCompleteChallenge:';
    function shouldShowBannerForRemotelyCompletedChallenge (challenge: GKChallenge): ObjCBOOL; message 'shouldShowBannerForRemotelyCompletedChallenge:';
    procedure remotePlayerDidCompleteChallenge (challenge: GKChallenge); message 'remotePlayerDidCompleteChallenge:';
  end;
{$endif}

{$ifdef CLASSES}

type
  GKChallengeEventHandler = objcclass external (NSObject)
  public
    class function challengeEventHandler: GKChallengeEventHandler; message 'challengeEventHandler'; deprecated 'in 10_8, 10_10, 6_0, 7_0';
    procedure setDelegate(newValue: GKChallengeEventHandlerDelegateProtocol); message 'setDelegate:';
    function delegate: GKChallengeEventHandlerDelegateProtocol; message 'delegate';
  end;
{$endif}

