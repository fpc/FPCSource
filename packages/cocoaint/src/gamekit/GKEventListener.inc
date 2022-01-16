{ Parsed from GameKit.framework GKEventListener.h }


{$ifdef TYPES}
type
  GKChallengeListenerProtocolPtr = ^GKChallengeListenerProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  GKChallengeListenerProtocol = objcprotocol external name 'GKChallengeListener' (NSObjectProtocol)
  optional
    procedure player_wantsToPlayChallenge (player: GKPlayer; challenge: GKChallenge); message 'player:wantsToPlayChallenge:'; { available in 10_10, 7_0 }
    procedure player_didReceiveChallenge (player: GKPlayer; challenge: GKChallenge); message 'player:didReceiveChallenge:'; { available in 10_10, 7_0 }
    procedure player_didCompleteChallenge_issuedByFriend (player: GKPlayer; challenge: GKChallenge; friendPlayer: GKPlayer); message 'player:didCompleteChallenge:issuedByFriend:'; { available in 10_10, 7_0 }
    procedure player_issuedChallengeWasCompleted_byFriend (player: GKPlayer; challenge: GKChallenge; friendPlayer: GKPlayer); message 'player:issuedChallengeWasCompleted:byFriend:'; { available in 10_10, 7_0 }
  end;
{$endif}

