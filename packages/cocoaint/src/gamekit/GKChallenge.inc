{ Parsed from GameKit.framework GKChallenge.h }


{ Types from GKScore_GKChallenge }
{$ifdef TYPES}

{$endif}


{ Types from GKAchievement_GKChallenge }
{$ifdef TYPES}

{$endif}


{ Types from GKScore_GKChallengeDeprecated }
{$ifdef TYPES}

{$endif}


{ Types from GKChallengeDeprecated }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  GKChallengePtr = ^GKChallenge;
  GKScoreChallengePtr = ^GKScoreChallenge;
  GKAchievementChallengePtr = ^GKAchievementChallenge;
{$endif}

{$if defined(TARGET_OS_IPHONE)}
{$ifdef TYPES}
type
  GKChallengeComposeCompletionBlock = OpaqueCBlock;
{$endif}

{$else}
{$ifdef TYPES}
type
  GKChallengeComposeCompletionBlock = OpaqueCBlock;
{$endif}

{$endif}
{$ifdef TYPES}
type
  GKChallengeState = NSInteger;
  GKChallengeStatePtr = ^GKChallengeState;

const
  GKChallengeStateInvalid = 0;
  GKChallengeStatePending = 1;
  GKChallengeStateCompleted = 2;
  GKChallengeStateDeclined = 3;
{$endif}

{$ifdef CLASSES}

type
  GKChallenge = objcclass external (NSObject, NSCodingProtocol, NSSecureCodingProtocol)
  public
    class procedure loadReceivedChallengesWithCompletionHandler (completionHandler: OpaqueCBlock); message 'loadReceivedChallengesWithCompletionHandler:';
    procedure decline; message 'decline';
    function issuingPlayerID: NSString; message 'issuingPlayerID';
    function receivingPlayerID: NSString; message 'receivingPlayerID';
    function issuingPlayer: GKPlayer; message 'issuingPlayer';
    function receivingPlayer: GKPlayer; message 'receivingPlayer';
    function state: GKChallengeState; message 'state';
    function issueDate: NSDate; message 'issueDate';
    function completionDate: NSDate; message 'completionDate';
    function message_: NSString; message 'message';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;

type
  GKScoreChallenge = objcclass external (GKChallenge)
  public
    function score: GKScore; message 'score';
  end;

type
  GKAchievementChallenge = objcclass external (GKChallenge)
  public
    function achievement: GKAchievement; message 'achievement';
  end;


type
  GKScore_GKChallenge = objccategory external name 'GKChallenge' (GKScore)
    {$if defined(TARGET_OS_IPHONE)}
    function challengeComposeControllerWithMessage_players_completionHandler (message_: NSString; players: NSArray; completionHandler: GKChallengeComposeCompletionBlock): UIViewController; message 'challengeComposeControllerWithMessage:players:completionHandler:'; { available in 8_0 }
    {$else}
    function challengeComposeControllerWithMessage_players_completionHandler (message_: NSString; players: NSArray; completionHandler: GKChallengeComposeCompletionBlock): NSViewController; message 'challengeComposeControllerWithMessage:players:completionHandler:'; { available in 10_10 }
    {$endif}
    procedure issueChallengeToPlayers_message (playerIDs: NSArray; message_: NSString); message 'issueChallengeToPlayers:message:'; deprecated 'in 10_8, 10_10, 6_0, 7_0, "pass GKPlayers to challengeComposeControllerWithMessage:players:completionHandler: and present the view controller instead"';
    class procedure reportScores_withEligibleChallenges_withCompletionHandler (scores: NSArray; challenges: NSArray; completionHandler: OpaqueCBlock); message 'reportScores:withEligibleChallenges:withCompletionHandler:'; { available in 10_10, 7_0 }
  end;

type
  GKAchievement_GKChallenge = objccategory external name 'GKChallenge' (GKAchievement)
    {$if defined(TARGET_OS_IPHONE)}
    function challengeComposeControllerWithMessage_players_completionHandler (message_: NSString; players: NSArray; completionHandler: GKChallengeComposeCompletionBlock): UIViewController; message 'challengeComposeControllerWithMessage:players:completionHandler:'; { available in 8_0 }
    {$else}
    function challengeComposeControllerWithMessage_players_completionHandler (message_: NSString; players: NSArray; completionHandler: GKChallengeComposeCompletionBlock): NSViewController; message 'challengeComposeControllerWithMessage:players:completionHandler:'; { available in 10_10 }
    {$endif}
    procedure issueChallengeToPlayers_message (playerIDs: NSArray; message_: NSString); message 'issueChallengeToPlayers:message:'; deprecated 'in 10_8, 10_10, 6_0, 7_0, "pass GKPlayers to challengeComposeControllerWithMessage:players:completionHandler: and present the view controller instead"';
    procedure selectChallengeablePlayers_withCompletionHandler (players: NSArray; completionHandler: OpaqueCBlock); message 'selectChallengeablePlayers:withCompletionHandler:'; { available in 10_10, 8_0 }
    class procedure reportAchievements_withEligibleChallenges_withCompletionHandler (achievements: NSArray; challenges: NSArray; completionHandler: OpaqueCBlock); message 'reportAchievements:withEligibleChallenges:withCompletionHandler:'; { available in 10_10, 7_0 }
  end;

type
  GKScore_GKChallengeDeprecated = objccategory external name 'GKChallengeDeprecated' (GKScore)
    {$if defined(TARGET_OS_IPHONE)}
    function challengeComposeControllerWithPlayers_message_completionHandler (playerIDs: NSArray; message_: NSString; completionHandler: GKChallengeComposeCompletionBlock): UIViewController; message 'challengeComposeControllerWithPlayers:message:completionHandler:'; deprecated 'in 7_0, 8_0, "pass GKPlayers to challengeComposeControllerWithMessage:players:"';
    {$endif}
    procedure issueChallengeToPlayers_message (playerIDs: NSArray; message_: NSString); message 'issueChallengeToPlayers:message:'; deprecated 'in 10_8, 10_10, 6_0, 7_0, "pass GKPlayers to challengeComposeControllerWithMessage:players:completionHandler: and present the view controller instead"';
  end;

type
  GKChallengeDeprecated = objccategory external (GKAchievement)
    procedure selectChallengeablePlayerIDs_withCompletionHandler (playerIDs: NSArray; completionHandler: OpaqueCBlock); message 'selectChallengeablePlayerIDs:withCompletionHandler:'; deprecated 'in 10_8, 10_10, 6_0, 8_0, "pass GKPlayers to selectChallengeablePlayers:"';
    procedure issueChallengeToPlayers_message (playerIDs: NSArray; message_: NSString); message 'issueChallengeToPlayers:message:'; deprecated 'in 10_8, 10_10, 6_0, 7_0, "pass GKPlayers to challengeComposeControllerWithMessage:players:completionHandler: and present the view controller instead"';
    {$if defined(TARGET_OS_IPHONE)}
    function challengeComposeControllerWithPlayers_message_completionHandler (playerIDs: NSArray; message_: NSString; completionHandler: GKChallengeComposeCompletionBlock): UIViewController; message 'challengeComposeControllerWithPlayers:message:completionHandler:'; deprecated 'in 7_0, 8_0, "pass GKPlayers to challengeComposeControllerWithMessage:players:"';
    {$endif}
  end;
{$endif}

