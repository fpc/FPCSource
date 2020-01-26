{ Parsed from GameKit.framework GKScore.h }


{$ifdef TYPES}
type
  GKScorePtr = ^GKScore;
{$endif}

{$ifdef CLASSES}

type
  GKScore = objcclass external (NSObject, NSCodingProtocol, NSSecureCodingProtocol)
  public
    function initWithLeaderboardIdentifier (identifier: NSString): instancetype; message 'initWithLeaderboardIdentifier:';
    function initWithLeaderboardIdentifier_player (identifier: NSString; player: GKPlayer): instancetype; message 'initWithLeaderboardIdentifier:player:'; { available in 10_10, 8_0 }
    procedure setValue(newValue: cint64); message 'setValue:';
    function value: cint64; message 'value';
    function formattedValue: NSString; message 'formattedValue';
    procedure setLeaderboardIdentifier(newValue: NSString); message 'setLeaderboardIdentifier:';
    function leaderboardIdentifier: NSString; message 'leaderboardIdentifier';
    procedure setContext(newValue: cuint64); message 'setContext:';
    function context: cuint64; message 'context';
    function date: NSDate; message 'date';
    function player: GKPlayer; message 'player';
    function rank: NSInteger; message 'rank';
    procedure setShouldSetDefaultLeaderboard(newValue: ObjCBOOL); message 'setShouldSetDefaultLeaderboard:';
    function shouldSetDefaultLeaderboard: ObjCBOOL; message 'shouldSetDefaultLeaderboard';
    class procedure reportScores_withCompletionHandler (scores: NSArray; completionHandler: OpaqueCBlock); message 'reportScores:withCompletionHandler:'; { available in 10_8, 6_0 }

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  GKScore_Deprecated = objccategory external name 'Deprecated' (GKScore)
    function initWithLeaderboardIdentifier_forPlayer (identifier: NSString; playerID: NSString): instancetype; message 'initWithLeaderboardIdentifier:forPlayer:'; deprecated 'in 7_0, 8_0, "use initWithLeaderboardIdentifier:player:"';
    procedure reportScoreWithCompletionHandler (completionHandler: OpaqueCBlock); message 'reportScoreWithCompletionHandler:'; deprecated 'in 10_8, 10_10, 4_1, 7_0, "Use +reportScores:withCompletionhandler: instead"';
    function initWithCategory (category: NSString): instancetype; message 'initWithCategory:'; deprecated 'in 10_8, 10_10, 4_1, 7_0, "Use initWithLeaderboardIdentifier: instead"';
    function playerID: NSString; message 'playerID';
    procedure setCategory(newValue: NSString); message 'setCategory:';
    function category: NSString; message 'category';
  end;
{$endif}

