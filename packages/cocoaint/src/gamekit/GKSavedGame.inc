{ Parsed from GameKit.framework GKSavedGame.h }


{$ifdef TYPES}
type
  GKSavedGamePtr = ^GKSavedGame;
{$endif}

{$ifdef CLASSES}

type
  GKSavedGame = objcclass external (NSObject, NSCopyingProtocol)
  public
    function name: NSString; message 'name';
    function deviceName: NSString; message 'deviceName';
    function modificationDate: NSDate; message 'modificationDate';
    procedure loadDataWithCompletionHandler (handler: OpaqueCBlock); message 'loadDataWithCompletionHandler:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;


type
  GKLocalPlayer_GKSavedGame = objccategory external name 'GKSavedGame' (GKLocalPlayer)
    procedure fetchSavedGamesWithCompletionHandler (handler: OpaqueCBlock); message 'fetchSavedGamesWithCompletionHandler:';
    procedure saveGameData_withName_completionHandler (data: NSData; name: NSString; handler: OpaqueCBlock); message 'saveGameData:withName:completionHandler:';
    procedure deleteSavedGamesWithName_completionHandler (name: NSString; handler: OpaqueCBlock); message 'deleteSavedGamesWithName:completionHandler:';
    procedure resolveConflictingSavedGames_withData_completionHandler (conflictingSavedGames: NSArray; data: NSData; handler: OpaqueCBlock); message 'resolveConflictingSavedGames:withData:completionHandler:';
  end;
{$endif}

