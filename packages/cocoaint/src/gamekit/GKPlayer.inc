{ Parsed from GameKit.framework GKPlayer.h }


{ Types from UI }
{$ifdef TYPES}

const
  GKPhotoSizeSmall = 0;
  GKPhotoSizeNormal = 1;

type
  GKPhotoSize = NSInteger;
type
  GKPhotoSizePtr = ^GKPhotoSize;

{$endif}


{$ifdef TYPES}
type
  GKPlayerPtr = ^GKPlayer;
{$endif}

{$ifdef CLASSES}

type
  GKPlayer = objcclass external (NSObject)
  public
    class procedure loadPlayersForIdentifiers_withCompletionHandler (identifiers: NSArray; completionHandler: OpaqueCBlock); message 'loadPlayersForIdentifiers:withCompletionHandler:';
    function playerID: NSString; message 'playerID';
    function displayName: NSString; message 'displayName';
    function alias: NSString; message 'alias';
  end;


type
  UI = objccategory external (GKPlayer)
    {$if defined(TARGET_OS_IPHONE)}
    procedure loadPhotoForSize_withCompletionHandler (size: GKPhotoSize; completionHandler: OpaqueCBlock); message 'loadPhotoForSize:withCompletionHandler:'; { available in 10_8, 5_0 }
    {$else}
    procedure loadPhotoForSize_withCompletionHandler (size: GKPhotoSize; completionHandler: OpaqueCBlock); message 'loadPhotoForSize:withCompletionHandler:'; { available in 10_8, 5_0 }
    {$endif}
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  GKPlayerDidChangeNotificationName: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  GKPlayer_Deprecated = objccategory external name 'Deprecated' (GKPlayer)
    function isFriend: ObjCBOOL; message 'isFriend';
  end;
{$endif}

