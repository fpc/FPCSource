{ Parsed from GameKit.framework GKAchievementDescription.h }


{ Types from GKAchievementDescription_UI }
{$ifdef TYPES}


{$endif}


{$ifdef TYPES}
type
  GKAchievementDescriptionPtr = ^GKAchievementDescription;
{$endif}

{$ifdef CLASSES}

type
  GKAchievementDescription = objcclass external (NSObject, NSCodingProtocol, NSSecureCodingProtocol)
  public
    class procedure loadAchievementDescriptionsWithCompletionHandler (completionHandler: OpaqueCBlock); message 'loadAchievementDescriptionsWithCompletionHandler:';
    function identifier: NSString; message 'identifier';
    function groupIdentifier: NSString; message 'groupIdentifier';
    function title: NSString; message 'title';
    function achievedDescription: NSString; message 'achievedDescription';
    function unachievedDescription: NSString; message 'unachievedDescription';
    function maximumPoints: NSInteger; message 'maximumPoints';
    function isHidden: ObjCBOOL; message 'isHidden';
    function isReplayable: ObjCBOOL; message 'isReplayable';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

{$ifdef CLASSES}

type
  GKAchievementDescription_UI = objccategory external name 'UI' (GKAchievementDescription)
    {$if defined(TARGET_OS_IPHONE)}
    function image: UIImage; message 'image';
    procedure loadImageWithCompletionHandler (completionHandler: OpaqueCBlock); message 'loadImageWithCompletionHandler:';
    {$else}
    function image_: NSImage; message 'image';
    procedure loadImageWithCompletionHandler (completionHandler: OpaqueCBlock); message 'loadImageWithCompletionHandler:';
    {$endif}
    {$if defined(TARGET_OS_IPHONE)}
    class function incompleteAchievementImage: UIImage; message 'incompleteAchievementImage';
    class function placeholderCompletedAchievementImage: UIImage; message 'placeholderCompletedAchievementImage';
    {$else}
    class function incompleteAchievementImage: NSImage; message 'incompleteAchievementImage';
    class function placeholderCompletedAchievementImage: NSImage; message 'placeholderCompletedAchievementImage';
    {$endif}
  end;
{$endif}

