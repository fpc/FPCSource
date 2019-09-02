{ Parsed from SceneKit.framework SCNSceneSource.h }


{$ifdef TYPES}
type
  SCNSceneSourcePtr = ^SCNSceneSource;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  SCNSceneSourceAssetContributorsKey: NSString; cvar; external;
  SCNSceneSourceAssetCreatedDateKey: NSString; cvar; external;
  SCNSceneSourceAssetModifiedDateKey: NSString; cvar; external;
  SCNSceneSourceAssetUpAxisKey: NSString; cvar; external;
  SCNSceneSourceAssetUnitKey: NSString; cvar; external;
  SCNSceneSourceAssetAuthoringToolKey: NSString; cvar; external;
  SCNSceneSourceAssetAuthorKey: NSString; cvar; external;
  SCNSceneSourceAssetUnitNameKey: NSString; cvar; external;
  SCNSceneSourceAssetUnitMeterKey: NSString; cvar; external;
  SCNSceneSourceCreateNormalsIfAbsentKey: NSString; cvar; external;
  SCNSceneSourceCheckConsistencyKey: NSString; cvar; external;
  SCNSceneSourceFlattenSceneKey: NSString; cvar; external;
  SCNSceneSourceUseSafeModeKey: NSString; cvar; external;
  SCNSceneSourceAssetDirectoryURLsKey: NSString; cvar; external;
  SCNSceneSourceOverrideAssetURLsKey: NSString; cvar; external;
  SCNSceneSourceStrictConformanceKey: NSString; cvar; external;
  SCNSceneSourceConvertUnitsToMetersKey: NSString { available in 10_10, NA }; cvar; external;
  SCNSceneSourceConvertToYUpKey: NSString { available in 10_10, NA }; cvar; external;
  SCNSceneSourceAnimationImportPolicyKey: NSString { available in 10_10, 8_0 }; cvar; external;
  SCNSceneSourceAnimationImportPolicyPlay: NSString { available in 10_10, 8_0 }; cvar; external;
  SCNSceneSourceAnimationImportPolicyPlayRepeatedly: NSString { available in 10_10, 8_0 }; cvar; external;
  SCNSceneSourceAnimationImportPolicyDoNotPlay: NSString { available in 10_10, 8_0 }; cvar; external;
  SCNSceneSourceAnimationImportPolicyPlayUsingSceneTimeBase: NSString { available in 10_10, 8_0 }; cvar; external;
  SCNDetailedErrorsKey: NSString; cvar; external;
  SCNConsistencyElementIDErrorKey: NSString; cvar; external;
  SCNConsistencyElementTypeErrorKey: NSString; cvar; external;
  SCNConsistencyLineNumberErrorKey: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
const
  SCNConsistencyInvalidURIError = 1000;
  SCNConsistencyInvalidCountError = 1001;
  SCNConsistencyInvalidArgumentError = 1002;
  SCNConsistencyMissingElementError = 1003;
  SCNConsistencyMissingAttributeError = 1004;
  SCNConsistencyXMLSchemaValidationError = 1005;

type
  SCNSceneSourceStatus = NSInteger;
  SCNSceneSourceStatusPtr = ^SCNSceneSourceStatus;

const
  SCNSceneSourceStatusError = -1;
  SCNSceneSourceStatusParsing = 4;
  SCNSceneSourceStatusValidating = 8;
  SCNSceneSourceStatusProcessing = 12;
  SCNSceneSourceStatusComplete = 16;

type
  SCNSceneSourceStatusHandler = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  SCNSceneSource = objcclass external (NSObject)
  private
    _reserved: id;
  public
    class function sceneSourceWithURL_options (url: NSURL; options: NSDictionary): instancetype; message 'sceneSourceWithURL:options:';
    class function sceneSourceWithData_options (data: NSData; options: NSDictionary): instancetype; message 'sceneSourceWithData:options:';
    function initWithURL_options (url: NSURL; options: NSDictionary): id; message 'initWithURL:options:';
    function initWithData_options (data: NSData; options: NSDictionary): id; message 'initWithData:options:';
    function url: NSURL; message 'url';
    function data: NSData; message 'data';
    function sceneWithOptions_statusHandler (options: NSDictionary; statusHandler: SCNSceneSourceStatusHandler): SCNScene; message 'sceneWithOptions:statusHandler:';
    function sceneWithOptions_error (options: NSDictionary; error: NSErrorPtr): SCNScene; message 'sceneWithOptions:error:';
    function propertyForKey (key: NSString): id; message 'propertyForKey:';
    function entryWithIdentifier_withClass (uid: NSString; entryClass: pobjc_class): id; message 'entryWithIdentifier:withClass:';
    function identifiersOfEntriesWithClass (entryClass: pobjc_class): NSArray; message 'identifiersOfEntriesWithClass:';
    function entriesPassingTest (predicate: OpaqueCBlock): NSArray; message 'entriesPassingTest:'; { available in 10_9, 8_0 }
  end;
{$endif}

