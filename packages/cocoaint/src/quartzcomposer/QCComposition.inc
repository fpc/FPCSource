{ Parsed from QuartzComposer.framework QCComposition.h }


{$ifdef TYPES}
type
  QCCompositionPtr = ^QCComposition;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  QCCompositionAttributeNameKey: NSString; cvar; external;
  QCCompositionAttributeDescriptionKey: NSString; cvar; external;
  QCCompositionAttributeCopyrightKey: NSString; cvar; external;
{$endif}

{$if (defined(MAC_OS_X_VERSION_10_5)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5))}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCCompositionAttributeBuiltInKey: NSString; cvar; external;
  QCCompositionAttributeIsTimeDependentKey: NSString; cvar; external;
  QCCompositionAttributeHasConsumersKey: NSString; cvar; external;
  QCCompositionAttributeCategoryKey: NSString; cvar; external;
{$endif}

{$endif}

{$if (defined(MAC_OS_X_VERSION_10_5)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5))}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCCompositionCategoryDistortion: NSString; cvar; external;
  QCCompositionCategoryStylize: NSString; cvar; external;
  QCCompositionCategoryUtility: NSString; cvar; external;
  QCCompositionInputImageKey: NSString; cvar; external;
  QCCompositionInputSourceImageKey: NSString; cvar; external;
  QCCompositionInputDestinationImageKey: NSString; cvar; external;
  QCCompositionInputRSSFeedURLKey: NSString; cvar; external;
  QCCompositionInputRSSArticleDurationKey: NSString; cvar; external;
  QCCompositionInputPreviewModeKey: NSString; cvar; external;
  QCCompositionInputXKey: NSString; cvar; external;
  QCCompositionInputYKey: NSString; cvar; external;
  QCCompositionInputScreenImageKey: NSString; cvar; external;
  QCCompositionInputAudioPeakKey: NSString; cvar; external;
  QCCompositionInputAudioSpectrumKey: NSString; cvar; external;
  QCCompositionInputTrackPositionKey: NSString; cvar; external;
  QCCompositionInputTrackInfoKey: NSString; cvar; external;
  QCCompositionInputTrackSignalKey: NSString; cvar; external;
  QCCompositionInputPrimaryColorKey: NSString; cvar; external;
  QCCompositionInputSecondaryColorKey: NSString; cvar; external;
  QCCompositionInputPaceKey: NSString; cvar; external;
{$endif}

{$ifdef __QUARTZCOMPOSER_CORE3D__}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCCompositionInputMeshKey: NSString; cvar; external;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCCompositionOutputImageKey: NSString; cvar; external;
  QCCompositionOutputWebPageURLKey: NSString; cvar; external;
{$endif}

{$ifdef __QUARTZCOMPOSER_CORE3D__}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCCompositionOutputMeshKey: NSString; cvar; external;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCCompositionProtocolGraphicAnimation: NSString; cvar; external;
  QCCompositionProtocolGraphicTransition: NSString; cvar; external;
  QCCompositionProtocolImageFilter: NSString; cvar; external;
  QCCompositionProtocolScreenSaver: NSString; cvar; external;
  QCCompositionProtocolRSSVisualizer: NSString; cvar; external;
  QCCompositionProtocolMusicVisualizer: NSString; cvar; external;
{$endif}

{$ifdef __QUARTZCOMPOSER_CORE3D__}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCCompositionProtocolMeshFilter: NSString; cvar; external;
{$endif}

{$endif}
{$ifdef CLASSES}

type
  QCComposition = objcclass external (NSObject, NSCopyingProtocol)
  private
    _protocols: NSArray;
    _attributes: NSDictionary;
    _inputKeys: NSArray;
    _outputKeys: NSArray;
    _backing: id;
    _identifier: NSString;
    _timestamp: NSTimeInterval;
    _stateOK: ObjCBOOL;
  public
    class function compositionWithFile (path: NSString): QCComposition; message 'compositionWithFile:';
    class function compositionWithData (data: NSData): QCComposition; message 'compositionWithData:';
    function protocols: NSArray; message 'protocols';
    function attributes: NSDictionary; message 'attributes';
    function inputKeys: NSArray; message 'inputKeys';
    function outputKeys: NSArray; message 'outputKeys';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$endif}
