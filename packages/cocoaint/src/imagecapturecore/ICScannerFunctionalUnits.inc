{ Parsed from ImageCaptureCore.framework ICScannerFunctionalUnits.h }


{$ifdef TYPES}
type
  ICScannerFeaturePtr = ^ICScannerFeature;
  ICScannerFeatureEnumerationPtr = ^ICScannerFeatureEnumeration;
  ICScannerFeatureRangePtr = ^ICScannerFeatureRange;
  ICScannerFeatureBooleanPtr = ^ICScannerFeatureBoolean;
  ICScannerFeatureTemplatePtr = ^ICScannerFeatureTemplate;
  ICScannerFunctionalUnitPtr = ^ICScannerFunctionalUnit;
  ICScannerFunctionalUnitFlatbedPtr = ^ICScannerFunctionalUnitFlatbed;
  ICScannerFunctionalUnitPositiveTransparencyPtr = ^ICScannerFunctionalUnitPositiveTransparency;
  ICScannerFunctionalUnitNegativeTransparencyPtr = ^ICScannerFunctionalUnitNegativeTransparency;
  ICScannerFunctionalUnitDocumentFeederPtr = ^ICScannerFunctionalUnitDocumentFeeder;
{$endif}

{$ifdef TYPES}
const
  ICScannerFunctionalUnitTypeFlatbed = 0;
  ICScannerFunctionalUnitTypePositiveTransparency = 1;
  ICScannerFunctionalUnitTypeNegativeTransparency = 2;
  ICScannerFunctionalUnitTypeDocumentFeeder = 3;

type
  ICScannerFunctionalUnitType = NSUInteger;
  ICScannerFunctionalUnitTypePtr = ^ICScannerFunctionalUnitType;

const
  ICScannerMeasurementUnitInches = 0;
  ICScannerMeasurementUnitCentimeters = 1;
  ICScannerMeasurementUnitPicas = 2;
  ICScannerMeasurementUnitPoints = 3;
  ICScannerMeasurementUnitTwips = 4;
  ICScannerMeasurementUnitPixels = 5;

type
  ICScannerMeasurementUnit = NSUInteger;
  ICScannerMeasurementUnitPtr = ^ICScannerMeasurementUnit;

const
  ICScannerBitDepth1Bit = 1;
  ICScannerBitDepth8Bits = 8;
  ICScannerBitDepth16Bits = 16;

type
  ICScannerBitDepth = NSUInteger;
  ICScannerBitDepthPtr = ^ICScannerBitDepth;

const
  ICScannerColorDataFormatTypeChunky = 0;
  ICScannerColorDataFormatTypePlanar = 1;

type
  ICScannerColorDataFormatType = NSUInteger;
  ICScannerColorDataFormatTypePtr = ^ICScannerColorDataFormatType;

const
  ICScannerPixelDataTypeBW = 0;
  ICScannerPixelDataTypeGray = 1;
  ICScannerPixelDataTypeRGB = 2;
  ICScannerPixelDataTypePalette = 3;
  ICScannerPixelDataTypeCMY = 4;
  ICScannerPixelDataTypeCMYK = 5;
  ICScannerPixelDataTypeYUV = 6;
  ICScannerPixelDataTypeYUVK = 7;
  ICScannerPixelDataTypeCIEXYZ = 8;

type
  ICScannerPixelDataType = NSUInteger;
  ICScannerPixelDataTypePtr = ^ICScannerPixelDataType;

const
  ICScannerDocumentTypeDefault = 0;
  ICScannerDocumentTypeA4 = 1;
  ICScannerDocumentTypeB5 = 2;
  ICScannerDocumentTypeUSLetter = 3;
  ICScannerDocumentTypeUSLegal = 4;
  ICScannerDocumentTypeA5 = 5;
  ICScannerDocumentTypeISOB4 = 6;
  ICScannerDocumentTypeISOB6 = 7;
  ICScannerDocumentTypeUSLedger = 9;
  ICScannerDocumentTypeUSExecutive = 10;
  ICScannerDocumentTypeA3 = 11;
  ICScannerDocumentTypeISOB3 = 12;
  ICScannerDocumentTypeA6 = 13;
  ICScannerDocumentTypeC4 = 14;
  ICScannerDocumentTypeC5 = 15;
  ICScannerDocumentTypeC6 = 16;
  ICScannerDocumentType4A0 = 17;
  ICScannerDocumentType2A0 = 18;
  ICScannerDocumentTypeA0 = 19;
  ICScannerDocumentTypeA1 = 20;
  ICScannerDocumentTypeA2 = 21;
  ICScannerDocumentTypeA7 = 22;
  ICScannerDocumentTypeA8 = 23;
  ICScannerDocumentTypeA9 = 24;
  ICScannerDocumentType10 = 25;
  ICScannerDocumentTypeISOB0 = 26;
  ICScannerDocumentTypeISOB1 = 27;
  ICScannerDocumentTypeISOB2 = 28;
  ICScannerDocumentTypeISOB5 = 29;
  ICScannerDocumentTypeISOB7 = 30;
  ICScannerDocumentTypeISOB8 = 31;
  ICScannerDocumentTypeISOB9 = 32;
  ICScannerDocumentTypeISOB10 = 33;
  ICScannerDocumentTypeJISB0 = 34;
  ICScannerDocumentTypeJISB1 = 35;
  ICScannerDocumentTypeJISB2 = 36;
  ICScannerDocumentTypeJISB3 = 37;
  ICScannerDocumentTypeJISB4 = 38;
  ICScannerDocumentTypeJISB6 = 39;
  ICScannerDocumentTypeJISB7 = 40;
  ICScannerDocumentTypeJISB8 = 41;
  ICScannerDocumentTypeJISB9 = 42;
  ICScannerDocumentTypeJISB10 = 43;
  ICScannerDocumentTypeC0 = 44;
  ICScannerDocumentTypeC1 = 45;
  ICScannerDocumentTypeC2 = 46;
  ICScannerDocumentTypeC3 = 47;
  ICScannerDocumentTypeC7 = 48;
  ICScannerDocumentTypeC8 = 49;
  ICScannerDocumentTypeC9 = 50;
  ICScannerDocumentTypeC10 = 51;
  ICScannerDocumentTypeUSStatement = 52;
  ICScannerDocumentTypeBusinessCard = 53;
  ICScannerDocumentTypeE = 60;
  ICScannerDocumentType3R = 61;
  ICScannerDocumentType4R = 62;
  ICScannerDocumentType5R = 63;
  ICScannerDocumentType6R = 64;
  ICScannerDocumentType8R = 65;
  ICScannerDocumentTypeS8R = 66;
  ICScannerDocumentType10R = 67;
  ICScannerDocumentTypeS10R = 68;
  ICScannerDocumentType11R = 69;
  ICScannerDocumentType12R = 70;
  ICScannerDocumentTypeS12R = 71;
  ICScannerDocumentType110 = 72;
  ICScannerDocumentTypeAPSH = 73;
  ICScannerDocumentTypeAPSC = 74;
  ICScannerDocumentTypeAPSP = 75;
  ICScannerDocumentType135 = 76;
  ICScannerDocumentTypeMF = 77;
  ICScannerDocumentTypeLF = 78;

type
  ICScannerDocumentType = NSUInteger;
  ICScannerDocumentTypePtr = ^ICScannerDocumentType;

const
  ICScannerFunctionalUnitStateReady = 1 shl 0;
  ICScannerFunctionalUnitStateScanInProgress = 1 shl 1;
  ICScannerFunctionalUnitStateOverviewScanInProgress = 1 shl 2;

type
  ICScannerFunctionalUnitState = cuint;
  ICScannerFunctionalUnitStatePtr = ^ICScannerFunctionalUnitState;

const
  ICScannerFeatureTypeEnumeration = 0;
  ICScannerFeatureTypeRange = 1;
  ICScannerFeatureTypeBoolean = 2;
  ICScannerFeatureTypeTemplate = 3;

type
  ICScannerFeatureType = NSUInteger;
  ICScannerFeatureTypePtr = ^ICScannerFeatureType;
{$endif}

{$ifdef CLASSES}

type
  ICScannerFeature = objcclass external (NSObject)
  private
    _sfProps: id;
  public
    function type_: ICScannerFeatureType; message 'type';
    function internalName: NSString; message 'internalName';
    function humanReadableName: NSString; message 'humanReadableName';
    function tooltip: NSString; message 'tooltip';
  end;

type
  ICScannerFeatureEnumeration = objcclass external (ICScannerFeature)
  private
    _evProps: id;
  public
    procedure setCurrentValue(newValue: id); message 'setCurrentValue:';
    function currentValue: id; message 'currentValue';
    function defaultValue: id; message 'defaultValue';
    function values: NSArray; message 'values';
    function menuItemLabels: NSArray; message 'menuItemLabels';
    function menuItemLabelsTooltips: NSArray; message 'menuItemLabelsTooltips';
  end;

type
  ICScannerFeatureRange = objcclass external (ICScannerFeature)
  private
    _rvProps: id;
  public
    procedure setCurrentValue(newValue: CGFloat); message 'setCurrentValue:';
    function currentValue: CGFloat; message 'currentValue';
    function defaultValue: CGFloat; message 'defaultValue';
    function minValue: CGFloat; message 'minValue';
    function maxValue: CGFloat; message 'maxValue';
    function stepSize: CGFloat; message 'stepSize';
  end;

type
  ICScannerFeatureBoolean = objcclass external (ICScannerFeature)
  private
    _bvProps: id;
  public
    procedure setValue(newValue: ObjCBOOL); message 'setValue:';
    function value: ObjCBOOL; message 'value';
  end;

type
  ICScannerFeatureTemplate = objcclass external (ICScannerFeature)
  private
    _tvProps: id;
  public
    function targets: NSArray; message 'targets';
  end;

type
  ICScannerFunctionalUnit = objcclass external (NSObject)
  private
    _fuProps: id;
  public
    function type_: ICScannerFunctionalUnitType; message 'type';
    procedure setPixelDataType(newValue: ICScannerPixelDataType); message 'setPixelDataType:';
    function pixelDataType: ICScannerPixelDataType; message 'pixelDataType';
    function supportedBitDepths: NSIndexSet; message 'supportedBitDepths';
    procedure setBitDepth(newValue: ICScannerBitDepth); message 'setBitDepth:';
    function bitDepth: ICScannerBitDepth; message 'bitDepth';
    function supportedMeasurementUnits: NSIndexSet; message 'supportedMeasurementUnits';
    procedure setMeasurementUnit(newValue: ICScannerMeasurementUnit); message 'setMeasurementUnit:';
    function measurementUnit: ICScannerMeasurementUnit; message 'measurementUnit';
    function supportedResolutions: NSIndexSet; message 'supportedResolutions';
    function preferredResolutions: NSIndexSet; message 'preferredResolutions';
    procedure setResolution(newValue: NSUInteger); message 'setResolution:';
    function resolution: NSUInteger; message 'resolution';
    function nativeXResolution: NSUInteger; message 'nativeXResolution';
    function nativeYResolution: NSUInteger; message 'nativeYResolution';
    function supportedScaleFactors: NSIndexSet; message 'supportedScaleFactors';
    function preferredScaleFactors: NSIndexSet; message 'preferredScaleFactors';
    procedure setScaleFactor(newValue: NSUInteger); message 'setScaleFactor:';
    function scaleFactor: NSUInteger; message 'scaleFactor';
    function templates: NSArray; message 'templates';
    function vendorFeatures: NSArray; message 'vendorFeatures';
    function physicalSize: NSSize; message 'physicalSize';
    procedure setScanArea(newValue: NSRect); message 'setScanArea:';
    function scanArea: NSRect; message 'scanArea';
    procedure setScanAreaOrientation(newValue: ICEXIFOrientationType); message 'setScanAreaOrientation:';
    function scanAreaOrientation: ICEXIFOrientationType; message 'scanAreaOrientation';
    function acceptsThresholdForBlackAndWhiteScanning: ObjCBOOL; message 'acceptsThresholdForBlackAndWhiteScanning';
    procedure setUsesThresholdForBlackAndWhiteScanning(newValue: ObjCBOOL); message 'setUsesThresholdForBlackAndWhiteScanning:';
    function usesThresholdForBlackAndWhiteScanning: ObjCBOOL; message 'usesThresholdForBlackAndWhiteScanning';
    function defaultThresholdForBlackAndWhiteScanning: char; message 'defaultThresholdForBlackAndWhiteScanning';
    procedure setThresholdForBlackAndWhiteScanning(newValue: char); message 'setThresholdForBlackAndWhiteScanning:';
    function thresholdForBlackAndWhiteScanning: char; message 'thresholdForBlackAndWhiteScanning';
    function state: ICScannerFunctionalUnitState; message 'state';
    function scanInProgress: ObjCBOOL; message 'scanInProgress';
    function scanProgressPercentDone: CGFloat; message 'scanProgressPercentDone';
    function canPerformOverviewScan: ObjCBOOL; message 'canPerformOverviewScan';
    function overviewScanInProgress: ObjCBOOL; message 'overviewScanInProgress';
    function overviewImage: CGImageRef; message 'overviewImage';
    procedure setOverviewResolution(newValue: NSUInteger); message 'setOverviewResolution:';
    function overviewResolution: NSUInteger; message 'overviewResolution';
  end;

type
  ICScannerFunctionalUnitFlatbed = objcclass external (ICScannerFunctionalUnit)
  private
    _fbProps: id;
  public
    function supportedDocumentTypes: NSIndexSet; message 'supportedDocumentTypes';
    procedure setDocumentType(newValue: ICScannerDocumentType); message 'setDocumentType:';
    function documentType: ICScannerDocumentType; message 'documentType';
    function documentSize: NSSize; message 'documentSize';
  end;

type
  ICScannerFunctionalUnitPositiveTransparency = objcclass external (ICScannerFunctionalUnit)
  private
    _ptrProps: id;
  public
    function supportedDocumentTypes: NSIndexSet; message 'supportedDocumentTypes';
    procedure setDocumentType(newValue: ICScannerDocumentType); message 'setDocumentType:';
    function documentType: ICScannerDocumentType; message 'documentType';
    function documentSize: NSSize; message 'documentSize';
  end;

type
  ICScannerFunctionalUnitNegativeTransparency = objcclass external (ICScannerFunctionalUnit)
  private
    _ntrProps: id;
  public
    function supportedDocumentTypes: NSIndexSet; message 'supportedDocumentTypes';
    procedure setDocumentType(newValue: ICScannerDocumentType); message 'setDocumentType:';
    function documentType: ICScannerDocumentType; message 'documentType';
    function documentSize: NSSize; message 'documentSize';
  end;

type
  ICScannerFunctionalUnitDocumentFeeder = objcclass external (ICScannerFunctionalUnit)
  private
    _dfProps: id;
  public
    function supportedDocumentTypes: NSIndexSet; message 'supportedDocumentTypes';
    procedure setDocumentType(newValue: ICScannerDocumentType); message 'setDocumentType:';
    function documentType: ICScannerDocumentType; message 'documentType';
    function documentSize: NSSize; message 'documentSize';
    function supportsDuplexScanning: ObjCBOOL; message 'supportsDuplexScanning';
    procedure setDuplexScanningEnabled(newValue: ObjCBOOL); message 'setDuplexScanningEnabled:';
    function duplexScanningEnabled: ObjCBOOL; message 'duplexScanningEnabled';
    function documentLoaded: ObjCBOOL; message 'documentLoaded';
    procedure setOddPageOrientation(newValue: ICEXIFOrientationType); message 'setOddPageOrientation:';
    function oddPageOrientation: ICEXIFOrientationType; message 'oddPageOrientation';
    procedure setEvenPageOrientation(newValue: ICEXIFOrientationType); message 'setEvenPageOrientation:';
    function evenPageOrientation: ICEXIFOrientationType; message 'evenPageOrientation';
    function reverseFeederPageOrder: ObjCBOOL; message 'reverseFeederPageOrder';
  end;
{$endif}

