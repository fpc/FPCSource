{ Parsed from CoreMediaIO.framework CMIOHardwareControl.h }

{$if  not (defined(__CMIOHardwareControl_h__))}

{$ifdef TYPES}
type
  CMIOControlID = CMIOObjectID;
  CMIOControlIDPtr = ^CMIOControlID;

const
  kCMIOControlClassID = 'actl';
  kCMIOBooleanControlClassID = 'togl';
  kCMIOSelectorControlClassID = 'slct';
  kCMIOFeatureControlClassID = 'ftct';
const
  kCMIOJackControlClassID = 'jack';
  kCMIODirectionControlClassID = 'dire';
const
  kCMIODataSourceControlClassID = 'dsrc';
  kCMIODataDestinationControlClassID = 'dest';
const
  kCMIOBlackLevelControlClassID = 'bklv';
  kCMIOWhiteLevelControlClassID = 'whlv';
  kCMIOHueControlClassID = 'hue ';
  kCMIOSaturationControlClassID = 'satu';
  kCMIOContrastControlClassID = 'ctst';
  kCMIOSharpnessControlClassID = 'shrp';
  kCMIOBrightnessControlClassID = 'brit';
  kCMIOGainControlClassID = 'gain';
  kCMIOIrisControlClassID = 'iris';
  kCMIOShutterControlClassID = 'shtr';
  kCMIOExposureControlClassID = 'xpsr';
  kCMIOWhiteBalanceUControlClassID = 'whbu';
  kCMIOWhiteBalanceVControlClassID = 'whbv';
  kCMIOWhiteBalanceControlClassID = 'whbl';
  kCMIOGammaControlClassID = 'gmma';
  kCMIOTemperatureControlClassID = 'temp';
  kCMIOZoomControlClassID = 'zoom';
  kCMIOFocusControlClassID = 'fcus';
  kCMIOPanControlClassID = 'pan ';
  kCMIOTiltControlClassID = 'tilt';
  kCMIOOpticalFilterClassID = 'opft';
  kCMIOBacklightCompensationControlClassID = 'bklt';
  kCMIOPowerLineFrequencyControlClassID = 'pwfq';
  kCMIONoiseReductionControlClassID = 's2nr';
const
  kCMIOControlPropertyScope = 'cscp';
  kCMIOControlPropertyElement = 'celm';
  kCMIOControlPropertyVariant = 'cvar';
const
  kCMIOBooleanControlPropertyValue = 'bcvl';
const
  kCMIOSelectorControlPropertyCurrentItem = 'scci';
  kCMIOSelectorControlPropertyAvailableItems = 'scai';
  kCMIOSelectorControlPropertyItemName = 'scin';
const
  kCMIOFeatureControlPropertyOnOff = 'fcoo';
  kCMIOFeatureControlPropertyAutomaticManual = 'fcam';
  kCMIOFeatureControlPropertyAbsoluteNative = 'fcna';
  kCMIOFeatureControlPropertyTune = 'fctn';
  kCMIOFeatureControlPropertyNativeValue = 'fcnv';
  kCMIOFeatureControlPropertyAbsoluteValue = 'fcav';
  kCMIOFeatureControlPropertyNativeRange = 'fcnr';
  kCMIOFeatureControlPropertyAbsoluteRange = 'fcar';
  kCMIOFeatureControlPropertyConvertNativeToAbsolute = 'fn2a';
  kCMIOFeatureControlPropertyConvertAbsoluteToNative = 'fa2n';
  kCMIOFeatureControlPropertyAbsoluteUnitName = 'fcun';
const
  kCMIOExposureControlPropertyRegionOfInterest = 'eroi';
  kCMIOExposureControlPropertyLockThreshold = 'elck';
  kCMIOExposureControlPropertyUnlockThreshold = 'eulk';
  kCMIOExposureControlPropertyTarget = 'etgt';
  kCMIOExposureControlPropertyConvergenceSpeed = 'ecsp';
  kCMIOExposureControlPropertyStability = 'esty';
  kCMIOExposureControlPropertyStable = 'estb';
  kCMIOExposureControlPropertyIntegrationTime = 'eint';
  kCMIOExposureControlPropertyMaximumGain = 'emax';
{$endif}

{$endif}
