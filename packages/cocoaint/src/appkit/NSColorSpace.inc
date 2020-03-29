{ Parsed from AppKit.framework NSColorSpace.h }


{$ifdef TYPES}
type
  NSColorSpacePtr = ^NSColorSpace;
{$endif}

{$ifdef TYPES}
type
  NSColorSpaceModel = NSInteger;
  NSColorSpaceModelPtr = ^NSColorSpaceModel;

const
  NSUnknownColorSpaceModel = -1;
  NSGrayColorSpaceModel = 0;
  NSRGBColorSpaceModel = 1;
  NSCMYKColorSpaceModel = 2;
  NSLABColorSpaceModel = 3;
  NSDeviceNColorSpaceModel = 4;
  NSIndexedColorSpaceModel = 5;
  NSPatternColorSpaceModel = 6;
{$endif}

{$ifdef CLASSES}

type
  NSColorSpace = objcclass external (NSObject, NSSecureCodingProtocol)
  private
    _profile: id;
    _flags: bitpacked record
      case byte of
        0: (_anonBitField__flags0: cuint);
        1: (
          colorSpaceID: 0..((1 shl 8)-1);
          storageType: 0..((1 shl 3)-1);
          replacedDuringUnarchiving: 0..1;
          int: 0..((1 shl 20)-1);
          {$if defined(__LP64__)}
          int_: 0..((1 shl 32)-1);
          {$endif}
        );
      end;
    __cgColorSpace: pointer;
    __cgColorTransform: pointer;
    _reserved:array[0..3] of pointer;
  public
    function initWithICCProfileData (iccData: NSData): instancetype; message 'initWithICCProfileData:';
    function ICCProfileData: NSData; message 'ICCProfileData';
    function initWithColorSyncProfile (prof: pointer): instancetype; message 'initWithColorSyncProfile:';
    function colorSyncProfile: pointer; message 'colorSyncProfile';
    function initWithCGColorSpace (cgColorSpace: CGColorSpaceRef): instancetype; message 'initWithCGColorSpace:'; { available in 10_5 }
    function CGColorSpace: CGColorSpaceRef; message 'CGColorSpace';
    function numberOfColorComponents: NSInteger; message 'numberOfColorComponents';
    function colorSpaceModel: NSColorSpaceModel; message 'colorSpaceModel';
    function localizedName: NSString; message 'localizedName';
    class function genericRGBColorSpace: NSColorSpace; message 'genericRGBColorSpace';
    class function genericGrayColorSpace: NSColorSpace; message 'genericGrayColorSpace';
    class function genericCMYKColorSpace: NSColorSpace; message 'genericCMYKColorSpace';
    class function deviceRGBColorSpace: NSColorSpace; message 'deviceRGBColorSpace';
    class function deviceGrayColorSpace: NSColorSpace; message 'deviceGrayColorSpace';
    class function deviceCMYKColorSpace: NSColorSpace; message 'deviceCMYKColorSpace';
    class function sRGBColorSpace: NSColorSpace; message 'sRGBColorSpace'; { available in 10_5 }
    class function genericGamma22GrayColorSpace: NSColorSpace; message 'genericGamma22GrayColorSpace'; { available in 10_6 }
    class function adobeRGB1998ColorSpace: NSColorSpace; message 'adobeRGB1998ColorSpace'; { available in 10_5 }
    class function availableColorSpacesWithModel (model: NSColorSpaceModel): NSArray; message 'availableColorSpacesWithModel:'; { available in 10_6 }

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

