{ Parsed from Appkit.framework NSColorSpace.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCOLORSPACE_PAS_H}
{$define NSCOLORSPACE_PAS_H}
type
  NSColorSpacePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCOLORSPACE_PAS_T}
{$define NSCOLORSPACE_PAS_T}

{ Constants }

const
  NSUnknownColorSpaceModel = -1;
  NSGrayColorSpaceModel = 0;
  NSRGBColorSpaceModel = 1;
  NSCMYKColorSpaceModel = 2;
  NSLABColorSpaceModel = 3;
  NSDeviceNColorSpaceModel = 4;
  NSIndexedColorSpaceModel = 5;
  NSPatternColorSpaceModel = 6;

{ Types }
type
  NSColorSpaceModel = NSInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCOLORSPACE_PAS_R}
{$define NSCOLORSPACE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCOLORSPACE_PAS_F}
{$define NSCOLORSPACE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCOLORSPACE_PAS_T}
{$define NSCOLORSPACE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSColorSpace = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOLORSPACE_PAS_C}
{$define NSCOLORSPACE_PAS_C}

{ NSColorSpace }
  NSColorSpace = objcclass(NSObject, NSCodingProtocol)
  private
    __profile: id;
    __flags: bitpacked record
        colorSpaceID: 0..((1 shl 8)-1);
        storageType: 0..((1 shl 3)-1);
        replacedDuringUnarchiving: 0..1;
        int: 0..((1 shl 20)-1);
        {$ifdef cpu64}
        int: 0..((1 shl 32)-1);
        {$endif}
      end;
    ___cgColorSpace: Pointer;
        _reserved: array[0..4] of Pointer;
    
  public
    class function alloc: NSColorSpace; message 'alloc';

    function initWithICCProfileData(iccData: NSData): id; message 'initWithICCProfileData:';
    function ICCProfileData: NSData; message 'ICCProfileData';
    function initWithColorSyncProfile(prof: Pointer): id; message 'initWithColorSyncProfile:';
    function colorSyncProfile: Pointer; message 'colorSyncProfile';
    function initWithCGColorSpace(CGColorSpace_: CGColorSpaceRef): id; message 'initWithCGColorSpace:';
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
    class function sRGBColorSpace: NSColorSpace; message 'sRGBColorSpace';
    class function adobeRGB1998ColorSpace: NSColorSpace; message 'adobeRGB1998ColorSpace';
  end; external;

{$endif}
{$endif}
