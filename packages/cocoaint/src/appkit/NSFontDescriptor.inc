{ Parsed from AppKit.framework NSFontDescriptor.h }


{$ifdef TYPES}
type
  NSFontDescriptorPtr = ^NSFontDescriptor;
{$endif}

{$ifdef TYPES}
type
  NSFontSymbolicTraits = cuint32;
  NSFontSymbolicTraitsPtr = ^NSFontSymbolicTraits;

const
  NSFontUnknownClass = 0 shl 28;
  NSFontOldStyleSerifsClass = 1 shl 28;
  NSFontTransitionalSerifsClass = 2 shl 28;
  NSFontModernSerifsClass = 3 shl 28;
  NSFontClarendonSerifsClass = 4 shl 28;
  NSFontSlabSerifsClass = 5 shl 28;
  NSFontFreeformSerifsClass = 7 shl 28;
  NSFontSansSerifClass = 8 shl 28;
  NSFontOrnamentalsClass = 9 shl 28;
  NSFontScriptsClass = 10 shl 28;
  NSFontSymbolicClass = 12 shl 28;

type
  NSFontFamilyClass = cuint32;
  NSFontFamilyClassPtr = ^NSFontFamilyClass;

const
  NSFontFamilyClassMask = $F0000000;
const
  NSFontItalicTrait = 1 shl 0;
  NSFontBoldTrait = 1 shl 1;
  NSFontExpandedTrait = 1 shl 5;
  NSFontCondensedTrait = 1 shl 6;
  NSFontMonoSpaceTrait = 1 shl 10;
  NSFontVerticalTrait = 1 shl 11;
  NSFontUIOptimizedTrait = 1 shl 12;
{$endif}

{$ifdef CLASSES}

type
  NSFontDescriptor = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  private
    _attributes: NSMutableDictionary;
    _reserved1: id;
    _reserved2: id;
    _reserved3: id;
    _reserved4: id;
    _reserved5: id;
  public
    function postscriptName: NSString; message 'postscriptName';
    function pointSize: CGFloat; message 'pointSize';
    function matrix: NSAffineTransform; message 'matrix';
    function symbolicTraits: NSFontSymbolicTraits; message 'symbolicTraits';
    function objectForKey (anAttribute: NSString): id; message 'objectForKey:';
    function fontAttributes: NSDictionary; message 'fontAttributes';
    class function fontDescriptorWithFontAttributes (attributes: NSDictionary): NSFontDescriptor; message 'fontDescriptorWithFontAttributes:';
    class function fontDescriptorWithName_size (fontName: NSString; size: CGFloat): NSFontDescriptor; message 'fontDescriptorWithName:size:';
    class function fontDescriptorWithName_matrix (fontName: NSString; matrix_: NSAffineTransform): NSFontDescriptor; message 'fontDescriptorWithName:matrix:';
    function initWithFontAttributes (attributes: NSDictionary): instancetype; message 'initWithFontAttributes:';
    function matchingFontDescriptorsWithMandatoryKeys (mandatoryKeys: NSSet): NSArray; message 'matchingFontDescriptorsWithMandatoryKeys:';
    function matchingFontDescriptorWithMandatoryKeys (mandatoryKeys: NSSet): NSFontDescriptor; message 'matchingFontDescriptorWithMandatoryKeys:'; { available in 10_5 }
    function fontDescriptorByAddingAttributes (attributes: NSDictionary): NSFontDescriptor; message 'fontDescriptorByAddingAttributes:';
    function fontDescriptorWithSymbolicTraits (symbolicTraits_: NSFontSymbolicTraits): NSFontDescriptor; message 'fontDescriptorWithSymbolicTraits:';
    function fontDescriptorWithSize (newPointSize: CGFloat): NSFontDescriptor; message 'fontDescriptorWithSize:';
    function fontDescriptorWithMatrix (matrix_: NSAffineTransform): NSFontDescriptor; message 'fontDescriptorWithMatrix:';
    function fontDescriptorWithFace (newFace: NSString): NSFontDescriptor; message 'fontDescriptorWithFace:';
    function fontDescriptorWithFamily (newFamily: NSString): NSFontDescriptor; message 'fontDescriptorWithFamily:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSFontFamilyAttribute: NSString; cvar; external;
  NSFontNameAttribute: NSString; cvar; external;
  NSFontFaceAttribute: NSString; cvar; external;
  NSFontSizeAttribute: NSString; cvar; external;
  NSFontVisibleNameAttribute: NSString; cvar; external;
  NSFontMatrixAttribute: NSString; cvar; external;
  NSFontVariationAttribute: NSString; cvar; external;
  NSFontCharacterSetAttribute: NSString; cvar; external;
  NSFontCascadeListAttribute: NSString; cvar; external;
  NSFontTraitsAttribute: NSString; cvar; external;
  NSFontFixedAdvanceAttribute: NSString; cvar; external;
  NSFontFeatureSettingsAttribute: NSString { available in 10_5 }; cvar; external;
  NSFontColorAttribute: NSString deprecated 'in 10_3, 10_4'; cvar; external;
  NSFontSymbolicTrait: NSString; cvar; external;
  NSFontWeightTrait: NSString; cvar; external;
  NSFontWidthTrait: NSString; cvar; external;
  NSFontSlantTrait: NSString; cvar; external;
  NSFontVariationAxisIdentifierKey: NSString; cvar; external;
  NSFontVariationAxisMinimumValueKey: NSString; cvar; external;
  NSFontVariationAxisMaximumValueKey: NSString; cvar; external;
  NSFontVariationAxisDefaultValueKey: NSString; cvar; external;
  NSFontVariationAxisNameKey: NSString; cvar; external;
  NSFontFeatureTypeIdentifierKey: NSString { available in 10_5 }; cvar; external;
  NSFontFeatureSelectorIdentifierKey: NSString { available in 10_5 }; cvar; external;
{$endif}

