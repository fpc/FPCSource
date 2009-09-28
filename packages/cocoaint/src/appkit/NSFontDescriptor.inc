{ Parsed from Appkit.framework NSFontDescriptor.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSFONTDESCRIPTOR_PAS_H}
{$define NSFONTDESCRIPTOR_PAS_H}
type
  NSFontDescriptorPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSFONTDESCRIPTOR_PAS_T}
{$define NSFONTDESCRIPTOR_PAS_T}

{ Types }
type
  NSFontSymbolicTraits = cardinal;
  NSFontFamilyClass = cardinal;

{ Constants }

const
  NSFontUnknownClass = 0  shl  28;
  NSFontOldStyleSerifsClass = 1  shl  28;
  NSFontTransitionalSerifsClass = 2  shl  28;
  NSFontModernSerifsClass = 3  shl  28;
  NSFontClarendonSerifsClass = 4  shl  28;
  NSFontSlabSerifsClass = 5  shl  28;
  NSFontFreeformSerifsClass = 7  shl  28;
  NSFontSansSerifClass = 8  shl  28;
  NSFontOrnamentalsClass = 9  shl  28;
  NSFontScriptsClass = 10  shl  28;
  NSFontSymbolicClass = 12  shl  28;

const
  NSFontFamilyClassMask = $F0000000;

const
  NSFontItalicTrait = 1  shl  0;
  NSFontBoldTrait = 1  shl  1;
  NSFontExpandedTrait = 1  shl  5;
  NSFontCondensedTrait = 1  shl  6;
  NSFontMonoSpaceTrait = 1  shl  10;
  NSFontVerticalTrait = 1  shl  11;
  NSFontUIOptimizedTrait = 1  shl  12;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSFONTDESCRIPTOR_PAS_R}
{$define NSFONTDESCRIPTOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSFONTDESCRIPTOR_PAS_F}
{$define NSFONTDESCRIPTOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSFONTDESCRIPTOR_PAS_S}
{$define NSFONTDESCRIPTOR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFontDescriptor = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFONTDESCRIPTOR_PAS_C}
{$define NSFONTDESCRIPTOR_PAS_C}

{ NSFontDescriptor }
  NSFontDescriptor = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __attributes: NSMutableDictionary;
    __reserved1: id;
    __reserved2: id;
    __reserved3: id;
    __reserved4: id;
    __reserved5: id;
    
  public
    class function alloc: NSFontDescriptor; message 'alloc';

    function postscriptName: NSString; message 'postscriptName';
    function pointSize: CGFloat; message 'pointSize';
    function matrix: NSAffineTransform; message 'matrix';
    function symbolicTraits: NSFontSymbolicTraits; message 'symbolicTraits';
    function objectForKey(anAttribute: NSString): id; message 'objectForKey:';
    function fontAttributes: NSDictionary; message 'fontAttributes';
    class function fontDescriptorWithFontAttributes(attributes: NSDictionary): NSFontDescriptor; message 'fontDescriptorWithFontAttributes:';
    class function fontDescriptorWithName_size(fontName: NSString; size: CGFloat): NSFontDescriptor; message 'fontDescriptorWithName:size:';
    class function fontDescriptorWithName_matrix(fontName: NSString; matrix_: NSAffineTransform): NSFontDescriptor; message 'fontDescriptorWithName:matrix:';
    function initWithFontAttributes(attributes: NSDictionary): id; message 'initWithFontAttributes:';
    function matchingFontDescriptorsWithMandatoryKeys(mandatoryKeys: NSSet): NSArray; message 'matchingFontDescriptorsWithMandatoryKeys:';
    function matchingFontDescriptorWithMandatoryKeys(mandatoryKeys: NSSet): NSFontDescriptor; message 'matchingFontDescriptorWithMandatoryKeys:';
    function fontDescriptorByAddingAttributes(attributes: NSDictionary): NSFontDescriptor; message 'fontDescriptorByAddingAttributes:';
    function fontDescriptorWithSymbolicTraits(symbolicTraits_: NSFontSymbolicTraits): NSFontDescriptor; message 'fontDescriptorWithSymbolicTraits:';
    function fontDescriptorWithSize(newPointSize: CGFloat): NSFontDescriptor; message 'fontDescriptorWithSize:';
    function fontDescriptorWithMatrix(matrix_: NSAffineTransform): NSFontDescriptor; message 'fontDescriptorWithMatrix:';
    function fontDescriptorWithFace(newFace: NSString): NSFontDescriptor; message 'fontDescriptorWithFace:';
    function fontDescriptorWithFamily(newFamily: NSString): NSFontDescriptor; message 'fontDescriptorWithFamily:';
  end; external;

{$endif}
{$endif}
