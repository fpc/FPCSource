{ Parsed from Appkit.framework NSGlyphGenerator.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:48 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSGLYPHGENERATOR_PAS_H}
{$define NSGLYPHGENERATOR_PAS_H}
type
  NSGlyphGeneratorPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSGLYPHGENERATOR_PAS_T}
{$define NSGLYPHGENERATOR_PAS_T}

{ Constants }

const
  NSShowControlGlyphs = 1  shl  0;
  NSShowInvisibleGlyphs = 1  shl  1;
  NSWantsBidiLevels = 1  shl  2;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSGLYPHGENERATOR_PAS_R}
{$define NSGLYPHGENERATOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSGLYPHGENERATOR_PAS_F}
{$define NSGLYPHGENERATOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSGLYPHGENERATOR_PAS_T}
{$define NSGLYPHGENERATOR_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSGlyphStorageProtocol = objcprotocol;
  NSGlyphGenerator = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSGLYPHGENERATOR_PAS_C}
{$define NSGLYPHGENERATOR_PAS_C}

{ NSGlyphGenerator }
  NSGlyphGenerator = objcclass(NSObject)
    
  public
    class function alloc: NSGlyphGenerator; message 'alloc';

    procedure generateGlyphsForGlyphStorage_desiredNumberOfCharacters_glyphIndex_characterIndex(glyphStorage: id; nChars: NSUInteger; glyphIndex: NSUIntegerPointer; charIndex: NSUIntegerPointer); message 'generateGlyphsForGlyphStorage:desiredNumberOfCharacters:glyphIndex:characterIndex:';
    class function sharedGlyphGenerator: id; message 'sharedGlyphGenerator';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSGLYPHGENERATOR_PAS_P}
{$define NSGLYPHGENERATOR_PAS_P}
  
{ NSGlyphStorage Protocol }
  NSGlyphStorageProtocol = objcprotocol
    procedure insertGlyphs_length_forStartingGlyphAtIndex_characterIndex(glyphs: NSGlyphPointer; length: NSUInteger; glyphIndex: NSUInteger; charIndex: NSUInteger); message 'insertGlyphs:length:forStartingGlyphAtIndex:characterIndex:';
    procedure setIntAttribute_value_forGlyphAtIndex(attributeTag: NSInteger; val: NSInteger; glyphIndex: NSUInteger); message 'setIntAttribute:value:forGlyphAtIndex:';
    function attributedString: NSAttributedString; message 'attributedString';
    function layoutOptions: NSUInteger; message 'layoutOptions';
  end; external name 'NSGlyphStorage';
{$endif}
{$endif}
