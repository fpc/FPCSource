{ Parsed from Appkit.framework NSGlyphGenerator.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:02 ICT 2009 }

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

    procedure generateGlyphsForGlyphStorage_desiredNumberOfCharacters_glyphIndex_characterIndex(glyphStorage: id; nChars: culong; var glyphIndex: culong; var charIndex: culong); message 'generateGlyphsForGlyphStorage:desiredNumberOfCharacters:glyphIndex:characterIndex:';
    class function sharedGlyphGenerator: id; message 'sharedGlyphGenerator';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSGLYPHGENERATOR_PAS_P}
{$define NSGLYPHGENERATOR_PAS_P}
  
{ NSGlyphStorage Protocol }
  NSGlyphStorageProtocol = objcprotocol
    procedure insertGlyphs_length_forStartingGlyphAtIndex_characterIndex(var glyphs: NSGlyph; length: culong; glyphIndex: culong; charIndex: culong); message 'insertGlyphs:length:forStartingGlyphAtIndex:characterIndex:';
    procedure setIntAttribute_value_forGlyphAtIndex(attributeTag: clong; val: clong; glyphIndex: culong); message 'setIntAttribute:value:forGlyphAtIndex:';
    function attributedString: NSAttributedString; message 'attributedString';
    function layoutOptions: culong; message 'layoutOptions';
  end; external name 'NSGlyphStorage';
{$endif}
{$endif}
