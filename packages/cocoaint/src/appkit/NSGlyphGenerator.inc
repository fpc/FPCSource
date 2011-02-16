{ Parsed from Appkit.framework NSGlyphGenerator.h }

{$ifdef TYPES}
{$ifndef NSGLYPHGENERATOR_PAS_T}
{$define NSGLYPHGENERATOR_PAS_T}

{ Constants }

const
  NSShowControlGlyphs = 1 shl 0;
  NSShowInvisibleGlyphs = 1 shl 1;
  NSWantsBidiLevels = 1 shl 2;

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
{$ifndef NSGLYPHGENERATOR_PAS_S}
{$define NSGLYPHGENERATOR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSGlyphStorageProtocol = objcprotocol;
  NSGlyphGenerator = objcclass;
  NSGlyphGeneratorPointer = ^NSGlyphGenerator;
  NSGlyphGeneratorPtr = NSGlyphGeneratorPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSGLYPHGENERATOR_PAS_C}
{$define NSGLYPHGENERATOR_PAS_C}

{ NSGlyphGenerator }
  NSGlyphGenerator = objcclass external (NSObject)
    
  public
    procedure generateGlyphsForGlyphStorage_desiredNumberOfCharacters_glyphIndex_characterIndex(glyphStorage: NSGlyphStorageProtocol; nChars: NSUInteger; glyphIndex: NSUIntegerPtr; charIndex: NSUIntegerPtr); message 'generateGlyphsForGlyphStorage:desiredNumberOfCharacters:glyphIndex:characterIndex:';
    class function sharedGlyphGenerator: id; message 'sharedGlyphGenerator';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSGLYPHGENERATOR_PAS_P}
{$define NSGLYPHGENERATOR_PAS_P}
  
{ NSGlyphStorage Protocol }
  NSGlyphStorageProtocol = objcprotocol external name 'NSGlyphStorage'
    procedure insertGlyphs_length_forStartingGlyphAtIndex_characterIndex(glyphs: NSGlyphPtr; length: NSUInteger; glyphIndex: NSUInteger; charIndex: NSUInteger); message 'insertGlyphs:length:forStartingGlyphAtIndex:characterIndex:';
    procedure setIntAttribute_value_forGlyphAtIndex(attributeTag: NSInteger; val: NSInteger; glyphIndex: NSUInteger); message 'setIntAttribute:value:forGlyphAtIndex:';
    function attributedString: NSAttributedString; message 'attributedString';
    function layoutOptions: NSUInteger; message 'layoutOptions';
  end;
{$endif}
{$endif}
