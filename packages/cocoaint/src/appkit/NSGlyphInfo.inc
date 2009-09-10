{ Parsed from Appkit.framework NSGlyphInfo.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:02 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSGLYPHINFO_PAS_H}
{$define NSGLYPHINFO_PAS_H}
type
  NSGlyphInfoPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSGLYPHINFO_PAS_T}
{$define NSGLYPHINFO_PAS_T}

{ Types }
type
  NSCharacterCollection = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSGLYPHINFO_PAS_R}
{$define NSGLYPHINFO_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSGLYPHINFO_PAS_F}
{$define NSGLYPHINFO_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSGLYPHINFO_PAS_T}
{$define NSGLYPHINFO_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSGlyphInfo = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSGLYPHINFO_PAS_C}
{$define NSGLYPHINFO_PAS_C}

{ NSGlyphInfo }
  NSGlyphInfo = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __baseString: NSString;
    
  public
    class function alloc: NSGlyphInfo; message 'alloc';

    class function glyphInfoWithGlyphName_forFont_baseString(glyphName_: NSString; font: NSFont; theString: NSString): NSGlyphInfo; message 'glyphInfoWithGlyphName:forFont:baseString:';
    class function glyphInfoWithGlyph_forFont_baseString(glyph: NSGlyph; font: NSFont; theString: NSString): NSGlyphInfo; message 'glyphInfoWithGlyph:forFont:baseString:';
    class function glyphInfoWithCharacterIdentifier_collection_baseString(cid: culong; characterCollection_: NSCharacterCollection; theString: NSString): NSGlyphInfo; message 'glyphInfoWithCharacterIdentifier:collection:baseString:';
    function glyphName: NSString; message 'glyphName';
    function characterIdentifier: culong; message 'characterIdentifier';
    function characterCollection: NSCharacterCollection; message 'characterCollection';
  end; external;

{$endif}
{$endif}
