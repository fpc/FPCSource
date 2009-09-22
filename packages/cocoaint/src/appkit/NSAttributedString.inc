{ Parsed from Appkit.framework NSAttributedString.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:45 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSATTRIBUTEDSTRING_PAS_H}
{$define NSATTRIBUTEDSTRING_PAS_H}
type
  NSAttributedStringPointer = Pointer;
  NSMutableAttributedStringPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSATTRIBUTEDSTRING_PAS_T}
{$define NSATTRIBUTEDSTRING_PAS_T}

{ Constants }

const
  NSUnderlineStyleNone = $00;
  NSUnderlineStyleSingle = $01;
  NSUnderlineStyleThick = $02;
  NSUnderlineStyleDouble = $09;

const
  NSUnderlinePatternSolid = $0000;
  NSUnderlinePatternDot = $0100;
  NSUnderlinePatternDash = $0200;
  NSUnderlinePatternDashDot = $0300;
  NSUnderlinePatternDashDotDot = $0400;

const
  NSSpellingStateSpellingFlag = 1  shl  0;
  NSSpellingStateGrammarFlag = 1  shl  1;

const
  NSNoUnderlineStyle = 0;
  NSSingleUnderlineStyle = 0;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSATTRIBUTEDSTRING_PAS_R}
{$define NSATTRIBUTEDSTRING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSATTRIBUTEDSTRING_PAS_F}
{$define NSATTRIBUTEDSTRING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSATTRIBUTEDSTRING_PAS_T}
{$define NSATTRIBUTEDSTRING_PAS_T}

{ External string constants }
var
  NSFontAttributeName: NSString; external name '_NSFontAttributeName';
  NSParagraphStyleAttributeName: NSString; external name '_NSParagraphStyleAttributeName';
  NSForegroundColorAttributeName: NSString; external name '_NSForegroundColorAttributeName';
  NSUnderlineStyleAttributeName: NSString; external name '_NSUnderlineStyleAttributeName';
  NSSuperscriptAttributeName: NSString; external name '_NSSuperscriptAttributeName';
  NSBackgroundColorAttributeName: NSString; external name '_NSBackgroundColorAttributeName';
  NSAttachmentAttributeName: NSString; external name '_NSAttachmentAttributeName';
  NSLigatureAttributeName: NSString; external name '_NSLigatureAttributeName';
  NSBaselineOffsetAttributeName: NSString; external name '_NSBaselineOffsetAttributeName';
  NSKernAttributeName: NSString; external name '_NSKernAttributeName';
  NSLinkAttributeName: NSString; external name '_NSLinkAttributeName';
  NSCharacterShapeAttributeName: NSString; external name '_NSCharacterShapeAttributeName';
  NSGlyphInfoAttributeName: NSString; external name '_NSGlyphInfoAttributeName';
  NSPlainTextDocumentType: NSString; external name '_NSPlainTextDocumentType';
  NSRTFTextDocumentType: NSString; external name '_NSRTFTextDocumentType';
  NSRTFDTextDocumentType: NSString; external name '_NSRTFDTextDocumentType';
  NSMacSimpleTextDocumentType: NSString; external name '_NSMacSimpleTextDocumentType';
  NSHTMLTextDocumentType: NSString; external name '_NSHTMLTextDocumentType';

{ External symbols }
var
  NSUnderlineByWordMask: NSUInteger; external name '_NSUnderlineByWordMask';
  NSUnderlineStrikethroughMask: NSUInteger; external name '_NSUnderlineStrikethroughMask';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSAttributedString = objcclass;
  NSMutableAttributedString = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSATTRIBUTEDSTRING_PAS_C}
{$define NSATTRIBUTEDSTRING_PAS_C}

{ NSAttributedString }
  NSAttributedString = objcclass(NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSAttributedString; message 'alloc';

    function string_: NSString; message 'string';
    function attributesAtIndex_effectiveRange(location: NSUInteger; range: NSRangePointer): NSDictionary; message 'attributesAtIndex:effectiveRange:';

    { Category: NSExtendedAttributedString }
    function length: NSUInteger; message 'length';
    function attribute_atIndex_effectiveRange(attrName: NSString; location: NSUInteger; range: NSRangePointer): id; message 'attribute:atIndex:effectiveRange:';
    function attributedSubstringFromRange(range: NSRange): NSAttributedString; message 'attributedSubstringFromRange:';
    function attributesAtIndex_longestEffectiveRange_inRange(location: NSUInteger; range: NSRangePointer; rangeLimit: NSRange): NSDictionary; message 'attributesAtIndex:longestEffectiveRange:inRange:';
    function attribute_atIndex_longestEffectiveRange_inRange(attrName: NSString; location: NSUInteger; range: NSRangePointer; rangeLimit: NSRange): id; message 'attribute:atIndex:longestEffectiveRange:inRange:';
    function isEqualToAttributedString(other: NSAttributedString): Boolean; message 'isEqualToAttributedString:';
    function initWithString(str: NSString): id; message 'initWithString:';
    function initWithString_attributes(str: NSString; attrs: NSDictionary): id; message 'initWithString:attributes:';
    function initWithAttributedString(attrStr: NSAttributedString): id; message 'initWithAttributedString:';

    { Category: NSAttributedStringKitAdditions }
    function fontAttributesInRange(range: NSRange): NSDictionary; message 'fontAttributesInRange:';
    function rulerAttributesInRange(range: NSRange): NSDictionary; message 'rulerAttributesInRange:';
    function containsAttachments: Boolean; message 'containsAttachments';
    function lineBreakBeforeIndex_withinRange(location: NSUInteger; aRange: NSRange): NSUInteger; message 'lineBreakBeforeIndex:withinRange:';
    function lineBreakByHyphenatingBeforeIndex_withinRange(location: NSUInteger; aRange: NSRange): NSUInteger; message 'lineBreakByHyphenatingBeforeIndex:withinRange:';
    function doubleClickAtIndex(location: NSUInteger): NSRange; message 'doubleClickAtIndex:';
    function nextWordFromIndex_forward(location: NSUInteger; isForward: Boolean): NSUInteger; message 'nextWordFromIndex:forward:';
    function URLAtIndex_effectiveRange(location: NSUInteger; effectiveRange: NSRangePointer): NSURL; message 'URLAtIndex:effectiveRange:';
    class function textTypes: NSArray; message 'textTypes';
    class function textUnfilteredTypes: NSArray; message 'textUnfilteredTypes';
    function rangeOfTextBlock_atIndex(block: NSTextBlock; location: NSUInteger): NSRange; message 'rangeOfTextBlock:atIndex:';
    function rangeOfTextTable_atIndex(table: NSTextTable; location: NSUInteger): NSRange; message 'rangeOfTextTable:atIndex:';
    function rangeOfTextList_atIndex(list: NSTextList; location: NSUInteger): NSRange; message 'rangeOfTextList:atIndex:';
    function itemNumberInTextList_atIndex(list: NSTextList; location: NSUInteger): NSInteger; message 'itemNumberInTextList:atIndex:';
    function initWithURL_options_documentAttributes_error(url: NSURL; options: NSDictionary; dict: NSDictionaryPointer; error: NSErrorPointer): id; message 'initWithURL:options:documentAttributes:error:';
    function initWithData_options_documentAttributes_error(data: NSData; options: NSDictionary; dict: NSDictionaryPointer; error: NSErrorPointer): id; message 'initWithData:options:documentAttributes:error:';
    function initWithPath_documentAttributes(path: NSString; dict: NSDictionaryPointer): id; message 'initWithPath:documentAttributes:';
    function initWithURL_documentAttributes(url: NSURL; dict: NSDictionaryPointer): id; message 'initWithURL:documentAttributes:';
    function initWithRTF_documentAttributes(data: NSData; dict: NSDictionaryPointer): id; message 'initWithRTF:documentAttributes:';
    function initWithRTFD_documentAttributes(data: NSData; dict: NSDictionaryPointer): id; message 'initWithRTFD:documentAttributes:';
    function initWithHTML_documentAttributes(data: NSData; dict: NSDictionaryPointer): id; message 'initWithHTML:documentAttributes:';
    function initWithHTML_baseURL_documentAttributes(data: NSData; base: NSURL; dict: NSDictionaryPointer): id; message 'initWithHTML:baseURL:documentAttributes:';
    function initWithDocFormat_documentAttributes(data: NSData; dict: NSDictionaryPointer): id; message 'initWithDocFormat:documentAttributes:';
    function initWithHTML_options_documentAttributes(data: NSData; options: NSDictionary; dict: NSDictionaryPointer): id; message 'initWithHTML:options:documentAttributes:';
    function initWithRTFDFileWrapper_documentAttributes(wrapper: NSFileWrapper; dict: NSDictionaryPointer): id; message 'initWithRTFDFileWrapper:documentAttributes:';
    function dataFromRange_documentAttributes_error(range: NSRange; dict: NSDictionary; error: NSErrorPointer): NSData; message 'dataFromRange:documentAttributes:error:';
    function fileWrapperFromRange_documentAttributes_error(range: NSRange; dict: NSDictionary; error: NSErrorPointer): NSFileWrapper; message 'fileWrapperFromRange:documentAttributes:error:';
    function RTFFromRange_documentAttributes(range: NSRange; dict: NSDictionary): NSData; message 'RTFFromRange:documentAttributes:';
    function RTFDFromRange_documentAttributes(range: NSRange; dict: NSDictionary): NSData; message 'RTFDFromRange:documentAttributes:';
    function RTFDFileWrapperFromRange_documentAttributes(range: NSRange; dict: NSDictionary): NSFileWrapper; message 'RTFDFileWrapperFromRange:documentAttributes:';
    function docFormatFromRange_documentAttributes(range: NSRange; dict: NSDictionary): NSData; message 'docFormatFromRange:documentAttributes:';

    { Category: NSDeprecatedKitAdditions }
    class function textFileTypes: NSArray; message 'textFileTypes';
    class function textPasteboardTypes: NSArray; message 'textPasteboardTypes';
    class function textUnfilteredFileTypes: NSArray; message 'textUnfilteredFileTypes';
    class function textUnfilteredPasteboardTypes: NSArray; message 'textUnfilteredPasteboardTypes';

    { Category: NSAttributedStringAttachmentConveniences }
    class function attributedStringWithAttachment(attachment: NSTextAttachment): NSAttributedString; message 'attributedStringWithAttachment:';

    { Category: NSStringDrawing }
    function size: NSSize; message 'size';
    procedure drawAtPoint(point: NSPoint); message 'drawAtPoint:';
    procedure drawInRect(rect: NSRect); message 'drawInRect:';

    { Category: NSExtendedStringDrawing }
    procedure drawWithRect_options(rect: NSRect; options: NSStringDrawingOptions); message 'drawWithRect:options:';
    function boundingRectWithSize_options(size_: NSSize; options: NSStringDrawingOptions): NSRect; message 'boundingRectWithSize:options:';
  end; external;

{ NSMutableAttributedString }
  NSMutableAttributedString = objcclass(NSAttributedString)
    
  public
    class function alloc: NSMutableAttributedString; message 'alloc';

    procedure replaceCharactersInRange_withString(range: NSRange; str: NSString); message 'replaceCharactersInRange:withString:';
    procedure setAttributes_range(attrs: NSDictionary; range: NSRange); message 'setAttributes:range:';

    { Category: NSExtendedMutableAttributedString }
    function mutableString: NSMutableString; message 'mutableString';
    procedure addAttribute_value_range(name: NSString; value: id; range: NSRange); message 'addAttribute:value:range:';
    procedure addAttributes_range(attrs: NSDictionary; range: NSRange); message 'addAttributes:range:';
    procedure removeAttribute_range(name: NSString; range: NSRange); message 'removeAttribute:range:';
    procedure replaceCharactersInRange_withAttributedString(range: NSRange; attrString: NSAttributedString); message 'replaceCharactersInRange:withAttributedString:';
    procedure insertAttributedString_atIndex(attrString: NSAttributedString; loc: NSUInteger); message 'insertAttributedString:atIndex:';
    procedure appendAttributedString(attrString: NSAttributedString); message 'appendAttributedString:';
    procedure deleteCharactersInRange(range: NSRange); message 'deleteCharactersInRange:';
    procedure setAttributedString(attrString: NSAttributedString); message 'setAttributedString:';
    procedure beginEditing; message 'beginEditing';
    procedure endEditing; message 'endEditing';

    { Category: NSMutableAttributedStringKitAdditions }
    function readFromURL_options_documentAttributes_error(url: NSURL; opts: NSDictionary; dict: NSDictionaryPointer; error: NSErrorPointer): Boolean; message 'readFromURL:options:documentAttributes:error:';
    function readFromData_options_documentAttributes_error(data: NSData; opts: NSDictionary; dict: NSDictionaryPointer; error: NSErrorPointer): Boolean; message 'readFromData:options:documentAttributes:error:';
    function readFromURL_options_documentAttributes(url: NSURL; options: NSDictionary; dict: NSDictionaryPointer): Boolean; message 'readFromURL:options:documentAttributes:';
    function readFromData_options_documentAttributes(data: NSData; options: NSDictionary; dict: NSDictionaryPointer): Boolean; message 'readFromData:options:documentAttributes:';
    procedure superscriptRange(range: NSRange); message 'superscriptRange:';
    procedure subscriptRange(range: NSRange); message 'subscriptRange:';
    procedure unscriptRange(range: NSRange); message 'unscriptRange:';
    procedure applyFontTraits_range(traitMask: NSFontTraitMask; range: NSRange); message 'applyFontTraits:range:';
    procedure setAlignment_range(alignment: NSTextAlignment; range: NSRange); message 'setAlignment:range:';
    procedure setBaseWritingDirection_range(writingDirection: NSWritingDirection; range: NSRange); message 'setBaseWritingDirection:range:';
    procedure fixAttributesInRange(range: NSRange); message 'fixAttributesInRange:';
    procedure fixFontAttributeInRange(range: NSRange); message 'fixFontAttributeInRange:';
    procedure fixParagraphStyleAttributeInRange(range: NSRange); message 'fixParagraphStyleAttributeInRange:';
    procedure fixAttachmentAttributeInRange(range: NSRange); message 'fixAttachmentAttributeInRange:';

    { Category: NSMutableAttributedStringAttachmentConveniences }
    procedure updateAttachmentsFromPath(path: NSString); message 'updateAttachmentsFromPath:';
  end; external;

{$endif}
{$endif}
