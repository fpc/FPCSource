{ Parsed from Appkit.framework NSAttributedString.h }

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
  NSSpellingStateSpellingFlag = 1 shl 0;
  NSSpellingStateGrammarFlag = 1 shl 1;

const
  NSNoUnderlineStyle = 0;
  NSSingleUnderlineStyle = 1;

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
{$ifndef NSATTRIBUTEDSTRING_PAS_S}
{$define NSATTRIBUTEDSTRING_PAS_S}

{ External string constants }
var
  NSFontAttributeName: NSString; cvar; external;
  NSParagraphStyleAttributeName: NSString; cvar; external;
  NSForegroundColorAttributeName: NSString; cvar; external;
  NSUnderlineStyleAttributeName: NSString; cvar; external;
  NSSuperscriptAttributeName: NSString; cvar; external;
  NSBackgroundColorAttributeName: NSString; cvar; external;
  NSAttachmentAttributeName: NSString; cvar; external;
  NSLigatureAttributeName: NSString; cvar; external;
  NSBaselineOffsetAttributeName: NSString; cvar; external;
  NSKernAttributeName: NSString; cvar; external;
  NSLinkAttributeName: NSString; cvar; external;
  NSStrokeWidthAttributeName: NSString; cvar; external;
  NSStrokeColorAttributeName: NSString; cvar; external;
  NSUnderlineColorAttributeName: NSString; cvar; external;
  NSStrikethroughStyleAttributeName: NSString; cvar; external;
  NSStrikethroughColorAttributeName: NSString; cvar; external;
  NSShadowAttributeName: NSString; cvar; external;
  NSObliquenessAttributeName: NSString; cvar; external;
  NSExpansionAttributeName: NSString; cvar; external;
  NSCursorAttributeName: NSString; cvar; external;
  NSToolTipAttributeName: NSString; cvar; external;
  NSCharacterShapeAttributeName: NSString; cvar; external;
  NSGlyphInfoAttributeName: NSString; cvar; external;
  NSWritingDirectionAttributeName: NSString; cvar; external;
  NSMarkedClauseSegmentAttributeName: NSString; cvar; external;
  NSSpellingStateAttributeName: NSString; cvar; external;
  NSPlainTextDocumentType: NSString; cvar; external;
  NSRTFTextDocumentType: NSString; cvar; external;
  NSRTFDTextDocumentType: NSString; cvar; external;
  NSMacSimpleTextDocumentType: NSString; cvar; external;
  NSHTMLTextDocumentType: NSString; cvar; external;
  NSDocFormatTextDocumentType: NSString; cvar; external;
  NSWordMLTextDocumentType: NSString; cvar; external;
  NSWebArchiveTextDocumentType: NSString; cvar; external;
  NSOfficeOpenXMLTextDocumentType: NSString; cvar; external;
  NSOpenDocumentTextDocumentType: NSString; cvar; external;
  NSPaperSizeDocumentAttribute: NSString; cvar; external;
  NSLeftMarginDocumentAttribute: NSString; cvar; external;
  NSRightMarginDocumentAttribute: NSString; cvar; external;
  NSTopMarginDocumentAttribute: NSString; cvar; external;
  NSBottomMarginDocumentAttribute: NSString; cvar; external;
  NSViewSizeDocumentAttribute: NSString; cvar; external;
  NSViewZoomDocumentAttribute: NSString; cvar; external;
  NSViewModeDocumentAttribute: NSString; cvar; external;
  NSDocumentTypeDocumentAttribute: NSString; cvar; external;
  NSReadOnlyDocumentAttribute: NSString; cvar; external;
  NSConvertedDocumentAttribute: NSString; cvar; external;
  NSCocoaVersionDocumentAttribute: NSString; cvar; external;
  NSBackgroundColorDocumentAttribute: NSString; cvar; external;
  NSHyphenationFactorDocumentAttribute: NSString; cvar; external;
  NSDefaultTabIntervalDocumentAttribute: NSString; cvar; external;
  NSCharacterEncodingDocumentAttribute: NSString; cvar; external;
  NSFileTypeDocumentAttribute: NSString; cvar; external;
  NSTitleDocumentAttribute: NSString; cvar; external;
  NSCompanyDocumentAttribute: NSString; cvar; external;
  NSCopyrightDocumentAttribute: NSString; cvar; external;
  NSSubjectDocumentAttribute: NSString; cvar; external;
  NSAuthorDocumentAttribute: NSString; cvar; external;
  NSKeywordsDocumentAttribute: NSString; cvar; external;
  NSCommentDocumentAttribute: NSString; cvar; external;
  NSEditorDocumentAttribute: NSString; cvar; external;
  NSCreationTimeDocumentAttribute: NSString; cvar; external;
  NSModificationTimeDocumentAttribute: NSString; cvar; external;
  NSManagerDocumentAttribute: NSString; cvar; external;
  NSCategoryDocumentAttribute: NSString; cvar; external;
  NSExcludedElementsDocumentAttribute: NSString; cvar; external;
  NSTextEncodingNameDocumentAttribute: NSString; cvar; external;
  NSPrefixSpacesDocumentAttribute: NSString; cvar; external;
  NSDocumentTypeDocumentOption: NSString; cvar; external;
  NSDefaultAttributesDocumentOption: NSString; cvar; external;
  NSCharacterEncodingDocumentOption: NSString; cvar; external;
  NSTextEncodingNameDocumentOption: NSString; cvar; external;
  NSBaseURLDocumentOption: NSString; cvar; external;
  NSTimeoutDocumentOption: NSString; cvar; external;
  NSWebPreferencesDocumentOption: NSString; cvar; external;
  NSWebResourceLoadDelegateDocumentOption: NSString; cvar; external;
  NSTextSizeMultiplierDocumentOption: NSString; cvar; external;
  NSFileTypeDocumentOption: NSString; cvar; external;

{ External symbols }
var
  NSUnderlineByWordMask: NSUInteger; cvar; external;
  NSUnderlineStrikethroughMask: NSUInteger; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSAttributedString = objcclass;
  NSAttributedStringPointer = ^NSAttributedString;
  NSAttributedStringPtr = NSAttributedStringPointer;
  NSMutableAttributedString = objcclass;
  NSMutableAttributedStringPointer = ^NSMutableAttributedString;
  NSMutableAttributedStringPtr = NSMutableAttributedStringPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSATTRIBUTEDSTRING_PAS_C}
{$define NSATTRIBUTEDSTRING_PAS_C}

{ NSAttributedString }
  NSAttributedString = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol)
    
  public
    function string_: NSString; message 'string';
    function attributesAtIndex_effectiveRange(location: NSUInteger; range: NSRangePointer): NSDictionary; message 'attributesAtIndex:effectiveRange:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    function mutableCopyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSMutableAttributedString }
  NSMutableAttributedString = objcclass external (NSAttributedString)
    
  public
    procedure replaceCharactersInRange_withString(range: NSRange; str: NSString); message 'replaceCharactersInRange:withString:';
    procedure setAttributes_range(attrs: NSDictionary; range: NSRange); message 'setAttributes:range:';
  end;

{ NSExtendedAttributedStringCategory }
  NSExtendedAttributedStringCategory = objccategory external (NSAttributedString)
    function length: NSUInteger; message 'length';
    function attribute_atIndex_effectiveRange(attrName: NSString; location: NSUInteger; range: NSRangePointer): id; message 'attribute:atIndex:effectiveRange:';
    function attributedSubstringFromRange(range: NSRange): NSAttributedString; message 'attributedSubstringFromRange:';
    function attributesAtIndex_longestEffectiveRange_inRange(location: NSUInteger; range: NSRangePointer; rangeLimit: NSRange): NSDictionary; message 'attributesAtIndex:longestEffectiveRange:inRange:';
    function attribute_atIndex_longestEffectiveRange_inRange(attrName: NSString; location: NSUInteger; range: NSRangePointer; rangeLimit: NSRange): id; message 'attribute:atIndex:longestEffectiveRange:inRange:';
    function isEqualToAttributedString(other: NSAttributedString): Boolean; message 'isEqualToAttributedString:';
    function initWithString(str: NSString): id; message 'initWithString:';
    function initWithString_attributes(str: NSString; attrs: NSDictionary): id; message 'initWithString:attributes:';
    function initWithAttributedString(attrStr: NSAttributedString): id; message 'initWithAttributedString:';
  end;

{ NSExtendedMutableAttributedStringCategory }
  NSExtendedMutableAttributedStringCategory = objccategory external (NSMutableAttributedString)
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
  end;

{ NSAttributedStringKitAdditionsCategory }
  NSAttributedStringKitAdditionsCategory = objccategory external (NSAttributedString)
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
  end;

{ NSDeprecatedKitAdditionsCategory }
  NSDeprecatedKitAdditionsCategory = objccategory external (NSAttributedString)
    class function textFileTypes: NSArray; message 'textFileTypes'; deprecated 'in Mac OS X 10.5 and later';
    class function textPasteboardTypes: NSArray; message 'textPasteboardTypes'; deprecated 'in Mac OS X 10.5 and later';
    class function textUnfilteredFileTypes: NSArray; message 'textUnfilteredFileTypes'; deprecated 'in Mac OS X 10.5 and later';
    class function textUnfilteredPasteboardTypes: NSArray; message 'textUnfilteredPasteboardTypes'; deprecated 'in Mac OS X 10.5 and later';
  end;

{ NSMutableAttributedStringKitAdditionsCategory }
  NSMutableAttributedStringKitAdditionsCategory = objccategory external (NSMutableAttributedString)
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
  end;

{$endif}
{$endif}
