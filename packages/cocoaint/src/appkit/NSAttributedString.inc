{ Parsed from AppKit.framework NSAttributedString.h }

{$ifdef EXTERNAL_SYMBOLS}
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
  NSWritingDirectionAttributeName: NSString { available in 10_6 }; cvar; external;
  NSMarkedClauseSegmentAttributeName: NSString; cvar; external;
  NSVerticalGlyphFormAttributeName: NSString { available in 10_7 }; cvar; external;
  NSTextAlternativesAttributeName: NSString { available in 10_8 }; cvar; external;
  NSTextEffectAttributeName: NSString { available in 10_10, 7_0 }; cvar; external;
  NSTextEffectLetterpressStyle: NSString { available in 10_10, 7_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
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
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSUnderlineByWordMask: NSUInteger; cvar; external;
  NSSpellingStateAttributeName: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
const
  NSSpellingStateSpellingFlag = 1 shl 0 { available in 10_5 };
  NSSpellingStateGrammarFlag = 1 shl 1 { available in 10_5 };
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSPlainTextDocumentType: NSString; cvar; external;
  NSRTFTextDocumentType: NSString; cvar; external;
  NSRTFDTextDocumentType: NSString; cvar; external;
  NSMacSimpleTextDocumentType: NSString; cvar; external;
  NSHTMLTextDocumentType: NSString; cvar; external;
  NSDocFormatTextDocumentType: NSString; cvar; external;
  NSWordMLTextDocumentType: NSString; cvar; external;
  NSWebArchiveTextDocumentType: NSString; cvar; external;
  NSOfficeOpenXMLTextDocumentType: NSString { available in 10_5 }; cvar; external;
  NSOpenDocumentTextDocumentType: NSString { available in 10_5 }; cvar; external;
  NSTextLayoutSectionOrientation: NSString { available in 10_7 }; cvar; external;
  NSTextLayoutSectionRange: NSString { available in 10_7 }; cvar; external;
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
  NSFileTypeDocumentAttribute: NSString { available in 10_6 }; cvar; external;
  NSUsesScreenFontsDocumentAttribute: NSString { available in 10_8 }; cvar; external;
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
  NSManagerDocumentAttribute: NSString { available in 10_5 }; cvar; external;
  NSCategoryDocumentAttribute: NSString { available in 10_6 }; cvar; external;
  NSTextLayoutSectionsAttribute: NSString { available in 10_7 }; cvar; external;
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
  NSFileTypeDocumentOption: NSString { available in 10_6 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSAttributedStringKitAdditions = objccategory external (NSAttributedString)
    function fontAttributesInRange (range: NSRange): NSDictionary; message 'fontAttributesInRange:';
    function rulerAttributesInRange (range: NSRange): NSDictionary; message 'rulerAttributesInRange:';
    function containsAttachments: ObjCBOOL; message 'containsAttachments';
    function lineBreakBeforeIndex_withinRange (location: NSUInteger; aRange: NSRange): NSUInteger; message 'lineBreakBeforeIndex:withinRange:';
    function lineBreakByHyphenatingBeforeIndex_withinRange (location: NSUInteger; aRange: NSRange): NSUInteger; message 'lineBreakByHyphenatingBeforeIndex:withinRange:';
    function doubleClickAtIndex (location: NSUInteger): NSRange; message 'doubleClickAtIndex:';
    function nextWordFromIndex_forward (location: NSUInteger; isForward: ObjCBOOL): NSUInteger; message 'nextWordFromIndex:forward:';
    function URLAtIndex_effectiveRange (location: NSUInteger; effectiveRange: NSRangePointer): NSURL; message 'URLAtIndex:effectiveRange:'; { available in 10_5 }
    function rangeOfTextBlock_atIndex (block: NSTextBlock; location: NSUInteger): NSRange; message 'rangeOfTextBlock:atIndex:';
    function rangeOfTextTable_atIndex (table: NSTextTable; location: NSUInteger): NSRange; message 'rangeOfTextTable:atIndex:';
    function rangeOfTextList_atIndex (list: NSTextList; location: NSUInteger): NSRange; message 'rangeOfTextList:atIndex:';
    function itemNumberInTextList_atIndex (list: NSTextList; location: NSUInteger): NSInteger; message 'itemNumberInTextList:atIndex:';
    function initWithURL_options_documentAttributes_error (url: NSURL; options: NSDictionary; dict: NSDictionaryPtr; error: NSErrorPtr): instancetype; message 'initWithURL:options:documentAttributes:error:';
    function initWithData_options_documentAttributes_error (data: NSData; options: NSDictionary; dict: NSDictionaryPtr; error: NSErrorPtr): instancetype; message 'initWithData:options:documentAttributes:error:';
    function initWithPath_documentAttributes (path: NSString; dict: NSDictionaryPtr): instancetype; message 'initWithPath:documentAttributes:';
    function initWithURL_documentAttributes (url: NSURL; dict: NSDictionaryPtr): instancetype; message 'initWithURL:documentAttributes:';
    function initWithRTF_documentAttributes (data: NSData; dict: NSDictionaryPtr): instancetype; message 'initWithRTF:documentAttributes:';
    function initWithRTFD_documentAttributes (data: NSData; dict: NSDictionaryPtr): instancetype; message 'initWithRTFD:documentAttributes:';
    function initWithHTML_documentAttributes (data: NSData; dict: NSDictionaryPtr): instancetype; message 'initWithHTML:documentAttributes:';
    function initWithHTML_baseURL_documentAttributes (data: NSData; base: NSURL; dict: NSDictionaryPtr): instancetype; message 'initWithHTML:baseURL:documentAttributes:';
    function initWithDocFormat_documentAttributes (data: NSData; dict: NSDictionaryPtr): instancetype; message 'initWithDocFormat:documentAttributes:';
    function initWithHTML_options_documentAttributes (data: NSData; options: NSDictionary; dict: NSDictionaryPtr): instancetype; message 'initWithHTML:options:documentAttributes:';
    function initWithRTFDFileWrapper_documentAttributes (wrapper: NSFileWrapper; dict: NSDictionaryPtr): instancetype; message 'initWithRTFDFileWrapper:documentAttributes:';
    function dataFromRange_documentAttributes_error (range: NSRange; dict: NSDictionary; error: NSErrorPtr): NSData; message 'dataFromRange:documentAttributes:error:';
    function fileWrapperFromRange_documentAttributes_error (range: NSRange; dict: NSDictionary; error: NSErrorPtr): NSFileWrapper; message 'fileWrapperFromRange:documentAttributes:error:';
    function RTFFromRange_documentAttributes (range: NSRange; dict: NSDictionary): NSData; message 'RTFFromRange:documentAttributes:';
    function RTFDFromRange_documentAttributes (range: NSRange; dict: NSDictionary): NSData; message 'RTFDFromRange:documentAttributes:';
    function RTFDFileWrapperFromRange_documentAttributes (range: NSRange; dict: NSDictionary): NSFileWrapper; message 'RTFDFileWrapperFromRange:documentAttributes:';
    function docFormatFromRange_documentAttributes (range: NSRange; dict: NSDictionary): NSData; message 'docFormatFromRange:documentAttributes:';
  end;

type
  NSAttributedStringPasteboardAdditions = objccategory external (NSAttributedString)
    class function textTypes: NSArray; message 'textTypes'; { available in 10_5 }
    class function textUnfilteredTypes: NSArray; message 'textUnfilteredTypes'; { available in 10_5 }
  end;

type
  NSDeprecatedKitAdditions = objccategory external (NSAttributedString)
    class function textFileTypes: NSArray; message 'textFileTypes'; deprecated 'in 10_1, 10_5';
    class function textPasteboardTypes: NSArray; message 'textPasteboardTypes'; deprecated 'in 10_1, 10_5';
    class function textUnfilteredFileTypes: NSArray; message 'textUnfilteredFileTypes'; deprecated 'in 10_1, 10_5';
    class function textUnfilteredPasteboardTypes: NSArray; message 'textUnfilteredPasteboardTypes'; deprecated 'in 10_1, 10_5';
  end;

type
  NSMutableAttributedStringKitAdditions = objccategory external (NSMutableAttributedString)
    function readFromURL_options_documentAttributes_error (url: NSURL; opts: NSDictionary; dict: NSDictionaryPtr; error: NSErrorPtr): ObjCBOOL; message 'readFromURL:options:documentAttributes:error:';
    function readFromData_options_documentAttributes_error (data: NSData; opts: NSDictionary; dict: NSDictionaryPtr; error: NSErrorPtr): ObjCBOOL; message 'readFromData:options:documentAttributes:error:';
    function readFromURL_options_documentAttributes (url: NSURL; options: NSDictionary; dict: NSDictionaryPtr): ObjCBOOL; message 'readFromURL:options:documentAttributes:';
    function readFromData_options_documentAttributes (data: NSData; options: NSDictionary; dict: NSDictionaryPtr): ObjCBOOL; message 'readFromData:options:documentAttributes:';
    procedure superscriptRange (range: NSRange); message 'superscriptRange:';
    procedure subscriptRange (range: NSRange); message 'subscriptRange:';
    procedure unscriptRange (range: NSRange); message 'unscriptRange:';
    procedure applyFontTraits_range (traitMask: NSFontTraitMask; range: NSRange); message 'applyFontTraits:range:';
    procedure setAlignment_range (alignment: NSTextAlignment; range: NSRange); message 'setAlignment:range:';
    procedure setBaseWritingDirection_range (writingDirection: NSWritingDirection; range: NSRange); message 'setBaseWritingDirection:range:';
    procedure fixAttributesInRange (range: NSRange); message 'fixAttributesInRange:';
    procedure fixFontAttributeInRange (range: NSRange); message 'fixFontAttributeInRange:';
    procedure fixParagraphStyleAttributeInRange (range: NSRange); message 'fixParagraphStyleAttributeInRange:';
    procedure fixAttachmentAttributeInRange (range: NSRange); message 'fixAttachmentAttributeInRange:';
  end;
{$endif}

{$ifdef TYPES}
const
  NSNoUnderlineStyle = 0;
  NSSingleUnderlineStyle = 1;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSUnderlineStrikethroughMask: NSUInteger deprecated 'in 10_0, 10_9'; cvar; external;
{$endif}

