{ Parsed from Foundation.framework NSFormatter.h }


{$ifdef TYPES}
type
  NSFormatterPtr = ^NSFormatter;
{$endif}

{$ifdef TYPES}
type
  NSFormattingContext = NSInteger;
  NSFormattingContextPtr = ^NSFormattingContext;

const
  NSFormattingContextUnknown = 0;
  NSFormattingContextDynamic = 1;
  NSFormattingContextStandalone = 2;
  NSFormattingContextListItem = 3;
  NSFormattingContextBeginningOfSentence = 4;
  NSFormattingContextMiddleOfSentence = 5;

type
  NSFormattingUnitStyle = NSInteger;
  NSFormattingUnitStylePtr = ^NSFormattingUnitStyle;

const
  NSFormattingUnitStyleShort = 1;
  NSFormattingUnitStyleMedium = 2;
  NSFormattingUnitStyleLong = 3;
{$endif}

{$ifdef CLASSES}

type
  NSFormatter = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  public
    function stringForObjectValue (obj: id): NSString; message 'stringForObjectValue:';
    function attributedStringForObjectValue_withDefaultAttributes (obj: id; attrs: NSDictionary): NSAttributedString; message 'attributedStringForObjectValue:withDefaultAttributes:';
    function editingStringForObjectValue (obj: id): NSString; message 'editingStringForObjectValue:';
    function getObjectValue_forString_errorDescription (obj: idPtr; string_: NSString; error: NSStringPtr): ObjCBOOL; message 'getObjectValue:forString:errorDescription:';
    function isPartialStringValid_newEditingString_errorDescription (partialString: NSString; newString: NSStringPtr; error: NSStringPtr): ObjCBOOL; message 'isPartialStringValid:newEditingString:errorDescription:';
    function isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescription (partialStringPtr: NSStringPtr; proposedSelRangePtr: NSRangePointer; origString: NSString; origSelRange: NSRange; error: NSStringPtr): ObjCBOOL; message 'isPartialStringValid:proposedSelectedRange:originalString:originalSelectedRange:errorDescription:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

