{ Parsed from Foundation.framework NSFormatter.h }

{$ifdef TYPES}
{$ifndef NSFORMATTER_PAS_T}
{$define NSFORMATTER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSFORMATTER_PAS_R}
{$define NSFORMATTER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSFORMATTER_PAS_F}
{$define NSFORMATTER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSFORMATTER_PAS_S}
{$define NSFORMATTER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFormatter = objcclass;
  NSFormatterPointer = ^NSFormatter;
  NSFormatterPtr = NSFormatterPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFORMATTER_PAS_C}
{$define NSFORMATTER_PAS_C}

{ NSFormatter }
  NSFormatter = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    function stringForObjectValue(obj: id): NSString; message 'stringForObjectValue:';
    function attributedStringForObjectValue_withDefaultAttributes(obj: id; attrs: NSDictionary): NSAttributedString; message 'attributedStringForObjectValue:withDefaultAttributes:';
    function editingStringForObjectValue(obj: id): NSString; message 'editingStringForObjectValue:';
    function getObjectValue_forString_errorDescription(obj: idPtr; string_: NSString; error: NSStringPointer): Boolean; message 'getObjectValue:forString:errorDescription:';
    function isPartialStringValid_newEditingString_errorDescription(partialString: NSString; newString: NSStringPointer; error: NSStringPointer): Boolean; message 'isPartialStringValid:newEditingString:errorDescription:';
    function isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescription(partialStringPtr: NSStringPointer; proposedSelRangePtr: NSRangePointer; origString: NSString; origSelRange: NSRange; error: NSStringPointer): Boolean; message 'isPartialStringValid:proposedSelectedRange:originalString:originalSelectedRange:errorDescription:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{$endif}
{$endif}
