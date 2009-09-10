{ Parsed from Foundation.framework NSFormatter.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:30:59 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSFORMATTER_PAS_H}
{$define NSFORMATTER_PAS_H}
type
  NSFormatterPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSFORMATTER_PAS_T}
{$define NSFORMATTER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFormatter = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFORMATTER_PAS_C}
{$define NSFORMATTER_PAS_C}

{ NSFormatter }
  NSFormatter = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSFormatter; message 'alloc';

    function stringForObjectValue(obj: id): NSString; message 'stringForObjectValue:';
    function attributedStringForObjectValue_withDefaultAttributes(obj: id; attrs: NSDictionary): NSAttributedString; message 'attributedStringForObjectValue:withDefaultAttributes:';
    function editingStringForObjectValue(obj: id): NSString; message 'editingStringForObjectValue:';
    function getObjectValue_forString_errorDescription(obj: id; string_: NSString; var error: NSString): Boolean; message 'getObjectValue:forString:errorDescription:';
    function isPartialStringValid_newEditingString_errorDescription(partialString: NSString; var newString: NSString; var error: NSString): Boolean; message 'isPartialStringValid:newEditingString:errorDescription:';
    function isPartialStringValid_proposedSelectedRange_originalString_originalSelectedRange_errorDescription(partialStringPtr: NSStringPointer; proposedSelRangePtr: NSRangePointerPointer; origString: NSString; origSelRange: NSRange; var error: NSString): Boolean; message 'isPartialStringValid:proposedSelectedRange:originalString:originalSelectedRange:errorDescription:';
  end; external;

{$endif}
{$endif}
