{ Parsed from Foundation.framework NSAttributedString.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSATTRIBUTEDSTRING_PAS_T}
{$define NSATTRIBUTEDSTRING_PAS_T}

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
  NSAttributedString = objcclass(NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSAttributedString; message 'alloc';

    function string_: NSString; message 'string';
    function attributesAtIndex_effectiveRange(location: NSUInteger; range: NSRangePointer): NSDictionary; message 'attributesAtIndex:effectiveRange:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    function mutableCopyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end; external;

{ NSMutableAttributedString }
  NSMutableAttributedString = objcclass(NSAttributedString)
    
  public
    class function alloc: NSMutableAttributedString; message 'alloc';

    procedure replaceCharactersInRange_withString(range: NSRange; str: NSString); message 'replaceCharactersInRange:withString:';
    procedure setAttributes_range(attrs: NSDictionary; range: NSRange); message 'setAttributes:range:';
  end; external;

{ NSExtendedAttributedStringCategory }
  NSExtendedAttributedStringCategory = objccategory(NSAttributedString)
    function length: NSUInteger; message 'length';
    function attribute_atIndex_effectiveRange(attrName: NSString; location: NSUInteger; range: NSRangePointer): id; message 'attribute:atIndex:effectiveRange:';
    function attributedSubstringFromRange(range: NSRange): NSAttributedString; message 'attributedSubstringFromRange:';
    function attributesAtIndex_longestEffectiveRange_inRange(location: NSUInteger; range: NSRangePointer; rangeLimit: NSRange): NSDictionary; message 'attributesAtIndex:longestEffectiveRange:inRange:';
    function attribute_atIndex_longestEffectiveRange_inRange(attrName: NSString; location: NSUInteger; range: NSRangePointer; rangeLimit: NSRange): id; message 'attribute:atIndex:longestEffectiveRange:inRange:';
    function isEqualToAttributedString(other: NSAttributedString): Boolean; message 'isEqualToAttributedString:';
    function initWithString(str: NSString): id; message 'initWithString:';
    function initWithString_attributes(str: NSString; attrs: NSDictionary): id; message 'initWithString:attributes:';
    function initWithAttributedString(attrStr: NSAttributedString): id; message 'initWithAttributedString:';
  end; external;

{ NSExtendedMutableAttributedStringCategory }
  NSExtendedMutableAttributedStringCategory = objccategory(NSMutableAttributedString)
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
  end; external;

{$endif}
{$endif}
