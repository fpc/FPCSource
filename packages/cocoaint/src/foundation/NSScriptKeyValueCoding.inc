{ Parsed from Foundation.framework NSScriptKeyValueCoding.h }

{$ifdef TYPES}
{$ifndef NSSCRIPTKEYVALUECODING_PAS_T}
{$define NSSCRIPTKEYVALUECODING_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSCRIPTKEYVALUECODING_PAS_R}
{$define NSSCRIPTKEYVALUECODING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCRIPTKEYVALUECODING_PAS_F}
{$define NSSCRIPTKEYVALUECODING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCRIPTKEYVALUECODING_PAS_S}
{$define NSSCRIPTKEYVALUECODING_PAS_S}

{ External string constants }
var
  NSOperationNotSupportedForKeyException: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTKEYVALUECODING_PAS_C}
{$define NSSCRIPTKEYVALUECODING_PAS_C}

{ NSScriptKeyValueCodingCategory }
  NSScriptKeyValueCodingCategory = objccategory external (NSObject)
    function valueAtIndex_inPropertyWithKey(index: NSUInteger; key: NSString): id; message 'valueAtIndex:inPropertyWithKey:';
    function valueWithName_inPropertyWithKey(name: NSString; key: NSString): id; message 'valueWithName:inPropertyWithKey:';
    function valueWithUniqueID_inPropertyWithKey(uniqueID: id; key: NSString): id; message 'valueWithUniqueID:inPropertyWithKey:';
    procedure insertValue_atIndex_inPropertyWithKey(value: id; index: NSUInteger; key: NSString); message 'insertValue:atIndex:inPropertyWithKey:';
    procedure removeValueAtIndex_fromPropertyWithKey(index: NSUInteger; key: NSString); message 'removeValueAtIndex:fromPropertyWithKey:';
    procedure replaceValueAtIndex_inPropertyWithKey_withValue(index: NSUInteger; key: NSString; value: id); message 'replaceValueAtIndex:inPropertyWithKey:withValue:';
    procedure insertValue_inPropertyWithKey(value: id; key: NSString); message 'insertValue:inPropertyWithKey:';
    function coerceValue_forKey(value: id; key: NSString): id; message 'coerceValue:forKey:';
  end;

{$endif}
{$endif}
