{ Parsed from Foundation.framework NSObjectScripting.h }

{$ifdef TYPES}
{$ifndef NSOBJECTSCRIPTING_PAS_T}
{$define NSOBJECTSCRIPTING_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSOBJECTSCRIPTING_PAS_R}
{$define NSOBJECTSCRIPTING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSOBJECTSCRIPTING_PAS_F}
{$define NSOBJECTSCRIPTING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSOBJECTSCRIPTING_PAS_S}
{$define NSOBJECTSCRIPTING_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSOBJECTSCRIPTING_PAS_C}
{$define NSOBJECTSCRIPTING_PAS_C}

{ NSScriptingCategory }
  NSScriptingCategory = objccategory external (NSObject)
    function scriptingValueForSpecifier(objectSpecifier: NSScriptObjectSpecifier): id; message 'scriptingValueForSpecifier:';
    function scriptingProperties: NSDictionary; message 'scriptingProperties';
    procedure setScriptingProperties(properties: NSDictionary); message 'setScriptingProperties:';
    function copyScriptingValue_forKey_withProperties(value: id; key: NSString; properties: NSDictionary): id; message 'copyScriptingValue:forKey:withProperties:';
    function newScriptingObjectOfClass_forValueForKey_withContentsValue_properties(objectClass: Pobjc_class; key: NSString; contentsValue: id; properties: NSDictionary): id; message 'newScriptingObjectOfClass:forValueForKey:withContentsValue:properties:';
  end;

{$endif}
{$endif}
