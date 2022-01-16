{ Parsed from Foundation.framework NSObjectScripting.h }

{$ifdef CLASSES}

type
  NSScripting = objccategory external (NSObject)
    function scriptingValueForSpecifier (objectSpecifier: NSScriptObjectSpecifier): id; message 'scriptingValueForSpecifier:'; { available in 10_5, NA }
    procedure setScriptingProperties(newValue: NSDictionary); message 'setScriptingProperties:';
    function scriptingProperties: NSDictionary; message 'scriptingProperties';
    function copyScriptingValue_forKey_withProperties (value: id; key: NSString; properties: NSDictionary): id; message 'copyScriptingValue:forKey:withProperties:'; { available in 10_5, NA }
    function newScriptingObjectOfClass_forValueForKey_withContentsValue_properties (objectClass: pobjc_class; key: NSString; contentsValue: id; properties: NSDictionary): id; message 'newScriptingObjectOfClass:forValueForKey:withContentsValue:properties:'; { available in 10_5, NA }
  end;
{$endif}

