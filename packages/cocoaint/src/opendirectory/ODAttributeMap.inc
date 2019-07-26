{ Parsed from OpenDirectory.framework ODAttributeMap.h }


{$ifdef TYPES}
type
  ODAttributeMapPtr = ^ODAttributeMap;
{$endif}

{$ifdef CLASSES}

type
  ODAttributeMap = objcclass external (NSObject)
  private
    customQueryFunction: NSString;
    customTranslationFunction: NSString;
    customAttributes: NSArray;
    value: NSString;
  public
    procedure setCustomQueryFunction(newValue: NSString); message 'setCustomQueryFunction:';
    function customQueryFunction_: NSString; message 'customQueryFunction';
    procedure setCustomTranslationFunction(newValue: NSString); message 'setCustomTranslationFunction:';
    function customTranslationFunction_: NSString; message 'customTranslationFunction';
    procedure setCustomAttributes(newValue: NSArray); message 'setCustomAttributes:';
    function customAttributes_: NSArray; message 'customAttributes';
    procedure setValue(newValue: NSString); message 'setValue:';
    function value_: NSString; message 'value';
    class function attributeMapWithValue (value__: NSString): instancetype; message 'attributeMapWithValue:';
    class function attributeMapWithStaticValue (staticValue: NSString): instancetype; message 'attributeMapWithStaticValue:';
    procedure setStaticValue (staticValue: NSString); message 'setStaticValue:'; { available in 10_9 }
    procedure setVariableSubstitution (variableSubstitution: NSString); message 'setVariableSubstitution:'; { available in 10_9 }
  end;
{$endif}

