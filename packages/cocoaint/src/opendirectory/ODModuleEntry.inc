{ Parsed from OpenDirectory.framework ODModuleEntry.h }


{$ifdef TYPES}
type
  ODModuleEntryPtr = ^ODModuleEntry;
{$endif}

{$ifdef CLASSES}

type
  ODModuleEntry = objcclass external (NSObject)
  private
    mappings: ODMappings;
    supportedOptions: NSArray;
    name: NSString;
    xpcServiceName: NSString;
    options: NSMutableDictionary;
    uuidString: NSString;
  public
    procedure setMappings(newValue: ODMappings); message 'setMappings:';
    function mappings_: ODMappings; message 'mappings';
    function supportedOptions_: NSArray; message 'supportedOptions';
    procedure setName(newValue: NSString); message 'setName:';
    function name_: NSString; message 'name';
    procedure setXpcServiceName(newValue: NSString); message 'setXpcServiceName:';
    function xpcServiceName_: NSString; message 'xpcServiceName';
    procedure setUuidString(newValue: NSString); message 'setUuidString:';
    function uuidString_: NSString; message 'uuidString';
    class function moduleEntryWithName_xpcServiceName (name__: NSString; xpcServiceName__: NSString): instancetype; message 'moduleEntryWithName:xpcServiceName:'; { available in 10_9 }
    procedure setOption_value (optionName: NSString; value: id); message 'setOption:value:'; { available in 10_9 }
    function option (optionName: NSString): id; message 'option:'; { available in 10_9 }
  end;
{$endif}

