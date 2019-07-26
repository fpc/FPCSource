{ Parsed from ITunesLibrary.framework ITLibMediaEntity.h }
{ Created on Sun Mar 8 1:48:57 2015 }


{$ifdef TYPES}
type
  ITLibMediaEntityPtr = ^ITLibMediaEntity;
{$endif}

{$ifdef CLASSES}

type
  ITLibMediaEntity = objcclass external (NSObject)
  private
    _fields: NSMutableDictionary;
    _persistentID: NSNumber;
  public
    function persistentID: NSNumber; message 'persistentID';
    function valueForProperty (property_: NSString): id; message 'valueForProperty:';
    procedure enumerateValuesForProperties_usingBlock (properties: NSSet; block: OpaqueCBlock); message 'enumerateValuesForProperties:usingBlock:';
    procedure enumerateValuesExceptForProperties_usingBlock (properties: NSSet; block: OpaqueCBlock); message 'enumerateValuesExceptForProperties:usingBlock:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  ITLibMediaEntityPropertyPersistentID: NSString; cvar; external;
{$endif}

