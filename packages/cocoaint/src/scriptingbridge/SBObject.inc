{ Parsed from ScriptingBridge.framework SBObject.h }


{$ifdef TYPES}
type
  SBObjectPtr = ^SBObject;
{$endif}

{$ifdef CLASSES}

type
  SBObject = objcclass external (NSObject, NSCodingProtocol)
  private
    _specifier: AEDesc;
    _ctx: SBAppContext;
    _reserved: id;
  public
    function init: id; message 'init';
    function initWithProperties (properties: NSDictionary): id; message 'initWithProperties:';
    function initWithData (data: id): id; message 'initWithData:';
    function get: id; message 'get';
    function lastError: NSError; message 'lastError'; { available in 10_6, NA }

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  SBGlueInterface = objccategory external (SBObject)
    function initWithElementCode_properties_data (code: DescType; properties: NSDictionary; data: id): id; message 'initWithElementCode:properties:data:';
    function propertyWithCode (code: AEKeyword): SBObject; message 'propertyWithCode:';
    function propertyWithClass_code (cls: pobjc_class; code: AEKeyword): SBObject; message 'propertyWithClass:code:';
    function elementArrayWithCode (code: DescType): SBElementArray; message 'elementArrayWithCode:';
    function sendEvent_id_parameters (eventClass: AEEventClass; eventID: AEEventID; firstParamCode: DescType): id; varargs; message 'sendEvent:id:parameters:';
    procedure setTo (value: id); message 'setTo:';
  end;
{$endif}

