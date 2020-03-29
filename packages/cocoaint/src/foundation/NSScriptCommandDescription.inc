{ Parsed from Foundation.framework NSScriptCommandDescription.h }


{$ifdef TYPES}
type
  NSScriptCommandDescriptionPtr = ^NSScriptCommandDescription;
{$endif}

{$ifdef CLASSES}

type
  NSScriptCommandDescription = objcclass external (NSObject, NSCodingProtocol)
  private
    _suiteName: NSString;
    _plistCommandName: NSString;
    _classAppleEventCode: FourCharCode;
    _idAppleEventCode: FourCharCode;
    _objcClassName: NSString;
    _resultTypeNameOrDescription: NSObject;
    _plistResultTypeAppleEventCode: FourCharCode;
    _moreVars: id;
  public
    function init: id; message 'init';
    function initWithSuiteName_commandName_dictionary (suiteName: NSString; commandName: NSString; commandDeclaration: NSDictionary): instancetype; message 'initWithSuiteName:commandName:dictionary:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (inCoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    function suiteName: NSString; message 'suiteName';
    function commandName: NSString; message 'commandName';
    function appleEventClassCode: FourCharCode; message 'appleEventClassCode';
    function appleEventCode: FourCharCode; message 'appleEventCode';
    function commandClassName: NSString; message 'commandClassName';
    function returnType: NSString; message 'returnType';
    function appleEventCodeForReturnType: FourCharCode; message 'appleEventCodeForReturnType';
    function argumentNames: NSArray; message 'argumentNames';
    function typeForArgumentWithName (argumentName: NSString): NSString; message 'typeForArgumentWithName:';
    function appleEventCodeForArgumentWithName (argumentName: NSString): FourCharCode; message 'appleEventCodeForArgumentWithName:';
    function isOptionalArgumentWithName (argumentName: NSString): ObjCBOOL; message 'isOptionalArgumentWithName:';
    function createCommandInstance: NSScriptCommand; message 'createCommandInstance';
    function createCommandInstanceWithZone (zone: NSZonePtr): NSScriptCommand; message 'createCommandInstanceWithZone:';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
  end;
{$endif}

