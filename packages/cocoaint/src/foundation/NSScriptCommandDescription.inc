{ Parsed from Foundation.framework NSScriptCommandDescription.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSSCRIPTCOMMANDDESCRIPTION_PAS_T}
{$define NSSCRIPTCOMMANDDESCRIPTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSCRIPTCOMMANDDESCRIPTION_PAS_R}
{$define NSSCRIPTCOMMANDDESCRIPTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCRIPTCOMMANDDESCRIPTION_PAS_F}
{$define NSSCRIPTCOMMANDDESCRIPTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCRIPTCOMMANDDESCRIPTION_PAS_S}
{$define NSSCRIPTCOMMANDDESCRIPTION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScriptCommandDescription = objcclass;
  NSScriptCommandDescriptionPointer = ^NSScriptCommandDescription;
  NSScriptCommandDescriptionPtr = NSScriptCommandDescriptionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTCOMMANDDESCRIPTION_PAS_C}
{$define NSSCRIPTCOMMANDDESCRIPTION_PAS_C}

{ NSScriptCommandDescription }
  NSScriptCommandDescription = objcclass(NSObject, NSCodingProtocol)
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
    class function alloc: NSScriptCommandDescription; message 'alloc';

    function initWithSuiteName_commandName_dictionary(suiteName_: NSString; commandName_: NSString; commandDeclaration: NSDictionary): id; message 'initWithSuiteName:commandName:dictionary:';
    function suiteName: NSString; message 'suiteName';
    function commandName: NSString; message 'commandName';
    function appleEventClassCode: FourCharCode; message 'appleEventClassCode';
    function appleEventCode: FourCharCode; message 'appleEventCode';
    function commandClassName: NSString; message 'commandClassName';
    function returnType: NSString; message 'returnType';
    function appleEventCodeForReturnType: FourCharCode; message 'appleEventCodeForReturnType';
    function argumentNames: NSArray; message 'argumentNames';
    function typeForArgumentWithName(argumentName: NSString): NSString; message 'typeForArgumentWithName:';
    function appleEventCodeForArgumentWithName(argumentName: NSString): FourCharCode; message 'appleEventCodeForArgumentWithName:';
    function isOptionalArgumentWithName(argumentName: NSString): Boolean; message 'isOptionalArgumentWithName:';
    function createCommandInstance: NSScriptCommand; message 'createCommandInstance';
    function createCommandInstanceWithZone(zone_: NSZonePtr): NSScriptCommand; message 'createCommandInstanceWithZone:';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end; external;

{$endif}
{$endif}
