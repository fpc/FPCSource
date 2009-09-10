{ Parsed from Foundation.framework NSScriptCommandDescription.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSCRIPTCOMMANDDESCRIPTION_PAS_H}
{$define NSSCRIPTCOMMANDDESCRIPTION_PAS_H}
type
  NSScriptCommandDescriptionPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSSCRIPTCOMMANDDESCRIPTION_PAS_T}
{$define NSSCRIPTCOMMANDDESCRIPTION_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScriptCommandDescription = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTCOMMANDDESCRIPTION_PAS_C}
{$define NSSCRIPTCOMMANDDESCRIPTION_PAS_C}

{ NSScriptCommandDescription }
  NSScriptCommandDescription = objcclass(NSObject, NSCodingProtocol)
  private
    __suiteName: NSString;
    __plistCommandName: NSString;
    __classAppleEventCode: FourCharCode;
    __idAppleEventCode: FourCharCode;
    __objcClassName: NSString;
    __resultTypeNameOrDescription: NSObject;
    __plistResultTypeAppleEventCode: FourCharCode;
    __moreVars: id;
    
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
    function createCommandInstanceWithZone(var zone_: NSZone): NSScriptCommand; message 'createCommandInstanceWithZone:';
  end; external;

{$endif}
{$endif}
