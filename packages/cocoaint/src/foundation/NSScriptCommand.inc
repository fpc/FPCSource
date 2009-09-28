{ Parsed from Foundation.framework NSScriptCommand.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSCRIPTCOMMAND_PAS_H}
{$define NSSCRIPTCOMMAND_PAS_H}
type
  NSScriptCommandPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSCRIPTCOMMAND_PAS_T}
{$define NSSCRIPTCOMMAND_PAS_T}

{ Constants }

const
  NSNoScriptError = 0;
  NSReceiverEvaluationScriptError = 0;
  NSKeySpecifierEvaluationScriptError = 1;
  NSArgumentEvaluationScriptError = 2;
  NSReceiversCantHandleCommandScriptError = 3;
  NSRequiredArgumentsMissingScriptError = 4;
  NSArgumentsWrongScriptError = 5;
  NSUnknownKeyScriptError = 6;
  NSInternalScriptError = 7;
  NSOperationNotSupportedForKeyScriptError = 8;
  NSCannotCreateScriptCommandError = 9;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSCRIPTCOMMAND_PAS_R}
{$define NSSCRIPTCOMMAND_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCRIPTCOMMAND_PAS_F}
{$define NSSCRIPTCOMMAND_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCRIPTCOMMAND_PAS_S}
{$define NSSCRIPTCOMMAND_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScriptCommand = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTCOMMAND_PAS_C}
{$define NSSCRIPTCOMMAND_PAS_C}

{ NSScriptCommand }
  NSScriptCommand = objcclass(NSObject, NSCodingProtocol)
  private
    __commandDescription: NSScriptCommandDescription;
    __directParameter: id;
    __receiversSpecifier: NSScriptObjectSpecifier;
    __evaluatedReceivers: id;
    __arguments: NSDictionary;
    __evaluatedArguments: NSMutableDictionary;
    __flags: bitpacked record
        hasEvaluatedReceivers: 0..1;
        hasEvaluatedArguments: 0..1;
        RESERVED: 0..((1 shl 30)-1);
      end;
    __moreVars: id;
    __reserved: Pointer;
    
  public
    class function alloc: NSScriptCommand; message 'alloc';

    function initWithCommandDescription(commandDef: NSScriptCommandDescription): id; message 'initWithCommandDescription:';
    function commandDescription: NSScriptCommandDescription; message 'commandDescription';
    procedure setDirectParameter(directParameter_: id); message 'setDirectParameter:';
    function directParameter: id; message 'directParameter';
    procedure setReceiversSpecifier(receiversRef: NSScriptObjectSpecifier); message 'setReceiversSpecifier:';
    function receiversSpecifier: NSScriptObjectSpecifier; message 'receiversSpecifier';
    function evaluatedReceivers: id; message 'evaluatedReceivers';
    procedure setArguments(args: NSDictionary); message 'setArguments:';
    function arguments: NSDictionary; message 'arguments';
    function evaluatedArguments: NSDictionary; message 'evaluatedArguments';
    function isWellFormed: Boolean; message 'isWellFormed';
    function performDefaultImplementation: id; message 'performDefaultImplementation';
    function executeCommand: id; message 'executeCommand';
    procedure setScriptErrorNumber(errorNumber: cint); message 'setScriptErrorNumber:';
    procedure setScriptErrorOffendingObjectDescriptor(errorOffendingObjectDescriptor: NSAppleEventDescriptor); message 'setScriptErrorOffendingObjectDescriptor:';
    procedure setScriptErrorExpectedTypeDescriptor(errorExpectedTypeDescriptor: NSAppleEventDescriptor); message 'setScriptErrorExpectedTypeDescriptor:';
    procedure setScriptErrorString(errorString: NSString); message 'setScriptErrorString:';
    function scriptErrorNumber: cint; message 'scriptErrorNumber';
    function scriptErrorOffendingObjectDescriptor: NSAppleEventDescriptor; message 'scriptErrorOffendingObjectDescriptor';
    function scriptErrorExpectedTypeDescriptor: NSAppleEventDescriptor; message 'scriptErrorExpectedTypeDescriptor';
    function scriptErrorString: NSString; message 'scriptErrorString';
    class function currentCommand: NSScriptCommand; message 'currentCommand';
    function appleEvent: NSAppleEventDescriptor; message 'appleEvent';
    procedure suspendExecution; message 'suspendExecution';
    procedure resumeExecutionWithResult(result_: id); message 'resumeExecutionWithResult:';
  end; external;

{$endif}
{$endif}
