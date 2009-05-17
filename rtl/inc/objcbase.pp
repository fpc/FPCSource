{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 by the Free Pascal development team

    This unit provides the definition of the NSObject root class

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit objcbase;

{$ifdef FPC_HAS_FEATURE_OBJECTIVEC1}

{$modeswitch objectivec1}

interface

uses
  ctypes;

type
{
  NSString = objcclass; external;
  NSZone = objcclass; external;
  NSInvocation = objcclass; external;
  NSMethodSignature = objcclass; external;
  NSCoder = objcclass; external;
}

  NSObject = objcclass
   strict protected
    isa: pobjc_class;
   public
    { NSObject protocol }
    function isEqual_(obj: id): boolean; message 'isEqual:';
    function hash: cuint; message 'hash';
// implemented as class method instead?
//     function superclass: pobjc_class;
    { "self" is both a hidden parameter to each method, and a method of
      NSObject and thereby of each subclass as well
    }
    function self: id;  message 'self';
    function zone: id;  message 'zone';{ NSZone }

    function performSelector_(aSelector: SEL): id; message 'performSelector:';
    function performSelector_withObject_(aSelector: SEL; obj: id): id; message 'performSelector:withObject:';
    function performSelector_withObject_withObject(aSelector: SEL; obj1, obj2: id): id; message 'performSelector:withObject:withObject:';

    function isProxy: boolean; message 'isProxy';

    function isKindOfClass_(aClass: pobjc_class): boolean; message 'isKindOfClass:';
    function isMemberOfClass_(aClass: pobjc_class): boolean; message 'isMemberOfClass:';
// implemented as class method instead?
//     function conformsToProtocol(aProtocol: pobjc_protocal): boolean;

    function respondsToSelector_(aSelector: SEL): boolean; message 'respondsToSelector:';

    function retain: id; message 'retain';
    procedure release;  message 'release'; { oneway }
    function autorelease: id; message 'autorelease';
    function retainCount: cint; message 'retainCount';

// implemented as class method instead?
//     function description: NSString;

    { NSObject methods }

    class procedure load; message 'load';

    class procedure initialize; message 'initialize';
    function init: id; message 'init';

    class function new: id; message 'new';
    class function allocWithZone_(_zone: id {NSZone}): id; message 'allocWithZone:';
    class function alloc: id; message 'alloc';
    procedure dealloc; message 'dealloc';
    { if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4 }
    procedure finalize; message 'finalize';
    { endif }

    function copy: id; message 'copy';
    function mutableCopy: id; message 'mutableCopy';

    class function copyWithZone_(_zone: id {NSZone}): id; message 'copyWithZone:';
    class function mutableCopyWithZone_(_zone: id {NSZone}): id; message 'mutableCopyWithZone:';

    class function superclass: pobjc_class; message 'superclass';
    class function _class: pobjc_class; message 'class';
    class procedure poseAsClass_(aClass: pobjc_class); message 'poseAsClass:';
    class function instancesRespondToSelector_(aSelector: SEL): boolean; message 'instancesRespondToSelector:';
    class function conformsToProtocol_(aProtocol: pobjc_protocal): boolean; message 'conformsToProtocol:';
    function methodForSelector_(aSelector: SEL): IMP; message 'methodForSelector:';
    class function instanceMethodForSelector_(aSelector: SEL): IMP; message 'instanceMethodForSelector:';
    class function version: cint; message 'version';
    class procedure setVersion_(aVersion: cint); message 'setVersion:';
    procedure doesNotRecognizeSelector_(aSelector: SEL); message 'doesNotRecognizeSelector:';
    procedure forwardInvocation_(anInvocation: id {NSInvocation}); message 'forwardInvocation:';
    function methodSignatureForSelector_(aSelector: SEL): id {NSMethodSignature}; message 'methodSignatureForSelector:';

    class function description: id {NSString}; message 'description';

    function classForCoder: pobjc_class; message 'classForCoder';
    function replacementObjectForCoder_(aCoder: id {NSCoder}): id; message 'replacementObjectForCoder:';
    function awakeAfterUsingCoder_(aDecoder: id {NSCoder}): id; message 'awakeAfterUsingCoder:';
  end; external;

implementation

{$else }

interface

implementation

{$endif}

end.


