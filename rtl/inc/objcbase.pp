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

  Protocol = objcclass
  end; external;

  NSObjectProtocol = objcprotocol
    function isEqual_(obj: id): boolean; message 'isEqual:';
    function hash: cuint; message 'hash';

    function superclass: pobjc_class; message 'superclass';
    function _class: pobjc_class; message 'class';
    { "self" is both a hidden parameter to each method, and a method of
      NSObject and thereby of each subclass as well
    }
    function self: id;  message 'self';
    function zone: id;  message 'zone';{ NSZone }

    function performSelector_(aSelector: SEL): id; message 'performSelector:';
    function performSelector_withObject_(aSelector: SEL; obj: id): id; message 'performSelector:withObject:';
    function performSelector_withObject_withObject_(aSelector: SEL; obj1, obj2: id): id; message 'performSelector:withObject:withObject:';

    function isProxy: boolean; message 'isProxy';

    function isKindOfClass_(aClass: pobjc_class): boolean; message 'isKindOfClass:';
    function isMemberOfClass_(aClass: pobjc_class): boolean; message 'isMemberOfClass:';
    function conformsToProtocol_(aProtocol: Protocol): boolean; message 'conformsToProtocol:';

    function respondsToSelector_(aSelector: SEL): boolean; message 'respondsToSelector:';

    function retain: id; message 'retain';
    procedure release;  message 'release'; { oneway }
    function autorelease: id; message 'autorelease';
    function retainCount: cint; message 'retainCount';

    function description: {NSString} id; message 'description';
  end; external name 'NSObject';


  NSObject = objcclass(NSObjectProtocol)
   strict protected
    isa: pobjc_class;
   public
    { NSObjectProtocol -- the message names are copied from the protocol
      definition by the compiler, but you can still repeat them if you want }
    function isEqual_(obj: id): boolean;
    function hash: cuint;

    function superclass: pobjc_class;
    function _class: pobjc_class;
    { "self" is both a hidden parameter to each method, and a method of
      NSObject and thereby of each subclass as well
    }
    function self: id;
    function zone: id; { NSZone }

    function performSelector_(aSelector: SEL): id;
    function performSelector_withObject_(aSelector: SEL; obj: id): id;
    function performSelector_withObject_withObject(aSelector: SEL; obj1, obj2: id): id;

    function isProxy: boolean;

    function isKindOfClass_(aClass: pobjc_class): boolean;
    function isMemberOfClass_(aClass: pobjc_class): boolean;
    function conformsToProtocol_(aProtocol: Protocol): boolean;

    function respondsToSelector_(aSelector: SEL): boolean;

    function retain: id;
    procedure release; { oneway }
    function autorelease: id;
    function retainCount: cint;

    function description: {NSString} id;

    { NSObject class }
    { "class" prefix to method name to avoid name collision with NSObjectProtocol }
    class function classIsEqual_(obj: id): boolean; message 'isEqual:';
    { "class" prefix to method name to avoid name collision with NSObjectProtocol }
    class function classHash: cuint; message 'hash';

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

    { "class" prefix to method name to avoid name collision with NSObjectProtocol }
    class function classSuperclass: pobjc_class; message 'superclass';
    { "class" prefix to method name to avoid name collision with NSObjectProtocol }
    class function classClass: pobjc_class; message 'class';
    class procedure poseAsClass_(aClass: pobjc_class); message 'poseAsClass:';
    class function instancesRespondToSelector_(aSelector: SEL): boolean; message 'instancesRespondToSelector:';
    { "class" prefix to method name to avoid name collision with NSObjectProtocol }
    class function classConformsToProtocol_(aProtocol: Protocol): boolean; message 'conformsToProtocol:';
    function methodForSelector_(aSelector: SEL): IMP; message 'methodForSelector:';
    class function instanceMethodForSelector_(aSelector: SEL): IMP; message 'instanceMethodForSelector:';
    class function version: cint; message 'version';
    class procedure setVersion_(aVersion: cint); message 'setVersion:';
    procedure doesNotRecognizeSelector_(aSelector: SEL); message 'doesNotRecognizeSelector:';
    procedure forwardInvocation_(anInvocation: id {NSInvocation}); message 'forwardInvocation:';
    function methodSignatureForSelector_(aSelector: SEL): id {NSMethodSignature}; message 'methodSignatureForSelector:';

    class function classDescription: id {NSString}; message 'description';

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


