{ Parsed from Foundation.framework NSInvocation.h }


{$ifdef TYPES}
type
  NSInvocationPtr = ^NSInvocation;
{$endif}

{$ifdef CLASSES}

type
  NSInvocation = objcclass external (NSObject)
  private
    _frame: pointer;
    _retdata: pointer;
    _signature: id;
    _container: id;
    _retainedArgs: cuint8;
    _reserved:array[0..14] of cuint8;
  public
    class function invocationWithMethodSignature (sig: NSMethodSignature): NSInvocation; message 'invocationWithMethodSignature:';
    function methodSignature: NSMethodSignature; message 'methodSignature';
    procedure retainArguments; message 'retainArguments';
    function argumentsRetained: ObjCBOOL; message 'argumentsRetained';
    procedure setTarget(newValue: id); message 'setTarget:';
    function target: id; message 'target';
    procedure setSelector(newValue: SEL); message 'setSelector:';
    function selector: SEL; message 'selector';
    procedure getReturnValue (retLoc: pointer); message 'getReturnValue:';
    procedure setReturnValue (retLoc: pointer); message 'setReturnValue:';
    procedure getArgument_atIndex (argumentLocation: pointer; idx: NSInteger); message 'getArgument:atIndex:';
    procedure setArgument_atIndex (argumentLocation: pointer; idx: NSInteger); message 'setArgument:atIndex:';
    procedure invoke; message 'invoke';
    procedure invokeWithTarget (target_: id); message 'invokeWithTarget:';
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED <= MAC_OS_X_VERSION_10_5)}
{$ifdef TYPES}
const
  NSObjCNoType = 0;
  NSObjCVoidType = 'v';
  NSObjCCharType = 'c';
  NSObjCShortType = 's';
  NSObjCLongType = 'l';
  NSObjCLonglongType = 'q';
  NSObjCFloatType = 'f';
  NSObjCDoubleType = 'd';
  NSObjCBoolType = 'B';
  NSObjCSelectorType = ':';
  NSObjCObjectType = '@';
  NSObjCStructType = '{';
  NSObjCPointerType = '^';
  NSObjCStringType = '*';
  NSObjCArrayType = '[';
  NSObjCUnionType = '';
  NSObjCBitfield = 'b';

type
  NSObjCValue = record
    type_: NSInteger deprecated 'in 10_0, 10_5, 2_0, 2_0';
    value: record
      case longint of
        0: (charValue: char);
        1: (shortValue: cshort);
        2: (longValue: clong);
        3: (longlongValue: clonglong);
        4: (floatValue: single);
        5: (doubleValue: double);
        6: (boolValue: cbool);
        7: (selectorValue: SEL);
        8: (objectValue: id);
        9: (pointerValue: pointer);
        10: (structLocation: pointer);
        11: (cStringLocation: PChar);
    end;
  end;
type
  NSObjCValuePtr = ^NSObjCValue;
{$endif}

{$endif}
{$endif}
