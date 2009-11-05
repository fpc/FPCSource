{ Parsed from Foundation.framework NSObjCRuntime.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSOBJCRUNTIME_PAS_T}
{$define NSOBJCRUNTIME_PAS_T}

{ Defines }
const
  NSFoundationVersionNumber10_0 = 397.40;
  NSFoundationVersionNumber10_1 = 425.00;
  NSFoundationVersionNumber10_1_1 = 425.00;
  NSFoundationVersionNumber10_1_2 = 425.00;
  NSFoundationVersionNumber10_1_3 = 425.00;
  NSFoundationVersionNumber10_1_4 = 425.00;
  NSFoundationVersionNumber10_2 = 462.00;
  NSFoundationVersionNumber10_2_1 = 462.00;
  NSFoundationVersionNumber10_2_2 = 462.00;
  NSFoundationVersionNumber10_2_3 = 462.00;
  NSFoundationVersionNumber10_2_4 = 462.00;
  NSFoundationVersionNumber10_2_5 = 462.00;
  NSFoundationVersionNumber10_2_6 = 462.00;
  NSFoundationVersionNumber10_2_7 = 462.70;
  NSFoundationVersionNumber10_2_8 = 462.70;
  NSFoundationVersionNumber10_3 = 500.00;
  NSFoundationVersionNumber10_3_1 = 500.00;
  NSFoundationVersionNumber10_3_2 = 500.30;
  NSFoundationVersionNumber10_3_3 = 500.54;
  NSFoundationVersionNumber10_3_4 = 500.56;
  NSFoundationVersionNumber10_3_5 = 500.56;
  NSFoundationVersionNumber10_3_6 = 500.56;
  NSFoundationVersionNumber10_3_7 = 500.56;
  NSFoundationVersionNumber10_3_8 = 500.56;
  NSFoundationVersionNumber10_3_9 = 500.58;
  NSFoundationVersionNumber10_4 = 567.00;
  NSFoundationVersionNumber10_4_1 = 567.00;
  NSFoundationVersionNumber10_4_2 = 567.12;
  NSFoundationVersionNumber10_4_3 = 567.21;
  NSFoundationVersionNumber10_4_4_Intel = 567.23;
  NSFoundationVersionNumber10_4_4_PowerPC = 567.21;
  NSFoundationVersionNumber10_4_5 = 567.25;
  NSFoundationVersionNumber10_4_6 = 567.26;
  NSFoundationVersionNumber10_4_7 = 567.27;
  NSFoundationVersionNumber10_4_8 = 567.28;
  NSFoundationVersionNumber10_4_9 = 567.29;
  NSFoundationVersionNumber10_4_10 = 567.29;
  NSFoundationVersionNumber10_4_11 = 567.36;
  NSINTEGER_DEFINED = 1;

{ Types }
type
  NSInteger = cint;
  NSUInteger = cuint;
  NSComparisonResult = NSInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSOBJCRUNTIME_PAS_R}
{$define NSOBJCRUNTIME_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSOBJCRUNTIME_PAS_F}
{$define NSOBJCRUNTIME_PAS_F}

{ Functions }
function NSStringFromSelector(aSelector: SEL): NSString; cdecl; external name 'NSStringFromSelector';
function NSSelectorFromString(aSelectorNamePointer: NSString): SEL; cdecl; external name 'NSSelectorFromString';
function NSStringFromClass(aClass: Pobjc_class): NSString; cdecl; external name 'NSStringFromClass';
function NSClassFromString(aClassNamePointer: NSString): Pobjc_class; cdecl; external name 'NSClassFromString';
function NSStringFromProtocol(protoPointer: Protocol): NSString; cdecl; external name 'NSStringFromProtocol';
function NSProtocolFromString(namestrPointer: NSString): Protocol; cdecl; external name 'NSProtocolFromString';
function NSGetSizeAndAlignment(typePtrPointer: char; sizepPointer: NSUInteger; alignpPointer: NSUInteger): char; cdecl; external name 'NSGetSizeAndAlignment';
procedure NSLogv(formatPointer: NSString; args: va_list); cdecl; external name 'NSLogv';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSOBJCRUNTIME_PAS_S}
{$define NSOBJCRUNTIME_PAS_S}

{ External symbols }
var
  NSFoundationVersionNumber: double; external name '_NSFoundationVersionNumber';

{$endif}
{$endif}

{$ifdef USER_PATCHES}
{$ifndef NSOBJCRUNTIME_PAS_PATCH}
{$define NSOBJCRUNTIME_PAS_PATCH}
{$ifdef INTERFACE}
procedure NSLog(fmt:NSString); cdecl; varargs; external;
{$endif}

{$endif}
{$endif}
