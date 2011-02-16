{ Parsed from Foundation.framework NSObjCRuntime.h }

{$ifdef TYPES}
{$ifndef NSOBJCRUNTIME_PAS_T}
{$define NSOBJCRUNTIME_PAS_T}

{ Defines }
const
  NS_BLOCKS_AVAILABLE = 0;
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
  NSFoundationVersionNumber10_5 = 677.00;
  NSFoundationVersionNumber10_5_1 = 677.10;
  NSFoundationVersionNumber10_5_2 = 677.15;
  NSFoundationVersionNumber10_5_3 = 677.19;
  NSFoundationVersionNumber10_5_4 = 677.19;
  NSFoundationVersionNumber10_5_5 = 677.21;
  NSFoundationVersionNumber10_5_6 = 677.22;
  NSFoundationVersionNumber10_5_7 = 677.24;
  NSFoundationVersionNumber10_5_8 = 677.26;
  NSFoundationVersionNumber10_6 = 751.00;
  NSFoundationVersionNumber10_6_1 = 751.00;
  NSFoundationVersionNumber10_6_2 = 751.14;
  NSFoundationVersionNumber10_6_3 = 751.21;
  NSFoundationVersionNumber_iPhoneOS_2_0 = 678.24;
  NSFoundationVersionNumber_iPhoneOS_2_1 = 678.26;
  NSFoundationVersionNumber_iPhoneOS_2_2 = 678.29;
  NSFoundationVersionNumber_iPhoneOS_3_0 = 678.47;
  NSFoundationVersionNumber_iPhoneOS_3_1 = 678.51;
  NSFoundationVersionNumber_iPhoneOS_3_2 = 678.60;
  NSINTEGER_DEFINED = 1;

{ Types }
type
{$if defined(cpu64) or defined(cpuarm) or defined(win32)}
  NSInteger = clong;
  NSUInteger = culong;
{$else}
  NSInteger = cint;
  NSUInteger = cuint;
{$endif}
  NSIntegerPtr = ^NSInteger;
  NSUIntegerPtr = ^NSUInteger;
  NSComparisonResult = NSInteger;
  NSComparisonResultPtr = ^NSComparisonResult;
  NSEnumerationOptions = NSUInteger;
  NSEnumerationOptionsPtr = ^NSEnumerationOptions;
  NSSortOptions = NSUInteger;
  NSSortOptionsPtr = ^NSSortOptions;

{ Constants }

const
  NSOrderedAscending = -1;
  NSOrderedSame = 0;
  NSOrderedDescending = 1;

const
  NSEnumerationConcurrent = 1 shl 0;
  NSEnumerationReverse = 1 shl 1;

const
  NSSortConcurrent = 1 shl 0;
  NSSortStable = 1 shl 4;

const
  NSIntegerMax = high(NSInteger);
  NSIntegerMin = low(NSInteger);
  NSUIntegerMin = low(NSUinteger);
  NSUIntegerMax = high(NSUinteger);
  NSNotFound = NSIntegerMax;

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
function NSStringFromSelector(aSelector: SEL): NSString; cdecl; external;
function NSSelectorFromString(aSelectorName: NSString): SEL; cdecl; external;
function NSStringFromClass(aClass: Pobjc_class): NSString; cdecl; external;
function NSClassFromString(aClassName: NSString): Pobjc_class; cdecl; external;
function NSStringFromProtocol(proto: Protocol): NSString; cdecl; external;
function NSProtocolFromString(namestr: NSString): Protocol; cdecl; external;
function NSGetSizeAndAlignment(typePtr: PChar; sizep: NSUIntegerPtr; alignp: NSUIntegerPtr): PChar; cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSOBJCRUNTIME_PAS_S}
{$define NSOBJCRUNTIME_PAS_S}

{ External symbols }
var
  NSFoundationVersionNumber: double; cvar; external;

{$endif}
{$endif}

{$ifdef USER_PATCHES}
{$ifdef INTERFACE}
procedure NSLog(fmt:NSString); cdecl; varargs; external;
{$endif}



{$endif}
