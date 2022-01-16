{ Parsed from Foundation.framework NSObjCRuntime.h }

{$if not defined(instancetype)}
{$ifdef TYPES}
type
  instancetype = id;
{$endif}

{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSFoundationVersionNumber: double; cvar; external;
{$endif}

{$if defined(TARGET_OS_MAC)}
{$ifdef TYPES}
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
  NSFoundationVersionNumber10_6_4 = 751.29;
  NSFoundationVersionNumber10_6_5 = 751.42;
  NSFoundationVersionNumber10_6_6 = 751.53;
  NSFoundationVersionNumber10_6_7 = 751.53;
  NSFoundationVersionNumber10_6_8 = 751.62;
  NSFoundationVersionNumber10_7 = 833.10;
  NSFoundationVersionNumber10_7_1 = 833.10;
  NSFoundationVersionNumber10_7_2 = 833.20;
  NSFoundationVersionNumber10_7_3 = 833.24;
  NSFoundationVersionNumber10_7_4 = 833.25;
  NSFoundationVersionNumber10_8 = 945.00;
  NSFoundationVersionNumber10_8_1 = 945.00;
  NSFoundationVersionNumber10_8_2 = 945.11;
  NSFoundationVersionNumber10_8_3 = 945.16;
  NSFoundationVersionNumber10_8_4 = 945.18;
  NSFoundationVersionNumber10_9 = 1056;
  NSFoundationVersionNumber10_9_1 = 1056;
  NSFoundationVersionNumber10_9_2 = 1056.13;
{$endif}

{$endif}

{$if defined(TARGET_OS_IPHONE)}
{$ifdef TYPES}
const
  NSFoundationVersionNumber_iPhoneOS_2_0 = 678.24;
  NSFoundationVersionNumber_iPhoneOS_2_1 = 678.26;
  NSFoundationVersionNumber_iPhoneOS_2_2 = 678.29;
  NSFoundationVersionNumber_iPhoneOS_3_0 = 678.47;
  NSFoundationVersionNumber_iPhoneOS_3_1 = 678.51;
  NSFoundationVersionNumber_iPhoneOS_3_2 = 678.60;
  NSFoundationVersionNumber_iOS_4_0 = 751.32;
  NSFoundationVersionNumber_iOS_4_1 = 751.37;
  NSFoundationVersionNumber_iOS_4_2 = 751.49;
  NSFoundationVersionNumber_iOS_4_3 = 751.49;
  NSFoundationVersionNumber_iOS_5_0 = 881.00;
  NSFoundationVersionNumber_iOS_5_1 = 890.10;
  NSFoundationVersionNumber_iOS_6_0 = 992.00;
  NSFoundationVersionNumber_iOS_6_1 = 993.00;
  NSFoundationVersionNumber_iOS_7_0 = 1047.20;
  NSFoundationVersionNumber_iOS_7_1 = 1047.25;
{$endif}

{$endif}

{$if defined(__LP64__) or (defined(TARGET_OS_EMBEDDED) and not defined(TARGET_OS_IPHONE)) or defined(TARGET_OS_WIN32) or defined(NS_BUILD_32_LIKE_64)}
{$ifdef TYPES}
type
  NSInteger = clong;
  NSIntegerPtr = ^NSInteger;
  NSUInteger = culong;
  NSUIntegerPtr = ^NSUInteger;
{$endif}

{$else}
{$ifdef TYPES}
type
  NSInteger = cint;
  NSIntegerPtr = ^NSInteger;
  NSUInteger = cuint;
  NSUIntegerPtr = ^NSUInteger;
{$endif}

{$endif}
{$ifdef TYPES}
const
  NSIntegerMax = LONG_MAX;
  NSIntegerMin = LONG_MIN;
  NSUIntegerMax = ULONG_MAX;
{$endif}

{$ifdef FUNCTIONS}
function NSStringFromSelector(aSelector: SEL): NSString; cdecl; external;
function NSSelectorFromString(aSelectorName: NSString): SEL; cdecl; external;
function NSStringFromClass(aClass: pobjc_class): NSString; cdecl; external;
function NSClassFromString(aClassName: NSString): pobjc_class; cdecl; external;
function NSStringFromProtocol(proto: Protocol): NSString; cdecl; external; { available in 10_5, 2_0 }
function NSProtocolFromString(namestr: NSString): Protocol; cdecl; external; { available in 10_5, 2_0 }
function NSGetSizeAndAlignment(typePtr: PChar; sizep: NSUIntegerPtr; alignp: NSUIntegerPtr): PChar; cdecl; external;
procedure NSLog(format: NSString); varargs; cdecl; external;
procedure NSLogv(format: NSString; args: va_list); cdecl; external;
{$endif}

{$ifdef TYPES}
type
  NSComparisonResult = NSInteger;
  NSComparisonResultPtr = ^NSComparisonResult;

const
  NSOrderedAscending = -1;
  NSOrderedSame = 0;
  NSOrderedDescending = 1;
{$endif}

{$ifdef TYPES}
type
  NSComparator = OpaqueCBlock;
{$endif}

{$ifdef TYPES}
type
  NSEnumerationOptions = NSUInteger;
  NSEnumerationOptionsPtr = ^NSEnumerationOptions;

const
  NSEnumerationConcurrent = 1 shl 0;
  NSEnumerationReverse = 1 shl 1;

type
  NSSortOptions = NSUInteger;
  NSSortOptionsPtr = ^NSSortOptions;

const
  NSSortConcurrent = 1 shl 0;
  NSSortStable = 1 shl 4;

type
  NSQualityOfService = NSInteger;
  NSQualityOfServicePtr = ^NSQualityOfService;

const
  NSQualityOfServiceUserInteractive = $21;
  NSQualityOfServiceUserInitiated = $19;
  NSQualityOfServiceUtility = $11;
  NSQualityOfServiceBackground = $09;
  NSQualityOfServiceDefault = -1;
const
  NSNotFound = NSIntegerMax;
{$endif}

