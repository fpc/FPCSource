{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$macro on}

{$define instancetype := id}

{$ifndef LOADED_AVAILABILITY_MACROS}
{$define LOADED_AVAILABILITY_MACROS}

// System Versions MacOSX SDK
{$define MAC_OS_X_VERSION_10_0 := 1000}
{$define MAC_OS_X_VERSION_10_1 := 1010}
{$define MAC_OS_X_VERSION_10_2 := 1020}
{$define MAC_OS_X_VERSION_10_3 := 1030}
{$define MAC_OS_X_VERSION_10_4 := 1040}
{$define MAC_OS_X_VERSION_10_5 := 1050}
{$define MAC_OS_X_VERSION_10_6 := 1060}
{$define MAC_OS_X_VERSION_10_7 := 1070}
{$define MAC_OS_X_VERSION_10_8 := 1080}
{$define MAC_OS_X_VERSION_10_9 := 1090}
{$define MAC_OS_X_VERSION_10_10 := 101000}
{$define MAC_OS_X_VERSION_LATEST := MAC_OS_X_VERSION_10_10}

// System Versions iPhoneOS SDK
{$define __MAC_10_0 := 1000}
{$define __MAC_10_1 := 1010}
{$define __MAC_10_2 := 1020}
{$define __MAC_10_3 := 1030}
{$define __MAC_10_4 := 1040}
{$define __MAC_10_5 := 1050}
{$define __MAC_10_6 := 1060}
{$define __MAC_10_7 := 1070}
{$define __MAC_10_8 := 1080}
{$define __MAC_10_9 := 1090}
{$define __MAC_10_10 := 1100}
{$define __MAC_NA   := 9999}   

{$define __IPHONE_2_0 := 20000}
{$define __IPHONE_2_1 := 20100}
{$define __IPHONE_2_2 := 20200}
{$define __IPHONE_3_0 := 30000}
{$define __IPHONE_3_1 := 30100}
{$define __IPHONE_3_2 := 30200}
{$define __IPHONE_4_0 := 40000}
{$define __IPHONE_4_1 := 40100}
{$define __IPHONE_4_2 := 40200}
{$define __IPHONE_4_3 := 40300}
{$define __IPHONE_5_0 := 50000}
{$define __IPHONE_6_0 := 60000}
{$define __IPHONE_7_0 := 70000}
{$define __IPHONE_8_0 := 80000}
{$define __IPHONE_NA  := 99999}  
{$define __IPHONE_LATEST  := __IPHONE_8_0}

// Target Conditionals
{$if defined(CPUPOWERPC32)}
	{$define TARGET_CPU_PPC}
	{$undef TARGET_CPU_PPC64}
	{$undef TARGET_CPU_X86}
	{$undef TARGET_CPU_X86_64}
	{$undef TARGET_CPU_ARM}
	{$define TARGET_OS_MAC}
	{$undef TARGET_OS_IPHONE}
	{$undef TARGET_IPHONE_SIMULATOR}
	{$undef TARGET_RT_64_BIT}
{$elseif defined(CPUPOWERPC64)}
	{$undef TARGET_CPU_PPC}
	{$define TARGET_CPU_PPC64}
	{$undef TARGET_CPU_X86}
	{$undef TARGET_CPU_X86_64}
	{$undef TARGET_CPU_ARM}
	{$define TARGET_OS_MAC}
	{$undef TARGET_OS_IPHONE}
	{$undef TARGET_IPHONE_SIMULATOR}
	{$define TARGET_RT_64_BIT}
{$elseif defined(CPUI386)}
	{$undef TARGET_CPU_PPC}
	{$undef TARGET_CPU_PPC64}
	{$define TARGET_CPU_X86}
	{$undef TARGET_CPU_X86_64}
	{$undef TARGET_CPU_ARM}
	{$undef TARGET_RT_64_BIT}
	{$if defined(IPHONESIM)}
	 	{$undef TARGET_OS_MAC}
		{$define TARGET_OS_IPHONE}
		{$define TARGET_IPHONE_SIMULATOR}
	{$else}
		{$define TARGET_OS_MAC}
		{$undef TARGET_OS_IPHONE}
		{$undef TARGET_IPHONE_SIMULATOR}
	{$endif}
{$elseif defined(CPUX86_64)}
	{$undef TARGET_CPU_PPC}
	{$undef TARGET_CPU_PPC64}
	{$undef TARGET_CPU_X86}
	{$define TARGET_CPU_X86_64}
	{$undef TARGET_CPU_ARM}
	{$define TARGET_OS_MAC}
	{$undef TARGET_OS_IPHONE}
	{$undef TARGET_IPHONE_SIMULATOR}
	{$define TARGET_RT_64_BIT}
{$elseif defined(CPUARM)}
	{$undef TARGET_CPU_PPC}
	{$undef TARGET_CPU_PPC64}
	{$undef TARGET_CPU_X86}
	{$undef TARGET_CPU_X86_64}
	{$define TARGET_CPU_ARM}
	{$undef TARGET_OS_MAC}
	{$define TARGET_OS_IPHONE}
	{$undef TARGET_IPHONE_SIMULATOR}
	{$undef TARGET_RT_64_BIT}
{$endif}

{$ifdef CPU64}
  {$define TARGET_CPU_64}
	{$define TARGET_RT_64_BIT}
{$else}
  {$undef TARGET_CPU_64}
	{$undef TARGET_RT_64_BIT}
{$endif}

{$if defined(FPC_BIG_ENDIAN)}
	{$define TARGET_RT_BIG_ENDIAN}
	{$undef TARGET_RT_LITTLE_ENDIAN}
{$elseif defined(FPC_LITTLE_ENDIAN)}
	{$define TARGET_RT_LITTLE_ENDIAN}
	{$undef TARGET_RT_BIG_ENDIAN}
{$endif}

{$undef DEPLOYMENT_TARGET_EMBEDDED}
{$undef DEPLOYMENT_TARGET_WINDOWS}

{$define TARGET_API_MAC_CARBON}
{$undef TARGET_API_MAC_OS8}
{$define TARGET_API_MAC_OSX}
{$define TARGET_CARBON}
{$undef TARGET_CPU_68K}
{$undef TARGET_CPU_MIPS}
{$undef TARGET_CPU_SPARC}
{$undef TARGET_CPU_ALPHA}
{$undef TARGET_OS_UNIX}
{$undef TARGET_OS_WIN32}
{$undef TARGET_OS_EMBEDDED}
{$undef TARGET_RT_MAC_68881}
{$undef TARGET_RT_MAC_CFM}
{$define TARGET_RT_MAC_MACHO}

{$undef __OBJC2__}
{$undef __BLOCKS__}
{$undef NS_BLOCKS_AVAILABLE}
{$undef NS_BUILD_32_LIKE_64}
{$undef NS_NONATOMIC_IOSONLY}

{$define ACCESSOR_CALLS_ARE_FUNCTIONS}
{$undef CALL_NOT_IN_CARBON}
{$undef OLDROUTINENAMES}
{$define OPAQUE_TOOLBOX_STRUCTS}
{$define OPAQUE_UPP_TYPES}
{$define OTCARBONAPPLICATION}
{$undef OTKERNEL}
{$define PM_USE_SESSION_APIS}
{$define TYPED_FUNCTION_POINTERS}
{$undef TYPE_BOOL}
{$undef TYPE_EXTENDED}
{$define TYPE_LONGLONG}

{$ifdef CPU64}
  {$define __LP64__}
{$else}
  {$undef __LP64__}
{$endif}

// Mac OS X Version Requirements
{$if defined(TARGET_CPU_PPC64) or defined(TARGET_CPU_X86) or defined(TARGET_CPU_X86_64)}
    {$define MAC_OS_X_VERSION_MIN_REQUIRED := MAC_OS_X_VERSION_10_4}
{$elseif defined(TARGET_CPU_ARM)}
    {$define MAC_OS_X_VERSION_MIN_REQUIRED := MAC_OS_X_VERSION_10_5}
{$else}
    {$define MAC_OS_X_VERSION_MIN_REQUIRED := MAC_OS_X_VERSION_10_1}
{$endif}
{$define MAC_OS_X_VERSION_MAX_ALLOWED := MAC_OS_X_VERSION_LATEST}

// iOS Version Requirements
{$define __IPHONE_OS_VERSION_MAX_ALLOWED := __IPHONE_LATEST}
{$define __IPHONE_OS_VERSION_MIN_REQUIRED := __IPHONE_2_0}

// Mac OS X compiled for iOS
{$define __MAC_OS_X_VERSION_MIN_REQUIRED := MAC_OS_X_VERSION_MIN_REQUIRED}
{$define __MAC_OS_X_VERSION_MAX_ALLOWED := MAC_OS_X_VERSION_LATEST}

{$endif}

unit WebKit;
interface

{$linkframework WebKit}

uses
{$ifdef COCOAALL}
	{$ifdef MACOSALL}
	  DefinedClassesAppKit, DefinedClassesWebKit, CocoaAll, CTypes, MacOSAll;
	{$else}
		DefinedClassesAppKit, DefinedClassesWebKit, CTypes, MacTypes, MacOSXPosix, CocoaAll, AEDataModel, IconsCore, IOSurfaceAPI, SecBase, SecTrust, MacGL, CGLTypes, CFBase, CFArray, CFBag, CFCharacterSet, CFData, CFDate, CFDictionary, CFNumber, CFPropertyList, CFSet, CFString, CFStringEncodingExt, CFTimeZone, CFTree, CFURL, CFXMLNode, CFXMLParser, CFMachPort, CFMessagePort, CFRunLoop, CFSocket, CFBinaryHeap, CFBitVector, CFBundle, CFByteOrders, CFPlugIn, CFPreferences, CFURLAccess, CFUUID, CFLocale, CFStream, CFDateFormatter, CFNumberFormatter, CFCalendar, CFUserNotification, CFNotificationCenter, CFAttributedString, CFNetworkErrorss, CGBase, CGAffineTransforms, CGBitmapContext, CGColor, CGColorSpace, CGContext, CGDataConsumer, CGDataProvider, CGDirectDisplay, CGDirectPalette, CGDisplayConfiguration, CGDisplayFades, CGErrors, CGEvent, CGEventSource, CGEventTypes, CGFont, CGFunction, CGGLContext, CGGeometry, CGImage, CGLayer, CGPDFArray, CGPDFContentStream, CGPDFContext, CGPDFDictionary, CGPDFDocument, CGPDFObject, CGPDFOperatorTable, CGPDFPage, CGPDFScanner, CGPDFStream, CGPDFString, CGPSConverter, CGPath, CGPattern, CGRemoteOperation, CGSession, CGShading, CGWindowLevels, CVBase, CVImageBuffer;
	{$endif}
{$else}
	{$ifdef MACOSALL}
	  DefinedClassesAppKit, DefinedClassesWebKit, AppKit, Foundation, CTypes, MacOSAll;
	{$else}
		DefinedClassesAppKit, DefinedClassesWebKit, AppKit, Foundation, CTypes, MacTypes, MacOSXPosix, AEDataModel, IconsCore, IOSurfaceAPI, SecBase, SecTrust, MacGL, CGLTypes, CFBase, CFArray, CFBag, CFCharacterSet, CFData, CFDate, CFDictionary, CFNumber, CFPropertyList, CFSet, CFString, CFStringEncodingExt, CFTimeZone, CFTree, CFURL, CFXMLNode, CFXMLParser, CFMachPort, CFMessagePort, CFRunLoop, CFSocket, CFBinaryHeap, CFBitVector, CFBundle, CFByteOrders, CFPlugIn, CFPreferences, CFURLAccess, CFUUID, CFLocale, CFStream, CFDateFormatter, CFNumberFormatter, CFCalendar, CFUserNotification, CFNotificationCenter, CFAttributedString, CFNetworkErrorss, CGBase, CGAffineTransforms, CGBitmapContext, CGColor, CGColorSpace, CGContext, CGDataConsumer, CGDataProvider, CGDirectDisplay, CGDirectPalette, CGDisplayConfiguration, CGDisplayFades, CGErrors, CGEvent, CGEventSource, CGEventTypes, CGFont, CGFunction, CGGLContext, CGGeometry, CGImage, CGLayer, CGPDFArray, CGPDFContentStream, CGPDFContext, CGPDFDictionary, CGPDFDocument, CGPDFObject, CGPDFOperatorTable, CGPDFPage, CGPDFScanner, CGPDFStream, CGPDFString, CGPSConverter, CGPath, CGPattern, CGRemoteOperation, CGSession, CGShading, CGWindowLevels, CVBase, CVImageBuffer;
	{$endif}
{$endif}

{$define INTERFACE}
{$define CGFLOAT_DEFINED}
{$define NSINTEGER_DEFINED}

type
  OpaqueRecord = record end;
  OpaqueCBlock = pointer;
  OpaqueType = ^SInt32;
  objc_protocol = protocol;
  idPtr = ^id;
  SELPtr = ^SEL;
  va_list_rec = OpaqueRecord;
  va_list = ^va_list_rec;

{$include UndefinedTypes.inc}
{$include webkit/UndefinedTypes.inc}

{$define TYPES}
{$include webkit/Sources.inc}
{$undef TYPES}

{$define RECORDS}
{$include webkit/Sources.inc}
{$undef RECORDS}

{$define PROTOCOLS}
{$include webkit/Sources.inc}
{$undef PROTOCOLS}

{$define CLASSES}
{$include webkit/Sources.inc}
{$undef CLASSES}

{$define FUNCTIONS}
{$include webkit/Sources.inc}
{$undef FUNCTIONS}

{$define EXTERNAL_SYMBOLS}
{$include webkit/Sources.inc}
{$undef EXTERNAL_SYMBOLS}

{$include webkit/InlineFunctions.inc}

{$undef INTERFACE}
implementation
{$define IMPLEMENTATION}
{$include webkit/InlineFunctions.inc}
{$undef IMPLEMENTATION}
end.