<?php

$version = "FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.1";

require("pascocoa_parser.php");
require("objp_parser.php");

/**
 * Cocoa framework parser for PasCocoa
 * 
 * @author Ryan Joseph
 **/

/* VERSION HISTORY

1.3		- The parser is broken into subclasses in order to support the new FPC syntax.
		
1.2		- struct_register_types makes a new wrapper that sends structs based on CPU architecture
		- var parameters are parsed correctly 
		- getters and setters can now be overriden (still not constructors however)
		- enums without defined values are parsed
		- params named "somethingPtr" are parsed as pointers instead of "var"
		- #defines (integer values only)
		- fixed a bug missing CFString constants
		- added static accessor withObject
		- fixed bug with constructors not using NSObject "Handles" from parameter list
		- custom implementations for NSSet, NSString, NSDictionary constructors
		- IBActions now replace with void, like the C macro
		- Methods with (void *) as the return type are no longer parsed as procedures.
		
1.1:	- NSDelegatesAll now creates a .pas reference file
		- Methods in NSDelegatesAll are no longer declared abstract virtual to avoid compiler performance issues
		- super methods were deprecated in favor of "implemented super" methods
		- all non-delegate categories (which appear in NSDelegatesAll.inc) are merged into NSObject.inc
*/

/* TO-DO
	- NSAfflineTransform and NSAttributedString are making duplicates because they exist in AppKit And Foundation
		- the fix now is to delete the parts that are conflicting but leave them in the master file
	- NSGeometry, NSRange, NSHashTable, NSMapTable have conflicts with record/type order use, AddTypeDef instead
	- NSObjCRuntime, NSZone has errors parsing external functions
		- these are removed from the parse all command for now
	- NSExceptions didn't parse: typedef void NSUncaughtExceptionHandler(NSException *exception);
	- 	did not get captured by the regex:
		FOUNDATION_EXPORT NSString *NSStringFromRange(NSRange range); 
		√ FOUNDATION_EXPORT NSString * const NSParseErrorException; 
		√ FOUNDATION_EXPORT NSString * const NSCharacterConversionException;
	- 	compound enums on a single line
		enum {NSNotFound = NSIntegerMax};
		enum _NSComparisonResult {NSOrderedAscending = -1, NSOrderedSame, NSOrderedDescending};
		typedef struct {NSUInteger _pi; NSUInteger _si; void *_bs;} NSHashEnumerator;
	
	• SERIOUS
	
	* - Make functions in NS*** collection classes to return a CFType equivalent
				function CFType: CFTypeRef; at the top of each class
	
	* Var params are not initialized to nil inside the methods!
	
	* Some methods are broken up into different lines and not parsed! I'm sure we losts hundreds of methods
	  look in NSBezierPath.h for examples
	
		- (void)appendBezierPathWithArcWithCenter:(NSPoint)center radius:(CGFloat)radius
					       startAngle:(CGFloat)startAngle
						 endAngle:(CGFloat)endAngle;
		
				
	- erroneous var params in plain C functions
		function NSSelectorFromString(var aSelectorName: CFStringRef): SEL; cdecl; external name 'NSSelectorFromString';
		FOUNDATION_EXPORT SEL NSSelectorFromString(NSString *aSelectorName);
		
	- nested enums
		NSXMLNodePreserveQuotes = (NSXMLNodeUseSingleQuotes | NSXMLNodeUseDoubleQuotes),	
		NSWorkspaceLaunchDefault = NSWorkspaceLaunchAsync | NSWorkspaceLaunchAllowingClassicStartup
	
	- comma was at the START of the line so it didn't get parsed
		    NSRegularControlSize,
		    NSSmallControlSize
		#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3
		    , NSMiniControlSize
	
	- constant strings with AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER are not parsed 
	APPKIT_EXTERN NSString *NSDocFormatTextDocumentType	AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER;
	APPKIT_EXTERN NSString *NSWordMLTextDocumentType	AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER;
	APPKIT_EXTERN NSString *NSWebArchiveTextDocumentType	AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER;
	
	- How should we handle inout and out? Are these passed by referenced?
	- Should we include const in the params?
	- How should be handle inline array types? NSRect[4] -> type theMethodType1 = array[1..4] of NSRect
	
	
*/

/**
 * PRIMARY INPUT PARAMETERS
 */

// These files have duplicates in AppKit and Foundation so we ignore the foundation versions and everything is merged into AppKit
$duplicate_headers = array("foundation/NSAttributedString.inc", "foundation/NSAffineTransform.inc");

// Print only these files
$only_files = null;

$options = array();

function HandleCommandLineOptions ($argv) {
	global $options;
	global $output_directory;
	global $root_path;
	global $ignore_headers;
	global $only_files;
	
	foreach ($argv as $option) {
		$pair = explode("=", $option);
		$key = trim($pair[0], "-");
		$value = $pair[1];
		
		switch ($key) {
			
			case 'all':
				$options[$key] = true;
				break;
				
			case 'objp':
				$options[$key] = true;
				break;
					
			case 'encodings':
				$options[$key] = true;
				break;
				
			case 'delegates':
				$options[$key] = true;
				break;
				
			case 'noprint':
				$options[$key] = true;
				break;
			
			case 'show':
				$options[$key] = true;
				break;
			
			case 'ignore':
				$ignore_headers = explode(",", trim($value, "\""));
				break;
				
			case 'only':
				$only_files = explode(",", trim($value, "\""));
				break;
			
			case 'out':
				$output_directory = $value;
				$options[$key] = true;
				break;
			
			case 'root':
				$root_path = trim($value, "\"");
				break;
				
			case 'header':
				$where = explode("/", trim($value, "\""));
				$options[$key]["framework"] = ucfirst($where[0]);
				$options[$key]["name"] = $where[1];
				break;
			
			default:
				//print("unknown switch $key\n");
				break;
		}
	}
}

// ??? TESTING
$testing = false;

if ($testing) {
	$GLOBALS["root"][] = "/Developer/pascocoa";
	//$GLOBALS["argv"][] = "-all";
	//$GLOBALS["argv"][] = "-noprint";
	//$GLOBALS["argv"][] = "-only=\"NSHashTable.h\"";

	$GLOBALS["argv"][] = "-header=\"foundation/NSDate.h\"";
	//$GLOBALS["argv"][] = "-header=\"appkit/NSView.h\"";

	// PasCocoaKit
	//$GLOBALS["argv"][] = "-out=/dev";
	//$GLOBALS["argv"][] = "-ignore=\"NSObject.h,NSHashTable.h,NSGeometry\"";
	//$GLOBALS["argv"][] = "-delegates";

	// Objective-P
	/* Notes for master compile (-all):

	- Errors:
	1) NSWorkspace.h has a duplicate NSWorkspaceLaunchAllowingClassicStartup constant
	2) NSObjcRuntime.h contains a bad external function:
		function __attribute__(: (format(__NSString__; : 1; : 2))): void NSLog(NSStringRef *format, ...); cdecl; external name '__attribute__';
	3) NSZone.h contains 2 function parser errors:
		function NSAllocateCollectable(size: culong; options: culong): void *__strong; cdecl; external name 'NSAllocateCollectable';
		function NSReallocateCollectable(var ptr: Pointer; size: culong; options: culong): void *__strong; cdecl; external name 'NSReallocateCollectable';
	4) NSATSTypesetter.h needs to use NSObject for the super class until the internal error can be fixed or the class NSTypesetter parsed

	- General notes:
	1) NSObject.h is parsed for the merged category methods that should be inserted manually into the real root class
	2) NSRange.h was ignored because it contains custom code and can be maintained by hand very easily
	3) NSGeometry.h was ignored because it contains many parsing errors and custom code, do this by hand for now.
	*/
	$GLOBALS["argv"][] = "-out=/objp";
	$GLOBALS["argv"][] = "-ignore=\"NSGeometry.h,NSRange.h\""; //NSMapTable.h,NSHashTable.h
	$GLOBALS["argv"][] = "-objp";

	//$GLOBALS["argv"][] = "-show";
}

if (count($GLOBALS["argv"]) == 1) {
	print("Cocoa Framework Parser ($version) usage:\n");
	print("php framework_parser.php [switches]\n\n");
	print("switches:\n\n");
	print("  -all           print all headers (.h) from AppKit/Foundation frameworks\n");
	print("  -header=\"foundation/NSObject.h\"    prints a single header from system frameworks\n");
	print("  -root          sets the root path of the pascocoa directory\n");
	print("  -out           defines an output directory in the root directory for printing, use for testing\n");
	print("  -show     	    prints output to screen instead of file\n");
	print("  -ignore=\"NSObject.h,NSArray.h\"     ignores the list of headers during parsing (-all only, no spaces)\n");
	print("  -only=\"NSObject.h,NSArray.h\"       only prints these files (-all only, no spaces)\n");
	print("  -noprint       parses but does not print (-all only)\n");
	print("  -encodings     prints Pascal type encoding glue for GenerateTypeEncodings.p (-all only)\n");
	print("  -delegates     prints NSDelegateController.inc to foundation (-all only)\n");
	print("  -objp     		prints classes in FPC Objective-P dialect\n");
	print("\n\n");
}

// get the command line options
if (count($GLOBALS["argv"]) > 1) {
	HandleCommandLineOptions($GLOBALS["argv"]);
	//print_r($options);
}

// Make the output directory
if ($options["out"]) {
	@mkdir($root_path.$output_directory, 0777);
	@mkdir($root_path.$output_directory."/foundation", 0777);
	@mkdir($root_path.$output_directory."/appkit", 0777);
	@mkdir($root_path.$output_directory."/webkit", 0777);
	@mkdir($root_path.$output_directory."/reference", 0777);
}

// create the parser instance
if ($options["objp"]) {
	$parser = new TObjPParser ($root_path, $output_directory, $options["show"]);
} else {
	$parser = new TPasCocoaParser ($root_path, $output_directory, $options["show"]);
}

// Process single headers
if ($options["header"]) {
	$parser->ProcessFile("/System/Library/Frameworks/".$options["header"]["framework"].".framework/Headers/".$options["header"]["name"], true);
}

// Process all headers
if ($options["all"]) {
	$parser->ParseCocoaFrameworks($ignore_headers, null);
	if (!$options["noprint"]) $parser->PrintAllHeaders("", $duplicate_headers, $only_files);
	if ($options["delegates"]) $parser->ParseDelegateClasses();
	if ($options["encodings"]) $parser->PrintTypeEncodingGlue();
}

?>