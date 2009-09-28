<?php

$version = "FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4";

require("pascocoa_parser.php");
require("objp_parser.php");

/**
 * Cocoa framework parser for PasCocoa
 * 
 * @author Ryan Joseph
 **/

/* VERSION HISTORY

• Objective-P:
0.4		- Support for UIKit has been added
		- Objective-C 2.0 properties are parsed
		- NSString constants with AVAILABLE_MAC_OS_X_VERSION macros are parsed correctly
0.3		- Pass by reference arguments are translated as pointers instead of var
		- Variable length arguments are parsed with the varargs keyword instead of array of const
0.2		- Support for parsing instance variables is added

• PasCocoaKit
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
	global $root_path;
	global $ignore_headers;
	global $only_files;
	
	// defaults
	$options["framework_path"] = "/System/Library/Frameworks";

	foreach ($argv as $option) {
		$pair = explode("=", $option);
		$key = trim($pair[0], "-");
		$value = $pair[1];
		
		switch ($key) {
			
			case 'root':
				$root_path = trim($value, "\"");
				break;
				
			case 'header':
				$where = explode("/", trim($value, "\""));
				$options[$key]["framework"] = ucfirst($where[0]);
				$options[$key]["name"] = $where[1];
				break;
				
			case 'framework_path':
				$options["framework_path"] = trim($value, "\"");
				break;
				
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
				
			case 'reference':
				$options[$key] = true;
				break;
			
			case 'iphone':
				$options[$key] = true;
				break;
				
			case 'cocoa':
				$options[$key] = true;
				break;

			case 'ignore':
				$ignore_headers = explode(",", trim($value, "\""));
				break;

			case 'only':
				$only_files = explode(",", trim($value, "\""));
				break;
				
			case 'frameworks':
				$options[$key] = explode(",", trim($value, "\""));
				break;
				
			default:
				//print("unknown switch $key\n");
				break;
		}
	}
}

// ??? TESTING
$testing = true;

if ($testing) {
	$GLOBALS["argv"][] = "-iphone";
	$GLOBALS["argv"][] = "-root=/Developer/ObjectivePascal";
	//$GLOBALS["argv"][] = "-delegates";
	//$GLOBALS["argv"][] = "-reference";
	//$GLOBALS["argv"][] = "-all";
	//$GLOBALS["argv"][] = "-noprint";
	//$GLOBALS["argv"][] = "-show";
	//$GLOBALS["argv"][] = "-only=\"UIWindow.h\"";
	$GLOBALS["argv"][] = "-frameworks=\"appkit,foundation\"";

	$GLOBALS["argv"][] = "-framework_path=\"/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS2.2.1.sdk/System/Library/Frameworks\"";
	$GLOBALS["argv"][] = "-header=\"uikit/UIView.h\"";
	
	//$GLOBALS["argv"][] = "-framework_path=\"/System/Library/Frameworks\"";
	//$GLOBALS["argv"][] = "-header=\"foundation/NSXMLNodeOptions.h\"";
	//$GLOBALS["argv"][] = "-header=\"appkit/NSBrowser.h\"";
	
	$GLOBALS["argv"][] = "-ignore=\"NSGeometry.h,NSRange.h\"";
	$GLOBALS["argv"][] = "-objp";

	// Objective-P
	/* Notes for master compile (-all):

	• CocoaAll.pas:
		1) NSWorkspace.h has a duplicate NSWorkspaceLaunchAllowingClassicStartup constant
		2) NSObjcRuntime.h contains a bad external function:
			function __attribute__(: (format(__NSString__; : 1; : 2))): void NSLog(NSStringRef *format, ...); cdecl; external name '__attribute__';
			procedure NSLog(fmt:NSString); cdecl; varargs; external;
		3) NSMenuItemCell.h has a duplicate (case sensitive name not allowed in Pascal) field that must be changed by hand.

		- Extra hand parsing. These units parse but contain unions which need to be fixed by hand.
	
		• NSEvent.h
		• NSIndexSet.h:
	
		_internal: record
		 case byte of
		   0: (_singleRange:
		         record
		           _range: NSRange;
		         end;
		      );
		   1: (_multipleRanges:
		         record
		           _data: pointer;
		           _reserved: pointer;
		         end;
		      );
		end;
		4) These methods have problems in the params. This is a Objc convention where an absent type is always "id"
			- (void)performClick:sender;
			- (void)setDelegate:delegate;
			NSControl.inc(136,15) Error: Mismatch between number of declared parameters and number of colons in message string.
			NSRuleEditor.inc(124,15) Error: Mismatch between number of declared parameters and number of colons in message string.
		5) NSInteger types are wrong in NSObjcRuntime (should be long)
		  NSInteger = clong;
		  NSUInteger = culong;
		  NSNotFound = high(NSInteger);
	    6) Many description and classDescription identifiers are not protected in name space and cause errors

	• iPhoneAll.pas
		1) UIAccelerometer: FPC bug causes methods with a single character message to fail. Remove the 3 methods affected
			UIAccelerometer.inc(67,49) Error: Illegal expression after message directive
		2) There's no way to know that UITextInputTraits is actually UITextInputTraitsProtocol due to name changes for Pascal syntax
			UITextField.inc(91,32) Error: Identifier not found "UITextInputTraits"
		
	- General notes:
	1) NSObject.h is parsed for the merged category methods that should be inserted manually into the real root class
	2) NSRange.h was ignored because it contains custom code and can be maintained by hand very easily
	3) NSGeometry.h was ignored because it contains many parsing errors and custom code, do this by hand for now.
	4) All instance variables are placed into "private" for now. There are a very small number of classes that have public ivar's.
	*/
	
	//$GLOBALS["argv"][] = "-show";
}

if (count($GLOBALS["argv"]) == 1) {
	print("Cocoa Framework Parser ($version) usage:\n");
	print("php parser.php [switches]\n\n");
	print("switches:\n\n");
	print("  -all           print all headers (.h) from AppKit/Foundation frameworks\n");
	print("  -header=\"foundation/NSObject.h\"    prints a single header from system frameworks\n");
	print("  -root          sets the root path of the pascocoa directory\n");
	print("  -framework_path	sets the root path of the frameworks directory (defaults to /System/Library/Frameworks)\n");
	print("  -show     	    prints output to screen instead of file\n");
	print("  -ignore=\"NSObject.h,NSArray.h\"     ignores the list of headers during parsing (-all only, no spaces)\n");
	print("  -only=\"NSObject.h,NSArray.h\"       only prints these files (-all only, no spaces)\n");
	print("  -noprint       parses but does not print (-all only)\n");
	print("  -encodings     prints Pascal type encoding glue for GenerateTypeEncodings.p (-all only)\n");
	print("  -delegates     prints NSDelegateController.inc to foundation (-all only)\n");
	print("  -objp     		prints classes in FPC Objective-P dialect\n");
	print("  -iphone     	one-time parse for iPhone headers\n");
	print("  -cocoa     	one-time parse for Cocoa (AppKit/Foundation) headers\n");
	print("  -frameworks=\"appkit,foundation\"    list of supported frameworks to parse\n");
	print("\n\n");
}

// get the command line options
if (count($GLOBALS["argv"]) > 1) {
	HandleCommandLineOptions($GLOBALS["argv"]);
	//print_r($options);
}

// Make the output directory
if ($options["out"]) {
	@mkdir($root_path, 0777);
	@mkdir($root_path."/foundation", 0777);
	@mkdir($root_path."/appkit", 0777);
//	@mkdir($root_path."/webkit", 0777);
	@mkdir($root_path."/uikit", 0777);
//	@mkdir($root_path."/reference", 0777);
}

// setup -iphone options
if ($options["iphone"]) {
	if (!$root_path) $root_path .= "/units/i386-darwin/cocoaint/src";
	$options["all"] = true;
	$options["objp"] = true;
	$options["frameworks"] = array("uikit");
}

// setup -cocoa options
if ($options["cocoa"]) {
	$options["all"] = true;
	$options["objp"] = true;
	$options["frameworks"] = array("appkit","foundation");
	$ignore_headers = array("NSGeometry.h","NSRange.h");
}

// create the parser instance
if ($options["objp"]) {
	$parser = new TObjPParser ($root_path, "", $options["frameworks"], $options["show"]);
} else {
	$parser = new TPasCocoaParser ($root_path, "", $options["frameworks"], $options["show"]);
}

// Process single headers
if ($options["header"] && !$options["all"]) {
	$path = $options["framework_path"]."/".$options["header"]["framework"].".framework/Headers/".$options["header"]["name"];
	print("* Processing $path...\n");
	$parser->ProcessFile($path, true);
}

//$parser->PrintIvarSizeComparison("/Users/ryanjoseph/Desktop/objp/IvarSize.p");
//exit;

// Process all headers
if ($options["all"]) {
	$parser->ParseCocoaFrameworks($ignore_headers, null);
	if (!$options["noprint"]) $parser->PrintAllHeaders("", $duplicate_headers, $only_files, $options["reference"]);
	if ($options["delegates"]) $parser->ParseDelegateClasses();
	if ($options["encodings"]) $parser->PrintTypeEncodingGlue();
}

?>