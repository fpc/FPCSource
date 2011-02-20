<?php

/**
 * ObjectivePParserBase contains all instance variables and settings for the parser
 * separated from the main file for easier navigation.
 */
	
class ObjectivePParserBase {

	// Frameworks to parse (loaded from frameworks.xml)
	var $frameworks = array();
	
	var $maximum_method_length = 111; 	// WE GET THIS ERROR: NSDelegatesAll.pas(6109,296) Error: Identifier not found "NSDelegateController_NSLayoutManagerDelegate_layoutManager_shouldUseTemporaryAttributes_forDrawingToScreen_atCharacterIndex_effectiveRange"
	
	var $output;									// current output file handle                        
	var $root;										// root for printing/locating resources              
	var $out;										// directory to print                                
	var $show;										// print output to screen instead of file            
	var $framework;									// current framework being parsed                    
	var $current_class;								// reference to current class structure being parsed 
	var $current_header;							// reference to current header structure being parsed
	var $method_count;								// count of all methods parsed                       
	var $class_count;								// count of all classes parsed                       
	var $warning_count;								// count of parser warnings                          
	var $parser_skipping;							// the parser is skipping lines in the current file  
	var $inside_macro_block;						// the parser is inside a macro block, no nesting!
	var $instance_var_scope;						// current scope of the instance variable parser
	var $dump = array();							// Convert Pascal classes
	var $delegate_methods = array();				// Delegate methods and types from GEN_BRIDGE_METADATA XML data
	var $delegate_method_names = array();			// Delegate method name array
	var $type_encodings = array();					// Master listing of type encodings for each method in the frameworks
	var $docset;										// Current parsed docset for the framework (-all only)

	// Comments Builder
	var $comment_eol;
	var $comment_terminated;
	var $comment_fragment;
	var $comment_fragment_previous;
	var $comment_fragment_open;
	var $comment_previous;
	var $comment_block_open;
	var $comment_block_closed;
	var $comment_header;
	
	// Macros Builder
	var $macro_block = "";
	var $in_macro_block = false;

	/**
	 * PARSER OPTIONS
	 */
	var $objc_id = "id";									// Default type for generic objects
	var $sel_string = "SEL";								// The selector string type which registers selectors internally
	var $protocol_suffix = "Protocol";						// All protocols append this suffix 
	var $pointer_type_suffx = "Ref";						// NS*** pointers in parameter lists are suffixed with this
	var $class_pointer_suffix = "Pointer";					// Pointers to NS*** classes are suffxed with this
	var $register_selectors = true;							// Register selectors automatically inside the wrappers
	var $show_added_messages = false;						// show messages when methods are added to a class
	var $show_class_hierarchy = false;
	var $objects_are_wrappers = false;						// Treat all objects (id) like wrappers. i.e aObject.Handle;
	var $replace_hinted_params = false;						// replace comment hints with hinted type - (void * /* CMProfileRef */)colorSyncProfile;
	var $record_keyword = "record";							// The keyword used for printing records
	var $bitpacked_record_keyword = "bitpacked record";		// The keyword used for printing bitpacked records
	var $string_macro = "NSString";
	var $category_suffix = "Category";								// To prevent naming conlicts all objccategories are suffixed with this work
	var $parse_comments = false;							// Comments are parsed and inserted by context
	var $merge_headers = false;								// Headers are printed by merging difference instead of overwritting
	var $comment_break = "\n";								// This text will be inserted before each comment to break lines
	var $comment_padding_left = " ";						// Padding applied to the left side of all comments
	var $comment_padding_right = " ";						// Padding applied to the right side of all comments
	var $varargs_param_name = "firstKey";					// The name of the first parameter for methods using varargs
	var $parse_docsets = false;									// Parses docsets for symbol documentation
	
	// array of all known framework classes (both regular and anonymous)
	var $cocoa_classes = array("Protocol");
	
	// array of all anonymous external classes. This list will include
	// classes that are first defined anonymously and then normally
	var $anon_cocoa_classes = array();
	
	// array of all defined external classes. This list will only
	// include classes that are completely defined
	var $defined_cocoa_classes = array();

	// array of function pointer types that are declared without an indirection
	// (e.g. NSUncaughtExceptionHandler in NSException.h) -> do not add Ptr
	// suffix when deriving the pointer type
	var $implicit_function_pointer_types = array();
	
	// array of opaque struct pointer types
	var $opaque_structs = array();
	
	// array of all known framework categories					                     
	var $cocoa_categories = array(); 						                 

	// Pascal keywords to protect
	var $reserved_keywords = array( "const", "object", "string", "array", "var", "set", "interface", "unit", "begin", "end",
									"type", "raise", "property", "to", "for", "with", "function", "procedure", "result",
									"pointer", "create", "new", "dispose", "label", "packed", "record", "class", "implementation",
									);

									
	// FPC methods that can't be overloaded
	var $reserved_methods = array("");
	
	// Types which can not be altered by reserved keywords
	var $reserved_types = array("Pointer");
	
	// Objective-c types to convert
	var $replace_types = array("BOOL"=>"Boolean", "long"=>"clong", "int"=>"cint",
								"unsigned long"=>"culong", "unsigned short"=>"cushort", "void *"=>"Pointer", "unsigned int"=>"cuint",
								"Class"=>"Pobjc_class", "uint"=>"cuint",
								"uint8_t"=>"cuint8", "signed int"=>"cint", "const char"=>"char", "const void *"=>"Pointer",
								"const uint8_t"=>"cuint8", "unsigned"=>"cuint", "int32_t"=>"cint32", "float"=>"single",
								"unsigned long long"=>"culonglong", "int64_t"=>"cint64", "uint32_t"=>"cuint32", "uint16_t"=>"cuint16",
								"unsigned char"=>"char", "short"=>"cshort", "double"=>"double", "long long"=>"clonglong",
								"uintptr_t"=>"culong","intptr_t"=>"clong",
								"signed char"=>"char", "uint64_t"=>"cuint64", 
								
								// work-arounds - the type replacement needs regex to handle with spaces I guess
								"void*"=>"Pointer",
								
								// macros
								"IBAction"=>"void", "IBOutlet"=>"",
								
								// special pointers
								"const id *"=>"NSObjectArrayOfObjectsPtr", "Protocol *"=>"objc_protocol", "NSObject *"=>"NSObject",
								"const char *"=>"PChar", "const void *"=>"Pointer", "unsigned char *"=>"PByte", "char *"=>"PChar",
								"unsigned *"=>"pcuint", "unichar *"=>"PChar", "const unichar *"=>"PChar", 
								);
		
	// These "types" are hints to the Objective-C garbage collector
	var $garbage_collector_hints = array("__strong", "__weak", "volatile", "___strong", "___weak");

	// These identifiers are used with remote messaging
	var $remote_messaging_modifiers = array("oneway", "in", "out", "inout", "bycopy", "byref");

	var $null_macros = array("IBOutlet", "IBAction");
	
	// External NSString macros. Additional values are imported from frameworks.xml
	var $external_string_macros = "APPKIT_EXTERN|FOUNDATION_EXPORT|EXTERN|extern";

	// Types which have known pointers declared in the headers
	var $pointer_types = array(	// MacOSAll types
								// C/Cocoa types
								"void"=>"Pointer","const void"=>"Pointer",
								"Boolean"=>"pboolean",
								"clong"=>"pclong","cint"=>"pcint",
								"culong"=>"pculong","cushort"=>"pcushort","cuint"=>"pcuint",
								"cuint8"=>"pbyte","char"=>"PChar",
								"clonglong"=>"pclonglong","culonglong"=>"pculonglong",
								"cint64"=>"pcint64",
								"cuint32"=>"pcuint32","cuint16"=>"pcuint16",
								"cshort"=>"pcshort",

								"single"=>"psingle", "double"=>"pdouble",
								);
	
	// These methods require that the last parameter append a trailing underscore (if $trailing_underscore is on)
	// Method format should be Objective-C copied directly from the headers
	var $trailing_underscore_methods = array(	"- (void)copy:(id)sender;", 
																						"- (void)setNeedsDisplay:(BOOL)flag;",
																						"- (void*)QTMovie;","- (QTMovie *)QTMovie;",
																						"- (BOOL)load:(NSError **)error;",
																						);
	var $trailing_underscore = true;

	// Types to ignore
	var $ignore_types = array("CGFloat");
	
	// Comments (or fragments starting with the pattern) to ignore (regex)
	var $ignore_comments = array();
	
	// Categories to ignore
	// NSURLLoading is deprecated in 10.4 and later, and causes problems
	// when parsing the iPhoneOS foundation
	var $ignore_categories = array("NSURLLoading");
	
	// Methods to ignore
	var $ignore_methods = array();

 	// methods to rename to particular alternatives
	var $replace_instance_methods = array ( "class" => "_class", );
	
	var $replace_class_methods = array ("respondsToSelector" => "classRespondsToSelector",
												"isEqual" => "classIsEqual",
												"hash" => "classHash",
												"superClass" => "classSuperClass",
												"class" => "classClass",
												"conformsToProtocol" => "classConformsToProtocol",
												"classDescription" => "_classDescription", );
	
	// Lines to ignore
	var $ignore_lines = array();
	
	// Default protected keywords by class/category
	// These may be useful if super classes were not parsed before
	var $default_protected = array(	"*"=>array("description", "classDescription", "zone"),
	//								"NSDeprecated"=>array("accessoryView"),
									"NSToolbarSupport"=>array("toolbar"),
									"DOMNode"=>array("version"),
									"WebView"=>array("frame"),
									"DOMImplementation"=>array("version"),
									"NSTableView"=>array("cell","toolTip","menu"),
									"NSMovie"=>array("QTMovie"),
									"UIAcceleration"=>array("timestamp","x","y","z"),
									);
	
	var $skip_blocks = array(	//"^#if __LP64__.*"=>"^#(else|endif)+",
													"^#if __BLOCKS__"=>"^#(else|endif)+",
													"^#if NS_BLOCKS_AVAILABLE"=>"^#(else|endif)+",
													"^#ifndef CGFLOAT_DEFINED"=>"^#(else|endif)+",
													);
								
	var $macro_blocks = array(	"^#if \(__LP64__\)"=>"\$ifdef cpu64",
								"^#if __LP64__.*"=>"\$ifdef cpu64",
								"^#if !__LP64__.*"=>"\$ifndef cpu64",
								"^#if defined \(__LP64__\)"=>"\$ifdef cpu64",
								"^#if ![[:space:]]*\(__LP64__\)"=>"\$ifndef cpu64",
								"^#ifdef __BIG_ENDIAN__"=>"\$ifdef fpc_big_endian",
								// not really clean (assumes all arm devices are embedded)
								"^#if __LP64__ \|\| TARGET_OS_EMBEDDED \|\| TARGET_OS_IPHONE \|\| TARGET_OS_WIN32 \|\| NS_BUILD_32_LIKE_64"=>"\$if defined(cpu64) or defined(cpuarm) or defined(win32)",
								// replacement must be updated once we support AppleTV (presumably defines TARGET_OS_EMBEDDED but not TARGET_OS_IPHONE)
								"^#if __LP64__ \|\| (TARGET_OS_EMBEDDED && !TARGET_OS_IPHONE) \|\| TARGET_OS_WIN32 \|\| NS_BUILD_32_LIKE_64"=>"\$if defined(cpu64) or defined(win32)"
								//"^#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_[0-9]+_[0-9]+"=>"*",
								);
	
		// These macros are used to suggest which version of a framework the symbol is available in	but removed to assist the parser			
		var $version_macros = array(	"[[:space:]]*DEPRECATED_IN_[^(]*_VERSION_[0-9]+_[0-9]+_AND_LATER[[:space:]]*",
																	"[[:space:]]*AVAILABLE_[^(]*_VERSION_[0-9]+_[0-9]+_AND_LATER_BUT_DEPRECATED_IN_[^(]*_VERSION_[0-9]+_[0-9]+[[:space:]]*",
																	"[[:space:]]*AVAILABLE_[^(]*_VERSION_[0-9]+_[0-9]+_AND_LATER_BUT_DEPRECATED[[:space:]]*",
																	"[[:space:]]*AVAILABLE_[^(]*_VERSION_[0-9]+_[0-9]+_AND_LATER[[:space:]]*",
																	"[[:space:]]*AVAILABLE_[^(]*_VERSION_[0-9]+_[0-9]+[[:space:]]*",
																	"[[:space:]]*.*_VERSION_MAX_ALLOWED[[:space:]]*",
																	"[[:space:]]__OSX_AVAILABLE[^(]*\([^;)]+\)[[:space:]]*",
																	"[[:space:]]*UIKIT_CLASS_AVAILABLE\([^;]*\)[[:space:]]*",
																	"[[:space:]]*NS_AVAILABLE[^(]*\([^;]*\)[[:space:]]*",
																	"[[:space:]]*NS_DEPRECATED[^(]*\([^;]*\)[[:space:]]*",
																	"[[:space:]]WEBKIT_OBJC_METHOD_ANNOTATION\([^;]*\)[[:space:]]*",
																	);
	
	// Subpats to search for docsets							
	var $docset_paths = array(	"com.apple.adc.documentation.AppleSnowLeopard.CoreReference.docset" => array("/Cocoa/Reference"),
															"com.apple.adc.documentation.AppleiOS4_2.iOSLibrary.docset" => array("/UIKit/Reference"),
															);
	
																		
	/**
	 * COMMON REGULAR EXPRESIONS
	 */
	var $pregex_objc_method_params = "!^(-|\+)\s*\(([^)]*)\)\s*(\w+):([^;]*)\s*[^:;]*;!";
	var $pregex_objc_method_no_params = "!^(-|\+)\s*\(([^)]*)\)\s*(\w+)\s*[^:;]*;!";
	var $pregex_objc_method_partial = "!^(-|\+)\s*\(([^)]*)\)\s*(\w+):([^;]*)!";
	var $pregex_objc_method_terminate = "!([^;]*);\s*$!";
	var $regex_objc_category = "^@interface ([a-zA-Z]+)[[:space:]]*\(([a-zA-Z]+)\)";
	var $regex_objc_class = "^@interface ([a-zA-Z]+)[[:space:]]*:[[:space:]]*([a-zA-Z0-9_]+)[[:space:]]*(<(.*)>)*";
	var $regex_objc_class_no_super = "^@interface ([a-zA-Z]+)[[:space:]]*<(.*)>[[:space:]]*";
	var $regex_objc_protocol = "^@protocol ([a-zA-Z0-9_]+)";
	var $regex_procedure_type = "^[[:space:]]*(void|IBAction)+[[:space:]]*$";
	var $regex_scope_compiler_directive = "^\s*@(private|protected|public)(;*)+\s*$";
	var $regex_objc_property_attributes = "^@property[[:space:]]*\(([^)]*)\)[[:space:]]*([a-zA-Z_0-9]+)[[:space:]]*(\*)*[[:space:]]*(.*);";
	var $regex_objc_property = "^@property[[:space:]]*([a-zA-Z_0-9]+)[[:space:]]*(\*)*[[:space:]]*(.*);";
	var $regex_objc_anon_class = "^@class[[:space:]]*(.*);";
	
	// $1 = return type
	// $2 = pointer modifiers
	// $3 = inline para name if any
	// $4 = parameteres
	var $pregex_function_pointer = "!^\s*([^*]+)\s*([*]*)\s*\(\s*[*]\s*(\w+)?\s*\)\s*\((.*)\)\s*;\s*$!";
	// same as above except no ";" at the end
	var $pregex_function_pointer_c_paratype = "!^\s*([^*]+)\s*([*]*)\s*\(\s*[*]\s*(\w+)?\s*\)\s*\((.*)\)\s*$!";
	// Obj-C: additional brackets and para name
	// $1 = return type
	// $2 = pointer modifiers for return type
	// $3 = inline type name if any
	// $4 = parameters
	// $5 = para name
	// last "word" is next part of method name before next colon
	var $pregex_function_pointer_objc_paratype = "!^\s*\(([^*]+)\s*([*]*)\s*\(\s*[*]\s*(\w+)?\s*\)\s*\((.*)\)\s*\)\s*(\w+)\s*\w+\s*$!";
	// the (*) for the function pointer is "optional" in the sense that you can
	// also declare a (useless?) function type (i.e., without the pointer) and
	// then afterwards declare variables to be pointers to this type
	var $pregex_function_pointer_typedef = "!^\s*typedef\s*([^*]+)\s*([*]*)\b\s*(?:\(\s*([*])\s*(\w+)?\s*\)|(\w+))\s*\((.*)\)\s*(\w+)?\s*;\s*$!";
	
	// semi-colon optional because sometimes on next line with availability
	// macro
	// $1 = return type
	// $2 = pointer modifiers for return type
	// $3 = function name
	// $4 = parameters
	var $pregex_external_function_end = "\s+([^*]+)\s*([*]*)\b\s*(\w+)\s*\((.*)\)\s*;?\s*$!";
	
	var $pregex_deprecated_macro = "!DEPRECATED!";

}

?>