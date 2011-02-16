<?php

$version = "2.1.6";

require("source/objp.php");

/**
 * Cocoa framework parser for Objective-Pascal
 * 
 * @author Ryan Joseph
 * @author Jonas Maebe
 **/

// These files have duplicates in AppKit and Foundation so we ignore the foundation versions and everything is merged into AppKit
# $duplicate_headers = array("foundation/NSAttributedString.inc", "foundation/NSAffineTransform.inc");
$duplicate_headers = array();

// Print only these files
$only_files = null;

$options = array();

function HandleCommandLineOptions ($argv) {
	global $options;
	global $root_path;
	global $ignore_headers;
	global $only_files;
	
	// Define defaults
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
				$options["all"] = false;	// headers override -all
				break;
				
			case 'framework_path':
				$options["framework_path"] = trim($value, "\"");
				break;
				
			case 'all':
				$options[$key] = true;
				break;

			case 'encodings':
				$options[$key] = true;
				break;

			case 'noprint':
				$options[$key] = true;
				break;
				
			case 'comments':
				$options[$key] = true;
				break;
			
			case 'docsets':
				$options[$key] = true;
				break;

			case 'merge':
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
				
			case 'webkit':
				$options[$key] = true;
				break;
			
			case 'sdk':
				$options[$key] = trim($value, "\"");
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

// Print the script usage if the command line is empty
if (count($GLOBALS["argv"]) == 1) {
	print("Cocoa Framework Parser ($version) usage:\n");
	print("php parser.php [options]\n\n");
	print("Options:\n\n");
	print("  -all           	Print all headers (.h) from AppKit/Foundation frameworks.\n");
	print("  -header=\"foundation/NSObject.h\"    Prints a single header from system frameworks (if specified) or direct path.\n");
	print("  -root          	Sets the root path of the output directory.\n");
	print("  -framework_path	Sets the root path of the frameworks directory (defaults to /System/Library/Frameworks).\n");
	print("  -show     	    	Prints output to screen instead of file (mutually exclusive to -noprint).\n");
	print("  -comments     		Parses comments.\n");
	print("  -merge     		Headers are merged by difference (using diff/patch) instead of overwritten.\n");
	print("  -ignore=\"NSObject.h,NSArray.h\"     Ignores the list of headers during parsing (-all only, no spaces).\n");
	print("  -only=\"NSObject.h,NSArray.h\"       Only print these files (-all only, no spaces).\n");
	print("  -noprint       	Parses but does not print.\n");
	print("  -encodings    	 	Prints Pascal type encoding glue for GenerateTypeEncodings.p (-all only).\n");
	print("  -iphone     		One-time parse for iPhone headers.\n");
	print("  -cocoa     		One-time parse for Cocoa (AppKit/Foundation) headers.\n");
	print("  -frameworks=\"appkit,foundation\"    List of supported frameworks to parse.\n");
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
	@mkdir($root_path."/uikit", 0777);
}

// setup -iphone options
if ($options["iphone"]) {
	$options["all"] = true;
	
	if (!$options["sdk"]) $options["sdk"] = "4.2";
	
	//$options["framework_path"] = "/Developer/Platforms/iPhoneOS.Platform/Developer/SDKs/iPhoneOS$sdk_version.sdk/System/Library/Frameworks";
	$options["framework_path"] = "/Developer/Platforms/iPhoneSimulator.Platform/Developer/SDKs/iPhoneSimulator".$options["sdk"].".sdk/System/Library/Frameworks";
	
	$options["frameworks"] = array("foundation","quartzcore","opengles","uikit");
}

// setup -cocoa options
if ($options["cocoa"]) {
	$options["all"] = true;
	$options["frameworks"] = array("appkit","foundation","quartzcore");
	$ignore_headers = array();
}

if ($options["webkit"]) {
	$options["all"] = true;
	$options["frameworks"] = array("foundation","webkit");
}

// create the parser instance
$parser = new ObjectivePParser($root_path, "", $options["frameworks"],  $options["framework_path"], $options["show"]);

// Set additional options
// ??? These should be accessors
$parser->parse_comments = $options["comments"];
$parser->merge_headers = $options["merge"];
$parser->parse_docsets = $options["docsets"];

// Process single headers
if ($options["header"] && !$options["all"]) {
	$path = $options["framework_path"]."/".$options["header"]["framework"].".framework/Headers/".$options["header"]["name"];
	print("* Processing $path...\n");
	$parser->ProcessFile($path, !$options["noprint"]);
}

// Process all headers
if ($options["all"]) {
	$parser->ParseAllFrameworks($ignore_headers, null);
	if (!$options["noprint"]) $parser->PrintAllHeaders("", $duplicate_headers, $only_files, $options["reference"]);
}

?>