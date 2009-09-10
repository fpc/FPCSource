<?php

class TObjPParser extends TPasCocoaParser {
	
	var $objc_id = "id";							// Default type for generic objects
	var $objc_id_real = "id";						// The real type of generic objects (id)
	var $sel_string = "SEL";						
	var $pointer_type_suffx = "";
	var $trailing_underscore = true;
	
	var $reserved_keywords = array(	"const", "object", "string", "array", "var", "set", "interface", "classname", "unit",
									"self", "type", "raise", "property", "to", "for", "with", "function", "procedure", "result",
									"pointer", "create", "new", "dispose", "label", "packed", "record", "char", "class",
									
									// identifiers from NSObject
									"zone", 
									);
	
	var $replace_types = array(	"void"=>"Pointer", "BOOL"=>"Boolean", "long"=>"clong", "int"=>"cint",
								"unsigned long"=>"culong", "unsigned short"=>"cushort", "void *"=>"Pointer", "unsigned int"=>"cuint",
								"NSUInteger"=>"culong", "NSInteger"=>"clong", "Class"=>"Pobjc_class", "uint"=>"cuint",
								"uint8_t"=>"byte", "signed int"=>"cint", "const char"=>"char", "const void"=>"Pointer",
								"const uint8_t"=>"byte", "unsigned"=>"cuint", "int32_t"=>"longint", "float"=>"single",
								"unsigned long long"=>"culonglong", "int64_t"=>"clonglong", "uint32_t"=>"cardinal", "uint16_t"=>"word",
								"unsigned char"=>"char", "short"=>"cshort", "double"=>"double", "long long"=>"clonglong",
								
								// macros
								"IBAction"=>"void", 
								
								// special pointers
								"const id *"=>"NSObjectArrayOfObjectsPtr", "Protocol *"=>"objc_protocol", "NSObject *"=>"NSObject",
								"const char *"=>"PChar", "const void *"=>"Pointer", "unsigned char *"=>"Pointer", "char *"=>"Pointer",
								"unsigned *"=>"Pointer", 
								);
	
	// These methods require that the last parameter append a trailing underscore (if $trailing_underscore is on)
	var $trailing_underscore_methods = array("- (void)copy:(id)sender;", "- (void)setNeedsDisplay:(BOOL)flag;");

	// We use direct Cocoa classes now always
	var $toll_free_bridge = array();
	
	var $ignore_methods = array("observationInfo");	

	// Converts an Objective-c method name to Pascal
	function ConvertObjcMethodName ($method) {
		$params = explode(":", $method);
		$name = "";
		
		if (count($params) > 1) {
			foreach ($params as $value) {
				if (eregi("([a-zA-Z0-9]+)$", $value, $captures)) $name .= $captures[1]."_";
			}
		} else {
			if (eregi("([a-zA-Z0-9]+)(;)*$", $params[0], $captures)) $name .= $captures[1]."_";
		}
		
		// clean it up
		if ($this->trailing_underscore) {
		 if (!in_array($method, $this->trailing_underscore_methods)) $name = trim($name, "_");	
		}
		
		$name = $this->ReplaceObjcType($name);
		
		return $name;
	}	

	// We use direct objc classes now so we don't need to replace them with references like in PasCocoa
	function ReplaceNSTypesWithRef ($string) {
		return $string;
	}

	// Converts an Objective-C method to Pascal format 
	function ConvertObjcMethodToPascal ($class, $source, $parts, $protected_keywords, $has_params) {

	// replace "hinted" params comment with hinted type
	if ($this->replace_hinted_params) {

		// param string
		if (eregi("(/\*[[:space:]]*(.*)[[:space:]]*\*/)", $parts[4], $captures)) {
			// ??? change the parameter to the hinted type
			//$parts[4] = eregi_replace("(/\*.*\*/)", $captures[2], $parts[4]);
			//$parts[4] = trim($parts[4], " ");
		}

		// return type
		if (eregi("(/\*[[:space:]]*(.*)[[:space:]]*\*/)", $parts[2], $captures)) $parts[2] = $captures[2];

		//print_r($parts);

	} else { // remmove comments from params and return type
		$parts[4] = eregi_replace("(/\*.*\*/)", "", $parts[4]);
		$parts[4] = trim($parts[4], " ");

		$parts[2] = eregi_replace("(/\*.*\*/)", "", $parts[2]);
		$parts[2] = trim($parts[2], " ");
	}

	$return_type_clean = $parts[2];

	// perform preformatting before attempting to protect keywords
	$parts[2] = $this->FormatObjcType($parts[2], $modifiers);
	$parts[4] = $this->FormatObjcParams($parts[4]);

	// protect keywords in the parameter and return type
	if (count($protected_keywords) > 0) {
		foreach ($protected_keywords as $keyword) {
			$parts[4] = istr_replace_word($keyword, $keyword."_", $parts[4]);
			$parts[2] = istr_replace_word($keyword, $keyword."_", $parts[2]);
		}
	}

	if ($has_params) {
		$name = $this->ConvertObjcMethodName($source);

		// merge default protected keywords for the class/category
		if ($this->default_protected[$class]) $protected_keywords = array_merge($this->default_protected[$class], $protected_keywords);

		$param_array = $this->ConvertObjcParamsToPascal($parts[4], $protected_keywords);
		$params = "(".$param_array["string"].")";
		$params_with_modifiers = "(".$param_array["string_with_modifiers"].")";
	} else {
		$params = "";
		$params_with_modifiers = "";
		$name = $parts[3];
		$param_array = null;
	}

	//print("$params_with_modifiers\n");

	// protect method name from keywords
	if ($this->IsKeywordReserved($name)) $name .= "_";

	// clean return type
	$return_type = trim($parts[2], "* ");
	$return_type = $this->ReplaceObjcType($return_type);
	$return_type = $this->ReplaceTollFreeBridgeType($return_type);

	$virtual = "";
	$class_prefix = "";

	// determine the type based on return value
	if (ereg($this->regex_procedure_type, $return_type_clean)) {
		$kind = "procedure";
	} else {
		$kind = "function";
	}

	// determine if this is a class method
	if ($parts[1] == "+") {
		$class_prefix = "class ";
		
		// These methods probably return the an allocated instance of the class, a typical convenience method.
		// ??? Ack! $class may be the category or protocol name
		//if ($return_type == $this->objc_id) $return_type = $class; 
	}

	// Determine if the method needs a particular modifier
	// ??? THIS IS NOT COMPILING???
	//if (ereg($this->objc_object_array, $params)) $modifier = " cdecl;";

	// Replace SEL with the string equivalent
	if ($this->register_selectors) {
		$params_with_modifiers = str_replace_word("SEL", $this->sel_string, $params_with_modifiers);
	}

	// make method templates
	if ($kind != "function") {
		$method = "$class_prefix$kind $name$params_with_modifiers;$modifier$virtual";
		$method_template = "[KIND] [PREFIX]$name"."[PARAMS];$modifier";
	} else {
		$method = $class_prefix."function $name$params_with_modifiers: $return_type;$modifier$virtual";
		$method_template = "[KIND] [PREFIX]$name"."[PARAMS]: [RETURN];$modifier";
		$method_template_function = "function [PREFIX]$name"."[PARAMS]: [RETURN];$modifier";
	}

	$method_template_procedure = "procedure [PREFIX]$name"."[PARAMS];$modifier";
	$method_template_function = "function [PREFIX]$name"."[PARAMS]: [RETURN];$modifier";

	// ??? DEBUGGING
	//print("$method\n");

	// build structure
	$struct["def"] = $method;
	$struct["template"] = $method_template;
	$struct["template_function"] = $method_template_function;
	$struct["template_procedure"] = $method_template_procedure;
	$struct["objc_method"] = $this->CopyObjcMethodName($source);
	$struct["class_prefix"] = $class_prefix;
	//$struct["def_objc"] = eregi("(.*);", $source, $captures[1]);
	if ($return_type == "void") $return_type = "";
	$struct["return"] = $return_type;
	if (in_array($return_type, $this->cocoa_classes)) $struct["returns_wrapper"] = true;
	$struct["param_string_clean"] = trim($params, "()");
	$struct["param_string_clean_with_modifiers"] = trim($params_with_modifiers, "()");
	$struct["param_string"] = $params;
	$struct["param_string_with_modifiers"] = $params_with_modifiers;
	$struct["param_array"] = $param_array["pairs"];
	$struct["param_list"] = $param_array["list"];
	$struct["class"] = $class;
	$struct["name"] = $name;
	$struct["kind"] = $kind;

	if ($struct["param_array"] != null) $struct["has_params"] = true;

	// FPC bug work around
	if (strlen($name) > $this->maximum_method_length) {
		$struct["can_override"] = false;
		print("	# WARNING: method $name can't override because the name is too long\n");
		$this->warning_count ++;
	}

	return $struct;
	}
								
	// Prints all classes from the header in Objective-P FPC format
	function PrintHeader ($header) {
		global $version;

		$this->output = fopen($header["path"], "w+");

		$this->PrintOutput(0, "{ Parsed from ".ucfirst($header["framework"]).".framework ".$header["name"]." }");

		$date = date("D M j G:i:s T Y");
		$this->PrintOutput(0, "{ Version $version - $date }");
		$this->PrintOutput(0, "");

		$macro = strtoupper(substr($header["name"], 0, (strripos($header["name"], "."))));
		
		if ($header["classes"]) {
			$this->PrintOutput(0, "{\$ifdef HEADER}");
			$this->PrintOutput(0, "{\$ifndef $macro"."_PAS_H}");
			$this->PrintOutput(0, "{\$define $macro"."_PAS_H}");
			$this->PrintOutput(0, "type");
			
			foreach ($header["classes"] as $class) {
				
				// Make a pointer to each class
				$this->PrintOutput(1, $class["name"]."Pointer = Pointer;");
			}
		
			$this->PrintOutput(0, "");
			$this->PrintOutput(0, "{\$endif}");
			$this->PrintOutput(0, "{\$endif}");
		}
		
		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{\$ifdef TYPES}");
		$this->PrintOutput(0, "{\$ifndef $macro"."_PAS_T}");
		$this->PrintOutput(0, "{\$define $macro"."_PAS_T}");
		$this->PrintTypes($header);
		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{\$endif}");
		$this->PrintOutput(0, "{\$endif}");

		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{\$ifdef RECORDS}");
		$this->PrintOutput(0, "{\$ifndef $macro"."_PAS_R}");
		$this->PrintOutput(0, "{\$define $macro"."_PAS_R}");
		$this->PrintRecords($header);
		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{\$endif}");
		$this->PrintOutput(0, "{\$endif}");

		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{\$ifdef FUNCTIONS}");
		$this->PrintOutput(0, "{\$ifndef $macro"."_PAS_F}");
		$this->PrintOutput(0, "{\$define $macro"."_PAS_F}");
		$this->PrintFunctions($header);
		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{\$endif}");
		$this->PrintOutput(0, "{\$endif}");

		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{\$ifdef EXTERNAL_SYMBOLS}");
		$this->PrintOutput(0, "{\$ifndef $macro"."_PAS_T}");
		$this->PrintOutput(0, "{\$define $macro"."_PAS_T}");
		$this->PrintExternalSymbols($header);
		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{\$endif}");
		$this->PrintOutput(0, "{\$endif}");

		if (($header["classes"]) || ($header["protocols"])) {
			$this->PrintOutput(0, "");
			$this->PrintOutput(0, "{\$ifdef FORWARD}");

			if ($header["protocols"]) {
				foreach ($header["protocols"] as $protocol) $this->PrintOutput(1, $protocol["name"]."$this->protocol_suffix = objcprotocol;");
			}
			
			if ($header["classes"]) {
				foreach ($header["classes"] as $class) $this->PrintOutput(1, $class["name"]." = objcclass;");
			}
			
			$this->PrintOutput(0, "");
			$this->PrintOutput(0, "{\$endif}");
		}

		if ($header["classes"]) {
			$this->PrintOutput(0, "");
			$this->PrintOutput(0, "{\$ifdef CLASSES}");
			$this->PrintOutput(0, "{\$ifndef $macro"."_PAS_C}");
			$this->PrintOutput(0, "{\$define $macro"."_PAS_C}");

			foreach ($header["classes"] as $class) {
				if (in_array($class["name"], $this->cocoa_classes)) $this->PrintClass($class);
			}

			$this->PrintOutput(0, "");
			$this->PrintOutput(0, "{\$endif}");
			$this->PrintOutput(0, "{\$endif}");
		}


		if ($header["protocols"]) {
			$this->PrintOutput(0, "{\$ifdef PROTOCOLS}");
			$this->PrintOutput(0, "{\$ifndef $macro"."_PAS_P}");
			$this->PrintOutput(0, "{\$define $macro"."_PAS_P}");

			
			foreach ($header["protocols"] as $protocol) {
				$this->PrintOutput(1, "");
				$this->PrintOutput(0, "{ ".$protocol["name"]." Protocol }");
				$this->PrintOutput(1, $protocol["name"]."$this->protocol_suffix = objcprotocol");

				// print methods
				if ($protocol["methods"]) {
					foreach ($protocol["methods"] as $name => $method) {
						$this->PrintOutput(2, $method["def"]." message '".$method["objc_method"]."';");
					}
				}
				
				$this->PrintOutput(1, "end; external name '".$protocol["name"]."';");		
			}
			
			$this->PrintOutput(0, "{\$endif}");
			$this->PrintOutput(0, "{\$endif}");
		}
	}
	
	
	function PrintClass ($class) {

		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{ ".$class["name"]." }");
		
		// print super class or protocol which the class conforms to
		if ($class["conforms"]) {
			$this->PrintOutput(1, $class["name"]." = objcclass(".$class["super"].", ".$class["conforms"].")");
		} elseif ($class["super"]) {
			$this->PrintOutput(1, $class["name"]." = objcclass(".$class["super"].")");
		}

		// print alloc method for the class
		$this->PrintOutput(2, "class function alloc: ".$class["name"]."; message 'alloc';");

		// print class-level methods
		if ($class["methods"]) {
			$this->PrintOutput(0, "");
			foreach ($class["methods"] as $method) {
				$this->PrintOutput(2, $method["def"]." message '".$method["objc_method"]."';");
			}
		}

		// print category-level methods
		if (count($class["categories"]) > 0) {
			foreach ($class["categories"] as $name => $category) {
				$this->PrintOutput(0, "");
				$this->PrintOutput(2, "{ Category: $name }");

				if ($category["methods"]) {
					foreach ($category["methods"] as $method) {
						$this->PrintOutput(2, $method["def"]." message '".$method["objc_method"]."';");
					}
				}	
			}
		}

		$this->PrintOutput(1, "end; external;");
	}

}
?>