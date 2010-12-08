<?php

class TObjPParser extends TPasCocoaParser {
	
	var $trailing_underscore = true;
	
	// ignore these classes when testing for ivar size
	var $ignore_class_ivar_comparison = array(	"NSNibOutletConnector", "NSNibConnector", "NSNibControlConnector", "NSPredicateEditorRowTemplate", "NSSegmentedCell",
												"NSSimpleHorizontalTypesetter", "NSInvocation", "NSPointerFunctions", "NSConstantString");
	
	// These methods require that the last parameter append a trailing underscore (if $trailing_underscore is on)
	var $trailing_underscore_methods = array("- (void)copy:(id)sender;", "- (void)setNeedsDisplay:(BOOL)flag;","- (void*)QTMovie;","- (QTMovie *)QTMovie;","- (BOOL)load:(NSError **)error;");
	
	var $ignore_methods = array("observationInfo"); 

	// Converts an Objective-c method name to Pascal
	function ConvertObjcMethodName ($method) {
		$params = explode(":", $method);
		$name = "";
		$count = 0;
		
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

	// Converts an Objective-C method to Pascal format 
	function ConvertObjcMethodToPascal ($class, $source, $parts, $protected_keywords, $has_params, $deprecatedmods) {
		
		//print("$source\n");
		//print_r($parts);
		
		// replace "hinted" params comment with hinted type
		if ($this->replace_hinted_params) {

			// param string
			if (eregi("(/\*[[:space:]]*(.*)[[:space:]]*\*/)", $parts[4], $captures)) {
				// ??? change the parameter to the hinted type
				//$parts[4] = eregi_replace("(/\*.*\*/)", $captures[2], $parts[4]);
				//$parts[4] = trim($parts[4], " ");
			}

			// return type
			if (eregi("(/\*[[:space:]]*(.*)[[:space:]]*\*/)", $parts[2], $captures)) $parts[2] = $this->ReplaceRemoteMessagingModifiers($captures[2], $null);

			//print_r($parts);

		} else { // remove comments from params and return type
			$parts[4] = eregi_replace("(/\*.*\*/)", "", $parts[4]);
			$parts[4] = trim($parts[4]);

			$parts[2] = eregi_replace("(/\*.*\*/)", "", $parts[2]);
			$parts[2] = $this->ReplaceRemoteMessagingModifiers($parts[2], $null);
		}
	
		$return_type_clean = $parts[2];
		$return_type_pointers = preg_replace("![^*]+!e", "", $return_type_clean);
		$return_type_clean = trim($return_type_clean,"* 	");

		// perform preformatting before attempting to protect keywords
		$parts[2] = $this->FormatObjcType($parts[2], $modifiers);
		$parts[4] = $this->FormatObjcParams($parts[4], $variable_arguments);
		//print($parts[4]."\n");
		
		if ($has_params) {
			$name = $this->ConvertObjcMethodName($source);

			// merge default protected keywords for the class/category
			if ($this->default_protected["*"]) $protected_keywords = array_merge($this->default_protected["*"], $protected_keywords);
			if ($this->default_protected[$class]) $protected_keywords = array_merge($this->default_protected[$class], $protected_keywords);
			
			
			$param_array = $this->ConvertObjcParamsToPascal($parts[4], $protected_keywords);
			$params = "(".$param_array["string"].")";
			$params_with_modifiers = "(".$param_array["string_with_modifiers"].")";
			
		} else {
			$params = "";
			$params_with_modifiers = "";
			// no parameters -> definitely no underscore normally, but there are some
			// conflicts...
			$name = $parts[3];
			// clean it up
			if ($this->trailing_underscore) {
				if (in_array($source, $this->trailing_underscore_methods)) $name = $name . "_";	
			}
			$param_array = null;
			$variable_arguments = false;
		}

		// protect method name from keywords
		if ($this->IsKeywordReserved($name)) $name .= "_";
		
		// replace objc type
		$return_type = $this->ConvertReturnType($return_type_clean,$return_type_pointers);

		$virtual = "";
		$class_prefix = "";

		// determine the type based on return value
		if (ereg($this->regex_procedure_type, $return_type_clean.$return_type_pointers)) {
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

		// Replace SEL with the string equivalent
		if ($this->register_selectors) {
			$params_with_modifiers = str_replace_word("SEL", $this->sel_string, $params_with_modifiers);
		}

		// make method templates
		if ($kind != "function") {
			if ($variable_arguments) $modifier .= " varargs;";
		
			$method = "$class_prefix$kind $name$params_with_modifiers;$modifier$virtual";
			$method_template = "[KIND] [PREFIX]$name"."[PARAMS];$modifier";
		} else {
			if ($variable_arguments) $return_type = "$return_type; varargs";

			$method = $class_prefix."function $name$params_with_modifiers: $return_type;$modifier$virtual";
			$method_template = "[KIND] [PREFIX]$name"."[PARAMS]: [RETURN];$modifier";
		}

		$method_template_procedure = "procedure [PREFIX]$name"."[PARAMS];$modifier";
		$method_template_function = "function [PREFIX]$name"."[PARAMS]: [RETURN];$modifier";

		// build structure
		$struct["def"] = $method;
		$struct["template"] = $method_template;
		$struct["template_function"] = $method_template_function;
		$struct["template_procedure"] = $method_template_procedure;
		$struct["objc_method"] = $this->CopyObjcMethodName($source);
		$struct["class_prefix"] = $class_prefix;
		if ($deprecatedmods != "") $struct["deprecated"] = $deprecatedmods.";";
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
			print(" # WARNING: method $name can't override because the name is too long\n");
			$this->warning_count ++;
		}

		return $struct;
	}
	
	function InsertPatches ($header) {
		$path = "$this->root/patches/".$header["name_clean"].".patch";
		if ($handle = @fopen($path, "r")) {
			$text = ReadTextFile($path);
			$this->PrintOutput(0, $text);
			fclose($handle);
		}
	}
	
	function HeaderContainsPatch ($header) {
		if ($handle = @fopen("$this->root/patches/".$header["name_clean"].".patch", "r")) {
			fclose($handle);
			return true;
		}
	}

  function PrintGlobalClassInfo($all_classes, $defined_classes, $anon_classes) {
		// add all classes as anonymous external classes to a separate unit.
		// They will be overridden by the actual definitions in the translated
		// headers part of the main unit, but this way they can appear as record
		// field types and as callback parameters

		// open the output file if we not printing to terminal
		if (!$this->show) {
			$this->output = fopen("$this->root$this->out/AnonClassDefinitions".ucfirst($this->framework).".pas", "w+");
		}
		
		$this->PrintOutput(0, "{ Parsed from ".ucfirst($this->framework)." }");

		$date = @date("D M j G:i:s T Y");
		
		$this->PrintOutput(0, "");
		// allows parameter names conflicting with field names
		$this->PrintOutput(0, "{\$mode delphi}");
		$this->PrintOutput(0, "{\$modeswitch objectivec1}");
		// enables "external" after the semi-colon
		$this->PrintOutput(0, "{\$modeswitch cvar}");
		$this->PrintOutput(0, "");

		$this->PrintOutPut(0,"unit AnonClassDefinitions".ucfirst($this->framework).";");
		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "interface");
		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "type");

    foreach ($all_classes as $class)
			$this->PrintOutput(1, $class." = objcclass; external;");

		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "implementation");
		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "end.");


		// Now all anonymous external classes that have no real definition to an
		// include file that is added to the main unit. This way it is possible
		// to declare variables of these types in user programs without having to
		// include the unit above will all anonymous classes (should not be used)

		// open the output file if we not printing to terminal
		if (!$this->show) {
			$this->output = fopen("$this->root$this->out/$this->framework/AnonIncludeClassDefinitions".ucfirst($this->framework).".inc", "w+");
		}
		
		$this->PrintOutput(0, "{ Parsed from ".ucfirst($this->framework)." }");

		$date = @date("D M j G:i:s T Y");
		

		// add all classes as anonymous external classes. They will be overridden
		// by the actual definitions in the translated headers, but this way they
		// can appear as record field types and as callback parameters
		$first = true;
    foreach ($anon_classes as $class) {
    	if (!in_array($class,$defined_classes)) {
    		if ($first) {
    			$this->PrintOutput(0, "type");
    			$first = false;
    		}
				$this->PrintOutput(1, $class." = objcclass; external;");
			}
		}
  }

	
	// Prints all classes from the header in Objective-P FPC format
	function PrintHeader ($header) {
		global $version;
		//print_r($header);
		//print_r($this->dump["categories"]);
		
		// open the output file if we not printing to terminal
		if (!$this->show) {
			if ($this->merge_headers) {
				$this->output = fopen($header["path_merge"], "w+");
			} else {
				$this->output = fopen($header["path"], "w+");
			}
		}

		$this->PrintOutput(0, "{ Parsed from ".ucfirst($header["framework"]).".framework ".$header["name"]." }");

		$date = @date("D M j G:i:s T Y");
		$this->PrintOutput(0, "{ Version: $version - $date }");
		$this->PrintOutput(0, "");

		$macro = strtoupper(substr($header["name"], 0, (strripos($header["name"], "."))));
		
		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{\$ifdef TYPES}");
		$this->PrintOutput(0, "{\$ifndef $macro"."_PAS_T}");
		$this->PrintOutput(0, "{\$define $macro"."_PAS_T}");
		$this->PrintTypes($header, false);
		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{\$endif}");
		$this->PrintOutput(0, "{\$endif}");

		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{\$ifdef RECORDS}");
		$this->PrintOutput(0, "{\$ifndef $macro"."_PAS_R}");
		$this->PrintOutput(0, "{\$define $macro"."_PAS_R}");
		
		// Records from types
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
		$this->PrintOutput(0, "{\$ifndef $macro"."_PAS_S}");
		$this->PrintOutput(0, "{\$define $macro"."_PAS_S}");
		$this->PrintExternalSymbols($header);
		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{\$endif}");
		$this->PrintOutput(0, "{\$endif}");
		
		// insert user patches
		if ($this->HeaderContainsPatch($header)) {
			$this->PrintOutput(0, "");
			$this->PrintOutput(0, "{\$ifdef USER_PATCHES}");
			//$this->PrintOutput(0, "{\$ifndef $macro"."_PAS_PATCH}");
			//$this->PrintOutput(0, "{\$define $macro"."_PAS_PATCH}");
			$this->InsertPatches($header);
			$this->PrintOutput(0, "");
			//$this->PrintOutput(0, "{\$endif}");
			$this->PrintOutput(0, "{\$endif}");
		}

		if (($header["classes"]) || ($header["protocols"])) {
			$this->PrintOutput(0, "");
			$this->PrintOutput(0, "{\$ifdef FORWARD}");

			if ($header["protocols"]) {
				foreach ($header["protocols"] as $protocol) $this->PrintOutput(1, $protocol["name"]."$this->protocol_suffix = objcprotocol;");
			}
			
			if ($header["classes"]) {
				foreach ($header["classes"] as $class) {
					if ($class["name"]) {
						$this->PrintOutput(1, $class["name"]." = objcclass;");
						$this->PrintOutput(1, $class["name"].$this->class_pointer_suffix." = ^".$class["name"].";");
						// for consistency also offer Ptr-name variant
						$this->PrintOutput(1, $class["name"]."Ptr = ".$class["name"].$this->class_pointer_suffix.";");
					}
				}
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
				if ($class["name"]) $this->PrintClass($class);
			}

			if (count($header["categories"]) > 0) {
				foreach ($header["categories"] as $category) {
					$this->PrintCategory($class, $category);
				}
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
				if ($protocol["comment"]) $this->PrintOutput(0, $protocol["comment"]);
				$this->PrintOutput(1, $protocol["name"]."$this->protocol_suffix = objcprotocol");

				// print methods
				if ($protocol["methods"]) {
					foreach ($protocol["methods"] as $name => $method) {
						if ($method["comment"]) $this->PrintOutput(2, $method["comment"]);
						$this->PrintOutput(2, $method["def"]." message '".$method["objc_method"]."';".$method["deprecated"]);
					}
				}
				
				$this->PrintOutput(1, "end; external name '".$protocol["name"]."';");		
			}
			
			$this->PrintOutput(0, "{\$endif}");
			$this->PrintOutput(0, "{\$endif}");
		}
	}
	
	function PrintCategory ($class, $category) {

		// declare real category if external
		if ($category["external"]) {
			$new_name = " name '".$category["external_name"]."'";
		}
		
		$category_name = $category["name"].$this->category_suffix;
		
		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{ $category_name }");
		if ($category["comment"]) $this->PrintOutput(0, $category["comment"]);
		
		// print super class or protocol which the class conforms to
		$this->PrintOutput(1, "$category_name = objccategory(".$category["super"].")");
		
		// print methods
		if ($category["methods"]) {
			foreach ($category["methods"] as $method) {
				if ($method["comment"]) $this->PrintOutput(2, $method["comment"]);
				$this->PrintOutput(2, $method["def"]." message '".$method["objc_method"]."';".$method["deprecated"]);
			}
		} 

		$this->PrintOutput(1, "end; external$new_name;");
	}
		
	function PrintClass ($class) {

		$this->PrintOutput(0, "");
		$this->PrintOutput(0, "{ ".$class["name"]." }");
		
		if ($class["comment"]) $this->PrintOutput(0, $class["comment"]);
		//print_r($class["methods"]);
		
		
		// print super class or protocol which the class conforms to
		if ($class["adopts"]) {
			$this->PrintOutput(1, $class["name"]." = objcclass(".$class["super"].", ".$class["adopts"].")");
		} elseif ($class["super"]) {
			$this->PrintOutput(1, $class["name"]." = objcclass(".$class["super"].")");
		}

		// print instance variables
		if ($class["ivars"]) {
			$this->PrintOutput(1, "private");
			foreach ($class["ivars"] as $ivar) {
				$this->PrintOutput(2, $ivar);
			}
		}

		// print alloc method for the class
		$this->PrintOutput(2, "");
		$this->PrintOutput(1, "public");
		$this->PrintOutput(2, "class function alloc: ".$class["name"]."; message 'alloc';");
		
		// print class-level methods
		if ($class["methods"]) {
			$this->PrintOutput(0, "");
			foreach ($class["methods"] as $method) {
				if ($method["comment"]) $this->PrintOutput(2, $method["comment"]);
				$this->PrintOutput(2, $method["def"]." message '".$method["objc_method"]."';".$method["deprecated"]);
			}
		}
		
		// print adopted protocol methods
		if (count($class["protocols"]) > 0) {
			$this->PrintOutput(0, "");
			$this->PrintOutput(2, "{ Adopted Protocols }");
			//print_r($this->dump["protocols"]);

			foreach ($class["protocols"] as $name) {
				if ($this->dump["protocols"][$name]) {
					foreach ($this->dump["protocols"][$name] as $method) {
						if (!$this->ClassContainsMethod($class, $method)) $this->PrintOutput(2, $method["def"]);
					}
				}
			}
		}

		$this->PrintOutput(1, "end; external;");
	}

}
?>