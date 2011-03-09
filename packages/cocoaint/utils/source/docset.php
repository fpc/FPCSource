<?

// Returns an array of sub-directories for $directory
function sub_directories ($directory, &$directories) {
	
	if (file_exists($directory)) {
		$directories[] = $directory;
	} else {
		$directories = array();
	}
	
	if ($handle = @opendir($directory)) {
		while (($file = readdir($handle)) !== false) {
			if (($file != '.') && ($file != '..') && ($file[0] != '.')) {
				$path = "$directory/$file";
				
				if (is_dir($path)) sub_directories($path, $directories);
			}
		}
		closedir($handle);
	}
}


class DocSetParser {

	var $docset;
	var $methods = array();

	// Parse a single reference file
	private function parse ($path, $class) {
		$method = null;
		$methods = array();
		
		if ($lines = @file($path)) {
			foreach ($lines as $line) {

				// Parse instance method description
				if ($method) {
					if (preg_match("/<span.*data-abstract='(.*)'>/i", $line, $captures)) {
						$methods[$method] = strip_tags($captures[1]);
						$method = null;
					}
				}

				// Found type definition tag
				if (preg_match("/<h3 class=\"tight jump typeDef\">(\w+)<\/h3><p class=\"abstract\">(.*)<\/p>/i", $line, $captures)) {
					
					// Read until the end </p> because I can't get preg_match to do this
					$text = substr($captures[2], 0, (stripos($captures[2], "</p>")));
					if (!$text) $text = $captures[2];
					
					$methods[$captures[1]] = strip_tags($text);
				}
				
				// Found constant tag
				if (preg_match("/<code class=\"jump constantName\">(\w+)<\/code><\/dt><dd><p>(.*)<\/p>/i", $line, $captures)) {
					//print_r($captures);
					// Read until the end </p> because I can't get preg_match to do this
					$text = substr($captures[2], 0, (stripos($captures[2], "</p>")));
					if (!$text) $text = $captures[2];
					
					$methods[$captures[1]] = strip_tags($text);
				}
				
				// Found instance method tag
				if (preg_match("<a href=\"#//apple_ref/occ/(instm|intfm|intfp|clm)+/([a-zA-Z:_]+)/([a-zA-Z:_]+)\">", $line, $captures)) {
					//print_r($captures);
					if ($captures[2] == $class) {
						$method = $captures[3];
						continue;
					}
				}

			}

		} else {
			print("* Warning: Can't find the docset at $path.\n");
		}
		
		return $methods;
	}
	
	// Parses the reference index.html file for the path to the reference html file
	private function parse_reference_index ($path) {
		$lines = file($path);
		foreach ($lines as $line) {
			if (preg_match("/<meta id=\"refresh\" http-equiv=\"refresh\" CONTENT=\"0; URL=(.*)\">/i", $line, $captures)) {
				return $captures[1];
			}
		}
	}
	
	public function parse_directory ($paths) {

		foreach ($paths as $path) {
			$path = $this->docset."/Contents/Resources/Documents/documentation".$path;
			
			if (file_exists($path)) {
				sub_directories($path, $sub_directories);
				foreach ($sub_directories as $directory) {
					$name = basename($directory);
					if (preg_match("/^(\w+)_(class|protocol)$/i", $name, $captures)) {
						
						$class = $captures[1];
						$sub_path = $this->parse_reference_index("$directory/index.html");
						
						if ($methods = $this->parse("$directory/$sub_path", $class)) {
							$this->methods[$class] = $methods;
						}
					}
				}
			} else {
				print("* Warning: The docset at $path can't be found.\n");
			}
		}
		
		return true;
	}
		
	function __construct ($docset) {
		$this->docset = $docset;
	}
}


// Cocoa
//$path = "/Users/ryanjoseph/Desktop/com.apple.adc.documentation.AppleSnowLeopard.CoreReference.docset";
//$folders = array("/Cocoa/Reference");

// UIKIT
// $path = "/Users/ryanjoseph/Desktop/com.apple.adc.documentation.AppleiOS4_2.iOSLibrary.docset";
//$folders = array("/UIKit/Reference");

//$parser = new DocSetParser($path);
//$parser->parse_directory($folders);
//print_r($parser->methods);


?>