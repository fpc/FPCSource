<?php
		
	function str_replace_word ($needle, $replacement, $haystack) {
	    $pattern = "/\b$needle\b/";
	    $haystack = preg_replace($pattern, $replacement, $haystack);
	    return $haystack;
	}

	function istr_replace_word ($needle, $replacement, $haystack) {
	    $pattern = "/\b$needle\b/i";
	    $haystack = preg_replace($pattern, $replacement, $haystack);
	    return $haystack;
	}

	function strtolowerref(&$str) {
	  $str = strtolower($str);
	}

	// converts "STR_XX_YY_ZZ" to "Str Xx Yy Zz"
	function DeprecatedMacroToDirective($str) {
		// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4 etc
		if (preg_match("!DEPRECATED_(IN_.*VERSION_[0-9]*(?:_[0-9]*)?)!", $str, $matches)) {
			$str = strtolower($matches[1]);
			// cleanup
			$str = preg_replace("!in_mac_os_x_version_!", "in Mac OS X ", $str);
			$str = preg_replace("!([0-9])_([0-9])!e", '\1.\2', $str);
			return " deprecated '" . $str . " and later'";
		// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED
		} elseif (preg_match("!MAC_OS_X_VERSION_([0-9]*(?:_[0-9]*)?)(?:_AND_LATER)?_BUT_DEPRECATED!", $str, $matches)) {
			$str = preg_replace("!_!", ".", $matches[1]);
			return " deprecated 'in Mac OS X " . $str . " and later'";
		// NS_DEPRECATED(__MAC_10_1,__MAC_10_6,__IPHONE_NA,__IPHONE_NA),
		// __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1,__MAC_10_6,__IPHONE_NA,__IPHONE_NA,
		// NS_DEPRECATED_IPHONE(__IPHONE_2_0,__IPHONE_2_0),
		// NS_DEPRECATED_MAC(__MAC_10_1, __MAC_10_5)
		} elseif (preg_match("!DEPRECATED(_IPHONE|_MAC)?\(([^)]+)\)!", $str, $matches)) {
			$str = strtolower($matches[2]);
			$versions = preg_replace("!__MAC_!e", "Mac OS X ", $str);
			$versions = preg_replace("!__IPHONE_!e", "iOS ", $versions);
			$versions = preg_replace("!([0-9])_([0-9])!e", '\1.\2', $versions);
			$deprecatedversions = explode(",", $matches[2]);
			$result = "";
			if ($matches[1] == "") {
				$macindex = 2;
				$iphoneindex = 4;
			} elseif ($matches[1] == "_IPHONE") {
				$macindex = -1;
				$iphoneindex = 2;
			} else {
				$macindex = 2;
				$iphoneindex = -1;
			}
			if (($macindex != -1) && !preg_match("!Mac OS X( Version)? NA|MAC_NA!", $deprecatedversions[$macindex])) {
				$result = "in " . $deprecatedversions[$macindex] . " and later";
			}
			if (($iphoneindex != -1) && !preg_match("!iOS( Version)? NA|IPHONE_NA!", $deprecatedversions[$iphoneindex])) {
				if ($result != "") $result .= ", ";
				$result .= "in " . $deprecatedversions[$iphoneindex] . " and later";
			}
			return " deprecated '" . $result . "'";
		}
	}

?>