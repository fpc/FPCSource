This folder requires the next files to be present:

  Extracted from https://www.unicode.org/Public/zipped/14.0.0/UCD.zip:
    * UnicodeData.txt
    * HangulSyllableType.txt
    * PropList.txt

  Extracted from https://www.unicode.org/Public/cldr/40/core.zip :
    * in the subfolder "common\uca"
        ** allkeys.txt : this file is actually the allkeys_CLDR.txt file renamed. It is the CLDR's root collation.
        ** UCA_Rules_SHORT.txt
        ** CollationTest_CLDR_NON_IGNORABLE_SHORT.txt
        ** CollationTest_CLDR_SHIFTED_SHORT.txt
    * in the subfolder "common\collation"
        ** all the language specific xml files (de.xml, es.xml, ...)
