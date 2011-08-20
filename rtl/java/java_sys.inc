  JLObject = class external 'java.lang' name 'Object' 
  public
    constructor create(); overload;
    function getClass(): JLClass; overload; virtual; final;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
  strict protected
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  public
    function toString(): JLString; overload; virtual;
    procedure notify(); overload; virtual; final;
    procedure notifyAll(); overload; virtual; final;
    procedure wait(para1: jlong); overload; virtual; final;  // throws java.lang.InterruptedException
    procedure wait(para1: jlong; para2: jint); overload; virtual; final;  // throws java.lang.InterruptedException
    procedure wait(); overload; virtual; final;  // throws java.lang.InterruptedException
  strict protected
    procedure finalize(); overload; virtual;  // throws java.lang.Throwable
  end;

  JISerializable = interface external 'java.io' name 'Serializable' 
  end;

  JLAppendable = interface external 'java.lang' name 'Appendable' 
    function append(para1: JLCharSequence): JLAppendable; overload;  // throws java.io.IOException
    function append(para1: JLCharSequence; para2: jint; para3: jint): JLAppendable; overload;  // throws java.io.IOException
    function append(para1: jchar): JLAppendable; overload;  // throws java.io.IOException
  end;

  JLCharSequence = interface external 'java.lang' name 'CharSequence' 
    function length(): jint; overload;
    function charAt(para1: jint): jchar; overload;
    function subSequence(para1: jint; para2: jint): JLCharSequence; overload;
    function toString(): JLString; overload;
  end;

  JLCloneable = interface external 'java.lang' name 'Cloneable' 
  end;

  JLComparable = interface external 'java.lang' name 'Comparable' 
    function compareTo(para1: JLObject): jint; overload;
  end;

  JLSystem = class sealed external 'java.lang' name 'System' (JLObject)
  public
    final class var
      fin: JIInputStream; external name 'in';
      fout: JIPrintStream; external name 'out';
      ferr: JIPrintStream; external name 'err';
  public
    class procedure setIn(para1: JIInputStream); static; overload;
    class procedure setOut(para1: JIPrintStream); static; overload;
    class procedure setErr(para1: JIPrintStream); static; overload;
    class function console(): JIConsole; static; overload;
    class function inheritedChannel(): JNCChannel; static; overload;  // throws java.io.IOException
    class procedure setSecurityManager(para1: JLSecurityManager); static; overload;
    class function getSecurityManager(): JLSecurityManager; static; overload;
    class function currentTimeMillis(): jlong; static; overload;
    class function nanoTime(): jlong; static; overload;
    class procedure arraycopy(para1: JLObject; para2: jint; para3: JLObject; para4: jint; para5: jint); static; overload;
    class function identityHashCode(para1: JLObject): jint; static; overload;
    class function getProperties(): JUProperties; static; overload;
    class procedure setProperties(para1: JUProperties); static; overload;
    class function getProperty(para1: JLString): JLString; static; overload;
    class function getProperty(para1: JLString; para2: JLString): JLString; static; overload;
    class function setProperty(para1: JLString; para2: JLString): JLString; static; overload;
    class function clearProperty(para1: JLString): JLString; static; overload;
    class function getenv(para1: JLString): JLString; static; overload;
    class function getenv(): JUMap; static; overload;
    class procedure exit(para1: jint); static; overload;
    class procedure gc(); static; overload;
    class procedure runFinalization(); static; overload;
    class procedure runFinalizersOnExit(para1: jboolean); static; overload;
    class procedure load(para1: JLString); static; overload;
    class procedure loadLibrary(para1: JLString); static; overload;
    class function mapLibraryName(para1: JLString): JLString; static; overload;
  end;

  JUComparator = interface external 'java.util' name 'Comparator' 
    function compare(para1: JLObject; para2: JLObject): jint; overload;
    function equals(para1: JLObject): jboolean; overload;
  end;

  JLNumber = class abstract external 'java.lang' name 'Number' (JLObject, JISerializable)
  public
    constructor create(); overload;
    function intValue(): jint; overload; virtual; abstract;
    function longValue(): jlong; overload; virtual; abstract;
    function floatValue(): jfloat; overload; virtual; abstract;
    function doubleValue(): jdouble; overload; virtual; abstract;
    function byteValue(): jbyte; overload; virtual;
    function shortValue(): jshort; overload; virtual;
  end;

  JLThrowable = class external 'java.lang' name 'Throwable' (JLObject, JISerializable)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
    constructor create(para1: JLThrowable); overload;
    function getMessage(): JLString; overload; virtual;
    function getLocalizedMessage(): JLString; overload; virtual;
    function getCause(): JLThrowable; overload; virtual;
    function initCause(para1: JLThrowable): JLThrowable; overload; virtual;
    function toString(): JLString; overload; virtual;
    procedure printStackTrace(); overload; virtual;
    procedure printStackTrace(para1: JIPrintStream); overload; virtual;
    procedure printStackTrace(para1: JIPrintWriter); overload; virtual;
    function fillInStackTrace(): JLThrowable; overload; virtual;
    function getStackTrace(): Arr1JLStackTraceElement; overload; virtual;
    procedure setStackTrace(para1: Arr1JLStackTraceElement); overload; virtual;
    procedure setStackTrace(var para1: array of JLStackTraceElement); overload; virtual;
  end;

  JLAbstractStringBuilder = class abstract external 'java.lang' name 'AbstractStringBuilder' (JLObject, JLAppendable, JLCharSequence)
  public
    function length(): jint; overload; virtual;
    function capacity(): jint; overload; virtual;
    procedure ensureCapacity(para1: jint); overload; virtual;
    procedure trimToSize(); overload; virtual;
    procedure setLength(para1: jint); overload; virtual;
    function charAt(para1: jint): jchar; overload; virtual;
    function codePointAt(para1: jint): jint; overload; virtual;
    function codePointBefore(para1: jint): jint; overload; virtual;
    function codePointCount(para1: jint; para2: jint): jint; overload; virtual;
    function offsetByCodePoints(para1: jint; para2: jint): jint; overload; virtual;
    procedure getChars(para1: jint; para2: jint; para3: Arr1jchar; para4: jint); overload; virtual;
    procedure getChars(para1: jint; para2: jint; var para3: array of jchar; para4: jint); overload; virtual;
    procedure setCharAt(para1: jint; para2: jchar); overload; virtual;
    function append(para1: JLObject): JLAbstractStringBuilder; overload; virtual;
    function append(para1: JLString): JLAbstractStringBuilder; overload; virtual;
    function append(para1: JLStringBuffer): JLAbstractStringBuilder; overload; virtual;
    function append(para1: JLCharSequence): JLAbstractStringBuilder; overload; virtual;
    function append(para1: JLCharSequence; para2: jint; para3: jint): JLAbstractStringBuilder; overload; virtual;
    function append(para1: Arr1jchar): JLAbstractStringBuilder; overload; virtual;
    function append(var para1: array of jchar): JLAbstractStringBuilder; overload; virtual;
    function append(para1: Arr1jchar; para2: jint; para3: jint): JLAbstractStringBuilder; overload; virtual;
    function append(var para1: array of jchar; para2: jint; para3: jint): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jboolean): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jchar): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jint): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jlong): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jfloat): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jdouble): JLAbstractStringBuilder; overload; virtual;
    function delete(para1: jint; para2: jint): JLAbstractStringBuilder; overload; virtual;
    function appendCodePoint(para1: jint): JLAbstractStringBuilder; overload; virtual;
    function deleteCharAt(para1: jint): JLAbstractStringBuilder; overload; virtual;
    function replace(para1: jint; para2: jint; para3: JLString): JLAbstractStringBuilder; overload; virtual;
    function substring(para1: jint): JLString; overload; virtual;
    function subSequence(para1: jint; para2: jint): JLCharSequence; overload; virtual;
    function substring(para1: jint; para2: jint): JLString; overload; virtual;
    function insert(para1: jint; para2: Arr1jchar; para3: jint; para4: jint): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; var para2: array of jchar; para3: jint; para4: jint): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLObject): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLString): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: Arr1jchar): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; var para2: array of jchar): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLCharSequence): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLCharSequence; para3: jint; para4: jint): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jboolean): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jchar): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jint): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jlong): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jfloat): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jdouble): JLAbstractStringBuilder; overload; virtual;
    function indexOf(para1: JLString): jint; overload; virtual;
    function indexOf(para1: JLString; para2: jint): jint; overload; virtual;
    function lastIndexOf(para1: JLString): jint; overload; virtual;
    function lastIndexOf(para1: JLString; para2: jint): jint; overload; virtual;
    function reverse(): JLAbstractStringBuilder; overload; virtual;
    function toString(): JLString; overload; virtual; abstract;
    function append(para1: jchar): JLAppendable; overload; virtual;  // throws java.io.IOException
    function append(para1: JLCharSequence; para2: jint; para3: jint): JLAppendable; overload; virtual;  // throws java.io.IOException
    function append(para1: JLCharSequence): JLAppendable; overload; virtual;  // throws java.io.IOException
  end;

  JLCharacter = class sealed external 'java.lang' name 'Character' (JLObject, JISerializable, JLComparable)
  public
    type
      InnerCharacterCache = class;
      Arr1InnerCharacterCache = array of InnerCharacterCache;
      Arr2InnerCharacterCache = array of Arr1InnerCharacterCache;
      Arr3InnerCharacterCache = array of Arr2InnerCharacterCache;
      InnerCharacterCache = class external 'java.lang' name 'Character$CharacterCache' 
      end;

  public
    type
      InnerSubset = class;
      Arr1InnerSubset = array of InnerSubset;
      Arr2InnerSubset = array of Arr1InnerSubset;
      Arr3InnerSubset = array of Arr2InnerSubset;
      InnerUnicodeBlock = class;
      Arr1InnerUnicodeBlock = array of InnerUnicodeBlock;
      Arr2InnerUnicodeBlock = array of Arr1InnerUnicodeBlock;
      Arr3InnerUnicodeBlock = array of Arr2InnerUnicodeBlock;
      InnerSubset = class external 'java.lang' name 'Character$Subset' (JLObject)
      strict protected
        constructor create(para1: JLString); overload;
      public
        function equals(para1: JLObject): jboolean; overload; virtual; final;
        function hashCode(): jint; overload; virtual; final;
        function toString(): JLString; overload; virtual; final;
      end;

      InnerUnicodeBlock = class sealed external 'java.lang' name 'Character$UnicodeBlock' (InnerSubset)
      public
        final class var
          fBASIC_LATIN: JLCharacter.InnerUnicodeBlock; external name 'BASIC_LATIN';
          fLATIN_1_SUPPLEMENT: JLCharacter.InnerUnicodeBlock; external name 'LATIN_1_SUPPLEMENT';
          fLATIN_EXTENDED_A: JLCharacter.InnerUnicodeBlock; external name 'LATIN_EXTENDED_A';
          fLATIN_EXTENDED_B: JLCharacter.InnerUnicodeBlock; external name 'LATIN_EXTENDED_B';
          fIPA_EXTENSIONS: JLCharacter.InnerUnicodeBlock; external name 'IPA_EXTENSIONS';
          fSPACING_MODIFIER_LETTERS: JLCharacter.InnerUnicodeBlock; external name 'SPACING_MODIFIER_LETTERS';
          fCOMBINING_DIACRITICAL_MARKS: JLCharacter.InnerUnicodeBlock; external name 'COMBINING_DIACRITICAL_MARKS';
          fGREEK: JLCharacter.InnerUnicodeBlock; external name 'GREEK';
          fCYRILLIC: JLCharacter.InnerUnicodeBlock; external name 'CYRILLIC';
          fARMENIAN: JLCharacter.InnerUnicodeBlock; external name 'ARMENIAN';
          fHEBREW: JLCharacter.InnerUnicodeBlock; external name 'HEBREW';
          fARABIC: JLCharacter.InnerUnicodeBlock; external name 'ARABIC';
          fDEVANAGARI: JLCharacter.InnerUnicodeBlock; external name 'DEVANAGARI';
          fBENGALI: JLCharacter.InnerUnicodeBlock; external name 'BENGALI';
          fGURMUKHI: JLCharacter.InnerUnicodeBlock; external name 'GURMUKHI';
          fGUJARATI: JLCharacter.InnerUnicodeBlock; external name 'GUJARATI';
          fORIYA: JLCharacter.InnerUnicodeBlock; external name 'ORIYA';
          fTAMIL: JLCharacter.InnerUnicodeBlock; external name 'TAMIL';
          fTELUGU: JLCharacter.InnerUnicodeBlock; external name 'TELUGU';
          fKANNADA: JLCharacter.InnerUnicodeBlock; external name 'KANNADA';
          fMALAYALAM: JLCharacter.InnerUnicodeBlock; external name 'MALAYALAM';
          fTHAI: JLCharacter.InnerUnicodeBlock; external name 'THAI';
          fLAO: JLCharacter.InnerUnicodeBlock; external name 'LAO';
          fTIBETAN: JLCharacter.InnerUnicodeBlock; external name 'TIBETAN';
          fGEORGIAN: JLCharacter.InnerUnicodeBlock; external name 'GEORGIAN';
          fHANGUL_JAMO: JLCharacter.InnerUnicodeBlock; external name 'HANGUL_JAMO';
          fLATIN_EXTENDED_ADDITIONAL: JLCharacter.InnerUnicodeBlock; external name 'LATIN_EXTENDED_ADDITIONAL';
          fGREEK_EXTENDED: JLCharacter.InnerUnicodeBlock; external name 'GREEK_EXTENDED';
          fGENERAL_PUNCTUATION: JLCharacter.InnerUnicodeBlock; external name 'GENERAL_PUNCTUATION';
          fSUPERSCRIPTS_AND_SUBSCRIPTS: JLCharacter.InnerUnicodeBlock; external name 'SUPERSCRIPTS_AND_SUBSCRIPTS';
          fCURRENCY_SYMBOLS: JLCharacter.InnerUnicodeBlock; external name 'CURRENCY_SYMBOLS';
          fCOMBINING_MARKS_FOR_SYMBOLS: JLCharacter.InnerUnicodeBlock; external name 'COMBINING_MARKS_FOR_SYMBOLS';
          fLETTERLIKE_SYMBOLS: JLCharacter.InnerUnicodeBlock; external name 'LETTERLIKE_SYMBOLS';
          fNUMBER_FORMS: JLCharacter.InnerUnicodeBlock; external name 'NUMBER_FORMS';
          fARROWS: JLCharacter.InnerUnicodeBlock; external name 'ARROWS';
          fMATHEMATICAL_OPERATORS: JLCharacter.InnerUnicodeBlock; external name 'MATHEMATICAL_OPERATORS';
          fMISCELLANEOUS_TECHNICAL: JLCharacter.InnerUnicodeBlock; external name 'MISCELLANEOUS_TECHNICAL';
          fCONTROL_PICTURES: JLCharacter.InnerUnicodeBlock; external name 'CONTROL_PICTURES';
          fOPTICAL_CHARACTER_RECOGNITION: JLCharacter.InnerUnicodeBlock; external name 'OPTICAL_CHARACTER_RECOGNITION';
          fENCLOSED_ALPHANUMERICS: JLCharacter.InnerUnicodeBlock; external name 'ENCLOSED_ALPHANUMERICS';
          fBOX_DRAWING: JLCharacter.InnerUnicodeBlock; external name 'BOX_DRAWING';
          fBLOCK_ELEMENTS: JLCharacter.InnerUnicodeBlock; external name 'BLOCK_ELEMENTS';
          fGEOMETRIC_SHAPES: JLCharacter.InnerUnicodeBlock; external name 'GEOMETRIC_SHAPES';
          fMISCELLANEOUS_SYMBOLS: JLCharacter.InnerUnicodeBlock; external name 'MISCELLANEOUS_SYMBOLS';
          fDINGBATS: JLCharacter.InnerUnicodeBlock; external name 'DINGBATS';
          fCJK_SYMBOLS_AND_PUNCTUATION: JLCharacter.InnerUnicodeBlock; external name 'CJK_SYMBOLS_AND_PUNCTUATION';
          fHIRAGANA: JLCharacter.InnerUnicodeBlock; external name 'HIRAGANA';
          fKATAKANA: JLCharacter.InnerUnicodeBlock; external name 'KATAKANA';
          fBOPOMOFO: JLCharacter.InnerUnicodeBlock; external name 'BOPOMOFO';
          fHANGUL_COMPATIBILITY_JAMO: JLCharacter.InnerUnicodeBlock; external name 'HANGUL_COMPATIBILITY_JAMO';
          fKANBUN: JLCharacter.InnerUnicodeBlock; external name 'KANBUN';
          fENCLOSED_CJK_LETTERS_AND_MONTHS: JLCharacter.InnerUnicodeBlock; external name 'ENCLOSED_CJK_LETTERS_AND_MONTHS';
          fCJK_COMPATIBILITY: JLCharacter.InnerUnicodeBlock; external name 'CJK_COMPATIBILITY';
          fCJK_UNIFIED_IDEOGRAPHS: JLCharacter.InnerUnicodeBlock; external name 'CJK_UNIFIED_IDEOGRAPHS';
          fHANGUL_SYLLABLES: JLCharacter.InnerUnicodeBlock; external name 'HANGUL_SYLLABLES';
          fPRIVATE_USE_AREA: JLCharacter.InnerUnicodeBlock; external name 'PRIVATE_USE_AREA';
          fCJK_COMPATIBILITY_IDEOGRAPHS: JLCharacter.InnerUnicodeBlock; external name 'CJK_COMPATIBILITY_IDEOGRAPHS';
          fALPHABETIC_PRESENTATION_FORMS: JLCharacter.InnerUnicodeBlock; external name 'ALPHABETIC_PRESENTATION_FORMS';
          fARABIC_PRESENTATION_FORMS_A: JLCharacter.InnerUnicodeBlock; external name 'ARABIC_PRESENTATION_FORMS_A';
          fCOMBINING_HALF_MARKS: JLCharacter.InnerUnicodeBlock; external name 'COMBINING_HALF_MARKS';
          fCJK_COMPATIBILITY_FORMS: JLCharacter.InnerUnicodeBlock; external name 'CJK_COMPATIBILITY_FORMS';
          fSMALL_FORM_VARIANTS: JLCharacter.InnerUnicodeBlock; external name 'SMALL_FORM_VARIANTS';
          fARABIC_PRESENTATION_FORMS_B: JLCharacter.InnerUnicodeBlock; external name 'ARABIC_PRESENTATION_FORMS_B';
          fHALFWIDTH_AND_FULLWIDTH_FORMS: JLCharacter.InnerUnicodeBlock; external name 'HALFWIDTH_AND_FULLWIDTH_FORMS';
          fSPECIALS: JLCharacter.InnerUnicodeBlock; external name 'SPECIALS';
          fSURROGATES_AREA: JLCharacter.InnerUnicodeBlock deprecated; external name 'SURROGATES_AREA';
          fSYRIAC: JLCharacter.InnerUnicodeBlock; external name 'SYRIAC';
          fTHAANA: JLCharacter.InnerUnicodeBlock; external name 'THAANA';
          fSINHALA: JLCharacter.InnerUnicodeBlock; external name 'SINHALA';
          fMYANMAR: JLCharacter.InnerUnicodeBlock; external name 'MYANMAR';
          fETHIOPIC: JLCharacter.InnerUnicodeBlock; external name 'ETHIOPIC';
          fCHEROKEE: JLCharacter.InnerUnicodeBlock; external name 'CHEROKEE';
          fUNIFIED_CANADIAN_ABORIGINAL_SYLLABICS: JLCharacter.InnerUnicodeBlock; external name 'UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS';
          fOGHAM: JLCharacter.InnerUnicodeBlock; external name 'OGHAM';
          fRUNIC: JLCharacter.InnerUnicodeBlock; external name 'RUNIC';
          fKHMER: JLCharacter.InnerUnicodeBlock; external name 'KHMER';
          fMONGOLIAN: JLCharacter.InnerUnicodeBlock; external name 'MONGOLIAN';
          fBRAILLE_PATTERNS: JLCharacter.InnerUnicodeBlock; external name 'BRAILLE_PATTERNS';
          fCJK_RADICALS_SUPPLEMENT: JLCharacter.InnerUnicodeBlock; external name 'CJK_RADICALS_SUPPLEMENT';
          fKANGXI_RADICALS: JLCharacter.InnerUnicodeBlock; external name 'KANGXI_RADICALS';
          fIDEOGRAPHIC_DESCRIPTION_CHARACTERS: JLCharacter.InnerUnicodeBlock; external name 'IDEOGRAPHIC_DESCRIPTION_CHARACTERS';
          fBOPOMOFO_EXTENDED: JLCharacter.InnerUnicodeBlock; external name 'BOPOMOFO_EXTENDED';
          fCJK_UNIFIED_IDEOGRAPHS_EXTENSION_A: JLCharacter.InnerUnicodeBlock; external name 'CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A';
          fYI_SYLLABLES: JLCharacter.InnerUnicodeBlock; external name 'YI_SYLLABLES';
          fYI_RADICALS: JLCharacter.InnerUnicodeBlock; external name 'YI_RADICALS';
          fCYRILLIC_SUPPLEMENTARY: JLCharacter.InnerUnicodeBlock; external name 'CYRILLIC_SUPPLEMENTARY';
          fTAGALOG: JLCharacter.InnerUnicodeBlock; external name 'TAGALOG';
          fHANUNOO: JLCharacter.InnerUnicodeBlock; external name 'HANUNOO';
          fBUHID: JLCharacter.InnerUnicodeBlock; external name 'BUHID';
          fTAGBANWA: JLCharacter.InnerUnicodeBlock; external name 'TAGBANWA';
          fLIMBU: JLCharacter.InnerUnicodeBlock; external name 'LIMBU';
          fTAI_LE: JLCharacter.InnerUnicodeBlock; external name 'TAI_LE';
          fKHMER_SYMBOLS: JLCharacter.InnerUnicodeBlock; external name 'KHMER_SYMBOLS';
          fPHONETIC_EXTENSIONS: JLCharacter.InnerUnicodeBlock; external name 'PHONETIC_EXTENSIONS';
          fMISCELLANEOUS_MATHEMATICAL_SYMBOLS_A: JLCharacter.InnerUnicodeBlock; external name 'MISCELLANEOUS_MATHEMATICAL_SYMBOLS_A';
          fSUPPLEMENTAL_ARROWS_A: JLCharacter.InnerUnicodeBlock; external name 'SUPPLEMENTAL_ARROWS_A';
          fSUPPLEMENTAL_ARROWS_B: JLCharacter.InnerUnicodeBlock; external name 'SUPPLEMENTAL_ARROWS_B';
          fMISCELLANEOUS_MATHEMATICAL_SYMBOLS_B: JLCharacter.InnerUnicodeBlock; external name 'MISCELLANEOUS_MATHEMATICAL_SYMBOLS_B';
          fSUPPLEMENTAL_MATHEMATICAL_OPERATORS: JLCharacter.InnerUnicodeBlock; external name 'SUPPLEMENTAL_MATHEMATICAL_OPERATORS';
          fMISCELLANEOUS_SYMBOLS_AND_ARROWS: JLCharacter.InnerUnicodeBlock; external name 'MISCELLANEOUS_SYMBOLS_AND_ARROWS';
          fKATAKANA_PHONETIC_EXTENSIONS: JLCharacter.InnerUnicodeBlock; external name 'KATAKANA_PHONETIC_EXTENSIONS';
          fYIJING_HEXAGRAM_SYMBOLS: JLCharacter.InnerUnicodeBlock; external name 'YIJING_HEXAGRAM_SYMBOLS';
          fVARIATION_SELECTORS: JLCharacter.InnerUnicodeBlock; external name 'VARIATION_SELECTORS';
          fLINEAR_B_SYLLABARY: JLCharacter.InnerUnicodeBlock; external name 'LINEAR_B_SYLLABARY';
          fLINEAR_B_IDEOGRAMS: JLCharacter.InnerUnicodeBlock; external name 'LINEAR_B_IDEOGRAMS';
          fAEGEAN_NUMBERS: JLCharacter.InnerUnicodeBlock; external name 'AEGEAN_NUMBERS';
          fOLD_ITALIC: JLCharacter.InnerUnicodeBlock; external name 'OLD_ITALIC';
          fGOTHIC: JLCharacter.InnerUnicodeBlock; external name 'GOTHIC';
          fUGARITIC: JLCharacter.InnerUnicodeBlock; external name 'UGARITIC';
          fDESERET: JLCharacter.InnerUnicodeBlock; external name 'DESERET';
          fSHAVIAN: JLCharacter.InnerUnicodeBlock; external name 'SHAVIAN';
          fOSMANYA: JLCharacter.InnerUnicodeBlock; external name 'OSMANYA';
          fCYPRIOT_SYLLABARY: JLCharacter.InnerUnicodeBlock; external name 'CYPRIOT_SYLLABARY';
          fBYZANTINE_MUSICAL_SYMBOLS: JLCharacter.InnerUnicodeBlock; external name 'BYZANTINE_MUSICAL_SYMBOLS';
          fMUSICAL_SYMBOLS: JLCharacter.InnerUnicodeBlock; external name 'MUSICAL_SYMBOLS';
          fTAI_XUAN_JING_SYMBOLS: JLCharacter.InnerUnicodeBlock; external name 'TAI_XUAN_JING_SYMBOLS';
          fMATHEMATICAL_ALPHANUMERIC_SYMBOLS: JLCharacter.InnerUnicodeBlock; external name 'MATHEMATICAL_ALPHANUMERIC_SYMBOLS';
          fCJK_UNIFIED_IDEOGRAPHS_EXTENSION_B: JLCharacter.InnerUnicodeBlock; external name 'CJK_UNIFIED_IDEOGRAPHS_EXTENSION_B';
          fCJK_COMPATIBILITY_IDEOGRAPHS_SUPPLEMENT: JLCharacter.InnerUnicodeBlock; external name 'CJK_COMPATIBILITY_IDEOGRAPHS_SUPPLEMENT';
          fTAGS: JLCharacter.InnerUnicodeBlock; external name 'TAGS';
          fVARIATION_SELECTORS_SUPPLEMENT: JLCharacter.InnerUnicodeBlock; external name 'VARIATION_SELECTORS_SUPPLEMENT';
          fSUPPLEMENTARY_PRIVATE_USE_AREA_A: JLCharacter.InnerUnicodeBlock; external name 'SUPPLEMENTARY_PRIVATE_USE_AREA_A';
          fSUPPLEMENTARY_PRIVATE_USE_AREA_B: JLCharacter.InnerUnicodeBlock; external name 'SUPPLEMENTARY_PRIVATE_USE_AREA_B';
          fHIGH_SURROGATES: JLCharacter.InnerUnicodeBlock; external name 'HIGH_SURROGATES';
          fHIGH_PRIVATE_USE_SURROGATES: JLCharacter.InnerUnicodeBlock; external name 'HIGH_PRIVATE_USE_SURROGATES';
          fLOW_SURROGATES: JLCharacter.InnerUnicodeBlock; external name 'LOW_SURROGATES';
      public
        class function &of(para1: jchar): JLCharacter.InnerUnicodeBlock; static; overload;
        class function &of(para1: jint): JLCharacter.InnerUnicodeBlock; static; overload;
        class function forName(para1: JLString): JLCharacter.InnerUnicodeBlock; static; overload; virtual; final;
      end;

  public
    const
      MIN_RADIX = 2;
      MAX_RADIX = 36;
      MIN_VALUE = 0;
      MAX_VALUE = 65535;
  public
    final class var
      fTYPE: JLClass; external name 'TYPE';
  public
    const
      UNASSIGNED = 0;
      UPPERCASE_LETTER = 1;
      LOWERCASE_LETTER = 2;
      TITLECASE_LETTER = 3;
      MODIFIER_LETTER = 4;
      OTHER_LETTER = 5;
      NON_SPACING_MARK = 6;
      ENCLOSING_MARK = 7;
      COMBINING_SPACING_MARK = 8;
      DECIMAL_DIGIT_NUMBER = 9;
      LETTER_NUMBER = 10;
      OTHER_NUMBER = 11;
      SPACE_SEPARATOR = 12;
      LINE_SEPARATOR = 13;
      PARAGRAPH_SEPARATOR = 14;
      CONTROL = 15;
      FORMAT = 16;
      PRIVATE_USE = 18;
      SURROGATE = 19;
      DASH_PUNCTUATION = 20;
      START_PUNCTUATION = 21;
      END_PUNCTUATION = 22;
      CONNECTOR_PUNCTUATION = 23;
      OTHER_PUNCTUATION = 24;
      MATH_SYMBOL = 25;
      CURRENCY_SYMBOL = 26;
      MODIFIER_SYMBOL = 27;
      OTHER_SYMBOL = 28;
      INITIAL_QUOTE_PUNCTUATION = 29;
      FINAL_QUOTE_PUNCTUATION = 30;
      DIRECTIONALITY_UNDEFINED = -1;
      DIRECTIONALITY_LEFT_TO_RIGHT = 0;
      DIRECTIONALITY_RIGHT_TO_LEFT = 1;
      DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC = 2;
      DIRECTIONALITY_EUROPEAN_NUMBER = 3;
      DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR = 4;
      DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR = 5;
      DIRECTIONALITY_ARABIC_NUMBER = 6;
      DIRECTIONALITY_COMMON_NUMBER_SEPARATOR = 7;
      DIRECTIONALITY_NONSPACING_MARK = 8;
      DIRECTIONALITY_BOUNDARY_NEUTRAL = 9;
      DIRECTIONALITY_PARAGRAPH_SEPARATOR = 10;
      DIRECTIONALITY_SEGMENT_SEPARATOR = 11;
      DIRECTIONALITY_WHITESPACE = 12;
      DIRECTIONALITY_OTHER_NEUTRALS = 13;
      DIRECTIONALITY_LEFT_TO_RIGHT_EMBEDDING = 14;
      DIRECTIONALITY_LEFT_TO_RIGHT_OVERRIDE = 15;
      DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING = 16;
      DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE = 17;
      DIRECTIONALITY_POP_DIRECTIONAL_FORMAT = 18;
      MIN_HIGH_SURROGATE = 55296;
      MAX_HIGH_SURROGATE = 56319;
      MIN_LOW_SURROGATE = 56320;
      MAX_LOW_SURROGATE = 57343;
      MIN_SURROGATE = 55296;
      MAX_SURROGATE = 57343;
      MIN_SUPPLEMENTARY_CODE_POINT = 65536;
      MIN_CODE_POINT = 0;
      MAX_CODE_POINT = 1114111;
      SIZE = 16;
  public
    constructor create(para1: jchar); overload;
    class function valueOf(para1: jchar): JLCharacter; static; overload;
    function charValue(): jchar; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function toString(): JLString; overload; virtual;
    class function toString(para1: jchar): JLString; static; overload;
    class function isValidCodePoint(para1: jint): jboolean; static; overload;
    class function isSupplementaryCodePoint(para1: jint): jboolean; static; overload;
    class function isHighSurrogate(para1: jchar): jboolean; static; overload;
    class function isLowSurrogate(para1: jchar): jboolean; static; overload;
    class function isSurrogatePair(para1: jchar; para2: jchar): jboolean; static; overload;
    class function charCount(para1: jint): jint; static; overload;
    class function toCodePoint(para1: jchar; para2: jchar): jint; static; overload;
    class function codePointAt(para1: JLCharSequence; para2: jint): jint; static; overload;
    class function codePointAt(para1: Arr1jchar; para2: jint): jint; static; overload;
    class function codePointAt(var para1: array of jchar; para2: jint): jint; static; overload;
    class function codePointAt(para1: Arr1jchar; para2: jint; para3: jint): jint; static; overload;
    class function codePointAt(var para1: array of jchar; para2: jint; para3: jint): jint; static; overload;
    class function codePointBefore(para1: JLCharSequence; para2: jint): jint; static; overload;
    class function codePointBefore(para1: Arr1jchar; para2: jint): jint; static; overload;
    class function codePointBefore(var para1: array of jchar; para2: jint): jint; static; overload;
    class function codePointBefore(para1: Arr1jchar; para2: jint; para3: jint): jint; static; overload;
    class function codePointBefore(var para1: array of jchar; para2: jint; para3: jint): jint; static; overload;
    class function toChars(para1: jint; para2: Arr1jchar; para3: jint): jint; static; overload;
    class function toChars(para1: jint; var para2: array of jchar; para3: jint): jint; static; overload;
    class function toChars(para1: jint): Arr1jchar; static; overload;
    class function codePointCount(para1: JLCharSequence; para2: jint; para3: jint): jint; static; overload;
    class function codePointCount(para1: Arr1jchar; para2: jint; para3: jint): jint; static; overload;
    class function codePointCount(var para1: array of jchar; para2: jint; para3: jint): jint; static; overload;
    class function offsetByCodePoints(para1: JLCharSequence; para2: jint; para3: jint): jint; static; overload;
    class function offsetByCodePoints(para1: Arr1jchar; para2: jint; para3: jint; para4: jint; para5: jint): jint; static; overload;
    class function offsetByCodePoints(var para1: array of jchar; para2: jint; para3: jint; para4: jint; para5: jint): jint; static; overload;
    class function isLowerCase(para1: jchar): jboolean; static; overload;
    class function isLowerCase(para1: jint): jboolean; static; overload;
    class function isUpperCase(para1: jchar): jboolean; static; overload;
    class function isUpperCase(para1: jint): jboolean; static; overload;
    class function isTitleCase(para1: jchar): jboolean; static; overload;
    class function isTitleCase(para1: jint): jboolean; static; overload;
    class function isDigit(para1: jchar): jboolean; static; overload;
    class function isDigit(para1: jint): jboolean; static; overload;
    class function isDefined(para1: jchar): jboolean; static; overload;
    class function isDefined(para1: jint): jboolean; static; overload;
    class function isLetter(para1: jchar): jboolean; static; overload;
    class function isLetter(para1: jint): jboolean; static; overload;
    class function isLetterOrDigit(para1: jchar): jboolean; static; overload;
    class function isLetterOrDigit(para1: jint): jboolean; static; overload;
    class function isJavaLetter(para1: jchar): jboolean; static; overload;
    class function isJavaLetterOrDigit(para1: jchar): jboolean; static; overload;
    class function isJavaIdentifierStart(para1: jchar): jboolean; static; overload;
    class function isJavaIdentifierStart(para1: jint): jboolean; static; overload;
    class function isJavaIdentifierPart(para1: jchar): jboolean; static; overload;
    class function isJavaIdentifierPart(para1: jint): jboolean; static; overload;
    class function isUnicodeIdentifierStart(para1: jchar): jboolean; static; overload;
    class function isUnicodeIdentifierStart(para1: jint): jboolean; static; overload;
    class function isUnicodeIdentifierPart(para1: jchar): jboolean; static; overload;
    class function isUnicodeIdentifierPart(para1: jint): jboolean; static; overload;
    class function isIdentifierIgnorable(para1: jchar): jboolean; static; overload;
    class function isIdentifierIgnorable(para1: jint): jboolean; static; overload;
    class function toLowerCase(para1: jchar): jchar; static; overload;
    class function toLowerCase(para1: jint): jint; static; overload;
    class function toUpperCase(para1: jchar): jchar; static; overload;
    class function toUpperCase(para1: jint): jint; static; overload;
    class function toTitleCase(para1: jchar): jchar; static; overload;
    class function toTitleCase(para1: jint): jint; static; overload;
    class function digit(para1: jchar; para2: jint): jint; static; overload;
    class function digit(para1: jint; para2: jint): jint; static; overload;
    class function getNumericValue(para1: jchar): jint; static; overload;
    class function getNumericValue(para1: jint): jint; static; overload;
    class function isSpace(para1: jchar): jboolean; static; overload;
    class function isSpaceChar(para1: jchar): jboolean; static; overload;
    class function isSpaceChar(para1: jint): jboolean; static; overload;
    class function isWhitespace(para1: jchar): jboolean; static; overload;
    class function isWhitespace(para1: jint): jboolean; static; overload;
    class function isISOControl(para1: jchar): jboolean; static; overload;
    class function isISOControl(para1: jint): jboolean; static; overload;
    class function getType(para1: jchar): jint; static; overload;
    class function getType(para1: jint): jint; static; overload;
    class function forDigit(para1: jint; para2: jint): jchar; static; overload;
    class function getDirectionality(para1: jchar): jbyte; static; overload;
    class function getDirectionality(para1: jint): jbyte; static; overload;
    class function isMirrored(para1: jchar): jboolean; static; overload;
    class function isMirrored(para1: jint): jboolean; static; overload;
    function compareTo(para1: JLCharacter): jint; overload; virtual;
    class function reverseBytes(para1: jchar): jchar; static; overload;
    function compareTo(para1: JLObject): jint; overload; virtual;
  end;

  JLString = class sealed external 'java.lang' name 'String' (JLObject, JISerializable, JLComparable, JLCharSequence)
  public
    type
      InnerCaseInsensitiveComparator = class;
      Arr1InnerCaseInsensitiveComparator = array of InnerCaseInsensitiveComparator;
      Arr2InnerCaseInsensitiveComparator = array of Arr1InnerCaseInsensitiveComparator;
      Arr3InnerCaseInsensitiveComparator = array of Arr2InnerCaseInsensitiveComparator;
      InnerCaseInsensitiveComparator = class external 'java.lang' name 'String$CaseInsensitiveComparator' 
      end;

  public
    final class var
      fCASE_INSENSITIVE_ORDER: JUComparator; external name 'CASE_INSENSITIVE_ORDER';
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: Arr1jchar); overload;
    constructor create(const para1: array of jchar); overload;
    constructor create(para1: Arr1jchar; para2: jint; para3: jint); overload;
    constructor create(const para1: array of jchar; para2: jint; para3: jint); overload;
    constructor create(para1: Arr1jint; para2: jint; para3: jint); overload;
    constructor create(const para1: array of jint; para2: jint; para3: jint); overload;
    constructor create(para1: Arr1jbyte; para2: jint; para3: jint; para4: jint); overload;
    constructor create(const para1: array of jbyte; para2: jint; para3: jint; para4: jint); overload;
    constructor create(para1: Arr1jbyte; para2: jint); overload;
    constructor create(const para1: array of jbyte; para2: jint); overload;
    constructor create(para1: Arr1jbyte; para2: jint; para3: jint; para4: JLString); overload;  // throws java.io.UnsupportedEncodingException
    constructor create(const para1: array of jbyte; para2: jint; para3: jint; para4: JLString); overload;  // throws java.io.UnsupportedEncodingException
    constructor create(para1: Arr1jbyte; para2: jint; para3: jint; para4: JNCCharset); overload;
    constructor create(const para1: array of jbyte; para2: jint; para3: jint; para4: JNCCharset); overload;
    constructor create(para1: Arr1jbyte; para2: JLString); overload;  // throws java.io.UnsupportedEncodingException
    constructor create(const para1: array of jbyte; para2: JLString); overload;  // throws java.io.UnsupportedEncodingException
    constructor create(para1: Arr1jbyte; para2: JNCCharset); overload;
    constructor create(const para1: array of jbyte; para2: JNCCharset); overload;
    constructor create(para1: Arr1jbyte; para2: jint; para3: jint); overload;
    constructor create(const para1: array of jbyte; para2: jint; para3: jint); overload;
    constructor create(para1: Arr1jbyte); overload;
    constructor create(const para1: array of jbyte); overload;
    constructor create(para1: JLStringBuffer); overload;
    constructor create(para1: JLStringBuilder); overload;
    function length(): jint; overload; virtual;
    function isEmpty(): jboolean; overload; virtual;
    function charAt(para1: jint): jchar; overload; virtual;
    function codePointAt(para1: jint): jint; overload; virtual;
    function codePointBefore(para1: jint): jint; overload; virtual;
    function codePointCount(para1: jint; para2: jint): jint; overload; virtual;
    function offsetByCodePoints(para1: jint; para2: jint): jint; overload; virtual;
    procedure getChars(para1: jint; para2: jint; para3: Arr1jchar; para4: jint); overload; virtual;
    procedure getChars(para1: jint; para2: jint; var para3: array of jchar; para4: jint); overload; virtual;
    procedure getBytes(para1: jint; para2: jint; para3: Arr1jbyte; para4: jint); overload; virtual;
    procedure getBytes(para1: jint; para2: jint; var para3: array of jbyte; para4: jint); overload; virtual;
    function getBytes(para1: JLString): Arr1jbyte; overload; virtual;  // throws java.io.UnsupportedEncodingException
    function getBytes(para1: JNCCharset): Arr1jbyte; overload; virtual;
    function getBytes(): Arr1jbyte; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function contentEquals(para1: JLStringBuffer): jboolean; overload; virtual;
    function contentEquals(para1: JLCharSequence): jboolean; overload; virtual;
    function equalsIgnoreCase(para1: JLString): jboolean; overload; virtual;
    function compareTo(para1: JLString): jint; overload; virtual;
    function compareToIgnoreCase(para1: JLString): jint; overload; virtual;
    function regionMatches(para1: jint; para2: JLString; para3: jint; para4: jint): jboolean; overload; virtual;
    function regionMatches(para1: jboolean; para2: jint; para3: JLString; para4: jint; para5: jint): jboolean; overload; virtual;
    function startsWith(para1: JLString; para2: jint): jboolean; overload; virtual;
    function startsWith(para1: JLString): jboolean; overload; virtual;
    function endsWith(para1: JLString): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function indexOf(para1: jint): jint; overload; virtual;
    function indexOf(para1: jint; para2: jint): jint; overload; virtual;
    function lastIndexOf(para1: jint): jint; overload; virtual;
    function lastIndexOf(para1: jint; para2: jint): jint; overload; virtual;
    function indexOf(para1: JLString): jint; overload; virtual;
    function indexOf(para1: JLString; para2: jint): jint; overload; virtual;
    function lastIndexOf(para1: JLString): jint; overload; virtual;
    function lastIndexOf(para1: JLString; para2: jint): jint; overload; virtual;
    function substring(para1: jint): JLString; overload; virtual;
    function substring(para1: jint; para2: jint): JLString; overload; virtual;
    function subSequence(para1: jint; para2: jint): JLCharSequence; overload; virtual;
    function concat(para1: JLString): JLString; overload; virtual;
    function replace(para1: jchar; para2: jchar): JLString; overload; virtual;
    function matches(para1: JLString): jboolean; overload; virtual;
    function contains(para1: JLCharSequence): jboolean; overload; virtual;
    function replaceFirst(para1: JLString; para2: JLString): JLString; overload; virtual;
    function replaceAll(para1: JLString; para2: JLString): JLString; overload; virtual;
    function replace(para1: JLCharSequence; para2: JLCharSequence): JLString; overload; virtual;
    function split(para1: JLString; para2: jint): Arr1JLString; overload; virtual;
    function split(para1: JLString): Arr1JLString; overload; virtual;
    function toLowerCase(para1: JULocale): JLString; overload; virtual;
    function toLowerCase(): JLString; overload; virtual;
    function toUpperCase(para1: JULocale): JLString; overload; virtual;
    function toUpperCase(): JLString; overload; virtual;
    function trim(): JLString; overload; virtual;
    function toString(): JLString; overload; virtual;
    function toCharArray(): Arr1jchar; overload; virtual;
    class function format(para1: JLString; para2: Arr1JLObject): JLString; static; overload;
    class function format(para1: JLString; var para2: array of JLObject): JLString; static; overload;
    class function format(para1: JULocale; para2: JLString; para3: Arr1JLObject): JLString; static; overload;
    class function format(para1: JULocale; para2: JLString; var para3: array of JLObject): JLString; static; overload;
    class function valueOf(para1: JLObject): JLString; static; overload;
    class function valueOf(para1: Arr1jchar): JLString; static; overload;
    class function valueOf(var para1: array of jchar): JLString; static; overload;
    class function valueOf(para1: Arr1jchar; para2: jint; para3: jint): JLString; static; overload;
    class function valueOf(var para1: array of jchar; para2: jint; para3: jint): JLString; static; overload;
    class function copyValueOf(para1: Arr1jchar; para2: jint; para3: jint): JLString; static; overload;
    class function copyValueOf(var para1: array of jchar; para2: jint; para3: jint): JLString; static; overload;
    class function copyValueOf(para1: Arr1jchar): JLString; static; overload;
    class function copyValueOf(var para1: array of jchar): JLString; static; overload;
    class function valueOf(para1: jboolean): JLString; static; overload;
    class function valueOf(para1: jchar): JLString; static; overload;
    class function valueOf(para1: jint): JLString; static; overload;
    class function valueOf(para1: jlong): JLString; static; overload;
    class function valueOf(para1: jfloat): JLString; static; overload;
    class function valueOf(para1: jdouble): JLString; static; overload;
    function intern(): JLString; overload; virtual;
    function compareTo(para1: JLObject): jint; overload; virtual;
  end;

  JTCollator = class abstract external 'java.text' name 'Collator' (JLObject, JUComparator, JLCloneable)
  public
    type
      InnerCollatorGetter = class;
      Arr1InnerCollatorGetter = array of InnerCollatorGetter;
      Arr2InnerCollatorGetter = array of Arr1InnerCollatorGetter;
      Arr3InnerCollatorGetter = array of Arr2InnerCollatorGetter;
      InnerCollatorGetter = class external 'java.text' name 'Collator$CollatorGetter' 
      end;

  public
    const
      PRIMARY = 0;
      SECONDARY = 1;
      TERTIARY = 2;
      IDENTICAL = 3;
      NO_DECOMPOSITION = 0;
      CANONICAL_DECOMPOSITION = 1;
      FULL_DECOMPOSITION = 2;
  public
    class function getInstance(): JTCollator; static; overload;
    class function getInstance(para1: JULocale): JTCollator; static; overload;
    function compare(para1: JLString; para2: JLString): jint; overload; virtual; abstract;
    function compare(para1: JLObject; para2: JLObject): jint; overload; virtual;
    function getCollationKey(para1: JLString): JTCollationKey; overload; virtual; abstract;
    function equals(para1: JLString; para2: JLString): jboolean; overload; virtual;
    function getStrength(): jint; overload; virtual;
    procedure setStrength(para1: jint); overload; virtual;
    function getDecomposition(): jint; overload; virtual;
    procedure setDecomposition(para1: jint); overload; virtual;
    class function getAvailableLocales(): Arr1JULocale; static; overload;
    function clone(): JLObject; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual; abstract;
  strict protected
    constructor create(); overload;
  end;

  JLDouble = class sealed external 'java.lang' name 'Double' (JLNumber, JLComparable)
  public
    const
      POSITIVE_INFINITY = jdouble(1.0/0.0);
      NEGATIVE_INFINITY = jdouble(-1.0/0.0);
      NaN = jdouble(0.0/0.0);
      MAX_VALUE = jdouble(1.7976931348623157E308);
      MIN_NORMAL = jdouble(2.2250738585072014E-308);
      MIN_VALUE = jdouble(4.9E-324);
      MAX_EXPONENT = 1023;
      MIN_EXPONENT = -1022;
      SIZE = 64;
  public
    final class var
      fTYPE: JLClass; external name 'TYPE';
  public
    class function toString(para1: jdouble): JLString; static; overload;
    class function toHexString(para1: jdouble): JLString; static; overload;
    class function valueOf(para1: JLString): JLDouble; static; overload;  // throws java.lang.NumberFormatException
    class function valueOf(para1: jdouble): JLDouble; static; overload;
    class function parseDouble(para1: JLString): jdouble; static; overload;  // throws java.lang.NumberFormatException
    class function isNaN(para1: jdouble): jboolean; static; overload;
    class function isInfinite(para1: jdouble): jboolean; static; overload;
    constructor create(para1: jdouble); overload;
    constructor create(para1: JLString); overload;  // throws java.lang.NumberFormatException
    function isNaN(): jboolean; overload; virtual;
    function isInfinite(): jboolean; overload; virtual;
    function toString(): JLString; overload; virtual;
    function byteValue(): jbyte; overload; virtual;
    function shortValue(): jshort; overload; virtual;
    function intValue(): jint; overload; virtual;
    function longValue(): jlong; overload; virtual;
    function floatValue(): jfloat; overload; virtual;
    function doubleValue(): jdouble; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    class function doubleToLongBits(para1: jdouble): jlong; static; overload;
    class function doubleToRawLongBits(para1: jdouble): jlong; static; overload;
    class function longBitsToDouble(para1: jlong): jdouble; static; overload;
    function compareTo(para1: JLDouble): jint; overload; virtual;
    class function compare(para1: jdouble; para2: jdouble): jint; static; overload;
    function compareTo(para1: JLObject): jint; overload; virtual;
  end;

  JLFloat = class sealed external 'java.lang' name 'Float' (JLNumber, JLComparable)
  public
    const
      POSITIVE_INFINITY = jfloat(1.0/0.0);
      NEGATIVE_INFINITY = jfloat(-1.0/0.0);
      NaN = jfloat(0.0/0.0);
      MAX_VALUE = jfloat(3.4028235E38);
      MIN_NORMAL = jfloat(1.17549435E-38);
      MIN_VALUE = jfloat(1.4E-45);
      MAX_EXPONENT = 127;
      MIN_EXPONENT = -126;
      SIZE = 32;
  public
    final class var
      fTYPE: JLClass; external name 'TYPE';
  public
    class function toString(para1: jfloat): JLString; static; overload;
    class function toHexString(para1: jfloat): JLString; static; overload;
    class function valueOf(para1: JLString): JLFloat; static; overload;  // throws java.lang.NumberFormatException
    class function valueOf(para1: jfloat): JLFloat; static; overload;
    class function parseFloat(para1: JLString): jfloat; static; overload;  // throws java.lang.NumberFormatException
    class function isNaN(para1: jfloat): jboolean; static; overload;
    class function isInfinite(para1: jfloat): jboolean; static; overload;
    constructor create(para1: jfloat); overload;
    constructor create(para1: jdouble); overload;
    constructor create(para1: JLString); overload;  // throws java.lang.NumberFormatException
    function isNaN(): jboolean; overload; virtual;
    function isInfinite(): jboolean; overload; virtual;
    function toString(): JLString; overload; virtual;
    function byteValue(): jbyte; overload; virtual;
    function shortValue(): jshort; overload; virtual;
    function intValue(): jint; overload; virtual;
    function longValue(): jlong; overload; virtual;
    function floatValue(): jfloat; overload; virtual;
    function doubleValue(): jdouble; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    class function floatToIntBits(para1: jfloat): jint; static; overload;
    class function floatToRawIntBits(para1: jfloat): jint; static; overload;
    class function intBitsToFloat(para1: jint): jfloat; static; overload;
    function compareTo(para1: JLFloat): jint; overload; virtual;
    class function compare(para1: jfloat; para2: jfloat): jint; static; overload;
    function compareTo(para1: JLObject): jint; overload; virtual;
  end;

  JLError = class external 'java.lang' name 'Error' (JLThrowable)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
    constructor create(para1: JLThrowable); overload;
  end;

  JLException = class external 'java.lang' name 'Exception' (JLThrowable)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
    constructor create(para1: JLThrowable); overload;
  end;

  JLStringBuffer = class sealed external 'java.lang' name 'StringBuffer' (JLAbstractStringBuilder, JISerializable, JLCharSequence)
  public
    constructor create(); overload;
    constructor create(para1: jint); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLCharSequence); overload;
    function length(): jint; overload; virtual;
    function capacity(): jint; overload; virtual;
    procedure ensureCapacity(para1: jint); overload; virtual;
    procedure trimToSize(); overload; virtual;
    procedure setLength(para1: jint); overload; virtual;
    function charAt(para1: jint): jchar; overload; virtual;
    function codePointAt(para1: jint): jint; overload; virtual;
    function codePointBefore(para1: jint): jint; overload; virtual;
    function codePointCount(para1: jint; para2: jint): jint; overload; virtual;
    function offsetByCodePoints(para1: jint; para2: jint): jint; overload; virtual;
    procedure getChars(para1: jint; para2: jint; para3: Arr1jchar; para4: jint); overload; virtual;
    procedure getChars(para1: jint; para2: jint; var para3: array of jchar; para4: jint); overload; virtual;
    procedure setCharAt(para1: jint; para2: jchar); overload; virtual;
    function append(para1: JLObject): JLStringBuffer; overload; virtual;
    function append(para1: JLString): JLStringBuffer; overload; virtual;
    function append(para1: JLStringBuffer): JLStringBuffer; overload; virtual;
    function append(para1: JLCharSequence): JLStringBuffer; overload; virtual;
    function append(para1: JLCharSequence; para2: jint; para3: jint): JLStringBuffer; overload; virtual;
    function append(para1: Arr1jchar): JLStringBuffer; overload; virtual;
    function append(var para1: array of jchar): JLStringBuffer; overload; virtual;
    function append(para1: Arr1jchar; para2: jint; para3: jint): JLStringBuffer; overload; virtual;
    function append(var para1: array of jchar; para2: jint; para3: jint): JLStringBuffer; overload; virtual;
    function append(para1: jboolean): JLStringBuffer; overload; virtual;
    function append(para1: jchar): JLStringBuffer; overload; virtual;
    function append(para1: jint): JLStringBuffer; overload; virtual;
    function appendCodePoint(para1: jint): JLStringBuffer; overload; virtual;
    function append(para1: jlong): JLStringBuffer; overload; virtual;
    function append(para1: jfloat): JLStringBuffer; overload; virtual;
    function append(para1: jdouble): JLStringBuffer; overload; virtual;
    function delete(para1: jint; para2: jint): JLStringBuffer; overload; virtual;
    function deleteCharAt(para1: jint): JLStringBuffer; overload; virtual;
    function replace(para1: jint; para2: jint; para3: JLString): JLStringBuffer; overload; virtual;
    function substring(para1: jint): JLString; overload; virtual;
    function subSequence(para1: jint; para2: jint): JLCharSequence; overload; virtual;
    function substring(para1: jint; para2: jint): JLString; overload; virtual;
    function insert(para1: jint; para2: Arr1jchar; para3: jint; para4: jint): JLStringBuffer; overload; virtual;
    function insert(para1: jint; var para2: array of jchar; para3: jint; para4: jint): JLStringBuffer; overload; virtual;
    function insert(para1: jint; para2: JLObject): JLStringBuffer; overload; virtual;
    function insert(para1: jint; para2: JLString): JLStringBuffer; overload; virtual;
    function insert(para1: jint; para2: Arr1jchar): JLStringBuffer; overload; virtual;
    function insert(para1: jint; var para2: array of jchar): JLStringBuffer; overload; virtual;
    function insert(para1: jint; para2: JLCharSequence): JLStringBuffer; overload; virtual;
    function insert(para1: jint; para2: JLCharSequence; para3: jint; para4: jint): JLStringBuffer; overload; virtual;
    function insert(para1: jint; para2: jboolean): JLStringBuffer; overload; virtual;
    function insert(para1: jint; para2: jchar): JLStringBuffer; overload; virtual;
    function insert(para1: jint; para2: jint): JLStringBuffer; overload; virtual;
    function insert(para1: jint; para2: jlong): JLStringBuffer; overload; virtual;
    function insert(para1: jint; para2: jfloat): JLStringBuffer; overload; virtual;
    function insert(para1: jint; para2: jdouble): JLStringBuffer; overload; virtual;
    function indexOf(para1: JLString): jint; overload; virtual;
    function indexOf(para1: JLString; para2: jint): jint; overload; virtual;
    function lastIndexOf(para1: JLString): jint; overload; virtual;
    function lastIndexOf(para1: JLString; para2: jint): jint; overload; virtual;
    function reverse(): JLStringBuffer; overload; virtual;
    function toString(): JLString; overload; virtual;
    function reverse(): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jdouble): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jfloat): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jlong): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jint): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jchar): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jboolean): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLCharSequence; para3: jint; para4: jint): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLCharSequence): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: Arr1jchar): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; var para2: array of jchar): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLString): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLObject): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: Arr1jchar; para3: jint; para4: jint): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; var para2: array of jchar; para3: jint; para4: jint): JLAbstractStringBuilder; overload; virtual;
    function replace(para1: jint; para2: jint; para3: JLString): JLAbstractStringBuilder; overload; virtual;
    function deleteCharAt(para1: jint): JLAbstractStringBuilder; overload; virtual;
    function appendCodePoint(para1: jint): JLAbstractStringBuilder; overload; virtual;
    function delete(para1: jint; para2: jint): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jdouble): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jfloat): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jlong): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jint): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jchar): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jboolean): JLAbstractStringBuilder; overload; virtual;
    function append(para1: Arr1jchar; para2: jint; para3: jint): JLAbstractStringBuilder; overload; virtual;
    function append(var para1: array of jchar; para2: jint; para3: jint): JLAbstractStringBuilder; overload; virtual;
    function append(para1: Arr1jchar): JLAbstractStringBuilder; overload; virtual;
    function append(var para1: array of jchar): JLAbstractStringBuilder; overload; virtual;
    function append(para1: JLCharSequence; para2: jint; para3: jint): JLAbstractStringBuilder; overload; virtual;
    function append(para1: JLCharSequence): JLAbstractStringBuilder; overload; virtual;
    function append(para1: JLStringBuffer): JLAbstractStringBuilder; overload; virtual;
    function append(para1: JLString): JLAbstractStringBuilder; overload; virtual;
    function append(para1: JLObject): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jchar): JLAppendable; overload; virtual;  // throws java.io.IOException
    function append(para1: JLCharSequence; para2: jint; para3: jint): JLAppendable; overload; virtual;  // throws java.io.IOException
    function append(para1: JLCharSequence): JLAppendable; overload; virtual;  // throws java.io.IOException
  end;

  JLStringBuilder = class sealed external 'java.lang' name 'StringBuilder' (JLAbstractStringBuilder, JISerializable, JLCharSequence)
  public
    constructor create(); overload;
    constructor create(para1: jint); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLCharSequence); overload;
    function append(para1: JLObject): JLStringBuilder; overload; virtual;
    function append(para1: JLString): JLStringBuilder; overload; virtual;
    function append(para1: JLStringBuffer): JLStringBuilder; overload; virtual;
    function append(para1: JLCharSequence): JLStringBuilder; overload; virtual;
    function append(para1: JLCharSequence; para2: jint; para3: jint): JLStringBuilder; overload; virtual;
    function append(para1: Arr1jchar): JLStringBuilder; overload; virtual;
    function append(var para1: array of jchar): JLStringBuilder; overload; virtual;
    function append(para1: Arr1jchar; para2: jint; para3: jint): JLStringBuilder; overload; virtual;
    function append(var para1: array of jchar; para2: jint; para3: jint): JLStringBuilder; overload; virtual;
    function append(para1: jboolean): JLStringBuilder; overload; virtual;
    function append(para1: jchar): JLStringBuilder; overload; virtual;
    function append(para1: jint): JLStringBuilder; overload; virtual;
    function append(para1: jlong): JLStringBuilder; overload; virtual;
    function append(para1: jfloat): JLStringBuilder; overload; virtual;
    function append(para1: jdouble): JLStringBuilder; overload; virtual;
    function appendCodePoint(para1: jint): JLStringBuilder; overload; virtual;
    function delete(para1: jint; para2: jint): JLStringBuilder; overload; virtual;
    function deleteCharAt(para1: jint): JLStringBuilder; overload; virtual;
    function replace(para1: jint; para2: jint; para3: JLString): JLStringBuilder; overload; virtual;
    function insert(para1: jint; para2: Arr1jchar; para3: jint; para4: jint): JLStringBuilder; overload; virtual;
    function insert(para1: jint; var para2: array of jchar; para3: jint; para4: jint): JLStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLObject): JLStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLString): JLStringBuilder; overload; virtual;
    function insert(para1: jint; para2: Arr1jchar): JLStringBuilder; overload; virtual;
    function insert(para1: jint; var para2: array of jchar): JLStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLCharSequence): JLStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLCharSequence; para3: jint; para4: jint): JLStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jboolean): JLStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jchar): JLStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jint): JLStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jlong): JLStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jfloat): JLStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jdouble): JLStringBuilder; overload; virtual;
    function indexOf(para1: JLString): jint; overload; virtual;
    function indexOf(para1: JLString; para2: jint): jint; overload; virtual;
    function lastIndexOf(para1: JLString): jint; overload; virtual;
    function lastIndexOf(para1: JLString; para2: jint): jint; overload; virtual;
    function reverse(): JLStringBuilder; overload; virtual;
    function toString(): JLString; overload; virtual;
    function reverse(): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jdouble): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jfloat): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jlong): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jint): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jchar): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: jboolean): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLCharSequence; para3: jint; para4: jint): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLCharSequence): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: Arr1jchar): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; var para2: array of jchar): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLString): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: JLObject): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; para2: Arr1jchar; para3: jint; para4: jint): JLAbstractStringBuilder; overload; virtual;
    function insert(para1: jint; var para2: array of jchar; para3: jint; para4: jint): JLAbstractStringBuilder; overload; virtual;
    function substring(para1: jint; para2: jint): JLString; overload; virtual;
    function subSequence(para1: jint; para2: jint): JLCharSequence; overload; virtual;
    function substring(para1: jint): JLString; overload; virtual;
    function replace(para1: jint; para2: jint; para3: JLString): JLAbstractStringBuilder; overload; virtual;
    function deleteCharAt(para1: jint): JLAbstractStringBuilder; overload; virtual;
    function appendCodePoint(para1: jint): JLAbstractStringBuilder; overload; virtual;
    function delete(para1: jint; para2: jint): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jdouble): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jfloat): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jlong): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jint): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jchar): JLAbstractStringBuilder; overload; virtual;
    function append(para1: jboolean): JLAbstractStringBuilder; overload; virtual;
    function append(para1: Arr1jchar; para2: jint; para3: jint): JLAbstractStringBuilder; overload; virtual;
    function append(var para1: array of jchar; para2: jint; para3: jint): JLAbstractStringBuilder; overload; virtual;
    function append(para1: Arr1jchar): JLAbstractStringBuilder; overload; virtual;
    function append(var para1: array of jchar): JLAbstractStringBuilder; overload; virtual;
    function append(para1: JLCharSequence; para2: jint; para3: jint): JLAbstractStringBuilder; overload; virtual;
    function append(para1: JLCharSequence): JLAbstractStringBuilder; overload; virtual;
    function append(para1: JLStringBuffer): JLAbstractStringBuilder; overload; virtual;
    function append(para1: JLString): JLAbstractStringBuilder; overload; virtual;
    function append(para1: JLObject): JLAbstractStringBuilder; overload; virtual;
    procedure setCharAt(para1: jint; para2: jchar); overload; virtual;
    procedure getChars(para1: jint; para2: jint; para3: Arr1jchar; para4: jint); overload; virtual;
    procedure getChars(para1: jint; para2: jint; var para3: array of jchar; para4: jint); overload; virtual;
    function offsetByCodePoints(para1: jint; para2: jint): jint; overload; virtual;
    function codePointCount(para1: jint; para2: jint): jint; overload; virtual;
    function codePointBefore(para1: jint): jint; overload; virtual;
    function codePointAt(para1: jint): jint; overload; virtual;
    function charAt(para1: jint): jchar; overload; virtual;
    procedure setLength(para1: jint); overload; virtual;
    procedure trimToSize(); overload; virtual;
    procedure ensureCapacity(para1: jint); overload; virtual;
    function capacity(): jint; overload; virtual;
    function length(): jint; overload; virtual;
    function append(para1: jchar): JLAppendable; overload; virtual;  // throws java.io.IOException
    function append(para1: JLCharSequence; para2: jint; para3: jint): JLAppendable; overload; virtual;  // throws java.io.IOException
    function append(para1: JLCharSequence): JLAppendable; overload; virtual;  // throws java.io.IOException
  end;

  JLLinkageError = class external 'java.lang' name 'LinkageError' (JLError)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
  end;

  JLRuntimeException = class external 'java.lang' name 'RuntimeException' (JLException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
    constructor create(para1: JLThrowable); overload;
  end;

  JLIllegalArgumentException = class external 'java.lang' name 'IllegalArgumentException' (JLRuntimeException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
    constructor create(para1: JLThrowable); overload;
  end;

  JLIndexOutOfBoundsException = class external 'java.lang' name 'IndexOutOfBoundsException' (JLRuntimeException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
  end;

