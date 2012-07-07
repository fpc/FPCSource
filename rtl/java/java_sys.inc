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

  JLIterable = interface external 'java.lang' name 'Iterable' 
    function iterator(): JUIterator; overload;
  end;

  JLMath = class sealed external 'java.lang' name 'Math' (JLObject)
  public
    const
      E = jdouble(2.718281828459045);
      PI = jdouble(3.141592653589793);
  public
    class function sin(para1: jdouble): jdouble; static; overload;
    class function cos(para1: jdouble): jdouble; static; overload;
    class function tan(para1: jdouble): jdouble; static; overload;
    class function asin(para1: jdouble): jdouble; static; overload;
    class function acos(para1: jdouble): jdouble; static; overload;
    class function atan(para1: jdouble): jdouble; static; overload;
    class function toRadians(para1: jdouble): jdouble; static; overload;
    class function toDegrees(para1: jdouble): jdouble; static; overload;
    class function exp(para1: jdouble): jdouble; static; overload;
    class function log(para1: jdouble): jdouble; static; overload;
    class function log10(para1: jdouble): jdouble; static; overload;
    class function sqrt(para1: jdouble): jdouble; static; overload;
    class function cbrt(para1: jdouble): jdouble; static; overload;
    class function IEEEremainder(para1: jdouble; para2: jdouble): jdouble; static; overload;
    class function ceil(para1: jdouble): jdouble; static; overload;
    class function floor(para1: jdouble): jdouble; static; overload;
    class function rint(para1: jdouble): jdouble; static; overload;
    class function atan2(para1: jdouble; para2: jdouble): jdouble; static; overload;
    class function pow(para1: jdouble; para2: jdouble): jdouble; static; overload;
    class function round(para1: jfloat): jint; static; overload;
    class function round(para1: jdouble): jlong; static; overload;
    class function random(): jdouble; static; overload;
    class function abs(para1: jint): jint; static; overload;
    class function abs(para1: jlong): jlong; static; overload;
    class function abs(para1: jfloat): jfloat; static; overload;
    class function abs(para1: jdouble): jdouble; static; overload;
    class function max(para1: jint; para2: jint): jint; static; overload;
    class function max(para1: jlong; para2: jlong): jlong; static; overload;
    class function max(para1: jfloat; para2: jfloat): jfloat; static; overload;
    class function max(para1: jdouble; para2: jdouble): jdouble; static; overload;
    class function min(para1: jint; para2: jint): jint; static; overload;
    class function min(para1: jlong; para2: jlong): jlong; static; overload;
    class function min(para1: jfloat; para2: jfloat): jfloat; static; overload;
    class function min(para1: jdouble; para2: jdouble): jdouble; static; overload;
    class function ulp(para1: jdouble): jdouble; static; overload;
    class function ulp(para1: jfloat): jfloat; static; overload;
    class function signum(para1: jdouble): jdouble; static; overload;
    class function signum(para1: jfloat): jfloat; static; overload;
    class function sinh(para1: jdouble): jdouble; static; overload;
    class function cosh(para1: jdouble): jdouble; static; overload;
    class function tanh(para1: jdouble): jdouble; static; overload;
    class function hypot(para1: jdouble; para2: jdouble): jdouble; static; overload;
    class function expm1(para1: jdouble): jdouble; static; overload;
    class function log1p(para1: jdouble): jdouble; static; overload;
    class function copySign(para1: jdouble; para2: jdouble): jdouble; static; overload;
    class function copySign(para1: jfloat; para2: jfloat): jfloat; static; overload;
    class function getExponent(para1: jfloat): jint; static; overload;
    class function getExponent(para1: jdouble): jint; static; overload;
    class function nextAfter(para1: jdouble; para2: jdouble): jdouble; static; overload;
    class function nextAfter(para1: jfloat; para2: jdouble): jfloat; static; overload;
    class function nextUp(para1: jdouble): jdouble; static; overload;
    class function nextUp(para1: jfloat): jfloat; static; overload;
    class function scalb(para1: jdouble; para2: jint): jdouble; static; overload;
    class function scalb(para1: jfloat; para2: jint): jfloat; static; overload;
  end;

  JLReadable = interface external 'java.lang' name 'Readable' 
    function read(para1: JNCharBuffer): jint; overload;  // throws java.io.IOException
  end;

  JLRuntime = class external 'java.lang' name 'Runtime' (JLObject)
  public
    class function getRuntime(): JLRuntime; static; overload;
    procedure exit(para1: jint); overload; virtual;
    procedure addShutdownHook(para1: JLThread); overload; virtual;
    function removeShutdownHook(para1: JLThread): jboolean; overload; virtual;
    procedure halt(para1: jint); overload; virtual;
    class procedure runFinalizersOnExit(para1: jboolean); static; overload;
    function exec(para1: JLString): JLProcess; overload; virtual;  // throws java.io.IOException
    function exec(para1: JLString; para2: Arr1JLString): JLProcess; overload; virtual;  // throws java.io.IOException
    function exec(para1: JLString; var para2: array of JLString): JLProcess; overload; virtual;  // throws java.io.IOException
    function exec(para1: JLString; para2: Arr1JLString; para3: JIFile): JLProcess; overload; virtual;  // throws java.io.IOException
    function exec(para1: JLString; var para2: array of JLString; para3: JIFile): JLProcess; overload; virtual;  // throws java.io.IOException
    function exec(para1: Arr1JLString): JLProcess; overload; virtual;  // throws java.io.IOException
    function exec(var para1: array of JLString): JLProcess; overload; virtual;  // throws java.io.IOException
    function exec(para1: Arr1JLString; para2: Arr1JLString): JLProcess; overload; virtual;  // throws java.io.IOException
    function exec(var para1: array of JLString; var para2: array of JLString): JLProcess; overload; virtual;  // throws java.io.IOException
    function exec(para1: Arr1JLString; para2: Arr1JLString; para3: JIFile): JLProcess; overload; virtual;  // throws java.io.IOException
    function exec(var para1: array of JLString; var para2: array of JLString; para3: JIFile): JLProcess; overload; virtual;  // throws java.io.IOException
    function availableProcessors(): jint; overload; virtual;
    function freeMemory(): jlong; overload; virtual;
    function totalMemory(): jlong; overload; virtual;
    function maxMemory(): jlong; overload; virtual;
    procedure gc(); overload; virtual;
    procedure runFinalization(); overload; virtual;
    procedure traceInstructions(para1: jboolean); overload; virtual;
    procedure traceMethodCalls(para1: jboolean); overload; virtual;
    procedure load(para1: JLString); overload; virtual;
    procedure loadLibrary(para1: JLString); overload; virtual;
    function getLocalizedInputStream(para1: JIInputStream): JIInputStream; overload; virtual;
    function getLocalizedOutputStream(para1: JIOutputStream): JIOutputStream; overload; virtual;
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

  JLThreadLocal = class external 'java.lang' name 'ThreadLocal' (JLObject)
  public
    type
      InnerThreadLocalMap = class;
      Arr1InnerThreadLocalMap = array of InnerThreadLocalMap;
      Arr2InnerThreadLocalMap = array of Arr1InnerThreadLocalMap;
      Arr3InnerThreadLocalMap = array of Arr2InnerThreadLocalMap;
      InnerEntry = class;
      Arr1InnerEntry = array of InnerEntry;
      Arr2InnerEntry = array of Arr1InnerEntry;
      Arr3InnerEntry = array of Arr2InnerEntry;
      InnerThreadLocalMap = class external 'java.lang' name 'ThreadLocalMap' 
      public
        type
          InnerEntry = class;
          Arr1InnerEntry = array of InnerEntry;
          Arr2InnerEntry = array of Arr1InnerEntry;
          Arr3InnerEntry = array of Arr2InnerEntry;
          InnerEntry = class external 'java.lang' name 'Entry' 
          end;

      end;

      InnerEntry = class external 'java.lang' name 'Entry' 
      end;

  strict protected
    function initialValue(): JLObject; overload; virtual;
  public
    constructor create(); overload;
    function get(): JLObject; overload; virtual;
    procedure &set(para1: JLObject); overload; virtual;
    procedure remove(); overload; virtual;
  end;

  JLRAnnotatedElement = interface external 'java.lang.reflect' name 'AnnotatedElement' 
    function isAnnotationPresent(para1: JLClass): jboolean; overload;
    function getAnnotation(para1: JLClass): JLAAnnotation; overload;
    function getAnnotations(): Arr1JLAAnnotation; overload;
    function getDeclaredAnnotations(): Arr1JLAAnnotation; overload;
  end;

  JLRArray = class sealed external 'java.lang.reflect' name 'Array' (JLObject)
  public
    class function newInstance(para1: JLClass; para2: jint): JLObject; static; overload;  // throws java.lang.NegativeArraySizeException
    class function newInstance(para1: JLClass; para2: Arr1jint): JLObject; static; overload;  // throws java.lang.IllegalArgumentException, java.lang.NegativeArraySizeException
    class function newInstance(para1: JLClass; const para2: array of jint): JLObject; static; overload;  // throws java.lang.IllegalArgumentException, java.lang.NegativeArraySizeException
    class function getLength(para1: JLObject): jint; static; overload;  // throws java.lang.IllegalArgumentException
    class function get(para1: JLObject; para2: jint): JLObject; static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class function getBoolean(para1: JLObject; para2: jint): jboolean; static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class function getByte(para1: JLObject; para2: jint): jbyte; static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class function getChar(para1: JLObject; para2: jint): jchar; static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class function getShort(para1: JLObject; para2: jint): jshort; static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class function getInt(para1: JLObject; para2: jint): jint; static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class function getLong(para1: JLObject; para2: jint): jlong; static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class function getFloat(para1: JLObject; para2: jint): jfloat; static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class function getDouble(para1: JLObject; para2: jint): jdouble; static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class procedure &set(para1: JLObject; para2: jint; para3: JLObject); static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class procedure setBoolean(para1: JLObject; para2: jint; para3: jboolean); static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class procedure setByte(para1: JLObject; para2: jint; para3: jbyte); static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class procedure setChar(para1: JLObject; para2: jint; para3: jchar); static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class procedure setShort(para1: JLObject; para2: jint; para3: jshort); static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class procedure setInt(para1: JLObject; para2: jint; para3: jint); static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class procedure setLong(para1: JLObject; para2: jint; para3: jlong); static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class procedure setFloat(para1: JLObject; para2: jint; para3: jfloat); static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
    class procedure setDouble(para1: JLObject; para2: jint; para3: jdouble); static; overload;  // throws java.lang.IllegalArgumentException, java.lang.ArrayIndexOutOfBoundsException
  end;

  JLRGenericDeclaration = interface external 'java.lang.reflect' name 'GenericDeclaration' 
    function getTypeParameters(): Arr1JLRTypeVariable; overload;
  end;

  JLRMember = interface external 'java.lang.reflect' name 'Member' 
    const
      &PUBLIC = 0;
      DECLARED = 1;
    function getDeclaringClass(): JLClass; overload;
    function getName(): JLString; overload;
    function getModifiers(): jint; overload;
    function isSynthetic(): jboolean; overload;
  end;

  JLRType = interface external 'java.lang.reflect' name 'Type' 
  end;

  JNBuffer = class abstract external 'java.nio' name 'Buffer' (JLObject)
  public
    function capacity(): jint; overload; virtual; final;
    function position(): jint; overload; virtual; final;
    function position(para1: jint): JNBuffer; overload; virtual; final;
    function limit(): jint; overload; virtual; final;
    function limit(para1: jint): JNBuffer; overload; virtual; final;
    function mark(): JNBuffer; overload; virtual; final;
    function reset(): JNBuffer; overload; virtual; final;
    function clear(): JNBuffer; overload; virtual; final;
    function flip(): JNBuffer; overload; virtual; final;
    function rewind(): JNBuffer; overload; virtual; final;
    function remaining(): jint; overload; virtual; final;
    function hasRemaining(): jboolean; overload; virtual; final;
    function isReadOnly(): jboolean; overload; virtual; abstract;
    function hasArray(): jboolean; overload; virtual; abstract;
    function &array(): JLObject; overload; virtual; abstract;
    function arrayOffset(): jint; overload; virtual; abstract;
    function isDirect(): jboolean; overload; virtual; abstract;
  end;

  JNCCharsetDecoder = class abstract external 'java.nio.charset' name 'CharsetDecoder' (JLObject)
  strict protected
    constructor create(para1: JNCCharset; para2: jfloat; para3: jfloat); overload;
  public
    function charset(): JNCCharset; overload; virtual; final;
    function replacement(): JLString; overload; virtual; final;
    function replaceWith(para1: JLString): JNCCharsetDecoder; overload; virtual; final;
  strict protected
    procedure implReplaceWith(para1: JLString); overload; virtual;
  public
    function malformedInputAction(): JNCCodingErrorAction; overload; virtual;
    function onMalformedInput(para1: JNCCodingErrorAction): JNCCharsetDecoder; overload; virtual; final;
  strict protected
    procedure implOnMalformedInput(para1: JNCCodingErrorAction); overload; virtual;
  public
    function unmappableCharacterAction(): JNCCodingErrorAction; overload; virtual;
    function onUnmappableCharacter(para1: JNCCodingErrorAction): JNCCharsetDecoder; overload; virtual; final;
  strict protected
    procedure implOnUnmappableCharacter(para1: JNCCodingErrorAction); overload; virtual;
  public
    function averageCharsPerByte(): jfloat; overload; virtual; final;
    function maxCharsPerByte(): jfloat; overload; virtual; final;
    function decode(para1: JNByteBuffer; para2: JNCharBuffer; para3: jboolean): JNCCoderResult; overload; virtual; final;
    function flush(para1: JNCharBuffer): JNCCoderResult; overload; virtual; final;
  strict protected
    function implFlush(para1: JNCharBuffer): JNCCoderResult; overload; virtual;
  public
    function reset(): JNCCharsetDecoder; overload; virtual; final;
  strict protected
    procedure implReset(); overload; virtual;
    function decodeLoop(para1: JNByteBuffer; para2: JNCharBuffer): JNCCoderResult; overload; virtual; abstract;
  public
    function decode(para1: JNByteBuffer): JNCharBuffer; overload; virtual; final;  // throws java.nio.charset.CharacterCodingException
    function isAutoDetecting(): jboolean; overload; virtual;
    function isCharsetDetected(): jboolean; overload; virtual;
    function detectedCharset(): JNCCharset; overload; virtual;
  end;

  JNCCharsetEncoder = class abstract external 'java.nio.charset' name 'CharsetEncoder' (JLObject)
  strict protected
    constructor create(para1: JNCCharset; para2: jfloat; para3: jfloat; para4: Arr1jbyte); overload;
    constructor create(para1: JNCCharset; para2: jfloat; para3: jfloat; const para4: array of jbyte); overload;
    constructor create(para1: JNCCharset; para2: jfloat; para3: jfloat); overload;
  public
    function charset(): JNCCharset; overload; virtual; final;
    function replacement(): Arr1jbyte; overload; virtual; final;
    function replaceWith(para1: Arr1jbyte): JNCCharsetEncoder; overload; virtual; final;
    function replaceWith(var para1: array of jbyte): JNCCharsetEncoder; overload; virtual; final;
  strict protected
    procedure implReplaceWith(para1: Arr1jbyte); overload; virtual;
    procedure implReplaceWith(var para1: array of jbyte); overload; virtual;
  public
    function isLegalReplacement(para1: Arr1jbyte): jboolean; overload; virtual;
    function isLegalReplacement(var para1: array of jbyte): jboolean; overload; virtual;
    function malformedInputAction(): JNCCodingErrorAction; overload; virtual;
    function onMalformedInput(para1: JNCCodingErrorAction): JNCCharsetEncoder; overload; virtual; final;
  strict protected
    procedure implOnMalformedInput(para1: JNCCodingErrorAction); overload; virtual;
  public
    function unmappableCharacterAction(): JNCCodingErrorAction; overload; virtual;
    function onUnmappableCharacter(para1: JNCCodingErrorAction): JNCCharsetEncoder; overload; virtual; final;
  strict protected
    procedure implOnUnmappableCharacter(para1: JNCCodingErrorAction); overload; virtual;
  public
    function averageBytesPerChar(): jfloat; overload; virtual; final;
    function maxBytesPerChar(): jfloat; overload; virtual; final;
    function encode(para1: JNCharBuffer; para2: JNByteBuffer; para3: jboolean): JNCCoderResult; overload; virtual; final;
    function flush(para1: JNByteBuffer): JNCCoderResult; overload; virtual; final;
  strict protected
    function implFlush(para1: JNByteBuffer): JNCCoderResult; overload; virtual;
  public
    function reset(): JNCCharsetEncoder; overload; virtual; final;
  strict protected
    procedure implReset(); overload; virtual;
    function encodeLoop(para1: JNCharBuffer; para2: JNByteBuffer): JNCCoderResult; overload; virtual; abstract;
  public
    function encode(para1: JNCharBuffer): JNByteBuffer; overload; virtual; final;  // throws java.nio.charset.CharacterCodingException
    function canEncode(para1: jchar): jboolean; overload; virtual;
    function canEncode(para1: JLCharSequence): jboolean; overload; virtual;
  end;

  JNCCoderResult = class external 'java.nio.charset' name 'CoderResult' (JLObject)
  public
    type
      InnerCache = class;
      Arr1InnerCache = array of InnerCache;
      Arr2InnerCache = array of Arr1InnerCache;
      Arr3InnerCache = array of Arr2InnerCache;
      InnerCache = class abstract external 'java.nio.charset' name 'Cache' 
      end;

  public
    final class var
      fUNDERFLOW: JNCCoderResult; external name 'UNDERFLOW';
      fOVERFLOW: JNCCoderResult; external name 'OVERFLOW';
  public
    function toString(): JLString; overload; virtual;
    function isUnderflow(): jboolean; overload; virtual;
    function isOverflow(): jboolean; overload; virtual;
    function isError(): jboolean; overload; virtual;
    function isMalformed(): jboolean; overload; virtual;
    function isUnmappable(): jboolean; overload; virtual;
    function length(): jint; overload; virtual;
    class function malformedForLength(para1: jint): JNCCoderResult; static; overload;
    class function unmappableForLength(para1: jint): JNCCoderResult; static; overload;
    procedure throwException(); overload; virtual;  // throws java.nio.charset.CharacterCodingException
  end;

  JNCCodingErrorAction = class external 'java.nio.charset' name 'CodingErrorAction' (JLObject)
  public
    final class var
      fIGNORE: JNCCodingErrorAction; external name 'IGNORE';
      fREPLACE: JNCCodingErrorAction; external name 'REPLACE';
      fREPORT: JNCCodingErrorAction; external name 'REPORT';
  public
    function toString(): JLString; overload; virtual;
  end;

  JNCSCharsetProvider = class abstract external 'java.nio.charset.spi' name 'CharsetProvider' (JLObject)
  strict protected
    constructor create(); overload;
  public
    function charsets(): JUIterator; overload; virtual; abstract;
    function charsetForName(para1: JLString): JNCCharset; overload; virtual; abstract;
  end;

  JUArrays = class external 'java.util' name 'Arrays' (JLObject)
  public
    type
      InnerArrayList = class;
      Arr1InnerArrayList = array of InnerArrayList;
      Arr2InnerArrayList = array of Arr1InnerArrayList;
      Arr3InnerArrayList = array of Arr2InnerArrayList;
      InnerArrayList = class external 'java.util' name 'ArrayList' 
      end;

  public
    class procedure sort(para1: Arr1jlong); static; overload;
    class procedure sort(var para1: array of jlong); static; overload;
    class procedure sort(para1: Arr1jlong; para2: jint; para3: jint); static; overload;
    class procedure sort(var para1: array of jlong; para2: jint; para3: jint); static; overload;
    class procedure sort(para1: Arr1jint); static; overload;
    class procedure sort(var para1: array of jint); static; overload;
    class procedure sort(para1: Arr1jint; para2: jint; para3: jint); static; overload;
    class procedure sort(var para1: array of jint; para2: jint; para3: jint); static; overload;
    class procedure sort(para1: Arr1jshort); static; overload;
    class procedure sort(var para1: array of jshort); static; overload;
    class procedure sort(para1: Arr1jshort; para2: jint; para3: jint); static; overload;
    class procedure sort(var para1: array of jshort; para2: jint; para3: jint); static; overload;
    class procedure sort(para1: Arr1jchar); static; overload;
    class procedure sort(var para1: array of jchar); static; overload;
    class procedure sort(para1: Arr1jchar; para2: jint; para3: jint); static; overload;
    class procedure sort(var para1: array of jchar; para2: jint; para3: jint); static; overload;
    class procedure sort(para1: Arr1jbyte); static; overload;
    class procedure sort(var para1: array of jbyte); static; overload;
    class procedure sort(para1: Arr1jbyte; para2: jint; para3: jint); static; overload;
    class procedure sort(var para1: array of jbyte; para2: jint; para3: jint); static; overload;
    class procedure sort(para1: Arr1jdouble); static; overload;
    class procedure sort(var para1: array of jdouble); static; overload;
    class procedure sort(para1: Arr1jdouble; para2: jint; para3: jint); static; overload;
    class procedure sort(var para1: array of jdouble; para2: jint; para3: jint); static; overload;
    class procedure sort(para1: Arr1jfloat); static; overload;
    class procedure sort(var para1: array of jfloat); static; overload;
    class procedure sort(para1: Arr1jfloat; para2: jint; para3: jint); static; overload;
    class procedure sort(var para1: array of jfloat; para2: jint; para3: jint); static; overload;
    class procedure sort(para1: Arr1JLObject); static; overload;
    class procedure sort(var para1: array of JLObject); static; overload;
    class procedure sort(para1: Arr1JLObject; para2: jint; para3: jint); static; overload;
    class procedure sort(var para1: array of JLObject; para2: jint; para3: jint); static; overload;
    class procedure sort(para1: Arr1JLObject; para2: JUComparator); static; overload;
    class procedure sort(var para1: array of JLObject; para2: JUComparator); static; overload;
    class procedure sort(para1: Arr1JLObject; para2: jint; para3: jint; para4: JUComparator); static; overload;
    class procedure sort(var para1: array of JLObject; para2: jint; para3: jint; para4: JUComparator); static; overload;
    class function binarySearch(para1: Arr1jlong; para2: jlong): jint; static; overload;
    class function binarySearch(var para1: array of jlong; para2: jlong): jint; static; overload;
    class function binarySearch(para1: Arr1jlong; para2: jint; para3: jint; para4: jlong): jint; static; overload;
    class function binarySearch(var para1: array of jlong; para2: jint; para3: jint; para4: jlong): jint; static; overload;
    class function binarySearch(para1: Arr1jint; para2: jint): jint; static; overload;
    class function binarySearch(var para1: array of jint; para2: jint): jint; static; overload;
    class function binarySearch(para1: Arr1jint; para2: jint; para3: jint; para4: jint): jint; static; overload;
    class function binarySearch(var para1: array of jint; para2: jint; para3: jint; para4: jint): jint; static; overload;
    class function binarySearch(para1: Arr1jshort; para2: jshort): jint; static; overload;
    class function binarySearch(var para1: array of jshort; para2: jshort): jint; static; overload;
    class function binarySearch(para1: Arr1jshort; para2: jint; para3: jint; para4: jshort): jint; static; overload;
    class function binarySearch(var para1: array of jshort; para2: jint; para3: jint; para4: jshort): jint; static; overload;
    class function binarySearch(para1: Arr1jchar; para2: jchar): jint; static; overload;
    class function binarySearch(var para1: array of jchar; para2: jchar): jint; static; overload;
    class function binarySearch(para1: Arr1jchar; para2: jint; para3: jint; para4: jchar): jint; static; overload;
    class function binarySearch(var para1: array of jchar; para2: jint; para3: jint; para4: jchar): jint; static; overload;
    class function binarySearch(para1: Arr1jbyte; para2: jbyte): jint; static; overload;
    class function binarySearch(var para1: array of jbyte; para2: jbyte): jint; static; overload;
    class function binarySearch(para1: Arr1jbyte; para2: jint; para3: jint; para4: jbyte): jint; static; overload;
    class function binarySearch(var para1: array of jbyte; para2: jint; para3: jint; para4: jbyte): jint; static; overload;
    class function binarySearch(para1: Arr1jdouble; para2: jdouble): jint; static; overload;
    class function binarySearch(var para1: array of jdouble; para2: jdouble): jint; static; overload;
    class function binarySearch(para1: Arr1jdouble; para2: jint; para3: jint; para4: jdouble): jint; static; overload;
    class function binarySearch(var para1: array of jdouble; para2: jint; para3: jint; para4: jdouble): jint; static; overload;
    class function binarySearch(para1: Arr1jfloat; para2: jfloat): jint; static; overload;
    class function binarySearch(var para1: array of jfloat; para2: jfloat): jint; static; overload;
    class function binarySearch(para1: Arr1jfloat; para2: jint; para3: jint; para4: jfloat): jint; static; overload;
    class function binarySearch(var para1: array of jfloat; para2: jint; para3: jint; para4: jfloat): jint; static; overload;
    class function binarySearch(para1: Arr1JLObject; para2: JLObject): jint; static; overload;
    class function binarySearch(var para1: array of JLObject; para2: JLObject): jint; static; overload;
    class function binarySearch(para1: Arr1JLObject; para2: jint; para3: jint; para4: JLObject): jint; static; overload;
    class function binarySearch(var para1: array of JLObject; para2: jint; para3: jint; para4: JLObject): jint; static; overload;
    class function binarySearch(para1: Arr1JLObject; para2: JLObject; para3: JUComparator): jint; static; overload;
    class function binarySearch(var para1: array of JLObject; para2: JLObject; para3: JUComparator): jint; static; overload;
    class function binarySearch(para1: Arr1JLObject; para2: jint; para3: jint; para4: JLObject; para5: JUComparator): jint; static; overload;
    class function binarySearch(var para1: array of JLObject; para2: jint; para3: jint; para4: JLObject; para5: JUComparator): jint; static; overload;
    class function equals(para1: Arr1jlong; para2: Arr1jlong): jboolean; static; overload;
    class function equals(var para1: array of jlong; var para2: array of jlong): jboolean; static; overload;
    class function equals(para1: Arr1jint; para2: Arr1jint): jboolean; static; overload;
    class function equals(var para1: array of jint; var para2: array of jint): jboolean; static; overload;
    class function equals(para1: Arr1jshort; para2: Arr1jshort): jboolean; static; overload;
    class function equals(var para1: array of jshort; var para2: array of jshort): jboolean; static; overload;
    class function equals(para1: Arr1jchar; para2: Arr1jchar): jboolean; static; overload;
    class function equals(var para1: array of jchar; var para2: array of jchar): jboolean; static; overload;
    class function equals(para1: Arr1jbyte; para2: Arr1jbyte): jboolean; static; overload;
    class function equals(var para1: array of jbyte; var para2: array of jbyte): jboolean; static; overload;
    class function equals(para1: Arr1jboolean; para2: Arr1jboolean): jboolean; static; overload;
    class function equals(var para1: array of jboolean; var para2: array of jboolean): jboolean; static; overload;
    class function equals(para1: Arr1jdouble; para2: Arr1jdouble): jboolean; static; overload;
    class function equals(var para1: array of jdouble; var para2: array of jdouble): jboolean; static; overload;
    class function equals(para1: Arr1jfloat; para2: Arr1jfloat): jboolean; static; overload;
    class function equals(var para1: array of jfloat; var para2: array of jfloat): jboolean; static; overload;
    class function equals(para1: Arr1JLObject; para2: Arr1JLObject): jboolean; static; overload;
    class function equals(var para1: array of JLObject; var para2: array of JLObject): jboolean; static; overload;
    class procedure fill(para1: Arr1jlong; para2: jlong); static; overload;
    class procedure fill(var para1: array of jlong; para2: jlong); static; overload;
    class procedure fill(para1: Arr1jlong; para2: jint; para3: jint; para4: jlong); static; overload;
    class procedure fill(var para1: array of jlong; para2: jint; para3: jint; para4: jlong); static; overload;
    class procedure fill(para1: Arr1jint; para2: jint); static; overload;
    class procedure fill(var para1: array of jint; para2: jint); static; overload;
    class procedure fill(para1: Arr1jint; para2: jint; para3: jint; para4: jint); static; overload;
    class procedure fill(var para1: array of jint; para2: jint; para3: jint; para4: jint); static; overload;
    class procedure fill(para1: Arr1jshort; para2: jshort); static; overload;
    class procedure fill(var para1: array of jshort; para2: jshort); static; overload;
    class procedure fill(para1: Arr1jshort; para2: jint; para3: jint; para4: jshort); static; overload;
    class procedure fill(var para1: array of jshort; para2: jint; para3: jint; para4: jshort); static; overload;
    class procedure fill(para1: Arr1jchar; para2: jchar); static; overload;
    class procedure fill(var para1: array of jchar; para2: jchar); static; overload;
    class procedure fill(para1: Arr1jchar; para2: jint; para3: jint; para4: jchar); static; overload;
    class procedure fill(var para1: array of jchar; para2: jint; para3: jint; para4: jchar); static; overload;
    class procedure fill(para1: Arr1jbyte; para2: jbyte); static; overload;
    class procedure fill(var para1: array of jbyte; para2: jbyte); static; overload;
    class procedure fill(para1: Arr1jbyte; para2: jint; para3: jint; para4: jbyte); static; overload;
    class procedure fill(var para1: array of jbyte; para2: jint; para3: jint; para4: jbyte); static; overload;
    class procedure fill(para1: Arr1jboolean; para2: jboolean); static; overload;
    class procedure fill(var para1: array of jboolean; para2: jboolean); static; overload;
    class procedure fill(para1: Arr1jboolean; para2: jint; para3: jint; para4: jboolean); static; overload;
    class procedure fill(var para1: array of jboolean; para2: jint; para3: jint; para4: jboolean); static; overload;
    class procedure fill(para1: Arr1jdouble; para2: jdouble); static; overload;
    class procedure fill(var para1: array of jdouble; para2: jdouble); static; overload;
    class procedure fill(para1: Arr1jdouble; para2: jint; para3: jint; para4: jdouble); static; overload;
    class procedure fill(var para1: array of jdouble; para2: jint; para3: jint; para4: jdouble); static; overload;
    class procedure fill(para1: Arr1jfloat; para2: jfloat); static; overload;
    class procedure fill(var para1: array of jfloat; para2: jfloat); static; overload;
    class procedure fill(para1: Arr1jfloat; para2: jint; para3: jint; para4: jfloat); static; overload;
    class procedure fill(var para1: array of jfloat; para2: jint; para3: jint; para4: jfloat); static; overload;
    class procedure fill(para1: Arr1JLObject; para2: JLObject); static; overload;
    class procedure fill(var para1: array of JLObject; para2: JLObject); static; overload;
    class procedure fill(para1: Arr1JLObject; para2: jint; para3: jint; para4: JLObject); static; overload;
    class procedure fill(var para1: array of JLObject; para2: jint; para3: jint; para4: JLObject); static; overload;
    class function copyOf(para1: Arr1JLObject; para2: jint): Arr1JLObject; static; overload;
    class function copyOf(var para1: array of JLObject; para2: jint): Arr1JLObject; static; overload;
    class function copyOf(para1: Arr1JLObject; para2: jint; para3: JLClass): Arr1JLObject; static; overload;
    class function copyOf(var para1: array of JLObject; para2: jint; para3: JLClass): Arr1JLObject; static; overload;
    class function copyOf(para1: Arr1jbyte; para2: jint): Arr1jbyte; static; overload;
    class function copyOf(var para1: array of jbyte; para2: jint): Arr1jbyte; static; overload;
    class function copyOf(para1: Arr1jshort; para2: jint): Arr1jshort; static; overload;
    class function copyOf(var para1: array of jshort; para2: jint): Arr1jshort; static; overload;
    class function copyOf(para1: Arr1jint; para2: jint): Arr1jint; static; overload;
    class function copyOf(var para1: array of jint; para2: jint): Arr1jint; static; overload;
    class function copyOf(para1: Arr1jlong; para2: jint): Arr1jlong; static; overload;
    class function copyOf(var para1: array of jlong; para2: jint): Arr1jlong; static; overload;
    class function copyOf(para1: Arr1jchar; para2: jint): Arr1jchar; static; overload;
    class function copyOf(var para1: array of jchar; para2: jint): Arr1jchar; static; overload;
    class function copyOf(para1: Arr1jfloat; para2: jint): Arr1jfloat; static; overload;
    class function copyOf(var para1: array of jfloat; para2: jint): Arr1jfloat; static; overload;
    class function copyOf(para1: Arr1jdouble; para2: jint): Arr1jdouble; static; overload;
    class function copyOf(var para1: array of jdouble; para2: jint): Arr1jdouble; static; overload;
    class function copyOf(para1: Arr1jboolean; para2: jint): Arr1jboolean; static; overload;
    class function copyOf(var para1: array of jboolean; para2: jint): Arr1jboolean; static; overload;
    class function copyOfRange(para1: Arr1JLObject; para2: jint; para3: jint): Arr1JLObject; static; overload;
    class function copyOfRange(var para1: array of JLObject; para2: jint; para3: jint): Arr1JLObject; static; overload;
    class function copyOfRange(para1: Arr1JLObject; para2: jint; para3: jint; para4: JLClass): Arr1JLObject; static; overload;
    class function copyOfRange(var para1: array of JLObject; para2: jint; para3: jint; para4: JLClass): Arr1JLObject; static; overload;
    class function copyOfRange(para1: Arr1jbyte; para2: jint; para3: jint): Arr1jbyte; static; overload;
    class function copyOfRange(var para1: array of jbyte; para2: jint; para3: jint): Arr1jbyte; static; overload;
    class function copyOfRange(para1: Arr1jshort; para2: jint; para3: jint): Arr1jshort; static; overload;
    class function copyOfRange(var para1: array of jshort; para2: jint; para3: jint): Arr1jshort; static; overload;
    class function copyOfRange(para1: Arr1jint; para2: jint; para3: jint): Arr1jint; static; overload;
    class function copyOfRange(var para1: array of jint; para2: jint; para3: jint): Arr1jint; static; overload;
    class function copyOfRange(para1: Arr1jlong; para2: jint; para3: jint): Arr1jlong; static; overload;
    class function copyOfRange(var para1: array of jlong; para2: jint; para3: jint): Arr1jlong; static; overload;
    class function copyOfRange(para1: Arr1jchar; para2: jint; para3: jint): Arr1jchar; static; overload;
    class function copyOfRange(var para1: array of jchar; para2: jint; para3: jint): Arr1jchar; static; overload;
    class function copyOfRange(para1: Arr1jfloat; para2: jint; para3: jint): Arr1jfloat; static; overload;
    class function copyOfRange(var para1: array of jfloat; para2: jint; para3: jint): Arr1jfloat; static; overload;
    class function copyOfRange(para1: Arr1jdouble; para2: jint; para3: jint): Arr1jdouble; static; overload;
    class function copyOfRange(var para1: array of jdouble; para2: jint; para3: jint): Arr1jdouble; static; overload;
    class function copyOfRange(para1: Arr1jboolean; para2: jint; para3: jint): Arr1jboolean; static; overload;
    class function copyOfRange(var para1: array of jboolean; para2: jint; para3: jint): Arr1jboolean; static; overload;
    class function asList(para1: Arr1JLObject): JUList; static; overload;
    class function asList(const para1: array of JLObject): JUList; static; overload;
    class function hashCode(para1: Arr1jlong): jint; static; overload;
    class function hashCode(var para1: array of jlong): jint; static; overload;
    class function hashCode(para1: Arr1jint): jint; static; overload;
    class function hashCode(var para1: array of jint): jint; static; overload;
    class function hashCode(para1: Arr1jshort): jint; static; overload;
    class function hashCode(var para1: array of jshort): jint; static; overload;
    class function hashCode(para1: Arr1jchar): jint; static; overload;
    class function hashCode(var para1: array of jchar): jint; static; overload;
    class function hashCode(para1: Arr1jbyte): jint; static; overload;
    class function hashCode(var para1: array of jbyte): jint; static; overload;
    class function hashCode(para1: Arr1jboolean): jint; static; overload;
    class function hashCode(var para1: array of jboolean): jint; static; overload;
    class function hashCode(para1: Arr1jfloat): jint; static; overload;
    class function hashCode(var para1: array of jfloat): jint; static; overload;
    class function hashCode(para1: Arr1jdouble): jint; static; overload;
    class function hashCode(var para1: array of jdouble): jint; static; overload;
    class function hashCode(para1: Arr1JLObject): jint; static; overload;
    class function hashCode(var para1: array of JLObject): jint; static; overload;
    class function deepHashCode(para1: Arr1JLObject): jint; static; overload;
    class function deepHashCode(var para1: array of JLObject): jint; static; overload;
    class function deepEquals(para1: Arr1JLObject; para2: Arr1JLObject): jboolean; static; overload;
    class function deepEquals(var para1: array of JLObject; var para2: array of JLObject): jboolean; static; overload;
    class function toString(para1: Arr1jlong): JLString; static; overload;
    class function toString(var para1: array of jlong): JLString; static; overload;
    class function toString(para1: Arr1jint): JLString; static; overload;
    class function toString(var para1: array of jint): JLString; static; overload;
    class function toString(para1: Arr1jshort): JLString; static; overload;
    class function toString(var para1: array of jshort): JLString; static; overload;
    class function toString(para1: Arr1jchar): JLString; static; overload;
    class function toString(var para1: array of jchar): JLString; static; overload;
    class function toString(para1: Arr1jbyte): JLString; static; overload;
    class function toString(var para1: array of jbyte): JLString; static; overload;
    class function toString(para1: Arr1jboolean): JLString; static; overload;
    class function toString(var para1: array of jboolean): JLString; static; overload;
    class function toString(para1: Arr1jfloat): JLString; static; overload;
    class function toString(var para1: array of jfloat): JLString; static; overload;
    class function toString(para1: Arr1jdouble): JLString; static; overload;
    class function toString(var para1: array of jdouble): JLString; static; overload;
    class function toString(para1: Arr1JLObject): JLString; static; overload;
    class function toString(var para1: array of JLObject): JLString; static; overload;
    class function deepToString(para1: Arr1JLObject): JLString; static; overload;
    class function deepToString(var para1: array of JLObject): JLString; static; overload;
  end;

  JUComparator = interface external 'java.util' name 'Comparator' 
    function compare(para1: JLObject; para2: JLObject): jint; overload;
    function equals(para1: JLObject): jboolean; overload;
  end;

  JUIterator = interface external 'java.util' name 'Iterator' 
    function hasNext(): jboolean; overload;
    function next(): JLObject; overload;
    procedure remove(); overload;
  end;

  JUMap = interface external 'java.util' name 'Map' 
    type
      InnerEntry = interface;
      Arr1InnerEntry = array of InnerEntry;
      Arr2InnerEntry = array of Arr1InnerEntry;
      Arr3InnerEntry = array of Arr2InnerEntry;
      InnerEntry = interface external 'java.util' name 'Entry' 
        function getKey(): JLObject; overload;
        function getValue(): JLObject; overload;
        function setValue(para1: JLObject): JLObject; overload;
        function equals(para1: JLObject): jboolean; overload;
        function hashCode(): jint; overload;
      end;

    function size(): jint; overload;
    function isEmpty(): jboolean; overload;
    function containsKey(para1: JLObject): jboolean; overload;
    function containsValue(para1: JLObject): jboolean; overload;
    function get(para1: JLObject): JLObject; overload;
    function put(para1: JLObject; para2: JLObject): JLObject; overload;
    function remove(para1: JLObject): JLObject; overload;
    procedure putAll(para1: JUMap); overload;
    procedure clear(); overload;
    function keySet(): JUSet; overload;
    function values(): JUCollection; overload;
    function entrySet(): JUSet; overload;
    function equals(para1: JLObject): jboolean; overload;
    function hashCode(): jint; overload;
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

  JUBitSet = class external 'java.util' name 'BitSet' (JLObject, JLCloneable, JISerializable)
  public
    constructor create(); overload;
    constructor create(para1: jint); overload;
    procedure flip(para1: jint); overload; virtual;
    procedure flip(para1: jint; para2: jint); overload; virtual;
    procedure &set(para1: jint); overload; virtual;
    procedure &set(para1: jint; para2: jboolean); overload; virtual;
    procedure &set(para1: jint; para2: jint); overload; virtual;
    procedure &set(para1: jint; para2: jint; para3: jboolean); overload; virtual;
    procedure clear(para1: jint); overload; virtual;
    procedure clear(para1: jint; para2: jint); overload; virtual;
    procedure clear(); overload; virtual;
    function get(para1: jint): jboolean; overload; virtual;
    function get(para1: jint; para2: jint): JUBitSet; overload; virtual;
    function nextSetBit(para1: jint): jint; overload; virtual;
    function nextClearBit(para1: jint): jint; overload; virtual;
    function length(): jint; overload; virtual;
    function isEmpty(): jboolean; overload; virtual;
    function intersects(para1: JUBitSet): jboolean; overload; virtual;
    function cardinality(): jint; overload; virtual;
    procedure &and(para1: JUBitSet); overload; virtual;
    procedure &or(para1: JUBitSet); overload; virtual;
    procedure &xor(para1: JUBitSet); overload; virtual;
    procedure andNot(para1: JUBitSet); overload; virtual;
    function hashCode(): jint; overload; virtual;
    function size(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function clone(): JLObject; overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  JLBoolean = class sealed external 'java.lang' name 'Boolean' (JLObject, JISerializable, JLComparable)
  public
    final class var
      fTRUE: JLBoolean; external name 'TRUE';
      fFALSE: JLBoolean; external name 'FALSE';
      fTYPE: JLClass; external name 'TYPE';
  public
    constructor create(para1: jboolean); overload;
    constructor create(para1: JLString); overload;
    class function parseBoolean(para1: JLString): jboolean; static; overload;
    function booleanValue(): jboolean; overload; virtual;
    class function valueOf(para1: jboolean): JLBoolean; static; overload;
    class function valueOf(para1: JLString): JLBoolean; static; overload;
    class function toString(para1: jboolean): JLString; static; overload;
    function toString(): JLString; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    class function getBoolean(para1: JLString): jboolean; static; overload;
    function compareTo(para1: JLBoolean): jint; overload; virtual;
    function compareTo(para1: JLObject): jint; overload; virtual;
  end;

  JLCharacter = class sealed external 'java.lang' name 'Character' (JLObject, JISerializable, JLComparable)
  public
    type
      InnerCharacterCache = class;
      Arr1InnerCharacterCache = array of InnerCharacterCache;
      Arr2InnerCharacterCache = array of Arr1InnerCharacterCache;
      Arr3InnerCharacterCache = array of Arr2InnerCharacterCache;
      InnerCharacterCache = class external 'java.lang' name 'CharacterCache' 
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
      InnerSubset = class external 'java.lang' name 'Subset' (JLObject)
      strict protected
        constructor create(para1: JLString); overload;
      public
        function equals(para1: JLObject): jboolean; overload; virtual; final;
        function hashCode(): jint; overload; virtual; final;
        function toString(): JLString; overload; virtual; final;
      end;

      InnerUnicodeBlock = class sealed external 'java.lang' name 'UnicodeBlock' (InnerSubset)
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
        class function forName(para1: JLString): JLCharacter.InnerUnicodeBlock; static; overload; final;
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

  JLEnum = class abstract external 'java.lang' name 'Enum' (JLObject, JLComparable, JISerializable)
  public
    function name(): JLString; overload; virtual; final;
    function ordinal(): jint; overload; virtual; final;
  strict protected
    constructor create(para1: JLString; para2: jint); overload;
  public
    function toString(): JLString; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual; final;
    function hashCode(): jint; overload; virtual; final;
  strict protected
    function clone(): JLObject; overload; virtual; final;  // throws java.lang.CloneNotSupportedException
  public
    function compareTo(para1: JLEnum): jint; overload; virtual; final;
    function getDeclaringClass(): JLClass; overload; virtual; final;
    class function valueOf(para1: JLClass; para2: JLString): JLEnum; static; overload;
  strict protected
    procedure finalize(); overload; virtual; final;
  public
    function compareTo(para1: JLObject): jint; overload; virtual;
  end;

  JLString = class sealed external 'java.lang' name 'String' (JLObject, JISerializable, JLComparable, JLCharSequence)
  public
    type
      InnerCaseInsensitiveComparator = class;
      Arr1InnerCaseInsensitiveComparator = array of InnerCaseInsensitiveComparator;
      Arr2InnerCaseInsensitiveComparator = array of Arr1InnerCaseInsensitiveComparator;
      Arr3InnerCaseInsensitiveComparator = array of Arr2InnerCaseInsensitiveComparator;
      InnerCaseInsensitiveComparator = class external 'java.lang' name 'CaseInsensitiveComparator' 
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
    class function format(para1: JLString; const para2: array of JLObject): JLString; static; overload;
    class function format(para1: JULocale; para2: JLString; para3: Arr1JLObject): JLString; static; overload;
    class function format(para1: JULocale; para2: JLString; const para3: array of JLObject): JLString; static; overload;
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

  JNCCharset = class abstract external 'java.nio.charset' name 'Charset' (JLObject, JLComparable)
  public
    class function isSupported(para1: JLString): jboolean; static; overload;
    class function forName(para1: JLString): JNCCharset; static; overload;
    class function availableCharsets(): JUSortedMap; static; overload;
    class function defaultCharset(): JNCCharset; static; overload;
  strict protected
    constructor create(para1: JLString; para2: Arr1JLString); overload;
    constructor create(para1: JLString; const para2: array of JLString); overload;
  public
    function name(): JLString; overload; virtual; final;
    function aliases(): JUSet; overload; virtual; final;
    function displayName(): JLString; overload; virtual;
    function isRegistered(): jboolean; overload; virtual; final;
    function displayName(para1: JULocale): JLString; overload; virtual;
    function contains(para1: JNCCharset): jboolean; overload; virtual; abstract;
    function newDecoder(): JNCCharsetDecoder; overload; virtual; abstract;
    function newEncoder(): JNCCharsetEncoder; overload; virtual; abstract;
    function canEncode(): jboolean; overload; virtual;
    function decode(para1: JNByteBuffer): JNCharBuffer; overload; virtual; final;
    function encode(para1: JNCharBuffer): JNByteBuffer; overload; virtual; final;
    function encode(para1: JLString): JNByteBuffer; overload; virtual; final;
    function compareTo(para1: JNCCharset): jint; overload; virtual; final;
    function hashCode(): jint; overload; virtual; final;
    function equals(para1: JLObject): jboolean; overload; virtual; final;
    function toString(): JLString; overload; virtual; final;
    function compareTo(para1: JLObject): jint; overload; virtual;
  end;

  JUCalendar = class abstract external 'java.util' name 'Calendar' (JLObject, JISerializable, JLCloneable, JLComparable)
  public
    type
      InnerCalendarAccessControlContext = class;
      Arr1InnerCalendarAccessControlContext = array of InnerCalendarAccessControlContext;
      Arr2InnerCalendarAccessControlContext = array of Arr1InnerCalendarAccessControlContext;
      Arr3InnerCalendarAccessControlContext = array of Arr2InnerCalendarAccessControlContext;
      InnerCalendarAccessControlContext = class external 'java.util' name 'CalendarAccessControlContext' 
      end;

  public
    const
      ERA = 0;
      YEAR = 1;
      MONTH = 2;
      WEEK_OF_YEAR = 3;
      WEEK_OF_MONTH = 4;
      DATE = 5;
      DAY_OF_MONTH = 5;
      DAY_OF_YEAR = 6;
      DAY_OF_WEEK = 7;
      DAY_OF_WEEK_IN_MONTH = 8;
      AM_PM = 9;
      HOUR = 10;
      HOUR_OF_DAY = 11;
      MINUTE = 12;
      SECOND = 13;
      MILLISECOND = 14;
      ZONE_OFFSET = 15;
      DST_OFFSET = 16;
      FIELD_COUNT = 17;
      SUNDAY = 1;
      MONDAY = 2;
      TUESDAY = 3;
      WEDNESDAY = 4;
      THURSDAY = 5;
      FRIDAY = 6;
      SATURDAY = 7;
      JANUARY = 0;
      FEBRUARY = 1;
      MARCH = 2;
      APRIL = 3;
      MAY = 4;
      JUNE = 5;
      JULY = 6;
      AUGUST = 7;
      SEPTEMBER = 8;
      OCTOBER = 9;
      NOVEMBER = 10;
      DECEMBER = 11;
      UNDECIMBER = 12;
      AM = 0;
      PM = 1;
      ALL_STYLES = 0;
      SHORT = 1;
      LONG = 2;
  strict protected
    var
      ffields: Arr1jint; external name 'fields';
      fisSet: Arr1jboolean; external name 'isSet';
      ftime: jlong; external name 'time';
      fisTimeSet: jboolean; external name 'isTimeSet';
      fareFieldsSet: jboolean; external name 'areFieldsSet';
  strict protected
    constructor create(); overload;
    constructor create(para1: JUTimeZone; para2: JULocale); overload;
  public
    class function getInstance(): JUCalendar; static; overload;
    class function getInstance(para1: JUTimeZone): JUCalendar; static; overload;
    class function getInstance(para1: JULocale): JUCalendar; static; overload;
    class function getInstance(para1: JUTimeZone; para2: JULocale): JUCalendar; static; overload;
    class function getAvailableLocales(): Arr1JULocale; static; overload;
  strict protected
    procedure computeTime(); overload; virtual; abstract;
    procedure computeFields(); overload; virtual; abstract;
  public
    function getTime(): JUDate; overload; virtual; final;
    procedure setTime(para1: JUDate); overload; virtual; final;
    function getTimeInMillis(): jlong; overload; virtual;
    procedure setTimeInMillis(para1: jlong); overload; virtual;
    function get(para1: jint): jint; overload; virtual;
  strict protected
    function internalGet(para1: jint): jint; overload; virtual; final;
  public
    procedure &set(para1: jint; para2: jint); overload; virtual;
    procedure &set(para1: jint; para2: jint; para3: jint); overload; virtual; final;
    procedure &set(para1: jint; para2: jint; para3: jint; para4: jint; para5: jint); overload; virtual; final;
    procedure &set(para1: jint; para2: jint; para3: jint; para4: jint; para5: jint; para6: jint); overload; virtual; final;
    procedure clear(); overload; virtual; final;
    procedure clear(para1: jint); overload; virtual; final;
    function isSet(para1: jint): jboolean; overload; virtual; final;
    function getDisplayName(para1: jint; para2: jint; para3: JULocale): JLString; overload; virtual;
    function getDisplayNames(para1: jint; para2: jint; para3: JULocale): JUMap; overload; virtual;
  strict protected
    procedure complete(); overload; virtual;
  public
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function before(para1: JLObject): jboolean; overload; virtual;
    function after(para1: JLObject): jboolean; overload; virtual;
    function compareTo(para1: JUCalendar): jint; overload; virtual;
    procedure add(para1: jint; para2: jint); overload; virtual; abstract;
    procedure roll(para1: jint; para2: jboolean); overload; virtual; abstract;
    procedure roll(para1: jint; para2: jint); overload; virtual;
    procedure setTimeZone(para1: JUTimeZone); overload; virtual;
    function getTimeZone(): JUTimeZone; overload; virtual;
    procedure setLenient(para1: jboolean); overload; virtual;
    function isLenient(): jboolean; overload; virtual;
    procedure setFirstDayOfWeek(para1: jint); overload; virtual;
    function getFirstDayOfWeek(): jint; overload; virtual;
    procedure setMinimalDaysInFirstWeek(para1: jint); overload; virtual;
    function getMinimalDaysInFirstWeek(): jint; overload; virtual;
    function getMinimum(para1: jint): jint; overload; virtual; abstract;
    function getMaximum(para1: jint): jint; overload; virtual; abstract;
    function getGreatestMinimum(para1: jint): jint; overload; virtual; abstract;
    function getLeastMaximum(para1: jint): jint; overload; virtual; abstract;
    function getActualMinimum(para1: jint): jint; overload; virtual;
    function getActualMaximum(para1: jint): jint; overload; virtual;
    function clone(): JLObject; overload; virtual;
    function toString(): JLString; overload; virtual;
    function compareTo(para1: JLObject): jint; overload; virtual;
  end;

  JUCollection = interface external 'java.util' name 'Collection' (JLIterable)
    function size(): jint; overload;
    function isEmpty(): jboolean; overload;
    function contains(para1: JLObject): jboolean; overload;
    function iterator(): JUIterator; overload;
    function toArray(): Arr1JLObject; overload;
    function toArray(para1: Arr1JLObject): Arr1JLObject; overload;
    function toArray(var para1: array of JLObject): Arr1JLObject; overload;
    function add(para1: JLObject): jboolean; overload;
    function remove(para1: JLObject): jboolean; overload;
    function containsAll(para1: JUCollection): jboolean; overload;
    function addAll(para1: JUCollection): jboolean; overload;
    function removeAll(para1: JUCollection): jboolean; overload;
    function retainAll(para1: JUCollection): jboolean; overload;
    procedure clear(); overload;
    function equals(para1: JLObject): jboolean; overload;
    function hashCode(): jint; overload;
  end;

  JLRAccessibleObject = class external 'java.lang.reflect' name 'AccessibleObject' (JLObject, JLRAnnotatedElement)
  public
    class procedure setAccessible(para1: Arr1JLRAccessibleObject; para2: jboolean); static; overload;  // throws java.lang.SecurityException
    class procedure setAccessible(var para1: array of JLRAccessibleObject; para2: jboolean); static; overload;  // throws java.lang.SecurityException
    procedure setAccessible(para1: jboolean); overload; virtual;  // throws java.lang.SecurityException
    function isAccessible(): jboolean; overload; virtual;
  strict protected
    constructor create(); overload;
  public
    function getAnnotation(para1: JLClass): JLAAnnotation; overload; virtual;
    function isAnnotationPresent(para1: JLClass): jboolean; overload; virtual;
    function getAnnotations(): Arr1JLAAnnotation; overload; virtual;
    function getDeclaredAnnotations(): Arr1JLAAnnotation; overload; virtual;
  end;

  JLClass = class sealed external 'java.lang' name 'Class' (JLObject, JISerializable, JLRGenericDeclaration, JLRType, JLRAnnotatedElement)
  public
    type
      InnerEnclosingMethodInfo = class;
      Arr1InnerEnclosingMethodInfo = array of InnerEnclosingMethodInfo;
      Arr2InnerEnclosingMethodInfo = array of Arr1InnerEnclosingMethodInfo;
      Arr3InnerEnclosingMethodInfo = array of Arr2InnerEnclosingMethodInfo;
      InnerMethodArray = class;
      Arr1InnerMethodArray = array of InnerMethodArray;
      Arr2InnerMethodArray = array of Arr1InnerMethodArray;
      Arr3InnerMethodArray = array of Arr2InnerMethodArray;
      InnerEnclosingMethodInfo = class sealed external 'java.lang' name 'EnclosingMethodInfo' 
      end;

      InnerMethodArray = class external 'java.lang' name 'MethodArray' 
      end;

  public
    function toString(): JLString; overload; virtual;
    class function forName(para1: JLString): JLClass; static; overload;  // throws java.lang.ClassNotFoundException
    class function forName(para1: JLString; para2: jboolean; para3: JLClassLoader): JLClass; static; overload;  // throws java.lang.ClassNotFoundException
    function newInstance(): JLObject; overload; virtual;  // throws java.lang.InstantiationException, java.lang.IllegalAccessException
    function isInstance(para1: JLObject): jboolean; overload; virtual;
    function isAssignableFrom(para1: JLClass): jboolean; overload; virtual;
    function isInterface(): jboolean; overload; virtual;
    function isArray(): jboolean; overload; virtual;
    function isPrimitive(): jboolean; overload; virtual;
    function isAnnotation(): jboolean; overload; virtual;
    function isSynthetic(): jboolean; overload; virtual;
    function getName(): JLString; overload; virtual;
    function getClassLoader(): JLClassLoader; overload; virtual;
    function getTypeParameters(): Arr1JLRTypeVariable; overload; virtual;
    function getSuperclass(): JLClass; overload; virtual;
    function getGenericSuperclass(): JLRType; overload; virtual;
    function getPackage(): JLPackage; overload; virtual;
    function getInterfaces(): Arr1JLClass; overload; virtual;
    function getGenericInterfaces(): Arr1JLRType; overload; virtual;
    function getComponentType(): JLClass; overload; virtual;
    function getModifiers(): jint; overload; virtual;
    function getSigners(): Arr1JLObject; overload; virtual;
    function getEnclosingMethod(): JLRMethod; overload; virtual;
    function getEnclosingConstructor(): JLRConstructor; overload; virtual;
    function getDeclaringClass(): JLClass; overload; virtual;
    function getEnclosingClass(): JLClass; overload; virtual;
    function getSimpleName(): JLString; overload; virtual;
    function getCanonicalName(): JLString; overload; virtual;
    function isAnonymousClass(): jboolean; overload; virtual;
    function isLocalClass(): jboolean; overload; virtual;
    function isMemberClass(): jboolean; overload; virtual;
    function getClasses(): Arr1JLClass; overload; virtual;
    function getFields(): Arr1JLRField; overload; virtual;  // throws java.lang.SecurityException
    function getMethods(): Arr1JLRMethod; overload; virtual;  // throws java.lang.SecurityException
    function getConstructors(): Arr1JLRConstructor; overload; virtual;  // throws java.lang.SecurityException
    function getField(para1: JLString): JLRField; overload; virtual;  // throws java.lang.NoSuchFieldException, java.lang.SecurityException
    function getMethod(para1: JLString; para2: Arr1JLClass): JLRMethod; overload; virtual;  // throws java.lang.NoSuchMethodException, java.lang.SecurityException
    function getMethod(para1: JLString; const para2: array of JLClass): JLRMethod; overload; virtual;  // throws java.lang.NoSuchMethodException, java.lang.SecurityException
    function getConstructor(para1: Arr1JLClass): JLRConstructor; overload; virtual;  // throws java.lang.NoSuchMethodException, java.lang.SecurityException
    function getConstructor(const para1: array of JLClass): JLRConstructor; overload; virtual;  // throws java.lang.NoSuchMethodException, java.lang.SecurityException
    function getDeclaredClasses(): Arr1JLClass; overload; virtual;  // throws java.lang.SecurityException
    function getDeclaredFields(): Arr1JLRField; overload; virtual;  // throws java.lang.SecurityException
    function getDeclaredMethods(): Arr1JLRMethod; overload; virtual;  // throws java.lang.SecurityException
    function getDeclaredConstructors(): Arr1JLRConstructor; overload; virtual;  // throws java.lang.SecurityException
    function getDeclaredField(para1: JLString): JLRField; overload; virtual;  // throws java.lang.NoSuchFieldException, java.lang.SecurityException
    function getDeclaredMethod(para1: JLString; para2: Arr1JLClass): JLRMethod; overload; virtual;  // throws java.lang.NoSuchMethodException, java.lang.SecurityException
    function getDeclaredMethod(para1: JLString; const para2: array of JLClass): JLRMethod; overload; virtual;  // throws java.lang.NoSuchMethodException, java.lang.SecurityException
    function getDeclaredConstructor(para1: Arr1JLClass): JLRConstructor; overload; virtual;  // throws java.lang.NoSuchMethodException, java.lang.SecurityException
    function getDeclaredConstructor(const para1: array of JLClass): JLRConstructor; overload; virtual;  // throws java.lang.NoSuchMethodException, java.lang.SecurityException
    function getResourceAsStream(para1: JLString): JIInputStream; overload; virtual;
    function getResource(para1: JLString): JNURL; overload; virtual;
    function getProtectionDomain(): JSProtectionDomain; overload; virtual;
    function desiredAssertionStatus(): jboolean; overload; virtual;
    function isEnum(): jboolean; overload; virtual;
    function getEnumConstants(): Arr1JLObject; overload; virtual;
    function cast(para1: JLObject): JLObject; overload; virtual;
    function asSubclass(para1: JLClass): JLClass; overload; virtual;
    function getAnnotation(para1: JLClass): JLAAnnotation; overload; virtual;
    function isAnnotationPresent(para1: JLClass): jboolean; overload; virtual;
    function getAnnotations(): Arr1JLAAnnotation; overload; virtual;
    function getDeclaredAnnotations(): Arr1JLAAnnotation; overload; virtual;
  end;

  JNByteBuffer = class abstract external 'java.nio' name 'ByteBuffer' (JNBuffer, JLComparable)
  public
    class function allocateDirect(para1: jint): JNByteBuffer; static; overload;
    class function allocate(para1: jint): JNByteBuffer; static; overload;
    class function wrap(para1: Arr1jbyte; para2: jint; para3: jint): JNByteBuffer; static; overload;
    class function wrap(var para1: array of jbyte; para2: jint; para3: jint): JNByteBuffer; static; overload;
    class function wrap(para1: Arr1jbyte): JNByteBuffer; static; overload;
    class function wrap(var para1: array of jbyte): JNByteBuffer; static; overload;
    function slice(): JNByteBuffer; overload; virtual; abstract;
    function duplicate(): JNByteBuffer; overload; virtual; abstract;
    function asReadOnlyBuffer(): JNByteBuffer; overload; virtual; abstract;
    function get(): jbyte; overload; virtual; abstract;
    function put(para1: jbyte): JNByteBuffer; overload; virtual; abstract;
    function get(para1: jint): jbyte; overload; virtual; abstract;
    function put(para1: jint; para2: jbyte): JNByteBuffer; overload; virtual; abstract;
    function get(para1: Arr1jbyte; para2: jint; para3: jint): JNByteBuffer; overload; virtual;
    function get(var para1: array of jbyte; para2: jint; para3: jint): JNByteBuffer; overload; virtual;
    function get(para1: Arr1jbyte): JNByteBuffer; overload; virtual;
    function get(var para1: array of jbyte): JNByteBuffer; overload; virtual;
    function put(para1: JNByteBuffer): JNByteBuffer; overload; virtual;
    function put(para1: Arr1jbyte; para2: jint; para3: jint): JNByteBuffer; overload; virtual;
    function put(var para1: array of jbyte; para2: jint; para3: jint): JNByteBuffer; overload; virtual;
    function put(para1: Arr1jbyte): JNByteBuffer; overload; virtual; final;
    function put(var para1: array of jbyte): JNByteBuffer; overload; virtual; final;
    function hasArray(): jboolean; overload; virtual; final;
    function &array(): Arr1jbyte; overload; virtual; final;
    function arrayOffset(): jint; overload; virtual; final;
    function compact(): JNByteBuffer; overload; virtual; abstract;
    function isDirect(): jboolean; overload; virtual; abstract;
    function toString(): JLString; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function compareTo(para1: JNByteBuffer): jint; overload; virtual;
    function order(): JNByteOrder; overload; virtual; final;
    function order(para1: JNByteOrder): JNByteBuffer; overload; virtual; final;
    function getChar(): jchar; overload; virtual; abstract;
    function putChar(para1: jchar): JNByteBuffer; overload; virtual; abstract;
    function getChar(para1: jint): jchar; overload; virtual; abstract;
    function putChar(para1: jint; para2: jchar): JNByteBuffer; overload; virtual; abstract;
    function asCharBuffer(): JNCharBuffer; overload; virtual; abstract;
    function getShort(): jshort; overload; virtual; abstract;
    function putShort(para1: jshort): JNByteBuffer; overload; virtual; abstract;
    function getShort(para1: jint): jshort; overload; virtual; abstract;
    function putShort(para1: jint; para2: jshort): JNByteBuffer; overload; virtual; abstract;
    function asShortBuffer(): JNShortBuffer; overload; virtual; abstract;
    function getInt(): jint; overload; virtual; abstract;
    function putInt(para1: jint): JNByteBuffer; overload; virtual; abstract;
    function getInt(para1: jint): jint; overload; virtual; abstract;
    function putInt(para1: jint; para2: jint): JNByteBuffer; overload; virtual; abstract;
    function asIntBuffer(): JNIntBuffer; overload; virtual; abstract;
    function getLong(): jlong; overload; virtual; abstract;
    function putLong(para1: jlong): JNByteBuffer; overload; virtual; abstract;
    function getLong(para1: jint): jlong; overload; virtual; abstract;
    function putLong(para1: jint; para2: jlong): JNByteBuffer; overload; virtual; abstract;
    function asLongBuffer(): JNLongBuffer; overload; virtual; abstract;
    function getFloat(): jfloat; overload; virtual; abstract;
    function putFloat(para1: jfloat): JNByteBuffer; overload; virtual; abstract;
    function getFloat(para1: jint): jfloat; overload; virtual; abstract;
    function putFloat(para1: jint; para2: jfloat): JNByteBuffer; overload; virtual; abstract;
    function asFloatBuffer(): JNFloatBuffer; overload; virtual; abstract;
    function getDouble(): jdouble; overload; virtual; abstract;
    function putDouble(para1: jdouble): JNByteBuffer; overload; virtual; abstract;
    function getDouble(para1: jint): jdouble; overload; virtual; abstract;
    function putDouble(para1: jint; para2: jdouble): JNByteBuffer; overload; virtual; abstract;
    function asDoubleBuffer(): JNDoubleBuffer; overload; virtual; abstract;
    function &array(): JLObject; overload; virtual;
    function compareTo(para1: JLObject): jint; overload; virtual;
  end;

  JNCharBuffer = class abstract external 'java.nio' name 'CharBuffer' (JNBuffer, JLComparable, JLAppendable, JLCharSequence, JLReadable)
  public
    class function allocate(para1: jint): JNCharBuffer; static; overload;
    class function wrap(para1: Arr1jchar; para2: jint; para3: jint): JNCharBuffer; static; overload;
    class function wrap(var para1: array of jchar; para2: jint; para3: jint): JNCharBuffer; static; overload;
    class function wrap(para1: Arr1jchar): JNCharBuffer; static; overload;
    class function wrap(var para1: array of jchar): JNCharBuffer; static; overload;
    function read(para1: JNCharBuffer): jint; overload; virtual;  // throws java.io.IOException
    class function wrap(para1: JLCharSequence; para2: jint; para3: jint): JNCharBuffer; static; overload;
    class function wrap(para1: JLCharSequence): JNCharBuffer; static; overload;
    function slice(): JNCharBuffer; overload; virtual; abstract;
    function duplicate(): JNCharBuffer; overload; virtual; abstract;
    function asReadOnlyBuffer(): JNCharBuffer; overload; virtual; abstract;
    function get(): jchar; overload; virtual; abstract;
    function put(para1: jchar): JNCharBuffer; overload; virtual; abstract;
    function get(para1: jint): jchar; overload; virtual; abstract;
    function put(para1: jint; para2: jchar): JNCharBuffer; overload; virtual; abstract;
    function get(para1: Arr1jchar; para2: jint; para3: jint): JNCharBuffer; overload; virtual;
    function get(var para1: array of jchar; para2: jint; para3: jint): JNCharBuffer; overload; virtual;
    function get(para1: Arr1jchar): JNCharBuffer; overload; virtual;
    function get(var para1: array of jchar): JNCharBuffer; overload; virtual;
    function put(para1: JNCharBuffer): JNCharBuffer; overload; virtual;
    function put(para1: Arr1jchar; para2: jint; para3: jint): JNCharBuffer; overload; virtual;
    function put(var para1: array of jchar; para2: jint; para3: jint): JNCharBuffer; overload; virtual;
    function put(para1: Arr1jchar): JNCharBuffer; overload; virtual; final;
    function put(var para1: array of jchar): JNCharBuffer; overload; virtual; final;
    function put(para1: JLString; para2: jint; para3: jint): JNCharBuffer; overload; virtual;
    function put(para1: JLString): JNCharBuffer; overload; virtual; final;
    function hasArray(): jboolean; overload; virtual; final;
    function &array(): Arr1jchar; overload; virtual; final;
    function arrayOffset(): jint; overload; virtual; final;
    function compact(): JNCharBuffer; overload; virtual; abstract;
    function isDirect(): jboolean; overload; virtual; abstract;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function compareTo(para1: JNCharBuffer): jint; overload; virtual;
    function toString(): JLString; overload; virtual;
    function length(): jint; overload; virtual; final;
    function charAt(para1: jint): jchar; overload; virtual; final;
    function subSequence(para1: jint; para2: jint): JLCharSequence; overload; virtual; abstract;
    function append(para1: JLCharSequence): JNCharBuffer; overload; virtual;
    function append(para1: JLCharSequence; para2: jint; para3: jint): JNCharBuffer; overload; virtual;
    function append(para1: jchar): JNCharBuffer; overload; virtual;
    function order(): JNByteOrder; overload; virtual; abstract;
    function &array(): JLObject; overload; virtual;
    function compareTo(para1: JLObject): jint; overload; virtual;
    function append(para1: jchar): JLAppendable; overload; virtual;  // throws java.io.IOException
    function append(para1: JLCharSequence; para2: jint; para3: jint): JLAppendable; overload; virtual;  // throws java.io.IOException
    function append(para1: JLCharSequence): JLAppendable; overload; virtual;  // throws java.io.IOException
  end;

  JTCollator = class abstract external 'java.text' name 'Collator' (JLObject, JUComparator, JLCloneable)
  public
    type
      InnerCollatorGetter = class;
      Arr1InnerCollatorGetter = array of InnerCollatorGetter;
      Arr2InnerCollatorGetter = array of Arr1InnerCollatorGetter;
      Arr3InnerCollatorGetter = array of Arr2InnerCollatorGetter;
      InnerCollatorGetter = class external 'java.text' name 'CollatorGetter' 
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

  JUAbstractMap = class abstract external 'java.util' name 'AbstractMap' (JLObject, JUMap)
  public
    type
      InnerSimpleEntry = class;
      Arr1InnerSimpleEntry = array of InnerSimpleEntry;
      Arr2InnerSimpleEntry = array of Arr1InnerSimpleEntry;
      Arr3InnerSimpleEntry = array of Arr2InnerSimpleEntry;
      InnerSimpleImmutableEntry = class;
      Arr1InnerSimpleImmutableEntry = array of InnerSimpleImmutableEntry;
      Arr2InnerSimpleImmutableEntry = array of Arr1InnerSimpleImmutableEntry;
      Arr3InnerSimpleImmutableEntry = array of Arr2InnerSimpleImmutableEntry;
      InnerSimpleEntry = class external 'java.util' name 'SimpleEntry' (JLObject, JUMap.InnerEntry, JISerializable)
      public
        constructor create(para1: JLObject; para2: JLObject); overload;
        constructor create(para1: JUMap.InnerEntry); overload;
        function getKey(): JLObject; overload; virtual;
        function getValue(): JLObject; overload; virtual;
        function setValue(para1: JLObject): JLObject; overload; virtual;
        function equals(para1: JLObject): jboolean; overload; virtual;
        function hashCode(): jint; overload; virtual;
        function toString(): JLString; overload; virtual;
      end;

      InnerSimpleImmutableEntry = class external 'java.util' name 'SimpleImmutableEntry' (JLObject, JUMap.InnerEntry, JISerializable)
      public
        constructor create(para1: JLObject; para2: JLObject); overload;
        constructor create(para1: JUMap.InnerEntry); overload;
        function getKey(): JLObject; overload; virtual;
        function getValue(): JLObject; overload; virtual;
        function setValue(para1: JLObject): JLObject; overload; virtual;
        function equals(para1: JLObject): jboolean; overload; virtual;
        function hashCode(): jint; overload; virtual;
        function toString(): JLString; overload; virtual;
      end;

  strict protected
    constructor create(); overload;
  public
    function size(): jint; overload; virtual;
    function isEmpty(): jboolean; overload; virtual;
    function containsValue(para1: JLObject): jboolean; overload; virtual;
    function containsKey(para1: JLObject): jboolean; overload; virtual;
    function get(para1: JLObject): JLObject; overload; virtual;
    function put(para1: JLObject; para2: JLObject): JLObject; overload; virtual;
    function remove(para1: JLObject): JLObject; overload; virtual;
    procedure putAll(para1: JUMap); overload; virtual;
    procedure clear(); overload; virtual;
    function keySet(): JUSet; overload; virtual;
    function values(): JUCollection; overload; virtual;
    function entrySet(): JUSet; overload; virtual; abstract;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function toString(): JLString; overload; virtual;
  strict protected
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  JLByte = class sealed external 'java.lang' name 'Byte' (JLNumber, JLComparable)
  public
    type
      InnerByteCache = class;
      Arr1InnerByteCache = array of InnerByteCache;
      Arr2InnerByteCache = array of Arr1InnerByteCache;
      Arr3InnerByteCache = array of Arr2InnerByteCache;
      InnerByteCache = class external 'java.lang' name 'ByteCache' 
      end;

  public
    const
      MIN_VALUE = -128;
      MAX_VALUE = 127;
  public
    final class var
      fTYPE: JLClass; external name 'TYPE';
  public
    const
      SIZE = 8;
  public
    class function toString(para1: jbyte): JLString; static; overload;
    class function valueOf(para1: jbyte): JLByte; static; overload;
    class function parseByte(para1: JLString): jbyte; static; overload;  // throws java.lang.NumberFormatException
    class function parseByte(para1: JLString; para2: jint): jbyte; static; overload;  // throws java.lang.NumberFormatException
    class function valueOf(para1: JLString; para2: jint): JLByte; static; overload;  // throws java.lang.NumberFormatException
    class function valueOf(para1: JLString): JLByte; static; overload;  // throws java.lang.NumberFormatException
    class function decode(para1: JLString): JLByte; static; overload;  // throws java.lang.NumberFormatException
    constructor create(para1: jbyte); overload;
    constructor create(para1: JLString); overload;  // throws java.lang.NumberFormatException
    function byteValue(): jbyte; overload; virtual;
    function shortValue(): jshort; overload; virtual;
    function intValue(): jint; overload; virtual;
    function longValue(): jlong; overload; virtual;
    function floatValue(): jfloat; overload; virtual;
    function doubleValue(): jdouble; overload; virtual;
    function toString(): JLString; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function compareTo(para1: JLByte): jint; overload; virtual;
    function compareTo(para1: JLObject): jint; overload; virtual;
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

  JLInteger = class sealed external 'java.lang' name 'Integer' (JLNumber, JLComparable)
  public
    type
      InnerIntegerCache = class;
      Arr1InnerIntegerCache = array of InnerIntegerCache;
      Arr2InnerIntegerCache = array of Arr1InnerIntegerCache;
      Arr3InnerIntegerCache = array of Arr2InnerIntegerCache;
      InnerIntegerCache = class external 'java.lang' name 'IntegerCache' 
      end;

  public
    const
      MIN_VALUE = -2147483648;
      MAX_VALUE = 2147483647;
  public
    final class var
      fTYPE: JLClass; external name 'TYPE';
  public
    const
      SIZE = 32;
  public
    class function toString(para1: jint; para2: jint): JLString; static; overload;
    class function toHexString(para1: jint): JLString; static; overload;
    class function toOctalString(para1: jint): JLString; static; overload;
    class function toBinaryString(para1: jint): JLString; static; overload;
    class function toString(para1: jint): JLString; static; overload;
    class function parseInt(para1: JLString; para2: jint): jint; static; overload;  // throws java.lang.NumberFormatException
    class function parseInt(para1: JLString): jint; static; overload;  // throws java.lang.NumberFormatException
    class function valueOf(para1: JLString; para2: jint): JLInteger; static; overload;  // throws java.lang.NumberFormatException
    class function valueOf(para1: JLString): JLInteger; static; overload;  // throws java.lang.NumberFormatException
    class function valueOf(para1: jint): JLInteger; static; overload;
    constructor create(para1: jint); overload;
    constructor create(para1: JLString); overload;  // throws java.lang.NumberFormatException
    function byteValue(): jbyte; overload; virtual;
    function shortValue(): jshort; overload; virtual;
    function intValue(): jint; overload; virtual;
    function longValue(): jlong; overload; virtual;
    function floatValue(): jfloat; overload; virtual;
    function doubleValue(): jdouble; overload; virtual;
    function toString(): JLString; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    class function getInteger(para1: JLString): JLInteger; static; overload;
    class function getInteger(para1: JLString; para2: jint): JLInteger; static; overload;
    class function getInteger(para1: JLString; para2: JLInteger): JLInteger; static; overload;
    class function decode(para1: JLString): JLInteger; static; overload;  // throws java.lang.NumberFormatException
    function compareTo(para1: JLInteger): jint; overload; virtual;
    class function highestOneBit(para1: jint): jint; static; overload;
    class function lowestOneBit(para1: jint): jint; static; overload;
    class function numberOfLeadingZeros(para1: jint): jint; static; overload;
    class function numberOfTrailingZeros(para1: jint): jint; static; overload;
    class function bitCount(para1: jint): jint; static; overload;
    class function rotateLeft(para1: jint; para2: jint): jint; static; overload;
    class function rotateRight(para1: jint; para2: jint): jint; static; overload;
    class function reverse(para1: jint): jint; static; overload;
    class function signum(para1: jint): jint; static; overload;
    class function reverseBytes(para1: jint): jint; static; overload;
    function compareTo(para1: JLObject): jint; overload; virtual;
  end;

  JLLong = class sealed external 'java.lang' name 'Long' (JLNumber, JLComparable)
  public
    type
      InnerLongCache = class;
      Arr1InnerLongCache = array of InnerLongCache;
      Arr2InnerLongCache = array of Arr1InnerLongCache;
      Arr3InnerLongCache = array of Arr2InnerLongCache;
      InnerLongCache = class external 'java.lang' name 'LongCache' 
      end;

  public
    const
      MIN_VALUE = jlong(-9223372036854775808);
      MAX_VALUE = jlong(9223372036854775807);
  public
    final class var
      fTYPE: JLClass; external name 'TYPE';
  public
    const
      SIZE = 64;
  public
    class function toString(para1: jlong; para2: jint): JLString; static; overload;
    class function toHexString(para1: jlong): JLString; static; overload;
    class function toOctalString(para1: jlong): JLString; static; overload;
    class function toBinaryString(para1: jlong): JLString; static; overload;
    class function toString(para1: jlong): JLString; static; overload;
    class function parseLong(para1: JLString; para2: jint): jlong; static; overload;  // throws java.lang.NumberFormatException
    class function parseLong(para1: JLString): jlong; static; overload;  // throws java.lang.NumberFormatException
    class function valueOf(para1: JLString; para2: jint): JLLong; static; overload;  // throws java.lang.NumberFormatException
    class function valueOf(para1: JLString): JLLong; static; overload;  // throws java.lang.NumberFormatException
    class function valueOf(para1: jlong): JLLong; static; overload;
    class function decode(para1: JLString): JLLong; static; overload;  // throws java.lang.NumberFormatException
    constructor create(para1: jlong); overload;
    constructor create(para1: JLString); overload;  // throws java.lang.NumberFormatException
    function byteValue(): jbyte; overload; virtual;
    function shortValue(): jshort; overload; virtual;
    function intValue(): jint; overload; virtual;
    function longValue(): jlong; overload; virtual;
    function floatValue(): jfloat; overload; virtual;
    function doubleValue(): jdouble; overload; virtual;
    function toString(): JLString; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    class function getLong(para1: JLString): JLLong; static; overload;
    class function getLong(para1: JLString; para2: jlong): JLLong; static; overload;
    class function getLong(para1: JLString; para2: JLLong): JLLong; static; overload;
    function compareTo(para1: JLLong): jint; overload; virtual;
    class function highestOneBit(para1: jlong): jlong; static; overload;
    class function lowestOneBit(para1: jlong): jlong; static; overload;
    class function numberOfLeadingZeros(para1: jlong): jint; static; overload;
    class function numberOfTrailingZeros(para1: jlong): jint; static; overload;
    class function bitCount(para1: jlong): jint; static; overload;
    class function rotateLeft(para1: jlong; para2: jint): jlong; static; overload;
    class function rotateRight(para1: jlong; para2: jint): jlong; static; overload;
    class function reverse(para1: jlong): jlong; static; overload;
    class function signum(para1: jlong): jint; static; overload;
    class function reverseBytes(para1: jlong): jlong; static; overload;
    function compareTo(para1: JLObject): jint; overload; virtual;
  end;

  JLShort = class sealed external 'java.lang' name 'Short' (JLNumber, JLComparable)
  public
    type
      InnerShortCache = class;
      Arr1InnerShortCache = array of InnerShortCache;
      Arr2InnerShortCache = array of Arr1InnerShortCache;
      Arr3InnerShortCache = array of Arr2InnerShortCache;
      InnerShortCache = class external 'java.lang' name 'ShortCache' 
      end;

  public
    const
      MIN_VALUE = -32768;
      MAX_VALUE = 32767;
  public
    final class var
      fTYPE: JLClass; external name 'TYPE';
  public
    const
      SIZE = 16;
  public
    class function toString(para1: jshort): JLString; static; overload;
    class function parseShort(para1: JLString): jshort; static; overload;  // throws java.lang.NumberFormatException
    class function parseShort(para1: JLString; para2: jint): jshort; static; overload;  // throws java.lang.NumberFormatException
    class function valueOf(para1: JLString; para2: jint): JLShort; static; overload;  // throws java.lang.NumberFormatException
    class function valueOf(para1: JLString): JLShort; static; overload;  // throws java.lang.NumberFormatException
    class function valueOf(para1: jshort): JLShort; static; overload;
    class function decode(para1: JLString): JLShort; static; overload;  // throws java.lang.NumberFormatException
    constructor create(para1: jshort); overload;
    constructor create(para1: JLString); overload;  // throws java.lang.NumberFormatException
    function byteValue(): jbyte; overload; virtual;
    function shortValue(): jshort; overload; virtual;
    function intValue(): jint; overload; virtual;
    function longValue(): jlong; overload; virtual;
    function floatValue(): jfloat; overload; virtual;
    function doubleValue(): jdouble; overload; virtual;
    function toString(): JLString; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function compareTo(para1: JLShort): jint; overload; virtual;
    class function reverseBytes(para1: jshort): jshort; static; overload;
    function compareTo(para1: JLObject): jint; overload; virtual;
  end;

  JMBigInteger = class external 'java.math' name 'BigInteger' (JLNumber, JLComparable)
  public
    final class var
      fZERO: JMBigInteger; external name 'ZERO';
      fONE: JMBigInteger; external name 'ONE';
      fTEN: JMBigInteger; external name 'TEN';
  public
    constructor create(para1: Arr1jbyte); overload;
    constructor create(const para1: array of jbyte); overload;
    constructor create(para1: jint; para2: Arr1jbyte); overload;
    constructor create(para1: jint; const para2: array of jbyte); overload;
    constructor create(para1: JLString; para2: jint); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: jint; para2: JURandom); overload;
    constructor create(para1: jint; para2: jint; para3: JURandom); overload;
    class function probablePrime(para1: jint; para2: JURandom): JMBigInteger; static; overload;
    function nextProbablePrime(): JMBigInteger; overload; virtual;
    class function valueOf(para1: jlong): JMBigInteger; static; overload;
    function add(para1: JMBigInteger): JMBigInteger; overload; virtual;
    function subtract(para1: JMBigInteger): JMBigInteger; overload; virtual;
    function multiply(para1: JMBigInteger): JMBigInteger; overload; virtual;
    function divide(para1: JMBigInteger): JMBigInteger; overload; virtual;
    function divideAndRemainder(para1: JMBigInteger): Arr1JMBigInteger; overload; virtual;
    function remainder(para1: JMBigInteger): JMBigInteger; overload; virtual;
    function pow(para1: jint): JMBigInteger; overload; virtual;
    function gcd(para1: JMBigInteger): JMBigInteger; overload; virtual;
    function abs(): JMBigInteger; overload; virtual;
    function negate(): JMBigInteger; overload; virtual;
    function signum(): jint; overload; virtual;
    function &mod(para1: JMBigInteger): JMBigInteger; overload; virtual;
    function modPow(para1: JMBigInteger; para2: JMBigInteger): JMBigInteger; overload; virtual;
    function modInverse(para1: JMBigInteger): JMBigInteger; overload; virtual;
    function shiftLeft(para1: jint): JMBigInteger; overload; virtual;
    function shiftRight(para1: jint): JMBigInteger; overload; virtual;
    function &and(para1: JMBigInteger): JMBigInteger; overload; virtual;
    function &or(para1: JMBigInteger): JMBigInteger; overload; virtual;
    function &xor(para1: JMBigInteger): JMBigInteger; overload; virtual;
    function &not(): JMBigInteger; overload; virtual;
    function andNot(para1: JMBigInteger): JMBigInteger; overload; virtual;
    function testBit(para1: jint): jboolean; overload; virtual;
    function setBit(para1: jint): JMBigInteger; overload; virtual;
    function clearBit(para1: jint): JMBigInteger; overload; virtual;
    function flipBit(para1: jint): JMBigInteger; overload; virtual;
    function getLowestSetBit(): jint; overload; virtual;
    function bitLength(): jint; overload; virtual;
    function bitCount(): jint; overload; virtual;
    function isProbablePrime(para1: jint): jboolean; overload; virtual;
    function compareTo(para1: JMBigInteger): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function min(para1: JMBigInteger): JMBigInteger; overload; virtual;
    function max(para1: JMBigInteger): JMBigInteger; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function toString(para1: jint): JLString; overload; virtual;
    function toString(): JLString; overload; virtual;
    function toByteArray(): Arr1jbyte; overload; virtual;
    function intValue(): jint; overload; virtual;
    function longValue(): jlong; overload; virtual;
    function floatValue(): jfloat; overload; virtual;
    function doubleValue(): jdouble; overload; virtual;
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

  JUAbstractCollection = class abstract external 'java.util' name 'AbstractCollection' (JLObject, JUCollection)
  strict protected
    constructor create(); overload;
  public
    function iterator(): JUIterator; overload; virtual; abstract;
    function size(): jint; overload; virtual; abstract;
    function isEmpty(): jboolean; overload; virtual;
    function contains(para1: JLObject): jboolean; overload; virtual;
    function toArray(): Arr1JLObject; overload; virtual;
    function toArray(para1: Arr1JLObject): Arr1JLObject; overload; virtual;
    function toArray(var para1: array of JLObject): Arr1JLObject; overload; virtual;
    function add(para1: JLObject): jboolean; overload; virtual;
    function remove(para1: JLObject): jboolean; overload; virtual;
    function containsAll(para1: JUCollection): jboolean; overload; virtual;
    function addAll(para1: JUCollection): jboolean; overload; virtual;
    function removeAll(para1: JUCollection): jboolean; overload; virtual;
    function retainAll(para1: JUCollection): jboolean; overload; virtual;
    procedure clear(); overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  JUSet = interface external 'java.util' name 'Set' (JUCollection)
    function size(): jint; overload;
    function isEmpty(): jboolean; overload;
    function contains(para1: JLObject): jboolean; overload;
    function iterator(): JUIterator; overload;
    function toArray(): Arr1JLObject; overload;
    function toArray(para1: Arr1JLObject): Arr1JLObject; overload;
    function toArray(var para1: array of JLObject): Arr1JLObject; overload;
    function add(para1: JLObject): jboolean; overload;
    function remove(para1: JLObject): jboolean; overload;
    function containsAll(para1: JUCollection): jboolean; overload;
    function addAll(para1: JUCollection): jboolean; overload;
    function retainAll(para1: JUCollection): jboolean; overload;
    function removeAll(para1: JUCollection): jboolean; overload;
    procedure clear(); overload;
    function equals(para1: JLObject): jboolean; overload;
    function hashCode(): jint; overload;
  end;

  JLRField = class sealed external 'java.lang.reflect' name 'Field' (JLRAccessibleObject, JLRMember)
  public
    function getDeclaringClass(): JLClass; overload; virtual;
    function getName(): JLString; overload; virtual;
    function getModifiers(): jint; overload; virtual;
    function isEnumConstant(): jboolean; overload; virtual;
    function isSynthetic(): jboolean; overload; virtual;
    function getType(): JLClass; overload; virtual;
    function getGenericType(): JLRType; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function toString(): JLString; overload; virtual;
    function toGenericString(): JLString; overload; virtual;
    function get(para1: JLObject): JLObject; overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    function getBoolean(para1: JLObject): jboolean; overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    function getByte(para1: JLObject): jbyte; overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    function getChar(para1: JLObject): jchar; overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    function getShort(para1: JLObject): jshort; overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    function getInt(para1: JLObject): jint; overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    function getLong(para1: JLObject): jlong; overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    function getFloat(para1: JLObject): jfloat; overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    function getDouble(para1: JLObject): jdouble; overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    procedure &set(para1: JLObject; para2: JLObject); overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    procedure setBoolean(para1: JLObject; para2: jboolean); overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    procedure setByte(para1: JLObject; para2: jbyte); overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    procedure setChar(para1: JLObject; para2: jchar); overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    procedure setShort(para1: JLObject; para2: jshort); overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    procedure setInt(para1: JLObject; para2: jint); overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    procedure setLong(para1: JLObject; para2: jlong); overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    procedure setFloat(para1: JLObject; para2: jfloat); overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    procedure setDouble(para1: JLObject; para2: jdouble); overload; virtual;  // throws java.lang.IllegalArgumentException, java.lang.IllegalAccessException
    function getAnnotation(para1: JLClass): JLAAnnotation; overload; virtual;
    function getDeclaredAnnotations(): Arr1JLAAnnotation; overload; virtual;
  end;

  JLRMethod = class sealed external 'java.lang.reflect' name 'Method' (JLRAccessibleObject, JLRGenericDeclaration, JLRMember)
  public
    function getDeclaringClass(): JLClass; overload; virtual;
    function getName(): JLString; overload; virtual;
    function getModifiers(): jint; overload; virtual;
    function getTypeParameters(): Arr1JLRTypeVariable; overload; virtual;
    function getReturnType(): JLClass; overload; virtual;
    function getGenericReturnType(): JLRType; overload; virtual;
    function getParameterTypes(): Arr1JLClass; overload; virtual;
    function getGenericParameterTypes(): Arr1JLRType; overload; virtual;
    function getExceptionTypes(): Arr1JLClass; overload; virtual;
    function getGenericExceptionTypes(): Arr1JLRType; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function toString(): JLString; overload; virtual;
    function toGenericString(): JLString; overload; virtual;
    function invoke(para1: JLObject; para2: Arr1JLObject): JLObject; overload; virtual;  // throws java.lang.IllegalAccessException, java.lang.IllegalArgumentException, java.lang.reflect.InvocationTargetException
    function invoke(para1: JLObject; const para2: array of JLObject): JLObject; overload; virtual;  // throws java.lang.IllegalAccessException, java.lang.IllegalArgumentException, java.lang.reflect.InvocationTargetException
    function isBridge(): jboolean; overload; virtual;
    function isVarArgs(): jboolean; overload; virtual;
    function isSynthetic(): jboolean; overload; virtual;
    function getAnnotation(para1: JLClass): JLAAnnotation; overload; virtual;
    function getDeclaredAnnotations(): Arr1JLAAnnotation; overload; virtual;
    function getDefaultValue(): JLObject; overload; virtual;
    function getParameterAnnotations(): Arr2JLAAnnotation; overload; virtual;
  end;

  JUHashMap = class external 'java.util' name 'HashMap' (JUAbstractMap, JUMap, JLCloneable, JISerializable)
  public
    type
      InnerEntry = class;
      Arr1InnerEntry = array of InnerEntry;
      Arr2InnerEntry = array of Arr1InnerEntry;
      Arr3InnerEntry = array of Arr2InnerEntry;
      InnerEntryIterator = class;
      Arr1InnerEntryIterator = array of InnerEntryIterator;
      Arr2InnerEntryIterator = array of Arr1InnerEntryIterator;
      Arr3InnerEntryIterator = array of Arr2InnerEntryIterator;
      InnerKeyIterator = class;
      Arr1InnerKeyIterator = array of InnerKeyIterator;
      Arr2InnerKeyIterator = array of Arr1InnerKeyIterator;
      Arr3InnerKeyIterator = array of Arr2InnerKeyIterator;
      InnerValueIterator = class;
      Arr1InnerValueIterator = array of InnerValueIterator;
      Arr2InnerValueIterator = array of Arr1InnerValueIterator;
      Arr3InnerValueIterator = array of Arr2InnerValueIterator;
      InnerEntrySet = class;
      Arr1InnerEntrySet = array of InnerEntrySet;
      Arr2InnerEntrySet = array of Arr1InnerEntrySet;
      Arr3InnerEntrySet = array of Arr2InnerEntrySet;
      InnerKeySet = class;
      Arr1InnerKeySet = array of InnerKeySet;
      Arr2InnerKeySet = array of Arr1InnerKeySet;
      Arr3InnerKeySet = array of Arr2InnerKeySet;
      InnerHashIterator = class;
      Arr1InnerHashIterator = array of InnerHashIterator;
      Arr2InnerHashIterator = array of Arr1InnerHashIterator;
      Arr3InnerHashIterator = array of Arr2InnerHashIterator;
      InnerValues = class;
      Arr1InnerValues = array of InnerValues;
      Arr2InnerValues = array of Arr1InnerValues;
      Arr3InnerValues = array of Arr2InnerValues;
      InnerEntry = class external 'java.util' name 'Entry' 
      end;

      InnerEntryIterator = class sealed external 'java.util' name 'EntryIterator' 
      end;

      InnerKeyIterator = class sealed external 'java.util' name 'KeyIterator' 
      end;

      InnerValueIterator = class sealed external 'java.util' name 'ValueIterator' 
      end;

      InnerEntrySet = class sealed external 'java.util' name 'EntrySet' 
      end;

      InnerKeySet = class sealed external 'java.util' name 'KeySet' 
      end;

      InnerHashIterator = class abstract external 'java.util' name 'HashIterator' 
      end;

      InnerValues = class sealed external 'java.util' name 'Values' 
      end;

  public
    constructor create(para1: jint; para2: jfloat); overload;
    constructor create(para1: jint); overload;
    constructor create(); overload;
    constructor create(para1: JUMap); overload;
    function size(): jint; overload; virtual;
    function isEmpty(): jboolean; overload; virtual;
    function get(para1: JLObject): JLObject; overload; virtual;
    function containsKey(para1: JLObject): jboolean; overload; virtual;
    function put(para1: JLObject; para2: JLObject): JLObject; overload; virtual;
    procedure putAll(para1: JUMap); overload; virtual;
    function remove(para1: JLObject): JLObject; overload; virtual;
    procedure clear(); overload; virtual;
    function containsValue(para1: JLObject): jboolean; overload; virtual;
    function clone(): JLObject; overload; virtual;
    function keySet(): JUSet; overload; virtual;
    function values(): JUCollection; overload; virtual;
    function entrySet(): JUSet; overload; virtual;
  end;

  JLAssertionError = class external 'java.lang' name 'AssertionError' (JLError)
  public
    constructor create(); overload;
    constructor create(para1: JLObject); overload;
    constructor create(para1: jboolean); overload;
    constructor create(para1: jchar); overload;
    constructor create(para1: jint); overload;
    constructor create(para1: jlong); overload;
    constructor create(para1: jfloat); overload;
    constructor create(para1: jdouble); overload;
  end;

  JLLinkageError = class external 'java.lang' name 'LinkageError' (JLError)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
  end;

  JNCCoderMalfunctionError = class external 'java.nio.charset' name 'CoderMalfunctionError' (JLError)
  public
    constructor create(para1: JLException); overload;
  end;

  JIIOException = class external 'java.io' name 'IOException' (JLException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
    constructor create(para1: JLThrowable); overload;
  end;

  JLNoSuchMethodException = class external 'java.lang' name 'NoSuchMethodException' (JLException)
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

  JLRInvocationTargetException = class external 'java.lang.reflect' name 'InvocationTargetException' (JLException)
  strict protected
    constructor create(); overload;
  public
    constructor create(para1: JLThrowable); overload;
    constructor create(para1: JLThrowable; para2: JLString); overload;
    function getTargetException(): JLThrowable; overload; virtual;
    function getCause(): JLThrowable; overload; virtual;
  end;

  JUAbstractSet = class abstract external 'java.util' name 'AbstractSet' (JUAbstractCollection, JUSet)
  strict protected
    constructor create(); overload;
  public
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function removeAll(para1: JUCollection): jboolean; overload; virtual;
  end;

  JNCCharacterCodingException = class external 'java.nio.charset' name 'CharacterCodingException' (JIIOException)
  public
    constructor create(); overload;
  end;

  JLIllegalArgumentException = class external 'java.lang' name 'IllegalArgumentException' (JLRuntimeException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
    constructor create(para1: JLThrowable); overload;
  end;

  JLIllegalStateException = class external 'java.lang' name 'IllegalStateException' (JLRuntimeException)
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

  JLUnsupportedOperationException = class external 'java.lang' name 'UnsupportedOperationException' (JLRuntimeException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
    constructor create(para1: JLThrowable); overload;
  end;

  JUEnumSet = class abstract external 'java.util' name 'EnumSet' (JUAbstractSet, JLCloneable, JISerializable)
  public
    type
      InnerSerializationProxy = class;
      Arr1InnerSerializationProxy = array of InnerSerializationProxy;
      Arr2InnerSerializationProxy = array of Arr1InnerSerializationProxy;
      Arr3InnerSerializationProxy = array of Arr2InnerSerializationProxy;
      InnerSerializationProxy = class external 'java.util' name 'SerializationProxy' 
      end;

  public
    class function noneOf(para1: JLClass): JUEnumSet; static; overload;
    class function allOf(para1: JLClass): JUEnumSet; static; overload;
    class function copyOf(para1: JUEnumSet): JUEnumSet; static; overload;
    class function copyOf(para1: JUCollection): JUEnumSet; static; overload;
    class function complementOf(para1: JUEnumSet): JUEnumSet; static; overload;
    class function &of(para1: JLEnum): JUEnumSet; static; overload;
    class function &of(para1: JLEnum; para2: JLEnum): JUEnumSet; static; overload;
    class function &of(para1: JLEnum; para2: JLEnum; para3: JLEnum): JUEnumSet; static; overload;
    class function &of(para1: JLEnum; para2: JLEnum; para3: JLEnum; para4: JLEnum): JUEnumSet; static; overload;
    class function &of(para1: JLEnum; para2: JLEnum; para3: JLEnum; para4: JLEnum; para5: JLEnum): JUEnumSet; static; overload;
    class function &of(para1: JLEnum; para2: Arr1JLEnum): JUEnumSet; static; overload;
    class function &of(para1: JLEnum; const para2: array of JLEnum): JUEnumSet; static; overload;
    class function range(para1: JLEnum; para2: JLEnum): JUEnumSet; static; overload;
    function clone(): JUEnumSet; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  JNCMalformedInputException = class external 'java.nio.charset' name 'MalformedInputException' (JNCCharacterCodingException)
  public
    constructor create(para1: jint); overload;
    function getInputLength(): jint; overload; virtual;
    function getMessage(): JLString; overload; virtual;
  end;

  JNCUnmappableCharacterException = class external 'java.nio.charset' name 'UnmappableCharacterException' (JNCCharacterCodingException)
  public
    constructor create(para1: jint); overload;
    function getInputLength(): jint; overload; virtual;
    function getMessage(): JLString; overload; virtual;
  end;

  JNCIllegalCharsetNameException = class external 'java.nio.charset' name 'IllegalCharsetNameException' (JLIllegalArgumentException)
  public
    constructor create(para1: JLString); overload;
    function getCharsetName(): JLString; overload; virtual;
  end;

  JNCUnsupportedCharsetException = class external 'java.nio.charset' name 'UnsupportedCharsetException' (JLIllegalArgumentException)
  public
    constructor create(para1: JLString); overload;
    function getCharsetName(): JLString; overload; virtual;
  end;

  JUWeakHashMap = class external 'java.util' name 'WeakHashMap' (JUAbstractMap, JUMap)
  public
    type
      InnerEntry = class;
      Arr1InnerEntry = array of InnerEntry;
      Arr2InnerEntry = array of Arr1InnerEntry;
      Arr3InnerEntry = array of Arr2InnerEntry;
      InnerEntryIterator = class;
      Arr1InnerEntryIterator = array of InnerEntryIterator;
      Arr2InnerEntryIterator = array of Arr1InnerEntryIterator;
      Arr3InnerEntryIterator = array of Arr2InnerEntryIterator;
      InnerKeyIterator = class;
      Arr1InnerKeyIterator = array of InnerKeyIterator;
      Arr2InnerKeyIterator = array of Arr1InnerKeyIterator;
      Arr3InnerKeyIterator = array of Arr2InnerKeyIterator;
      InnerValueIterator = class;
      Arr1InnerValueIterator = array of InnerValueIterator;
      Arr2InnerValueIterator = array of Arr1InnerValueIterator;
      Arr3InnerValueIterator = array of Arr2InnerValueIterator;
      InnerEntrySet = class;
      Arr1InnerEntrySet = array of InnerEntrySet;
      Arr2InnerEntrySet = array of Arr1InnerEntrySet;
      Arr3InnerEntrySet = array of Arr2InnerEntrySet;
      InnerKeySet = class;
      Arr1InnerKeySet = array of InnerKeySet;
      Arr2InnerKeySet = array of Arr1InnerKeySet;
      Arr3InnerKeySet = array of Arr2InnerKeySet;
      InnerHashIterator = class;
      Arr1InnerHashIterator = array of InnerHashIterator;
      Arr2InnerHashIterator = array of Arr1InnerHashIterator;
      Arr3InnerHashIterator = array of Arr2InnerHashIterator;
      InnerValues = class;
      Arr1InnerValues = array of InnerValues;
      Arr2InnerValues = array of Arr1InnerValues;
      Arr3InnerValues = array of Arr2InnerValues;
      InnerEntry = class external 'java.util' name 'Entry' 
      end;

      InnerEntryIterator = class external 'java.util' name 'EntryIterator' 
      end;

      InnerKeyIterator = class external 'java.util' name 'KeyIterator' 
      end;

      InnerValueIterator = class external 'java.util' name 'ValueIterator' 
      end;

      InnerEntrySet = class external 'java.util' name 'EntrySet' 
      end;

      InnerKeySet = class external 'java.util' name 'KeySet' 
      end;

      InnerHashIterator = class abstract external 'java.util' name 'HashIterator' 
      end;

      InnerValues = class external 'java.util' name 'Values' 
      end;

  public
    constructor create(para1: jint; para2: jfloat); overload;
    constructor create(para1: jint); overload;
    constructor create(); overload;
    constructor create(para1: JUMap); overload;
    function size(): jint; overload; virtual;
    function isEmpty(): jboolean; overload; virtual;
    function get(para1: JLObject): JLObject; overload; virtual;
    function containsKey(para1: JLObject): jboolean; overload; virtual;
    function put(para1: JLObject; para2: JLObject): JLObject; overload; virtual;
    procedure putAll(para1: JUMap); overload; virtual;
    function remove(para1: JLObject): JLObject; overload; virtual;
    procedure clear(); overload; virtual;
    function containsValue(para1: JLObject): jboolean; overload; virtual;
    function keySet(): JUSet; overload; virtual;
    function values(): JUCollection; overload; virtual;
    function entrySet(): JUSet; overload; virtual;
  end;

