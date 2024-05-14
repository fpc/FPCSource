{
  JOB - JS Object Bridge for Webassembly

  Webassembly unit giving access to the browser DOM.

  see https://wiki.freepascal.org/WebAssembly/DOM
}
{$IFNDEF FPC_DOTTEDUNITS}
unit JOB.Stub;
{$ENDIF}

{$mode ObjFPC}
{$H+}
{$ModeSwitch advancedrecords}

{off $define VerboseJOB}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Types, System.Math, System.Classes, System.Variants;
  {$ELSE}
  SysUtils, Types, Math, Classes, Variants;
  {$ENDIF}

const
  MinSafeIntDouble = -$1fffffffffffff; // -9007199254740991 54 bits (52 plus signed bit plus implicit highest bit)
  MaxSafeIntDouble =  $1fffffffffffff; //  9007199254740991

Type
//  TDOMHighResTimeStamp = Int64;
  TJOBObjectID = Integer;
  TJOBResult = integer;
  
  EJSObject = class(Exception);
  EJSInvoke = class(EJSObject)
  public
    ObjectID: Integer;
    FuncName: UTF8String;
  end;
  EJSArgParse = class(EJSObject);

  TJOB_JSValueKind = (
    jjvkUndefined,
    jjvkBoolean,
    jjvkDouble,
    jjvkString,
    jjvkObject,
    jjvkMethod,
    jjvkDictionary,
    jjvkArrayOfJSValue,
    jjvkArrayOfDouble
    );
  TJOB_JSValueKinds = set of TJOB_JSValueKind;

const
  JOB_JSValueKindNames: array[TJOB_JSValueKind] of UTF8String = (
    'Undefined',
    'Boolean',
    'Double',
    'String',
    'Object',
    'Method',
    'Dictionary',
    'ArrayOfJSValue',
    'ArrayOfDouble'
    );

  JOB_Undefined = Pointer(1);

type
  TUnicodeStringDynArray = array of UnicodeString;

  { TJOB_JSValue }

  TJOB_JSValue = class
  public
    Kind: TJOB_JSValueKind;
    constructor Create(aKind: TJOB_JSValueKind);
    function AsString: UTF8String; virtual;
  end;
  TJOB_JSValueClass = class of TJOB_JSValue;
  TJOB_JSValueArray = array of TJOB_JSValue;

  { TJOB_Boolean }

  TJOB_Boolean = class(TJOB_JSValue)
  public
    Value: Boolean;
    constructor Create(aValue: Boolean);
    function AsString: UTF8string; override;
  end;

  { TJOB_Double }

  TJOB_Double = class(TJOB_JSValue)
  public
    Value: Double;
    constructor Create(const aValue: Double);
    function AsString: UTF8String; override;
  end;

  { TJOB_String }

  TJOB_String = class(TJOB_JSValue)
  public
    Value: UnicodeString;
    constructor Create(const aValue: UnicodeString);
    function AsString: UTF8string; override;
  end;

  IJSObject = interface;

  { TJOB_Object }

  TJOB_Object = class(TJOB_JSValue)
  public
    Value: IJSObject;
    constructor Create(aValue: IJSObject);
    function AsString: UTF8String; override;
  end;

  TJOBInvokeType = (
    jiCall,  // call function
    jiGet, // read property
    jiGetTypeOf, // read property and do typeof
    jiSet, // write property
    jiNew // new operator
    );
  TJOBInvokeTypes = set of TJOBInvokeType;

  TJSObject = class;
  TJSArray = class;
  TJSObjectClass = class of TJSObject;


  { TJOB_Method }

  TJOB_Method = class(TJOB_JSValue)
  public
    Value: TMethod;
    constructor Create(const aMethod: TMethod);
    function AsString: UTF8String; override;
  end;

  TJOB_Pair = record
    Name: UnicodeString;
    Value: TJOB_JSValue;
  end;
  TJOB_PairArray = array of TJOB_Pair;

  { TJOB_Dictionary }

  TJOB_Dictionary = class(TJOB_JSValue)
  public
    Values: TJOB_PairArray;
    procedure Add(const aName: UnicodeString; const aValue: TJOB_JSValue);
    constructor Create(const Pairs: array of const);
    destructor Destroy; override;
    procedure Clear;
  end;

  TJOB_ArrayBase = class(TJOB_JSValue)
  end;

  { TJOB_ArrayOfJSValue }

  TJOB_ArrayOfJSValue = class(TJOB_ArrayBase)
  public
    Values: TJOB_JSValueArray;
    procedure Add(const aValue: TJOB_JSValue);
    constructor Create(const TheValues: array of const);
    destructor Destroy; override;
    procedure Clear;
  end;

  { TJOB_ArrayOfDouble }

  TJOB_ArrayOfDouble = class(TJOB_ArrayBase)
  public
    Values: TDoubleDynArray;
    constructor Create(const TheValues: TDoubleDynArray);
  end;

  IJSArray = interface;

  { IJSObject }

  IJSObject = interface
    ['{BE5CDE03-D471-4AB3-8F27-A5EA637416F7}']
    function GetPascalClassName: UTF8string;
    function GetProperties(const PropName: UTF8String): Variant; virtual;
    procedure SetProperties(const PropName: UTF8String; const AValue: Variant); virtual;
    // JS members
    function getOwnPropertyNames(const Obj: IJSObject): TUnicodeStringDynArray;
    function getPrototypeOf(const Obj: IJSObject): IJSObject;
    function hasOwnProperty(const PropName: UTF8String): boolean; virtual;
    function isPrototypeOf(const Obj: IJSObject): boolean; virtual;
    function propertyIsEnumerable(const PropName: UTF8String): boolean; virtual;
    function toLocaleString: UnicodeString; virtual; overload;
    function toString: RTLString; override; overload;
    function toUString: UnicodeString; virtual; overload;
    function valueOf: Variant; virtual; overload;
    property Properties[const PropName: UTF8String]: Variant read GetProperties write SetProperties; default;
  end;

  { TJSObject }

  TJSObject = class(TInterfacedObject,IJSObject)
  protected
    function GetPascalClassName: UTF8String;
    function GetProperties(const PropName: UTF8String): Variant;
    procedure SetProperties(const PropName: UTF8String; const AValue: Variant);
  public
    class function Cast(const Intf: IJSObject): IJSObject; overload;
    constructor Create; virtual;
    constructor CreateEmpty; virtual;
    destructor Destroy; override;
    // JS members
    function getOwnPropertyNames(const Obj: IJSObject): TUnicodeStringDynArray;
    function getPrototypeOf(const Obj: IJSObject): IJSObject;
    function hasOwnProperty(const PropName: UTF8String): boolean; virtual;
    function isPrototypeOf(const Obj: IJSObject): boolean; virtual;
    function propertyIsEnumerable(const PropName: UTF8String): boolean; virtual;
    function toLocaleString: UnicodeString; virtual; overload;
    function toString: RTLString; override; overload;
    function toUString: UnicodeString; virtual; overload;
    function valueOf: Variant; virtual; overload;
    property Properties[const PropName: UTF8String]: Variant read GetProperties write SetProperties; default;
  end;

  { IJSSet }

  IJSSet = interface(IJSObject)
    ['{1D276953-95E2-4B07-8D4E-BE70D1CEF356}']
  end;

  { TJSSet }

  TJSSet = class(TJSObject,IJSSet)
  public
    class function Cast(const Intf: IJSObject): IJSSet; overload;
  end;

  { IJSMap }

  IJSMap = interface(IJSObject)
    ['{D31F19A1-388E-4612-BC71-9392ECA90DA3}']
  end;

  { TJSMap }

  TJSMap = class(TJSObject,IJSMap)
  public
    class function Cast(const Intf: IJSObject): IJSMap; overload;
  end;

  { IJSFunction }

  IJSFunction = interface(IJSObject)
    ['{8BD36F12-F6F7-4F8B-91FB-43D8626A72FE}']
    function _GetLength: NativeInt;
    function _GetName: UnicodeString;
    function _GetPrototyp: IJSFunction;
    procedure _SetName(const AValue: UnicodeString);
    property name: UnicodeString read _GetName write _SetName;
    property prototyp: IJSFunction read _GetPrototyp;
    property length: NativeInt read _GetLength;
    //function apply(thisArg: TJSObject; const ArgArray: TJSValueDynArray): JSValue; varargs;
    //function bind(thisArg: TJSObject): JSValue; varargs;
    //function call(thisArg: TJSObject): JSValue; varargs;
  end;

  { TJSFunction }

  TJSFunction = class(TJSObject,IJSFunction)
  public
    function _GetLength: NativeInt;
    function _GetName: UnicodeString;
    function _GetPrototyp: IJSFunction;
    procedure _SetName(const AValue: UnicodeString);
    property name: UnicodeString read _GetName write _SetName;
    property prototyp: IJSFunction read _GetPrototyp;
    property length: NativeInt read _GetLength;
    class function Cast(const Intf: IJSObject): IJSFunction; overload;
  end;

  { IJSDate }

  IJSDate = interface(IJSObject)
    ['{F12818EA-542E-488C-A3C5-279E05639E9E}']
    function Create(aYear: NativeInt; aMonth: NativeInt; aDayOfMonth: NativeInt = 1;
      TheHours: NativeInt = 0; TheMinutes: NativeInt = 0; TheSeconds: NativeInt = 0;
      TheMilliseconds: NativeInt = 0): IJSDate;
    function toLocaleDateString: UnicodeString; overload; // date in locale timezone, no time
  end;

  { TJSDate }

  TJSDate = class(TJSObject,IJSDate)
  public
    class function Cast(const Intf: IJSObject): IJSDate; overload;
    function Create(aYear: NativeInt; aMonth: NativeInt; aDayOfMonth: NativeInt = 1;
      TheHours: NativeInt = 0; TheMinutes: NativeInt = 0; TheSeconds: NativeInt = 0;
      TheMilliseconds: NativeInt = 0): IJSDate; overload;
    function toLocaleDateString: UnicodeString; overload; // date in locale timezone, no time
  end;

  { IJSRegExp }

  IJSRegExp = interface(IJSObject)
    ['{3E9E4F54-10DA-45BF-ABED-7ED2C255617E}']
    function exec(const aString: UnicodeString): IJSArray;
    function _GetGlobal: Boolean;
    function _GetIgnoreCase: Boolean;
    function _GetLastIndex: NativeInt;
    function _GetMultiLine: Boolean;
    function _GetSource: UnicodeString;
    function _GetUnicode: boolean;
    procedure _SetGlobal(const AValue: Boolean);
    procedure _SetIgnoreCase(const AValue: Boolean);
    procedure _SetlastIndex(const AValue: NativeInt);
    procedure _SetMultiline(const AValue: Boolean);
    procedure _SetSource(const AValue: UnicodeString);
    procedure _SetUnicode(const AValue: boolean);
    function test(const aString: UnicodeString): boolean;
    property lastIndex: NativeInt read _GetLastIndex write _SetlastIndex;
    property global: Boolean read _GetGlobal write _SetGlobal;
    property ignoreCase: Boolean read _GetIgnoreCase write _SetIgnoreCase;
    property multiline: Boolean Read _GetMultiLine write _SetMultiline;
    property source: UnicodeString Read _GetSource write _SetSource;
    property unicode: boolean Read _GetUnicode write _SetUnicode;
  end;

  { TJSRegExp }

  TJSRegExp = class(TJSObject,IJSRegExp)
  public
    function exec(const aString: UnicodeString): IJSArray;
    function _GetGlobal: Boolean;
    function _GetIgnoreCase: Boolean;
    function _GetLastIndex: NativeInt;
    function _GetMultiLine: Boolean;
    function _GetSource: UnicodeString;
    function _GetUnicode: boolean;
    procedure _SetGlobal(const AValue: Boolean);
    procedure _SetIgnoreCase(const AValue: Boolean);
    procedure _SetlastIndex(const AValue: NativeInt);
    procedure _SetMultiline(const AValue: Boolean);
    procedure _SetSource(const AValue: UnicodeString);
    procedure _SetUnicode(const AValue: boolean);
    function test(const aString: UnicodeString): boolean;
    property lastIndex: NativeInt read _GetLastIndex write _SetlastIndex;
    property global: Boolean read _GetGlobal write _SetGlobal;
    property ignoreCase: Boolean read _GetIgnoreCase write _SetIgnoreCase;
    property multiline: Boolean Read _GetMultiLine write _SetMultiline;
    property source: UnicodeString Read _GetSource write _SetSource;
    property unicode: boolean Read _GetUnicode write _SetUnicode;
    class function Cast(const Intf: IJSObject): IJSRegExp; overload;
  end;

  { IJSString }

  IJSString = interface(IJSObject)
    ['{4C3B1B1C-4C0D-42A2-81BE-36CC78DCF9AE}']
  end;

  { TJSString }

  TJSString = class(TJSObject,IJSString)
  public
    class function Cast(const Intf: IJSObject): IJSString; overload;
  end;
  
  IJSIterator = interface (IJSObject) ['{21E331BA-7B57-42DD-8DCE-B26FEA85C639}']
  end;
  
  TJSIterator = class(TJSObject,IJSIterator)
  end;

  { IJSArray }

  IJSArray = interface(IJSObject)
    ['{21E331BA-7B57-42DD-8DCE-B26FEA85C693}']
    function _GetElements(Index: NativeInt): TJOB_JSValue;
    function _GetLength: NativeInt;
    procedure _SetElements(Index: NativeInt; const AValue: TJOB_JSValue);
    procedure _SetLength(const AValue: NativeInt);
    function isArray(a: TJOB_JSValue): Boolean; overload;
    function concat(el: TJOB_JSValue): IJSArray; overload; {varargs;}
    //function copyWithin(aTarget: NativeInt): TJSArray;overload; // not in IE
    //function copyWithin(aTarget, aStart: NativeInt): TJSArray;overload; // not in IE
    //function copyWithin(aTarget, aStart, aEnd: NativeInt): TJSArray;overload; // not in IE
    //function entries: TJSIterator;
    //Function every(const aCallback: TJSArrayCallBack): boolean;overload;
    //Function every(const aCallback: TJSArrayEvent; aThis: TObject): boolean;overload;
    //Function filter(const aCallBack: TJSArrayCallBack): TJSArray; overload;
    //Function filter(const aCallBack: TJSArrayEvent; aThis: TObject): TJSArray;overload;
    Function fill(aValue: TJOB_JSValue): IJSArray; overload;
    Function fill(aValue: TJOB_JSValue; aStartIndex: NativeInt): IJSArray; overload;
    Function fill(aValue: TJOB_JSValue; aStartIndex,aEndIndex: NativeInt): IJSArray; overload;
    //Function find(const aCallBack: TJSArrayCallBack): TJOB_JSValue; overload;
    //Function find(const aCallBack: TJSArrayEvent; aThis: TObject): TJOB_JSValue; overload;
    //Function findIndex(const aCallBack: TJSArrayCallBack): NativeInt; overload;
    //Function findIndex(const aCallBack: TJSArrayEvent; aThis: TObject): NativeInt; overload;
    //procedure forEach(const aCallBack: TJSArrayEventProc); overload;
    //procedure forEach(const aCallBack: TJSArrayEvent); overload;
    //procedure forEach(const aCallBack: TJSArrayEvent; aThis: TObject); overload;
    function includes(aElement: TJOB_JSValue): Boolean; overload;
    function includes(aElement: TJOB_JSValue; FromIndex: NativeInt): Boolean; overload;
    function indexOf(aElement: TJOB_JSValue): NativeInt; overload;
    function indexOf(aElement: TJOB_JSValue; FromIndex: NativeInt): NativeInt; overload;
    function join: UnicodeString; overload;
    function join (const aSeparator: UnicodeString): UnicodeString; overload;
    //function keys: TJSIterator;
    function lastIndexOf(aElement: TJOB_JSValue): NativeInt; overload;
    function lastIndexOf(aElement: TJOB_JSValue; FromIndex: NativeInt): NativeInt; overload;
    //Function map(const aCallBack: TJSArrayMapCallBack): TJSArray; overload;
    //Function map(const aCallBack: TJSArrayMapEvent; aThis: TObject): TJSArray; overload;
    function pop: TJOB_JSValue;
    function push(aElement: TJOB_JSValue): NativeInt; overload; {varargs;}
    //function reduce(const aCallBack: TJSArrayReduceCallBack): TJOB_JSValue; overload;
    //function reduce(const aCallBack: TJSArrayReduceCallBack; initialValue: TJOB_JSValue): TJOB_JSValue; overload;
    //function reduceRight(const aCallBack: TJSArrayReduceCallBack): TJOB_JSValue; overload;
    //function reduceRight(const aCallBack: TJSArrayReduceCallBack; initialValue: TJOB_JSValue): TJOB_JSValue; overload;
    Function reverse: IJSArray;
    Function shift: TJOB_JSValue;
    Function slice: IJSArray; overload;
    function slice(aBegin: NativeInt): IJSArray; overload;
    function slice(aBegin,aEnd: NativeInt): IJSArray; overload;
    //Function some(const aCallback: TJSArrayCallBack): boolean; overload;
    //Function some(const aCallback: TJSArrayEvent; aThis: TObject): boolean; overload;
    //Function sort(const aCallback: TJSArrayCompareCallBack): IJSArray; overload;
    Function sort(): IJSArray; overload;
    function splice(aStart: NativeInt): IJSArray; overload;
    function splice(aStart,aDeleteCount: NativeInt): IJSArray; {varargs;} overload;
    function toLocaleString(const locales: UnicodeString): UnicodeString; overload;
    //function toLocaleString(locales: string; const Options: TLocaleCompareOptions): String; overload;
    function unshift: NativeInt; {varargs;}
    //function values: TJSIterator;
    Property Length: NativeInt Read _GetLength Write _SetLength;
    property Elements[Index: NativeInt]: TJOB_JSValue read _GetElements write _SetElements; default;
  end;

  { TJSArray }

  TJSArray = class(TJSObject,IJSArray)
  private
    function _GetElements(Index: NativeInt): TJOB_JSValue;
    function _GetLength: NativeInt;
    procedure _SetElements(Index: NativeInt; const AValue: TJOB_JSValue);
    procedure _SetLength(const AValue: NativeInt);
  public
    function isArray(a: TJOB_JSValue): Boolean; overload;
    function concat(el: TJOB_JSValue): IJSArray; overload; {varargs;}
    //function copyWithin(aTarget: NativeInt): IJSArray;overload; // not in IE
    //function copyWithin(aTarget, aStart: NativeInt): IJSArray;overload; // not in IE
    //function copyWithin(aTarget, aStart, aEnd: NativeInt): IJSArray;overload; // not in IE
    //function entries: TJSIterator;
    //Function every(const aCallback: TJSArrayCallBack): boolean;overload;
    //Function every(const aCallback: TJSArrayEvent; aThis: TObject): boolean;overload;
    //Function filter(const aCallBack: TJSArrayCallBack): IJSArray; overload;
    //Function filter(const aCallBack: TJSArrayEvent; aThis: TObject): IJSArray;overload;
    Function fill(aValue: TJOB_JSValue): IJSArray; overload;
    Function fill(aValue: TJOB_JSValue; aStartIndex: NativeInt): IJSArray; overload;
    Function fill(aValue: TJOB_JSValue; aStartIndex,aEndIndex: NativeInt): IJSArray; overload;
    //Function find(const aCallBack: TJSArrayCallBack): TJOB_JSValue; overload;
    //Function find(const aCallBack: TJSArrayEvent; aThis: TObject): TJOB_JSValue; overload;
    //Function findIndex(const aCallBack: TJSArrayCallBack): NativeInt; overload;
    //Function findIndex(const aCallBack: TJSArrayEvent; aThis: TObject): NativeInt; overload;
    //procedure forEach(const aCallBack: TJSArrayEventProc); overload;
    //procedure forEach(const aCallBack: TJSArrayEvent); overload;
    //procedure forEach(const aCallBack: TJSArrayEvent; aThis: TObject); overload;
    function includes(aElement: TJOB_JSValue): Boolean; overload;
    function includes(aElement: TJOB_JSValue; FromIndex: NativeInt): Boolean; overload;
    function indexOf(aElement: TJOB_JSValue): NativeInt; overload;
    function indexOf(aElement: TJOB_JSValue; FromIndex: NativeInt): NativeInt; overload;
    function join: UnicodeString; overload;
    function join (const aSeparator: UnicodeString): UnicodeString; overload;
    //function keys: TJSIterator;
    function lastIndexOf(aElement: TJOB_JSValue): NativeInt; overload;
    function lastIndexOf(aElement: TJOB_JSValue; FromIndex: NativeInt): NativeInt; overload;
    //Function map(const aCallBack: TJSArrayMapCallBack): IJSArray; overload;
    //Function map(const aCallBack: TJSArrayMapEvent; aThis: TObject): IJSArray; overload;
    function pop: TJOB_JSValue;
    function push(aElement: TJOB_JSValue): NativeInt; overload; {varargs;}
    //function reduce(const aCallBack: TJSArrayReduceCallBack): TJOB_JSValue; overload;
    //function reduce(const aCallBack: TJSArrayReduceCallBack; initialValue: TJOB_JSValue): TJOB_JSValue; overload;
    //function reduceRight(const aCallBack: TJSArrayReduceCallBack): TJOB_JSValue; overload;
    //function reduceRight(const aCallBack: TJSArrayReduceCallBack; initialValue: TJOB_JSValue): TJOB_JSValue; overload;
    Function reverse: IJSArray;
    Function shift: TJOB_JSValue;
    Function slice: IJSArray; overload;
    function slice(aBegin: NativeInt): IJSArray; overload;
    function slice(aBegin,aEnd: NativeInt): IJSArray; overload;
    //Function some(const aCallback: TJSArrayCallBack): boolean; overload;
    //Function some(const aCallback: TJSArrayEvent; aThis: TObject): boolean; overload;
    //Function sort(const aCallback: TJSArrayCompareCallBack): IJSArray; overload;
    Function sort(): IJSArray; overload;
    function splice(aStart: NativeInt): IJSArray; overload;
    function splice(aStart,aDeleteCount: NativeInt): IJSArray; {varargs;} overload;
    function toLocaleString(const locales: UnicodeString): UnicodeString; overload;
    //function toLocaleString(locales: string; const Options: TLocaleCompareOptions): String; overload;
    function unshift: NativeInt; {varargs;}
    //function values: TJSIterator;
    Property Length: NativeInt Read _GetLength Write _SetLength;
    property Elements[Index: NativeInt]: TJOB_JSValue read _GetElements write _SetElements; default;
    class function Cast(const Intf: IJSObject): IJSArray; overload;
  end;

  { IJSArrayBuffer }

  IJSArrayBuffer = interface(IJSObject)
    ['{A1612EED-4F05-46C0-90BE-ACD511B15E89}']
  end;


  { TJSArrayBuffer }

  TJSArrayBuffer = class(TJSObject,IJSArrayBuffer)
  public
    class function Cast(const Intf: IJSObject): IJSArrayBuffer; overload;
  end;

  { IJSArrayBufferView }
  
  IJSArrayBufferView = interface(IJSObject)
    ['{A1612EED-4F05-46C0-90BE-ACD511B1598E}']
  end;
  
  { TJSArrayBufferView }
  
  TJSArrayBufferView = class(TJSObject,IJSArrayBufferView)
  public
    class function Cast(const Intf: IJSObject): IJSArrayBufferView; overload;
  end;


  { IJSTypedArray }

  IJSTypedArray = interface(IJSObject)
    ['{6A76602B-9555-4136-A7B7-2E683265EA82}']
  end;

  { TJSTypedArray }

  TJSTypedArray = class(TJSObject,IJSTypedArray)
  public
    class function Cast(const Intf: IJSObject): IJSTypedArray; overload;
  end;

  { IJSInt8Array }

  IJSInt8Array = interface(IJSTypedArray)
    ['{72D65C5E-E18E-4294-8709-D7A63BF12958}']
  end;

  { TJSInt8Array }

  TJSInt8Array = class(TJSTypedArray,IJSInt8Array)
  public
    class function Cast(const Intf: IJSObject): IJSInt8Array; overload;
  end;

  { IJSUint8Array }

  IJSUint8Array = interface(IJSTypedArray)
    ['{99EC7B3A-30E5-425F-933C-C169B2F4193C}']
  end;

  { TJSUint8Array }

  TJSUint8Array = class(TJSTypedArray,IJSUint8Array)
  public
    class function Cast(const Intf: IJSObject): IJSUint8Array; overload;
  end;

  { IJSUint8ClampedArray }

  IJSUint8ClampedArray = interface(IJSTypedArray)
    ['{A1508D6E-8629-4416-875E-9F669ECDC47F}']
  end;

  { TJSUint8ClampedArray }

  TJSUint8ClampedArray = class(TJSTypedArray,IJSUint8ClampedArray)
  public
    class function Cast(const Intf: IJSObject): IJSUint8ClampedArray; overload;
  end;

  { IJSInt16Array }

  IJSInt16Array = interface(IJSTypedArray)
    ['{B5FA7A13-D8CA-44E4-ADAE-F10FFFAE46B4}']
  end;

  { TJSInt16Array }

  TJSInt16Array = class(TJSTypedArray,IJSInt16Array)
  public
    class function Cast(const Intf: IJSObject): IJSInt16Array; overload;
  end;

  { IJSUint16Array }

  IJSUint16Array = interface(IJSTypedArray)
    ['{6023E2BC-C464-4288-A8DA-4A5D0B2B915E}']
  end;

  { TJSUint16Array }

  TJSUint16Array = class(TJSTypedArray,IJSUint16Array)
  public
    class function Cast(const Intf: IJSObject): IJSUint16Array; overload;
  end;

  { IJSInt32Array }

  IJSInt32Array = interface(IJSTypedArray)
    ['{16F1A6FB-2F26-4A64-8A2B-D883DE2F58C4}']
  end;

  { TJSInt32Array }

  TJSInt32Array = class(TJSTypedArray,IJSInt32Array)
  public
    class function Cast(const Intf: IJSObject): IJSInt32Array; overload;
  end;

  { IJSUint32Array }

  IJSUint32Array = interface(IJSTypedArray)
    ['{C637B2FA-CED6-4EC7-8D97-C56824EAF8B3}']
  end;

  { TJSUint32Array }

  TJSUint32Array = class(TJSTypedArray,IJSUint32Array)
  public
    class function Cast(const Intf: IJSObject): IJSUint32Array; overload;
  end;

  { IJSFloat32Array }

  IJSFloat32Array = interface(IJSTypedArray)
    ['{B5CE57F6-CA7C-4168-AEA3-32EF13DA52D6}']
  end;

  { TJSFloat32Array }

  TJSFloat32Array = class(TJSTypedArray,IJSFloat32Array)
  public
    class function Cast(const Intf: IJSObject): IJSFloat32Array; overload;
  end;

  { IJSFloat64Array }

  IJSFloat64Array = interface(IJSTypedArray)
    ['{A7876DC5-9549-4FDA-BE35-A641CE9D9F0B}']
  end;

  { TJSFloat64Array }

  TJSFloat64Array = class(TJSTypedArray,IJSFloat64Array)
  public
    class function Cast(const Intf: IJSObject): IJSFloat64Array; overload;
  end;

  { IJSBufferSource }

  IJSBufferSource = interface(IJSObject)
    ['{7F2A68EE-2FA6-445C-BFC1-2C9E4D45FFBF}']
  end;

  { TJSBufferSource }

  TJSBufferSource = class(TJSObject,IJSBufferSource)
  public
    class function Cast(const Intf: IJSObject): IJSBufferSource; overload;
  end;

  { IJSDataView }

  IJSDataView = interface(IJSObject)
    ['{42F14387-FAD2-46BA-8CB4-057445095CEE}']
  end;

  { TJSDataView }

  TJSDataView = class(TJSObject,IJSDataView)
  public
    class function Cast(const Intf: IJSObject): IJSDataView; overload;
  end;

  { IJSJSON }

  IJSJSON = interface(IJSObject)
    ['{73535059-91DD-4A22-91A6-D8072008C5F3}']
    function parse(const aJSON: UnicodeString): TJOB_JSValue; overload;
    // Use this only when you are sure you will get an object, no checking is done.
    function parseObject(const aJSON: UnicodeString): IJSObject; overload;
    function stringify(aValue: TJOB_JSValue): UnicodeString; overload;
    function stringify(aValue,aReplacer: TJOB_JSValue): UnicodeString; overload;
    function stringify(aValue,aReplacer: TJOB_JSValue; space:  NativeInt): UnicodeString; overload;
    function stringify(aValue,aReplacer: TJOB_JSValue; const space: UnicodeString): UnicodeString; overload;
  end;

  { TJSJSON }

  TJSJSON = class(TJSObject,IJSJSON)
  public
    function parse(const aJSON: UnicodeString): TJOB_JSValue; overload;
    // Use this only when you are sure you will get an object, no checking is done.
    function parseObject(const aJSON: UnicodeString): IJSObject; overload;
    function stringify(aValue: TJOB_JSValue): UnicodeString; overload;
    function stringify(aValue,aReplacer: TJOB_JSValue): UnicodeString; overload;
    function stringify(aValue,aReplacer: TJOB_JSValue; space:  NativeInt): UnicodeString; overload;
    function stringify(aValue,aReplacer: TJOB_JSValue; const space: UnicodeString): UnicodeString; overload;
    class function Cast(const Intf: IJSObject): IJSJSON; overload;
  end;

  { IJSError }

  IJSError = interface(IJSObject)
    ['{80532C4D-CAD2-4C70-A4EA-01B29BB8C2C8}']
  end;

  { TJSError }

  TJSError = class(TJSObject,IJSError)
  public
    class function Cast(const Intf: IJSObject): IJSError; overload;
  end;

  TJSPromiseResolver = function(const aValue: Variant): Variant of object;
  TJSPromiseExecutor = procedure(const OnResolve, OnReject: TJSPromiseResolver) of object;
  TJSPromiseFinallyHandler = procedure of object;

  { IJSPromise }

  IJSPromise = interface(IJSObject)
    ['{2BFE673B-B5D4-4F31-96CD-5E1A60EFBE26}']
    function all(const arg: Variant): IJSPromise; overload;
    function allSettled(const arg: Variant): IJSPromise; overload;
    function race(const arg: Variant): IJSPromise; overload;
    function reject(const reason: Variant): IJSPromise; overload;
    function resolve(const value: Variant): IJSPromise; overload;
    function resolve: IJSPromise; overload;
    function _then(const OnAccepted: TJSPromiseResolver): IJSPromise; overload;
    function _then(const OnAccepted, OnRejected: TJSPromiseResolver) : IJSPromise; overload;
    function catch(const OnRejected: TJSPromiseResolver): IJSPromise; overload;
    function _finally(const Handler: TJSPromiseFinallyHandler): IJSPromise; overload;
  end;

  { TJSPromise }

  TJSPromise = class(TJSObject,IJSPromise)
    FAccept : Boolean;
    FValue : Variant;
  public
    constructor create(aAccept : Boolean; aValue : Variant); overload;
    //class function Create(const Executor: TJSPromiseExecutor): IJSPromise; overload;
    function all(const arg: Variant): IJSPromise; overload;
    function allSettled(const arg: Variant): IJSPromise; overload;
    function race(const arg: Variant): IJSPromise; overload;
    function reject(const reason: Variant): IJSPromise; overload;
    function resolve(const value: Variant): IJSPromise; overload;
    function resolve: IJSPromise; overload;
    function _then(const OnAccepted: TJSPromiseResolver): IJSPromise; overload;
    function _then(const OnAccepted, OnRejected: TJSPromiseResolver) : IJSPromise; overload;
    function catch(const OnRejected: TJSPromiseResolver): IJSPromise; overload;
    function _finally(const Handler: TJSPromiseFinallyHandler): IJSPromise; overload;
  end;

  { IJSTextDecoder }

  IJSTextDecoder = interface(IJSObject)
    ['{EB42F04D-B92D-42AC-96F8-58DEC2F7F8D0}']
  end;

  { TJSTextDecoder }

  TJSTextDecoder = class(TJSObject,IJSTextDecoder)
  public
    class function Cast(const Intf: IJSObject): IJSTextDecoder; overload;
  end;

  { IJSTextEncoder }

  IJSTextEncoder = interface(IJSObject)
    ['{C2964DC1-E9AE-4321-99BD-EB788A7F2D9E}']
  end;

  { TJSTextEncoder }

  TJSTextEncoder = class(TJSObject,IJSTextEncoder)
  public
    class function Cast(const Intf: IJSObject): IJSTextEncoder; overload;
  end;

var
  JSObject: IJSObject; // singleton of JS 'Object'
  JSDate: IJSDate; // singleton of JS 'Date'

implementation


{ TJSTextEncoder }

class function TJSTextEncoder.Cast(const Intf: IJSObject): IJSTextEncoder;
begin
  Result:=Intf as IJSTextEncoder;
end;

{ TJSTextDecoder }

class function TJSTextDecoder.Cast(const Intf: IJSObject): IJSTextDecoder;
begin
  Result:=Intf as IJSTextDecoder;
end;

{ TJSPromise }

constructor tjspromise.create(aAccept : Boolean; aValue : Variant);
begin
 FValue:=aValue;
 FAccept:=aAccept;
end;
 
function TJSPromise.all(const arg: Variant): IJSPromise;
begin
  Result:=TJSPromise.Create;
end;

function TJSPromise.allSettled(const arg: Variant): IJSPromise;
begin
  Result:=TJSPromise.Create;
end;

function TJSPromise.race(const arg: Variant): IJSPromise;
begin
  Result:=TJSPromise.Create();
end;

function TJSPromise.reject(const reason: Variant): IJSPromise;
begin
  Result:=TJSPromise.Create(False,Reason);
end;

function TJSPromise.resolve(const value: Variant): IJSPromise;
begin
  Result:=TJSPromise.Create(True,Value);
end;

function TJSPromise.resolve: IJSPromise;
begin
  Result:=TJSPromise.Create(True,Unassigned);
end;

function TJSPromise._then(const OnAccepted: TJSPromiseResolver): IJSPromise;

begin
  if Faccept then 
    OnAccepted(FValue);
end;

function TJSPromise._then(const OnAccepted, OnRejected: TJSPromiseResolver
  ): IJSPromise;
var
  ma, mr: TJOB_Method;
begin
  if FAccept then
    OnAccepted(FValue)
  else
    OnRejected(FValue);  
end;

function TJSPromise.catch(const OnRejected: TJSPromiseResolver): IJSPromise;

begin
  //
end;

function TJSPromise._finally(const Handler: TJSPromiseFinallyHandler
  ): IJSPromise;
begin
  // 
end;

{ TJSError }

class function TJSError.Cast(const Intf: IJSObject): IJSError;
begin
  Result:=Intf as IJSError;
end;

{ TJSJSON }

function TJSJSON.parse(const aJSON: UnicodeString): TJOB_JSValue;
begin
  Result:=Default(TJOB_JSValue);
end;

function TJSJSON.parseObject(const aJSON: UnicodeString): IJSObject;
begin
  Result:=TJSObject.Create;
end;

function TJSJSON.stringify(aValue: TJOB_JSValue): UnicodeString;
begin
  Result:='';
end;

function TJSJSON.stringify(aValue, aReplacer: TJOB_JSValue): UnicodeString;
begin
  Result:='';
end;

function TJSJSON.stringify(aValue, aReplacer: TJOB_JSValue; space: NativeInt
  ): UnicodeString;
begin
  Result:='';
end;

function TJSJSON.stringify(aValue, aReplacer: TJOB_JSValue;
  const space: UnicodeString): UnicodeString;
begin
  Result:='';
end;

class function TJSJSON.Cast(const Intf: IJSObject): IJSJSON;
begin
  Result:=Intf as IJSJSON;
end;

{ TJSDataView }

class function TJSDataView.Cast(const Intf: IJSObject): IJSDataView;
begin
  Result:=TJSDataView.Cast(Intf);
end;

{ TJSBufferSource }

class function TJSBufferSource.Cast(const Intf: IJSObject): IJSBufferSource;
begin
  Result:=Intf as IJSBufferSource;
end;

{ TJSFloat64Array }

class function TJSFloat64Array.Cast(const Intf: IJSObject): IJSFloat64Array;
begin
  Result:=Intf as IJSFloat64Array;
end;

{ TJSFloat32Array }

class function TJSFloat32Array.Cast(const Intf: IJSObject): IJSFloat32Array;
begin
  Result:=Intf as IJSFloat32Array;
end;

{ TJSUint32Array }

class function TJSUint32Array.Cast(const Intf: IJSObject): IJSUint32Array;
begin
  Result:=Intf as IJSUint32Array;
end;

{ TJSInt32Array }

class function TJSInt32Array.Cast(const Intf: IJSObject): IJSInt32Array;
begin
  Result:=Intf as IJSInt32Array;
end;

{ TJSUint16Array }

class function TJSUint16Array.Cast(const Intf: IJSObject): IJSUint16Array;
begin
  Result:=Intf as IJSUint16Array;
end;

{ TJSInt16Array }

class function TJSInt16Array.Cast(const Intf: IJSObject): IJSInt16Array;
begin
  Result:=Intf as IJSInt16Array
end;

{ TJSUint8ClampedArray }

class function TJSUint8ClampedArray.Cast(const Intf: IJSObject
  ): IJSUint8ClampedArray;
begin
  Result:=Intf as IJSUint8ClampedArray;
end;

{ TJSUInt8Array }

class function TJSUint8Array.Cast(const Intf: IJSObject): IJSUint8Array;
begin
  Result:=Intf as IJSUint8Array;
end;

{ TJSInt8Array }

class function TJSInt8Array.Cast(const Intf: IJSObject): IJSInt8Array;
begin
  Result:=Intf as IJSInt8Array;
end;

{ TJSTypedArray }

class function TJSTypedArray.Cast(const Intf: IJSObject): IJSTypedArray;
begin
  Result:=Intf as IJSTypedArray;
end;

{ TJSArrayBuffer }

class function TJSArrayBuffer.Cast(const Intf: IJSObject): IJSArrayBuffer;
begin
  Result:=Intf as IJSArrayBuffer;
end;

{ TJSArrayBufferView }

class function TJSArrayBufferView.Cast(const Intf: IJSObject): IJSArrayBufferView;
begin
  Result:=Intf as IJSArrayBufferView;
end;




{ TJSArray }

function TJSArray._GetElements(Index: NativeInt): TJOB_JSValue;
begin
  Result:=TJOB_JSValue.Create(jjvkUndefined);
end;

function TJSArray._GetLength: NativeInt;
begin
  Result:=0;
end;

procedure TJSArray._SetElements(Index: NativeInt; const AValue: TJOB_JSValue);
begin
  // 
end;

procedure TJSArray._SetLength(const AValue: NativeInt);
begin
//  FLength:=aValue;
end;

function TJSArray.isArray(a: TJOB_JSValue): Boolean;
begin
  Result:=True;
end;

function TJSArray.concat(el: TJOB_JSValue): IJSArray;
begin
  Result:=Self;
end;

function TJSArray.fill(aValue: TJOB_JSValue): IJSArray;
begin
  Result:=Self;
end;

function TJSArray.fill(aValue: TJOB_JSValue; aStartIndex: NativeInt): IJSArray;
begin
  Result:=Self;
end;

function TJSArray.fill(aValue: TJOB_JSValue; aStartIndex, aEndIndex: NativeInt
  ): IJSArray;
begin
  Result:=Self;
end;

function TJSArray.includes(aElement: TJOB_JSValue): Boolean;
begin
  Result:=False;
end;

function TJSArray.includes(aElement: TJOB_JSValue; FromIndex: NativeInt
  ): Boolean;
begin
  Result:=False;
end;

function TJSArray.indexOf(aElement: TJOB_JSValue): NativeInt;
begin
  Result:=-1;
end;

function TJSArray.indexOf(aElement: TJOB_JSValue; FromIndex: NativeInt
  ): NativeInt;
begin
  Result:=-1;
end;

function TJSArray.join: UnicodeString;
begin
  Result:='';
end;

function TJSArray.join(const aSeparator: UnicodeString): UnicodeString;
begin
  Result:='';
end;

function TJSArray.lastIndexOf(aElement: TJOB_JSValue): NativeInt;
begin
  Result:=-1;
end;

function TJSArray.lastIndexOf(aElement: TJOB_JSValue; FromIndex: NativeInt
  ): NativeInt;
begin
  Result:=-1;
end;

function TJSArray.pop: TJOB_JSValue;
begin
  Result:=TJOB_JSValue.Create(jjvkUndefined);
end;

function TJSArray.push(aElement: TJOB_JSValue): NativeInt;
begin
  Result:=0;
end;

function TJSArray.reverse: IJSArray;
begin
  Result:=Self;
end;

function TJSArray.shift: TJOB_JSValue;
begin
  Result:=TJOB_JSValue.Create(jjvkUndefined);
end;

function TJSArray.slice: IJSArray;
begin
  Result:=Self;
end;

function TJSArray.slice(aBegin: NativeInt): IJSArray;
begin
  Result:=Self;
end;

function TJSArray.slice(aBegin, aEnd: NativeInt): IJSArray;
begin
  Result:=Self;
end;

function TJSArray.sort(): IJSArray;
begin
  Result:=Self;
end;

function TJSArray.splice(aStart: NativeInt): IJSArray;
begin
  Result:=Self;
end;

function TJSArray.splice(aStart, aDeleteCount: NativeInt): IJSArray;
begin
  Result:=Self;
end;

function TJSArray.toLocaleString(const locales: UnicodeString): UnicodeString;
begin
  Result:='';
end;

function TJSArray.unshift: NativeInt;
begin
  Result:=0;
end;

class function TJSArray.Cast(const Intf: IJSObject): IJSArray;
begin
  Result:=Intf as IJSArray;
end;

{ TJSString }

class function TJSString.Cast(const Intf: IJSObject): IJSString;
begin
  Result:=Intf as IJSString;
end;

{ TJSRegExp }

function TJSRegExp.exec(const aString: UnicodeString): IJSArray;
begin
  Result:=Nil;
end;

function TJSRegExp._GetGlobal: Boolean;
begin
  Result:=False
end;

function TJSRegExp._GetIgnoreCase: Boolean;
begin
  Result:=True;
end;

function TJSRegExp._GetLastIndex: NativeInt;
begin
  Result:=-1;
end;

function TJSRegExp._GetMultiLine: Boolean;
begin
  Result:=False;
end;

function TJSRegExp._GetSource: UnicodeString;
begin
  Result:='';
end;

function TJSRegExp._GetUnicode: boolean;
begin
  Result:=False;
end;

procedure TJSRegExp._SetGlobal(const AValue: Boolean);
begin
  // 
end;

procedure TJSRegExp._SetIgnoreCase(const AValue: Boolean);
begin
  //
end;

procedure TJSRegExp._SetlastIndex(const AValue: NativeInt);
begin
  // WriteertyLongInt('lastIndex',AValue);
end;

procedure TJSRegExp._SetMultiline(const AValue: Boolean);
begin
  // WriteertyBoolean('multiline',AValue);
end;

procedure TJSRegExp._SetSource(const AValue: UnicodeString);
begin
  // WriteertyUnicodeString('source',AValue);
end;

procedure TJSRegExp._SetUnicode(const AValue: boolean);
begin
  // WriteertyBoolean('unicode',AValue);
end;

function TJSRegExp.test(const aString: UnicodeString): boolean;
begin
  Result:=False;
end;

class function TJSRegExp.Cast(const Intf: IJSObject): IJSRegExp;
begin
  Result:=Intf as IJSRegExp;
end;

{ TJSFunction }

function TJSFunction._GetLength: NativeInt;
begin
  Result:=0;
end;

function TJSFunction._GetName: UnicodeString;
begin
  Result:='';
end;

function TJSFunction._GetPrototyp: IJSFunction;
begin
  Result:=Self;
end;

procedure TJSFunction._SetName(const AValue: UnicodeString);
begin
  // WriteertyUnicodeString('length',AValue);
end;

class function TJSFunction.Cast(const Intf: IJSObject): IJSFunction;
begin
  Result:=Intf As IJSFunction;
end;

{ TJSMap }

class function TJSMap.Cast(const Intf: IJSObject): IJSMap;
begin
  Result:=Intf as IJSMap;
end;

{ TJSSet }

class function TJSSet.Cast(const Intf: IJSObject): IJSSet;
begin
  Result:=Intf as IJSSet;
end;

{ TJOB_Boolean }

constructor TJOB_Boolean.Create(aValue: Boolean);
begin
  Kind:=jjvkBoolean;
  Value:=aValue;
end;

function TJOB_Boolean.AsString: UTF8String;
begin
  str(Value,Result);
end;

{ TJOB_Double }

constructor TJOB_Double.Create(const aValue: Double);
begin
  Kind:=jjvkDouble;
  Value:=aValue;
end;

function TJOB_Double.AsString: UTF8string;
begin
  str(Value,Result);
end;

{ TJOB_String }

constructor TJOB_String.Create(const aValue: UnicodeString);
begin
  Kind:=jjvkString;
  Value:=aValue;
end;

function TJOB_String.AsString: UTF8string;
begin
  Result:=AnsiQuotedStr(String(Value),'"');
end;

{ TJOB_Object }

constructor TJOB_Object.Create(aValue: IJSObject);
begin
  Kind:=jjvkObject;
  Value:=aValue;
end;

function TJOB_Object.AsString: UTF8string;
begin
  if Value=nil then
    Result:='nil'
  else
    Result:='[]';
end;

{ TJOB_Method }

constructor TJOB_Method.Create(const aMethod: TMethod);
begin
  Kind:=jjvkMethod;
  Value:=aMethod;
end;

function TJOB_Method.AsString: UTF8string;
begin
  Result:='Callback';
end;

{ TJOB_Dictionary }

procedure TJOB_Dictionary.Add(const aName: UnicodeString;
  const aValue: TJOB_JSValue);
begin
end;

constructor TJOB_Dictionary.Create(const Pairs: array of const);
begin
  inherited Create(jjvkDictionary);
end;

destructor TJOB_Dictionary.Destroy;
begin
  inherited Destroy;
end;

procedure TJOB_Dictionary.Clear;
begin
end;

{ TJOB_ArrayOfJSValue }

procedure TJOB_ArrayOfJSValue.Add(const aValue: TJOB_JSValue);
begin
end;

constructor TJOB_ArrayOfJSValue.Create(const TheValues: array of const);
begin
end;

destructor TJOB_ArrayOfJSValue.Destroy;
begin
  inherited Destroy;
end;

procedure TJOB_ArrayOfJSValue.Clear;
begin
end;

{ TJOB_ArrayOfDouble }

constructor TJOB_ArrayOfDouble.Create(const TheValues: TDoubleDynArray);
begin
end;

{ TJSObject }


function TJSObject.GetPascalClassName: UTF8String;
begin
  Result:=ClassName;
end;

function TJSObject.GetProperties(const PropName: UTF8String): Variant;
begin
  //Variant(PropName);
end;

procedure TJSObject.SetProperties(const PropName: UTF8String; const AValue: Variant);
begin
  // WriteertyVariant(PropName,AValue);
end;



class function TJSObject.Cast(const Intf: IJSObject): IJSObject;
begin
  Result:=Intf;
end;

constructor TJSObject.Create;

begin
end;

constructor TJSObject.CreateEmpty;

begin
end;

destructor TJSObject.Destroy;
begin
  inherited Destroy;
end;



function TJSObject.getOwnPropertyNames(const Obj: IJSObject
  ): TUnicodeStringDynArray;
begin
  Result:=[];
end;

function TJSObject.getPrototypeOf(const Obj: IJSObject): IJSObject;
begin
  Result:=Self;
end;

function TJSObject.hasOwnProperty(const PropName: UTF8String): boolean;
begin
  Result:=False;
end;

function TJSObject.isPrototypeOf(const Obj: IJSObject): boolean;
begin
  Result:=False;
end;

function TJSObject.propertyIsEnumerable(const PropName: UTF8String): boolean;
begin
  Result:=False;
end;

function TJSObject.toLocaleString: UnicodeString;
begin
  Result:='';
end;

function TJSObject.toString: RTLString;
begin
  Result:=''
end;

function TJSObject.toUString: UnicodeString;
begin
  Result:=''
end;

function TJSObject.valueOf: Variant;
begin
  Result:=Unassigned;
end;

{ TJSDate }

class function TJSDate.Cast(const Intf: IJSObject): IJSDate;
begin
  Result:=Intf as IJSDate;
end;

function TJSDate.Create(aYear: NativeInt; aMonth: NativeInt;
  aDayOfMonth: NativeInt; TheHours: NativeInt; TheMinutes: NativeInt;
  TheSeconds: NativeInt; TheMilliseconds: NativeInt): IJSDate;
begin
  Result:=Self;
end;

function TJSDate.toLocaleDateString: UnicodeString;
begin
  Result:=''
end;

constructor TJOB_JSValue.Create(aKind: TJOB_JSValueKind);

begin
  Kind:=aKind;
end;

function TJOB_JSValue.AsString : UTF8String;

begin
  Result:='';
end;

initialization
  JSObject:=TJSObject.Create;
  JSDate:=TJSDate.Create;

end.

