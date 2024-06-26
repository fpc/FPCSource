{
  JOB - JS Object Bridge for Webassembly

  Webassembly unit giving access to the browser DOM.

  see https://wiki.freepascal.org/WebAssembly/DOM
}
{$IFNDEF FPC_DOTTEDUNITS}
unit job.js;
{$ENDIF}

{$mode ObjFPC}
{$H+}
{$ModeSwitch advancedrecords}

{ $define VerboseJOB}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Types, System.Math, System.Classes, System.Variants, Wasm.Job.Shared;
  {$ELSE}
  SysUtils, Types, Math, Classes, Variants, JOB.Shared;
  {$ENDIF}

const
  MinSafeIntDouble = -$1fffffffffffff; // -9007199254740991 54 bits (52 plus signed bit plus implicit highest bit)
  MaxSafeIntDouble =  $1fffffffffffff; //  9007199254740991

Type
//  TDOMHighResTimeStamp = Int64;

  PJOBObjectID = ^TJOBObjectID;

  EJSObject = class(Exception);
  EJSInvoke = class(EJSObject)
  public
    ObjectID: TJOBObjectID;
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
    jjvkArrayOfDouble,
    jjvkArrayOfByte
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
    'ArrayOfDouble',
    'ArrayOfByte'
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
    function AsVariant : Variant; virtual;
  end;
  TJOB_JSValueClass = class of TJOB_JSValue;
  TJOB_JSValueArray = array of TJOB_JSValue;

  { TJOB_Boolean }

  TJOB_Boolean = class(TJOB_JSValue)
  public
    Value: Boolean;
    constructor Create(aValue: Boolean);
    function AsString: UTF8string; override;
    function AsVariant : Variant; override;
  end;

  { TJOB_Double }

  TJOB_Double = class(TJOB_JSValue)
  public
    Value: Double;
    constructor Create(const aValue: Double);
    function AsString: UTF8String; override;
    function AsVariant : Variant; override;
  end;

  { TJOB_String }

  TJOB_String = class(TJOB_JSValue)
  public
    Value: UnicodeString;
    constructor Create(const aValue: UnicodeString);
    function AsString: UTF8string; override;
    function AsVariant : Variant; override;
  end;


  IJSObject = interface;

  { TJOB_Object }

  TJOB_Object = class(TJOB_JSValue)
  public
    Value: IJSObject;
    constructor Create(aValue: IJSObject);
    function AsString: UTF8String; override;
    function AsVariant : Variant; override;
  end;

  { TJOB_Function }
  IJSFunction = interface;

  TJOB_Function = class(TJOB_JSValue)
  public
    Value: IJSFunction;
    constructor Create(aValue: IJSFunction);
    function AsString: UTF8String; override;
    function AsVariant : Variant; override;
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
  TJSFunction = class;
  TJSObjectClass = class of TJSObject;

  { TJOBCallbackHelper - parse callback arguments and create result }

  TJOBCallbackHelper = record
    p: PByte;
    Index: integer;
    Count: integer;
    procedure Init(Args: PByte);
    function GetType: byte; // see JOBArg* constants, keeps p
    procedure Skip;
    function GetBoolean: boolean;
    function GetDouble: double;
    function GetString: UnicodeString;
    function GetObject(aResultClass: TJSObjectClass): TJSObject;
    function GetValue: TJOB_JSValue;
    function GetVariant: Variant;
    function GetLongInt: longint;
    function GetMaxInt: int64;
    function GetArray : TJSArray;
    function GetFunction : IJSFunction;

    function AllocUndefined: PByte;
    function AllocBool(b: boolean): PByte;
    function AllocLongint(i: longint): PByte;
    function AllocDouble(const d: double): PByte;
    function AllocString(const s: UnicodeString): PByte;
    function AllocNil: PByte;
    function AllocIntf(const Intf: IJSObject): PByte;
    function AllocObject(Obj: TJSObject): PByte;
    function AllocObjId(ObjId: TJOBObjectID): PByte;
    function AllocJSValue(const Value: TJOB_JSValue): PByte;
    function AllocVariant(const Value: Variant): PByte;
  end;

  TJOBCallback = function(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;

  { TJOB_Method }

  TJOB_Method = class(TJOB_JSValue)
  public
    Value: TMethod;
    Invoke: TJOBCallback;
    constructor Create(const aMethod: TMethod; const AnInvoke: TJOBCallback);
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
    function AsVariant : Variant; override;
  end;

  { TJOB_ArrayOfDouble }

  TJOB_ArrayOfDouble = class(TJOB_ArrayBase)
  public
    Values: TDoubleDynArray;
    constructor Create(const TheValues: TDoubleDynArray);
    function AsVariant : Variant; override;
  end;

  { TJOB_ArrayOfDouble }

  { TJOB_ArrayOfByte }

  TJOB_ArrayOfByte = class(TJOB_ArrayBase)
  public
    Values: PByte;
    Len : NativeUInt;
    constructor Create(const TheValues: PByte; TheLen : NativeUInt);
    constructor Create(const TheValues: TBytes);
    function AsVariant : Variant; override;
  end;


  IJSArray = interface;

  { IJSObject }

  IJSObject = interface
    ['{BE5CDE03-D471-4AB3-8F27-A5EA637416F7}']
    function GetJSObjectID: TJOBObjectID;
    function GetJSObjectCastSrc: IJSObject;
    function GetPascalClassName: UTF8string;
    function GetProperties(const PropName: UTF8String): Variant; virtual;
    procedure SetProperties(const PropName: UTF8String; const AValue: Variant); virtual;
    // call a function
    procedure InvokeJSNoResult(const aName: UTF8String; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall); virtual;
    function InvokeJSBooleanResult(const aName: UTF8String; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): Boolean; virtual;
    function InvokeJSDoubleResult(const aName: UTF8String; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): Double; virtual;
    function InvokeJSUnicodeStringResult(const aName: UTF8String; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): UnicodeString; virtual;
    function InvokeJSObjectResult(const aName: UTF8String; Const Args: Array of const; aResultClass: TJSObjectClass; Invoke: TJOBInvokeType = jiCall): TJSObject; virtual;
    function InvokeJSValueResult(const aName: UTF8String; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): TJOB_JSValue; virtual;
    function InvokeJSVariantResult(const aName: UTF8String; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): Variant; virtual;
    function InvokeJSUtf8StringResult(const aName: UTF8String; Const args: Array of const; Invoke: TJOBInvokeType = jiCall): UTF8String; virtual;
    function InvokeJSLongIntResult(const aName: UTF8String; Const args: Array of const; Invoke: TJOBInvokeType = jiCall): LongInt; virtual;
    function InvokeJSTypeOf(const aName: UTF8String; Const Args: Array of const): TJOBResult; virtual;
    function InvokeJSUnicodeStringArrayResult(const aName: UTF8String; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): TUnicodeStringDynArray; virtual;
    // read a property
    function ReadJSPropertyBoolean(const aName: UTF8String): boolean; virtual;
    function ReadJSPropertyDouble(const aName: UTF8String): double; virtual;
    function ReadJSPropertyUnicodeString(const aName: UTF8String): UnicodeString; virtual;
    function ReadJSPropertyObject(const aName: UTF8String; aResultClass: TJSObjectClass): TJSObject; virtual;
    function ReadJSPropertyUtf8String(const aName: UTF8String): UTF8String; virtual;
    function ReadJSPropertyLongInt(const aName: UTF8String): LongInt; virtual;
    function ReadJSPropertyInt64(const aName: UTF8String): Int64; virtual;
    function ReadJSPropertyValue(const aName: UTF8String): TJOB_JSValue; virtual;
    function ReadJSPropertyVariant(const aName: UTF8String): Variant; virtual;
    function ReadJSPropertyMethod(const aName: UTF8String): TMethod; virtual;
    // write a property
    procedure WriteJSPropertyBoolean(const aName: UTF8String; Value: Boolean); virtual;
    procedure WriteJSPropertyDouble(const aName: UTF8String; Value: Double); virtual;
    procedure WriteJSPropertyUnicodeString(const aName: UTF8String; const Value: UnicodeString); virtual;
    procedure WriteJSPropertyUtf8String(const aName: UTF8String; const Value: UTF8String); virtual;
    procedure WriteJSPropertyObject(const aName: UTF8String; Value: IJSObject); virtual;
    procedure WriteJSPropertyLongInt(const aName: UTF8String; Value: LongInt); virtual;
    procedure WriteJSPropertyInt64(const aName: UTF8String; Value: Int64); virtual;
    procedure WriteJSPropertyValue(const aName: UTF8String; Value: TJOB_JSValue); virtual;
    procedure WriteJSPropertyVariant(const aName: UTF8String; const Value: Variant); virtual;
    procedure WriteJSPropertyMethod(const aName: UTF8String; const Value: TMethod); virtual;
    // create a new object using the new-operator
    function NewJSObject(Const Args: Array of const; aResultClass: TJSObjectClass): TJSObject; virtual;
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
  private
    FJOBObjectID: TJOBObjectID;
    FJOBCastSrc: IJSObject;
    FJOBObjectIDOwner: boolean;
  protected
    type
      TJOBInvokeNoResultFunc = function(
          ObjID: TJOBObjectID;
          NameP: PChar;
          NameLen: longint;
          Invoke: longint;
          ArgP: PByte
        ): TJOBResult;
      TJOBInvokeOneResultFunc = function(
          ObjID: TJOBObjectID;
          NameP: PChar;
          NameLen: longint;
          Invoke: longint;
          ArgP: PByte;
          ResultP: PByte
        ): TJOBResult;
    function GetJSObjectID: TJOBObjectID;
    function GetJSObjectCastSrc: IJSObject;
    function GetPascalClassName: UTF8String;
    function FetchString(Len: NativeInt): UnicodeString;
    function InvokeJSNoResultFunc(const aName: UTF8string; Const Args: Array of const;
      const InvokeFunc: TJOBInvokeNoResultFunc; Invoke: TJOBInvokeType): TJOBResult;
    function InvokeJSOneResult(const aName: UTF8string; Const Args: Array of const;
      const InvokeFunc: TJOBInvokeOneResultFunc; ResultP: PByte; Invoke: TJOBInvokeType): TJOBResult;
    procedure InvokeJS_Raise(const aName, Msg: UTF8string); virtual;
    procedure InvokeJS_RaiseResultMismatch(const aName: UTF8string; Expected, Actual: TJOBResult); virtual;
    procedure InvokeJS_RaiseResultMismatchStr(const aName: UTF8string; const Expected, Actual: UTF8string); virtual;
    function CreateInvokeJSArgs(const Args: array of const): PByte; virtual;
    function GetProperties(const PropName: UTF8String): Variant;
    procedure SetProperties(const PropName: UTF8String; const AValue: Variant);
  public
    constructor JOBCast(const Intf: IJSObject); overload;
    constructor JOBCreateFromID(aID: TJOBObjectID); virtual; // use this only for the owner (it will release it on free)
    constructor JOBCreateGlobal(const aID: UnicodeString); virtual;
    constructor JOBCreate(const Args : Array of const);
    constructor JOBCreate(aOwnsObjectID : Boolean; const Args : Array of const);
    class function JSClassName : UnicodeString; virtual;
    class function Cast(const Intf: IJSObject): IJSObject; overload;
    constructor Create; virtual;
    destructor Destroy; override;
    property JOBObjectID: TJOBObjectID read FJOBObjectID;
    property JOBObjectIDOwner: boolean read FJOBObjectIDOwner write FJOBObjectIDOwner;
    property JOBCastSrc: IJSObject read FJOBCastSrc; // nil means it is the original, otherwise it is a typecast
    // call a function
    procedure InvokeJSNoResult(const aName: UTF8String; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall); virtual;
    function InvokeJSBooleanResult(const aName: UTF8string; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): Boolean; virtual;
    function InvokeJSDoubleResult(const aName: UTF8string; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): Double; virtual;
    function InvokeJSUnicodeStringResult(const aName: UTF8string; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): UnicodeString; virtual;
    function InvokeJSObjectResult(const aName: UTF8String; Const Args: Array of const; aResultClass: TJSObjectClass; Invoke: TJOBInvokeType = jiCall): TJSObject; virtual;
    function InvokeJSValueResult(const aName: UTF8String; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): TJOB_JSValue; virtual;
    function InvokeJSVariantResult(const aName: UTF8string; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): Variant; virtual;
    function InvokeJSUtf8StringResult(const aName: UTF8string; Const args: Array of const; Invoke: TJOBInvokeType = jiCall): UTF8String; virtual;
    function InvokeJSLongIntResult(const aName: UTF8String; Const args: Array of const; Invoke: TJOBInvokeType = jiCall): LongInt; virtual;
    function InvokeJSMaxIntResult(const aName: UTF8String; Const args: Array of const; Invoke: TJOBInvokeType = jiCall): int64; virtual;
    function InvokeJSTypeOf(const aName: UTF8String; Const Args: Array of const): TJOBResult; virtual;
    function InvokeJSUnicodeStringArrayResult(const aName: UTF8String; Const Args: Array of const; Invoke: TJOBInvokeType = jiCall): TUnicodeStringDynArray; virtual;
    // read a property
    function ReadJSPropertyBoolean(const aName: UTF8String): boolean; virtual;
    function ReadJSPropertyDouble(const aName: UTF8String): double; virtual;
    function ReadJSPropertyUnicodeString(const aName: UTF8String): UnicodeString; virtual;
    function ReadJSPropertyObject(const aName: UTF8String; aResultClass: TJSObjectClass): TJSObject; virtual;
    function ReadJSPropertyUtf8String(const aName: UTF8String): UTF8String; virtual;
    function ReadJSPropertyLongInt(const aName: UTF8String): LongInt; virtual;
    function ReadJSPropertyInt64(const aName: UTF8String): Int64; virtual;
    function ReadJSPropertyValue(const aName: UTF8String): TJOB_JSValue; virtual;
    function ReadJSPropertyVariant(const aName: UTF8String): Variant; virtual;
    function ReadJSPropertyMethod(const aName: UTF8String): TMethod; virtual;
    // write a property
    procedure WriteJSPropertyBoolean(const aName: UTF8String; Value: Boolean); virtual;
    procedure WriteJSPropertyDouble(const aName: UTF8String; Value: Double); virtual;
    procedure WriteJSPropertyUnicodeString(const aName: UTF8String; const Value: UnicodeString); virtual;
    procedure WriteJSPropertyUtf8String(const aName: UTF8String; const Value: UTF8String); virtual;
    procedure WriteJSPropertyObject(const aName: UTF8String; Value: IJSObject); virtual;
    procedure WriteJSPropertyLongInt(const aName: UTF8String; Value: LongInt); virtual;
    procedure WriteJSPropertyInt64(const aName: UTF8String; Value: Int64); virtual;
    procedure WriteJSPropertyValue(const aName: UTF8String; Value: TJOB_JSValue); virtual;
    procedure WriteJSPropertyVariant(const aName: UTF8String; const Value: Variant); virtual;
    procedure WriteJSPropertyMethod(const aName: UTF8String; const Value: TMethod); virtual;
    // create a new object using the new-operator
    function NewJSObject(Const Args: Array of const; aResultClass: TJSObjectClass): TJSObject; virtual;
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
    function apply(thisArg: TJSObject; const ArgArray: Array of const): Variant;
    function apply(const ArgArray: Array of const): Variant;
    //function bind(thisArg: TJSObject): JSValue; varargs;
    //function call(thisArg: TJSObject): JSValue; varargs;
  end;

  { TJSFunction }

  TJSFunction = class(TJSObject,IJSFunction)
  private
    FThisID: TJOBObjectID;
  public
    Constructor Create(aObjectID : TJOBObjectID);
    Constructor Create(aObjectID,aThisID : TJOBObjectID);
    destructor Destroy; override;
    function _GetLength: NativeInt;
    function _GetName: UnicodeString;
    function _GetPrototyp: IJSFunction;
    procedure _SetName(const AValue: UnicodeString);
    property name: UnicodeString read _GetName write _SetName;
    property prototyp: IJSFunction read _GetPrototyp;
    property length: NativeInt read _GetLength;
    function apply(thisArg: TJSObject; const ArgArray: Array of const): Variant;
    function apply(const ArgArray: Array of const): Variant;
    class function Cast(const Intf: IJSObject): IJSFunction; overload;
    Property ThisID : TJOBObjectID Read FThisID Write FThisID;
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
      TheMilliseconds: NativeInt = 0): IJSDate;
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
    constructor Create(aArgs : Array of const); overload;
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
    class function JSClassName: UnicodeString; override;
  end;

  { IJSArrayBuffer }

  IJSArrayBuffer = interface(IJSObject)
    ['{A1612EED-4F05-46C0-90BE-ACD511B15E89}']
    function Slice : IJSArrayBuffer;
    function Slice (aStart : NativeInt): IJSArrayBuffer;
    function Slice (aStart,aEndExclusive : NativeInt): IJSArrayBuffer;
    function _getByteLength: Nativeint;
    function _getDetached: Boolean;
    function _getMaxByteLength: Nativeint;
    function _getResizable: Boolean;
    property byteLength : Nativeint Read _getByteLength;
    property maxByteLength : Nativeint Read _getMaxByteLength;
    property detached : Boolean Read _getDetached;
    property resizable : Boolean Read _getResizable;
  end;


  { TJSArrayBuffer }

  TJSArrayBuffer = class(TJSObject,IJSArrayBuffer)
  Protected
    function _getByteLength: Nativeint;
    function _getDetached: Boolean;
    function _getMaxByteLength: Nativeint;
    function _getResizable: Boolean;
  public
    constructor create (aSize : integer);
    class function GlobalMemory : TJSArrayBuffer;
    function Slice : IJSArrayBuffer;
    function Slice (aStart : NativeInt): IJSArrayBuffer;
    function Slice (aStart,aEndExclusive : NativeInt): IJSArrayBuffer;
    class function Cast(const Intf: IJSObject): IJSArrayBuffer; overload;
    class function JSClassName: UnicodeString; override;
    property byteLength : Nativeint Read _getByteLength;
    property maxByteLength : Nativeint Read _getMaxByteLength;
    property detached : Boolean Read _getDetached;
    property resizable : Boolean Read _getResizable;

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
    function GetBuffer: IJSArrayBuffer;
    function _GetLength: NativeInt;
    function _GetByteLength: NativeInt;
    function _GetByteOffset: NativeInt;
    procedure  set_(aArray : IJSTypedArray; TargetOffset : Integer);
    procedure  set_(aArray : IJSTypedArray);
    property Buffer : IJSArrayBuffer read GetBuffer;
    Property Length: NativeInt Read _GetLength;
    Property byteLength: NativeInt Read _GetByteLength;
    Property byteOffset: NativeInt Read _GetByteOffset;
  end;

  { TJSTypedArray }

  TJSTypedArray = class(TJSObject,IJSTypedArray)
  private
    function GetBuffer: IJSArrayBuffer;
    function _GetLength: NativeInt;
    function _GetByteLength: NativeInt;
    function _GetByteOffset: NativeInt;
  public
    constructor Create(aBytes : PByte; aLen : NativeUInt);
    constructor Create(aBytes : TBytes);
    constructor create(aArray : IJSArrayBuffer);
    class function Cast(const Intf: IJSObject): IJSTypedArray; overload;
    procedure set_(aArray : IJSTypedArray; TargetOffset : Integer);
    procedure set_(aArray : IJSTypedArray);
    property Buffer : IJSArrayBuffer read GetBuffer;
    Property Length: NativeInt Read _GetLength;
    Property byteLength: NativeInt Read _GetByteLength;
    Property byteOffset: NativeInt Read _GetByteOffset;
  end;

  { IJSInt8Array }

  IJSInt8Array = interface(IJSTypedArray)
    ['{72D65C5E-E18E-4294-8709-D7A63BF12958}']
    function _GetElement(aIndex : NativeInt) : Shortint;
    procedure _SetElement(aIndex : NativeInt; aValue : Shortint);
    property Element[Index: NativeInt]: Shortint read _GetElement write _SetElement; default;
  end;

  { TJSInt8Array }

  TJSInt8Array = class(TJSTypedArray,IJSInt8Array)
  Protected
    function _GetElement(aIndex : NativeInt) : Shortint;
    procedure _SetElement(aIndex : NativeInt; aValue : Shortint);
  public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSInt8Array; overload;
    property Element[Index: NativeInt]: Shortint read _GetElement write _SetElement; default;
  end;

  { IJSUint8Array }

  IJSUint8Array = interface(IJSTypedArray)
    ['{99EC7B3A-30E5-425F-933C-C169B2F4193C}']
    function _GetElement(aIndex : NativeInt) : Byte;
    procedure _SetElement(aIndex : NativeInt; aValue : Byte);
    property Element[Index: NativeInt]: Byte read _GetElement write _SetElement; default;
  end;

  { TJSUint8Array }

  TJSUint8Array = class(TJSTypedArray,IJSUint8Array)
  Protected
    function _GetElement(aIndex : NativeInt) : Byte;
    procedure _SetElement(aIndex : NativeInt; aValue : Byte);
  public
    Class function GetGlobal : TJSUint8Array;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSUint8Array; overload;
    property Element[Index: NativeInt]: byte read _GetElement write _SetElement; default;
  end;

  { IJSUint8ClampedArray }

  IJSUint8ClampedArray = interface(IJSTypedArray)
    ['{A1508D6E-8629-4416-875E-9F669ECDC47F}']
    function _GetElement(aIndex : NativeInt) : Byte;
    procedure _SetElement(aIndex : NativeInt; aValue : Byte);
    property Element[Index: NativeInt]: Byte read _GetElement write _SetElement; default;
  end;

  { TJSUint8ClampedArray }

  TJSUint8ClampedArray = class(TJSTypedArray,IJSUint8ClampedArray)
  Protected
    function _GetElement(aIndex : NativeInt) : Byte;
    procedure _SetElement(aIndex : NativeInt; aValue : Byte);
  public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSUint8ClampedArray; overload;
    property Element[Index: NativeInt]: Byte read _GetElement write _SetElement; default;
  end;

  { IJSInt16Array }

  IJSInt16Array = interface(IJSTypedArray)
    ['{B5FA7A13-D8CA-44E4-ADAE-F10FFFAE46B4}']
    function _GetElement(aIndex : NativeInt) : SmallInt;
    procedure _SetElement(aIndex : NativeInt; aValue : SmallInt);
    property Element[Index: NativeInt]: SmallInt read _GetElement write _SetElement; default;
  end;

  { TJSInt16Array }

  TJSInt16Array = class(TJSTypedArray,IJSInt16Array)
  Protected
    function _GetElement(aIndex : NativeInt) : SmallInt;
    procedure _SetElement(aIndex : NativeInt; aValue : SmallInt);
  public
    class function Cast(const Intf: IJSObject): IJSInt16Array; overload;
    class function JSClassName: UnicodeString; override;
    property Element[Index: NativeInt]: SmallInt read _GetElement write _SetElement; default;
  end;

  { IJSUint16Array }

  IJSUint16Array = interface(IJSTypedArray)
    ['{6023E2BC-C464-4288-A8DA-4A5D0B2B915E}']
    function _GetElement(aIndex : NativeInt) : Word;
    procedure _SetElement(aIndex : NativeInt; aValue : Word);
    property Element[Index: NativeInt]: Word read _GetElement write _SetElement; default;
  end;

  { TJSUint16Array }

  TJSUint16Array = class(TJSTypedArray,IJSUint16Array)
  Protected
    function _GetElement(aIndex : NativeInt) : Word;
    procedure _SetElement(aIndex : NativeInt; aValue : Word);
  public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSUint16Array; overload;
    property Element[Index: NativeInt]: Word read _GetElement write _SetElement; default;
  end;

  { IJSInt32Array }

  IJSInt32Array = interface(IJSTypedArray)
    ['{16F1A6FB-2F26-4A64-8A2B-D883DE2F58C4}']
    function _GetElement(aIndex : NativeInt) : LongInt;
    procedure _SetElement(aIndex : NativeInt; aValue : LongInt);
    property Element[Index: NativeInt]: LongInt read _GetElement write _SetElement; default;
  end;

  { TJSInt32Array }

  TJSInt32Array = class(TJSTypedArray,IJSInt32Array)
  Protected
    function _GetElement(aIndex : NativeInt) : LongInt;
    procedure _SetElement(aIndex : NativeInt; aValue : LongInt);
  public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSInt32Array; overload;
    property Element[Index: NativeInt]: LongInt read _GetElement write _SetElement; default;
  end;

  { IJSUint32Array }

  IJSUint32Array = interface(IJSTypedArray)
    ['{C637B2FA-CED6-4EC7-8D97-C56824EAF8B3}']
    function _GetElement(aIndex : NativeInt) : Cardinal;
    procedure _SetElement(aIndex : NativeInt; aValue : Cardinal);
    property Element[Index: NativeInt]: Cardinal read _GetElement write _SetElement; default;
  end;

  { TJSUint32Array }

  TJSUint32Array = class(TJSTypedArray,IJSUint32Array)
  Protected
    function _GetElement(aIndex : NativeInt) : Cardinal;
    procedure _SetElement(aIndex : NativeInt; aValue : Cardinal);
  public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSUint32Array; overload;
    property Element[Index: NativeInt]: Cardinal read _GetElement write _SetElement; default;
  end;

  { IJSFloat32Array }

  IJSFloat32Array = interface(IJSTypedArray)
    ['{B5CE57F6-CA7C-4168-AEA3-32EF13DA52D6}']
    function _GetElement(aIndex : NativeInt) : SIngle;
    procedure _SetElement(aIndex : NativeInt; aValue : SIngle);
    property Element[Index: NativeInt]: SIngle read _GetElement write _SetElement; default;
  end;

  { TJSFloat32Array }

  TJSFloat32Array = class(TJSTypedArray,IJSFloat32Array)
  Protected
    function _GetElement(aIndex : NativeInt) : Single;
    procedure _SetElement(aIndex : NativeInt; aValue : Single);
  public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSFloat32Array; overload;
    property Element[Index: NativeInt]: Single read _GetElement write _SetElement; default;
  end;

  { IJSFloat64Array }

  IJSFloat64Array = interface(IJSTypedArray)
    ['{A7876DC5-9549-4FDA-BE35-A641CE9D9F0B}']
    function _GetElement(aIndex : NativeInt) : Double;
    procedure _SetElement(aIndex : NativeInt; aValue : Double);
    property Element[Index: NativeInt]: Double read _GetElement write _SetElement; default;
  end;

  { TJSFloat64Array }

  TJSFloat64Array = class(TJSTypedArray,IJSFloat64Array)
  Protected
    function _GetElement(aIndex : NativeInt) : Double;
    procedure _SetElement(aIndex : NativeInt; aValue : Double);
  public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSFloat64Array; overload;
    property Element[Index: NativeInt]: Double read _GetElement write _SetElement; default;
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
    FResolveCallback,
    FRejectCallback : IJSFunction;
    FExecutor : TJSPromiseExecutor;
    function HandleResolve(const aValue : Variant): variant;
    function HandleReject(const aValue : Variant): variant;
  protected
    Procedure DoExecutor(const OnResolve, OnReject: TJSPromiseResolver); virtual;
  public
    constructor Create(const Executor: TJSPromiseExecutor); overload;
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
    class function Cast(const Intf: IJSObject): IJSPromise; overload;
    class function JSClassName: UnicodeString; override;
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

// imported functions from browser
function __job_invoke_noresult(
  ObjID: TJOBObjectID;
  NameP: PChar;
  NameLen: longint;
  Invoke: longint;
  ArgP: PByte
): TJOBResult; external JOBExportName name JOBFn_InvokeNoResult;

function __job_invoke_boolresult(
  ObjID: TJOBObjectID;
  NameP: PChar;
  NameLen: longint;
  Invoke: longint;
  ArgP: PByte;
  ResultByteBoolP: PByte
): TJOBResult; external JOBExportName name JOBFn_InvokeBooleanResult;

function __job_invoke_doubleresult(
  ObjID: TJOBObjectID;
  NameP: PChar;
  NameLen: longint;
  Invoke: longint;
  ArgP: PByte;
  ResultDoubleP: PByte
): TJOBResult; external JOBExportName name JOBFn_InvokeDoubleResult;

function __job_invoke_stringresult(
  ObjID: TJOBObjectID;
  NameP: PChar;
  NameLen: longint;
  Invoke: longint;
  ArgP: PByte;
  ResultLenP: PByte // nativeint
): TJOBResult; external JOBExportName name JOBFn_InvokeStringResult;

function __job_getstringresult(
  ResultP: PByte
): TJOBResult; external JOBExportName name JOBFn_GetStringResult;

function __job_releasestringresult(
): TJOBResult; external JOBExportName name JOBFn_ReleaseStringResult;

function __job_invoke_objectresult(
  ObjID: TJOBObjectID;
  NameP: PChar;
  NameLen: longint;
  Invoke: longint;
  ArgP: PByte;
  ResultObjIDP: PByte // nativeint
): TJOBResult; external JOBExportName name JOBFn_InvokeObjectResult;

function __job_release_object(
  ObjID: TJOBObjectID
): TJOBResult; external JOBExportName name JOBFn_ReleaseObject;

function __job_invoke_jsvalueresult(
  ObjID: TJOBObjectID;
  NameP: PChar;
  NameLen: longint;
  Invoke: longint;
  ArgP: PByte;
  ResultP: PByte  // various
): TJOBResult; external JOBExportName name JOBFn_InvokeJSValueResult;

function __job_invoke_arraystringresult(
  ObjID: TJOBObjectID;
  NameP: PChar;
  NameLen: longint;
  Invoke: longint;
  ArgP: PByte;
  ResultLenP: PByte // nativeint
): TJOBResult; external JOBExportName name JOBFn_InvokeArrayStringResult;

function __job_get_global(
  NameP: PWideChar;
  NameLen: longint
  ): TJOBObjectID; external JOBExportName name JOBFn_GetGlobal;

function __job_create_object(
  NameP: PWideChar;
  NameLen: longint;
  ArgP: PByte
  ): TJOBObjectID; external JOBExportName name JOBFn_CreateObject;

function JOBCallback(const Func: TJOBCallback; Data, Code: Pointer; Args: PByte): PByte;
function VarRecToJSValue(const V: TVarRec): TJOB_JSValue;

implementation

const
  InvokeGetToInt: array[TJOBInvokeType] of integer = (
    JOBInvokeCall,
    JOBInvokeGet,
    JOBInvokeGetTypeOf,
    JOBInvokeSet,
    JOBInvokeNew
    );

{$IFDEF VerboseJOB}
function GetVarRecName(vt: word): string;
begin
  case vt of
    vtInteger: Result:='vtInteger';
    vtBoolean: Result:='vtBoolean';
    vtChar: Result:='vtChar';
    {$ifndef FPUNONE}
    vtExtended: Result:='vtExtended';
    {$endif}
    vtString: Result:='vtString';
    vtPointer: Result:='vtPointer';
    vtPChar: Result:='vtPChar';
    vtObject: Result:='vtObject';
    vtClass: Result:='vtClass';
    vtWideChar: Result:='vtWideChar';
    vtPWideChar: Result:='vtPWideChar';
    vtAnsiString: Result:='vtAnsiString';
    vtCurrency: Result:='vtCurrency';
    vtVariant: Result:='vtVariant';
    vtInterface: Result:='vtInterface';
    vtWideString: Result:='vtWideString';
    vtInt64: Result:='vtInt64';
    vtQWord: Result:='vtQWord';
    vtUnicodeString: Result:='vtUnicodeString';
  else
    Result:='vt?';
  end;
end;
{$ENDIF}

function __job_callback(w: NativeInt): boolean;
begin
  {$IFDEF VERBOSEJOB}
  writeln('__job_callback w=',w);
  {$ENDIF}
  Result:=true;
end;

function JOBCallback(const Func: TJOBCallback; Data, Code: Pointer; Args: PByte
  ): PByte;
var
  m: TMethod;
  h: TJOBCallbackHelper;
begin
  Result:=nil;
  try
    {$IFDEF VERBOSEJOB}
    writeln('In JOBCallback');
    {$ENDIF}
    m.Data:=Data;
    m.Code:=Code;
    h.Init(Args);
    Result:=Func(m,h);
  finally
    if Args<>nil then
      FreeMem(Args);
  end;
end;

function VarRecToJSValue(const V: TVarRec): TJOB_JSValue;
var
  p: Pointer;
  CurLen: SizeInt;
  S: String;
  Obj: TObject;
  Intf: IJSObject;
begin
  case V.VType of
  vtInteger:
    Result:=TJOB_Double.Create(V.VInteger);
  vtBoolean:
    Result:=TJOB_Boolean.Create(V.VBoolean);
  vtChar:
    Result:=TJOB_String.Create(UnicodeString(V.VChar));
  {$ifndef FPUNONE}
  vtExtended:
    Result:=TJOB_Double.Create(V.VExtended^);
  {$endif}
  vtString:
    Result:=TJOB_String.Create(UTF8Decode(V.VString^));
  vtPointer:
    begin
    p:=V.VPointer;
    if p=nil then
      Result:=TJOB_Object.Create(nil)
    else if p=JOB_Undefined then
      Result:=TJOB_JSValue.Create(jjvkUndefined)
    else
      raise EJSArgParse.Create('VarRecToJSValue pointer not supported');
    end;
  vtPChar:
    begin
    CurLen:=strlen(V.VPChar);
    SetString(S,V.VPChar,CurLen);
    Result:=TJOB_String.Create(UTF8Decode(S));
    end;
  vtObject:
    begin
    Obj:=V.VObject;
    if Obj=nil then
      Result:=TJOB_Object.Create(nil)
    else if Obj is TJOB_JSValue then
      Result:=TJOB_JSValue(Obj)
    else if Obj is TJSObject then
      Result:=TJOB_Object.Create(TJSObject(Obj) as IJSObject)
    else
      raise EJSArgParse.Create('VarRecToJSValue object '+Obj.ClassName+' not supported');
    end;
  vtClass:
    raise EJSArgParse.Create('VarRecToJSValue class not supported');
  vtWideChar:
    Result:=TJOB_String.Create(V.VWideChar);
  vtPWideChar:
    raise EJSArgParse.Create('VarRecToJSValue vtPWideChar not supported');
  vtAnsiString:
    Result:=TJOB_String.Create(UTF8Decode(PAnsiString(V.VAnsiString)^));
  vtCurrency:
    Result:=TJOB_Double.Create(V.VCurrency^);
  vtVariant:
    raise EJSArgParse.Create('VarRecToJSValue vtVariant not supported');
  vtInterface:
    begin
    Intf:=IJSObject(V.VInterface);
    Result:=TJOB_Object.Create(Intf);
    end;
  vtWideString:
    raise EJSArgParse.Create('VarRecToJSValue vtWideString not supported');
  vtInt64:
    Result:=TJOB_Double.Create(V.VInt64^);
  vtQWord:
    Result:=TJOB_Double.Create(V.VQWord^);
  vtUnicodeString:
    Result:=TJOB_String.Create(PUnicodeString(V.VUnicodeString)^);
  else
    raise EJSArgParse.Create('VarRecToJSValue unsupported VType '+IntToStr(V.VType));
  end;
end;

function JOBCallTJSPromiseResolver(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  aValue: Variant;
begin
  aValue:=H.GetVariant;
  Result:=H.AllocVariant(TJSPromiseResolver(aMethod)(aValue));
end;

function JOBCallTJSPromiseFinallyHandler(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
begin
  Result:=H.AllocUndefined;
  TJSPromiseFinallyHandler(aMethod)();
end;

function JOBCallTJSPromiseExecutor(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;

var
  P : TJSPromise;

begin
  P:=TJSPromise(aMethod.Data);
  P.FResolveCallback:=H.GetFunction;
  P.FRejectCallBack:=H.GetFunction;
  try
     TJSPromiseExecutor(aMethod)(@P.HandleResolve, @P.HandleReject);
  except
    on E : Exception do
      begin
      {$IFDEF VerboseJOB}
      Writeln('Wasm error calling promise executor : ',E.Message);
      {$ENDIF}
      Raise;
      end;
    on O : TObject do
      begin
      {$IFDEF VerboseJOB}
      Writeln('Wasm error calling promise executor : ',O.ClassName);
      {$ENDIF}
      Raise;
      end;
  end;
  {$IFDEF VerboseJOB}
  Writeln('Wasm: Making function result');
  {$ENDIF}
  Result:=H.AllocUndefined;
end;


{ TJSTextEncoder }

class function TJSTextEncoder.Cast(const Intf: IJSObject): IJSTextEncoder;
begin
  Result:=TJSTextEncoder.Cast(Intf);
end;

{ TJSTextDecoder }

class function TJSTextDecoder.Cast(const Intf: IJSObject): IJSTextDecoder;
begin
  Result:=TJSTextDecoder.Cast(Intf);
end;

{ TJSPromise }



function TJSPromise.HandleResolve(const aValue: Variant): variant;
begin
  result:=FResolveCallback.apply([aValue]);
end;

function TJSPromise.HandleReject(const aValue: Variant): variant;
begin
  Result:=FRejectCallback.apply([aValue]);
end;

procedure TJSPromise.DoExecutor(const OnResolve, OnReject: TJSPromiseResolver);
begin
  FExecutor(OnResolve,OnReject);
end;

constructor TJSPromise.Create(const Executor: TJSPromiseExecutor);

var
  m: TJOB_Method;
begin
  FExecutor:=Executor;
  m:=TJOB_Method.Create(TMethod(@DoExecutor),@JobCallTJSPromiseExecutor);
  try
    JOBCreate([m]);
  finally
    m.Free;
  end;
end;

function TJSPromise.all(const arg: Variant): IJSPromise;
begin
  Result:=InvokeJSObjectResult('all',[arg],TJSPromise) as IJSPromise;
end;

function TJSPromise.allSettled(const arg: Variant): IJSPromise;
begin
  Result:=InvokeJSObjectResult('allSettled',[arg],TJSPromise) as IJSPromise;
end;

function TJSPromise.race(const arg: Variant): IJSPromise;
begin
  Result:=InvokeJSObjectResult('race',[arg],TJSPromise) as IJSPromise;
end;

function TJSPromise.reject(const reason: Variant): IJSPromise;
begin
  Result:=InvokeJSObjectResult('reject',[reason],TJSPromise) as IJSPromise;
end;

function TJSPromise.resolve(const value: Variant): IJSPromise;
begin
  Result:=InvokeJSObjectResult('resolve',[value],TJSPromise) as IJSPromise;
end;

function TJSPromise.resolve: IJSPromise;
begin
  Result:=InvokeJSObjectResult('resolve',[],TJSPromise) as IJSPromise;
end;

function TJSPromise._then(const OnAccepted: TJSPromiseResolver): IJSPromise;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(onAccepted),@JOBCallTJSPromiseResolver);
  try
    Result:=InvokeJSObjectResult('then',[m],TJSPromise) as IJSPromise;
  finally
    m.Free;
  end;
end;

function TJSPromise._then(const OnAccepted, OnRejected: TJSPromiseResolver
  ): IJSPromise;
var
  ma, mr: TJOB_Method;
begin
  ma:=TJOB_Method.Create(TMethod(OnAccepted),@JOBCallTJSPromiseResolver);
  mr:=TJOB_Method.Create(TMethod(OnRejected),@JOBCallTJSPromiseResolver);
  try
    Result:=InvokeJSObjectResult('then',[ma,mr],TJSPromise) as IJSPromise;
  finally
    mr.Free;
    ma.Free;
  end;
end;

function TJSPromise.catch(const OnRejected: TJSPromiseResolver): IJSPromise;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(OnRejected),@JOBCallTJSPromiseResolver);
  try
    Result:=InvokeJSObjectResult('catch',[m],TJSPromise) as IJSPromise;
  finally
    m.Free;
  end;
end;

function TJSPromise._finally(const Handler: TJSPromiseFinallyHandler
  ): IJSPromise;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(Handler),@JOBCallTJSPromiseFinallyHandler);
  try
    Result:=InvokeJSObjectResult('finally',[m],TJSPromise) as IJSPromise;
  finally
    m.Free;
  end;
end;

class function TJSPromise.Cast(const Intf: IJSObject): IJSPromise;
begin
  Result:=TJSPromise.Cast(Intf);
end;

class function TJSPromise.JSClassName: UnicodeString;
begin
  Result:='Promise';
end;

{ TJSError }

class function TJSError.Cast(const Intf: IJSObject): IJSError;
begin
  Result:=TJSError.Cast(Intf);
end;

{ TJSJSON }

function TJSJSON.parse(const aJSON: UnicodeString): TJOB_JSValue;
begin
  Result:=InvokeJSValueResult('parse',[aJSON]);
end;

function TJSJSON.parseObject(const aJSON: UnicodeString): IJSObject;
begin
  Result:=InvokeJSObjectResult('parse',[aJSON],TJSObject) as IJSObject;
end;

function TJSJSON.stringify(aValue: TJOB_JSValue): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('stringify',[aValue]);
end;

function TJSJSON.stringify(aValue, aReplacer: TJOB_JSValue): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('stringify',[aValue,aReplacer]);
end;

function TJSJSON.stringify(aValue, aReplacer: TJOB_JSValue; space: NativeInt
  ): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('stringify',[aValue,aReplacer,space]);
end;

function TJSJSON.stringify(aValue, aReplacer: TJOB_JSValue;
  const space: UnicodeString): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('stringify',[aValue,aReplacer,space]);
end;

class function TJSJSON.Cast(const Intf: IJSObject): IJSJSON;
begin
  Result:=TJSJSON.Cast(Intf);
end;

{ TJSDataView }

class function TJSDataView.Cast(const Intf: IJSObject): IJSDataView;
begin
  Result:=TJSDataView.Cast(Intf);
end;

{ TJSBufferSource }

class function TJSBufferSource.Cast(const Intf: IJSObject): IJSBufferSource;
begin
  Result:=TJSBufferSource.Cast(Intf);
end;

{ TJSFloat64Array }

function TJSFloat64Array._GetElement(aIndex: NativeInt): Double;
begin
  Result:=InvokeJSDoubleResult(IntToStr(aIndex),[],jiGet);
end;

procedure TJSFloat64Array._SetElement(aIndex: NativeInt; aValue: Double);
begin
  InvokeJSNoResult(IntToStr(aIndex),[aValue],jiSet);
end;

class function TJSFloat64Array.JSClassName: UnicodeString;
begin
  Result:='Float64Array';
end;

class function TJSFloat64Array.Cast(const Intf: IJSObject): IJSFloat64Array;
begin
  Result:=TJSFloat64Array.Cast(Intf);
end;

{ TJSFloat32Array }

function TJSFloat32Array._GetElement(aIndex: NativeInt): SIngle;
begin
  Result:=InvokeJSDoubleResult(IntToStr(aIndex),[],jiGet);
end;

procedure TJSFloat32Array._SetElement(aIndex: NativeInt; aValue: SIngle);
begin
  InvokeJSNoResult(IntToStr(aIndex),[aValue],jiSet);
end;

class function TJSFloat32Array.JSClassName: UnicodeString;
begin
  Result:='Float32Array';
end;

class function TJSFloat32Array.Cast(const Intf: IJSObject): IJSFloat32Array;
begin
  Result:=TJSFloat32Array.Cast(Intf);
end;

{ TJSUint32Array }

function TJSUint32Array._GetElement(aIndex: NativeInt): Cardinal;
begin
  Result:=Cardinal(InvokeJSMaxIntResult(IntToStr(aIndex),[],jiGet));
end;

procedure TJSUint32Array._SetElement(aIndex: NativeInt; aValue: Cardinal);
begin
  InvokeJSNoResult(IntToStr(aIndex),[aValue],jiSet);
end;

class function TJSUint32Array.JSClassName: UnicodeString;
begin
  Result:=inherited JSClassName;
end;

class function TJSUint32Array.Cast(const Intf: IJSObject): IJSUint32Array;
begin
  Result:=TJSUint32Array.Cast(Intf);
end;

{ TJSInt32Array }

function TJSInt32Array._GetElement(aIndex: NativeInt): LongInt;
begin
  Result:=InvokeJSLongintResult(IntToStr(aIndex),[],jiGet);
end;

procedure TJSInt32Array._SetElement(aIndex: NativeInt; aValue: LongInt);
begin
  InvokeJSNoResult(IntToStr(aIndex),[aValue],jiSet);
end;

class function TJSInt32Array.JSClassName: UnicodeString;
begin
  Result:='Int32Array';
end;

class function TJSInt32Array.Cast(const Intf: IJSObject): IJSInt32Array;
begin
  Result:=TJSInt32Array.Cast(Intf);
end;

{ TJSUint16Array }

class function TJSUint16Array.JSClassName: UnicodeString;
begin
  Result:='Uint16Array';
end;

class function TJSUint16Array.Cast(const Intf: IJSObject): IJSUint16Array;
begin
  Result:=TJSUint16Array.Cast(Intf);
end;

function TJSUint16Array._GetElement(aIndex: NativeInt): Word;
begin
  Result:=InvokeJSLongintResult(IntToStr(aIndex),[],jiGet);
end;

procedure TJSUint16Array._SetElement(aIndex: NativeInt; aValue: Word);
begin
  InvokeJSNoResult(IntToStr(aIndex),[aValue],jiSet);
end;

{ TJSInt16Array }

function TJSInt16Array._GetElement(aIndex: NativeInt): SmallInt;
begin
  Result:=InvokeJSLongintResult(IntToStr(aIndex),[],jiGet);
end;

procedure TJSInt16Array._SetElement(aIndex: NativeInt; aValue: SmallInt);
begin
  InvokeJSNoResult(IntToStr(aIndex),[aValue],jiSet);
end;

class function TJSInt16Array.Cast(const Intf: IJSObject): IJSInt16Array;
begin
  Result:=TJSInt16Array.Cast(Intf);
end;

class function TJSInt16Array.JSClassName: UnicodeString;
begin
  Result:='Int16Array';
end;

{ TJSUint8ClampedArray }

function TJSUint8ClampedArray._GetElement(aIndex: NativeInt): Byte;
begin
  Result:=InvokeJSLongintResult(IntToStr(aIndex),[],jiGet);
end;

procedure TJSUint8ClampedArray._SetElement(aIndex: NativeInt; aValue: Byte);
begin
  InvokeJSNoResult(IntToStr(aIndex),[aValue],jiSet);
end;

class function TJSUint8ClampedArray.JSClassName: UnicodeString;
begin
  Result:='Uint8ClampedArray';
end;

class function TJSUint8ClampedArray.Cast(const Intf: IJSObject
  ): IJSUint8ClampedArray;
begin
  Result:=TJSUint8ClampedArray.JobCast(Intf);
end;

{ TJSUInt8Array }

function TJSUint8Array._GetElement(aIndex: NativeInt): Byte;
begin
  Result:=InvokeJSLongintResult(IntToStr(aIndex),[],jiGet);
end;

procedure TJSUint8Array._SetElement(aIndex: NativeInt; aValue: Byte);
begin
  InvokeJSNoResult(IntToStr(aIndex),[aValue],jiSet);
end;

class function TJSUint8Array.GetGlobal: TJSUint8Array;
begin
  // We must free it.
  Result:=TJSUInt8Array.JOBCreateGlobal('InstanceMemory');
  TJSUInt8Array(Result).FJOBObjectIDOwner:=True;
end;

class function TJSUint8Array.JSClassName: UnicodeString;
begin
  Result:='Uint8Array';
end;

class function TJSUint8Array.Cast(const Intf: IJSObject): IJSUint8Array;
begin
  Result:=TJSUInt8Array.Cast(Intf);
end;

{ TJSInt8Array }

function TJSInt8Array._GetElement(aIndex: NativeInt): Shortint;
begin
  Result:=InvokeJSLongintResult(IntToStr(aIndex),[],jiGet);
end;

procedure TJSInt8Array._SetElement(aIndex: NativeInt; aValue: Shortint);
begin
  InvokeJSNoResult(IntToStr(aIndex),[aValue],jiSet);
end;

class function TJSInt8Array.JSClassName: UnicodeString;
begin
  Result:='Int8Array';
end;

class function TJSInt8Array.Cast(const Intf: IJSObject): IJSInt8Array;
begin
  Result:=TJSInt8Array.JobCast(Intf);
end;

{ TJSTypedArray }

function TJSTypedArray.GetBuffer: IJSArrayBuffer;
begin
  Result:=ReadJSPropertyObject('buffer',TJSArrayBuffer) as IJSArrayBuffer;
end;

function TJSTypedArray._GetLength: NativeInt;
begin
  // For the time being
  Result:=ReadJSPropertyLongInt('length');
end;

function TJSTypedArray._GetByteLength: NativeInt;
begin
  Result:=ReadJSPropertyLongInt('byteLength');
end;

function TJSTypedArray._GetByteOffset: NativeInt;
begin
  Result:=ReadJSPropertyLongInt('byteOffset');
end;

constructor TJSTypedArray.Create(aBytes: PByte; aLen: NativeUInt);

var
  Data : TJOB_JSValue;

begin
  Data:=TJOB_ArrayOfByte.Create(aBytes,aLen);
  JobCreate(True,[Data]);
end;

constructor TJSTypedArray.Create(aBytes: TBytes);
var
  Data : TJOB_JSValue;
begin
  Data:=TJOB_ArrayOfByte.Create(aBytes);
  JobCreate(True,[Data]);
end;

constructor TJSTypedArray.create(aArray: IJSArrayBuffer);
begin
  JobCreate(True,[aArray]);
end;

class function TJSTypedArray.Cast(const Intf: IJSObject): IJSTypedArray;
begin
  Result:=TJSTypedArray.Cast(Intf);
end;

procedure TJSTypedArray.set_(aArray: IJSTypedArray; TargetOffset: Integer);
begin
  InvokeJSNoResult('set',[aArray,TargetOffset]);
end;

procedure TJSTypedArray.set_(aArray: IJSTypedArray);
begin
  InvokeJSNoResult('set',[aArray]);
end;

{ TJSArrayBuffer }

function TJSArrayBuffer._getByteLength: Nativeint;
begin
  Result:=ReadJSPropertyInt64('byteLength');

end;

function TJSArrayBuffer._getDetached: Boolean;
begin
  Result:=ReadJSPropertyBoolean('detached');
end;

function TJSArrayBuffer._getMaxByteLength: Nativeint;
begin
  Result:=ReadJSPropertyInt64('maxByteLength');
end;

function TJSArrayBuffer._getResizable: Boolean;
begin
  Result:=ReadJSPropertyBoolean('resizable');
end;

constructor TJSArrayBuffer.create(aSize: integer);
begin
  JobCreate(True,[aSize])
end;

class function TJSArrayBuffer.GlobalMemory: TJSArrayBuffer;
begin
  Result:=JOBCreateGlobal('InstanceBuffer');
end;

function TJSArrayBuffer.Slice: IJSArrayBuffer;
begin
  Result:=InvokeJSObjectResult('slice',[],TJSArrayBuffer) as IJSArrayBuffer;
end;

function TJSArrayBuffer.Slice(aStart: NativeInt): IJSArrayBuffer;
begin
  Result:=InvokeJSObjectResult('slice',[aStart],TJSArrayBuffer) as IJSArrayBuffer;
end;

function TJSArrayBuffer.Slice(aStart, aEndExclusive: NativeInt): IJSArrayBuffer;
begin
  Result:=InvokeJSObjectResult('slice',[aStart,aEndExclusive],TJSArrayBuffer) as IJSArrayBuffer;
end;

class function TJSArrayBuffer.Cast(const Intf: IJSObject): IJSArrayBuffer;
begin
  Result:=TJSArrayBuffer.JOBCast(Intf);
end;

class function TJSArrayBuffer.JSClassName: UnicodeString;
begin
  Result:='ArrayBuffer';
end;

{ TJSArrayBufferView }

class function TJSArrayBufferView.Cast(const Intf: IJSObject): IJSArrayBufferView;
begin
  Result:=TJSArrayBufferView.JOBCast(Intf);
end;

{ TJSArray }

function TJSArray._GetElements(Index: NativeInt): TJOB_JSValue;
begin
  Result:=InvokeJSValueResult(IntToStr(Index),[],jiGet);
end;

function TJSArray._GetLength: NativeInt;
begin
  Result:=ReadJSPropertyLongInt('length');
end;

procedure TJSArray._SetElements(Index: NativeInt; const AValue: TJOB_JSValue);
begin
  InvokeJSNoResult(IntToStr(Index),[AValue],jiSet);
end;

procedure TJSArray._SetLength(const AValue: NativeInt);
begin
  WriteJSPropertyLongInt('length',AValue);
end;

constructor TJSArray.Create(aArgs: array of const);
begin
  JOBCreate(aArgs);
end;

function TJSArray.isArray(a: TJOB_JSValue): Boolean;
begin
  Result:=InvokeJSBooleanResult('isArray',[a]);
end;

function TJSArray.concat(el: TJOB_JSValue): IJSArray;
begin
  Result:=InvokeJSObjectResult('isArray',[el],TJSArray) as IJSArray;
end;

function TJSArray.fill(aValue: TJOB_JSValue): IJSArray;
begin
  Result:=InvokeJSObjectResult('fill',[aValue],TJSArray) as IJSArray;
end;

function TJSArray.fill(aValue: TJOB_JSValue; aStartIndex: NativeInt): IJSArray;
begin
  Result:=InvokeJSObjectResult('fill',[aValue,aStartIndex],TJSArray) as IJSArray;
end;

function TJSArray.fill(aValue: TJOB_JSValue; aStartIndex, aEndIndex: NativeInt
  ): IJSArray;
begin
  Result:=InvokeJSObjectResult('fill',[aValue,aStartIndex,aEndIndex],TJSArray) as IJSArray;
end;

function TJSArray.includes(aElement: TJOB_JSValue): Boolean;
begin
  Result:=InvokeJSBooleanResult('includes',[aElement]);
end;

function TJSArray.includes(aElement: TJOB_JSValue; FromIndex: NativeInt
  ): Boolean;
begin
  Result:=InvokeJSBooleanResult('includes',[aElement,FromIndex]);
end;

function TJSArray.indexOf(aElement: TJOB_JSValue): NativeInt;
begin
  Result:=InvokeJSMaxIntResult('indexOf',[aElement]);
end;

function TJSArray.indexOf(aElement: TJOB_JSValue; FromIndex: NativeInt
  ): NativeInt;
begin
  Result:=InvokeJSMaxIntResult('indexOf',[aElement,FromIndex]);
end;

function TJSArray.join: UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('join',[]);
end;

function TJSArray.join(const aSeparator: UnicodeString): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('join',[aSeparator]);
end;

function TJSArray.lastIndexOf(aElement: TJOB_JSValue): NativeInt;
begin
  Result:=InvokeJSMaxIntResult('lastIndexOf',[aElement]);
end;

function TJSArray.lastIndexOf(aElement: TJOB_JSValue; FromIndex: NativeInt
  ): NativeInt;
begin
  Result:=InvokeJSMaxIntResult('lastIndexOf',[aElement,FromIndex]);
end;

function TJSArray.pop: TJOB_JSValue;
begin
  Result:=InvokeJSValueResult('pop',[]);
end;

function TJSArray.push(aElement: TJOB_JSValue): NativeInt;
begin
  Result:=InvokeJSMaxIntResult('push',[aElement]);
end;

function TJSArray.reverse: IJSArray;
begin
  Result:=InvokeJSObjectResult('reverse',[],TJSArray) as IJSArray;
end;

function TJSArray.shift: TJOB_JSValue;
begin
  Result:=InvokeJSValueResult('shift',[]);
end;

function TJSArray.slice: IJSArray;
begin
  Result:=InvokeJSObjectResult('slice',[],TJSArray) as IJSArray;
end;

function TJSArray.slice(aBegin: NativeInt): IJSArray;
begin
  Result:=InvokeJSObjectResult('slice',[aBegin],TJSArray) as IJSArray;
end;

function TJSArray.slice(aBegin, aEnd: NativeInt): IJSArray;
begin
  Result:=InvokeJSObjectResult('slice',[aBegin,aEnd],TJSArray) as IJSArray;
end;

function TJSArray.sort(): IJSArray;
begin
  Result:=InvokeJSObjectResult('sort',[],TJSArray) as IJSArray;
end;

function TJSArray.splice(aStart: NativeInt): IJSArray;
begin
  Result:=InvokeJSObjectResult('splice',[aStart],TJSArray) as IJSArray;
end;

function TJSArray.splice(aStart, aDeleteCount: NativeInt): IJSArray;
begin
  Result:=InvokeJSObjectResult('splice',[aStart,aDeleteCount],TJSArray) as IJSArray;
end;

function TJSArray.toLocaleString(const locales: UnicodeString): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('toLocaleString',[locales]);
end;

function TJSArray.unshift: NativeInt;
begin
  Result:=InvokeJSMaxIntResult('unshift',[]);
end;

class function TJSArray.Cast(const Intf: IJSObject): IJSArray;
begin
  Result:=TJSArray.Cast(Intf);
end;

class function TJSArray.JSClassName: UnicodeString;
begin
  Result:='Array';
end;

{ TJSString }

class function TJSString.Cast(const Intf: IJSObject): IJSString;
begin
  Result:=TJSString.Cast(Intf);
end;

{ TJSRegExp }

function TJSRegExp.exec(const aString: UnicodeString): IJSArray;
begin
  Result:=InvokeJSObjectResult('exec',[aString],TJSArray) as IJSArray;
end;

function TJSRegExp._GetGlobal: Boolean;
begin
  Result:=ReadJSPropertyBoolean('global');
end;

function TJSRegExp._GetIgnoreCase: Boolean;
begin
  Result:=ReadJSPropertyBoolean('ignoreCase');
end;

function TJSRegExp._GetLastIndex: NativeInt;
begin
  Result:=ReadJSPropertyLongInt('lastIndex');
end;

function TJSRegExp._GetMultiLine: Boolean;
begin
  Result:=ReadJSPropertyBoolean('multiline');
end;

function TJSRegExp._GetSource: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('source');
end;

function TJSRegExp._GetUnicode: boolean;
begin
  Result:=ReadJSPropertyBoolean('unicode');
end;

procedure TJSRegExp._SetGlobal(const AValue: Boolean);
begin
  WriteJSPropertyBoolean('global',AValue);
end;

procedure TJSRegExp._SetIgnoreCase(const AValue: Boolean);
begin
  WriteJSPropertyBoolean('ignoreCase',AValue);
end;

procedure TJSRegExp._SetlastIndex(const AValue: NativeInt);
begin
  WriteJSPropertyLongInt('lastIndex',AValue);
end;

procedure TJSRegExp._SetMultiline(const AValue: Boolean);
begin
  WriteJSPropertyBoolean('multiline',AValue);
end;

procedure TJSRegExp._SetSource(const AValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('source',AValue);
end;

procedure TJSRegExp._SetUnicode(const AValue: boolean);
begin
  WriteJSPropertyBoolean('unicode',AValue);
end;

function TJSRegExp.test(const aString: UnicodeString): boolean;
begin
  Result:=InvokeJSBooleanResult('test',[aString]);
end;

class function TJSRegExp.Cast(const Intf: IJSObject): IJSRegExp;
begin
  Result:=TJSRegExp.Cast(Intf);
end;

{ TJSFunction }

constructor TJSFunction.Create(aObjectID: TJOBObjectID);
begin
  Create(aObjectID,0);
end;

constructor TJSFunction.Create(aObjectID, aThisID: TJOBObjectID);
begin
  JOBCreateFromID(aObjectID);
  FThisID:=aThisID;
end;

destructor TJSFunction.destroy;
begin
  Inherited;
end;

function TJSFunction._GetLength: NativeInt;
begin
  Result:=ReadJSPropertyLongInt('length');
end;

function TJSFunction._GetName: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSFunction._GetPrototyp: IJSFunction;
begin
  Result:=ReadJSPropertyObject('prototyp',TJSFunction) as IJSFunction;
end;

procedure TJSFunction._SetName(const AValue: UnicodeString);
begin
  WriteJSPropertyUnicodeString('length',AValue);
end;

function TJSFunction.apply(thisArg: TJSObject; const ArgArray: array of const): Variant;

Var
  Arr : IJSArray;
  J : TJOB_JSValue;

begin
  {$IFDEF VerboseJOB}
  Writeln('Wasm: in TJSFunction.apply with this. Creating argument array');
  {$ENDIF}
  Arr:=TJSArray.Create(ArgArray);
  {$IFDEF VerboseJOB}
  Writeln('Wasm: invoking apply');
  {$ENDIF}
  J:=InvokeJSValueResult('apply',[thisArg,Arr]);
  try
    Result:=J.AsVariant;
  finally
    J.Free;
  end;
end;

function TJSFunction.apply(const ArgArray: array of const): Variant;

var
  aThis : TJSObject;
  iThis : IJSObject;

begin
  {$IFDEF VerboseJOB}
  Writeln('Wasm: in TJSFunction.apply without this');
  {$ENDIF}
  if FThisID>0 then
    begin
    aThis:=TJSObject.JOBCreateFromID(FThisID);
    iThis:=aThis
    end
  else
    aThis:=Nil;
  {$IFDEF VerboseJOB}
  Writeln('Wasm: have this for apply: ',Assigned(aThis));
  {$ENDIF}
  aThis.FJOBObjectIDOwner:=False;
  try
    {$IFDEF VerboseJOB}
    Writeln('Wasm: calling apply: ',Assigned(aThis));
    {$ENDIF}
    Result:=Apply(aThis,ArgArray);
  finally
    aThis.Free;
  end;
end;

class function TJSFunction.Cast(const Intf: IJSObject): IJSFunction;
begin
  Result:=TJSFunction.Cast(Intf);
end;

{ TJSMap }

class function TJSMap.Cast(const Intf: IJSObject): IJSMap;
begin
  Result:=TJSMap.Cast(Intf);
end;

{ TJSSet }

class function TJSSet.Cast(const Intf: IJSObject): IJSSet;
begin
  Result:=TJSSet.Cast(Intf);
end;

{ TJOBCallbackHelper }

procedure TJOBCallbackHelper.Init(Args: PByte);
begin
  p:=Args;
  Index:=0;
  if p<>nil then
  begin
    Count:=p^;
    inc(p);
  end else
    Count:=0;
end;

function TJOBCallbackHelper.GetType: byte;
begin
  if Index=Count then
    Result:=JOBArgUndefined
  else
    Result:=p^;
end;

procedure TJOBCallbackHelper.Skip;
var
  Len: LongWord;
begin
  if Index=Count then exit;
  case p^ of
  JOBArgUndefined,
  JOBArgTrue,
  JOBArgFalse,
  JOBArgNil: inc(p);
  JOBArgDouble: inc(p,9);
  JOBArgMethod: inc(p,3*SizeOf(Pointer));
  JOBArgUnicodeString:
    begin
      inc(p);
      Len:=PLongWord(p)^;
      inc(p,4+2*Len);
    end
  else
    raise EJSArgParse.Create(JOBArgNames[p^]);
  end;
  inc(Index);
end;

function TJOBCallbackHelper.GetBoolean: boolean;
begin
  Result:=false;
  if Index=Count then
    exit;
  case p^ of
  JOBArgUndefined: ;
  JOBArgTrue: Result:=true;
  JOBArgFalse: ;
  else
    raise EJSArgParse.Create(JOBArgNames[p^]);
  end;
  inc(p);
  inc(Index);
end;

function TJOBCallbackHelper.GetDouble: double;
begin
  Result:=NaN;
  if Index=Count then
    exit;
  case p^ of
  JOBArgUndefined:
    inc(p);
  JOBArgDouble:
    begin
      inc(p);
      Result:=PDouble(p)^;
      inc(p,8);
    end
  else
    raise EJSArgParse.Create(JOBArgNames[p^]);
  end;
  inc(Index);
end;

function TJOBCallbackHelper.GetString: UnicodeString;
var
  Len: LongWord;
begin
  Result:='';
  if Index=Count then
    exit;
  case p^ of
  JOBArgUndefined:
    inc(p);
  JOBArgUnicodeString:
    begin
      inc(p);
      Len:=PLongWord(p)^;
      inc(p,4);
      if Len>0 then
      begin
        SetLength(Result,Len);
        Move(p^,Result[1],2*Len);
        inc(p,2*Len);
      end;
    end
  else
    raise EJSArgParse.Create(JOBArgNames[p^]);
  end;
  inc(Index);
end;

function TJOBCallbackHelper.GetObject(aResultClass: TJSObjectClass): TJSObject;
var
  ObjId: LongWord;
begin
  //writeln('TJOBCallbackHelper.GetObject ',Index,' Count=',Count);
  Result:=nil;
  if Index=Count then
    exit;
  //writeln('TJOBCallbackHelper.GetObject type=',p^);
  case p^ of
  JOBArgUndefined,
  JOBArgNil:
    inc(p);
  JOBArgObject:
    begin
      inc(p);
      ObjId:=PLongWord(p)^;
      inc(p,4);
      Result:=aResultClass.JOBCreateFromID(ObjId);
      Result.JOBObjectIDOwner:=false; // owned by caller (JS code in browser)
    end
  else
    raise EJSArgParse.Create(JOBArgNames[p^]);
  end;
  inc(Index);
end;

function TJOBCallbackHelper.GetArray: TJSArray;
var
  ObjId: LongWord;
begin
  //writeln('TJOBCallbackHelper.GetObject ',Index,' Count=',Count);
  Result:=nil;
  if Index=Count then
    exit;
  //writeln('TJOBCallbackHelper.GetObject type=',p^);
  case p^ of
  JOBArgUndefined,
  JOBArgNil:
    inc(p);
  JOBArgObject:
    begin
      inc(p);
      ObjId:=PLongWord(p)^;
      inc(p,4);
      Result:=TJSArray.JOBCreateFromID(ObjId);
      Result.JOBObjectIDOwner:=false; // owned by caller (JS code in browser)
    end
  else
    raise EJSArgParse.Create(JOBArgNames[p^]);
  end;
  inc(Index);
end;

function TJOBCallbackHelper.GetFunction: IJSFunction;
var
  aType : byte;
  ObjId,ThisId: LongWord;
  F : TJSFunction;

begin
  {$IFDEF VerboseJOB}
  writeln('TJOBCallbackHelper.GetFunction ',Index,' Count=',Count);
  {$ENDIF}
  Result:=Nil;
  aType:=p^;
  if not (aType in [JOBArgObject,JOBArgFunction]) then
    raise EJSArgParse.Create(JOBArgNames[aType]);
  Inc(p);
  ThisId:=0;
  ObjId:=PLongWord(p)^;
  inc(p,4);
  if (aType=JOBArgFunction) then
    begin
    ThisId:=PLongWord(p)^;
    inc(p,4);
    end;
  F:=TJSFunction.Create(ObjId,ThisId);
  F.JOBObjectIDOwner:=false; // owned by caller (JS code in browser)
  Result:=F;
end;

function TJOBCallbackHelper.GetValue: TJOB_JSValue;
var
  ObjId, Len: LongWord;
  Obj: TJSObject;
  S: UnicodeString;
begin
  Result:=nil;
  if (Index=Count) or (p^=JOBArgUndefined) then
  begin
    Result:=TJOB_JSValue.Create(jjvkUndefined);
    exit;
  end;
  case p^ of
  JOBArgTrue:
    begin
      Result:=TJOB_Boolean.Create(true);
      inc(p);
    end;
  JOBArgFalse:
    begin
      Result:=TJOB_Boolean.Create(false);
      inc(p);
    end;
  JOBArgDouble:
    begin
      inc(p);
      Result:=TJOB_Double.Create(PDouble(p)^);
      inc(p,8);
    end;
  JOBArgUnicodeString:
    begin
      inc(p);
      Len:=PLongWord(p)^;
      inc(p,4);
      S:='';
      if Len>0 then
      begin
        SetLength(S,Len);
        Move(p^,S[1],2*Len);
        inc(p,2*Len);
      end;
      Result:=TJOB_String.Create(S);
    end;
  JOBArgNil:
    begin
      Result:=TJOB_Object.Create(nil);
      inc(p);
    end;
  JOBArgObject:
    begin
      inc(p);
      ObjId:=PLongWord(p)^;
      inc(p,4);
      Obj:=TJSObject.JOBCreateFromID(ObjId);
      Result:=TJOB_Object.Create(Obj);
    end;
  else
    raise EJSArgParse.Create(JOBArgNames[p^]);
  end;
  inc(Index);
end;

function TJOBCallbackHelper.GetVariant: Variant;
var
  ObjId, Len: LongWord;
  Obj: TJSObject;
  S: UnicodeString;
begin
  if Index=Count then
  begin
    Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Variants.Unassigned;
    exit;
  end;
  case p^ of
  JOBArgUndefined:
    begin
      Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Variants.Unassigned;
      inc(p);
    end;
  JOBArgTrue:
    begin
      Result:=true;
      inc(p);
    end;
  JOBArgFalse:
    begin
      Result:=false;
      inc(p);
    end;
  JOBArgDouble:
    begin
      inc(p);
      Result:=PDouble(p)^;
      inc(p,8);
    end;
  JOBArgUnicodeString:
    begin
      inc(p);
      Len:=PLongWord(p)^;
      inc(p,4);
      S:='';
      if Len>0 then
      begin
        SetLength(S,Len);
        Move(p^,S[1],2*Len);
        inc(p,2*Len);
      end;
      Result:=S;
    end;
  JOBArgNil:
    begin
      Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Variants.Null;
      inc(p);
    end;
  JOBArgObject:
    begin
      inc(p);
      ObjId:=PLongWord(p)^;
      inc(p,4);
      Obj:=TJSObject.JOBCreateFromID(ObjId);
      Obj.JOBObjectIDOwner:=false;
      Result:=Obj as IJSObject;
    end;
  else
    raise EJSArgParse.Create(JOBArgNames[p^]);
  end;
  inc(Index);
end;

function TJOBCallbackHelper.GetLongInt: longint;
var
  d: Double;
begin
  d:=GetDouble;
  if (Frac(d)<>0) or (d<low(longint)) or (d>high(longint)) then
    raise EJSArgParse.Create('expected longint, but got double')
  else
    Result:=Trunc(d);
end;

function TJOBCallbackHelper.GetMaxInt: int64;
var
  d: Double;
begin
  d:=GetDouble;
  if (Frac(d)<>0) or (d<low(int64)) or (d>high(int64)) then
    raise EJSArgParse.Create('expected int64, but got double')
  else
    Result:=Trunc(d);
end;

function TJOBCallbackHelper.AllocUndefined: PByte;
begin
  GetMem(Result,1);
  Result^:=JOBArgUndefined;
end;

function TJOBCallbackHelper.AllocBool(b: boolean): PByte;
begin
  GetMem(Result,1);
  if b then
    Result^:=JOBArgTrue
  else
    Result^:=JOBArgFalse;
end;

function TJOBCallbackHelper.AllocLongint(i: longint): PByte;
begin
  GetMem(Result,5);
  Result^:=JOBArgLongint;
  PLongint(Result+1)^:=i;
end;

function TJOBCallbackHelper.AllocDouble(const d: double): PByte;
begin
  GetMem(Result,9);
  Result^:=JOBArgDouble;
  PDouble(Result+1)^:=d;
end;

function TJOBCallbackHelper.AllocString(const s: UnicodeString): PByte;
var
  l: SizeInt;
begin
  l:=length(s);
  GetMem(Result,5+2*l);
  Result^:=JOBArgUnicodeString;
  PLongWord(Result+1)^:=l;
  if l>0 then
    Move(s[1],Result[5],2*l);
end;

function TJOBCallbackHelper.AllocNil: PByte;
begin
  GetMem(Result,1);
  Result^:=JOBArgNil;
end;

function TJOBCallbackHelper.AllocIntf(const Intf: IJSObject): PByte;
begin
  if Intf=nil then
    Result:=AllocNil
  else
    Result:=AllocObjId(Intf.GetJSObjectID);
end;

function TJOBCallbackHelper.AllocObject(Obj: TJSObject): PByte;
begin
  if Obj=nil then
    Result:=AllocNil
  else
    Result:=AllocObjId(Obj.JOBObjectID);
end;

function TJOBCallbackHelper.AllocObjId(ObjId: TJOBObjectID): PByte;
begin
  //writeln('TJOBCallbackHelper.AllocObjId ObjID=',ObjId);
  GetMem(Result,1+SizeOf(TJOBObjectID));
  Result^:=JOBArgObject;
  PJOBObjectID(Result+1)^:=ObjId;
end;

function TJOBCallbackHelper.AllocJSValue(const Value: TJOB_JSValue): PByte;
begin
  if Value=nil then
    exit(AllocUndefined);
  case Value.Kind of
    jjvkUndefined: Result:=AllocUndefined;
    jjvkBoolean: Result:=AllocBool(TJOB_Boolean(Value).Value);
    jjvkDouble: Result:=AllocDouble(TJOB_Double(Value).Value);
    jjvkString: Result:=AllocString(TJOB_String(Value).Value);
    jjvkObject: Result:=AllocIntf(TJOB_Object(Value).Value);
  else
    raise EJSArgParse.Create('AllocJSValue unsupported: '+JOB_JSValueKindNames[Value.Kind]);
  end;
end;

function TJOBCallbackHelper.AllocVariant(const Value: Variant): PByte;
var
  t: tvartype;
  Intf: IJSObject;
begin
  t:=VarType(Value);
  case t of
  varEmpty:
    Result:=AllocUndefined;
  varNull:
    Result:=AllocNil;
  varSmallInt,varInteger,varByte,varWord,varShortInt:
    Result:=AllocLongint(Value);
  varLongWord,varCurrency,varInt64,varQWord,varSingle,varDouble,varDate:
    Result:=AllocDouble(Value);
  varOleStr,varString:
    Result:=AllocString(Value);
  varBoolean:
    Result:=AllocBool(Value);
  varUnknown:
    begin
    if tvardata(Value).vunknown=nil then
      Result:=AllocNil
    else if VarSupports(Value,IJSObject,Intf) then
      Result:=AllocIntf(Intf)
    else
      raise EJSInvoke.Create('TJOBCallbackHelper.AllocVariant: [20220822103744] unsupported variant: '+IntToStr(t));
    end
  else
    raise EJSInvoke.Create('TJOBCallbackHelper.AllocVariant: [20220822103751] unsupported variant: '+IntToStr(t));
  end;
end;

{ TJOB_JSValue }

constructor TJOB_JSValue.Create(aKind: TJOB_JSValueKind);
begin
  Kind:=aKind;
end;

function TJOB_JSValue.AsString: UTF8string;
begin
  if Kind=jjvkUndefined then
    Result:='undefined'
  else begin
    Result:='';
    str(Kind,Result);
  end;
end;

function TJOB_JSValue.AsVariant: Variant;
begin
  Result:=Unassigned;
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

function TJOB_Boolean.AsVariant: Variant;
begin
  Result:=Value;
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

function TJOB_Double.AsVariant: Variant;
begin
  Result:=Value;
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

function TJOB_String.AsVariant: Variant;
begin
  Result:=Value;
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
    Result:='['+IntToStr(Value.GetJSObjectID)+']:'+Value.GetPascalClassName;
end;

function TJOB_Object.AsVariant: Variant;
begin
  Result:=Value;
end;

{ TJOB_Function }

constructor TJOB_Function.Create(aValue: IJSFunction);
begin
  Kind:=jjvkObject;
  Value:=aValue;
end;

function TJOB_Function.AsString: UTF8String;
begin
  Result:=inherited AsString;
end;

function TJOB_Function.AsVariant: Variant;
begin
  Result:=Value;
end;

{ TJOB_Method }

constructor TJOB_Method.Create(const aMethod: TMethod;
  const AnInvoke: TJOBCallback);
begin
  Kind:=jjvkMethod;
  Value:=aMethod;
  Invoke:=AnInvoke;
end;

function TJOB_Method.AsString: UTF8string;
begin
  Result:='Callback';
end;

{ TJOB_Dictionary }

procedure TJOB_Dictionary.Add(const aName: UnicodeString;
  const aValue: TJOB_JSValue);
var
  p: TJOB_Pair;
begin
  p.Name:=aName;
  p.Value:=aValue;
  Insert(p,Values,length(Values));
end;

constructor TJOB_Dictionary.Create(const Pairs: array of const);
var
  i: Integer;
  l, CurLen: SizeInt;
  CurName: UnicodeString;
begin
  inherited Create(jjvkDictionary);
  l:=length(Pairs);
  SetLength(Values,l div 2);
  for i:=0 to length(Values)-1 do
    Values[i].Value:=nil;
  i:=0;
  while i<l do
  begin
    case Pairs[i].VType of
    vtChar:
      CurName:=UnicodeString(Pairs[i].VChar);
    vtString:
      CurName:=UTF8Decode(Pairs[i].VString^);
    vtPChar:
      begin
      CurLen:=strlen(Pairs[i].VPChar);
      SetString(CurName,Pairs[i].VPChar,CurLen);
      end;
    vtWideChar:
      CurName:=Pairs[i].VWideChar;
    vtAnsiString:
      CurName:=UTF8Decode(PAnsiString(Pairs[i].VAnsiString)^);
    vtUnicodeString:
      CurName:=PUnicodeString(Pairs[i].VUnicodeString)^;
    else
      raise EJSArgParse.Create('TJOB_Dictionary.Create expected name at index '+IntToStr(i)+', but found '+IntToStr(Pairs[i].VType));
    end;
    Values[i div 2].Name:=CurName;
    inc(i);
    if i=l then
      raise EJSArgParse.Create('TJOB_Dictionary.Create name "'+String(CurName)+'" has no value');
    Values[i div 2].Value:=VarRecToJSValue(Pairs[i]);
    inc(i);
  end;
end;

destructor TJOB_Dictionary.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJOB_Dictionary.Clear;
var
  i: Integer;
begin
  for i:=0 to length(Values)-1 do
    FreeAndNil(Values[i].Value);
  Values:=nil;
end;

{ TJOB_ArrayOfJSValue }

procedure TJOB_ArrayOfJSValue.Add(const aValue: TJOB_JSValue);
begin
  Insert(aValue,Values,length(Values));
end;

constructor TJOB_ArrayOfJSValue.Create(const TheValues: array of const);
var
  l: SizeInt;
  i: Integer;
begin
  inherited Create(jjvkArrayOfJSValue);
  l:=length(TheValues);
  SetLength(Values,l);
  for i:=0 to l-1 do
    Values[i]:=nil;
  for i:=0 to l-1 do
    Values[i]:=VarRecToJSValue(TheValues[i]);
end;

destructor TJOB_ArrayOfJSValue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJOB_ArrayOfJSValue.Clear;
var
  i: Integer;
begin
  for i:=0 to length(Values)-1 do
    FreeAndNil(Values[i]);
  Values:=nil;
end;

function TJOB_ArrayOfJSValue.AsVariant: Variant;

var
  I : integer;

begin
  Result:=VarArrayCreate([0,Length(Values)-1],varVariant);
  for i:=0 to Length(Values)-1 do
    Result[i]:=Values[i].AsVariant;
end;

{ TJOB_ArrayOfDouble }

constructor TJOB_ArrayOfDouble.Create(const TheValues: TDoubleDynArray);
begin
  inherited Create(jjvkArrayOfDouble);
  Values:=TheValues;
end;

function TJOB_ArrayOfDouble.AsVariant: Variant;

var
  I : integer;

begin
  Result:=VarArrayCreate([0,Length(Values)-1],varDouble);
  for i:=0 to Length(Values)-1 do
    Result[i]:=Values[i];
end;

{ TJOB_ArrayOfByte }

constructor TJOB_ArrayOfByte.Create(const TheValues: PByte; TheLen: NativeUInt);
begin
  inherited Create(jjvkArrayOfByte);
  Values:=TheValues;
  Len:=TheLen;
end;

constructor TJOB_ArrayOfByte.Create(const TheValues: TBytes);
begin
  Create(PByte(TheValues),length(TheValues))
end;

function TJOB_ArrayOfByte.AsVariant: Variant;
var
  I : integer;

begin
  Result:=VarArrayCreate([0,Len-1],varByte);
  for i:=0 to Len-1 do
    Result[i]:=Values[i];
end;

{ TJSObject }

function TJSObject.GetJSObjectID: TJOBObjectID;
begin
  Result:=FJOBObjectID;
end;

function TJSObject.GetJSObjectCastSrc: IJSObject;
begin
  Result:=FJOBCastSrc;
end;

function TJSObject.GetPascalClassName: UTF8String;
begin
  Result:=ClassName;
end;

function TJSObject.GetProperties(const PropName: UTF8String): Variant;
begin
  Result:=ReadJSPropertyVariant(PropName);
end;

procedure TJSObject.SetProperties(const PropName: UTF8String; const AValue: Variant);
begin
  WriteJSPropertyVariant(PropName,AValue);
end;

function TJSObject.FetchString(Len: NativeInt): UnicodeString;
var
  ok: Boolean;
begin
  if Len=0 then exit('');
  ok:=false;
  try
    // try to allocate the memory
    SetLength(Result,Len);
    ok:=true;
  finally
    if not ok then
      __job_releasestringresult();
  end;
  __job_getstringresult(PByte(Result));
end;

function TJSObject.InvokeJSNoResultFunc(const aName: UTF8string; const Args: array of const;
  const InvokeFunc: TJOBInvokeNoResultFunc; Invoke: TJOBInvokeType): TJOBResult;
var
  InvokeArgs: PByte;
begin
  if length(Args)=0 then
    Result:=InvokeFunc(JOBObjectID,PChar(aName),length(aName),InvokeGetToInt[Invoke],nil)
  else begin
    InvokeArgs:=CreateInvokeJSArgs(Args);
    try
      Result:=InvokeFunc(JOBObjectID,PChar(aName),length(aName),InvokeGetToInt[Invoke],InvokeArgs);
    finally
      if InvokeArgs<>nil then
        FreeMem(InvokeArgs);
    end;
  end;
end;

function TJSObject.InvokeJSOneResult(const aName: UTF8string; const Args: array of const;
  const InvokeFunc: TJOBInvokeOneResultFunc; ResultP: PByte; Invoke: TJOBInvokeType): TJOBResult;
var
  InvokeArgs: PByte;
begin
  if length(Args)=0 then
    Result:=InvokeFunc(JOBObjectID,PChar(aName),length(aName),InvokeGetToInt[Invoke],nil,ResultP)
  else begin
    InvokeArgs:=CreateInvokeJSArgs(Args);
    try
      Result:=InvokeFunc(JOBObjectID,PChar(aName),length(aName),InvokeGetToInt[Invoke],InvokeArgs,ResultP);
    finally
      if InvokeArgs<>nil then
        FreeMem(InvokeArgs);
    end;
  end;
end;

procedure TJSObject.InvokeJS_Raise(const aName, Msg: UTF8string);
var
  E: EJSInvoke;
begin
  E:=EJSInvoke.Create(Msg);
  E.ObjectID:=JOBObjectID;
  E.FuncName:=aName;
  raise E;
end;

procedure TJSObject.InvokeJS_RaiseResultMismatch(const aName: UTF8string; Expected, Actual: TJOBResult);
begin
  case Actual of
  JOBResult_UnknownObjId: InvokeJS_Raise(aName,'unknown object id '+IntToStr(JOBObjectID));
  JOBResult_NotAFunction: InvokeJS_Raise(aName,'object '+IntToStr(JOBObjectID)+' does not have a function "'+aName+'"');
  else
    InvokeJS_RaiseResultMismatchStr(aName,JOBResult_Names[Expected],JOBResult_Names[Actual]);
  end;
end;

procedure TJSObject.InvokeJS_RaiseResultMismatchStr(const aName: UTF8string; const Expected, Actual: UTF8string);
begin
  InvokeJS_Raise(aName,'expected '+Expected+', but got '+Actual+' from object '+IntToStr(JOBObjectID)+' function "'+aName+'"');
end;

function TJSObject.CreateInvokeJSArgs(const Args: array of const): PByte;

  procedure RaiseNotSupported(const Msg: string);
  begin
    raise EJSInvoke.Create('Invoke js: type not supported '+Msg);
  end;

  procedure RaiseRange;
  begin
    raise ERangeError.Create('Invoke js: number out of bounds');
  end;

var
  p: PByte;
  Len: NativeInt;

  function SizeOfTJOB_JSValue(JSValue: TJOB_JSValue): integer;
  var
    Dict: TJOB_PairArray;
    i: Integer;
    Arr: TJOB_JSValueArray;
  begin
    case JSValue.Kind of
      jjvkUndefined: Result:=1;
      jjvkBoolean: Result:=1;
      jjvkDouble: Result:=9;
      jjvkString: Result:=1+SizeOf(NativeInt)+SizeOf(PByte);
      jjvkObject:
        if TJOB_Object(JSValue).Value=nil then
          Result:=1
        else
          Result:=1+SizeOf(TJOBObjectID);
      jjvkMethod: Result:=1+3*SizeOf(PByte);
      jjvkDictionary:
        begin
          Result:=1+SizeOf(NativeInt);
          Dict:=TJOB_Dictionary(JSValue).Values;
          for i:=0 to length(Dict)-1 do
            begin
            inc(Result,1+SizeOf(NativeInt)+SizeOf(PByte));
            inc(Result,SizeOfTJOB_JSValue(Dict[i].Value));
            end;
        end;
      jjvkArrayOfJSValue:
        begin
          Result:=1+SizeOf(NativeInt);
          Arr:=TJOB_ArrayOfJSValue(JSValue).Values;
          for i:=0 to length(Arr)-1 do
            inc(Result,SizeOfTJOB_JSValue(Dict[i].Value));
        end;
      jjvkArrayOfDouble:
        Result:=1+SizeOf(NativeInt)+SizeOf(PByte);
      else
        RaiseNotSupported('20220630135718'){%H-};
    end;
  end;

  procedure Grow(Need: NativeInt);
  begin
    inc(Need,p-Result);
    if Need<=Len then exit;
    Len:=Len*2;
    if Len<Need then
      Len:=Need;
    Need:=p-Result;
    ReAllocMem(Result,Len);
    p:=Result+Need;
  end;

  procedure Prep(Need: NativeInt; aType: Byte);
  begin
    Grow(Need);
    p^:=aType;
    inc(p);
  end;

  procedure AddBoolean(b: boolean);
  begin
    Grow(1);
    if b then
      p^:=JOBArgTrue
    else
      p^:=JOBArgFalse;
    inc(p);
  end;

  procedure AddLongInt(const i: LongInt);
  begin
    Prep(5,JOBArgLongint);
    PLongint(p)^:=i;
    inc(p,4);
  end;

  procedure AddDouble(const d: double);
  begin
    Prep(9,JOBArgDouble);
    PDouble(p)^:=d;
    inc(p,8);
  end;

  procedure AddChar(c: word);
  begin
    Prep(3,JOBArgChar);
    PWord(p)^:=c;
    inc(p,2);
  end;

  procedure AddObjectID(const ObjId: TJOBObjectID);
  begin
    Prep(1+SizeOf(NativeInt),JOBArgObject);
    PNativeInt(p)^:=ObjId;
    inc(p,sizeof(NativeInt));
  end;

  procedure AddIJSObject(const Intf: IJSObject);
  begin
    if Intf=nil then
      Prep(1,JOBArgNil)
    else
      AddObjectID(Intf.GetJSObjectID);
  end;

  procedure AddUnicodeString(s: PByte; Len: NativeInt); overload;
  begin
    Prep(1+SizeOf(NativeInt)+SizeOf(Pointer),JOBArgUnicodeString);
    PNativeInt(p)^:=Len;
    inc(p,sizeof(NativeInt));
    PPointer(p)^:=s;
    inc(p,sizeof(Pointer));
  end;

  procedure AddUnicodeString(const us: UnicodeString); overload;
  begin
    if us='' then
      AddUnicodeString(nil,0)
    else
      AddUnicodeString(@us[1],length(us));
  end;

  procedure AddUTF8String(const s: String); overload;
  var
    us: UnicodeString;
    l: SizeInt;
  begin
    // writeln('AddUTF8String s="',s,'"');
    if s='' then
    begin
      AddUnicodeString(nil,0);
      exit;
    end;
    us:=UTF8Decode(s);
    l:=length(us);
    // writeln('AddUTF8String us="',us,'"');
    if l=0 then
    begin
      AddUnicodeString(nil,0);
      exit;
    end;
    Prep(1+SizeOf(NativeInt)+2*l,JOBArgString);
    PNativeInt(p)^:=l;
    inc(p,SizeOf(NativeInt));
    Move(us[1],p^,2*l);
    inc(p,2*l);
  end;

  procedure AddUTF8String(p: PByte; l: NativeInt);
  var
    s: string;
  begin
    if (p=nil) or (l=0) then
    begin
      AddUnicodeString(nil,0);
      exit;
    end;
    SetString(s,PAnsiChar(p),l);
    AddUTF8String(s);
  end;

  procedure Add_TJOB_JSValue(aValue: TJOB_JSValue);
  var
    us: UnicodeString;
    h: PByte;
    aMethod: TJOB_Method;
    Dict: TJOB_PairArray;
    i: Integer;
    Arr: TJOB_JSValueArray;
  begin
    case aValue.Kind of
      jjvkUndefined:
        Prep(1,JOBArgUndefined);
      jjvkBoolean:
        AddBoolean(TJOB_Boolean(aValue).Value);
      jjvkDouble:
        AddDouble(TJOB_Double(aValue).Value);
      jjvkString:
        begin
          us:=TJOB_String(aValue).Value;
          h:=PByte(PWideChar(us));
          AddUnicodeString(h,length(us));
        end;
      jjvkObject:
        AddIJSObject(TJOB_Object(aValue).Value);
      jjvkMethod:
        begin
          aMethod:=TJOB_Method(aValue);
          Prep(1+3*SizeOf(Pointer),JOBArgMethod);
          PPointer(p)^:=Pointer(aMethod.Invoke);
          inc(p,sizeof(Pointer));
          PPointer(p)^:=aMethod.Value.Data;
          inc(p,sizeof(Pointer));
          PPointer(p)^:=aMethod.Value.Code;
          inc(p,sizeof(Pointer));
        end;
      jjvkDictionary:
        begin
          Dict:=TJOB_Dictionary(aValue).Values;
          Prep(1+SizeOf(NativeInt),JOBArgDictionary);
          PNativeInt(p)^:=length(Dict);
          inc(p,SizeOf(NativeInt));
          for i:=0 to length(Dict)-1 do
          begin
            AddUnicodeString(Dict[i].Name);
            Add_TJOB_JSValue(Dict[i].Value);
          end;
        end;
      jjvkArrayOfJSValue:
        begin
          Arr:=TJOB_ArrayOfJSValue(aValue).Values;
          Prep(1+SizeOf(NativeInt),JOBArgArrayOfJSValue);
          PNativeInt(p)^:=length(Arr);
          inc(p,SizeOf(NativeInt));
          for i:=0 to length(Arr)-1 do
            Add_TJOB_JSValue(Arr[i]);
        end;
      jjvkArrayOfDouble:
        begin
          Prep(1+SizeOf(NativeInt)+SizeOf(Pointer),JOBArgArrayOfDouble);
          i:=length(TJOB_ArrayOfDouble(aValue).Values);
          PNativeInt(p)^:=i;
          inc(p,SizeOf(NativeInt));
          if i=0 then
            PPointer(p)^:=nil
          else
            PPointer(p)^:=@TJOB_ArrayOfDouble(aValue).Values[0];
          inc(p,sizeof(Pointer));
        end;
      jjvkArrayOfByte:
        begin
          Prep(1+SizeOf(NativeInt)+SizeOf(Pointer),JOBArgArrayOfByte);
          i:=TJOB_ArrayOfByte(aValue).Len;
          PNativeInt(p)^:=i;
          inc(p,SizeOf(NativeInt));
          if i=0 then
            PPointer(p)^:=nil
          else
            PPointer(p)^:=@TJOB_ArrayOfByte(aValue).Values[0];
          inc(p,sizeof(Pointer));
        end;
    end;
  end;

  procedure AddVariant(Index: integer);
  var
    v: Variant;
    t: tvartype;
    us: UnicodeString;
    Intf: IJSObject;
  begin
    v:=Args[Index].VVariant^;
    t:=VarType(v);
    {$IFDEF VERBOSEJOB}
    writeln('AddVariant Index=',Index,' VarType=',t);
    {$ENDIF}
    case t of
    varEmpty:
      Prep(1,JOBArgUndefined);
    varNull:
      Prep(1,JOBArgNil);
    varSmallInt,varInteger,varByte,varWord,varShortInt:
      AddLongInt(v);
    varLongWord,varCurrency,varInt64,varQWord,varSingle,varDouble,varDate:
      AddDouble(v);
    varOleStr:
      begin
        us:=v;
        AddUnicodeString(us);
      end;
    varBoolean:
      if v then
        Prep(1,JOBArgTrue)
      else
        Prep(1,JOBArgFalse);
    varString:
      AddUTF8String(v);
    varUnknown:
      begin
      if tvardata(v).vunknown=nil then
        Prep(1,JOBArgNil)
      else if VarSupports(v,IJSObject,Intf) then
        AddObjectID(Intf.GetJSObjectID)
      else
        raise EJSInvoke.Create('Invoke js: [20220820210022] unsupported variant: '+IntToStr(t));
      end
    else
      raise EJSInvoke.Create('Invoke js: [20220820185131] unsupported variant: '+IntToStr(t));
    end;
  end;

var
  i: Integer;
  qw: QWord;
  i64: Int64;
  h: PByte;
  s: String;
  ws: WideString;
  us: UnicodeString;
  Obj: TObject;
  JSValue: TJOB_JSValue;
  ok: Boolean;
begin
  Result:=nil;
  if length(Args)>255 then
    raise EJSInvoke.Create('Invoke js: too many args');

  Len:=1+length(Args);
  Result:=GetMem(Len);
  ok:=false;
  try
    p:=Result;

    p^:=length(Args);
    inc(p);
    for i:=0 to high(Args) do
    begin
      case Args[i].VType of
      vtInteger:
        AddLongInt(Args[i].VInteger);
      vtBoolean:
        AddBoolean(Args[i].VBoolean);
      vtExtended:
        AddDouble(double(Args[i].VExtended^));
      vtChar:
        AddChar(ord(Args[i].VChar));
      vtWideChar:
        AddChar(ord(Args[i].VWideChar));
      vtString:
        begin
          // shortstring
          h:=PByte(Args[i].VString);
          AddUTF8String(h+1,h^);
        end;
      vtPointer:
        begin
          h:=Args[i].VPointer;
          if h=nil then
            Prep(1,JOBArgNil)
          else if h=JOB_Undefined then
            Prep(1,JOBArgUndefined)
          else begin
            Prep(1+SizeOf(Pointer),JOBArgPointer);
            PPointer(p)^:=h;
            inc(p,sizeof(Pointer));
          end;
        end;
      vtPChar:
        begin
          h:=PByte(Args[i].VPChar);
          AddUTF8String(h,strlen(PChar(h)));
        end;
      vtObject:
        begin
          Obj:=Args[i].VObject;
          if Obj=nil then
            Prep(1,JOBArgNil)
          else if Obj is TJSObject then
            AddObjectID(TJSObject(Obj).JOBObjectID)
          else if Obj is TJOB_JSValue then
          begin
            JSValue:=TJOB_JSValue(Obj);
            Add_TJOB_JSValue(JSValue);
          end else
            RaiseNotSupported(Obj.ClassName);
        end;
      vtClass: ;
      vtPWideChar:
        begin
          h:=PByte(Args[i].VPWideChar);
          AddUnicodeString(h,strlen(PWideChar(h)));
        end;
      vtAnsiString:
        begin
          h:=Args[i].VAnsiString;
          s:=AnsiString(h);
          AddUTF8String(h,length(s));
        end;
      vtCurrency:
        AddDouble(double(Args[i].VCurrency^));
      vtVariant:
        AddVariant(i);
      vtInterface:
        begin
          h:=Args[i].VInterface;
          AddIJSObject(IJSObject(h));
        end;
      vtWideString:
        begin
          h:=Args[i].VWideString;
          ws:=WideString(h);
          AddUnicodeString(h,length(ws));
        end;
      vtInt64:
        begin
          i64:=Args[i].VInt64^;
          if (i64>=low(longint)) and (i64<=high(longint)) then
            AddLongInt(i64)
          else
            AddDouble(i64);
        end;
      vtUnicodeString:
        begin
          h:=Args[i].VUnicodeString;
          us:=UnicodeString(h);
          AddUnicodeString(h,length(us));
        end;
      vtQWord:
        begin
          qw:=Args[i].VQWord^;
          if (qw<=high(longint)) then
            AddLongInt(qw)
          else
            AddDouble(qw);
        end;
      else
        RaiseNotSupported(IntToStr(Args[i].VType));
      end;
    end;
    Len:=p-Result;
    ReAllocMem(Result,Len);
    ok:=true;
  finally
    if not ok then
      FreeMemAndNil(Result);
  end;

  {$IFDEF VerboseInvokeJSArgs}
  s:='TJSObject.CreateInvokeJSArgs ArgCnt='+IntToStr(length(Args));
  for i:=0 to high(Args) do
    s:=s+' '+GetVarRecName(Args[i].VType);
  s:=s+' Len='+IntToStr(Len);
  s:=s+' Bytes=';
  for i:=0 to Len-1 do
    s:=s+HexStr(Result[i],2);
  writeln(s);
  {$ENDIF}
end;

constructor TJSObject.JOBCast(const Intf: IJSObject);
begin
  FJOBObjectID:=Intf.GetJSObjectID;
  FJOBCastSrc:=Intf.GetJSObjectCastSrc;
  if FJOBCastSrc=nil then
    FJOBCastSrc:=Intf;
end;

constructor TJSObject.JOBCreateFromID(aID: TJOBObjectID);
begin
  FJOBObjectID:=aID;
  FJOBObjectIDOwner:=true;
end;

constructor TJSObject.JOBCreateGlobal(const aID: UnicodeString);
begin
  FJOBObjectID:=__job_get_global(PWideChar(aID),length(aID));
  if FJOBObjectID=0 then
    raise EJSObject.Create('JS object "'+String(aID)+'" is not registered');
  FJOBObjectIDOwner:=true;
end;

constructor TJSObject.JOBCreate(const Args: array of const);

begin
  JOBCreate(True,Args);
end;

constructor TJSObject.JOBCreate(aOwnsObjectID: Boolean; const Args: array of const);
var
  N : Unicodestring;
  InvokeArgs: PByte;

begin
  N:=JSClassName;
  if Length(Args)>0 then
    InvokeArgs:=CreateInvokeJSArgs(Args)
  else
    InvokeArgs:=Nil;
  FJOBObjectIDOwner:=aOwnsObjectID;
  FJobObjectID:=__job_create_object(PWideChar(N),Length(N),InvokeArgs);
  {$IFDEF VERBOSEJOB}
  Writeln('[',ClassName,'] Created new object with ID: ',FJobObjectID);
  {$ENDIF}
end;


class function TJSObject.Cast(const Intf: IJSObject): IJSObject;
begin
  Result:=JOBCast(Intf);
end;

class function TJSObject.JSClassName : UnicodeString;

begin
  Result:='Object';
end;

constructor TJSObject.Create;

begin
  JOBCreate(True,[]);
end;

destructor TJSObject.Destroy;
begin
  {$IFDEF VERBOSEJOB}
  Writeln('Destroying ',ClassName,': Owning JOB object ID: ',FJOBObjectIDOwner);
  {$ENDIF}
  if FJOBCastSrc<>nil then
    FJOBCastSrc:=nil
  else if (JOBObjectID>=0) and JOBObjectIDOwner then
    __job_release_object(JOBObjectID);
  FJOBObjectID:=0;
  inherited Destroy;
end;

procedure TJSObject.InvokeJSNoResult(const aName: UTF8String; const Args: array of const; Invoke: TJOBInvokeType);
var
  aError: TJOBResult;
begin
  aError:=InvokeJSNoResultFunc(aName,Args,@__job_invoke_noresult,Invoke);
  if aError<>JOBResult_Success then
    InvokeJS_RaiseResultMismatch(aName,JOBResult_Success,aError);
end;

function TJSObject.InvokeJSBooleanResult(const aName: UTF8string; const Args: array of const; Invoke: TJOBInvokeType): Boolean;
var
  aError: TJOBResult;
  b: bytebool;
begin
  b:=false;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_boolresult,@b,Invoke);
  if aError=JOBResult_Boolean then
  else if aError=JOBResult_Undefined then
    b:=false
  else
    InvokeJS_RaiseResultMismatch(aName,JOBResult_Boolean,aError);
  Result:=b;
end;

function TJSObject.InvokeJSDoubleResult(const aName: UTF8string; const Args: array of const; Invoke: TJOBInvokeType): Double;
var
  aError: TJOBResult;
begin
  Result:=NaN;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_doubleresult,@Result,Invoke);
  if aError=JOBResult_Double then
  else if aError=JOBResult_Undefined then
    Result:=NaN
  else
    InvokeJS_RaiseResultMismatch(aName,JOBResult_Double,aError);
end;

function TJSObject.InvokeJSUnicodeStringResult(const aName: UTF8string; const Args: array of const; Invoke: TJOBInvokeType
  ): UnicodeString;
var
  ResultLen: NativeInt;
  aError: TJOBResult;
begin
  ResultLen:=0;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_stringresult,@ResultLen,Invoke);
  if aError=JOBResult_String then
    Result:=FetchString(ResultLen)
  else begin
    Result:='';
    if aError<>JOBResult_Undefined then
      InvokeJS_RaiseResultMismatch(aName,JOBResult_String,aError);
  end;
  //writeln('TJSObject.InvokeJSUnicodeStringResult Result="',Result,'"');
end;

function TJSObject.InvokeJSObjectResult(const aName: UTF8String; const Args: array of const; aResultClass: TJSObjectClass;
  Invoke: TJOBInvokeType): TJSObject;
var
  aError: TJOBResult;
  NewObjId: TJOBObjectID;
begin
  Result:=nil;
  NewObjId:=-1;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_objectresult,@NewObjId,Invoke);
  if (aError=JOBResult_Null) or (aError=JOBResult_Undefined) then
    exit;
  if aError<>JOBResult_Object then
    InvokeJS_RaiseResultMismatch(aName,JOBResult_Object,aError);

  Result:=aResultClass.JOBCreateFromID(NewObjId);
end;

function TJSObject.InvokeJSValueResult(const aName: UTF8String; const Args: array of const; Invoke: TJOBInvokeType): TJOB_JSValue;
var
  Buf: array[0..7] of byte;
  p: PByte;
  aError: TJOBResult;
  Obj: TJSObject;
begin
  Result:=nil;
  FillByte(Buf[0],length(Buf),0);
  p:=@Buf[0];
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_jsvalueresult,p,Invoke);
  case aError of
  JOBResult_Undefined:
    Result:=TJOB_JSValue.Create(jjvkUndefined);
  JOBResult_Null:
    Result:=TJOB_Object.Create(nil);
  JOBResult_Boolean:
    Result:=TJOB_Boolean.Create(p^<>0);
  JOBResult_Double:
    Result:=TJOB_Double.Create(PDouble(p)^);
  JOBResult_String:
    Result:=TJOB_String.Create(FetchString(PNativeInt(p)^));
  JOBResult_Function,
  JOBResult_Object:
    begin
    Obj:=TJSObject.JOBCreateFromID(PJOBObjectID(p)^);
    Result:=TJOB_Object.Create(Obj);
    end;
  else
    InvokeJS_RaiseResultMismatchStr(aName,'jsvalue',JOBResult_Names[aError]);
  end;
end;

function TJSObject.InvokeJSVariantResult(const aName: UTF8string; const Args: array of const; Invoke: TJOBInvokeType): Variant;
var
  Buf: array[0..7] of byte;
  p: PByte;
  r: TJOBResult;
  Obj: TJSObject;
  func : TJSFunction;
  objid,thisid : TJOBObjectID;
begin
  FillByte(Buf[0],length(Buf),0);
  p:=@Buf[0];
  r:=InvokeJSOneResult(aName,Args,@__job_invoke_jsvalueresult,p,Invoke);
  case r of
  JOBResult_Undefined:
    Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Variants.Unassigned;
  JOBResult_Null:
    Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Variants.Null;
  JOBResult_Boolean:
    Result:=p^<>0;
  JOBResult_Double:
    Result:=PDouble(p)^;
  JOBResult_String:
    Result:=FetchString(PNativeInt(p)^);
  JOBResult_Function:
    begin
    objId:=PJOBObjectID(p)^;
    inc(P,4);
    thisId:=PJOBObjectID(p)^;
    func:=TJSFunction.Create(Objid,ThisId);
    Result:=func as IJSFunction;
    end;
  JOBResult_Object:
    begin
    Obj:=TJSObject.JOBCreateFromID(PJOBObjectID(p)^);
    Result:=Obj as IJSObject;
    end;
  else
    VarClear(Result);
    InvokeJS_RaiseResultMismatchStr(aName,'jsvalue',JOBResult_Names[r]);
  end;
end;

function TJSObject.InvokeJSUtf8StringResult(const aName: UTF8string; const args: array of const; Invoke: TJOBInvokeType
  ): UTF8String;
begin
  Result:=UTF8Encode(InvokeJSUnicodeStringResult(aName,Args,Invoke));
end;

function TJSObject.InvokeJSLongIntResult(const aName: UTF8String; const args: array of const; Invoke: TJOBInvokeType): LongInt;
var
  d: Double;
begin
  d:=InvokeJSDoubleResult(aName,Args,Invoke);
  if (Frac(d)<>0) or (d<low(longint)) or (d>high(longint)) then
    InvokeJS_RaiseResultMismatchStr(aName,'longint','double')
  else
    Result:=Trunc(d);
end;

function TJSObject.InvokeJSMaxIntResult(const aName: UTF8String; const args: array of const; Invoke: TJOBInvokeType): int64;
var
  d: Double;
begin
  d:=InvokeJSDoubleResult(aName,Args,Invoke);
  if (Frac(d)<>0) or (d<low(int64)) or (d>high(int64)) then
    InvokeJS_RaiseResultMismatchStr(aName,'int64','double')
  else
    Result:=Trunc(d);
end;

function TJSObject.InvokeJSTypeOf(const aName: UTF8String; const Args: array of const): TJOBResult;
begin
  Result:=InvokeJSNoResultFunc(aName,Args,@__job_invoke_noresult,jiGetTypeOf);
end;

function TJSObject.InvokeJSUnicodeStringArrayResult(const aName: UTF8String; const Args: array of const; Invoke: TJOBInvokeType
  ): TUnicodeStringDynArray;
var
  ResultP: NativeInt;
  aError: TJOBResult;
begin
  ResultP:=0;
  aError:=InvokeJSOneResult(aName,Args,@__job_invoke_arraystringresult,@ResultP,Invoke);
  if aError=JOBResult_ArrayOfString then
    Result:=TUnicodeStringDynArray(ResultP)
  else begin
    Result:=[];
    if aError<>JOBResult_Undefined then
      InvokeJS_RaiseResultMismatch(aName,JOBResult_ArrayOfString,aError);
  end;
end;

function TJSObject.ReadJSPropertyBoolean(const aName: UTF8String): boolean;
begin
  Result:=InvokeJSBooleanResult(aName,[],jiGet);
end;

function TJSObject.ReadJSPropertyDouble(const aName: UTF8String): double;
begin
  Result:=InvokeJSDoubleResult(aName,[],jiGet);
end;

function TJSObject.ReadJSPropertyUnicodeString(const aName: UTF8String): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult(aName,[],jiGet);
end;

function TJSObject.ReadJSPropertyObject(const aName: UTF8String; aResultClass: TJSObjectClass): TJSObject;
begin
  Result:=InvokeJSObjectResult(aName,[],aResultClass,jiGet);
end;

function TJSObject.ReadJSPropertyUtf8String(const aName: UTF8String): UTF8String;
begin
  Result:=InvokeJSUtf8StringResult(aName,[],jiGet);
end;

function TJSObject.ReadJSPropertyLongInt(const aName: UTF8String): LongInt;
begin
  Result:=InvokeJSLongIntResult(aName,[],jiGet);
end;

function TJSObject.ReadJSPropertyInt64(const aName: UTF8String): Int64;
begin
  Result:=Trunc(InvokeJSDoubleResult(aName,[],jiGet));
end;

function TJSObject.ReadJSPropertyValue(const aName: UTF8String): TJOB_JSValue;
begin
  Result:=InvokeJSValueResult(aName,[],jiGet);
end;

function TJSObject.ReadJSPropertyVariant(const aName: UTF8String): Variant;
begin
  Result:=InvokeJSVariantResult(aName,[],jiGet);
end;

function TJSObject.ReadJSPropertyMethod(const aName: UTF8String): TMethod;
begin
//  Result:=InvokeJSVariantResult(aName,[],jiGet);
end;

procedure TJSObject.WriteJSPropertyBoolean(const aName: UTF8String; Value: Boolean);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyDouble(const aName: UTF8String; Value: Double);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyUnicodeString(const aName: UTF8String; const Value: UnicodeString);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyUtf8String(const aName: UTF8String; const Value: UTF8String);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyObject(const aName: UTF8String; Value: IJSObject);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyLongInt(const aName: UTF8String; Value: LongInt);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyInt64(const aName: UTF8String; Value: Int64);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyValue(const aName: UTF8String; Value: TJOB_JSValue);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyVariant(const aName: UTF8String; const Value: Variant);
begin
  InvokeJSNoResult(aName,[Value],jiSet);
end;

procedure TJSObject.WriteJSPropertyMethod(const aName: UTF8String; const Value: TMethod);
begin
  // TODO InvokeJSNoResult(aName,[Value],jiSet);
end;

function TJSObject.NewJSObject(const Args: array of const;
  aResultClass: TJSObjectClass): TJSObject;
begin
  Result:=InvokeJSObjectResult('',Args,aResultClass,jiNew);
end;

function TJSObject.getOwnPropertyNames(const Obj: IJSObject
  ): TUnicodeStringDynArray;
begin
  Result:=JSObject.InvokeJSUnicodeStringArrayResult('getOwnPropertyNames',[Obj]);
end;

function TJSObject.getPrototypeOf(const Obj: IJSObject): IJSObject;
begin
  Result:=JSObject.InvokeJSObjectResult('getPrototypeOf',[Obj],TJSObject) as IJSObject;
end;

function TJSObject.hasOwnProperty(const PropName: UTF8String): boolean;
begin
  Result:=InvokeJSBooleanResult('hasOwnProperty',[PropName]);
end;

function TJSObject.isPrototypeOf(const Obj: IJSObject): boolean;
begin
  Result:=InvokeJSBooleanResult('isPrototypeOf',[Obj]);
end;

function TJSObject.propertyIsEnumerable(const PropName: UTF8String): boolean;
begin
  Result:=InvokeJSBooleanResult('propertyIsEnumerable',[PropName]);
end;

function TJSObject.toLocaleString: UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('toLocaleString',[]);
end;

function TJSObject.toString: RTLString;
begin
  Result:=InvokeJSUtf8StringResult('toString',[]);
end;

function TJSObject.toUString: UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('toString',[]);
end;

function TJSObject.valueOf: Variant;
begin
  Result:=InvokeJSVariantResult('valueOf',[]);
end;

{ TJSDate }

class function TJSDate.Cast(const Intf: IJSObject): IJSDate;
begin
  Result:=TJSDate.JOBCast(Intf);
end;

function TJSDate.Create(aYear: NativeInt; aMonth: NativeInt;
  aDayOfMonth: NativeInt; TheHours: NativeInt; TheMinutes: NativeInt;
  TheSeconds: NativeInt; TheMilliseconds: NativeInt): IJSDate;
begin
  Result:=JSDate.NewJSObject([aYear,aMonth,aDayOfMonth,TheHours,TheMinutes,TheSeconds,TheMilliseconds],TJSDate) as IJSDate;
end;

function TJSDate.toLocaleDateString: UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('toLocaleDateString',[]);
end;


exports JOBCallback;

initialization
  JSObject:=TJSObject.JOBCreateGlobal('Object') as IJSObject;
  JSDate:=TJSDate.JOBCreateGlobal('Date') as IJSDate;
end.

