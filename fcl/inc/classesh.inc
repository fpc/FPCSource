{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ We NEED ansistrings !!}
{$H+}

type
   { extra types to compile with FPC }

   TRTLCriticalSection = class(TObject);
   HRSRC = longint;
   THANDLE = longint;
   TComponentName = string;
   IUnKnown = class(TObject);
   TGUID = longint;
   HMODULE = longint;

   TPoint = record
      x,y : integer;
   end;

   TSmallPoint = record
      x,y : smallint;
   end;

   TRect = record
      Left,Right,Top,Bottom : Integer;
   end;

const

{ Maximum TList size }

  MaxListSize = Maxint div 16;

{ values for TShortCut }

  scShift = $2000;
  scCtrl = $4000;
  scAlt = $8000;
  scNone = 0;

{ TStream seek origins }

  soFromBeginning = 0;
  soFromCurrent = 1;
  soFromEnd = 2;

{ TFileStream create mode }

  fmCreate        = $FFFF;
  fmOpenRead      = 0;
  fmOpenWrite     = 1;
  fmOpenReadWrite = 2;

{ TParser special tokens }

  toEOF     = Char(0);
  toSymbol  = Char(1);
  toString  = Char(2);
  toInteger = Char(3);
  toFloat   = Char(4);

Const 
  FilerSignature : Array[1..4] of char = 'TPF0';

type

{ Text alignment types }

  TAlignment = (taLeftJustify, taRightJustify, taCenter);

  { TLeftRight = taLeftJustify..taRightJustify; }

{ Types used by standard events }

  TShiftState = set of (ssShift, ssAlt, ssCtrl,
    ssLeft, ssRight, ssMiddle, ssDouble,
    // Extra additions
    ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum, ssScroll);

  { THelpContext = -MaxLongint..MaxLongint; }

{ Standard events }


  TNotifyEvent = procedure(Sender: TObject) of object;
  THelpEvent = function (Command: Word; Data: Longint;
    var CallHelp: Boolean): Boolean of object;
  TGetStrProc = procedure(const S: string) of object;

{ Exception classes }

  EStreamError = class(Exception);
  EFCreateError = class(EStreamError);
  EFOpenError = class(EStreamError);
  EFilerError = class(EStreamError);
  EReadError = class(EFilerError);
  EWriteError = class(EFilerError);
  EClassNotFound = class(EFilerError);
  EMethodNotFound = class(EFilerError);
  EInvalidImage = class(EFilerError);
  EResNotFound = class(Exception);
  EListError = class(Exception);
  EBitsError = class(Exception);
  EStringListError = class(Exception);
  EComponentError = class(Exception);
  EParserError = class(Exception);
  EOutOfResources = class(EOutOfMemory);
  EInvalidOperation = class(Exception);

{ Forward class declarations }

  TStream = class;
  TAbstractFiler = Class;
  TAbstractWriter = Class;
  TAbstractReader = Class;
  
  TFiler = class;
  TReader = class;
  TWriter = class;
  TComponent = class;

{ TList class }

  PPointerList = ^TPointerList;
  TPointerList = array[0..MaxListSize - 1] of Pointer;
  TListSortCompare = function (Item1, Item2: Pointer): Integer;

  TList = class(TObject)
  private
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TList;
    function First: Pointer;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PPointerList read FList;
  end;

{ TThreadList class }

  TThreadList = class
  private
    FList: TList;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; { override; }
    procedure Add(Item: Pointer);
    procedure Clear;
    function  LockList: TList;
    procedure Remove(Item: Pointer);
    procedure UnlockList;
  end;

const
   BITSHIFT = 5;
   MASK = 31; {for longs that are 32-bit in size}
   MaxBitRec = $FFFF Div (SizeOf(longint));
   MaxBitFlags = MaxBitRec * 32;

type
   TBitArray = array[0..MaxBitRec - 1] of longint;

   TBits = class(TObject)
   private
      { Private declarations }
      FBits : ^TBitArray;
      FSize : longint;  { total longints currently allocated }
      findIndex : longint;
      findState : boolean;

      { functions and properties to match TBits class }
      procedure SetBit(bit : longint; value : Boolean);
      function getSize : longint;
      procedure setSize(value : longint);
   public
      { Public declarations }
      constructor Create(theSize : longint); virtual;
      destructor Destroy; override;
      function  getFSize : longint;
      procedure seton(bit : longint);
      procedure clear(bit : longint);
      procedure clearall;
      procedure andbits(bitset : TBits);
      procedure orbits(bitset : TBits);
      procedure xorbits(bitset : TBits);
      procedure notbits(bitset : TBits);
      function  get(bit : longint) : boolean;
      procedure grow(nbit : longint);
      function  equals(bitset : TBits) : Boolean;
      procedure SetIndex(index : longint);
      function  FindFirstBit(state : boolean) : longint;
      function  FindNextBit : longint;
      function  FindPrevBit : longint;

      { functions and properties to match TBits class }
      function OpenBit: longint;
      property Bits[bit: longint]: Boolean read get write SetBit; default;
      property Size: longint read getSize write setSize;
   end;

{ TPersistent abstract class }

{$M+}

  TPersistent = class(TObject)
  private
    procedure AssignError(Source: TPersistent);
  protected
    procedure AssignTo(Dest: TPersistent); virtual;
    procedure DefineProperties(Filer: TFiler); virtual;
    function  GetOwner: TPersistent; dynamic;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); virtual;
    function  GetNamePath: string; virtual; {dynamic;}
  end;

{$M-}

{ TPersistent class reference type }

  TPersistentClass = class of TPersistent;

{ TCollection class }

  TCollection = class;

  TCollectionItem = class(TPersistent)
  private
    FCollection: TCollection;
    FID: Integer;
    function GetIndex: Integer;
    procedure SetCollection(Value: TCollection);
  protected
    procedure Changed(AllItems: Boolean);
    function GetNamePath: string; override;
    function GetOwner: TPersistent; override;
    function GetDisplayName: string; virtual;
    procedure SetIndex(Value: Integer); virtual;
    procedure SetDisplayName(const Value: string); virtual;
  public
    constructor Create(ACollection: TCollection); virtual;
    destructor Destroy; override;
    property Collection: TCollection read FCollection write SetCollection;
    property ID: Integer read FID;
    property Index: Integer read GetIndex write SetIndex;
    property DisplayName: string read GetDisplayName write SetDisplayName;
  end;

  TCollectionItemClass = class of TCollectionItem;

  TCollection = class(TPersistent)
  private
    FItemClass: TCollectionItemClass;
    FItems: TList;
    FUpdateCount: Integer;
    FNextID: Integer;
    FPropName: string;
    function GetCount: Integer;
    function GetPropName: string;
    procedure InsertItem(Item: TCollectionItem);
    procedure RemoveItem(Item: TCollectionItem);
  protected
    { Design-time editor support }
    function GetAttrCount: Integer; dynamic;
    function GetAttr(Index: Integer): string; dynamic;
    function GetItemAttr(Index, ItemIndex: Integer): string; dynamic;
    function GetNamePath: string; override;
    procedure Changed;
    function GetItem(Index: Integer): TCollectionItem;
    procedure SetItem(Index: Integer; Value: TCollectionItem);
    procedure SetItemName(Item: TCollectionItem); virtual;
    procedure SetPropName; virtual;
    procedure Update(Item: TCollectionItem); virtual;
    property PropName: string read GetPropName write FPropName;
  public
    constructor Create(AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add: TCollectionItem;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear;
    procedure EndUpdate;
    function FindItemID(ID: Integer): TCollectionItem;
    property Count: Integer read GetCount;
    property ItemClass: TCollectionItemClass read FItemClass;
    property Items[Index: Integer]: TCollectionItem read GetItem write SetItem;
  end;

  TStrings = class;

{ IStringsAdapter interface }
{ Maintains link between TStrings and IStrings implementations }

  { !!!! Interfaces aren't supported by FPC
  IStringsAdapter = interface
    procedure ReferenceStrings(S: TStrings);
    procedure ReleaseStrings;
  end;
  }
  IStringsAdapter = class(TObject);

{ TStrings class }

  TStrings = class(TPersistent)
  private
    FUpdateCount: Integer;
    FAdapter: IStringsAdapter;
    function GetCommaText: string;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: string);
    procedure SetStringsAdapter(const Value: IStringsAdapter);
    procedure SetValue(const Name, Value: string);
    procedure WriteData(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: string; Data: Integer);
    function Get(Index: Integer): string; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: string; virtual;
    procedure Put(Index: Integer; const S: string); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: string); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
  public
    destructor Destroy; override;
    function Add(const S: string): Integer; virtual;
    function AddObject(const S: string; AObject: TObject): Integer; virtual;
    procedure Append(const S: string);
    procedure AddStrings(TheStrings: TStrings); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(TheStrings: TStrings): Boolean;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetText: PChar; virtual;
    function IndexOf(const S: string): Integer; virtual;
    function IndexOfName(const Name: string): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: string); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: string;
      AObject: TObject);
    procedure LoadFromFile(const FileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(TheText: PChar); virtual;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: string read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: string]: string read GetValue write SetValue;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;
    property StringsAdapter: IStringsAdapter read FAdapter write SetStringsAdapter;
  end;

{ TStringList class }

  TDuplicates = (dupIgnore, dupAccept, dupError);

  PStringItem = ^TStringItem;
  TStringItem = record
    FString: string;
    FObject: TObject;
  end;

  PStringItemList = ^TStringItemList;
  TStringItemList = array[0..MaxListSize] of TStringItem;

  TStringList = class(TStrings)
  private
    FList: PStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer);
    procedure InsertItem(Index: Integer; const S: string);
    procedure SetSorted(Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: string; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Sort; virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

{ TStream abstract class }

  TStream = class(TObject)
  private
    function GetPosition: Longint;
    procedure SetPosition(Pos: Longint);
    function GetSize: Longint;
  protected
    procedure SetSize(NewSize: Longint); virtual;
  public
    function Read(var Buffer; Count: Longint): Longint; virtual; abstract;
    function Write(const Buffer; Count: Longint): Longint; virtual; abstract;
    function Seek(Offset: Longint; Origin: Word): Longint; virtual; abstract;
    procedure ReadBuffer(var Buffer; Count: Longint);
    procedure WriteBuffer(const Buffer; Count: Longint);
    function CopyFrom(Source: TStream; Count: Longint): Longint;
    function ReadComponent(Instance: TComponent): TComponent;
    function ReadComponentRes(Instance: TComponent): TComponent;
    procedure WriteComponent(Instance: TComponent);
    procedure WriteComponentRes(const ResName: string; Instance: TComponent);
    procedure WriteDescendent(Instance, Ancestor: TComponent);
    procedure WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent);
    procedure ReadResHeader;
    function ReadByte : Byte;
    function ReadWord : Word;
    function ReadDWord : Cardinal;
    function ReadAnsiString : String;
    procedure WriteByte(b : Byte);
    procedure WriteWord(w : Word);
    procedure WriteDWord(d : Cardinal);
    Procedure WriteAnsiString (S : String);
    property Position: Longint read GetPosition write SetPosition;
    property Size: Longint read GetSize write SetSize;
  end;

{ THandleStream class }

  THandleStream = class(TStream)
  private
    FHandle: Integer;
  public
    constructor Create(AHandle: Integer);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property Handle: Integer read FHandle;
  end;

{ TFileStream class }

  TFileStream = class(THandleStream)
  Private
    FFileName : String;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const AFileName: string; Mode: Word);
    destructor Destroy; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property FileName : String Read FFilename;
  end;

{ TCustomMemoryStream abstract class }

  TCustomMemoryStream = class(TStream)
  private
    FMemory: Pointer;
    FSize, FPosition: Longint;
  protected
    procedure SetPointer(Ptr: Pointer; ASize: Longint);
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    property Memory: Pointer read FMemory;
  end;

{ TMemoryStream }

  TMemoryStream = class(TCustomMemoryStream)
  private
    FCapacity: Longint;
    procedure SetCapacity(NewCapacity: Longint);
  protected
    function Realloc(var NewCapacity: Longint): Pointer; virtual;
    property Capacity: Longint read FCapacity write SetCapacity;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

{ TStringStream }

  TStringStream = class(TStream)
  private
    FDataString: string;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const AString: string);
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadString(Count: Longint): string;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure WriteString(const AString: string);
    property DataString: string read FDataString;
  end;

{ TResourceStream }

  TResourceStream = class(TCustomMemoryStream)
  private
    HResInfo: HRSRC;
    HGlobal: THandle;
    procedure Initialize(Instance: THandle; Name, ResType: PChar);
  public
    constructor Create(Instance: THandle; const ResName: string; ResType: PChar);
    constructor CreateFromID(Instance: THandle; ResID: Integer; ResType: PChar);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

{ TStreamAdapter }
{ Implements OLE IStream on VCL TStream }
{ we don't need that yet
  TStreamAdapter = class(TInterfacedObject, IStream)
  private
    FStream: TStream;
  public
    constructor Create(Stream: TStream);
    function Read(pv: Pointer; cb: Longint;
      pcbRead: PLongint): HResult; stdcall;
    function Write(pv: Pointer; cb: Longint;
      pcbWritten: PLongint): HResult; stdcall;
    function Seek(dlibMove: Largeint; dwOrigin: Longint;
      out libNewPosition: Largeint): HResult; stdcall;
    function SetSize(libNewSize: Largeint): HResult; stdcall;
    function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
      out cbWritten: Largeint): HResult; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function UnlockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function Stat(out statstg: TStatStg;
      grfStatFlag: Longint): HResult; stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
  end;
}
{ TFiler }

  TValueType = (vaNull, vaList, vaInt8, vaInt16, vaInt32, vaExtended,
    vaString, vaIdent, vaFalse, vaTrue, vaBinary, vaSet, vaLString,
    vaNil, vaCollection);

  TFilerFlag = (ffInherited, ffChildPos);
  TFilerFlags = set of TFilerFlag;

(*
  TReaderProc = procedure(Reader: TReader) of object;
  TWriterProc = procedure(Writer: TWriter) of object;
  TStreamProc = procedure(Stream: TStream) of object;
*)

  TReaderProc = procedure(Reader: TAbstractReader) of object;
  TWriterProc = procedure(Writer: TAbstractWriter) of object;
  TStreamProc = procedure(Stream: TStream) of object;

  TAbstractFiler = class(TObject)
  private
    FRoot: TComponent;
    FAncestor: TPersistent;
     FIgnoreChildren: Boolean;
     FPrefix : String;
   public
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); virtual; abstract;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); virtual; abstract;
    property Root: TComponent read FRoot write FRoot;
    property Ancestor: TPersistent read FAncestor write FAncestor;
    property IgnoreChildren: Boolean read FIgnoreChildren write FIgnoreChildren;
  end;

{ TReader }

  TFindMethodEvent = procedure(Reader: TReader; const MethodName: string;
    var Address: Pointer; var Error: Boolean) of object;
  TSetNameEvent = procedure(Reader: TReader; Component: TComponent;
    var Name: string) of object;
  TReferenceNameEvent = procedure(Reader: TReader; var Name: string) of object;
  TAncestorNotFoundEvent = procedure(Reader: TReader; const ComponentName: string;
    ComponentClass: TPersistentClass; var Component: TComponent) of object;
  TReadComponentsProc = procedure(Component: TComponent) of object;
  TReaderError = procedure(Reader: TReader; const Message: string; var Handled: Boolean) of object;


  TAbstractReader = class(TAbstractFiler);
(*  private
  protected
    function Error(const Message: string): Boolean; virtual;
    function FindMethod(ARoot: TComponent; const AMethodName: string): Pointer; virtual;
    procedure SetName(Component: TComponent; var Name: string); virtual;
    procedure ReferenceName(var Name: string); virtual;
    function FindAncestorComponent(const Name: string;
                                   ComponentClass: TPersistentClass): TComponent; virtual;
  public
    destructor Destroy; override;
    procedure BeginReferences;
    procedure DefineProperty(const Name: string;
      rd : TReaderProc; wd : TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      rd, wd: TStreamProc;
      HasData: Boolean); override;
    function EndOfList: Boolean;
    procedure EndReferences;
    procedure FixupReferences;
    procedure FlushBuffer; override;
    function NextValue: TValueType;
    procedure ReadPrefix(var Flags: TFilerFlags; var AChildPos: Integer);
    procedure ReadCollection(Collection: TCollection);
    function ReadComponent(Component: TComponent): TComponent;
    procedure ReadComponents(AOwner, AParent: TComponent;
      Proc: TReadComponentsProc);
    function ReadRootComponent(ARoot: TComponent): TComponent;
    { Abstract methods }
    procedure ReadSignature;
    function ReadBoolean: Boolean; abstract;
    function ReadChar: Char; abstract;
    function ReadFloat: Extended;
    function ReadIdent: string;
    function ReadInteger: Longint;
    procedure ReadListBegin;
    procedure ReadListEnd;
    function ReadStr: string;
    function ReadString: string;
    function ReadValue: TValueType;
    procedure CopyValue(Writer: TWriter); 
    {!!!}
    property Owner: TComponent read FOwner write FOwner;
    property Parent: TComponent read FParent write FParent;
    property Position: Longint read GetPosition write SetPosition;
    property OnError: TReaderError read FOnError write FOnError;
    property OnFindMethod: TFindMethodEvent read FOnFindMethod write FOnFindMethod;
    property OnSetName: TSetNameEvent read FOnSetName write FOnSetName;
    property OnReferenceName: TReferenceNameEvent read FOnReferenceName write FOnReferenceName;
    property OnAncestorNotFound: TAncestorNotFoundEvent read FOnAncestorNotFound write FOnAncestorNotFound;
  end;
*)

{ TAbstractWriter }

  TAbstractWriter = class(TAbstractFiler)
  private
    FRootAncestor: TComponent;
    FPropPath: string;
    FAncestorList: TList;
    FAncestorPos: Integer;
    FChildPos: Integer;
    procedure AddAncestor(Component: TComponent);
    procedure WriteData(Instance: TComponent); // linker optimization
    procedure WriteProperty(Instance: TPersistent; PropInfo: Pointer);
    procedure WriteProperties(Instance: TPersistent);
    Procedure DoOrdinalProp(Instance : TPersistent;Propinfo :PPropInfo);
    Procedure DoStringProp(Instance : TPersistent;Propinfo :PPropInfo);
    Procedure DoFloatProp(Instance : TPersistent;Propinfo :PPropInfo);
    Procedure DoCollectionProp(Name: ShortString; Value : TCollection);
    Procedure DoClassProp(Instance : TPersistent;Propinfo :PPropInfo);
    Procedure DoMethodProp(Instance : TPersistent;Propinfo :PPropInfo);
  protected
    procedure WritePrefix(Flags: TFilerFlags; AChildPos: Integer);Virtual;Abstract;
    Procedure StartObject(Const AClassName,AName : String);Virtual;abstract;
    Procedure EndObject;Virtual;Abstract;
    Procedure StartCollection(Const Name : String);Virtual;abstract;
    Procedure EndCollection;Virtual;Abstract;
    Procedure StartCollectionItem;Virtual;abstract;
    Procedure EndCollectionItem;Virtual;Abstract;
  public
    destructor Destroy; override;
    procedure DefineProperty(const Name: string;
      rd : TReaderProc; wd : TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      rd, wd: TStreamProc;
      HasData: Boolean); override;
    procedure WriteDescendent(ARoot: TComponent; AAncestor: TComponent);
    procedure WriteRootComponent(ARoot: TComponent);
    procedure WriteComponent(Component: TComponent);virtual;
    { Abstract }
    Procedure WriteIntegerProperty(Const Name : Shortstring;Value : Longint);virtual;abstract;
    Procedure WriteSetProperty (Const Name : ShortString;Value : longint; BaseType : TTypeInfo);virtual;abstract;
    Procedure WriteEnumerationProperty (Const Name : ShortString;Value : Longint; Const EnumName : ShortSTring);virtual;abstract;
    Procedure WriteStringProperty(Const Name : ShortString; Const Value : String);virtual;abstract;
    Procedure WriteFloatProperty(Const Name : ShortString; Value : Extended);virtual;abstract;
    Procedure WriteCollectionProperty(Const Name : ShortString;Value : TCollection);virtual;abstract;
    Procedure WriteClassProperty(Instance : TPersistent;Propinfo :PPropInfo);virtual;abstract;
    Procedure WriteComponentProperty(Const Name : ShortString;Value : TComponent);virtual;abstract;
    Procedure WriteNilProperty(Const Name : Shortstring);virtual; abstract;
    Procedure WriteMethodProperty(Const Name,AMethodName : ShortString);virtual;abstract;
    Procedure WriteBinaryProperty(Const Name; Value : TStream);Virtual;Abstract;
(*
    { Abstract compatibility methods}
    Procedure WriteValue(Value : TValueType);virtual;abstract;
    procedure Write(const Buf; Count: Longint);virtual;abstract;
    procedure WriteBoolean(Value: Boolean);virtual;abstract;
    procedure WriteCollection(Value: TCollection);virtual;abstract;
    procedure WriteComponent(Component: TComponent);virtual;abstract;
    procedure WriteChar(Value: Char);virtual;abstract;
    procedure WriteFloat(Value: Extended);virtual;abstract;
    procedure WriteIdent(const Ident: string);virtual;abstract;
    procedure WriteInteger(Value: Longint);virtual;abstract;
    procedure WriteListBegin;virtual;abstract;
    procedure WriteListEnd;virtual;abstract;
    procedure WriteSignature;virtual;abstract;
    procedure WriteStr(const Value: string);virtual;abstract;
    procedure WriteString(const Value: string);virtual;abstract;
*)
    property RootAncestor: TComponent read FRootAncestor write FRootAncestor;
  end;

  TWriter = class(TAbstractWriter)
  Private 
    FStream : TStream;
    function GetPosition: Longint;
    procedure SetPosition(Value: Longint);
    procedure WritePropName(const PropName: string);
  protected
    procedure WriteBinary(wd : TStreamProc);
  public
    Constructor Create(S : TStream);
    destructor Destroy; override;
    { Compatibility }
    procedure WriteBuffer;
    Procedure FlushBuffer;
    { Abstract }
    Procedure WriteIntegerProperty(Const Name : Shortstring;Value : Longint);override;
    Procedure WriteSetProperty (Const Name : ShortString;Value : longint; BaseType : TTypeInfo);override;
    Procedure WriteEnumerationProperty (Const Name : ShortString;Value : Longint; Const EnumName : ShortSTring);override;
    Procedure WriteStringProperty(Const Name : ShortString; Const Value : String);override;
    Procedure WriteFloatProperty(Const Name : ShortString; Value : Extended);override;
    Procedure WriteCollectionProperty(Const Name : ShortString;Value : TCollection);override;
    Procedure WriteClassProperty(Instance : TPersistent;Propinfo :PPropInfo);override;
    Procedure WriteComponentProperty(Const Name : ShortSTring; Value : TComponent);override;
    Procedure WriteNilProperty(Const Name : Shortstring);override;
    Procedure WriteMethodProperty(Const Name,AMethodName : ShortString);override;
    { Abstract compatibility methods}
    procedure WritePrefix(Flags: TFilerFlags; AChildPos: Integer); virtual;
    Procedure WriteValue(Value : TValueType);virtual;
    procedure Write(const Buf; Count: Longint);virtual;
    procedure WriteBoolean(Value: Boolean);virtual;
    procedure WriteCollection(Value: TCollection);virtual;
    procedure WriteChar(Value: Char);virtual;
    procedure WriteFloat(Value: Extended);virtual;
    procedure WriteIdent(const Ident: string);virtual;
    procedure WriteInteger(Value: Longint);virtual;
    procedure WriteListBegin;virtual;
    procedure WriteListEnd;virtual;
    procedure WriteSignature;virtual;
    procedure WriteStr(const Value: string);virtual;
    procedure WriteString(const Value: string);virtual;
    procedure DefineProperty(const Name: string;
      rd : TReaderProc; wd : TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      rd, wd: TStreamProc;
      HasData: Boolean); override;
    property Position: Longint read GetPosition write SetPosition;
  end;

  TTextWriter = class(TAbstractWriter)
  Private 
    FStream : TStream;
    Procedure Write(Const Msg : String);
    Procedure WriteLn(Const Msg : String);
    Procedure WriteFmt(Fmt : String; Args :  Array of const);
    procedure WritePropName(const PropName: string);
  protected
   Procedure StartCollection(Const AName : String);
   Procedure StartCollectionItem;
   Procedure EndCollectionItem;  
   Procedure EndCollection;
  public
    Constructor Create(S : TStream);
    destructor Destroy; override;
    { Abstract }
    Procedure StartObject(Const AClassName,AName : String);override;
    Procedure EndObject;Virtual;override;
    Procedure WriteIntegerProperty(Const Name : Shortstring;Value : Longint);override;
    Procedure WriteSetProperty (Const Name : ShortString;Value : longint; BaseType : TTypeInfo);override;
    Procedure WriteEnumerationProperty (Const Name : ShortString;Value : Longint; Const EnumName : ShortSTring);override;
    Procedure WriteStringProperty(Const Name : ShortString; Const Value : String);override;
    Procedure WriteFloatProperty(Const Name : ShortString; Value : Extended);override;
    Procedure WriteCollectionProperty(Const Name : ShortString;Value : TCollection);override;
    Procedure WriteClassProperty(Instance : TPersistent;Propinfo :PPropInfo);override;
    Procedure WriteComponentProperty(Const Name : ShortSTring; Value : TComponent);override;
    Procedure WriteNilProperty(Const Name : Shortstring);override;
    Procedure WriteMethodProperty(Const Name,AMethodName : ShortString);override;
  end;

  TFiler = Class(TAbstractFiler);
  TReader = Class(TWriter);

(*
  TFiler = class(TObject)
  private
    FStream: TStream;
    FBuffer: Pointer;
    FBufSize: Integer;
    FBufPos: Integer;
    FBufEnd: Integer;
    FRoot: TComponent;
    FAncestor: TPersistent;
    FIgnoreChildren: Boolean;
  public
    constructor Create(Stream: TStream; BufSize: Integer);
    destructor Destroy; override;
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); virtual; abstract;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); virtual; abstract;
    procedure FlushBuffer; virtual; abstract;
    property Root: TComponent read FRoot write FRoot;
    property Ancestor: TPersistent read FAncestor write FAncestor;
    property IgnoreChildren: Boolean read FIgnoreChildren write FIgnoreChildren;
  end;

{ TReader }

  TFindMethodEvent = procedure(Reader: TReader; const MethodName: string;
    var Address: Pointer; var Error: Boolean) of object;
  TSetNameEvent = procedure(Reader: TReader; Component: TComponent;
    var Name: string) of object;
  TReferenceNameEvent = procedure(Reader: TReader; var Name: string) of object;
  TAncestorNotFoundEvent = procedure(Reader: TReader; const ComponentName: string;
    ComponentClass: TPersistentClass; var Component: TComponent) of object;
  TReadComponentsProc = procedure(Component: TComponent) of object;
  TReaderError = procedure(Reader: TReader; const Message: string; var Handled: Boolean) of object;

  TReader = class(TFiler)
  private
    FOwner: TComponent;
    FParent: TComponent;
    FFixups: TList;
    FLoaded: TList;
    FOnFindMethod: TFindMethodEvent;
    FOnSetName: TSetNameEvent;
    FOnReferenceName: TReferenceNameEvent;
    FOnAncestorNotFound: TAncestorNotFoundEvent;
    FOnError: TReaderError;
    FCanHandleExcepts: Boolean;
    FPropName: string;
    procedure CheckValue(Value: TValueType);
    procedure DoFixupReferences;
    procedure FreeFixups;
    function GetPosition: Longint;
    procedure PropertyError;
    procedure ReadBuffer;
    procedure ReadData(Instance: TComponent);
    procedure ReadDataInner(Instance: TComponent);
    procedure ReadProperty(AInstance: TPersistent);
    procedure ReadPropValue(Instance: TPersistent; PropInfo: Pointer);
    function ReadSet(SetType: Pointer): Integer;
    procedure SetPosition(Value: Longint);
    procedure SkipSetBody;
    procedure SkipValue;
    procedure SkipProperty;
    procedure SkipComponent(SkipHeader: Boolean);
  protected
    function Error(const Message: string): Boolean; virtual;
    function FindMethod(ARoot: TComponent; const AMethodName: string): Pointer; virtual;
    procedure SetName(Component: TComponent; var Name: string); virtual;
    procedure ReferenceName(var Name: string); virtual;
    function FindAncestorComponent(const Name: string;
      ComponentClass: TPersistentClass): TComponent; virtual;
  public
    destructor Destroy; override;
    procedure BeginReferences;
    procedure DefineProperty(const Name: string;
      rd : TReaderProc; wd : TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      rd, wd: TStreamProc;
      HasData: Boolean); override;
    function EndOfList: Boolean;
    procedure EndReferences;
    procedure FixupReferences;
    procedure FlushBuffer; override;
    function NextValue: TValueType;
    procedure Read(var Buf; Count: Longint);
    function ReadBoolean: Boolean;
    function ReadChar: Char;
    procedure ReadCollection(Collection: TCollection);
    function ReadComponent(Component: TComponent): TComponent;
    procedure ReadComponents(AOwner, AParent: TComponent;
      Proc: TReadComponentsProc);
    function ReadFloat: Extended;
    function ReadIdent: string;
    function ReadInteger: Longint;
    procedure ReadListBegin;
    procedure ReadListEnd;
    procedure ReadPrefix(var Flags: TFilerFlags; var AChildPos: Integer);
    function ReadRootComponent(ARoot: TComponent): TComponent;
    procedure ReadSignature;
    function ReadStr: string;
    function ReadString: string;
    function ReadValue: TValueType;
    procedure CopyValue(Writer: TWriter); {!!!}
    property Owner: TComponent read FOwner write FOwner;
    property Parent: TComponent read FParent write FParent;
    property Position: Longint read GetPosition write SetPosition;
    property OnError: TReaderError read FOnError write FOnError;
    property OnFindMethod: TFindMethodEvent read FOnFindMethod write FOnFindMethod;
    property OnSetName: TSetNameEvent read FOnSetName write FOnSetName;
    property OnReferenceName: TReferenceNameEvent read FOnReferenceName write FOnReferenceName;
    property OnAncestorNotFound: TAncestorNotFoundEvent read FOnAncestorNotFound write FOnAncestorNotFound;
  end;

{ TWriter }

  TWriter = class(TFiler)
  private
    FRootAncestor: TComponent;
    FPropPath: string;
    FAncestorList: TList;
    FAncestorPos: Integer;
    FChildPos: Integer;
    procedure AddAncestor(Component: TComponent);
    function GetPosition: Longint;
    procedure SetPosition(Value: Longint);
    procedure WriteBuffer;
    procedure WriteData(Instance: TComponent); 
    procedure WriteProperty(Instance: TPersistent; PropInfo: Pointer);
    procedure WriteProperties(Instance: TPersistent);
    procedure WritePropName(const PropName: string);
  protected
    procedure WriteBinary(wd : TStreamProc);
    procedure WritePrefix(Flags: TFilerFlags; AChildPos: Integer);
    procedure WriteValue(Value: TValueType);
  public
    destructor Destroy; override;
    procedure DefineProperty(const Name: string;
      rd : TReaderProc; wd : TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      rd, wd: TStreamProc;
      HasData: Boolean); override;
    procedure FlushBuffer; override;
    procedure Write(const Buf; Count: Longint);
    procedure WriteBoolean(Value: Boolean);
    procedure WriteCollection(Value: TCollection);
    procedure WriteComponent(Component: TComponent);
    procedure WriteChar(Value: Char);
    procedure WriteDescendent(ARoot: TComponent; AAncestor: TComponent);
    procedure WriteFloat(Value: Extended);
    procedure WriteIdent(const Ident: string);
    procedure WriteInteger(Value: Longint);
    procedure WriteListBegin;
    procedure WriteListEnd;
    procedure WriteRootComponent(ARoot: TComponent);
    procedure WriteSignature;
    procedure WriteStr(const Value: string);
    procedure WriteString(const Value: string);
    property Position: Longint read GetPosition write SetPosition;
    property RootAncestor: TComponent read FRootAncestor write FRootAncestor;
  end;
*)

{ TParser }

  TParser = class(TObject)
  private
    FStream: TStream;
    FOrigin: Longint;
    FBuffer: PChar;
    FBufPtr: PChar;
    FBufEnd: PChar;
    FSourcePtr: PChar;
    FSourceEnd: PChar;
    FTokenPtr: PChar;
    FStringPtr: PChar;
    FSourceLine: Integer;
    FSaveChar: Char;
    FToken: Char;
    procedure ReadBuffer;
    procedure SkipBlanks;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure CheckToken(T: Char);
    procedure CheckTokenSymbol(const S: string);
    procedure Error(const Ident: string);
    procedure ErrorFmt(const Ident: string; const Args: array of const);
    procedure ErrorStr(const Message: string);
    procedure HexToBinary(Stream: TStream);
    function NextToken: Char;
    function SourcePos: Longint;
    function TokenComponentIdent: String;
    function TokenFloat: Extended;
    function TokenInt: Longint;
    function TokenString: string;
    function TokenSymbolIs(const S: string): Boolean;
    property SourceLine: Integer read FSourceLine;
    property Token: Char read FToken;
  end;

{ TThread }

  EThread = class(Exception);

  TThreadMethod = procedure of object;
  TThreadPriority = (tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest,
    tpTimeCritical);

  TThread = class
  private
    FHandle: THandle;
    FThreadID: THandle;
    FTerminated: Boolean;
    FSuspended: Boolean;
    FFreeOnTerminate: Boolean;
    FFinished: Boolean;
    FReturnValue: Integer;
    FOnTerminate: TNotifyEvent;
    FMethod: TThreadMethod;
    FSynchronizeException: TObject;
    procedure CallOnTerminate;
    function GetPriority: TThreadPriority;
    procedure SetPriority(Value: TThreadPriority);
    procedure SetSuspended(Value: Boolean);
  protected
    procedure DoTerminate; virtual;
    procedure Execute; virtual; abstract;
    procedure Synchronize(Method: TThreadMethod);
    property ReturnValue: Integer read FReturnValue write FReturnValue;
    property Terminated: Boolean read FTerminated;
  public
{$ifdef linux}
    { Needed for linux }
    FStackPointer : integer;
    FStackSize    : integer;
    FCallExitProcess : boolean;
{$endif}
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; { override; }
    procedure Resume;
    procedure Suspend;
    procedure Terminate;
    function WaitFor: Integer;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property Handle: THandle read FHandle;
    property Priority: TThreadPriority read GetPriority write SetPriority;
    property Suspended: Boolean read FSuspended write SetSuspended;
    property ThreadID: THandle read FThreadID;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

{ TComponent class }

  TOperation = (opInsert, opRemove);
  TComponentState = set of (csLoading, csReading, csWriting, csDestroying,
    csDesigning, csAncestor, csUpdating, csFixups);
  TComponentStyle = set of (csInheritable, csCheckPropAvail);
  TGetChildProc = procedure (Child: TComponent) of object;

  {
  TComponentName = type string;

  IVCLComObject = interface
    function GetTypeInfoCount(out Count: Integer): Integer; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): Integer; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): Integer; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): Integer; stdcall;
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): Integer;
    procedure FreeOnRelease;
  end;
  }

  TComponent = class(TPersistent)
  private
    FOwner: TComponent;
    FName: TComponentName;
    FTag: Longint;
    FComponents: TList;
    FFreeNotifies: TList;
    FDesignInfo: Longint;
    FVCLComObject: Pointer;
    FComponentState: TComponentState;
    // function GetComObject: IUnknown;
    function GetComponent(AIndex: Integer): TComponent;
    function GetComponentCount: Integer;
    function GetComponentIndex: Integer;
    procedure Insert(AComponent: TComponent);
    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure Remove(AComponent: TComponent);
    procedure SetComponentIndex(Value: Integer);
    procedure SetReference(Enable: Boolean);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);
  protected
    FComponentStyle: TComponentStyle;
    procedure ChangeName(const NewName: TComponentName);
    procedure DefineProperties(Filer: TFiler); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); dynamic;
    function GetChildOwner: TComponent; dynamic;
    function GetChildParent: TComponent; dynamic;
    function GetNamePath: string; override;
    function GetOwner: TPersistent; override;
    procedure Loaded; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); virtual;
    procedure ReadState(Reader: TAbstractReader); virtual;
    procedure SetAncestor(Value: Boolean);
    procedure SetDesigning(Value: Boolean);
    procedure SetName(const NewName: TComponentName); virtual;
    procedure SetChildOrder(Child: TComponent; Order: Integer); dynamic;
    procedure SetParentComponent(Value: TComponent); dynamic;
    procedure Updating; dynamic;
    procedure Updated; dynamic;
    class procedure UpdateRegistry(Register: Boolean; const ClassID, ProgID: string); dynamic;
    procedure ValidateRename(AComponent: TComponent;
      const CurName, NewName: string); virtual;
    procedure ValidateContainer(AComponent: TComponent); dynamic;
    procedure ValidateInsert(AComponent: TComponent); dynamic;
    { IUnknown }
    //!!!!! function QueryInterface(const IID: TGUID; out Obj): Integer; stdcall;
    //!!!! function _AddRef: Integer; stdcall;
    //!!!! function _Release: Integer; stdcall;
    { IDispatch }
    //!!!! function GetTypeInfoCount(out Count: Integer): Integer; stdcall;
    //!!!! function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): Integer; stdcall;
    //!!!! function GetIDsOfNames(const IID: TGUID; Names: Pointer;
    //!!!!   NameCount, LocaleID: Integer; DispIDs: Pointer): Integer; stdcall;
    //!!!! function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
    //!!!!   Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): Integer; stdcall;
  public
    //!! Moved temporary
    procedure WriteState(Writer: TAbstractWriter); virtual;
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure DestroyComponents;
    procedure Destroying;
    function FindComponent(const AName: string): TComponent;
    procedure FreeNotification(AComponent: TComponent);
    procedure FreeOnRelease;
    function GetParentComponent: TComponent; dynamic;
    function HasParent: Boolean; dynamic;
    procedure InsertComponent(AComponent: TComponent);
    procedure RemoveComponent(AComponent: TComponent);
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): Integer; override;
    // property ComObject: IUnknown read GetComObject;
    property Components[Index: Integer]: TComponent read GetComponent;
    property ComponentCount: Integer read GetComponentCount;
    property ComponentIndex: Integer read GetComponentIndex write SetComponentIndex;
    property ComponentState: TComponentState read FComponentState;
    property ComponentStyle: TComponentStyle read FComponentStyle;
    property DesignInfo: Longint read FDesignInfo write FDesignInfo;
    property Owner: TComponent read FOwner;
    property VCLComObject: Pointer read FVCLComObject write FVCLComObject;
//!!  published
    property Name: TComponentName read FName write SetName ; // stored False;
    property Tag: Longint read FTag write FTag ; // default 0;
  end;

{ TComponent class reference type }

  TComponentClass = class of TComponent;

{ Component registration handlers }

  TActiveXRegType = (axrComponentOnly, axrIncludeDescendants);

var
  RegisterComponentsProc: procedure(const Page: string;
    ComponentClasses: array of TComponentClass);
  RegisterNoIconProc: procedure(ComponentClasses: array of TComponentClass);
{!!!!  RegisterNonActiveXProc: procedure(ComponentClasses: array of TComponentClass;
    AxRegType: TActiveXRegType) = nil;
  CurrentGroup: Integer = -1;
  CreateVCLComObjectProc: procedure(Component: TComponent) = nil;}

{ Point and rectangle constructors }

function Point(AX, AY: Integer): TPoint;
function SmallPoint(AX, AY: SmallInt): TSmallPoint;
function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect;
function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;

{ Class registration routines }

procedure RegisterClass(AClass: TPersistentClass);
procedure RegisterClasses(AClasses: array of TPersistentClass);
procedure RegisterClassAlias(AClass: TPersistentClass; const Alias: string);
procedure UnRegisterClass(AClass: TPersistentClass);
procedure UnRegisterClasses(AClasses: array of TPersistentClass);
procedure UnRegisterModuleClasses(Module: HMODULE);
function FindClass(const ClassName: string): TPersistentClass;
function GetClass(const ClassName: string): TPersistentClass;

{ Component registration routines }

procedure RegisterComponents(const Page: string;
  ComponentClasses: array of TComponentClass);
procedure RegisterNoIcon(ComponentClasses: array of TComponentClass);
procedure RegisterNonActiveX(ComponentClasses: array of TComponentClass;
  AxRegType: TActiveXRegType);


{ Object filing routines }

type
  TIdentMapEntry = record
    Value: Integer;
    Name: String;
  end;

  TIdentToInt = function(const Ident: string; var Int: Longint): Boolean;
  TIntToIdent = function(Int: Longint; var Ident: string): Boolean;
  TFindGlobalComponent = function(const Name: string): TComponent;

var
  MainThreadID: THandle;
  FindGlobalComponent: TFindGlobalComponent;

procedure RegisterIntegerConsts(IntegerType: Pointer; IdentToInt: TIdentToInt;
  IntToIdent: TIntToIdent);
function IdentToInt(const Ident: string; var Int: Longint; const Map: array of TIdentMapEntry): Boolean;
function IntToIdent(Int: Longint; var Ident: string; const Map: array of TIdentMapEntry): Boolean;

function InitInheritedComponent(Instance: TComponent; RootAncestor: TClass): Boolean;
function InitComponentRes(const ResName: string; Instance: TComponent): Boolean;
function ReadComponentRes(const ResName: string; Instance: TComponent): TComponent;
function ReadComponentResEx(HInstance: THandle; const ResName: string): TComponent;
function ReadComponentResFile(const FileName: string; Instance: TComponent): TComponent;
procedure WriteComponentResFile(const FileName: string; Instance: TComponent);

procedure GlobalFixupReferences;
procedure GetFixupReferenceNames(Root: TComponent; Names: TStrings);
procedure GetFixupInstanceNames(Root: TComponent;
  const ReferenceRootName: string; Names: TStrings);
procedure RedirectFixupReferences(Root: TComponent; const OldRootName,
  NewRootName: string);
procedure RemoveFixupReferences(Root: TComponent; const RootName: string);
procedure RemoveFixups(Instance: TPersistent);

procedure BeginGlobalLoading;
procedure NotifyGlobalLoading;
procedure EndGlobalLoading;

function CollectionsEqual(C1, C2: TCollection): Boolean;

{ Object conversion routines }

procedure ObjectBinaryToText(Input, Output: TStream);
procedure ObjectTextToBinary(Input, Output: TStream);

procedure ObjectResourceToText(Input, Output: TStream);
procedure ObjectTextToResource(Input, Output: TStream);

{ Utility routines }

function LineStart(Buffer, BufPos: PChar): PChar;

{
  $Log$
  Revision 1.20  2000-01-06 01:20:32  peter
    * moved out of packages/ back to topdir

  Revision 1.3  2000/01/05 11:05:29  michael
  + Better collection support

  Revision 1.2  2000/01/04 18:07:16  michael
  + Streaming implemented

  Revision 1.1  2000/01/03 19:33:07  peter
    * moved to packages dir

  Revision 1.18  1999/11/30 15:28:38  michael
  + Added FileNAme property for filestreams

  Revision 1.17  1999/10/20 20:24:21  florian
    + sc* constants added as suggested by Shane Miller

  Revision 1.16  1999/09/13 08:35:16  fcl
  * Changed some argument names (Root->ARoot etc.) because the new compiler
    now performs more ambiguity checks  (sg)

  Revision 1.15  1999/09/11 22:01:03  fcl
  * Activated component registration callbacks  (sg)

  Revision 1.14  1999/08/26 21:11:25  peter
    * ShiftState extended

  Revision 1.13  1999/05/31 12:43:10  peter
    * fixed tthread for linux additions

  Revision 1.12  1999/05/14 17:52:53  peter
    * removed wrong destroy overrides (gave errors with the new compiler)

  Revision 1.11  1999/04/09 12:13:30  michael
  + Changed TBits to TbitsPlus from Michael A. Hess (renamed to Tbits)

  Revision 1.10  1998/10/30 14:52:49  michael
  + Added format in interface
  + Some errors in parser fixed, it uses exceptions now
  + Strings now has no more syntax errors.

  Revision 1.9  1998/10/24 13:45:35  michael
  + Implemented stringlist. Untested, since classes broken.

  Revision 1.8  1998/09/23 07:47:41  michael
  + Some changes by TSE

  Revision 1.7  1998/08/22 10:41:00  michael
  + Some adaptations for changed comment and published handling

  Revision 1.6  1998/06/11 13:46:32  michael
  + Fixed some functions. TFileStream OK.

  Revision 1.5  1998/06/10 21:53:06  michael
  + Implemented Handle/FileStreams

  Revision 1.4  1998/05/27 11:41:43  michael
  Implemented TCollection and TCollectionItem

  Revision 1.3  1998/05/06 12:58:35  michael
  + Added WriteAnsiString method to TStream

  Revision 1.2  1998/05/04 14:30:11  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

  Revision 1.1  1998/05/04 12:16:01  florian
    + Initial revisions after making a new directory structure

}
