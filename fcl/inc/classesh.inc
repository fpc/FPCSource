{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ We NEED ansistrings !!}
{$H+}

{ The 1.0 compiler cannot compile the Seek(int64) overloading correct }
{$ifndef ver1_0}
  {$define seek64bit}
{$endif ver1_0}

type
   { extra types to compile with FPC }
   HRSRC = longint;
   THANDLE = longint;
   TComponentName = string;
{$ifdef ver1_0}
   // 1.0 doesn't have thread
   TRTLCriticalSection = record
     locked : longint;
   end;
   // 1.1 and above has interfaces
   IUnknown = class(TObject);
   TGUID = longint;
{$endif ver1_0}
   HMODULE = longint;

   TPoint = record
      x,y : integer;
   end;
   TRect = record
      Case Integer of
       0 : ( Left,Top,Right,Bottom : integer);
       1 : ( TopLeft,BottomRight : TPoint);
   end;

   TSmallPoint = record
      x,y : smallint;
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
const
  soFromBeginning = 0;
  soFromCurrent = 1;
  soFromEnd = 2;

type
  TSeekOrigin = (soBeginning, soCurrent, soEnd);

{ TFileStream create mode }
const
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
    ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum, 
    ssScroll,ssTriple,ssQuad);

  THelpContext = -MaxLongint..MaxLongint;
  THelpType = (htKeyword, htContext);

  TShortCut = Low(Word)..High(Word);

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
  TFiler = class;
  TReader = class;
  TWriter = class;
  TComponent = class;

{ TList class }

  PPointerList = ^TPointerList;
  TPointerList = array[0..MaxListSize - 1] of Pointer;
  TListSortCompare = function (Item1, Item2: Pointer): Integer;
  TListNotification = (lnAdded, lnExtracted, lnDeleted);

  TList = class(TObject)
  private
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear; dynamic;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TList;
    function Extract(item: Pointer): Pointer;
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
    destructor Destroy; override;
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
   TBitArray = array[0..MaxBitRec - 1] of cardinal;

   TBits = class(TObject)
   private
      { Private declarations }
      FBits : ^TBitArray;
      FSize : longint;  { total longints currently allocated }
      findIndex : longint;
      findState : boolean;

      { functions and properties to match TBits class }
      procedure SetBit(bit : longint; value : Boolean);
      function GetSize : longint;
      procedure SetSize(value : longint);
      procedure CheckBitIndex (Bit : longint;CurrentSize : Boolean);

   public
      { Public declarations }
      constructor Create(TheSize : longint); virtual;
      destructor Destroy; override;
      function  GetFSize : longint;
      procedure SetOn(Bit : longint);
      procedure Clear(Bit : longint);
      procedure Clearall;
      procedure AndBits(BitSet : TBits);
      procedure OrBits(BitSet : TBits);
      procedure XorBits(BitSet : TBits);
      procedure NotBits(BitSet : TBits);
      function  Get(Bit : longint) : boolean;
      procedure Grow(NBit : longint);
      function  Equals(BitSet : TBits) : Boolean;
      procedure SetIndex(Index : longint);
      function  FindFirstBit(State : boolean) : longint;
      function  FindNextBit : longint;
      function  FindPrevBit : longint;

      { functions and properties to match TBits class }
      function OpenBit: longint;
      property Bits[Bit: longint]: Boolean read get write SetBit; default;
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

{ TInterfaced Persistent }

{$ifdef HASINTF}
  TInterfacedPersistent = class(TPersistent, IInterface)
  private
    FOwnerInterface: IInterface;
  protected
    { IInterface }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    procedure AfterConstruction; override;
  end;
{$endif HASINTF}

{ TRecall class }

  TRecall = class(TObject)
  private
    FStorage, FReference: TPersistent;
  public
    constructor Create(AStorage, AReference: TPersistent);
    destructor Destroy; override;
    procedure Store;
    procedure Forget;
    property Reference: TPersistent read FReference;
  end;

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
  TStringList = class;

  PStringItem = ^TStringItem;
  TStringItem = record
    FString: string;
    FObject: TObject;
  end;

  PStringItemList = ^TStringItemList;
  TStringItemList = array[0..MaxListSize] of TStringItem;
  TStringListSortCompare = function(List: TStringList; Index1, Index2: Integer): Integer;

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
    procedure QuickSort(L, R: Integer; CompareFn: TStringListSortCompare);
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
    procedure CustomSort(CompareFn: TStringListSortCompare);
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

{ TStream abstract class }

  TStream = class(TObject)
  private
{$ifdef seek64bit}
    function GetPosition: Int64;
    procedure SetPosition(Pos: Int64);
    function GetSize: Int64;
    procedure SetSize64(NewSize: Int64);
{$else seek64bit}
    function GetPosition: Longint;
    procedure SetPosition(Pos: Longint);
    function GetSize: Longint;
{$endif seek64bit}
  protected
{$ifdef seek64bit}
    procedure SetSize(NewSize: Longint); virtual;overload;
    procedure SetSize(NewSize: Int64); virtual;overload;
{$else seek64bit}
    procedure SetSize(NewSize: Longint); virtual;
{$endif seek64bit}
  public
    function Read(var Buffer; Count: Longint): Longint; virtual; abstract;
    function Write(const Buffer; Count: Longint): Longint; virtual; abstract;
{$ifdef seek64bit}
    function Seek(Offset: Longint; Origin: Word): Longint; virtual; overload;
    function Seek(Offset: Int64; Origin: TSeekOrigin): Int64; virtual; overload;
{$else seek64bit}
    function Seek(Offset: Longint; Origin: Word): Longint; virtual; abstract;
{$endif seek64bit}
    procedure ReadBuffer(var Buffer; Count: Longint);
    procedure WriteBuffer(const Buffer; Count: Longint);
    function CopyFrom(Source: TStream; Count: Int64): Int64;
    function ReadComponent(Instance: TComponent): TComponent;
    function ReadComponentRes(Instance: TComponent): TComponent;
    procedure WriteComponent(Instance: TComponent);
    procedure WriteComponentRes(const ResName: string; Instance: TComponent);
    procedure WriteDescendent(Instance, Ancestor: TComponent);
    procedure WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent);
    procedure WriteResourceHeader(const ResName: string; {!!!:out} var FixupInfo: Integer);
    procedure FixupResourceHeader(FixupInfo: Integer);
    procedure ReadResHeader;
    function ReadByte : Byte;
    function ReadWord : Word;
    function ReadDWord : Cardinal;
    function ReadAnsiString : String;
    procedure WriteByte(b : Byte);
    procedure WriteWord(w : Word);
    procedure WriteDWord(d : Cardinal);
    Procedure WriteAnsiString (S : String);
{$ifdef seek64bit}
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize64;
{$else seek64bit}
    property Position: Longint read GetPosition write SetPosition;
    property Size: Longint read GetSize write SetSize;
{$endif seek64bit}
  end;

{$ifdef HASINTF}
  IStreamPersist = interface ['{B8CD12A3-267A-11D4-83DA-00C04F60B2DD}']
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;
{$endif HASINTF}

{ THandleStream class }

  THandleStream = class(TStream)
  private
    FHandle: Integer;
  protected
{$ifdef seek64bit}
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(NewSize: Int64); override;
{$else seek64bit}
    procedure SetSize(NewSize: Longint); override;
{$endif seek64bit}
  public
    constructor Create(AHandle: Integer);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
{$ifdef seek64bit}
    function Seek(Offset: Int64; Origin: TSeekOrigin): Int64; override;
{$else seek64bit}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
{$endif seek64bit}
    property Handle: Integer read FHandle;
  end;

{ TFileStream class }

  TFileStream = class(THandleStream)
  Private
    FFileName : String;
  public
    constructor Create(const AFileName: string; Mode: Word);
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal);
    destructor Destroy; override;
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
    vaNil, vaCollection, vaSingle, vaCurrency, vaDate, vaWString, vaInt64);

  TFilerFlag = (ffInherited, ffChildPos, ffInline);
  TFilerFlags = set of TFilerFlag;

  TReaderProc = procedure(Reader: TReader) of object;
  TWriterProc = procedure(Writer: TWriter) of object;
  TStreamProc = procedure(Stream: TStream) of object;

  TFiler = class(TObject)
  private
    FRoot: TComponent;
    FLookupRoot: TComponent;
    FAncestor: TPersistent;
    FIgnoreChildren: Boolean;
  protected
    procedure SetRoot(ARoot: TComponent); virtual;
  public
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); virtual; abstract;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); virtual; abstract;
    property Root: TComponent read FRoot write SetRoot;
    property LookupRoot: TComponent read FLookupRoot;
    property Ancestor: TPersistent read FAncestor write FAncestor;
    property IgnoreChildren: Boolean read FIgnoreChildren write FIgnoreChildren;
  end;


{ TComponent class reference type }

  TComponentClass = class of TComponent;


{ TReader }

  TAbstractObjectReader = class
  public
    function NextValue: TValueType; virtual; abstract;
    function ReadValue: TValueType; virtual; abstract;
    procedure BeginRootComponent; virtual; abstract;
    procedure BeginComponent(var Flags: TFilerFlags; var AChildPos: Integer;
      var CompClassName, CompName: String); virtual; abstract;
    function BeginProperty: String; virtual; abstract;

    { All ReadXXX methods are called _after_ the value type has been read! }
    procedure ReadBinary(const DestData: TMemoryStream); virtual; abstract;
    function ReadFloat: Extended; virtual; abstract;
    function ReadSingle: Single; virtual; abstract;
    {!!!: function ReadCurrency: Currency; virtual; abstract;}
    function ReadDate: TDateTime; virtual; abstract;
    function ReadIdent(ValueType: TValueType): String; virtual; abstract;
    function ReadInt8: ShortInt; virtual; abstract;
    function ReadInt16: SmallInt; virtual; abstract;
    function ReadInt32: LongInt; virtual; abstract;
    function ReadInt64: Int64; virtual; abstract;
    function ReadSet(EnumType: Pointer): Integer; virtual; abstract;
    function ReadStr: String; virtual; abstract;
    function ReadString(StringType: TValueType): String; virtual; abstract;
    procedure SkipComponent(SkipComponentInfos: Boolean); virtual; abstract;
    procedure SkipValue; virtual; abstract;
  end;

  TBinaryObjectReader = class(TAbstractObjectReader)
  private
    FStream: TStream;
    FBuffer: Pointer;
    FBufSize: Integer;
    FBufPos: Integer;
    FBufEnd: Integer;
    procedure Read(var Buf; Count: LongInt);
    procedure SkipProperty;
    procedure SkipSetBody;
  public
    constructor Create(Stream: TStream; BufSize: Integer);
    destructor Destroy; override;

    function NextValue: TValueType; override;
    function ReadValue: TValueType; override;
    procedure BeginRootComponent; override;
    procedure BeginComponent(var Flags: TFilerFlags; var AChildPos: Integer;
      var CompClassName, CompName: String); override;
    function BeginProperty: String; override;

    procedure ReadBinary(const DestData: TMemoryStream); override;
    function ReadFloat: Extended; override;
    function ReadSingle: Single; override;
    {!!!: function ReadCurrency: Currency; override;}
    function ReadDate: TDateTime; override;
    function ReadIdent(ValueType: TValueType): String; override;
    function ReadInt8: ShortInt; override;
    function ReadInt16: SmallInt; override;
    function ReadInt32: LongInt; override;
    function ReadInt64: Int64; override;
    function ReadSet(EnumType: Pointer): Integer; override;
    function ReadStr: String; override;
    function ReadString(StringType: TValueType): String; override;
    procedure SkipComponent(SkipComponentInfos: Boolean); override;
    procedure SkipValue; override;
  end;


  TFindMethodEvent = procedure(Reader: TReader; const MethodName: string;
    var Address: Pointer; var Error: Boolean) of object;
  TSetNameEvent = procedure(Reader: TReader; Component: TComponent;
    var Name: string) of object;
  TReferenceNameEvent = procedure(Reader: TReader; var Name: string) of object;
  TAncestorNotFoundEvent = procedure(Reader: TReader; const ComponentName: string;
    ComponentClass: TPersistentClass; var Component: TComponent) of object;
  TReadComponentsProc = procedure(Component: TComponent) of object;
  TReaderError = procedure(Reader: TReader; const Message: string; var Handled: Boolean) of object;
  TFindComponentClassEvent = procedure(Reader: TReader; const ClassName: string;
    var ComponentClass: TComponentClass) of object;
  TCreateComponentEvent = procedure(Reader: TReader;
    ComponentClass: TComponentClass; var Component: TComponent) of object;

  TReader = class(TFiler)
  private
    FDriver: TAbstractObjectReader;
    FOwner: TComponent;
    FParent: TComponent;
    FFixups: TList;
    FLoaded: TList;
    FOnFindMethod: TFindMethodEvent;
    FOnSetName: TSetNameEvent;
    FOnReferenceName: TReferenceNameEvent;
    FOnAncestorNotFound: TAncestorNotFoundEvent;
    FOnError: TReaderError;
    FOnFindComponentClass: TFindComponentClassEvent;
    FOnCreateComponent: TCreateComponentEvent;
    FPropName: string;
    FCanHandleExcepts: Boolean;
    procedure DoFixupReferences;
    procedure FreeFixups;
    function FindComponentClass(const AClassName: string): TComponentClass;
  protected
    function Error(const Message: string): Boolean; virtual;
    function FindMethod(ARoot: TComponent; const AMethodName: string): Pointer; virtual;
    procedure ReadProperty(AInstance: TPersistent);
    procedure ReadPropValue(Instance: TPersistent; PropInfo: Pointer);
    procedure PropertyError;
    procedure ReadData(Instance: TComponent);
    property PropName: string read FPropName;
    property CanHandleExceptions: Boolean read FCanHandleExcepts;
  public
    constructor Create(Stream: TStream; BufSize: Integer);
    destructor Destroy; override;
    procedure BeginReferences;
    procedure CheckValue(Value: TValueType);
    procedure DefineProperty(const Name: string;
      AReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      AReadData, WriteData: TStreamProc;
      HasData: Boolean); override;
    function EndOfList: Boolean;
    procedure EndReferences;
    procedure FixupReferences;
    function NextValue: TValueType;
    function ReadBoolean: Boolean;
    function ReadChar: Char;
    procedure ReadCollection(Collection: TCollection);
    function ReadComponent(Component: TComponent): TComponent;
    procedure ReadComponents(AOwner, AParent: TComponent;
      Proc: TReadComponentsProc);
    function ReadFloat: Extended;
    function ReadSingle: Single;
    {!!!: function ReadCurrency: Currency;}
    function ReadDate: TDateTime;
    function ReadIdent: string;
    function ReadInteger: Longint;
    function ReadInt64: Int64;
    procedure ReadListBegin;
    procedure ReadListEnd;
    function ReadRootComponent(ARoot: TComponent): TComponent;
    function ReadString: string;
    {!!!: function ReadWideString: WideString;}
    function ReadValue: TValueType;
    procedure CopyValue(Writer: TWriter);
    property Driver: TAbstractObjectReader read FDriver;
    property Owner: TComponent read FOwner write FOwner;
    property Parent: TComponent read FParent write FParent;
    property OnError: TReaderError read FOnError write FOnError;
    property OnFindMethod: TFindMethodEvent read FOnFindMethod write FOnFindMethod;
    property OnSetName: TSetNameEvent read FOnSetName write FOnSetName;
    property OnReferenceName: TReferenceNameEvent read FOnReferenceName write FOnReferenceName;
    property OnAncestorNotFound: TAncestorNotFoundEvent read FOnAncestorNotFound write FOnAncestorNotFound;
    property OnCreateComponent: TCreateComponentEvent read FOnCreateComponent write FOnCreateComponent;
    property OnFindComponentClass: TFindComponentClassEvent read FOnFindComponentClass write FOnFindComponentClass;
  end;


{ TWriter }

  TAbstractObjectWriter = class
  public
    { Begin/End markers. Those ones who don't have an end indicator, use
      "EndList", after the occurrence named in the comment. Note that this
      only counts for "EndList" calls on the same level; each BeginXXX call
      increases the current level. }
    procedure BeginCollection; virtual; abstract;  { Ends with the next "EndList" }
    procedure BeginComponent(Component: TComponent; Flags: TFilerFlags;
      ChildPos: Integer); virtual; abstract;  { Ends after the second "EndList" }
    procedure BeginList; virtual; abstract;
    procedure EndList; virtual; abstract;
    procedure BeginProperty(const PropName: String); virtual; abstract;
    procedure EndProperty; virtual; abstract;

    procedure WriteBinary(const Buffer; Count: Longint); virtual; abstract;
    procedure WriteBoolean(Value: Boolean); virtual; abstract;
    // procedure WriteChar(Value: Char);
    procedure WriteFloat(const Value: Extended); virtual; abstract;
    procedure WriteSingle(const Value: Single); virtual; abstract;
    {!!!: procedure WriteCurrency(const Value: Currency); virtual; abstract;}
    procedure WriteDate(const Value: TDateTime); virtual; abstract;
    procedure WriteIdent(const Ident: string); virtual; abstract;
    procedure WriteInteger(Value: Int64); virtual; abstract;
    procedure WriteMethodName(const Name: String); virtual; abstract;
    procedure WriteSet(Value: LongInt; SetType: Pointer); virtual; abstract;
    procedure WriteString(const Value: String); virtual; abstract;
  end;

  TBinaryObjectWriter = class(TAbstractObjectWriter)
  private
    FStream: TStream;
    FBuffer: Pointer;
    FBufSize: Integer;
    FBufPos: Integer;
    FBufEnd: Integer;
    FSignatureWritten: Boolean;
    procedure FlushBuffer;
    procedure Write(const Buffer; Count: Longint);
    procedure WriteValue(Value: TValueType);
    procedure WriteStr(const Value: String);
  public
    constructor Create(Stream: TStream; BufSize: Integer);
    destructor Destroy; override;

    procedure BeginCollection; override;
    procedure BeginComponent(Component: TComponent; Flags: TFilerFlags;
      ChildPos: Integer); override;
    procedure BeginList; override;
    procedure EndList; override;
    procedure BeginProperty(const PropName: String); override;
    procedure EndProperty; override;

    procedure WriteBinary(const Buffer; Count: LongInt); override;
    procedure WriteBoolean(Value: Boolean); override;
    procedure WriteFloat(const Value: Extended); override;
    procedure WriteSingle(const Value: Single); override;
    {!!!: procedure WriteCurrency(const Value: Currency);  override;}
    procedure WriteDate(const Value: TDateTime); override;
    procedure WriteIdent(const Ident: string); override;
    procedure WriteInteger(Value: Int64); override;
    procedure WriteMethodName(const Name: String); override;
    procedure WriteSet(Value: LongInt; SetType: Pointer); override;
    procedure WriteString(const Value: String); override;
  end;

  TTextObjectWriter = class(TAbstractObjectWriter)
  end;


  TFindAncestorEvent = procedure (Writer: TWriter; Component: TComponent;
    const Name: string; var Ancestor, RootAncestor: TComponent) of object;

  TWriter = class(TFiler)
  private
    FDriver: TAbstractObjectWriter;
    FDestroyDriver: Boolean;
    FRootAncestor: TComponent;
    FPropPath: String;
    FAncestorList: TList;
    FAncestorPos: Integer;
    FChildPos: Integer;
    FOnFindAncestor: TFindAncestorEvent;
    procedure AddToAncestorList(Component: TComponent);
    procedure WriteComponentData(Instance: TComponent);
  protected
    procedure SetRoot(ARoot: TComponent); override;
    procedure WriteBinary(AWriteData: TStreamProc);
    procedure WriteProperty(Instance: TPersistent; PropInfo: Pointer);
    procedure WriteProperties(Instance: TPersistent);
  public
    constructor Create(ADriver: TAbstractObjectWriter);
    constructor Create(Stream: TStream; BufSize: Integer);
    destructor Destroy; override;
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; AWriteData: TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, AWriteData: TStreamProc;
      HasData: Boolean); override;
    procedure WriteBoolean(Value: Boolean);
    procedure WriteCollection(Value: TCollection);
    procedure WriteComponent(Component: TComponent);
    procedure WriteChar(Value: Char);
    procedure WriteDescendent(ARoot: TComponent; AAncestor: TComponent);
    procedure WriteFloat(const Value: Extended);
    procedure WriteSingle(const Value: Single);
    {!!!: procedure WriteCurrency(const Value: Currency);}
    procedure WriteDate(const Value: TDateTime);
    procedure WriteIdent(const Ident: string);
    procedure WriteInteger(Value: Longint); overload;
    procedure WriteInteger(Value: Int64); overload;
    procedure WriteListBegin;
    procedure WriteListEnd;
    procedure WriteRootComponent(ARoot: TComponent);
    procedure WriteString(const Value: string);
    {!!!: procedure WriteWideString(const Value: WideString);}
    property RootAncestor: TComponent read FRootAncestor write FRootAncestor;
    property OnFindAncestor: TFindAncestorEvent read FOnFindAncestor write FOnFindAncestor;

    property Driver: TAbstractObjectWriter read FDriver;
  end;


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
{$ifdef Unix}
    { Needed for linux }
    FStackPointer : integer;
    FStackSize    : integer;
    FCallExitProcess : boolean;
{$endif}
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
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
    csDesigning, csAncestor, csUpdating, csFixups, csFreeNotification,
    csInline, csDesignInstance);
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

  TBasicAction = class;

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
    procedure RemoveNotification(AComponent: TComponent);
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
    procedure ReadState(Reader: TReader); virtual;
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
    procedure WriteState(Writer: TWriter); virtual;
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure DestroyComponents;
    procedure Destroying;
    function ExecuteAction(Action: TBasicAction): Boolean; dynamic;
    function FindComponent(const AName: string): TComponent;
    procedure FreeNotification(AComponent: TComponent);
    procedure RemoveFreeNotification(AComponent: TComponent);
    procedure FreeOnRelease;
    function GetParentComponent: TComponent; dynamic;
    function HasParent: Boolean; dynamic;
    procedure InsertComponent(AComponent: TComponent);
    procedure RemoveComponent(AComponent: TComponent);
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): Integer; override;
    function UpdateAction(Action: TBasicAction): Boolean; dynamic;
    // property ComObject: IUnknown read GetComObject;
    property Components[Index: Integer]: TComponent read GetComponent;
    property ComponentCount: Integer read GetComponentCount;
    property ComponentIndex: Integer read GetComponentIndex write SetComponentIndex;
    property ComponentState: TComponentState read FComponentState;
    property ComponentStyle: TComponentStyle read FComponentStyle;
    property DesignInfo: Longint read FDesignInfo write FDesignInfo;
    property Owner: TComponent read FOwner;
    property VCLComObject: Pointer read FVCLComObject write FVCLComObject;
  published
    property Name: TComponentName read FName write SetName stored False;
    property Tag: Longint read FTag write FTag default 0;
  end;

{ TBasicActionLink }

  TBasicActionLink = class(TObject)
  private
    FOnChange: TNotifyEvent;
  protected
    FAction: TBasicAction;
    procedure AssignClient(AClient: TObject); virtual;
    procedure Change; virtual;
    function IsOnExecuteLinked: Boolean; virtual;
    procedure SetAction(Value: TBasicAction); virtual;
    procedure SetOnExecute(Value: TNotifyEvent); virtual;
  public
    constructor Create(AClient: TObject); virtual;
    destructor Destroy; override;
    function Execute(AComponent: TComponent{$ifndef VER1_0} = nil{$endif}): Boolean; virtual;
    function Update: Boolean; virtual;
    property Action: TBasicAction read FAction write SetAction;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TBasicActionLinkClass = class of TBasicActionLink;

{ TBasicAction }

  TBasicAction = class(TComponent)
  private
    FActionComponent: TComponent;
    FOnChange: TNotifyEvent;
    FOnExecute: TNotifyEvent;
    FOnUpdate: TNotifyEvent;
  protected
    FClients: TList;
    procedure Change; virtual;
    procedure SetOnExecute(Value: TNotifyEvent); virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; virtual;
    procedure UpdateTarget(Target: TObject); virtual;
    procedure ExecuteTarget(Target: TObject); virtual;
    function Execute: Boolean; dynamic;
    procedure RegisterChanges(Value: TBasicActionLink);
    procedure UnRegisterChanges(Value: TBasicActionLink);
    function Update: Boolean; virtual;
    property ActionComponent: TComponent read FActionComponent write FActionComponent;
    property OnExecute: TNotifyEvent read FOnExecute write SetOnExecute;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

{ TBasicAction class reference type }

  TBasicActionClass = class of TBasicAction;

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
function FindClass(const AClassName: string): TPersistentClass;
function GetClass(const AClassName: string): TPersistentClass;

{ Component registration routines }

procedure RegisterComponents(const Page: string;
  ComponentClasses: array of TComponentClass);
procedure RegisterNoIcon(ComponentClasses: array of TComponentClass);
procedure RegisterNonActiveX(ComponentClasses: array of TComponentClass;
  AxRegType: TActiveXRegType);

{!!!: var
  GlobalNameSpace: TMultiReadExclusiveWriteSynchronizer;}


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

procedure RegisterIntegerConsts(IntegerType: Pointer; IdentToIntFn: TIdentToInt;
  IntToIdentFn: TIntToIdent);
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
function FindNestedComponent(Root: TComponent; const NamePath: string): TComponent;

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
  Revision 1.19  2002-03-28 12:08:47  michael
  + Added sstriple and ssquad in shiftstate

  Revision 1.18  2002/01/06 21:54:49  peter
    * action classes added

  Revision 1.17  2001/12/03 21:39:58  peter
    * seek(int64) overload only for 1.1 compiler

  Revision 1.16  2001/11/24 20:41:04  carl
  * fix Peter's problems with compiling under version 1.0

  Revision 1.15  2001/11/20 18:53:29  peter
    * overload fix

  Revision 1.14  2001/10/29 19:38:13  michael
  + Added TRect definition

  Revision 1.13  2001/10/28 17:16:44  peter
    * int64 file functions added

  Revision 1.12  2001/10/23 21:51:02  peter
    * criticalsection renamed to rtlcriticalsection for kylix compatibility

  Revision 1.11  2001/08/12 22:10:36  peter
    * some interface updates for 1.1

  Revision 1.10  2001/05/14 21:17:24  florian
    * TGUID and IUnknown is defined in the system unit by
      1.1 and above

  Revision 1.9  2001/04/13 18:03:51  peter
    * added tthread.destroy override

  Revision 1.8  2001/02/09 20:38:28  sg
  Merges from Fixbranch:
  * Extended TRect type to have TopLeft and BottomRight fields as well
  * Added "Driver" property to TReader

  Revision 1.7  2001/02/02 23:51:27  peter
    * bit field to cardinal instead of longint

  Revision 1.6  2000/12/03 22:35:09  sg
  * Applied patch by Markus Kaemmerer (merged):
    - Added support for TStringList.CustomSort

  Revision 1.5  2000/11/13 15:46:55  marco
   * Unix renamefest for defines.

  Revision 1.4  2000/10/15 10:04:39  peter
    + Capitalization of TBits interface fixed; CheckBitIndex now checks for
      size (merged)

  Revision 1.3  2000/08/15 04:10:38  peter
    * delphi compatibility fix

  Revision 1.2  2000/07/13 11:32:59  michael
  + removed logs

}
