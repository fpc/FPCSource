{
    This file is part of the Free Pascal Run Time Library (rtl)
    Copyright (c) 1999-2008 by Michael Van Canneyt, Florian Klaempfl,
    and Micha Nelissen

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$H+}


{$IFDEF VER2_0}
// Sanity check
{$UNDEF FPC_TESTGENERICS}
{$ENDIF}

{$ifdef CLASSESINLINE}{$inline on}{$endif}


type
   { extra types to compile with FPC }
   HRSRC =  TFPResourceHandle {$ifndef ver2_2} deprecated {$endif};
   TComponentName = string;
   THandle = System.THandle;

   TPoint=Types.TPoint;
   TRect=Types.TRect;

{$ifndef windows}
   TSmallPoint = record
      x,y : smallint;
   end;
   HMODULE = longint;
{$else}
   TSmallPoint = Windows.TSmallPoint;
   HModule = System.HModule;
{$endif}

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
  TDuplicates = Types.TDuplicates;

// For Delphi and backwards compatibility.
const
  dupIgnore = Types.dupIgnore;
  dupAccept = Types.dupAccept;
  dupError  = Types.dupError;

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
  toWString = Char(5);

Const
  FilerSignature : Array[1..4] of char = 'TPF0';

type
{ Text alignment types }
  TAlignment = (taLeftJustify, taRightJustify, taCenter);

  TLeftRight = taLeftJustify..taRightJustify;

  TBiDiMode = (bdLeftToRight,bdRightToLeft,bdRightToLeftNoAlign,bdRightToLeftReadingOnly);


{ Types used by standard events }
  TShiftStateEnum = (ssShift, ssAlt, ssCtrl,
    ssLeft, ssRight, ssMiddle, ssDouble,
    // Extra additions
    ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum,
    ssScroll,ssTriple,ssQuad,ssExtra1,ssExtra2);

{$packset 1}
  TShiftState = set of TShiftStateEnum;
{$packset default}

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
{$ifdef FPC_TESTGENERICS}
  EListError = fgl.EListError;
{$else}
  EListError = class(Exception);
{$endif}
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

{ TFPList class }

  PPointerList = ^TPointerList;
  TPointerList = array[0..MaxListSize - 1] of Pointer;
  TListSortCompare = function (Item1, Item2: Pointer): Integer;
  TListCallback = Types.TListCallback;
  TListStaticCallback = Types.TListStaticCallback;


{$IFNDEF FPC_TESTGENERICS}

  TListAssignOp = (laCopy, laAnd, laOr, laXor, laSrcUnique, laDestUnique);
  TFPList = class;

  TFPListEnumerator = class
  private
    FList: TFPList;
    FPosition: Integer;
  public
    constructor Create(AList: TFPList);
    function GetCurrent: Pointer;
    function MoveNext: Boolean;
    property Current: Pointer read GetCurrent;
  end;

  TFPList = class(TObject)
  private
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;
    procedure CopyMove (aList : TFPList);
    procedure MergeMove (aList : TFPList);
    procedure DoCopy(ListA, ListB : TFPList);
    procedure DoSrcUnique(ListA, ListB : TFPList);
    procedure DoAnd(ListA, ListB : TFPList);
    procedure DoDestUnique(ListA, ListB : TFPList);
    procedure DoOr(ListA, ListB : TFPList);
    procedure DoXOr(ListA, ListB : TFPList);
  protected
    function Get(Index: Integer): Pointer; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    procedure Put(Index: Integer; Item: Pointer); {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    Procedure RaiseIndexError(Index: Integer);
  public
    destructor Destroy; override;
    Procedure AddList(AList : TFPList);
    function Add(Item: Pointer): Integer; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    procedure Clear;
    procedure Delete(Index: Integer); {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    class procedure Error(const Msg: string; Data: PtrInt);
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TFPList; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function Extract(Item: Pointer): Pointer;
    function First: Pointer;
    function GetEnumerator: TFPListEnumerator;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer); {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Assign (ListA: TFPList; AOperator: TListAssignOp=laCopy; ListB: TFPList=nil);
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure ForEachCall(proc2call:TListCallback;arg:pointer);
    procedure ForEachCall(proc2call:TListStaticCallback;arg:pointer);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PPointerList read FList;
  end;

{$else}

  TFPPtrList = specialize TFPGList<Pointer>;

  TFPList = class(TFPPtrList)
  public
    procedure Assign(Source: TFPList);
    procedure Sort(Compare: TListSortCompare);
    procedure ForEachCall(Proc2call: TListCallback; Arg: Pointer);
    procedure ForEachCall(Proc2call: TListStaticCallback; Arg: Pointer);
  end;

{$endif}

{ TList class}

  TListNotification = (lnAdded, lnExtracted, lnDeleted);
  TList = class;

  TListEnumerator = class
  private
    FList: TList;
    FPosition: Integer;
  public
    constructor Create(AList: TList);
    function GetCurrent: Pointer;
    function MoveNext: Boolean;
    property Current: Pointer read GetCurrent;
  end;

  TList = class(TObject)
  private
    FList: TFPList;
    procedure CopyMove (aList : TList);
    procedure MergeMove (aList : TList);
    procedure DoCopy(ListA, ListB : TList);
    procedure DoSrcUnique(ListA, ListB : TList);
    procedure DoAnd(ListA, ListB : TList);
    procedure DoDestUnique(ListA, ListB : TList);
    procedure DoOr(ListA, ListB : TList);
    procedure DoXOr(ListA, ListB : TList);
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    function GetCapacity: integer;
    procedure SetCount(NewCount: Integer);
    function GetCount: integer;
    function GetList: PPointerList;
  public
    constructor Create;
    destructor Destroy; override;
    Procedure AddList(AList : TList);
    function Add(Item: Pointer): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: PtrInt); virtual;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TList;
    function Extract(item: Pointer): Pointer;
    function First: Pointer;
    function GetEnumerator: TListEnumerator;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Assign (ListA: TList; AOperator: TListAssignOp=laCopy; ListB: TList=nil);
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PPointerList read GetList;
  end;

{ TThreadList class }

  TThreadList = class
  private
    FList: TList;
    FDuplicates: TDuplicates;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Pointer);
    procedure Clear;
    function  LockList: TList;
    procedure Remove(Item: Pointer);
    procedure UnlockList;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

{TBits Class}

const
   BITSHIFT = 5;
   MASK = 31; {for longs that are 32-bit in size}
   // to further increase, signed integer limits have to be researched.
   MaxBitFlags = $7FFFFFE0;
   MaxBitRec = MaxBitFlags Div (SizeOf(cardinal)*8);
type
   TBitArray = array[0..MaxBitRec - 1] of cardinal;

   TBits = class(TObject)
   private
      { Private declarations }
      FBits : ^TBitArray;
      FSize : longint;  { total longints currently allocated }
      FBSize: longint;  {total bits currently allocated}
      findIndex : longint;
      findState : boolean;

      { functions and properties to match TBits class }
      procedure SetBit(bit : longint; value : Boolean);
      procedure SetSize(value : longint);
      procedure CheckBitIndex (Bit : longint;CurrentSize : Boolean);
   public
      { Public declarations }
      constructor Create(TheSize : longint = 0); virtual;
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
      function  Equals(Obj : TObject): Boolean; override; overload;
      function  Equals(BitSet : TBits) : Boolean; overload;
      procedure SetIndex(Index : longint);
      function  FindFirstBit(State : boolean) : longint;
      function  FindNextBit : longint;
      function  FindPrevBit : longint;

      { functions and properties to match TBits class }
      function OpenBit: longint;
      property Bits[Bit: longint]: Boolean read get write SetBit; default;
      property Size: longint read FBSize write setSize;
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
    FUpdateCount: Integer;
    function GetIndex: Integer;
  protected
    procedure SetCollection(Value: TCollection);virtual;
    procedure Changed(AllItems: Boolean);
    function GetOwner: TPersistent; override;
    function GetDisplayName: string; virtual;
    procedure SetIndex(Value: Integer); virtual;
    procedure SetDisplayName(const Value: string); virtual;
    property UpdateCount: Integer read FUpdateCount;
  public
    constructor Create(ACollection: TCollection); virtual;
    destructor Destroy; override;
    function GetNamePath: string; override;
    property Collection: TCollection read FCollection write SetCollection;
    property ID: Integer read FID;
    property Index: Integer read GetIndex write SetIndex;
    property DisplayName: string read GetDisplayName write SetDisplayName;
  end;

  TCollectionEnumerator = class
  private
    FCollection: TCollection;
    FPosition: Integer;
  public
    constructor Create(ACollection: TCollection);
    function GetCurrent: TCollectionItem;
    function MoveNext: Boolean;
    property Current: TCollectionItem read GetCurrent;
  end;

  TCollectionItemClass = class of TCollectionItem;
  TCollectionNotification = (cnAdded, cnExtracting, cnDeleting);
  TCollectionSortCompare = function (Item1, Item2: TCollectionItem): Integer;

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
    procedure Changed;
    function GetItem(Index: Integer): TCollectionItem;
    procedure SetItem(Index: Integer; Value: TCollectionItem);
    procedure SetItemName(Item: TCollectionItem); virtual;
    procedure SetPropName; virtual;
    procedure Update(Item: TCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem;Action: TCollectionNotification); virtual;
    property PropName: string read GetPropName write FPropName;
    property UpdateCount: Integer read FUpdateCount;
  public
    constructor Create(AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Owner: TPersistent;
    function Add: TCollectionItem;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure Clear;
    procedure EndUpdate; virtual;
    procedure Delete(Index: Integer);
    function GetEnumerator: TCollectionEnumerator;
    function GetNamePath: string; override;
    function Insert(Index: Integer): TCollectionItem;
    function FindItemID(ID: Integer): TCollectionItem;
    procedure Sort(Const Compare : TCollectionSortCompare);
    property Count: Integer read GetCount;
    property ItemClass: TCollectionItemClass read FItemClass;
    property Items[Index: Integer]: TCollectionItem read GetItem write SetItem;
  end;

  TOwnedCollection = class(TCollection)
  private
    FOwner: TPersistent;
  protected
    Function GetOwner: TPersistent; override;
  public
    Constructor Create(AOwner: TPersistent;AItemClass: TCollectionItemClass);
  end;


  TStrings = class;

{ IStringsAdapter interface }

  { Maintains link between TStrings and IStrings implementations }
  IStringsAdapter = interface ['{739C2F34-52EC-11D0-9EA6-0020AF3D82DA}']
    procedure ReferenceStrings(S: TStrings);
    procedure ReleaseStrings;
  end;

{ TStringsEnumerator class }

  TStringsEnumerator = class
  private
    FStrings: TStrings;
    FPosition: Integer;
  public
    constructor Create(AStrings: TStrings);
    function GetCurrent: String;
    function MoveNext: Boolean;
    property Current: String read GetCurrent;
  end;

{ TStrings class }

  TStrings = class(TPersistent)
  private
    FSpecialCharsInited : boolean;
    FQuoteChar : Char;
    FDelimiter : Char;
    FNameValueSeparator : Char;
    FUpdateCount: Integer;
    FAdapter: IStringsAdapter;
    FLBS : TTextLineBreakStyle;
    FStrictDelimiter : Boolean;
    function GetCommaText: string;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
    Function GetLBS : TTextLineBreakStyle;
    Procedure SetLBS (AValue : TTextLineBreakStyle); 
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: string);
    procedure SetStringsAdapter(const Value: IStringsAdapter);
    procedure SetValue(const Name, Value: string);
    procedure SetDelimiter(c:Char);
    procedure SetQuoteChar(c:Char);
    procedure SetNameValueSeparator(c:Char);
    procedure WriteData(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: string; Data: Integer);
    procedure Error(const Msg: pstring; Data: Integer);
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
    property UpdateCount: Integer read FUpdateCount;
    Function DoCompareText(const s1,s2 : string) : PtrInt; virtual;
    Function GetDelimitedText: string;
    Procedure SetDelimitedText(Const AValue: string);
    Function GetValueFromIndex(Index: Integer): string;
    Procedure SetValueFromIndex(Index: Integer; const Value: string);
    Procedure CheckSpecialChars;
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
    function Equals(Obj: TObject): Boolean; override; overload;
    function Equals(TheStrings: TStrings): Boolean; overload;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetEnumerator: TStringsEnumerator;
    function GetText: PChar; virtual;
    function IndexOf(const S: string): Integer; virtual;
    function IndexOfName(const Name: string): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: string); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: string;
      AObject: TObject);
    procedure LoadFromFile(const FileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(TheText: PChar); virtual;
    procedure GetNameValue(Index : Integer; Out AName,AValue : String);
    function  ExtractName(Const S:String):String;
    Property TextLineBreakStyle : TTextLineBreakStyle Read GetLBS Write SetLBS;
    property Delimiter: Char read FDelimiter write SetDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    Property StrictDelimiter : Boolean Read FStrictDelimiter Write FStrictDelimiter;
    property QuoteChar: Char read FQuoteChar write SetQuoteChar;
    Property NameValueSeparator : Char Read FNameValueSeparator Write SetNameValueSeparator;
    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
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

  TStringList = class;

  TStringListSortCompare = function(List: TStringList; Index1, Index2: Integer): Integer;

{$IFNDEF FPC_TESTGENERICS}

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
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FDuplicates: TDuplicates;
    FCaseSensitive : Boolean;
    FSorted: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; CompareFn: TStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(b : boolean);
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
    procedure InsertItem(Index: Integer; const S: string); virtual;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); virtual;
    Function DoCompareText(const s1,s2 : string) : PtrInt; override;

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
    procedure CustomSort(CompareFn: TStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

{$else}

  TFPStrObjMap = specialize TFPGMap<string, TObject>;

  TStringListTextCompare = function(const s1, s2: string): PtrInt of object;

  TStringList = class(TStrings)
  private
    FMap: TFPStrObjMap;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOnCompareText: TStringListTextCompare;
    procedure SetCaseSensitive(NewSensitive: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function DefaultCompareText(const s1, s2: string): PtrInt;
    function DoCompareText(const s1, s2: string): PtrInt; override;
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetDuplicates: TDuplicates;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    function GetSorted: Boolean; {$ifdef CLASSESINLINE} inline; {$endif}
    function MapPtrCompare(Key1, Key2: Pointer): Integer;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure QuickSort(L, R: Integer; CompareFn: TStringListSortCompare);
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetDuplicates(NewDuplicates: TDuplicates);
    procedure SetSorted(NewSorted: Boolean); {$ifdef CLASSESINLINE} inline; {$endif}
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create;
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
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Sorted: Boolean read GetSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnCompareText: TStringListTextCompare read FOnCompareText write FOnCompareText;
  end;

{$endif}
  

{ TStream abstract class }

  TStream = class(TObject)
  protected
    function  GetPosition: Int64; virtual;
    procedure SetPosition(const Pos: Int64); virtual;
    function  GetSize: Int64; virtual;
    procedure SetSize64(const NewSize: Int64); virtual;
    procedure SetSize(NewSize: Longint); virtual;overload;
    procedure SetSize(const NewSize: Int64); virtual;overload;
	  procedure ReadNotImplemented;
	  procedure WriteNotImplemented;
  public
    function Read(var Buffer; Count: Longint): Longint; virtual;
    function Write(const Buffer; Count: Longint): Longint; virtual;
    function Seek(Offset: Longint; Origin: Word): Longint; virtual; overload;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; virtual; overload;
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
    function ReadQWord : QWord;
    function ReadAnsiString : String;
    procedure WriteByte(b : Byte);
    procedure WriteWord(w : Word);
    procedure WriteDWord(d : Cardinal);
    procedure WriteQWord(q : QWord);
    Procedure WriteAnsiString (const S : String);
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize64;
  end;

  TProxyStream = class(TStream)
  private
    FStream: IStream;
  protected
    function GetIStream: IStream;
  public
    constructor Create(const Stream: IStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Check(err:longint); virtual;
  end;

  { TOwnerStream }
  TOwnerStream = Class(TStream)
  Protected
    FOwner : Boolean;
    FSource : TStream;
  Public
    Constructor Create(ASource : TStream);
    Destructor Destroy; override;
    Property Source : TStream Read FSource;
    Property SourceOwner : Boolean Read Fowner Write FOwner;
  end;


  IStreamPersist = interface ['{B8CD12A3-267A-11D4-83DA-00C04F60B2DD}']
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

{ THandleStream class }

  THandleStream = class(TStream)
  private
    FHandle: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AHandle: Integer);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
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
    Function GetSize : Int64; Override;
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

{$ifdef UNICODE}
  TResourceStream = class(TCustomMemoryStream)
  private
    Res: TFPResourceHandle;
    Handle: THandle;
    procedure Initialize(Instance: THandle; Name, ResType: PWideChar);
  public
    constructor Create(Instance: THandle; const ResName: WideString; ResType: PWideChar);
    constructor CreateFromID(Instance: THandle; ResID: Integer; ResType: PWideChar);
    destructor Destroy; override;
  end;
{$else}
  TResourceStream = class(TCustomMemoryStream)
  private
    Res: TFPResourceHandle;
    Handle: THandle;
    procedure Initialize(Instance: THandle; Name, ResType: PChar);
  public
    constructor Create(Instance: THandle; const ResName: string; ResType: PChar);
    constructor CreateFromID(Instance: THandle; ResID: Integer; ResType: PChar);
    destructor Destroy; override;
  end;
{$endif UNICODE}

{ TStreamAdapter }

  TStreamOwnership = (soReference, soOwned);

{ Implements OLE IStream on TStream }
  TStreamAdapter = class(TInterfacedObject, IStream)
  private
    FStream    : TStream;
    FOwnership : TStreamOwnership;
    m_bReverted: Boolean;
  public
    constructor Create(Stream: TStream; Ownership: TStreamOwnership = soReference);
    destructor Destroy; override;
    function Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HResult; virtual; stdcall;
    function Write(pv: Pointer; cb: DWORD; pcbWritten: PDWORD): HResult; virtual; stdcall;
    function Seek(dlibMove: Largeint; dwOrigin: Longint; out libNewPosition: Largeint): HResult; virtual; stdcall;
    function SetSize(libNewSize: Largeint): HResult; virtual; stdcall;
    function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint; out cbWritten: Largeint): HResult; virtual; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; virtual; stdcall;
    function Revert: HResult; virtual; stdcall;
    function LockRegion(libOffset: Largeint; cb: Largeint; dwLockType: Longint): HResult; virtual; stdcall;
    function UnlockRegion(libOffset: Largeint; cb: Largeint; dwLockType: Longint): HResult; virtual; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult; virtual; stdcall;
    function Clone(out stm: IStream): HResult; virtual; stdcall;
    property Stream: TStream read FStream;
    property StreamOwnership: TStreamOwnership read FOwnership write FOwnership;
  end;

{ TFiler }

  TValueType = (vaNull, vaList, vaInt8, vaInt16, vaInt32, vaExtended,
    vaString, vaIdent, vaFalse, vaTrue, vaBinary, vaSet, vaLString,
    vaNil, vaCollection, vaSingle, vaCurrency, vaDate, vaWString, vaInt64,
    vaUTF8String, vaUString, vaQWord);

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

    //Please don't use read, better use ReadBinary whenever possible
    procedure Read(var Buf; Count: LongInt); virtual; abstract;
    { All ReadXXX methods are called _after_ the value type has been read! }
    procedure ReadBinary(const DestData: TMemoryStream); virtual; abstract;
{$ifndef FPUNONE}
    function ReadFloat: Extended; virtual; abstract;
    function ReadSingle: Single; virtual; abstract;
    function ReadDate: TDateTime; virtual; abstract;
{$endif}
    function ReadCurrency: Currency; virtual; abstract;
    function ReadIdent(ValueType: TValueType): String; virtual; abstract;
    function ReadInt8: ShortInt; virtual; abstract;
    function ReadInt16: SmallInt; virtual; abstract;
    function ReadInt32: LongInt; virtual; abstract;
    function ReadInt64: Int64; virtual; abstract;
    function ReadSet(EnumType: Pointer): Integer; virtual; abstract;
    function ReadStr: String; virtual; abstract;
    function ReadString(StringType: TValueType): String; virtual; abstract;
    function ReadWideString: WideString;virtual;abstract;
    function ReadUnicodeString: UnicodeString;virtual;abstract;
    procedure SkipComponent(SkipComponentInfos: Boolean); virtual; abstract;
    procedure SkipValue; virtual; abstract;
  end;

  { TBinaryObjectReader }

  TBinaryObjectReader = class(TAbstractObjectReader)
  protected
    FStream: TStream;
    FBuffer: Pointer;
    FBufSize: Integer;
    FBufPos: Integer;
    FBufEnd: Integer;

    function ReadWord : word; {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
    function ReadDWord : longword; {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
    function ReadQWord : qword; {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
{$ifndef FPUNONE}
    function ReadExtended : extended; {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
{$endif}
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

    //Please don't use read, better use ReadBinary whenever possible
    procedure Read(var Buf; Count: LongInt); override;
    procedure ReadBinary(const DestData: TMemoryStream); override;
{$ifndef FPUNONE}
    function ReadFloat: Extended; override;
    function ReadSingle: Single; override;
    function ReadDate: TDateTime; override;
{$endif}
    function ReadCurrency: Currency; override;
    function ReadIdent(ValueType: TValueType): String; override;
    function ReadInt8: ShortInt; override;
    function ReadInt16: SmallInt; override;
    function ReadInt32: LongInt; override;
    function ReadInt64: Int64; override;
    function ReadSet(EnumType: Pointer): Integer; override;
    function ReadStr: String; override;
    function ReadString(StringType: TValueType): String; override;
    function ReadWideString: WideString;override;
    function ReadUnicodeString: UnicodeString;override;
    procedure SkipComponent(SkipComponentInfos: Boolean); override;
    procedure SkipValue; override;
  end;


  TFindMethodEvent = procedure(Reader: TReader; const MethodName: string;
    var Address: Pointer; var Error: Boolean) of object;
  TSetMethodPropertyEvent = procedure(Reader: TReader; Instance: TPersistent;
    PropInfo: PPropInfo; const TheMethodName: string;
    var Handled: boolean) of object;
  TSetNameEvent = procedure(Reader: TReader; Component: TComponent;
    var Name: string) of object;
  TReferenceNameEvent = procedure(Reader: TReader; var Name: string) of object;
  TAncestorNotFoundEvent = procedure(Reader: TReader; const ComponentName: string;
    ComponentClass: TPersistentClass; var Component: TComponent) of object;
  TReadComponentsProc = procedure(Component: TComponent) of object;
  TReaderError = procedure(Reader: TReader; const Message: string;
    var Handled: Boolean) of object;
  TPropertyNotFoundEvent = procedure(Reader: TReader; Instance: TPersistent;
    var PropName: string; IsPath: boolean; var Handled, Skip: Boolean) of object;
  TFindComponentClassEvent = procedure(Reader: TReader; const ClassName: string;
    var ComponentClass: TComponentClass) of object;
  TCreateComponentEvent = procedure(Reader: TReader;
    ComponentClass: TComponentClass; var Component: TComponent) of object;

  TReadWriteStringPropertyEvent = procedure(Sender:TObject;
    const Instance: TPersistent; PropInfo: PPropInfo;
    var Content:string) of object;


  { TReader }

  TReader = class(TFiler)
  private
    FDriver: TAbstractObjectReader;
    FOwner: TComponent;
    FParent: TComponent;
    FFixups: TObject;
    FLoaded: TList;
    FOnFindMethod: TFindMethodEvent;
    FOnSetMethodProperty: TSetMethodPropertyEvent;
    FOnSetName: TSetNameEvent;
    FOnReferenceName: TReferenceNameEvent;
    FOnAncestorNotFound: TAncestorNotFoundEvent;
    FOnError: TReaderError;
    FOnPropertyNotFound: TPropertyNotFoundEvent;
    FOnFindComponentClass: TFindComponentClassEvent;
    FOnCreateComponent: TCreateComponentEvent;
    FPropName: string;
    FCanHandleExcepts: Boolean;
    FOnReadStringProperty:TReadWriteStringPropertyEvent;
    procedure DoFixupReferences;
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
    function CreateDriver(Stream: TStream; BufSize: Integer): TAbstractObjectReader; virtual;
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
    //Please don't use read, better use ReadBinary whenever possible
    //uuups, ReadBinary is protected ..
    procedure Read(var Buf; Count: LongInt); virtual;

    function ReadBoolean: Boolean;
    function ReadChar: Char;
    function ReadWideChar: WideChar;
    function ReadUnicodeChar: UnicodeChar;
    procedure ReadCollection(Collection: TCollection);
    function ReadComponent(Component: TComponent): TComponent;
    procedure ReadComponents(AOwner, AParent: TComponent;
      Proc: TReadComponentsProc);
{$ifndef FPUNONE}
    function ReadFloat: Extended;
    function ReadSingle: Single;
    function ReadDate: TDateTime;
{$endif}
    function ReadCurrency: Currency;
    function ReadIdent: string;
    function ReadInteger: Longint;
    function ReadInt64: Int64;
    procedure ReadListBegin;
    procedure ReadListEnd;
    function ReadRootComponent(ARoot: TComponent): TComponent;
    function ReadVariant: Variant;
    function ReadString: string;
    function ReadWideString: WideString;
    function ReadUnicodeString: UnicodeString;
    function ReadValue: TValueType;
    procedure CopyValue(Writer: TWriter);
    property Driver: TAbstractObjectReader read FDriver;
    property Owner: TComponent read FOwner write FOwner;
    property Parent: TComponent read FParent write FParent;
    property OnError: TReaderError read FOnError write FOnError;
    property OnPropertyNotFound: TPropertyNotFoundEvent read FOnPropertyNotFound write FOnPropertyNotFound;
    property OnFindMethod: TFindMethodEvent read FOnFindMethod write FOnFindMethod;
    property OnSetMethodProperty: TSetMethodPropertyEvent read FOnSetMethodProperty write FOnSetMethodProperty;
    property OnSetName: TSetNameEvent read FOnSetName write FOnSetName;
    property OnReferenceName: TReferenceNameEvent read FOnReferenceName write FOnReferenceName;
    property OnAncestorNotFound: TAncestorNotFoundEvent read FOnAncestorNotFound write FOnAncestorNotFound;
    property OnCreateComponent: TCreateComponentEvent read FOnCreateComponent write FOnCreateComponent;
    property OnFindComponentClass: TFindComponentClassEvent read FOnFindComponentClass write FOnFindComponentClass;
    property OnReadStringProperty: TReadWriteStringPropertyEvent read FOnReadStringProperty write FOnReadStringProperty;
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
    //Please don't use write, better use WriteBinary whenever possible
    procedure Write(const Buffer; Count: Longint); virtual;abstract;

    procedure WriteBinary(const Buffer; Count: Longint); virtual; abstract;
    procedure WriteBoolean(Value: Boolean); virtual; abstract;
    // procedure WriteChar(Value: Char);
{$ifndef FPUNONE}
    procedure WriteFloat(const Value: Extended); virtual; abstract;
    procedure WriteSingle(const Value: Single); virtual; abstract;
    procedure WriteDate(const Value: TDateTime); virtual; abstract;
{$endif}
    procedure WriteCurrency(const Value: Currency); virtual; abstract;
    procedure WriteIdent(const Ident: string); virtual; abstract;
    procedure WriteInteger(Value: Int64); virtual; abstract;
    procedure WriteUInt64(Value: QWord); virtual; abstract;
    procedure WriteVariant(const Value: Variant); virtual; abstract;
    procedure WriteMethodName(const Name: String); virtual; abstract;
    procedure WriteSet(Value: LongInt; SetType: Pointer); virtual; abstract;
    procedure WriteString(const Value: String); virtual; abstract;
    procedure WriteWideString(const Value: WideString);virtual;abstract;
    procedure WriteUnicodeString(const Value: UnicodeString);virtual;abstract;
  end;

  { TBinaryObjectWriter }

  TBinaryObjectWriter = class(TAbstractObjectWriter)
  protected
    FStream: TStream;
    FBuffer: Pointer;
    FBufSize: Integer;
    FBufPos: Integer;
    FBufEnd: Integer;
    FSignatureWritten: Boolean;

    procedure WriteWord(w : word); {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
    procedure WriteDWord(lw : longword); {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
    procedure WriteQWord(qw : qword); {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
{$ifndef FPUNONE}
    procedure WriteExtended(e : extended); {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
{$endif}
    procedure FlushBuffer;
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

    //Please don't use write, better use WriteBinary whenever possible
    procedure Write(const Buffer; Count: Longint); override;
    procedure WriteBinary(const Buffer; Count: LongInt); override;
    procedure WriteBoolean(Value: Boolean); override;
{$ifndef FPUNONE}
    procedure WriteFloat(const Value: Extended); override;
    procedure WriteSingle(const Value: Single); override;
    procedure WriteDate(const Value: TDateTime); override;
{$endif}
    procedure WriteCurrency(const Value: Currency); override;
    procedure WriteIdent(const Ident: string); override;
    procedure WriteInteger(Value: Int64); override;
    procedure WriteUInt64(Value: QWord); override;
    procedure WriteMethodName(const Name: String); override;
    procedure WriteSet(Value: LongInt; SetType: Pointer); override;
    procedure WriteString(const Value: String); override;
    procedure WriteWideString(const Value: WideString); override;
    procedure WriteUnicodeString(const Value: UnicodeString); override;
    procedure WriteVariant(const VarValue: Variant);override;
  end;

  TTextObjectWriter = class(TAbstractObjectWriter)
  end;


  TFindAncestorEvent = procedure (Writer: TWriter; Component: TComponent;
    const Name: string; var Ancestor, RootAncestor: TComponent) of object;
  TWriteMethodPropertyEvent = procedure (Writer: TWriter; Instance: TPersistent;
    PropInfo: PPropInfo;
    const MethodValue, DefMethodValue: TMethod;
    var Handled: boolean) of object;

  TWriter = class(TFiler)
  private
    FDriver: TAbstractObjectWriter;
    FDestroyDriver: Boolean;
    FRootAncestor: TComponent;
    FPropPath: String;
    FAncestors: TStringList;
    FAncestorPos: Integer;
    FCurrentPos: Integer;
    FOnFindAncestor: TFindAncestorEvent;
    FOnWriteMethodProperty: TWriteMethodPropertyEvent;
    FOnWriteStringProperty:TReadWriteStringPropertyEvent;
    procedure AddToAncestorList(Component: TComponent);
    procedure WriteComponentData(Instance: TComponent);
    Procedure DetermineAncestor(Component: TComponent);
    procedure DoFindAncestor(Component : TComponent);
  protected
    procedure SetRoot(ARoot: TComponent); override;
    procedure WriteBinary(AWriteData: TStreamProc);
    procedure WriteProperty(Instance: TPersistent; PropInfo: Pointer);
    procedure WriteProperties(Instance: TPersistent);
    procedure WriteChildren(Component: TComponent);
    function CreateDriver(Stream: TStream; BufSize: Integer): TAbstractObjectWriter; virtual;
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
    //Please don't use write, better use WriteBinary whenever possible
    //uuups, WriteBinary is protected ..
    procedure Write(const Buffer; Count: Longint); virtual;
    procedure WriteBoolean(Value: Boolean);
    procedure WriteCollection(Value: TCollection);
    procedure WriteComponent(Component: TComponent);
    procedure WriteChar(Value: Char);
    procedure WriteWideChar(Value: WideChar);
    procedure WriteDescendent(ARoot: TComponent; AAncestor: TComponent);
{$ifndef FPUNONE}
    procedure WriteFloat(const Value: Extended);
    procedure WriteSingle(const Value: Single);
    procedure WriteDate(const Value: TDateTime);
{$endif}
    procedure WriteCurrency(const Value: Currency);
    procedure WriteIdent(const Ident: string);
    procedure WriteInteger(Value: Longint); overload;
    procedure WriteInteger(Value: Int64); overload;
    procedure WriteListBegin;
    procedure WriteListEnd;
    procedure WriteRootComponent(ARoot: TComponent);
    procedure WriteString(const Value: string);
    procedure WriteWideString(const Value: WideString);
    procedure WriteUnicodeString(const Value: UnicodeString);
    procedure WriteVariant(const VarValue: Variant);
    property RootAncestor: TComponent read FRootAncestor write FRootAncestor;
    property OnFindAncestor: TFindAncestorEvent read FOnFindAncestor write FOnFindAncestor;
    property OnWriteMethodProperty: TWriteMethodPropertyEvent read FOnWriteMethodProperty write FOnWriteMethodProperty;
    property OnWriteStringProperty: TReadWriteStringPropertyEvent read FOnWriteStringProperty write FOnWriteStringProperty;

    property Driver: TAbstractObjectWriter read FDriver;
    property PropertyPath: string read FPropPath;
  end;


{ TParser }

  TParser = class(TObject)
  private
    fStream : TStream;
    fBuf : pchar;
    fBufLen : integer;
    fPos : integer;
    fDeltaPos : integer;
    fFloatType : char;
    fSourceLine : integer;
    fToken : char;
    fEofReached : boolean;
    fLastTokenStr : string;
    fLastTokenWStr : widestring;
    function GetTokenName(aTok : char) : string;
    procedure LoadBuffer;
    procedure CheckLoadBuffer; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    procedure ProcessChar; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function IsNumber : boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function IsHexNum : boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function IsAlpha : boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function IsAlphaNum : boolean; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function GetHexValue(c : char) : byte; {$ifdef CLASSESINLINE} inline; {$endif CLASSESINLINE}
    function GetAlphaNum : string;
    procedure HandleNewLine;
    procedure SkipSpaces;
    procedure SkipWhitespace;
    procedure HandleEof;
    procedure HandleAlphaNum;
    procedure HandleNumber;
    procedure HandleHexNumber;
    function HandleQuotedString : string;
    function HandleDecimalString(var ascii : boolean) : widestring;
    procedure HandleString;
    procedure HandleMinus;
    procedure HandleUnknown;
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
    function TokenComponentIdent: string;
{$ifndef FPUNONE}
    function TokenFloat: Extended;
{$endif}
    function TokenInt: Int64;
    function TokenString: string;
    function TokenWideString: WideString;
    function TokenSymbolIs(const S: string): Boolean;
    property FloatType: Char read fFloatType;
    property SourceLine: Integer read fSourceLine;
    property Token: Char read fToken;
  end;

{ TThread }

  EThread = class(Exception);
  EThreadDestroyCalled = class(EThread);
  TSynchronizeProcVar = procedure;
  TThreadMethod = procedure of object;

  TThreadPriority = (tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest,
    tpTimeCritical);

  TThread = class
  private
    FHandle: TThreadID;
    FTerminated: Boolean;
    FFreeOnTerminate: Boolean;
    FFinished: Boolean;
    FSuspended: LongBool;
    FReturnValue: Integer;
    FOnTerminate: TNotifyEvent;
    FFatalException: TObject;
    procedure CallOnTerminate;
    function GetPriority: TThreadPriority;
    procedure SetPriority(Value: TThreadPriority);
    procedure SetSuspended(Value: Boolean);
    function GetSuspended: Boolean;
  protected
    FThreadID: TThreadID; // someone might need it for pthread_* calls
    procedure DoTerminate; virtual;
    procedure Execute; virtual; abstract;
    procedure Synchronize(AMethod: TThreadMethod);
    property ReturnValue: Integer read FReturnValue write FReturnValue;
    property Terminated: Boolean read FTerminated;
{$ifdef Unix}
  private
    // see tthread.inc, ThreadFunc and TThread.Resume
    FSem: Pointer;
    FInitialSuspended: boolean;
    FSuspendedExternal: boolean;
    FThreadReaped: boolean;
{$endif}
{$ifdef netwlibc}
  private
    // see tthread.inc, ThreadFunc and TThread.Resume
    FSem: Pointer;
    FInitialSuspended: boolean;
    FSuspendedExternal: boolean;
    FPid: LongInt;
{$endif}
  public
    constructor Create(CreateSuspended: Boolean;
                       const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Resume;
    procedure Suspend;
    procedure Terminate;
    function WaitFor: Integer;
    class procedure Synchronize(AThread: TThread; AMethod: TThreadMethod);
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property Handle: TThreadID read FHandle;
    property Priority: TThreadPriority read GetPriority write SetPriority;
    property Suspended: Boolean read GetSuspended write SetSuspended;
    property ThreadID: TThreadID read FThreadID;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
    property FatalException: TObject read FFatalException;
  end;


{ TComponent class }

  TOperation = (opInsert, opRemove);
  TComponentState = set of (csLoading, csReading, csWriting, csDestroying,
    csDesigning, csAncestor, csUpdating, csFixups, csFreeNotification,
    csInline, csDesignInstance);
  TComponentStyle = set of (csInheritable, csCheckPropAvail, csSubComponent,
    csTransient);
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

  IInterfaceComponentReference = interface 
    ['{3FEEC8E1-E400-4A24-BCAC-1F01476439B1}']
    function GetComponent:TComponent;
   end;

  IDesignerNotify = interface
    ['{B971E807-E3A6-11D1-AAB1-00C04FB16FBC}']
    procedure Modified;
    procedure Notification(AnObject: TPersistent; Operation: TOperation);
  end;

  TComponentEnumerator = class
  private
    FComponent: TComponent;
    FPosition: Integer;
  public
    constructor Create(AComponent: TComponent);
    function GetCurrent: TComponent;
    function MoveNext: Boolean;
    property Current: TComponent read GetCurrent;
  end;

  TBasicAction = class;

  { TComponent }

  TComponent = class(TPersistent,IUnknown,IInterfaceComponentReference)
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
    function GetOwner: TPersistent; override;
    procedure Loaded; virtual;
    procedure Loading; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); virtual;
    procedure PaletteCreated; dynamic;
    procedure ReadState(Reader: TReader); virtual;
    procedure SetAncestor(Value: Boolean);
    procedure SetDesigning(Value: Boolean; SetChildren : Boolean = True);
    procedure SetDesignInstance(Value: Boolean);
    procedure SetInline(Value: Boolean);
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
    function QueryInterface(const IID: TGUID; out Obj): Hresult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function iicrGetComponent: TComponent;
    { IDispatch }
    //!!!! function GetTypeInfoCount(out Count: Integer): Integer; stdcall;
    //!!!! function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): Integer; stdcall;
    //!!!! function GetIDsOfNames(const IID: TGUID; Names: Pointer;
    //!!!!   NameCount, LocaleID: Integer; DispIDs: Pointer): Integer; stdcall;
    //!!!! function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
    //!!!!   Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): Integer; stdcall;
  public
    //!! Moved temporary
    // fpdoc doesn't handle this yet :(
{$ifndef fpdocsystem}
    function IInterfaceComponentReference.GetComponent=iicrgetcomponent;
{$endif}    
    procedure WriteState(Writer: TWriter); virtual;
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure DestroyComponents;
    procedure Destroying;
    function ExecuteAction(Action: TBasicAction): Boolean; dynamic;
    function FindComponent(const AName: string): TComponent;
    procedure FreeNotification(AComponent: TComponent);
    procedure RemoveFreeNotification(AComponent: TComponent);
    procedure FreeOnRelease;
    function GetEnumerator: TComponentEnumerator;
    function GetNamePath: string; override;
    function GetParentComponent: TComponent; dynamic;
    function HasParent: Boolean; dynamic;
    procedure InsertComponent(AComponent: TComponent);
    procedure RemoveComponent(AComponent: TComponent);
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): Integer; override;
    procedure SetSubComponent(ASubComponent: Boolean);
    function UpdateAction(Action: TBasicAction): Boolean; dynamic;
    // property ComObject: IUnknown read GetComObject;
    function IsImplementorOf (const Intf:IInterface):boolean;
    procedure ReferenceInterface(const intf:IInterface;op:TOperation);
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
    function Execute(AComponent: TComponent = nil): Boolean; virtual;
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

  IInterfaceList = interface ['{285DEA8A-B865-11D1-AAA7-00C04FB17A72}']
    function Get(i : Integer) : IUnknown;
    function GetCapacity : Integer;
    function GetCount : Integer;
    procedure Put(i : Integer;item : IUnknown);
    procedure SetCapacity(NewCapacity : Integer);
    procedure SetCount(NewCount : Integer);
    procedure Clear;
    procedure Delete(index : Integer);
    procedure Exchange(index1,index2 : Integer);
    function First : IUnknown;
    function IndexOf(item : IUnknown) : Integer;
    function Add(item : IUnknown) : Integer;
    procedure Insert(i : Integer;item : IUnknown);
    function Last : IUnknown;
    function Remove(item : IUnknown): Integer;
    procedure Lock;
    procedure Unlock;
    property Capacity : Integer read GetCapacity write SetCapacity;
    property Count : Integer read GetCount write SetCount;
    property Items[index : Integer] : IUnknown read Get write Put;default;
  end;

  TInterfaceList = class;

  TInterfaceListEnumerator = class
  private
    FList: TInterfaceList;
    FPosition: Integer;
  public
    constructor Create(AList: TInterfaceList);
    function GetCurrent: IUnknown;
    function MoveNext: Boolean;
    property Current: IUnknown read GetCurrent;
  end;

  TInterfaceList = class(TInterfacedObject,IInterfaceList)
  private
    FList : TThreadList;
  protected
    function Get(i : Integer) : IUnknown;
    function GetCapacity : Integer;
    function GetCount : Integer;
    procedure Put(i : Integer;item : IUnknown);
    procedure SetCapacity(NewCapacity : Integer);
    procedure SetCount(NewCount : Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Delete(index : Integer);
    procedure Exchange(index1,index2 : Integer);
    function First : IUnknown;
    function GetEnumerator: TInterfaceListEnumerator;
    function IndexOf(item : IUnknown) : Integer;
    function Add(item : IUnknown) : Integer;
    procedure Insert(i : Integer;item : IUnknown);
    function Last : IUnknown;
    function Remove(item : IUnknown): Integer;
    procedure Lock;
    procedure Unlock;

    function Expand : TInterfaceList;

    property Capacity : Integer read GetCapacity write SetCapacity;
    property Count : Integer read GetCount write SetCount;
    property Items[Index : Integer] : IUnknown read Get write Put;default;
  end;

{ ---------------------------------------------------------------------
    TDatamodule support
  ---------------------------------------------------------------------}
  TDataModule = class(TComponent)
  private
    FDPos: TPoint;
    FDSize: TPoint;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOldOrder : Boolean;
    Procedure ReadT(Reader: TReader);
    Procedure WriteT(Writer: TWriter);
    Procedure ReadL(Reader: TReader);
    Procedure WriteL(Writer: TWriter);
    Procedure ReadW(Reader: TReader);
    Procedure WriteW(Writer: TWriter);
    Procedure ReadH(Reader: TReader);
    Procedure WriteH(Writer: TWriter);
  protected
    Procedure DoCreate; virtual;
    Procedure DoDestroy; virtual;
    Procedure DefineProperties(Filer: TFiler); override;
    Procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    Function HandleCreateException: Boolean; virtual;
    Procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    Constructor CreateNew(AOwner: TComponent);
    Constructor CreateNew(AOwner: TComponent; CreateMode: Integer); virtual;
    destructor Destroy; override;
    Procedure AfterConstruction; override;
    Procedure BeforeDestruction; override;
    property DesignOffset: TPoint read FDPos write FDPos;
    property DesignSize: TPoint read FDSize write FDSize;
  published
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OldCreateOrder: Boolean read FOldOrder write FOldOrder;
  end;

var
  // IDE hooks for TDatamodule support.
  AddDataModule              : procedure (DataModule: TDataModule) of object;
  RemoveDataModule           : procedure (DataModule: TDataModule) of object;
  ApplicationHandleException : procedure (Sender: TObject) of object;
  ApplicationShowException   : procedure (E: Exception) of object;

{ ---------------------------------------------------------------------
    tthread helpers
  ---------------------------------------------------------------------}

{ function to be called when gui thread is ready to execute method
  result is true if a method has been executed
}
function CheckSynchronize(timeout : longint=0) : boolean;

var
  { method proc that is called to trigger gui thread to execute a
method }
  WakeMainThread : TNotifyEvent = nil;

{ ---------------------------------------------------------------------
    General streaming and registration routines
  ---------------------------------------------------------------------}


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

function PointsEqual(const P1, P2: TPoint): Boolean; {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
function PointsEqual(const P1, P2: TSmallPoint): Boolean; {$ifdef CLASSESINLINE}inline;{$endif CLASSESINLINE}
function InvalidPoint(X, Y: Integer): Boolean;
function InvalidPoint(const At: TPoint): Boolean;
function InvalidPoint(const At: TSmallPoint): Boolean;

{ Class registration routines }

procedure RegisterClass(AClass: TPersistentClass);
procedure RegisterClasses(AClasses: array of TPersistentClass);
procedure RegisterClassAlias(AClass: TPersistentClass; const Alias: string);
procedure UnRegisterClass(AClass: TPersistentClass);
procedure UnRegisterClasses(AClasses: array of TPersistentClass);
procedure UnRegisterModuleClasses(Module: HMODULE);
function FindClass(const AClassName: string): TPersistentClass;
function GetClass(const AClassName: string): TPersistentClass;
procedure StartClassGroup(AClass: TPersistentClass);
procedure GroupDescendentsWith(AClass, AClassGroup: TPersistentClass);
function ActivateClassGroup(AClass: TPersistentClass): TPersistentClass;
function ClassGroupOf(AClass: TPersistentClass): TPersistentClass;
function ClassGroupOf(Instance: TPersistent): TPersistentClass;

{ Component registration routines }

procedure RegisterComponents(const Page: string;
  ComponentClasses: array of TComponentClass);
procedure RegisterNoIcon(ComponentClasses: array of TComponentClass);
procedure RegisterNonActiveX(ComponentClasses: array of TComponentClass;
  AxRegType: TActiveXRegType);

var
  GlobalNameSpace: IReadWriteSync;

{ Object filing routines }

type
  TIdentMapEntry = record
    Value: Integer;
    Name: String;
  end;

  TIdentToInt = function(const Ident: string; var Int: Longint): Boolean;
  TIntToIdent = function(Int: Longint; var Ident: string): Boolean;
  TFindGlobalComponent = function(const Name: string): TComponent;
  TInitComponentHandler = function(Instance: TComponent; RootAncestor : TClass): boolean;

var
  MainThreadID: TThreadID;

procedure RegisterIntegerConsts(IntegerType: Pointer; IdentToIntFn: TIdentToInt;
  IntToIdentFn: TIntToIdent);
function IdentToInt(const Ident: string; var Int: Longint; const Map: array of TIdentMapEntry): Boolean;
function IntToIdent(Int: Longint; var Ident: string; const Map: array of TIdentMapEntry): Boolean;
function FindIntToIdent(AIntegerType: Pointer): TIntToIdent;
function FindIdentToInt(AIntegerType: Pointer): TIdentToInt;

procedure RegisterFindGlobalComponentProc(AFindGlobalComponent: TFindGlobalComponent);
procedure UnregisterFindGlobalComponentProc(AFindGlobalComponent: TFindGlobalComponent);
function FindGlobalComponent(const Name: string): TComponent;

function InitInheritedComponent(Instance: TComponent; RootAncestor: TClass): Boolean;
function InitComponentRes(const ResName: string; Instance: TComponent): Boolean;
function ReadComponentRes(const ResName: string; Instance: TComponent): TComponent;
function ReadComponentResEx(HInstance: THandle; const ResName: string): TComponent;
function ReadComponentResFile(const FileName: string; Instance: TComponent): TComponent;
procedure WriteComponentResFile(const FileName: string; Instance: TComponent);
procedure RegisterInitComponentHandler(ComponentClass: TComponentClass;   Handler: TInitComponentHandler);

procedure GlobalFixupReferences;
procedure GetFixupReferenceNames(Root: TComponent; Names: TStrings);
procedure GetFixupInstanceNames(Root: TComponent;
  const ReferenceRootName: string; Names: TStrings);
procedure RedirectFixupReferences(Root: TComponent; const OldRootName,
  NewRootName: string);
procedure RemoveFixupReferences(Root: TComponent; const RootName: string);
procedure RemoveFixups(Instance: TPersistent);
Function FindNestedComponent(Root : TComponent; APath : String; CStyle : Boolean = True) : TComponent;

procedure BeginGlobalLoading;
procedure NotifyGlobalLoading;
procedure EndGlobalLoading;

function CollectionsEqual(C1, C2: TCollection): Boolean;
function CollectionsEqual(C1, C2: TCollection; Owner1, Owner2: TComponent): Boolean;

{ Object conversion routines }

procedure ObjectBinaryToText(Input, Output: TStream);
procedure ObjectTextToBinary(Input, Output: TStream);

procedure ObjectResourceToText(Input, Output: TStream);
procedure ObjectTextToResource(Input, Output: TStream);

{ Utility routines }

function LineStart(Buffer, BufPos: PChar): PChar;
procedure BinToHex(BinValue, HexValue: PChar; BinBufSize: Integer);
function HexToBin(HexValue, BinValue: PChar; BinBufSize: Integer): Integer;
function ExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: PChar; Strings: TStrings): Integer;

