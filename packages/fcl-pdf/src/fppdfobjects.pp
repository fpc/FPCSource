{ **********************************************************************

  This file is part of the Free Component Library

  PDF File data structures
  Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  **********************************************************************}

unit fppdfobjects;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

{ $DEFINE DEBUGPDFALLOCATION}
{ $DEFINE DEBUGOBJECTDICT}

interface

uses
  TypInfo,
  {$IFDEF DEBUGPDFALLOCATION}
  Types,
  {$ENDIF}
  rtlConsts, SysUtils, Classes, Contnrs, fppdfconsts;

Const
  PDFTextArraySpaceTreshold = 200;

Type
  EPDF = Class(Exception);
  TPDFElementType = (peComment,peIndirectObject,peXREF,peXREFItem, peTrailer,peStartXRef,peMalFormed,
                     peValue,peStream,peArray,peContainer,peDictionary,peDictEntry,peRef,peCommand, peDocumentInfo);
  TPDFTokenType = (
     ptEOF,
     ptWhiteSpace,        //  #10,#13,#12,#11,#32,#8
     ptShl,               // <<
     ptShr,               // >>
     ptName,              // /
     ptComment,           // %
     ptSquareOpen,        // [
     ptSquareClose,       // ]
     ptCurlyOpen,         // {
     ptCurlyClose,        // }
     ptString,            // ( )
     ptByteString,        // Image data
     ptHexString,         // < >
     ptNumber,            // 0..9 and .
     ptKeyword            // Anything else
  );
  TPDFTokenTypes = set of TPDFTokenType;

  { TPDFToken }

  TPDFToken = record
    TokenType : TPDFTokenType;
    TokenData : RawByteString;
    TokenPos : Int64; // 0-Based
    Function CheckString(const aValue : RawByteString) : Boolean;
    Function HasOnlyHexChars : Boolean;
    Function IsInteger : Boolean;
    Function IsHexInteger : Boolean;
    Function AsHexInteger : Integer;
    Function AsBEHexInteger : Integer;
    Function AsInteger : Integer;
    Function AsDouble : Double;
    Function IsHexString : Boolean;
    Function IsString : Boolean;
    Function AsString : RawByteString;
    function AsName: RawByteString;
    Function IsInt64 : Boolean;
    Function AsInt64 : Int64;
    Function IsWhiteSpace : Boolean;
    Function IsKeyword: Boolean;
    Function IsName: Boolean;
    Function IsNumber : Boolean;
  end;

  TPDFTokenArray = Array of TPDFToken;

  TPDFObject = Class;
  TPDFIndirect = Class;
  TPDFDocument = Class;
  TPDFCommand = Class;
  TPDFCommandList = class;
  TPDFCMap = Class;

  TPDFIndirectClass = class of TPDFIndirect;
  TPDFCommandClass = Class of TPDFCommand;
  TPDFCommandListClass = class of TPDFCommandList;

  { TPDFObjectEnumerator }

  TPDFObjectEnumerator = class
  public
    function GetCurrent: TPDFObject; virtual; abstract;
    function MoveNext: Boolean; virtual;
    property Current: TPDFObject read GetCurrent;
  end;

  { TPDFSingleObjectEnumerator }

  TPDFSingleObjectEnumerator = Class(TPDFObjectEnumerator)
    FObject : TPDFObject;
    FFirst : Boolean;
  Public
    Constructor Create(aObj : TPDFObject);
    function GetCurrent: TPDFObject; override;
    function MoveNext: Boolean; override;
  end;


  { TPDFObject }

  TPDFObject = class
  Public
    Constructor Create(); virtual;
{$IFDEF DEBUGPDFALLOCATION}
    Destructor destroy; override;
{$ENDIF}
    class function ElementType : TPDFElementType; virtual; abstract;
    Function GetEnumerator : TPDFObjectEnumerator; virtual;
    Function GetDescription : String; virtual;
  end;

  { TPDFRef }

  { TPDFRefData }

  TPDFRefData = record
    ObjectID: Integer;
    ObjectGeneration: Integer;
    function IsEmpty : Boolean;
    Function AsString : String;
  end;


  TPDFRef = Class(TPDFObject)
  private
    FRef: TPDFRefData;
  public
    Constructor Create(aID,aVersion : Integer); overload; virtual;
    Function GetDescription: String; override;
    class function ElementType : TPDFElementType; override;
    property Ref : TPDFRefData Read FRef;
    Property ObjectID : Integer Read FRef.ObjectID  Write FRef.ObjectID;
    Property ObjectGeneration : Integer Read FRef.ObjectGeneration  Write FRef.ObjectGeneration;
  end;

  { TPDFValue }

  TPDFValue = Class(TPDFObject)
  private
    FValue: RawbyteString;
    FTokenType: TPDFTokenType;
    function GetAsBoolean: Boolean;
    function GetAsFloat: Double;
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    Function GetAsDateTime : TDateTime;
  Public
    Constructor Create(aValue : RawByteString; aTokenType : TPDFTokenType); overload; virtual;
    class function ElementType : TPDFElementType; override;

    function GetDescription : String; override;
    Function IsKeyword (const aKeyWord : string) : Boolean;
    Function IsInteger : Boolean;
    Function IsInt64 : Boolean;
    Property TokenType : TPDFTokentype Read FTokenType;
    Property Value : RawbyteString Read FValue Write FValue;
    Property AsInteger : Integer Read GetAsInteger;
    Property AsInt64 : Int64 Read GetAsInt64;
    Property AsBoolean : Boolean Read GetAsBoolean;
    Property AsFloat : Double Read GetAsFloat;
    Property AsDateTime : TDateTime Read GetAsDateTime;
  end;

  { TPDFTokensObject }

  TPDFTokensObject = class (TPDFObject)
  private
    FTokens: TPDFTokenArray;
  Public
    Constructor Create(const aTokens : TPDFTokenArray); overload;
    Property Tokens : TPDFTokenArray Read FTokens Write FTokens;
  end;

  { TPDFComment }

  TPDFComment = class(TPDFObject)
  private
    FComment: String;
  Public
    Constructor Create(const aComment : RawByteString); overload;
    class function ElementType : TPDFElementType; override;
    Property Comment : String Read FComment;
  end;

  { TPDFContainer }

  TPDFContainer = class(TPDFObject)
  Private
    FItems : TFPObjectList;
    function GetCount: Integer;
    function GetObject(aIndex : integer): TPDFObject;
    procedure SetObject(aIndex : integer; AValue: TPDFObject);
  Protected
    Function Replace(aIndex : Integer; aObj : TPDFObject) : TPDFObject;
  Public
    Constructor Create(); override;
    Class Function OwnsObjects : Boolean; virtual;
    class function ElementType : TPDFElementType; override;
    Destructor destroy; override;
    Function GetEnumerator : TPDFObjectEnumerator; override;
    Function Add(aObject : TPDFObject) : Integer; virtual;
    Property Count : Integer Read GetCount;
    Property Objects[aIndex : integer] : TPDFObject Read GetObject Write SetObject; default;
  end;

  { TPDFContainerObjectEnumerator }

  TPDFContainerObjectEnumerator = Class(TPDFObjectEnumerator)
  Private
    FContainer : TPDFContainer;
    FCurrentIdx : Integer;
  Public
    Constructor Create(aContainer : TPDFContainer);
    function GetCurrent: TPDFObject; override;
    function MoveNext: Boolean; override;
  end;



  { TPDFXRef }

  TPDFXRef = Class(TPDFObject)
  private
    FCompressed: Boolean;
    FInstance: TPDFIndirect;
    FInUse: Boolean;
    FNextFreeObject: Integer;
    FObjectIndex: Integer;
    FObjectOffset: Int64;
    FObjectGeneration: Integer;
    FReferenceIndex: Integer;
    FStreamObjectNr: Integer;
  Public
    class function ElementType: TPDFElementType; override;
    Function Match (aObjectID,aObjectGeneration : Integer) : Boolean;
    function GetDescription: String; override;
    // Reduce to 3 fields and special getter/setters
    // ID of the object being referenced.
    Property ReferenceIndex : Integer Read FReferenceIndex Write FReferenceIndex;
    // For free objects, ID of next object.
    Property NextFreeObject : Integer Read FNextFreeObject Write FNextFreeObject;
    // offset in the PDF file stream
    Property ObjectOffset : Int64 Read FObjectOffset Write FObjectOffset;
    // For compressed object Index in the compr
    Property ObjectIndex : Integer Read FObjectIndex Write FObjectIndex;
    // Version (generation) of the object  (both InUse and Not)
    Property ObjectGeneration : Integer Read FObjectGeneration Write FObjectGeneration;
    // For compressed streams, ID of the Stream object in which the object is.
    Property StreamObjectNr : Integer Read FStreamObjectNr Write FStreamObjectNr;
    // Is the object in use ?
    Property InUse : Boolean Read FInUse Write FInUse;
    // Is the object in a compressed stream ?
    Property Compressed : Boolean Read FCompressed Write FCompressed;
    // Reference to the object
    Property Instance : TPDFIndirect Read FInstance Write FInstance;
  end;
  TPDFXRefArray = Array of TPDFXRef;

  TPDFXRefList = class(TPDFContainer)
  private
    FCompressed: Boolean;
    function GetItm(aIndex : integer): TPDFXRef;
    procedure SetItm(aIndex : integer; AValue: TPDFXRef);
  Public
    class function ElementType : TPDFElementType; override;
    Function IndexOfReference(aObjectID,aObjectGeneration : Integer) : Integer;
    Function FindReference(aObjectID,aObjectGeneration : Integer) : TPDFXRef;
    Property References[aIndex : integer] : TPDFXRef Read GetItm Write SetItm; default;
    Property Compressed : Boolean Read FCompressed Write FCompressed;
  end;

  { TPDFStartXRef }

  TPDFStartXRef = class(TPDFObject)
  private
    FIndex: Int64;
  public
    class function ElementType : TPDFElementType; override;
    // Offset in PDF file where to find XRef index/object
    Property Index : Int64 Read FIndex Write FIndex;
  end;

  { TPDFArray }

  TPDFArray = class(TPDFContainer)
    class function ElementType : TPDFElementType; override;
    function GetDescription: String; override;
    Function IsIntegerAt(aIndex : Integer) : Boolean;
    Function IsKeywordAt(aIndex : Integer; const aKeyWord: RawByteString) : Boolean;
    Function GetIntegerAt(aIndex : Integer) : Integer;
  end;

  { TPDFDictEntry }

  TPDFDictEntry = Class(TPDFObject)
  private
    FKey: String;
    FValue: TPDFObject;
  Public
    class function ElementType : TPDFElementType; override;
    Destructor Destroy; override;
    function GetDescription: String; override;
    Property Key : String Read FKey Write FKey;
    // Value is owned by dict entry
    Property Value : TPDFObject Read FValue Write FValue;
  end;

  { TPDFDictionary }

  TPDFDictionary = class(TPDFContainer)
  Public
    class function ElementType : TPDFElementType; override;
    function GetDescription: String; override;
    Function AddEntry(const aKey : String; aValue : TPDFObject) : TPDFDictEntry;
    Function AddEntry(aEntry : TPDFDictEntry) : Integer;
    function ContainsKey(const aKeyword : RawByteString) : boolean;
    function IndexofKey(const aKeyword : RawByteString) : Integer;
    Function FindKey(const aKeyword : RawByteString) : TPDFDictEntry;
    Function GetKey(const aKeyword : RawByteString) : TPDFDictEntry;
    Function FindValue(const aKeyword : RawByteString) : TPDFObject;
    Function FindArrayValue(const aKeyword : RawByteString) : TPDFArray;
    Function FindDictionaryValue(const aKeyword : RawByteString) : TPDFDictionary;
    Function GetValue(const aKeyword : RawByteString) : TPDFObject;
    Function GetStringValue(const aKeyword : RawByteString) : RawByteString;
    Function GetIntegerValue(const aKeyword : RawByteString) : Integer;
    Function GetInt64Value(const aKeyword : RawByteString) : Int64;
    Function GetArrayValue(const aKeyword : RawByteString) : TPDFArray;
    Function GetDictionaryValue(const aKeyword : RawByteString) : TPDFDictionary;
  end;



  { TPDFTrailer }

  TPDFTrailer = class(TPDFDictionary)
    class function ElementType : TPDFElementType; override;
    function Contains(const aKeyword : RawByteString) : boolean;
  end;

  { TPDFStream }

  TPDFStream = Class(TPDFObject)
  private
    FData: TBytes;
  Public
    Constructor Create(const aData : TBytes); overload;
    class function ElementType : TPDFElementType; override;
    Property Data : TBytes Read FData Write FData;
  end;


  { TPDFIndirect }
  TPDFObjectPosition = record
    ID : Integer;
    Offset : Integer;
  end;
  TPDFObjectPositionArray = Array of TPDFObjectPosition;

  TPDFIndirect = class(TPDFContainer)
  private
    class var _ClassList : TFPDataHashTable;
  private
    FObjectType : String;
    FDict : TPDFDictionary;
    FObjectID: Integer;
    FObjectGeneration: Integer;
    FStream: TPDFStream;
    FUnfilteredStream: TStream;
    FObjectPositions : TPDFObjectPositionArray;
    FDocument : TPDFDocument;
    function GetObjectType: String;
  Protected
    function CheckObjectDict : Boolean;
    Class function RegisterAsType : String; virtual;
  public
    class constructor InitIndirect;
    class destructor DoneIndirect;
    Constructor Create(aSource : TPDFIndirect); overload; virtual;
    Destructor Destroy; override;
    Function ResolveObjectType : TPDFIndirect;
    Class function FindClassForType(const aType : String) : TPDFIndirectClass;
    Class Procedure RegisterType(const aType : String; aClass : TPDFIndirectClass);
    Class Procedure UnRegisterType(const aType : String);
    Class Procedure Register;
    Class Procedure UnRegister;
    class function ElementType : TPDFElementType; override;
    function FindDictValue(const aKey : RawByteString) : TPDFObject;
    function GetDictValue(const aKey : RawByteString) : TPDFObject;
    function GetDescription: String; override;
    Function Add(aObject : TPDFObject) : Integer; override;
    function Match(aObjectID : Integer; aObjectGeneration : Integer) : Boolean;
    // Document to which this indirect object belongs. Set by TPDFDocument.AddIndirect
    Property Document : TPDFDocument Read FDocument;
    // Set when adding items
    Property Stream : TPDFStream Read FStream;
    // owned by the Indirect object
    Property UnfilteredStream : TStream Read FUnfilteredStream Write FUnfilteredStream;
    // Object ID
    Property ObjectID : Integer Read FObjectID Write FObjectID;
    // object version/generation
    Property ObjectGeneration : Integer Read FObjectGeneration Write FObjectGeneration;
    // Set when adding items
    Property ObjectDict : TPDFDictionary Read FDict;
    // Determined from dictionary. empty steing if not found
    Property ObjectType : String Read GetObjectType;
    // For object stream
    Property ObjectPositions : TPDFObjectPositionArray Read FObjectPositions Write FObjectPositions;
  end;

  { TPDFIndirectXRef }

  TPDFIndirectXRef = class (TPDFIndirect)
  private
    FXref: TPDFXRefList;
  protected
    Class function RegisterAsType : String; override;
  Public
    Destructor destroy; override;
    Property XRef : TPDFXRefList Read FXref Write FXref;
  end;

  { TPDFPageObject }

  { TPDFObjectStreamObject }

  TPDFObjectStreamObject = Class(TPDFIndirect)
  protected
    Class function RegisterAsType : String; override;
  end;

  TPDFPageObject = Class(TPDFIndirect)
  private
    FResources : TPDFDictionary;
    FCommandList: TPDFCommandList;
    function GetContent(aIndex : integer): TPDFIndirect;
    function GetContentCount: Integer;
    function GetContentRef(aIndex : integer): TPDFrefData;
    function GetParent: TPDFIndirect;
    function GetParentRef: TPDFRef;
    function GetResources: TPDFDictionary;
  Protected
    Class function RegisterAsType : String; override;
    // Override if you want to use a descendent of TPDFCommandList;
    Class function CreateCommandList : TPDFCommandList; virtual;
  Public
    Constructor Create; override; overload;
    Destructor Destroy; override;
    function FindResources: TPDFDictionary;
    Function FindFontRefObj(const aFontName : String) : TPDFRef;
    Function FindFontRef(const aFontName : String) : TPDFRefData;
    Property ParentRef : TPDFRef Read GetParentRef;
    Property Parent : TPDFIndirect Read GetParent;
    Property Contents[aIndex : integer] : TPDFIndirect Read GetContent;
    Property ContentRef[aIndex : integer] : TPDFrefData Read GetContentRef;
    Property ContentCount : Integer read GetContentCount;
    Property CommandList : TPDFCommandList Read FCommandList;
    Property Resources : TPDFDictionary Read GetResources;
  end;
  TPDFPageClass = Class of TPDFPageObject;

  { TPDFFontObject }

  TPDFFontObject = Class(TPDFIndirect)
  private
    FUnicodeCMAP: TPDFCMap;
    function GetString(AIndex: Integer): String;
    function GetToUnicode: TPDFRefData;
    function GetToUnicodeObj: TPDFRef;
    function GetValueName(AIndex: Integer): String;
  Protected
    Class function RegisterAsType : String; override;
  Public
    Destructor Destroy; override;
    Function GetDescription: String; override;
    function ResolveToUnicodeMapStream(aDoc: TPDFDocument): TPDFIndirect;
    Property Type_ : String Index 0 Read GetString;
    Property SubType : String Index 1 Read GetString;
    Property Name : String Index 2 Read GetString;
    Property BaseFont: String Index 3 Read GetString;
    Property Encoding : String Index 4 Read GetString;
    Property ToUnicodeRef : TPDFRefData Read GetToUnicode; // 5
    Property ToUnicode : TPDFRef Read GetToUnicodeObj; // 5
    // Owned by Font !
    Property UnicodeCMAP : TPDFCMap Read FUnicodeCMAP Write FUnicodeCMAP;
  end;


  { TPDFPagesObject }

  TPDFPagesObject = Class(TPDFIndirect)
  private
    function GetChildCount: Integer;
    function GetChildObject(aIndex : integer): TPDFIndirect;
    function GetChildRef(aIndex : integer): TPDFRefData;
    function GetPageCount: Integer;
  Protected
    Class function RegisterAsType : String; override;

    Function ParentRef : TPdfRef;
    Function Parent : TPDFIndirect;
    // Find page with index aIndex. Will search in sub tree pages
    Function FindPage(aIndex : Integer) : TPDFPageObject;
    // Page count in this node
    Property PageCount : Integer Read GetPageCount;
    // Number of direct childen
    Property ChildCount : Integer Read GetChildCount;
    // Reference to child aIndex, 0-Based.
    Property ChildRef[aIndex : integer] : TPDFRefData Read GetChildRef;
    // Child I. Can be a TPDFPagesObject or a TPDFPageObject
    Property ChildObject[aIndex : integer] : TPDFIndirect Read GetChildObject;
  end;

  { TPDFCatalogObject }

  TPDFCatalogObject = Class(TPDFIndirect)
  private
    function GetPages: TPDFPagesObject;
  Protected
    Class function RegisterAsType : String; override;
    Property Pages : TPDFPagesObject Read GetPages;
  end;

  { TPDFCommand }
  TPDFCommand = Class(TPDFTokensObject)
  private
    class var _ClassList : TFPDataHashTable;
  private
    FCommand: String;
  Protected
    Class Function RegisterCommandName : String; virtual;
  Public
    Class constructor Init;
    Class Destructor Done;
    class function ElementType : TPDFElementType; override;
    Class function FindClassForCommand(const aCommand : String) : TPDFCommandClass;
    Class Procedure RegisterCommand(const aCommand : String; aClass : TPDFCommandClass);
    Class Procedure UnRegisterCommand(const aCommand : String);
    Class Procedure Register;
    Class Procedure UnRegister;
    Constructor Create(const aCommand : String; aTokens : TPDFTokenArray); reintroduce;
  Public
    Property Command : String Read FCommand Write FCommand;
  end;

  { TPDFBTCommand }

  TPDFBTCommand = class(TPDFCommand)
    Class Function RegisterCommandName : String;override;
  end;

  { TPDFETCommand }

  TPDFETCommand = class(TPDFCommand)
    Class Function RegisterCommandName : String; override;
  end;

  // Do not register this one.

  { TPDFTextCommand }

  TPDFTextCommand = Class(TPDFCommand)
  Public
    Function GetFullText(aUnicodeMap : TPDFCMap) : RawByteString; virtual; overload;
    Function GetFullText : RawByteString; virtual; abstract; overload;
  end;

  { TPDFTJCommand }

  TPDFTJCommand = class(TPDFTextCommand)
    Class Function RegisterCommandName : String; override;
    Function GetFullText(aUnicodeMap : TPDFCMap) : RawByteString; override; overload;
    Function GetFullText : RawByteString; override;
  end;

  { TPDFTfCommand }

  TPDFTfCommand = class(TPDFCommand)
  private
    function GetFontName: String;
    function GetFontSize: Integer;
  public
    Class Function RegisterCommandName : String; override;
    property FontName : String Read GetFontName;
    Property FontSize : Integer Read GetFontSize;
  end;

  { TPDFTj_Command }

  TPDFTj_Command = class(TPDFTextCommand)
    Class Function RegisterCommandName : String; override;
    Function GetFullText : RawByteString; override;
  end;


  { TPDFTdCommand }

  TPDFTDCommand = class(TPDFCommand)
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFTdCommand }

  { TPDFTd_Command }

  TPDFTd_Command = class(TPDFCommand)
    Class Function RegisterCommandName : String; override;
  end;


  { TPDFTfCommand }


  TPDFImageData = record
    Width,
    height,
    BitsPerComponent : Integer;
    ColorSpace : String;
    ColorSpaceComponents : Integer;
    Filters : Array of String;
  end;

  { TPDFImageDataCommand }

  TPDFImageDataCommand = Class(TPDFCommand)
    Class Procedure ParseImageOperands(aOperands : TPDFTokenArray; Out aImageData : TPDFImageData);
    Class Function RegisterCommandName : String; override;
  end;


  { TPDFUnknownCommand }
  // Catch all. Do not register
  TPDFUnknownCommand = class(TPDFCommand)
  end;


  { TPDFCommandEnumerator }

  TPDFCommandEnumerator = Class(TPDFContainerObjectEnumerator)
  Private
  Public
    function GetCurrent: TPDFCommand; override;
    Property Current : TPDFCommand Read GetCurrent;
  end;

  // This object owns all operands for all commands

  { TPDFCommandList }

  TPDFCommandList = Class(TPDFContainer)
  private
    function GetCommand(aIndex : Integer): TPDFCommand;
  Public
    Function GetEnumerator : TPDFCommandEnumerator; reintroduce;
    Property Commands[aIndex :Integer] : TPDFCommand Read GetCommand; default;
  end;

  { TPDFCodeSpaceRange }

  TPDFCodeSpaceRange = record
    RangeStart, RangeEnd : Cardinal;
    Function Contains(aCode : Cardinal) : Boolean;
  end;
  TPDFCodeSpaceRangeArray = Array of TPDFCodeSpaceRange;

  { TPDFNotDefRange }

  TPDFNotDefRange = record
    RangeStart, RangeEnd : Cardinal;
    ReplaceMent : Cardinal;
    Function Contains(aCode : Cardinal) : Boolean;
  end;
  TPDFNotDefRangeArray = Array of TPDFNotDefRange;

  TPDFBFChar = record
    Src,Dest : Cardinal;
    DestName : String;
  end;
  TPDFBFCharArray = Array of TPDFBFChar;

  { TPDFCIDRange }
  TPDFCIDUnicodeCharOrName = record
    Name : string;
    UnicodeChar : Cardinal;
  end;
  TPDFCIDUnicodeCharOrNameArray = Array of TPDFCIDUnicodeCharOrName;

  TPDFCIDRange = record
    RangeStart, RangeEnd : Cardinal;
    CharOffset : Cardinal;
    CharNames : TPDFCIDUnicodeCharOrNameArray;
    Function Contains(aCode : Cardinal) : Boolean;
  end;
  TPDFCIDRangeArray = Array of TPDFCIDRange;

  { TPDFCMapData }

  TPDFCMapData = class(TObject)
  private
    FBFChars: TPDFBFCharArray;
    FCIDRange: TPDFCIDRangeArray;
    FCodeSpaceRange: TPDFCodeSpaceRangeArray;
    FNotDefRange: TPDFNotDefRangeArray;
  Public
    Function Interpret(aRaw : RawByteString) : RawByteString;
    function GetNotDef(aCode: Integer): UnicodeString;
    function GetBFRangeChar(aCode: Cardinal; out aRes: Unicodestring): Boolean;
    function GetBFChar(aCode: Cardinal; out aRes: Unicodestring): Boolean;
    Function IndexInBFRange(aCode : Integer): Integer;
    Function IndexInBFChar(aCode : Integer): Integer;
    function IsValidCode(aCode: Cardinal): Boolean;
    Property CodeSpaceRange : TPDFCodeSpaceRangeArray Read FCodeSpaceRange Write FCodeSpaceRange;
    Property NotDefRange : TPDFNotDefRangeArray Read FNotDefRange Write FNotDefRange;
    Property BFRange : TPDFCIDRangeArray Read FCIDRange Write FCIDRange;
    Property BFChars : TPDFBFCharArray Read FBFChars Write FBFChars;
  end;

  { TPDFCMap }

  TPDFCMap = Class(TPDFIndirect)
  private
    FData: TPDFCMapData;
    function GetCMAPName: String;
    function GetRegistry: String;
    function GetSupplement: Integer;
    procedure SetData(AValue: TPDFCMapData);
  Public
    Destructor Destroy; override;
    Property CMapName : String Read GetCMAPName;
    Property Registry : String Read GetRegistry;
    Property Supplement : Integer Read GetSupplement;
    Function Interpret(aRaw : RawByteString) : RawByteString;
    Property Data : TPDFCMapData Read FData Write SetData;
  end;

  { TPDFContentStream }

  TPDFContentStream = class(TPDFIndirect);


  { TPDFMalFormed }

  TPDFMalFormed = class(TPDFTokensObject)
    class function ElementType : TPDFElementType; override;
  end;

  // This keeps a reference to the dictionary of the original TPDFIndirect object.
  // If that is destroyed, this object must also be destroyed.

  { TPDFDocumentInfo }

  TPDFDocumentInfo = class(TPDFObject)
  Private
    FSource : TPDFDictionary;
  Protected
    Function GetKeyName(aIndex : Integer) : RawByteString;
    Function GetString(aIndex : Integer) : String;
    Function GetDate(aIndex : Integer) : TDateTime;
    Function GetName(aIndex : integer) : String;
    Property Source : TPDFDictionary Read FSource;
  Public
    Class Function ElementType: TPDFElementType; override;
    Constructor Create(aSource : TPDFDictionary); reintroduce;
    // Keep Indexes unique, all indexes are passed through 1 routine to get the key name.
    Property Title : String Index 0 Read GetString;
    Property Author : String Index 1 Read GetString;
    Property Subject : String Index 2 Read GetString;
    Property Keywords : String Index 3 Read GetString;
    Property Creator : String Index 4 Read GetString;
    Property Producer : String index 5 Read GetString;
    Property CreationDate : TDateTime Index 6 Read GetDate;
    Property ModDate : TDateTime Index 7 Read GetDate;
    Property Trapped : String Index 8 Read GetName;
  end;


  { TPDFDocPagesEnumerator }

  TPDFDocPagesEnumerator = class
    FDoc: TPDFDocument;
    FCurrentIdx : Integer;
  public
    Constructor create(aDoc : TPDFDocument);
    function GetCurrent: TPDFPageObject;
    function MoveNext: Boolean;
    property Current: TPDFPageObject read GetCurrent;
  end;

  { TPDFPagesProxy }

  TPDFPagesProxy = class
    FDoc : TPDFDocument;
    Constructor Create(aDoc : TPDFDocument);
    function GetEnumerator : TPDFDocPagesEnumerator;
  end;


  { TPDFDocument }

  TPDFDocument = Class(TPDFContainer)
  private
    FPagesProxy: TPDFPagesProxy;
    FPDFVersion: String;
    FStartXref: TPDFStartXRef;
    FTrailerDict: TPDFDictionary;
    FCatalog : TPDFCatalogObject;
    FPages : TPDFIndirect;
    FXrefs : TPDFXRefArray;
    function GetPage(aIndex : Integer): TPDFPageObject;
    function GetPageNode(aIndex : Integer): TPDFIndirect;
    function GetPageCount: Integer;
    function GetStartXRef: TPDFStartXRef;
    function getXRef(aIndex : Integer): TPDFXRef;
    function GetXRefCount: Integer;
  Public
    Constructor Create; override;
    Destructor Destroy; override;
    Procedure SetXrefArray(aArray : TPDFXRefArray);
    // Find indirect object with given object ID and generation.
    Function FindInDirectObject(aID : Integer; aGeneration : Integer) : TPDFIndirect;
    Function FindInDirectObject(aRef : TPDFRef) : TPDFIndirect;
    // Returns Nil if aRef.IsEmpty=true
    Function FindInDirectObject(aRef : TPDFRefData) : TPDFIndirect;
    // Replace the object at given ref. Returns the old object
    Function ReplaceIndirectObj(aRef : TPDFRefData; aObject : TPDFIndirect) : TPDFIndirect;
    function ReplaceIndirectObj(aID: Integer; aGeneration: Integer; aObject : TPDFIndirect): TPDFIndirect;
    // Find font. Returns Nil if none found or object is not a font.
    Function FindFont(aRef : TPDFRef) : TPDFFontObject;
    Function FindFont(aRef : TPDFRefData) : TPDFFontObject;
    // Get font. Raises exception if not found or object is not a font.
    Function GetFont(aRef : TPDFRef) : TPDFFontObject;
    Function GetFont(aRef : TPDFRefData) : TPDFFontObject;
    // Find the document information object. May return Nil.
    Function FindDocumentInfoObject : TPDFIndirect;
    // Find document information, resolved to TPDFDocumentInfo. You must free this object yourself.
    Function FindDocumentInfo : TPDFDocumentInfo;
    // Get the document information object. Raises exception if not found. You must free this object yourself.
    Function GetDocumentInfo : TPDFDocumentInfo;
    //
    // Note: the following functions require parsing the document with ResolveObjects=True
    //
    // Find global catalog. Return nil if none found
    Function FindCatalog : TPDFCatalogObject;
    // Find global catalog. Raise exception if none found
    Function GetCatalog : TPDFCatalogObject;
    // Find pages object. Return nil if none found.
    Function FindPages : TPDFIndirect;
    // Find pages object. Raise exception if none found.
    Function GetPages : TPDFIndirect;
    // Will add to items only if it does not yet exist.
    Function AddInDirectObject(aObj : TPDFIndirect) : Boolean;
    // version in header line
    Property PDFversion: String Read FPDFVersion Write FPDFVersion;
    // StartXRef.
    Property StartXRef : TPDFStartXRef Read GetStartXRef Write FStartXref;
    // Trailer dictionary, set during parsing
    Property TrailerDict : TPDFDictionary Read FTrailerDict Write FTrailerDict;
    // Page count in this document
    Property PageCount : Integer Read GetPageCount;
    // Get the top-level indirect objects that represents a page tree node. 0 based.
    // Can be a pages node or a page object
    Property PageNodes[aIndex : Integer] : TPDFIndirect Read GetPageNode;
    // Get a page by index (0-based) (leaf in the page tree)
    Property Page[aIndex : Integer] : TPDFPageObject Read GetPage;
    // Enumerate the pages
    Property Pages : TPDFPagesProxy Read FPagesProxy;
    // Count of elements in XREfs
    Property XRefCount : Integer Read GetXRefCount;
    // Indexed access to XRefs
    Property XRefs[aIndex : Integer] : TPDFXRef Read getXRef;
  end;

implementation

Resourcestring
  SErrNotAnInteger = 'Token is not an integer';
  SErrNotAString = 'Token is not a string';
  SErrNotAName = 'Token is not a name';
  SErrNotAnInt64 = 'Token is not an int64';
  SErrNoSuchDictValue = 'No such dictionary value: %s';
  SErrDictValueIsNotInteger = 'Dictionary entry %s is not an integer';
  SErrDictValueIsNotArray = 'Dictionary entry %s is not an array';
  SErrDictValueIsNotDict = 'Dictionary entry %s is not a dictionary';
  SErrNoFontAt = 'No font found at: %s';

{ TPDFTrailer }


Function Canonicalize(const S : RawByteString) : RawByteString;

begin
  Result:=S;
end;

{ TPDFCommandEnumerator }

function TPDFCommandEnumerator.GetCurrent: TPDFCommand;
begin
  Result:=TPDFCommand(inherited GetCurrent);
end;

{ TPDFPagesProxy }

constructor TPDFPagesProxy.Create(aDoc: TPDFDocument);
begin
  FDoc:=aDoc;
end;

function TPDFPagesProxy.GetEnumerator: TPDFDocPagesEnumerator;
begin
  Result:=TPDFDocPagesEnumerator.Create(FDoc);
end;

{ TPDFDocPagesEnumerator }

constructor TPDFDocPagesEnumerator.create(aDoc: TPDFDocument);
begin
  FDoc:=aDoc;
  FCurrentIdx:=-1;
end;

function TPDFDocPagesEnumerator.GetCurrent: TPDFPageObject;
begin
  Result:=FDoc.Page[FCurrentIdx];
end;

function TPDFDocPagesEnumerator.MoveNext: Boolean;
begin
  Inc(FCurrentIdx);
  Result:=FCurrentIdx<FDoc.PageCount;
end;

{ TPDFNotDefRange }

function TPDFNotDefRange.Contains(aCode: Cardinal): Boolean;
begin
  Result:=(RangeStart<=aCode) and (aCode<=RangeEnd);
end;

{ TPDFCodeSpaceRange }

function TPDFCodeSpaceRange.Contains(aCode: Cardinal): Boolean;
begin
  Result:=(RangeStart<=aCode) and (aCode<=RangeEnd);
end;

{ TPDFCIDRange }

function TPDFCIDRange.Contains(aCode: Cardinal): Boolean;
begin
  Result:=(RangeStart<=aCode) and (aCode<=RangeEnd);
end;

{ TPDFCMapData }

function TPDFCMapData.IsValidCode (aCode : Cardinal) : Boolean;

Var
  I : Integer;

begin
  Result:=False;
  I:=Length(FCodeSpaceRange)-1;
  While not Result and (I>=0) do
    begin
    Result:=FCodeSpaceRange[i].Contains(aCode);
    Dec(I);
    end;
end;

function UInt32ToUnicodeChars(aChars : Cardinal) : UnicodeString;

Var
  First,Second : Word;

begin
  Second:=aChars and $FFFF;
  First:=(aChars shr 16) and $FFFF;
  if (First<>0) then
    Result:=UnicodeChar(First)
  else
    Result:='';
  Result:=Result+UnicodeChar(Second);
end;

function TPDFCMapData.GetNotDef(aCode : Integer) : UnicodeString;

Var
  Idx : Integer;

begin
  Idx:=Length(FNotDefRange)-1;
  While (Idx>=0) and not FNotDefRange[Idx].Contains(aCode) do
    Dec(Idx);
  if (Idx>=0) then
    Result:=UInt32ToUnicodeChars(FNotDefRange[Idx].ReplaceMent)
  else
    Result:='<?>'
end;


function TPDFCMapData.GetBFRangeChar(aCode : Cardinal; out aRes : Unicodestring) : Boolean;

var
  Idx,cOffset : integer;

begin
  aRes:='';
  Idx:=IndexInBFRange(aCode);
  Result:=Idx<>-1;
  if Result then
    begin
    With FCIDRange[Idx] do
      begin
      cOffset:=(aCode-RangeStart);
      if Length(CharNames)<>0 then
        begin
        if cOffset<Length(CharNames) then
          begin
          if CharNames[cOffset].Name='' then
            aRes:=UInt32ToUnicodeChars(CharNames[cOffset].UnicodeChar)
          else
            aRes:='?!';
          end
        else
          aRes:='<?>'
        end
      else
        aRes:=UInt32ToUnicodeChars(CharOffset+cOffset);
      end;
    end;
end;

function TPDFCMapData.GetBFChar(aCode: Cardinal; out aRes: Unicodestring
  ): Boolean;
var
  Idx : integer;

begin
  aRes:='';
  Idx:=IndexInBFChar(aCode);
  Result:=Idx<>-1;
  if Result then
    With BFChars[Idx] do
      if Length(DestName)<>0 then
        aRes:='XAXA' // Todo
      else
        aRes:=UInt32ToUnicodeChars(Dest);
end;

function TPDFCMapData.IndexInBFRange(aCode: Integer): Integer;

begin
  Result:=Length(FCIDRange)-1;
  While  (Result>=0) and not FCIDRange[Result].Contains(aCode) do
    Dec(Result);
end;

function TPDFCMapData.IndexInBFChar(aCode: Integer): Integer;

begin
  Result:=Length(FBFChars)-1;
  While  (Result>=0) and (FBFChars[Result].Src<>aCode) do
    Dec(Result);
end;

function TPDFCMapData.Interpret(aRaw: RawByteString): RawByteString;

Var
  aCode : Cardinal;
  I,Len : Integer;
  aResult : UnicodeString;
  C : UnicodeString;

begin
  aResult:='';
  I:=1;
  Len:=Length(aRaw);
  While (I<Len) do
    begin
    aCode:=(Ord(aRaw[i]) shl 8) +Ord(aRaw[i+1]);
    if not IsValidCode(aCode) then
      C:=GetNotDef(aCode)
    else if not GetBFRangeChar(aCode,C) then
      if not GetBFChar(aCode,C) then
        C:='?';
    aResult:=aResult+C;
    inc(I,2);
    end;
  Result:=UTF8Encode(aResult);

end;

{ TPDFCMap }

function TPDFCMap.GetCMAPName: String;
begin
  if Assigned(ObjectDict) then
    Result:=ObjectDict.GetStringValue(SCMAPKeyCMapName)
  else
    Result:='';
end;

function TPDFCMap.GetRegistry: String;
begin
  if Assigned(ObjectDict) then
    Result:=ObjectDict.GetStringValue(SPDFCIDSystemInfoKeyOrdering)
  else
    Result:='';
end;

function TPDFCMap.GetSupplement: Integer;
begin
  if Assigned(ObjectDict) then
    Result:=ObjectDict.GetIntegerValue(SPDFCIDSystemInfoKeySupplement)
  else
    Result:=0;
end;

procedure TPDFCMap.SetData(AValue: TPDFCMapData);
begin
  if FData=AValue then Exit;
  FData.Free;
  FData:=AValue;
end;

destructor TPDFCMap.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

function TPDFCMap.Interpret(aRaw: RawByteString): RawByteString;

begin
  Result:=Data.Interpret(aRaw);
end;

{ TPDFTextCommand }

function TPDFTextCommand.GetFullText(aUnicodeMap: TPDFCMap): RawByteString;

Var
  aRaw : RawByteString;

begin
  aRaw:=GetFullText();
  if not Assigned(aUnicodeMap) then
    Result:=aRaw
  else
    Result:=aUnicodeMap.InterPret(aRaw);
end;

{ TPDFRefData }

function TPDFRefData.IsEmpty: Boolean;
begin
  Result:=(ObjectGeneration=0) and (ObjectID=0);
end;

function TPDFRefData.AsString: String;
begin
  Result:=Format('%d %d',[ObjectID,ObjectGeneration]);
end;

{ TPDFFontObject }
function TPDFFontObject.GetValueName(AIndex: Integer): String;

begin
  Case aIndex of
    0 : Result:=SPDFFontKeyType;
    1 : Result:=SPDFFontKeySubType;
    2 : Result:=SPDFFontKeyName;
    3 : Result:=SPDFFontKeyBaseFont;
    4 : Result:=SPDFFontKeyEncoding;
    5 : Result:=SPDFFontKeyToUnicode;
  end;
end;

function TPDFFontObject.GetString(AIndex: Integer): String;

Var
  Obj : TPDFObject;

begin
  Obj:=ObjectDict.FindValue(GetValueName(aIndex));
  if Obj is TPDFValue then
    Result:=TPDFValue(Obj).Value
  else
    Result:='';
end;

function TPDFFontObject.GetToUnicode: TPDFRefData;

Var
  Ref : TPDFRef;

begin
  Ref:=GetToUnicodeObj;
  if Assigned(Ref) then
    Result:=Ref.FRef
  else
    Result:=Default(TPDFRefData);
end;

function TPDFFontObject.GetToUnicodeObj: TPDFRef;

Var
  Obj : TPDFObject;

begin
  Obj:=ObjectDict.FindValue(GetValueName(5));
  if Assigned(Obj) and (Obj is TPDFRef) then
    Result:=Obj as TPDFRef
  else
    Result:=Nil;
end;

class function TPDFFontObject.RegisterAsType: String;
begin
  Result:=SPDFTypeFont;
end;

destructor TPDFFontObject.Destroy;
begin
  FreeAndNil(FUnicodeCMAP);
  inherited Destroy;
end;

function TPDFFontObject.GetDescription: String;

  Procedure MaybeAdd(Const aName,aValue : String);

  begin
    if aValue<>'' then
      Result:=Result+sLineBreak+aName+': '+aValue;
  end;

begin
  Result:=Format('Font (%d %d):',[ObjectID,ObjectGeneration]);
  MaybeAdd('Name',Name);
  MaybeAdd('Subtype',SubType);
  MaybeAdd('Type',Type_);
  MaybeAdd('BaseFont',BaseFont);
  MaybeAdd('Encoding',Encoding);
  if Assigned(ToUnicode) then
    MaybeAdd('ToUnicode', ToUnicodeRef.AsString);
end;

function TPDFFontObject.ResolveToUnicodeMapStream(aDoc: TPDFDocument): TPDFIndirect;

Var
  Ref : TPDFRef;

begin
  Result:=Nil;
  Ref:=ToUnicode;
  if assigned(Ref) then
    Result:=aDoc.FindInDirectObject(Ref);
end;

{ TPDFImageDataCommand }

class procedure TPDFImageDataCommand.ParseImageOperands(
  aOperands: TPDFTokenArray; out aImageData: TPDFImageData);

Var
  I,J : Integer;


begin
  aImageData:=Default(TPDFImageData);
  I:=0;
  While (I<Length(aOperands)-1) do
    begin
    if aOperands[i].IsName then
      begin
      Case Copy(aOperands[i].TokenData,2,Length(aOperands[i].TokenData)-1) of
        SPDFImageKeyW :
          begin
          Inc(I);
          aImageData.Width:=aOperands[i].AsInteger;
          end;
        SPDFImageKeyH :
          begin
          Inc(I);
          aImageData.Height:=aOperands[i].AsInteger;
          end;
        SPDFImageKeyBPC:
          begin
          Inc(I);
          aImageData.BitsPerComponent:=aOperands[i].AsInteger;
          end;
        SPDFImageKeyCS:
          begin
          Inc(I);
          aImageData.ColorSpace:=aOperands[i].TokenData;
          end;
        SPDFImageKeyF:
          begin
          Inc(i);
          If aOperands[i].TokenType<>ptSquareOpen then
            begin
            Inc(i);
            aImageData.Filters:=[aOperands[i].TokenData];
            end
          else
            begin
            Inc(I);
            J:=I;
            While (J<Length(aOperands)) and (aOperands[J].TokenType<>ptSquareClose) do
              Inc(J);
            SetLength(aImageData.Filters,J);
            J:=I;
            While (J<Length(aOperands)) and (aOperands[J].TokenType<>ptSquareClose) do
              begin
              aImageData.Filters[J-I]:=aOperands[J].TokenData;
              Inc(J);
              end
            end;
          end;
      end;
      end;
    inc(I);
    end;
  Case Copy(aImageData.ColorSpace,2,Length(aImageData.ColorSpace)-1) of
    SPDFImageKeyCMYK : aImageData.ColorSpaceComponents:=4;
    SPDFImageKeyRGB : aImageData.ColorSpaceComponents:=3;
    SPDFImageKeyG : aImageData.ColorSpaceComponents:=1;
  end;
end;

class function TPDFImageDataCommand.RegisterCommandName: String;
begin
  Result:='ID';
end;

{ TPDFTd_Command }

class function TPDFTd_Command.RegisterCommandName: String;
begin
  Result:='Td';
end;

{ TPDFTj_Command }

class function TPDFTj_Command.RegisterCommandName: String;
begin
  Result:='Tj';
end;

function TPDFTj_Command.GetFullText: RawByteString;
begin
  Result:='';
  if Length(Self.Tokens)>0 then
    try
      Result:=Tokens[0].AsString;
    except
      on E : exception do
        begin
        Writeln('Exception ',E.ClassName,'getting text for token: "',E.Message,'". Token data :',GetDescription);
        Raise;
        end;

    end;
end;

{ TPDFTfCommand }

function TPDFTfCommand.GetFontName: String;
begin
  Result:='';
  If (Length(Tokens)>0) then
    if Tokens[0].IsString then
      Result:=Tokens[0].AsString
    else if Tokens[0].IsName then
      Result:=Tokens[0].AsName;
end;

function TPDFTfCommand.GetFontSize: Integer;

begin
  Result:=0;
  If (Length(Tokens)>1) and Tokens[1].IsInteger then
    Result:=Tokens[1].AsInteger
end;

class function TPDFTfCommand.RegisterCommandName: String;
begin
  Result:='Tf';
end;

{ TPDFTdCommand }

class function TPDFTdCommand.RegisterCommandName: String;
begin
  Result:='TD';
end;

{ TPDFTJCommand }

class function TPDFTJCommand.RegisterCommandName: String;
begin
  Result:='TJ';
end;

function TPDFTJCommand.GetFullText(aUnicodeMap: TPDFCMap): RawByteString;
Var
  i : integer;

begin
  if aUnicodeMap=Nil then
    Exit('');
  Result:='';
  if Length(Tokens)>=2 then
    For I:=1 to Length(Tokens)-2 do
      case Tokens[I].TokenType of
      ptString,ptHexString:
        Result:=Result+aUnicodeMap.InterPret(Tokens[I].TokenData);
      ptNumber:
        if Abs(Tokens[i].AsDouble)>PDFTextArraySpaceTreshold then
          Result:=Result+' ';
      else
        Raise EConvertError.Create('Unexpected char');
      end;
end;

function TPDFTJCommand.GetFullText: RawByteString;

Var
  i : integer;

begin
  Result:='';
  if Length(Tokens)>=2 then
    For I:=1 to Length(Tokens)-2 do
      begin
      if Tokens[I].TokenType=ptString then
        Result:=Result+Tokens[I].TokenData
      else if Tokens[i].IsNumber then
        begin
        if Abs(Tokens[i].AsDouble)>PDFTextArraySpaceTreshold then
          Result:=Result+' '
        end
      else
        Raise EConvertError.Create('Unexpected char');
      end;
end;

{ TPDFETCommand }

class function TPDFETCommand.RegisterCommandName: String;
begin
  Result:='ET';
end;

{ TPDFBTCommand }

class function TPDFBTCommand.RegisterCommandName: String;
begin
  Result:='BT';
end;

{ TPDFCommand }

class constructor TPDFCommand.Init;
begin
  _ClassList:=TFPDataHashTable.Create;
end;

class destructor TPDFCommand.Done;
begin
  FreeAndNil(_ClassList);
end;

class function TPDFCommand.ElementType: TPDFElementType;
begin
  Result:=peCommand;
end;

class function TPDFCommand.RegisterCommandName: String;
begin
  Result:='';
end;

class function TPDFCommand.FindClassForCommand(const aCommand: String
  ): TPDFCommandClass;
begin
  Result:=TPDFCommandClass(_ClassList.Items[aCommand]);
end;

class procedure TPDFCommand.RegisterCommand(const aCommand: String;
  aClass: TPDFCommandClass);
begin
  _ClassList.Add(aCommand,aClass)
end;

class procedure TPDFCommand.UnRegisterCommand(const aCommand: String);
begin
  _ClassList.Delete(aCommand);
end;

class procedure TPDFCommand.Register;

Var
  S : String;

begin
  S:=RegisterCommandName;
  If S<>'' then
    RegisterCommand(S,Self);
end;

class procedure TPDFCommand.UnRegister;
Var
  S : String;

begin
  S:=RegisterCommandName;
  If S<>'' then
    UnRegisterCommand(S);
end;

constructor TPDFCommand.Create(const aCommand: String; aTokens : TPDFTokenArray);
begin
  Inherited Create(aTokens);
  FCommand:=aCommand;
end;


{ TPDFCommandList }

function TPDFCommandList.GetCommand(aIndex : Integer): TPDFCommand;
begin
  Result:=Objects[aIndex] as TPDFCommand;
end;

function TPDFCommandList.GetEnumerator: TPDFCommandEnumerator;
begin
  Result:=TPDFCommandEnumerator.Create(Self);
end;


{ TPDFPagesObject }

function TPDFPagesObject.GetChildCount: Integer;

Var
  Value : TPDFObject;
  Kids : TPDFArray absolute Value;

begin
  Result:=0;
  if Not CheckObjectDict then
     exit;
  Value:=ObjectDict.FindValue(SPDFKeyKids);
  if Assigned(Value) and (Value is TPDFArray)  then
    Result:=Kids.Count div 3;
end;

function TPDFPagesObject.GetChildObject(aIndex : integer): TPDFIndirect;

Var
  Ref : TPDFRefData;

begin
  Result:=Nil;
  if Not CheckObjectDict then
     exit;
  Ref:=ChildRef[aIndex];
  Result:=Document.FindIndirectObject(Ref);
end;

function TPDFPagesObject.GetChildRef(aIndex : integer): TPDFRefData;

Var
  Value : TPDFObject;
  Idx : Integer;
  Kids : TPDFArray absolute Value;

begin
  Result:=Default(TPDFRefData);
  Idx:=aIndex * 3;
  Value:=ObjectDict.FindValue(SPDFKeyKids);
  if Assigned(Value) and (Value is TPDFArray)  then
    if Kids.IsIntegerAt(Idx) and  Kids.IsIntegerAt(Idx+1) and  Kids.IsKeywordAt(Idx+2,'R')  then
      begin
      Result.ObjectID:=Kids.GetIntegerAt(Idx);
      Result.ObjectGeneration:=Kids.GetIntegerAt(Idx+1);
      end;
end;

function TPDFPagesObject.GetPageCount: Integer;
begin
  Result:=ObjectDict.GetIntegerValue(SPDFKeyCount);
end;

class function TPDFPagesObject.RegisterAsType: String;
begin
  Result:=SPDFTypePages;
end;

function TPDFPagesObject.ParentRef: TPdfRef;

var
  Obj : TPDFObject;

begin
  Result:=nil;
  If Assigned(ObjectDict) then
    begin
    Obj:=ObjectDict.FindValue(SPDFKeyParent);
    if Obj is TPDFRef then
      Result:=TPDFRef(Obj);
    end;
end;

function TPDFPagesObject.Parent: TPDFIndirect;

Var
  Ref : TPDFRef;

begin
  Result:=Nil;
  Ref:=ParentRef;
  if (Ref<>Nil) and assigned(Document)  then
    Result:=Document.FindInDirectObject(Ref);
end;

function TPDFPagesObject.FindPage(aIndex: Integer): TPDFPageObject;
Var
  aCount : Integer;
  I,aOffset : integer;
  aNode : TPDFIndirect;
  aPages : TPDFPagesObject absolute aNode;
  aPage : TPDFPageObject absolute aNode;

begin
  Result:=nil;
  aOffset:=0;
  I:=0;
  aCount:=ChildCount;
  While (Result=Nil) and (I<aCount) do
    begin
    aNode:=ChildObject[I];
    if aNode is TPDFPagesObject then
      begin
      if (aOffset<=aIndex) and (aIndex<aOffset+aPages.PageCount) then
        Result:=aPages.FindPage(aIndex-aOffset)
      else
        Inc(aOffset,aPages.PageCount);
      end
    else if aNode is TPDFPageObject then
      if I=aIndex then
        Result:=aPage
      else
        inc(aOffset);
    inc(I);
    end;
end;

{ TPDFIndirectXRef }

class function TPDFIndirectXRef.RegisterAsType: String;
begin
  Result:=SPDFTypeXref;
end;

destructor TPDFIndirectXRef.destroy;
begin
  FreeAndNil(FXRef);
  inherited destroy;
end;

{ TPDFCatalogObject }

function TPDFCatalogObject.GetPages: TPDFPagesObject;

var
  aVal : TPDFObject;
  aPages : TPDFRef;

begin
  Result:=nil;
  aPages:=Nil;
  if not CheckObjectDict then
    Exit;
  aVal:=ObjectDict.FindValue(SPDFKeyPages);
  if aVal is TPDFRef then
    Result:=Document.FindInDirectObject(aPages) as TPDFPagesObject;
end;

class function TPDFCatalogObject.RegisterAsType: String;
begin
  Result:=SPDFTypeCatalog;
end;

{ TPDFObjectStreamObject }

class function TPDFObjectStreamObject.RegisterAsType: String;
begin
  Result:=SPDFTypeObjStm;
end;

{ TPDFPageObject }

{ TPDFContentStream }

constructor TPDFPageObject.Create;
begin
  inherited Create;
  FCommandList:=CreateCommandList;
end;

destructor TPDFPageObject.Destroy;
begin
  FreeAndNil(FCommandList);
  inherited Destroy;
end;

function TPDFPageObject.FindResources: TPDFDictionary;
Var
  Obj : TPDFObject;
  aParent : TPDFIndirect;

begin
  if FResources=Nil then
    begin
    Obj:=ObjectDict.FindValue(SPDFPageKeyResources);
    if Obj is TPDFRef then
      begin
      Obj:=Document.FindInDirectObject(TPDFRef(Obj));
      if Obj is TPDFIndirect then
        begin
        aParent:=TPDFIndirect(Obj);
        Obj:=TPDFIndirect(aParent).ObjectDict;
        if assigned(Obj) then
            Writeln('Indirect resource : ', TPDFDictionary(Obj).GetDescription)
        else
          Writeln('Indirect object ',aParent.ObjectID,'does not have a dict');
        end;
      end;
    if Obj is TPDFDictionary then
      FResources:=Obj as TPDFDictionary
    end;
  if (FResources=Nil) then
    begin
    aParent:=Parent;
    while (FResources=Nil) and (aParent<>Nil) do
      begin
      if assigned(aParent.ObjectDict) then
        begin
        Obj:=aParent.ObjectDict.FindValue(SPDFPageKeyResources);
        if Obj is TPDFDictionary then
         FResources:=Obj as TPDFDictionary;
        end;
      if FResources=Nil then
        begin
        if aParent is TPDFPagesObject then
          aParent:=TPDFPagesObject(aParent).Parent
        else
          aParent:=nil;
        end;
      end;
    end;
  Result:=Fresources;
end;

function TPDFPageObject.FindFontRefObj(const aFontName: String): TPDFRef;

var
  aDict  : TPDFDictionary;
  aFont : TPDFObject;

begin
  Result:=nil;
  aDict:=Resources.FindDictionaryValue(SPDFResourceKeyFont);
  if assigned(aDict) then
    begin
    aFont:=aDict.FindValue(aFontName);
    if (aFont is TPDFRef) then
      Result:=TPDFRef(aFont);
    end;
end;

function TPDFPageObject.FindFontRef(const aFontName: String): TPDFRefData;

var
  aRef : TPDFRef;

begin
  Result:=Default(TPDFRefData);
  aRef:=FindFontRefObj(aFontName);
  if Assigned(aRef) then
    Result:=aRef.FRef;
end;

class function TPDFPageObject.CreateCommandList: TPDFCommandList;
begin
  Result:=TPDFCommandList.Create;
end;


function TPDFPageObject.GetContentRef(aIndex : integer): TPDFrefData;

Var
  Cont : TPDFObject;
  ContArray : TPDFArray absolute cont;
  idx : integer;

begin
  Result:=Default(TPDFRefData);
  If aIndex>=ContentCount then
    Raise EListError.CreateFmt('Content index %d out of bounds [0..%d]',[aIndex,ContentCount-1]);
  Cont:=ObjectDict.FindValue(SPDFPageKeyContents);
  if (aIndex=0) and (Cont is TPDFRef) then
    Result:=(Cont as TPDFRef).FRef
  else if Cont is TPDFArray then
    begin
    Idx:=aIndex*3;
    if ContArray.IsIntegerAt(Idx) and  ContArray.IsIntegerAt(Idx+1) and  ContArray.IsKeywordAt(Idx+2,'R')  then
      begin
      Result.ObjectID:=ContArray.GetIntegerAt(Idx);
      Result.ObjectGeneration:=ContArray.GetIntegerAt(Idx+1);
      end;
    end;
end;

function TPDFPageObject.GetParent: TPDFIndirect;

begin
  Result:=Nil;
  if Not CheckObjectDict then
    exit;
  Result:=Document.FindInDirectObject(ParentRef);
end;

function TPDFPageObject.GetContent(aIndex : integer): TPDFIndirect;

Var
  Ref : TPDFRefData;

begin
  Result:=nil;
  if Not CheckObjectDict then
    exit;
  Ref:=ContentRef[aIndex];
  Result:=Document.FindInDirectObject(Ref)
end;

function TPDFPageObject.GetContentCount: Integer;

Var
  Cont: TPDFObject;

begin
  Result:=0;
  Cont:=ObjectDict.FindValue(SPDFPageKeyContents);
  if Assigned(Cont) then
    begin
    if (Cont is TPDFRef) then
      Result:=1
    else if Cont is TPDFArray then
      Result:=TPDFArray(cont).Count div 3;
    end;
end;

function TPDFPageObject.GetParentRef: TPDFRef;

Var
  aVal : TPDFObject;

begin
  Result:=Nil;
  If Not CheckObjectDict then
    Exit;
  aVal:=ObjectDict.FindValue(SPDFKeyParent);
  if assigned(aVal) and (aVal is TPDFRef) then
    Result:=TPDFRef(aVal);
end;

function TPDFPageObject.GetResources: TPDFDictionary;


begin
  Result:=FindResources;
  if Result=Nil then
    Raise EPDF.CreateFmt('No resource dictionary for page with ID %d',[Self.ObjectID]);
end;


class function TPDFPageObject.RegisterAsType: String;
begin
  Result:=SPDFTypePage;
end;

{ TPDFContainerObjectEnumerator }

constructor TPDFContainerObjectEnumerator.Create(aContainer: TPDFContainer);
begin
  FContainer:=aContainer;
  FCurrentIdx:=-1;
end;

function TPDFContainerObjectEnumerator.GetCurrent: TPDFObject;
begin
  Result:=FContainer.Objects[FCurrentIdx];
end;

function TPDFContainerObjectEnumerator.MoveNext: Boolean;
begin
  Inc(FCurrentIDX);
  Result:=FCurrentIDX<FContainer.Count;
end;

{ TPDFRef }

constructor TPDFRef.Create(aID, aVersion: Integer);
begin
  Inherited Create;
  FRef.ObjectID:=aID;
  FRef.ObjectGeneration:=aVersion;
end;

function TPDFRef.GetDescription: String;
begin
  Result:=Format('Ref (%d %d)',[ObjectID,ObjectGeneration]);
end;

class function TPDFRef.ElementType: TPDFElementType;
begin
  Result:=peRef;
end;

{ TPDFSingleObjectEnumerator }

constructor TPDFSingleObjectEnumerator.Create(aObj: TPDFObject);
begin
  FObject:=aObj;
  FFirst:=True;
end;

function TPDFSingleObjectEnumerator.GetCurrent: TPDFObject;
begin
  Result:=FObject;
end;

function TPDFSingleObjectEnumerator.MoveNext: Boolean;
begin
  Result:=FFirst;
  FFirst:=False;
end;

{ TPDFObjectEnumerator }

function TPDFObjectEnumerator.MoveNext: Boolean;
begin
  Result:=False;
end;

{ TPDFXRef }

class function TPDFXRef.ElementType: TPDFElementType;
begin
  Result:=peXREFItem;
end;

function TPDFXRef.Match(aObjectID, aObjectGeneration: Integer): Boolean;
begin
  Result:=InUse and (ReferenceIndex=aObjectID) and (aObjectGeneration=ObjectGeneration);
end;

function TPDFXRef.GetDescription: String;
begin
  Result:=Format('Xref object %d (InUse: %s',[ReferenceIndex,BoolToStr(InUse,'True','False')]);
  If Not InUse then
    Result:=Result+Format(', Next gen: %d)',[ObjectGeneration])
  else
    begin
    Result:=Result+Format(', Compressed: %s',[BoolToStr(Compressed,'True','False')]);
    if Not Compressed then
      Result:=Result+Format(', Offset: %d,  Generation: %d)',[ObjectOffset,ObjectGeneration])
    else
      Result:=Result+Format(', Stream obj: %d, Index : %d)',[StreamObjectNr,ObjectIndex]);
    end
end;

{ TPDFToken }

function TPDFToken.CheckString(const aValue : RawByteString): Boolean;
begin
  Result:=(TokenType=ptKeyword) and (aValue=TokenData);
end;

function TPDFToken.HasOnlyHexChars: Boolean;

var
  I,Len : Integer;

begin
  Result:=True;
  Len:=Length(TokenData);
  I:=1;
  While Result and (I<=Len) do
    begin
    Result:=TokenData[i] in ['0'..'9','A'..'F','a'..'f'];
    Inc(I);
    end;
end;


function TPDFToken.IsInteger: Boolean;

Var
  I : Integer;

begin
  Result:=(TokenType=ptNumber) and TryStrToInt(TokenData,I);
end;

function TPDFToken.IsHexInteger: Boolean;
begin
  Result:=(IsInteger) or ((TokenType=ptString) and (HasOnlyHexChars))
end;

function TPDFToken.AsHexInteger: Integer;
begin
  Result:=StrToInt('$'+TokenData);
end;


function TPDFToken.AsBEHexInteger: Integer;

Var
  I : integer;

begin
  if not (TokenType=ptHexString) then
    Raise EConvertError.Create('Not a hex string');
  Result:=0;
  for I:=1 to Length(TokenData) do
    Result:=(Result shl 8) + Ord(TokenData[i])
end;

function TPDFToken.AsInteger: Integer;
begin
  if not ((TokenType=ptNumber) and TryStrToInt(TokenData,Result)) then
    Raise EConvertError.Create(SErrNotAnInteger);
end;

function TPDFToken.AsDouble: Double;

var
  c : Integer;

begin
  if not (TokenType=ptNumber) then
    Raise EConvertError.Create(SErrNotAnInteger)
  else
    begin
    Val(TokenData,Result,C);
    if C<>0 then
      Raise EConvertError.Create(SErrNotAnInteger)
    end;

end;

function TPDFToken.IsHexString: Boolean;
begin
  Result:=(TokenType=ptHexString)
end;

function TPDFToken.IsString: Boolean;
begin
  Result:=(TokenType in [ptKeyword,ptHexString,ptString]);
end;

function TPDFToken.AsString: RawByteString;

Var
  I,Len : Integer;
  UString : Unicodestring;
//  P : PUnicodeChar;
  P : PWord;

begin
  if isString then
    begin
    If (Length(TokenData)>2) and (TokenData[1]=#254) and (TokenData[2]=#255) then
      begin
      Len:=Length(TokenData)-2;
      SetLength(UString{%H-},Len div 2);
      Move(TokenData[3],UString[1],Len);
      P:=PWord(PUnicodeChar(UString));
      For I:=1 to Length(UString) do
        begin
        P^:=Swap(P^);
        Inc(P);
        end;
      Result:=UTF8Encode(UString);
      end
    else
      Result:=TokenData
    end
  else
    Raise EConvertError.Create(SErrNotAString);
end;

function TPDFToken.AsName: RawByteString;
begin
  if IsName then
    Result:=TokenData
  else
    Raise EConvertError.Create(SErrNotAName);
end;

function TPDFToken.IsInt64: Boolean;

Var
  I : Int64;

begin
  Result:=(TokenType=ptNumber) and TryStrToInt64(TokenData,I);
end;

function TPDFToken.AsInt64: Int64;


begin
  if not ((TokenType=ptNumber) and TryStrToInt64(TokenData,Result)) then
    Raise EConvertError.Create(SErrNotAnInt64);

end;

function TPDFToken.IsWhiteSpace: Boolean;
begin
  Result:=(TokenType=ptWhiteSpace);
end;

function TPDFToken.IsKeyword: Boolean;
begin
  Result:=TokenType=ptKeyword;
end;

function TPDFToken.IsName: Boolean;
begin
  Result:=TokenType=ptName;
end;

function TPDFToken.IsNumber: Boolean;
begin
  Result:=TokenType = ptNumber;
end;

{ TPDFDocument }

function TPDFDocument.GetStartXRef: TPDFStartXRef;
begin
  Result:=FStartXref;
  if Result=Nil then
    Raise EPDF.Create('No StartXRef found');
end;

function TPDFDocument.getXRef(aIndex : Integer): TPDFXRef;
begin
  if (aIndex<0) or (aIndex>=Length(FXrefs)) then
    Raise EListError.CreateFmt(SListIndexError,[aIndex]);
  Result:=FXrefs[aIndex];
end;

function TPDFDocument.GetXRefCount: Integer;
begin
  Result:=Length(FXRefs);
end;

constructor TPDFDocument.Create;
begin
  inherited Create;
  FPagesProxy:=TPDFPagesProxy.Create(Self)
end;

destructor TPDFDocument.Destroy;
begin
  FreeAndNil(FPagesProxy);
  inherited Destroy;
end;

procedure TPDFDocument.SetXrefArray(aArray: TPDFXRefArray);
begin
  FXrefs:=aArray;
end;

function TPDFDocument.GetPageCount: Integer;


Var
  lPages : TPDFIndirect;

begin
  Result:=0;
  lPages:=GetPages;
  if Assigned(lPages) and Assigned(lPages.ObjectDict) then
     Result:=lPages.ObjectDict.GetIntegerValue(SPDFKeyCount);
end;

function TPDFDocument.GetPageNode(aIndex : Integer): TPDFIndirect;

Var
  lPages : TPDFIndirect;
  Value : TPDFObject;
  Idx : Integer;
  Kids : TPDFArray absolute Value;
  ObjID,ObjGen : Integer;

begin
  Result:=Nil;
  lPages:=GetPages;
  if Assigned(lPages) and Assigned(lPages.ObjectDict) then
    begin
    Value:=lPages.ObjectDict.FindValue('Kids');
    Idx:=aIndex * 3;
    if Assigned(Value) and (Value is TPDFArray) and (Idx<Kids.Count) then
      if Kids.IsIntegerAt(Idx) and  Kids.IsIntegerAt(Idx+1) and  Kids.IsKeywordAt(Idx+2,'R')  then
        begin
        ObjID:=Kids.GetIntegerAt(Idx);
        ObjGen:=Kids.GetIntegerAt(Idx+1);
        Result:=FindIndirectObject(ObjID,ObjGen);
      end;
    end;
end;

function TPDFDocument.GetPage(aIndex : Integer): TPDFPageObject;

Var
  aCount : Integer;
  I,aOffset : integer;
  aNode : TPDFIndirect;
  aPages : TPDFPagesObject absolute aNode;
  aPage : TPDFPageObject absolute aNode;

begin
  Result:=nil;
  aOffset:=0;
  I:=0;
  aCount:=PageCount;
  While (Result=Nil) and (I<aCount) do
    begin
    aNode:=PageNodes[I];
    if aNode is TPDFPagesObject then
      begin
      if (aOffset<=aIndex) and (aIndex<aOffset+aPages.PageCount) then
        Result:=aPages.FindPage(aIndex-aOffset)
      else
        Inc(aOffset,aPages.PageCount);
      end
    else if aNode is TPDFPageObject then
      if I=aIndex then
        Result:=aPage
      else
        inc(aOffset);
    inc(I);
    end;
end;

function TPDFDocument.FindInDirectObject(aID: Integer; aGeneration: Integer
  ): TPDFIndirect;

Var
  I,aMax : integer;

begin
  I:=0;
  aMax:=Count;
  Result:=Nil;
  While (Result=Nil) and (I<aMax) do
    begin
    if Objects[i].InheritsFrom(TPDFIndirect) then
      begin
      Result:=TPDFIndirect(Objects[i]);
      if Not Result.Match(aID,aGeneration) then
        Result:=Nil;
      end;
    Inc(I);
    end;
end;

function TPDFDocument.FindInDirectObject(aRef: TPDFRef): TPDFIndirect;
begin
  With aRef do
    Result:=FindInDirectObject(ObjectID,ObjectGeneration);
end;

function TPDFDocument.FindInDirectObject(aRef: TPDFRefData): TPDFIndirect;
begin
  if aRef.IsEmpty then
    Result:=Nil
  else
    Result:=FindIndirectObject(aRef.ObjectID,aRef.ObjectGeneration);
end;

function TPDFDocument.ReplaceIndirectObj(aRef: TPDFRefData;
  aObject: TPDFIndirect): TPDFIndirect;
begin
  Result:=ReplaceIndirectObj(aRef.ObjectID,aRef.ObjectGeneration,aObject);
end;

function TPDFDocument.ReplaceIndirectObj(aID: Integer; aGeneration: Integer;aObject: TPDFIndirect): TPDFIndirect;

Var
  I,aMax : integer;
  aObj: TPDFIndirect;

begin
  I:=0;
  aMax:=Count;
  Result:=Nil;
  While (Result=Nil) and (I<aMax) do
    begin
    if Objects[i].InheritsFrom(TPDFIndirect) then
      begin
      aObj:=TPDFIndirect(Objects[i]);
      if aObj.Match(aID,aGeneration) then
        Result:=Replace(I,aObject) as TPDFIndirect;
      end;
    Inc(I);
    end;
end;

function TPDFDocument.FindFont(aRef: TPDFRef): TPDFFontObject;
begin
  Result:=FindFont(aRef.Ref);
end;

function TPDFDocument.FindFont(aRef: TPDFRefData): TPDFFontObject;

Var
  aObj : TPDFIndirect;

begin
  aObj:=FindInDirectObject(aRef);
  if (aObj<>Nil) and Not (aObj is TPDFFontObject) then
    Result:=Nil
  else
    Result:=TPDFFontObject(aObj)
end;

function TPDFDocument.GetFont(aRef: TPDFRef): TPDFFontObject;
begin
  Result:=GetFont(aRef.Ref)
end;

function TPDFDocument.GetFont(aRef: TPDFRefData): TPDFFontObject;
begin
  Result:=FindFont(aRef);
  if Result=Nil then
    Raise EPDF.CreateFmt(SErrNoFontAt,[aRef.AsString]);
end;

function TPDFDocument.FindDocumentInfoObject: TPDFIndirect;

Var
  aVal : TPDFObject;
  Ref : TPDFRef absolute aVal;

begin
  Result:=Nil;
  if Not Assigned(TrailerDict) then exit;
  aVal:=TrailerDict.FindValue(SPDFKeyInfo);
  if Assigned(aVal) and (aVal is TPDFRef) then
    Result:=FindIndirectObject(Ref);
end;

function TPDFDocument.FindDocumentInfo: TPDFDocumentInfo;

Var
  Obj : TPDFIndirect; 

begin
  Result:=Nil;
  Obj:=FindDocumentInfoObject;
  if Assigned(Obj) and Assigned(Obj.ObjectDict) then
    Result:=TPDFDocumentInfo.Create(Obj.ObjectDict);
end;

function TPDFDocument.GetDocumentInfo: TPDFDocumentInfo;

begin
  Result:=FindDocumentInfo;
  if Result=Nil then
    Raise EPDF.Create('Document has no Document Information object');
end;

function TPDFDocument.FindCatalog: TPDFCatalogObject;
Var
  aVal : TPDFObject;
  aRoot : TPDFRef absolute aVal;

begin
  Result:=Nil;
  if (FCatalog=Nil) and  Assigned(TrailerDict) then
    begin
    aVal:=TrailerDict.FindValue(SPDFKeyRoot);
    if aVal is TPDFRef then
      FCatalog:=FindInDirectObject(aRoot) as TPDFCatalogObject;
    end;
  Result:=FCatalog;
end;

function TPDFDocument.GetCatalog: TPDFCatalogObject;

begin
  Result:=FindCatalog;
  if Result=Nil then
    Raise EPDF.Create('No catalog object found!');
end;

function TPDFDocument.FindPages: TPDFIndirect;

Var
  Cat : TPDFIndirect;
  aVal : TPDFObject;
  aPages : TPDFRef absolute aVal;

begin
  Result:=Nil;
  if (FPages=Nil) then
    begin
    Cat:=FindCatalog;
    if Assigned(Cat) then
      begin
      aVal:=Cat.ObjectDict.FindValue(SPDFKeyPages);
      if aVal is TPDFRef then
        FPages:=FindInDirectObject(aPages);
      end;
    end;
  Result:=FPages;
end;

function TPDFDocument.GetPages: TPDFIndirect;
begin
  Result:=FindPages;
  if Result=Nil then
    Raise EPDF.Create('No pages object found!');
end;

function TPDFDocument.AddInDirectObject(aObj: TPDFIndirect): Boolean;
begin
  Result:=FindIndirectObject(aObj.ObjectID,aObj.ObjectGeneration)=Nil;
  if Result then
    begin
    Add(aObj);
    aObj.FDocument:=Self;
    end;
end;

{ TPDFArray }

class function TPDFArray.ElementType: TPDFElementType;
begin
  Result:=peArray;
end;

function TPDFArray.GetDescription: String;

var
  Obj : TPDFObject;

begin
  Result:='Array [';
  For Obj in self do
    Result:=Result+' '+Obj.GetDescription;
  Result:=Result+']';
end;

function TPDFArray.IsIntegerAt(aIndex: Integer): Boolean;
begin
  Result:=(Objects[aIndex] is TPDFValue) and  (TPDFValue(Objects[aIndex]).IsInteger);
end;

function TPDFArray.IsKeywordAt(aIndex: Integer; const aKeyWord: RawByteString
  ): Boolean;
begin
  Result:=(Objects[aIndex] is TPDFValue) and  (TPDFValue(Objects[aIndex]).IsKeyword(aKeyword));
end;

function TPDFArray.GetIntegerAt(aIndex: Integer): Integer;
begin
  If (Objects[aIndex] is TPDFValue) and  (TPDFValue(Objects[aIndex]).IsInteger) then
    Result:=TPDFValue(Objects[aIndex]).AsInteger
  else
    Raise EConvertError.Create('Array element %d is not an integer value');
end;

{ TPDFStream }

constructor TPDFStream.Create(const aData: TBytes);
begin
  Inherited Create();
  FData:=aData;
end;

class function TPDFStream.ElementType: TPDFElementType;
begin
  Result:=peStream;
end;

{ TPDFValue }

function TPDFValue.GetAsInteger: Integer;
begin
  Result:=StrToInt(Value);
end;

function TPDFValue.GetAsBoolean: Boolean;
begin
  Result:=(Value='true');
  if not Result then
    if (Value<>'false') then
      Raise EConvertError.CreateFmt('Not a valid boolean value : %s',[Value]);
end;

function TPDFValue.GetAsDateTime: TDateTime;

Var
  aDate : String;
  Idx : Integer;
  Y,M,d,h,n,s : Word;

  Function GetNextPart(aLen : Integer) : Word; inline;
  begin
    Result:=StrToIntDef(Copy(aDate,Idx,aLen),0);
    Inc(Idx,aLen);
  end;

begin
  Result:=0;
  if Value='' then 
    exit;
  If not (Copy(Value,1,2)='D:') then
    Raise EConvertError.CreateFmt('Not a valid Date/Time value : %s',[Value]);
  aDate:=Value;
  Delete(aDate,1,2);
  idx:=1;
  Y:=GetNextPart(4);
  M:=GetNextPart(2);
  if M=0 then M:=1;
  D:=GetNextPart(2);
  if D=0 then D:=1;
  H:=GetNextPart(2);
  N:=GetNextPart(2);
  S:=GetNextPart(2);
  Result:=EncodeDate(Y,M,D)+EncodeTime(H,N,S,0); // Dropping the offset for the moment
end;

function TPDFValue.GetAsFloat: Double;

Var
  C : Integer;

begin
  Val(Value,Result,C);
  if C<>0 then
    Raise EConvertError.CreateFmt('Not a valid float value : %s',[Value]);
end;

function TPDFValue.GetAsInt64: Int64;
begin
  Result:=StrToInt64(Value);
end;

constructor TPDFValue.Create(aValue: RawByteString; aTokenType : TPDFTokenType);
begin
  Inherited Create();
  FValue:=aValue;
  FTokenType:=aTokenType;
end;

class function TPDFValue.ElementType: TPDFElementType;
begin
  Result:=peValue
end;

function TPDFValue.GetDescription: String;
begin
  Result:='Value ('+Value+')';
end;

function TPDFValue.IsKeyword(const aKeyWord: string): Boolean;
begin
  Result:=(Value=aKeyWord)
end;

function TPDFValue.IsInteger: Boolean;

Var
  I : Integer;

begin
  Result:=TryStrToInt(Value,I);
end;

function TPDFValue.IsInt64: Boolean;
Var
  I : Int64;

begin
  Result:=TryStrToInt64(Value,I);
end;

{ TPDFDictionary }

class function TPDFDictionary.ElementType: TPDFElementType;
begin
  Result:=peDictionary;
end;

function TPDFDictionary.GetDescription: String;

var
  I : Integer;
  E : TPDFDictEntry;

begin
  Result:='Dictionary: <<';
  For I:=0 to Count-1 do
    begin
    E:=Objects[i] as TPDFDictEntry;
    Result:=Result+sLineBreak+E.GetDescription;
    end;
  Result:=Result+sLineBreak+'>>';
end;

function TPDFDictionary.AddEntry(const aKey: String; aValue: TPDFObject): TPDFDictEntry;
begin
  Result:=TPDFDictEntry.Create();
  if (aKey<>'') and (aKey[1]='/') then
    Result.Key:=Copy(aKey,2,Length(aKey)-1)
  else
    Result.Key:=aKey;
  Result.Value:=aValue;
  AddEntry(Result);
end;

function TPDFDictionary.AddEntry(aEntry: TPDFDictEntry): Integer;
begin
  Result:=Add(aEntry);
end;

function TPDFDictionary.ContainsKey(const aKeyword: RawByteString): boolean;
begin
  Result:=IndexOfKey(aKeyword)<>-1;
end;

function TPDFDictionary.IndexofKey(const aKeyword: RawByteString): Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (TPDFDictEntry(Objects[Result]).Key<>aKeyWord) do Dec(Result);
end;

function TPDFDictionary.FindKey(const aKeyword: RawByteString): TPDFDictEntry;

Var
  Idx : Integer;

begin
  Result:=nil;
  Idx:=IndexOfKey(aKeyWord);
  if Idx<>-1 then
    Result:=Objects[Idx] as TPDFDictEntry
end;

function TPDFDictionary.GetKey(const aKeyword: RawByteString): TPDFDictEntry;

begin
  Result:=FindKey(aKeyword);
  if (Result=Nil) then
    Raise EPDF.CreateFmt('No such key in dictionary: %s',[aKeyWord]);
end;

function TPDFDictionary.FindValue(const aKeyword: RawByteString): TPDFObject;

Var
  Entry : TPDFDictEntry;

begin
  Result:=Nil;
  Entry:=FindKey(aKeyword);
  if Assigned(Entry) then
    Result:=Entry.Value;
end;

function TPDFDictionary.FindArrayValue(const aKeyword: RawByteString
  ): TPDFArray;
Var
  aVal : TPDFObject;

begin
  Result:=nil;
  aVal:=FindValue(aKeyWord) as TPDFObject;
  if aVal=Nil then exit;
  if not (aVal is TPDFArray) then
    Raise EPDF.CreateFmt(SErrDictValueIsNotArray,[aKeyWord]);
  Result:=TPDFArray(aVal);
end;

function TPDFDictionary.FindDictionaryValue(const aKeyword: RawByteString
  ): TPDFDictionary;

var
  aVal: TPDFObject;

begin
  Result:=Nil;
  aVal:=FindValue(aKeyWord);
  if aVal=Nil then
    exit;
  if not (aVal is TPDFDictionary) then
    Raise EPDF.CreateFmt(SErrDictValueIsNotDict,[aKeyWord]);
  Result:=TPDFDictionary(aVal);
end;

function TPDFDictionary.GetValue(const aKeyword: RawByteString): TPDFObject;

begin
  Result:=FindValue(aKeyWord);
  if (Result=Nil) then
    Raise EPDF.CreateFmt('No such value in dictionary: %s',[aKeyWord]);
end;

function TPDFDictionary.GetStringValue(const aKeyword: RawByteString): RawByteString;

Var
  aVal : TPDFValue;

begin
  aVal:=FindValue(aKeyWord) as TPDFValue;
  if (aVal=Nil) then
    Raise EPDF.CreateFmt(SErrNoSuchDictValue,[aKeyWord]);
  Result:=aVal.Value;
end;

function TPDFDictionary.GetInt64Value(const aKeyword: RawByteString): Int64;

Var
  aVal : TPDFValue;

begin
  aVal:=FindValue(aKeyWord) as TPDFValue;
  if (aVal=Nil) then
    Raise EPDF.CreateFmt(SErrNoSuchDictValue,[aKeyWord]);
  if not aVal.IsInt64 then
    Raise EPDF.CreateFmt(SErrDictValueIsNotInteger,[aKeyWord]);
  Result:=aVal.AsInt64;
end;

function TPDFDictionary.GetIntegerValue(const aKeyword: RawByteString): Integer;
Var
  aVal : TPDFValue;

begin
  aVal:=FindValue(aKeyWord) as TPDFValue;
  if (aVal=Nil) then
    Raise EPDF.CreateFmt(SErrNoSuchDictValue,[aKeyWord]);
  if not aVal.IsInteger then
    Raise EPDF.CreateFmt(SErrDictValueIsNotInteger,[aKeyWord]);
  Result:=aVal.AsInteger;
end;

function TPDFDictionary.GetArrayValue(const aKeyword: RawByteString): TPDFArray;

begin
  Result:=FindArrayValue(aKeyWord);
  if (Result=Nil) then
    Raise EPDF.CreateFmt(SErrNoSuchDictValue,[aKeyWord]);
end;

function TPDFDictionary.GetDictionaryValue(const aKeyword: RawByteString
  ): TPDFDictionary;

begin
  Result:=FindDictionaryValue(aKeyWord);
  if (Result=Nil) then
    Raise EPDF.CreateFmt(SErrNoSuchDictValue,[aKeyWord]);
end;

{ TPDFDictEntry }

class function TPDFDictEntry.ElementType: TPDFElementType;
begin
  Result:=peDictEntry;
end;

destructor TPDFDictEntry.Destroy;
begin
  FreeAndNil(FValue);
  inherited Destroy;
end;

function TPDFDictEntry.GetDescription: String;

begin
  if Assigned(Value) then
    Result:=Value.GetDescription
  else
    Result:='(nil)';
  Result:=Format('Entry "%s" : %s',[Key,Result]);
end;

{ TPDFTokensObject }

constructor TPDFTokensObject.Create(const aTokens: TPDFTokenArray);
begin
  Inherited Create();
  FTokens:=aTokens;
end;

{ TPDFContainer }

function TPDFContainer.GetCount: Integer;
begin
  Result:=FItems.Count;
end;

function TPDFContainer.GetObject(aIndex : integer): TPDFObject;
begin
  Result:=TPDFObject(FItems[aIndex]);
end;

procedure TPDFContainer.SetObject(aIndex : integer; AValue: TPDFObject);
begin
  FItems[aIndex]:=aValue;
end;

function TPDFContainer.Replace(aIndex: Integer; aObj: TPDFObject): TPDFObject;

Var
  OldOwns : Boolean;

begin
  OldOwns:=FItems.OwnsObjects;
  FItems.OwnsObjects:=False;
  try
    Result:=TPDFObject(FItems[aIndex]);
    FItems[aIndex]:=aObj;
  finally
    FItems.OwnsObjects:=OldOwns;
  end;
end;

constructor TPDFContainer.Create();
begin
  inherited Create();
  FItems:=TFPObjectList.Create(OwnsObjects);
end;

class function TPDFContainer.OwnsObjects: Boolean;
begin
  Result:=True;
end;

class function TPDFContainer.ElementType: TPDFElementType;
begin
  Result:=peContainer;
end;

destructor TPDFContainer.destroy;
begin
  FreeAndNil(FItems);
  inherited destroy;
end;

function TPDFContainer.GetEnumerator: TPDFObjectEnumerator;
begin
  Result:=TPDFContainerObjectEnumerator.Create(Self);
end;

function TPDFContainer.Add(aObject: TPDFObject): Integer;
begin
  Result:=FItems.Add(aObject);
end;

{ TPDFMalFormed }

class function TPDFMalFormed.ElementType: TPDFElementType;
begin
  Result:=peMalFormed
end;

{ TPDFStartXRef }

class function TPDFStartXRef.ElementType: TPDFElementType;
begin
  Result:=peStartXRef;
end;

{ TPDFIndirect }

function TPDFIndirect.GetObjectType: String;
begin
  if (FObjectType='') then
    if Assigned(ObjectDict) then
      begin
      if ObjectDict.ContainsKey('Type') then
        FObjectType:=ObjectDict.GetStringValue('Type');
      if (FObjectType<>'') and (FObjectType[1]='/') then
        Delete(FObjectType,1,1);
      end
{$IFDEF DEBUGOBJECTDICT}
    else
      Writeln('Warning : object ',ObjectID,' has no dictionary assigned');
{$ELSE}
      ;
{$ENDIF}
  Result:=FObjectType;
end;

function TPDFIndirect.CheckObjectDict: Boolean;
begin
  Result:=Assigned(ObjectDict);
{$IFDEF DEBUGOBJECTDICT}
  if Result=Nil then
    Writeln('No object dict for object ',ClassName,' : ',GetDescription);
{$ENDIF}
end;

function TPDFIndirect.FindDictValue(const aKey : RawbyteString) : TPDFObject;

begin
  Result:=Nil;
  if not CheckObjectDict then 
    exit;
  Result:=ObjectDict.FindValue(aKey);
end;

function TPDFIndirect.GetDictValue(const aKey : RawbyteString) : TPDFObject;

begin
  Result:=FindDictValue(aKey);
  if Result=Nil then
    Raise EPDF.CreateFmt('No such value in object dictionary: %s',[aKey]) 
end;

class constructor TPDFIndirect.InitIndirect;
begin
  _ClassList:=TFPDataHashTable.Create;
end;

class destructor TPDFIndirect.DoneIndirect;
begin
  _ClassList.Free;
end;

constructor TPDFIndirect.Create(aSource: TPDFIndirect);

Var
  L : TFPObjectList;

begin
  Create ;
  L:=aSource.FItems;
  aSource.FItems:=Self.FItems;
  Self.FItems:=L;
  FObjectType:=aSource.FObjectType;
  FDict:=aSource.FDict;
  FObjectID:=aSource.ObjectID;
  FObjectGeneration:= aSource.ObjectGeneration;
  FStream:=aSource.FStream;
  FUnfilteredStream:=aSource.FUnfilteredStream;
  FObjectPositions:=aSource.FObjectPositions;
end;

destructor TPDFIndirect.Destroy;
begin
  FreeAndNil(FunFilteredStream);
  inherited Destroy;
end;

function TPDFIndirect.ResolveObjectType: TPDFIndirect;

Var
  aType : String;
  aClass : TPDFIndirectClass;

begin
  aType:=ObjectType;
  If (aType='') then
    Result:=Self
  else
    begin
    aClass:=FindClassForType(aType);
    if aClass=Nil then
      Result:=Self
    else
      Result:=aClass.Create(Self);
    end;
end;

class function TPDFIndirect.FindClassForType(const aType: String): TPDFIndirectClass;
begin
  Result:=TPDFIndirectClass(_ClassList.Items[LowerCase(aType)]);
end;

class procedure TPDFIndirect.RegisterType(const aType: String;
  aClass: TPDFIndirectClass);
begin
  _ClassList.Add(LowerCase(aType),aClass);
end;

class procedure TPDFIndirect.UnRegisterType(const aType: String);
begin
  _ClassList.Delete(LowerCase(aType));
end;

class procedure TPDFIndirect.Register;

Var
  S : String;

begin
  S:=RegisterAsType;
  if S<>'' then
    RegisterType(S,Self);
end;

class function TPDFIndirect.RegisterAsType: String;
begin
  Result:='';
end;

class procedure TPDFIndirect.UnRegister;
Var
  S : String;

begin
  S:=RegisterAsType;
  if S<>'' then
    UnRegisterType(S);
end;

class function TPDFIndirect.ElementType: TPDFElementType;
begin
  Result:=peIndirectObject;
end;

function TPDFIndirect.GetDescription: String;
begin
  Result:=Format('Indirect object %d (Type: %s)',[ObjectID,ObjectType]);

end;

function TPDFIndirect.Add(aObject: TPDFObject): Integer;
begin
  Result:=inherited Add(aObject);
  if (FDict=Nil) and (aObject is TPDFDictionary) then
    FDict:=TPDFDictionary(aObject);
  if (FStream=Nil) and (aObject is TPDFStream) then
    FStream:=TPDFStream(aObject);
end;

function TPDFIndirect.Match(aObjectID: Integer; aObjectGeneration: Integer): Boolean;
begin
  Result:=(aObjectID=FObjectID) and (aObjectGeneration=FObjectGeneration);
end;

class function TPDFTrailer.ElementType: TPDFElementType;
begin
  Result:=peTrailer;
end;

function TPDFTrailer.Contains(const aKeyword: RawByteString): boolean;

begin
  Result:=IndexOfKey(aKeyword)<>0;
end;

{ TPDFXRefList }

function TPDFXRefList.GetItm(aIndex : integer): TPDFXRef;
begin
  Result:=Objects[aIndex] as TPDFXRef;
end;

procedure TPDFXRefList.SetItm(aIndex : integer; AValue: TPDFXRef);
begin
  Objects[aIndex]:=aValue;
end;

class function TPDFXRefList.ElementType: TPDFElementType;
begin
  Result:=peXREF;
end;

function TPDFXRefList.IndexOfReference(aObjectID, aObjectGeneration: Integer): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and not References[Result].Match(aObjectID,aObjectGeneration) do
    Dec(Result);
end;

function TPDFXRefList.FindReference(aObjectID, aObjectGeneration: Integer
  ): TPDFXRef;

var
  Idx : integer;

begin
  Idx:=IndexOfReference(aObjectID,aObjectGeneration);
  if Idx=-1 then
    Result:=Nil
  else
    Result:=References[Idx];
end;

{ TPDFObject }


{$IFDEF DEBUGPDFALLOCATION}
Var
  _Global : TFPObjectList;

destructor TPDFObject.destroy;
begin
  _Global.Remove(Self);
  inherited destroy;
end;
{$ENDIF}

constructor TPDFObject.Create();
begin
{$IFDEF DEBUGPDFALLOCATION}
  // Nothing
  if not assigned(_Global) then
    _Global:=TFPObjectList.Create(False);
  _Global.Add(Self);
{$ENDIF}
end;

function TPDFObject.GetEnumerator: TPDFObjectEnumerator;
begin
  Result:=TPDFSingleObjectEnumerator.Create(Self);
end;

function TPDFObject.GetDescription: String;
begin
  Result:=GetEnumName(TypeInfo(TPDFElementType),Ord(ElementType))
end;

{ TPDFComment }

constructor TPDFComment.Create(const aComment: RawByteString);
begin
  Inherited Create();
  FComment:=aComment;
end;

class function TPDFComment.ElementType: TPDFElementType;
begin
  Result:=peComment;
end;

{ TPDFDocumentInfo }

constructor TPDFDocumentInfo.Create(aSource: TPDFDictionary);

begin
  Inherited Create;
  FSource:=aSource;
end;

function TPDFDocumentInfo.GetKeyName(aIndex: Integer): RawByteString;

begin
  Result:='';
  Case aIndex of
    0 : Result:=SPDFKeyTitle;
    1 : Result:=SPDFKeyAuthor;
    2 : Result:=SPDFKeySubject;
    3 : Result:=SPDFKeyKeywords;
    4 : Result:=SPDFKeyCreator;
    5 : Result:=SPDFKeyProducer;
    6 : Result:=SPDFKeyCreationDate;
    7 : Result:=SPDFKeyModDate;
    8 : Result:=SPDFKeyTrapped;
  else
    Result:=''; // Silence compiler warning  
  end;    
  
end;

function TPDFDocumentInfo.GetString(aIndex: Integer): String;

Var
  Key : RawByteString;
  Obj : TPDFObject;
  Val : TPDFValue absolute Obj;
  
begin
  Result:='';
  Key:=GetKeyName(aindex);
  if Key<>'' then
    begin
    Obj:=FSource.FindValue(Key);
    if Obj is TPDFValue then
      Result:=Val.Value; // Conversion may need to be done here.
    end;
end;

function TPDFDocumentInfo.GetDate(aIndex: Integer): TDateTime;

 Var
  Key : RawByteString;
  Obj : TPDFObject;
  Val : TPDFValue absolute Obj;

begin
  Result:=0;
  Key:=GetKeyName(aindex);
  if Key<>'' then
    begin
    Obj:=FSource.FindValue(Key);
    if Obj is TPDFValue then
      Result:=Val.AsDateTime; 
    end;
end;

function TPDFDocumentInfo.GetName(aIndex: integer): String;

begin
  Result:=GetString(aIndex);
end;

class function TPDFDocumentInfo.ElementType: TPDFElementType;
begin
  Result:=peDocumentInfo;
end;

Procedure RegisterStandardClasses;

begin
  TPDFPageObject.Register;
  TPDFPagesObject.Register;
  TPDFCatalogObject.Register;
  TPDFObjectStreamObject.Register;
  TPDFIndirectXRef.Register;
  TPDFFontObject.Register;
end;

Procedure RegisterStandardCommands;

begin
  TPDFBTCommand.Register;
  TPDFETCommand.Register;
  TPDFTJCommand.Register;
  TPDFTj_Command.Register;
  TPDFTfCommand.Register;
  TPDFTd_Command.Register;
  TPDFTDCommand.Register;
end;

{$IFDEF DEBUGPDFALLOCATION}
Procedure DumpAllocations;

Var
  I : Integer;
begin
  if assigned (_Global) then
    begin
    For I:=0 to _Global.Count-1 do
      Writeln('Not freed: ',_Global[i].ClassName,' : ',TPDFObject(_Global[i]).GetDescription);
    FreeAndNil(_Global);
    end;
  Flush(output);
end;
{$ENDIF}

initialization
  RegisterStandardClasses;
  RegisterStandardCommands;

finalization
{$IFDEF DEBUGPDFALLOCATION}
  DumpAllocations
{$ENDIF}
end.

