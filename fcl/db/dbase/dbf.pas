unit dbf;
{===============================================================================
||         TDbf Component         ||         http://tdbf.netfirms.com         ||
===============================================================================}
interface

uses
{$ifdef fpc}
  SysUtils, Classes, db;
{$else}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db,  DsgnIntf, ExptIntf;
{$endif}  
// If you got a compilation error here or asking for dsgntf.pas, then just add
// this file in your project:
// dsgnintf.pas in 'C:\Program Files\Borland\Delphi5\Source\Toolsapi\dsgnintf.pas'

const
  _MAJOR_VERSION = 3;
  _MINOR_VERSION = 007;


{$ifdef VER100}      // Delphi 3
   {$define DELPHI_3}
{$endif}

{$ifdef VER110}      // CBuilder 3
   {$define DELPHI_3}
{$endif}

{$ifdef unix}
  DirSeparator = '/';
{$else}
  DirSeparator = '\';
{$endif}  

//====================================================================
// Delphi is a bit to permissive for me,  I mean protected doesn't work within
// one unit. So i decided that convention:
//    private member begins by '_'
// It's forbidden to access any '_something' except from the class where it
// is defined. To check that, I just have to look for '._' anywhere in the code.
//====================================================================
type

//====================================================================
//=== Common exceptions and constants
//====================================================================
  EBinaryDataSetError = class (Exception);
  EFieldToLongError = class (Exception);

  xBaseVersion = (xBaseIII,xBaseIV,xBaseV);

//====================================================================
//=== Utility classes
//====================================================================
  TPagedFile = class(TObject)
  protected
    Stream : TStream;
    HeaderSize : Integer;
    RecordSize : Integer;
    _cntuse:integer;
    _Filename:string;
  public
    constructor Create(const FileName: string; Mode: Word);
    destructor Destroy; override;

    procedure Release;
    function CalcRecordCount:Integer;
    procedure _Seek(page:Integer);
    procedure ReadRecord(IntRecNum:Integer;Buffer:Pointer);
    procedure WriteRecord(IntRecNum:Integer;Buffer:Pointer);
  end;
//====================================================================
//=== Dbf support (first part)
//====================================================================
  rDbfHdr = record
    VerDBF      : byte;   // 0
    Year        : byte;   // 1
    Month       : byte;   // 2
    Day         : byte;   // 3
    RecordCount : Integer;  // 4-7
    FullHdrSize : word;   // 8-9
    RecordSize  : word;   // 10-11
    Dummy1      : Word;   // 12-13
    IncTrans    : byte;   // 14
    Encrypt     : byte;   // 15
    Dummy2      : Integer; // 16-19
    Dummy3      : array[20..27] of byte; // 20-27
    MDXFlag     : char; // 28
    Language    : char; // 29
    dummy4      : word; // 30-31
  end;
//====================================================================
  TMyFieldInfo = class
  public
    FieldName:string;
    Size:Integer;
    Prec:Integer;
    Offset:Integer;
  end;
//====================================================================
  TDbfFile = class(TPagedFile)
  protected
    _RecordBufferSize:integer;
    _DataHdr : rDbfHdr;
    _DbfVersion : xBaseVersion;
    _MyFieldInfos: TList;
  public
    constructor Create(const FileName: string; Mode: Word);
    destructor Destroy; override;
    function RecordCount:integer;
    procedure CreateFieldDefs(FieldDefs:TFieldDefs);
    procedure ClearMyFieldInfos;
    procedure DbfFile_CreateTable(FieldDefs:TFieldDefs);
    procedure DbfFile_PackTable;
    function GetFieldInfo(FieldName:string):TMyFieldInfo;
    function GetFieldData(Column:Integer;DataType:TFieldType; Src,Dst: Pointer): Boolean;
    procedure SetFieldData(Column:integer;DataType:TFieldType; Src,Dst: Pointer);
    procedure WriteHeader;

  end;
//====================================================================
//=== Index support
//====================================================================
  TIndex = class;
//====================================================================
  rNdxHdr = record
    startpage : Integer; // 0..3
    nbPage : Integer; // 4..7
    keyformat: Char; //8
    keytype : char; //9
    dummy : Word; // 10..11
    keylen : Word; // 12..13
    nbkey : Word; // 14..15
    skeytype : Word; // 16..17
    keyreclen : Word; // 18..19
    dummy2 : Word; // 20..21
    dummy3 : Byte; // 22
    Unique : Byte; // 23
    KeyDesc : array[0..255] of char; // 24...
  end;

  rMdxTag = record
    pageno      : Integer; // 0..3
    tagname      : array [0..11] of char; // 4..14
    keyformat    : byte; // 15
    forwardTag1  : char; // 16
    forwardTag2 : byte; // 17
    backwardTag : byte; // 18
    dummy       : byte; // 19
    keytype     : byte; // 20
  end;

  NdxKeyType = (N,C);
  PNdxPage  = ^rNdxPage;
  rNdxPage  = record
    NbEntries : longint;  //  0..3 lower page
    Entries   : ARRAY [0..507] OF char;
  end;

  PNdxentry  = ^rNdxentry;
  rNdxentry  = record
    _LowerPage : longint;  //  0..3 lower page
    RecNo     : Longint;  //  4..7 recno
    case NdxKeyType of
      N: ( NKey: double);
      C: ( CKey: array [0..503] of char);
  end;
//====================================================================
  rMdxHdr = record
    MdxHdr   : byte;       // 0
    Year        : byte;       // 1
    Month       : byte;       // 2
    Day         : byte;      // 3
    FileName    : array[0..15] of char; // 4..19 of byte
    BlockSize    : word; // 20 21
    BlockAdder  : word; // 22 23
    IndexFlag   : byte; // 24
    NoTag       : byte; // 25
    TagSize     : byte; // 26
    Dummy1      : byte; // 27
    TagUsed     : word; // 28..29
    Dummy2      : word; // 30..31
    NbPage      : Integer; // 32..35
    FreePage    : Integer; // 36..39
    BlockFree   : Integer; // 40..43
    UpdYear     : byte; // 44
    UpdMonth    : byte; // 45
    UpdDay      : byte; // 46
  end;
//====================================================================
  TIndexFile = class(TPagedFile)
  protected
    _IndexVersion : xBaseVersion;
    _MdxHdr : rMdxHdr;
  public
    constructor Create(const FileName: string; Mode: Word);
    destructor Destroy; override;
  end;
//====================================================================
  PIndexPosInfo = ^TIndexPage;
  TIndexPage = class
  protected
    _Index : TIndex;
    _PageNo : Integer;
    _EntryNo : Integer;
    Entry : PNdxentry;
    _LowerLevel : TIndexPage;
    _UpperLevel : TIndexPage;
    _PageBuff:rNdxPage;

    procedure LocalFirst;
    procedure LocalLast;
    function  LocalPrev:boolean;
    function  LocalNext:boolean;
    function  LastEntryNo:integer;
    function  LocalInsert(Recno:Integer; Buffer:PChar; LowerPage:integer):boolean;
    function  LocalDelete:boolean;

    function  GetPEntry(EntryNo:integer):PNdxEntry;
    procedure First;
    procedure Last;
    function Prev:boolean;
    function Next:boolean;
    procedure Write;
    procedure AddNewLevel;
  public
    constructor Create(Parent:TIndex);
    destructor Destroy; override;

    procedure SetPageNo(page:Integer);
    procedure SetEntryNo(entryno:Integer);
    procedure WritePage(Page:integer);
    function FindNearest(Recno:integer; Key:PChar):integer;
    function Insert(Recno:integer; Buffer:pchar; LowerPage:integer):boolean;
    procedure SetEntry(Recno:integer; key:pchar; LowerPage:integer);
    function Delete:boolean;
    function LowerLevel : TIndexPage;
  end;
//====================================================================
  TIndex = class(TObject)
  protected
    _IndexFile:TIndexFile;
    _NdxHdr:rNdxHdr;
    _Root:TIndexPage;
    _TagPosition:Integer;
    _FieldPos : integer;
    _FieldLen : integer;
    _NbLevel : integer;
    _RootPage: integer;

    function Pos:TIndexPage;
  public
    IndexRecNo:integer;
    function Prev:boolean;
    function Next:boolean;
    procedure First;
    procedure Last;
    function Find(Recno:integer; Buffer:PChar; var pPos:TIndexPage):integer;
    procedure Insert(Recno:integer; Buffer:PChar);
    function Delete:boolean;
    procedure GotoKey(Recno:integer; Buffer:PChar);
    procedure Update(Recno: integer; PrevBuffer,NewBuffer: PChar);
//    procedure ResyncInd;
    function GetRealRecNo: Integer;
    constructor Create(Parent:TIndexFile; RootPage:integer;CreateIt:boolean);
    procedure InitFieldDef(dbfFile:TDbfFile;FieldDesc:string);
    destructor Destroy; override;
// optionnal
    function GuessRecordCount: Integer;
    function GuessRecNo: Integer;
  end;
//====================================================================
//=== Memo and binary fields support
//====================================================================
  rDbtHdr = record
    NextBlock:Longint;
    Dummy : array [4..7] of byte;
    _dbfFile : array [0..7] of Byte; //8..15
    bVer : Byte; //16
    Dummy2 : array [17..19] of byte;
    BlockLen:  Word;
  end;
//====================================================================
  TDbtFile = class(TPagedFile)
  protected
    _DbtVersion:xBaseVersion;
    _MemoHdr:rDbtHdr;
  public
    constructor Create(const FileName: string; Mode: Word; Ver:xBaseVersion);
    procedure ReadMemo(recno:Integer;Dst:TStream);
    procedure WriteMemo(var MemoRecno:Integer;ReadSize:Integer;Src:TStream);
  end;
//====================================================================
  TMyBlobFile = class(TMemoryStream)
  public
    Mode: TBlobStreamMode;
    Field:TField;
    MemoRecno:Integer;
    ReadSize:Integer;
    constructor Create(ModeVal:TBlobStreamMode; FieldVal:TField);
    destructor destroy;  override;
  end;
//====================================================================
//=== Dbf support 2
//====================================================================
  rFieldHdrIII = record
    FieldName   : array[0..10] of char;
    FieldType   : char; // 11
    Dummy        : array[12..15] of byte;
    FieldSize   : byte; // 16
    FieldPrecision  : byte; //17
    dummy2      : array[18..31] of byte;
  end;
//====================================================================
  rFieldHdrV = record
    FieldName   : array[0..10] of char;
    Dummy0        : array[11..31] of byte;
    FieldType   : char; // 32
    FieldSize   : byte; // 33
    FieldPrecision  : byte; //34
    dummy2      : array[35..47] of byte;
  end;
//====================================================================
  PBookMarkData = ^rBookMarkData;
  rBookmarkData = record
    RecNo:longint;
  end;
//====================================================================
  rBeforeRecord = record
    BookmarkData: rBookmarkData;
    BookmarkFlag: TBookmarkFlag;
    //... record come here
  end;
//====================================================================
  pDbfRecord = ^rDbfRecord;
  rDbfRecord = record
    BookmarkData: rBookmarkData;
    BookmarkFlag: TBookmarkFlag;
    DeletedFlag : char;
    Fields : array[0..4000] of char;
  end;
//====================================================================
  PRecInfo = ^TRecInfo;
  TRecInfo = record
    Bookmark: Longint;
    IdxBookmark: Longint;
    BookmarkFlag: TBookmarkFlag;
  end;
//====================================================================
  pRecordHdr = ^tRecordHdr;
  tRecordHdr = record
    DeletedFlag : char;
  end;

// and at LEAST the most useful class : TDbf
//====================================================================
  TDbf = class(TDataSet)
  private
    _ShowDeleted:boolean;
    _TableName: string;    // table path and file name
    _RunTimePath: string;    // table path and file name
    _DesignTimePath: string;    // table path and file name
    _ReadOnly : Boolean;
    _FilterBuffer:pchar;
    _PrevBuffer:pchar;
    _IndexFiles:TStrings;
  protected
    function _FullRecordSize:integer;
    function _FilterRecord(Buffer: PChar): Boolean;
    procedure _OpenFiles(CreateIt:boolean);
    procedure _CloseFiles;
    procedure _ResyncIndexes(Buffer: PChar);
    function _GetIndexName: string;
    procedure _SetIndexName(const Value: string);
    function _GetIndex(filename:string):TIndex;
    function _GetPath:string;
    function _ComponentInfo:string;
  public
    { my own methods and properties}
    { most looks like ttable functions but they are not tdataset related
     I use the same syntax to facilitate the conversion between bde and tdbf  }
    easyfilter:string;
    procedure CreateTable; //(FieldDefs:TFieldDefs);
    procedure DeleteIndex(const AName: string);
    property IndexName: string read _GetIndexName write _SetIndexName;

{$ifdef DELPHI_3}
    procedure AddIndex(const IndexName, Fields: String; Options: TIndexOptions);
{$else}
{$ifndef FPC}
    procedure AddIndex(const IndexName, Fields: String; Options: TIndexOptions; const DescFields: String='');
{$else}
    procedure AddIndex(const AnIndexName, IndexFields: String; Options: TIndexOptions);
    procedure AddIndex(const AnIndexName, IndexFields: String; Options: TIndexOptions; const DescFields: String);
 
{$endif}    
{$endif}
    procedure CloseIndexFile(const IndexFileName: string);
    procedure OpenIndexFile(AnIndexName:string);
    procedure PackTable;
  public
    { abstract methods }
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override; {virtual abstract}
    {virtual methods (mostly optionnal) }
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override; {virtual}
{$ifdef DELPHI_3}
    procedure Translate(Src, Dest: PChar; ToOem: Boolean); override; {virtual}
{$else}
{$ifdef fpc}
    procedure Translate(Src, Dest: PChar; ToOem: Boolean); override; {virtual}
{$else}
    function Translate(Src, Dest: PChar; ToOem: Boolean): Integer; override; {virtual}
{$endif}  
{$endif}
    procedure ClearCalcFields(Buffer : PChar); override;
  protected
    { abstract methods }
    function AllocRecordBuffer: PChar; override; {virtual abstract}
    procedure FreeRecordBuffer(var Buffer: PChar); override; {virtual abstract}
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override; {virtual abstract}
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override; {virtual abstract}
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override; {virtual abstract}
    function GetRecordSize: Word; override; {virtual abstract}
    procedure InternalAddRecord(Buffer: Pointer; DoAppend: Boolean); override; {virtual abstract}
    procedure InternalClose; override; {virtual abstract}
    procedure InternalDelete; override; {virtual abstract}
    procedure InternalFirst; override; {virtual abstract}
    procedure InternalGotoBookmark(ABookmark: Pointer); override; {virtual abstract}
    procedure InternalHandleException; override; {virtual abstract}
    procedure InternalInitFieldDefs; override; {virtual abstract}
    procedure InternalInitRecord(Buffer: PChar); override; {virtual abstract}
    procedure InternalLast; override; {virtual abstract}
    procedure InternalOpen; override; {virtual abstract}
    procedure InternalPost; override; {virtual abstract}
    procedure InternalSetToRecord(Buffer: PChar); override; {virtual abstract}
    function IsCursorOpen: Boolean; override; {virtual abstract}
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override; {virtual abstract}
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override; {virtual abstract}
    procedure SetFieldData(Field: TField; Buffer: Pointer); override; {virtual abstract}
    {virtual methods (mostly optionnal) }

    function GetRecordCount: Integer; override; {virtual}
    function GetRecNo: Integer; override; {virtual}
    procedure SetRecNo(Value: Integer); override; {virual}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ComponentInfo: string  read _ComponentInfo;
    property TableName: string  read _TableName write _TableName;
    property RunTimePath: string  read _RunTimePath write _RunTimePath;
    property DesignTimePath: string  read _DesignTimePath write _DesignTimePath;
    property ReadOnly : Boolean read _ReadOnly write _Readonly default False;
    property ShowDeleted:boolean read _ShowDeleted write _ShowDeleted;
    // redeclared data set properties
    property Active;
    property Filtered;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;

//my datas....
  protected
    _IsCursorOpen:boolean;
    _PhysicalRecno:integer;
    _CurIndex: TIndex;
    _Indexes:TList;      // index
    _indexFile : TIndexFile;
    _dbtFile : TDbtFile;
  public
    _dbfFile:TDbfFile;
    property PhysicalRecno:integer read _PhysicalRecno;
    function _RecordDataSize:integer;
  end;

{$ifndef fpc}
procedure Register;
{$endif}

var
  tDbf_TrimFields : boolean;

implementation

var
    _PagedFiles : TList;

//====================================================================
// Some types and consts which are not useful in the interface.
//====================================================================
(*
 * tSmallint  16 bits = -32768 to 32767
 *                      123456 = 6 digit max
 * ftInteger  32 bits = -2147483648 to 2147483647
 *                      12345678901 = 11 digits max
 * ftLargeInt 64 bits = -9223372036854775808 to 9223372036854775807
 *                      12345678901234567890 = 20 digits max
 *)
const
  DIGITS_SMALLINT = 6;
  DIGITS_INTEGER = 11;
  DIGITS_LARGEINT = 20;
  sDBF_DEC_SEP= '.';

type
  rAfterHdrIII = record // Empty
  end;

  rAfterHdrV = record
    Dummy   : array[32..67] of byte;
  end;

  PMdxTag = ^rMdxTag;

  rMdxTagHdr = record
    RootPage        : longint;// 0..3
    FilePages        : longint;// 4..7
    KeyFormat        : byte;   // 8
    KeyType          : char;   // 9
    dummy            : word;   // 10..11
    IndexKeyLength   : word;   // 12..13
    MaxNbKeys       : word;   // 14..15
    SecondKeyType   : word;   // 16..17
    IndexKeyItemLen  : word;   // 18..19
    dummy2           : array [20..22] of byte;
    UniqueFlag      : byte;   // 23
  end;


  rAfterHdrV3 = record
    Dummy   : array[12..31] of byte;
  end;

  rAfterHdrV4 = record
    Dummy   : array[12..67] of byte;
  end;

  rFieldHdrV3 = record
    FieldName   : array[0..10] of char;
    FieldType   : char; // 11
    Dummy        : array[12..15] of byte;
    FieldSize   : byte; // 16
    FieldPrecision  : byte; //17
    dummy2      : array[18..31] of byte;
  end;

  rFieldHdrV4 = record
    FieldName   : array[0..10] of char;
    Dummy0        : array[11..31] of byte;
    FieldType   : char; // 32
    FieldSize   : byte; // 33
    FieldPrecision  : byte; //34
    dummy2      : array[35..47] of byte;
  end;
  PDouble = ^double;
//====================================================================
// Now some common functions and procedure
//====================================================================
// ****************************************************************************
// International separator
// thanks to Bruno Depero from Italy
// and Andreas Wöllenstein from Denmark

function DbfStrToFloat(s: string): Extended;
var iPos: integer;
     eValue: extended;
begin
    iPos:= Pos(sDBF_DEC_SEP, s);
    if iPos> 0 then
      s[iPos]:= DecimalSeparator;
{$ifndef fpc}
    if TextToFloat(pchar(s), eValue, fvExtended) then
{$else}
                Val(s,eValue,Ipos);
                If Ipos=0 then
{$endif}
      Result:= eValue
    else Result:= 0;
end;

function FloatToDbfStr(f: Extended; size, prec: integer): string;
var iPos: integer;
begin
    Result:= FloatToStrF(f, ffFixed, Size, prec);
    iPos:= Pos(DecimalSeparator, Result);
    if iPos> 0 then
      Result[iPos]:= sDBF_DEC_SEP;
end;

procedure MyMove(Source, Dest:PChar; Count: Integer);
var
  c:char;
  i:integer;
begin
  i:=0;
  while i<Count do begin
    c:=PChar(Source)[i];
    if c=#0 then break;
    PChar(Dest)[i]:=c;
    Inc(i);
  end;
  while i<Count do begin
    PChar(Dest)[i]:=' ';
    Inc(i);
  end;
end;
//====================================================================
// TPagedFile
//====================================================================
function GetPagedFile(FileName: string):TPagedFile;
var
  idx:integer;
  idf:TPagedFile;
begin
  FileName:=LowerCase(FileName);
  for idx:=0 to _PagedFiles.Count-1 do begin
    idf:= TPagedFile(_PagedFiles[idx]);
    if idf._FileName=FileName then begin
      result:=idf;
      exit;
    end;
  end;
  result:=nil;
end;

procedure TPagedFile.Release;
begin
  dec(_cntuse);
  if _cntuse<=0 then begin
    _PagedFiles.Delete(_PagedFiles.IndexOf(self));
    Free;
  end;
end;

function TPagedFile.CalcRecordCount:Integer;
begin
  if RecordSize = 0 then Result:=0
  else Result:=(Stream.Size - HeaderSize) div RecordSize;
end;

constructor TPagedFile.Create(const FileName: string; Mode: Word);
begin
  if filename='' then Stream:=TMemoryStream.Create()
  else begin
    Stream:=TFileStream.Create(FileName,Mode);
  end;
  HeaderSize:=0;
  RecordSize:=0;
  _cntuse:=0;
  _filename:=lowercase(filename);
  _PagedFiles.Add(Self);
end;

destructor TPagedFile.Destroy;
begin
  Stream.Free;
  Stream:=nil;
  inherited;
end;

procedure TPagedFile._Seek(page:Integer);
var
  p:Integer;
begin
  p:=HeaderSize + (RecordSize * page );
  Stream.Position := p;
end;

Procedure TPagedFile.ReadRecord(IntRecNum:Integer; Buffer:Pointer);
begin
  _Seek(IntRecNum);
  Stream.Read(Buffer^,RecordSize);
end;

procedure TPagedFile.WriteRecord(IntRecNum:Integer; Buffer:Pointer);
begin
  _Seek(IntRecNum);
  Stream.Write(Buffer^, RecordSize);
end;

//====================================================================
// TDbfFile
//====================================================================
constructor TDbfFile.Create(const FileName: string; Mode: Word);
var
  lRecordCount:Integer;
begin
  _MyFieldInfos:=TList.Create;
  // check if the file exists
  inherited Create(Filename, Mode);


  if Mode = fmCreate then begin
    FillChar(_DataHdr,sizeof(_DataHdr),0);
    HeaderSize:=0;
    RecordSize:=0;
    _DataHdr.VerDBF:=$03; // Default version xBaseIV without memo
    _DataHdr.Language:='X';
  end else begin
    Stream.Seek(0,soFromBeginning);
    Stream.ReadBuffer (_DataHdr, SizeOf(_DataHdr));
    case _DataHdr.VerDBF of
    $03,$83:          _DbfVersion:=xBaseIII;
    $04,$8B,$8E,$7B:  _DbfVersion:=xBaseIV;
    $05 :             _DbfVersion:=xbaseV;
    else
      _DbfVersion:=xBaseIV; // My favorite...
    end;
    HeaderSize:=_DataHdr.FullHdrSize;
    RecordSize:=_DataHdr.RecordSize;
    lRecordCount:=CalcRecordCount;
    if _DataHdr.RecordCount <> lRecordCount then begin
{$ifndef fpc}    
      ShowMessage('Invalid Record Count,'+^M+
      'RecordCount in Hdr : '+IntToStr(_DataHdr.RecordCount)+^M+
      'expected : '+IntToStr(lRecordCount));
{$endif}      
      _DataHdr.RecordCount := lRecordCount;
    end;
  end;

end;


destructor TDbfFile.Destroy;
begin
  inherited;
  ClearMyFieldInfos;
  _MyFieldInfos.Free;
  _MyFieldInfos:=nil;

end;

function TDbfFile.RecordCount:integer;
begin
  if RecordSize=0 then result:=0
  else result:=(Stream.Size - HeaderSize) div RecordSize;
  if result<0 then result:=0;
end;

procedure TDbfFile.ClearMyFieldInfos;
var
  i:Integer;
begin
  for i:=0 to _MyFieldInfos.Count-1 do begin
    TMyFieldInfo(_MyFieldInfos.Items[i]).Free;
  end;
  _MyFieldInfos.Clear;
end;

procedure TDbfFile.CreateFieldDefs(FieldDefs:TFieldDefs);
var
  lColumnCount,lHeaderSize,lFieldSize:Integer;
  Il : Integer;
  lFieldOffset : Integer;
  fn:string;
  ft:TFieldType;
  fs,nfs,fd:Integer;
  MyFieldInfo:TMyFieldInfo;
  lFieldHdrIII:rFieldHdrIII;
  lFieldHdrV:rFieldHdrV;

  function ToFieldType(dbasetype:char;fs,fd:Integer):TFieldType;
  begin
    case dbasetype of
    'C' :
      begin
        Result:=ftString;
      end;
    'L' :
      begin
        Result:=ftBoolean;
      end;
    'F' :
      begin
        Result:=ftFloat;
      end;
    'N' :
      begin
        if fd=0 then begin
          if fs <= DIGITS_SMALLINT then begin
            Result:=ftSmallInt;
          end else begin
{$ifdef DELPHI_3}
            Result:=ftInteger;
{$else}
            if fs <= DIGITS_INTEGER then Result:=ftInteger
            else Result:=ftLargeInt;
{$endif}
          end;
        end else begin
          Result:=ftFloat;
        end;
      end;
    'D' :
      begin
        Result:=ftDate;
      end;
    'M' :
      begin
        Result:=ftMemo;
      end;
    else
      begin
        Result:=ftString;
      end;
    end; //case
  end;
begin
  ClearMyFieldInfos;

  if _DbfVersion>=xBaseV then begin
    lHeaderSize:=SizeOf(rAfterHdrV) + SizeOf(rDbfHdr);
    lFieldSize:=SizeOf(rFieldHdrV);
  end else begin
    lHeaderSize:=SizeOf(rAfterHdrIII) + SizeOf(rDbfHdr);
    lFieldSize:=SizeOf(rFieldHdrIII);
  end;
  lColumnCount:= (_DataHdr.FullHdrSize - lHeaderSize) div lFieldSize;

  if (lColumnCount <= 0) or (lColumnCount > 255) then
      Raise eBinaryDataSetError.Create('Invalid field count : ' + IntToStr(lColumnCount) + ' (must be between 1 and 255)');

  lFieldOffset := 1;

  Stream.Position := lHeaderSize;
  for Il:=0 to lColumnCount-1 do begin
    if _DbfVersion>=xBaseV then begin
      Stream.ReadBuffer(lFieldHdrV,SizeOf(lFieldHdrV));
      fn:=PCHAR(@lFieldHdrV.FieldName[0]);
      fs:=lFieldHdrV.FieldSize;
      fd:=lFieldHdrV.FieldPrecision;
      nfs:=fs;
      ft:=ToFieldType(lFieldHdrV.FieldType,nfs,fd);
    end else begin
      Stream.ReadBuffer(lFieldHdrIII,SizeOf(lFieldHdrIII));
      fn:=PCHAR(@lFieldHdrIII.FieldName[0]);
      fs:=lFieldHdrIII.FieldSize;
      fd:=lFieldHdrIII.FieldPrecision;
      nfs:=fs;
      ft:=ToFieldType(lFieldHdrIII.FieldType,nfs,fd);

    end;
    // first create the bde field
    if ft in [ftString,ftBCD] then fieldDefs.Add(fn,ft,fs,false)
    else fieldDefs.Add(fn,ft,0,false);
    // then create the for our own fieldinfo
    MyFieldInfo:=TMyFieldInfo.Create;
    MyFieldInfo.Offset:=lFieldOffset;
    MyFieldInfo.Size:=fs;
    MyFieldInfo.Prec:=fd;
    MyFieldInfo.FieldName:=lowercase(fn);

    _MyFieldInfos.Add(MyFieldInfo);
    Inc(lFieldOffset,fs);
  end;
  if (lFieldOffset <> _DataHdr.RecordSize) then begin
{$ifndef fpc}
    ShowMessage('Invalid Record Size,'+^M+
                                     'Record Size in Hdr : '+IntToStr(_DataHdr.RecordSize)+^M+
                                     'Expected : '+IntToStr(lFieldOffset));
{$endif}
    _DataHdr.RecordSize := lFieldOffset;
  end;
end;

procedure TDbfFile.DbfFile_CreateTable(FieldDefs:TFieldDefs);
var
  ix:Integer;
  lFieldHdrIII:rFieldHdrIII;
  lType:Char;
  lSize,lPrec:Integer;
  Offs:Integer;
  lterminator:Byte;
begin
  // first reset file.
  Stream.Size:= 0;
  Stream.Position:=SizeOf(rDbfHdr) + SizeOf(rAfterHdrIII);
  Offs:=1; // deleted mark count 1.
  for Ix:=0 to FieldDefs.Count-1 do
  begin
    with FieldDefs.Items[Ix] do
    begin
      FillChar(lFieldHdrIII,SizeOf(lFieldHdrIII),#0);
      lPrec:=0;
      case DataType of
        ftString:
          begin
            ltype:='C';
            lSize := Size;
          end;
        ftBoolean:
          begin
            ltype:='L';
            lSize := 1;
          end;
        ftSmallInt:
          begin
            ltype:='N';
            lSize := 6;
          end;
        ftInteger:
          begin
            ltype:='N';
            lSize := 11;
          end;
        ftCurrency:
          begin
            ltype:='N';
            lSize := 20;
            lPrec := 2;
          end;
{$ifndef DELPHI_3}
        ftLargeInt:
          begin
            ltype:='N';
            lSize := 20;
            lPrec := 0;
          end;
{$endif}
        ftFloat:
          begin
            ltype:='N';
            lSize := 20;
            lPrec := 4;
          end;
        ftDate:
          begin
            ltype:='D';
            lSize := 8;
          end;
        ftMemo:
          begin
            ltype:='M';
            lSize := 10;
          end;
        else
          begin
            raise EBinaryDataSetError.Create(
             'InitFieldDefs: Unsupported field type');
          end;
      end; // case

      lFieldHdrIII.FieldType:=ltype; //DataType;
      StrPCopy(lFieldHdrIII.FieldName,FieldDefs.Items[Ix].Name);
      lFieldHdrIII.FieldSize:=lSize;
      lFieldHdrIII.FieldPrecision:=lPrec;

      Stream.Write(lFieldHdrIII,SizeOf(lFieldHdrIII));
      Inc(Offs,lSize);
    end;
  end;
  // end of header
  lterminator := $0d;
  Stream.Write(lterminator,SizeOf(lterminator));

  // update header
  _DataHdr.RecordSize := Offs;
  _DataHdr.FullHdrSize := Stream.Position;
  RecordSize := _DataHdr.RecordSize;
  HeaderSize := _DataHdr.FullHdrSize;
  // write the updated header
  WriteHeader;
end;

procedure TDbfFile.DbfFile_PackTable;
var
  first,last:integer;
  p: Pointer;
begin
  // Non tested.
  if (RecordSize <> 0) then
  begin
    first:=0;
    last:=CalcRecordCount-1;
    GetMem(p, RecordSize);
    try
      while first<last do begin
        // first find the first hole
        while first<last do begin
          ReadRecord(first, p);
          if (pRecordHdr(p)^.DeletedFlag <> ' ') then break;
          inc(first);
        end;
        // now find last one non deleted.
        while first<last do begin
          ReadRecord(last, p);
          if (pRecordHdr(p)^.DeletedFlag = ' ') then break;
          dec(last);
        end;
        if first<last then begin
          // found a non deleted record to put in the hole.
          WriteRecord(first, p);
          inc(first);
          dec(last);
        end;
      end;
    last:=CalcRecordCount;
      Stream.Size:=(last+1) * RecordSize + HeaderSize;
    finally
      FreeMem(p);
    end;
  end;
end;

function TDbfFile.GetFieldInfo(FieldName:string):TMyFieldInfo;
var
  i:Integer;
  lfi:TMyFieldInfo;
begin
  FieldName:=LowerCase(FieldName);
  for i:=0 to _MyFieldInfos.Count-1 do begin
    lfi:=TMyFieldInfo(_MyFieldInfos.Items[i]);
    if lfi.FieldName = FieldName then begin
      result:=lfi;
      exit;
    end;
  end;
  result:=nil;
end;

function TDbfFile.GetFieldData(Column:Integer;DataType:TFieldType; Src,Dst:Pointer): Boolean;
var
  FieldOffset: Integer;
  FieldSize: Integer;
  s:string;
  d:TDateTime;
  ld,lm,ly: word;
  MyFieldInfo:TMyFieldInfo;
  function TrimStr(const s: string): string;
  var
    iPos: integer;
  begin
    if DataType=ftString then
    begin
      if tDbf_TrimFields then Result:=Trim(s)
      else Result:=TrimRight(s);
    end
    else Result:= Trim(s);
  end;
  procedure CorrectYear(var wYear: word);
  var wD, wM, wY, CenturyBase: word;
{$ifdef DELPHI_3}
  // Delphi 3 standard-behavior no change possible
  const TwoDigitYearCenturyWindow= 0;
{$endif}
  begin
     if wYear>= 100 then
       Exit;
     DecodeDate(Date, wY, wm, wD);
     // use Delphi-Date-Window
     CenturyBase := wY{must be CurrentYear} - TwoDigitYearCenturyWindow;
     Inc(wYear, CenturyBase div 100 * 100);
     if (TwoDigitYearCenturyWindow > 0) and (wYear < CenturyBase) then
        Inc(wYear, 100);
  end;
begin
  MyFieldInfo:=TMyFieldInfo(_MyFieldInfos.Items[Column]);
  FieldOffset := MyFieldInfo.Offset;
  FieldSize := MyFieldInfo.Size;
  SetString(s, PChar(Src) + FieldOffset, FieldSize );
  s:=TrimStr(s);
  result:=length(s)>0; // return if field is empty
  if Result and (Dst<>nil) then// data not needed if Result= FALSE or Dst=nil
    case DataType of
    ftBoolean:
      begin
        // in DBase- FileDescription lowercase t is allowed too
        // with asking for Result= TRUE s must be longer then 0
        // else it happens an AV, maybe field is NULL
        if (UpCase(s[1])='T') then Word(Dst^) := 1
        else Word(Dst^) := 0;
      end;
    ftInteger, ftSmallInt{$ifndef DELPHI_3},ftLargeInt{$endif}:
      begin
        case DataType of
        ftSmallInt : SmallInt(Dst^):= StrToIntDef(s, 0);
        {$ifndef DELPHI_3}
        ftLargeint : LargeInt(Dst^):= StrToInt64Def(s, 0);
        {$endif}
        else // ftInteger :
          Integer(Dst^):= StrToIntDef(s, 0);
        end; // case
      end;
    ftFloat:
      begin
        Double(Dst^) := DBFStrToFloat(s);
      end;
    ftCurrency:
      begin
        Double(Dst^) := DBFStrToFloat(s);
      end;
    ftDate:
      begin
        ld:=StrToIntDef(Copy(s,7,2),1);
        lm:=StrToIntDef(Copy(s,5,2),1);
        ly:=StrToIntDef(Copy(s,1,4),0);
        if ld=0 then ld:=1;
        if lm=0 then lm:=1;
//           if (ly<1900) or (ly>2100) then ly:=1900;
//           Year from 0001 to 9999 is possible
//           everyting else is an error, an empty string too
//           Do DateCorrection with Delphis possibillities for one or two digits
        if (ly< 100) and (Length(Trim(Copy(s,1,4)))in [1, 2]) then CorrectYear(ly);
        try
          d:=EncodeDate(ly,lm,ld);
          if Assigned(Dst) then  Integer(Dst^) := DateTimeToTimeStamp(d).Date;
        except
          Integer(Dst^) := 0;
        end;
      end;
        ftString: begin
        StrPCopy(Dst,s);
      end;
   end;
end;

procedure TDbfFile.SetFieldData(Column:integer;DataType:TFieldType; Src,Dst:Pointer);
var
  FieldSize,FieldPrec: Integer;
  s:string;
  fl:Double;
  ts:TTimeStamp;
  MyFieldInfo:TMyFieldInfo;
begin
  MyFieldInfo:=TMyFieldInfo(_MyFieldInfos.Items[Column]);
  FieldSize := MyFieldInfo.Size;
  FieldPrec := MyFieldInfo.Prec;

  Dst:=PChar(Dst)+MyFieldInfo.Offset;
  if src<>nil then begin
    case DataType of
    ftBoolean:
      begin
        if Word(Src^) = 1 then s:='T'
        else s:='F';
      end;
    ftInteger, ftSmallInt {$ifndef DELPHI_3},ftLargeInt{$endif}:
      begin
        case DataType of
        ftSmallInt : s:= IntToStr(SmallInt(Src^));
        {$ifndef DELPHI_3}
        ftLargeInt: s:= IntToStr(LargeInt(Src^));
        {$endif}
        else //ftInteger
          s:= IntToStr(Integer(Src^));
        end;
        // left filling
        if Length(s)<FieldSize then s:=StringOfChar(' ',FieldSize-Length(s)) + s;
      end;
    ftFloat,ftCurrency:
      begin
        fl := Double(Src^);
        s:=FloatToDbfStr(fl,FieldSize,FieldPrec);
        if Length(s)<FieldSize then s:=StringOfChar(' ',FieldSize-Length(s)) + s;
      end;
    ftDate:
      begin
        ts.Time:=0;
        ts.Date:=Integer(Src^);
        s:= FormatDateTime('yyyymmdd', TimeStampToDateTime(ts));
      end;
    ftString:
      begin
        s:=PChar(Src); // finish with first 0
      end;
    end; // case
  end; // if src<>nil (thanks andreas)
  if Length(s)<FieldSize then begin
    s:=s+StringOfChar(' ',FieldSize-Length(s));
  end else if (Length(s)>FieldSize) then begin
    if DataType= ftString then begin
      // never raise for strings to long, its not customary
      // TTable never raises
      SetLength(s, FieldSize)
    end else begin
      raise eFieldToLongError.Create('Fielddata too long :' + IntToStr(Length(s))
        + ' (must be between 1 and ' + IntToStr(FieldSize) + ').');
    end;
  end;
  Move(PChar(s)^, Dst^, FieldSize);
end;


procedure TDbfFile.WriteHeader;
var
  SystemTime: TSystemTime;
  lAfterHdrIII:rAfterHdrIII;
  lAfterHdrV:rAfterHdrV;
  lterminator:Byte;
begin
  Assert(Stream<>nil,'_dbfFile=Nil');

  Stream.Position:=0;
  GetLocalTime(SystemTime);
{$ifndef fpc}
  _DataHdr.Year := SystemTime.wYear - 1900;
  _DataHdr.Month := SystemTime.wMonth;
  _DataHdr.Day := SystemTime.wDay;
{$else}
  _DataHdr.Year := SystemTime.Year - 1900;
  _DataHdr.Month := SystemTime.Month;
  _DataHdr.Day := SystemTime.Day;
{$endif}  
  Stream.Seek(0,soFromBeginning);
  Stream.WriteBuffer (_DataHdr, SizeOf(_DataHdr));
  _DataHdr.RecordCount := CalcRecordCount;

  if _DbfVersion >= xBaseV then begin
    FillChar(lAfterHdrV,SizeOf(lAfterHdrV),0);
    Stream.WriteBuffer (lAfterHdrV, SizeOf(lAfterHdrV));
  end else begin
    FillChar(lAfterHdrIII,SizeOf(lAfterHdrIII),0);
    Stream.WriteBuffer (lAfterHdrIII, SizeOf(lAfterHdrIII));
  end;
  _Seek(_DataHdr.RecordCount); // last byte usually...
  lterminator := $1A;
  Stream.Write(lterminator,SizeOf(lterminator));
end;

function TDbf._ComponentInfo:string;
begin
  Result:='TDbf V' + IntToStr(_MAJOR_VERSION) + '.' + IntToStr(_MINOR_VERSION);
end;

procedure TDbf._OpenFiles(CreateIt:boolean);
var
  fileopenmode : integer;
  lPath,lFilename,lIndexName,lMemoName : string;
  isAbsolute:boolean;
  design,doreadonly:boolean;

begin
  design:=(csDesigning in ComponentState);
  doreadonly:=design or _ReadOnly;

  lPath:=_GetPath;
  isAbsolute:=((length(_TableName)>=1) and (_TableName[1]='\'))
    or ((length(_TableName)>=2) and (_TableName[2]=':'));
  if isAbsolute then lfilename:=_TableName
  else lFilename:=lPath+_TableName;
  lFilename:=ChangeFileExt(lFilename,'.dbf');
  lIndexName:=ChangeFileExt(lFilename,'.mdx');
  lMemoName:=ChangeFileExt(lFilename,'.dbt');

  // check if the file exists
  _dbfFile:=TDbfFile(GetPagedFile(lFileName));
  _indexFile:=TIndexFile(GetPagedFile(lIndexName));
  _dbtFile:=TDbtFile(GetPagedFile(lMemoName));

  if CreateIt then begin
    if _dbfFile=nil then _dbfFile:=TDbfFile.Create(lFileName,fmCreate);
    //if _indexfile=nil then _indexFile := TIndexFile.Create(lIndexName, fmCreate);
    if _dbtfile=nil then _dbtFile := TDbtFile.Create(lMemoName, fmCreate,_dbfFile._DbfVersion);
  end else if not FileExists(lFileName) then begin
    raise eBinaryDataSetError.Create ('Open: Table file not found : ' + lFileName);
  end else begin
    if DoReadOnly  then
      fileopenmode := fmOpenRead + fmShareDenyNone
    else
      fileopenmode := fmOpenReadWrite + fmShareDenyWrite;

    if _dbfFile=nil then _dbfFile := TDBFFile.Create(lFileName, fileopenmode);
    if (_indexFile=nil) and FileExists (lIndexName) then begin
      _indexFile := TIndexFile.Create(lIndexName, fileopenmode);
    end;
    if (_dbtFile=nil) and FileExists (lMemoName) then begin
      _dbtFile := TDbtFile.Create(lMemoName, fileopenmode,_dbfFile._DbfVersion);
    end;
  end;
  _PrevBuffer:=AllocRecordBuffer;
  _IsCursorOpen:=true;

end;

function TDbf._GetPath:string;
var
  lPath:string;
begin
  if (csDesigning in ComponentState) then begin
    lPath:=_DesignTimePath;
  end else begin
    if ((length(_RunTimePath)>=1) and (_RunTimePath[1]=DirSeparator))
      or ((length(_RunTimePath)>=2) and (_RunTimePath[2]=':'))
      then begin
      // if the _RunTimePath is absolute...
      // it is either \ or \blahblah or c:\
      lPath:=_RunTimePath;
    end else begin
{$ifndef fpc}
      lPath:=extractfilepath(Application.Exename)+_RunTimePath;
{$else}      
      lPath:=extractfilepath(paramstr(0))+_RunTimePath;
{$endif}
    end;
  end;
  lPath:=ExpandFileName(trim(lPath));
  if (length(lPath)>0) and (lPath[length(lPath)]<>DirSeparator) then lPath:=lPath+DirSeparator;
  result:=lPath;
end;

procedure TDbf._CloseFiles;
var
  i:integer;
begin
  if _dbfFile<>nil then begin
    if not _ReadOnly then _dbfFile.WriteHeader;
    _dbfFile.Release;
    _dbfFile:=nil;
  end;
  if _indexFile<>nil then begin
    _indexFile.Release;
    _indexFile:=nil;
  end;

  if _dbtFile<>nil then begin
    _dbtFile.Release;
    _dbtFile:=nil;
  end;

  if _indexes<>nil then begin
    for i:=0 to _Indexes.Count-1 do begin
      TIndex(_Indexes[i]).Free;
    end;
    _Indexes.Clear;
    _CurIndex:=nil;
  end;
  if (_PrevBuffer<>nil) then begin
    FreeRecordBuffer(_PrevBuffer);
    _PrevBuffer:=nil;
  end;
  _IsCursorOpen:=false;
end;

procedure TDbf._SetIndexName(const Value: string);
begin
  _CurIndex:=_GetIndex(Value);
  Resync([]);
end;

function TDbf._GetIndexName: string;
begin
  if _CurIndex=nil then Result:=''
  else Result:=_CurIndex._IndexFile._Filename;
end;

function TDbf._GetIndex(filename:string):TIndex;
var
  i:integer;
  lindex:TIndex;
begin
  result:=nil;
  filename:=lowercase(_GetPath + filename);
  for i:=0 to _indexes.Count-1 do begin
    lindex:=TIndex(_indexes.Items[i]);
    if lindex._IndexFile._Filename=filename then begin
      result:=lindex;
      exit;
    end;
  end;
end;


//==========================================================
//============ TMyBlobFile
//==========================================================
constructor TMyBlobFile.Create(ModeVal:TBlobStreamMode;FieldVal:TField);
begin
  Mode:=ModeVal;
  Field:=FieldVal;
end;

destructor TMyBlobFile.destroy;
var
  Dbf:TDbf;
begin
  if (Mode=bmWrite) then begin
    Size:=Position; // Strange but it leave tailing trash bytes if I do not write that.
    Dbf:=TDbf(Field.DataSet);
    Dbf._dbtFile.WriteMemo(MemoRecno,ReadSize,Self);

    Dbf._dbfFile.SetFieldData(Field.FieldNo-1,
      ftInteger,@MemoRecno,@pDbfRecord(TDbf(Field.DataSet).ActiveBuffer)^.deletedflag);
    // seems not bad
{$ifndef fpc}    
    // FPC doesn't allow to call protected methods ?!!
    Dbf.SetModified(true);
{$endif}    
    // but would that be better
    //if not (State in [dsCalcFields, dsFilter, dsNewValue]) then begin
    //  DataEvent(deFieldChange, Longint(Field));
    //end;
  end;
  inherited;
end;

//====================================================================
// TDbf = TDataset Descendant.
//====================================================================
constructor TDbf.Create(AOwner: TComponent); {override;}
begin
  inherited create(aOwner);
  BookmarkSize:=sizeof(rBookmarkData);

  _RunTimePath:='.';
  _IsCursorOpen:=false;
  _Indexes:=TList.Create;
  _CurIndex:=nil;
  _IndexFiles:=TStringList.Create;
end;

destructor TDbf.Destroy; {override;}
var
  i:integer;
begin
  inherited;
  _CurIndex:=nil;
  for i:=0 to _Indexes.Count-1 do begin
    TIndex(_Indexes[i]).Free;
  end;
  _Indexes.Free;
  _IndexFiles.Free;
//  _MemIndex.Free;
end;


function TDbf._FilterRecord(Buffer: PChar): Boolean;
var
  SaveState: TDatasetState;
  s:string;
begin
  Result:=True;
  if Length(easyfilter)<>0 then begin
    SetString(s,buffer,RecordSize);
    s:=LowerCase(s);
    if Pos(easyfilter,s)=0 then begin
      Result:=False;
      Exit;
    end;
  end;
  if not Assigned(OnFilterRecord) then Exit;
  if not Filtered then Exit;
  _FilterBuffer:=buffer;
  SaveState:=SetTempState(dsFilter);
  OnFilterRecord(self,Result);
  RestoreState(SaveState);
end;

function TDbf._RecordDataSize:integer;
begin
  if _dbfFile=nil then result:=0
  else result:=_dbfFile.RecordSize;
end;

function TDbf._FullRecordSize:integer;
begin
  result:=sizeof(rBeforeRecord) + _RecordDataSize + CalcFieldsSize;
end;

function TDbf.AllocRecordBuffer: PChar; {override virtual abstract from TDataset}
begin
  result:=StrAlloc(_FullRecordSize);
  InternalInitRecord(result);
end;

procedure TDbf.FreeRecordBuffer(var Buffer: PChar); {override virtual abstract from TDataset}
begin
  StrDispose(Buffer);
end;

procedure TDbf.GetBookmarkData(Buffer: PChar; Data: Pointer); {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
begin
  prec:=pDbfRecord(Buffer);
  pBookMarkData(Data)^:=prec^.BookMarkData;
end;

function TDbf.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
begin
  prec:=pDbfRecord(Buffer);
  result:=prec^.BookMarkFlag;
end;

function TDbf.GetFieldData(Field: TField; Buffer: Pointer): Boolean; {override virtual abstract from TDataset}
var
  ptr:pointer;
begin
  Result := False;
  if State=dsFilter then begin
    Ptr:=_FilterBuffer;
  end else   if State = dsCalcFields then  begin
    // ***** calc fields *****  set correct buffer
    ptr := @(pDbfRecord(CalcBuffer)^.deletedflag);
  end else begin
    if IsEmpty then exit;
    ptr:=@(pDbfRecord(ActiveBuffer)^.deletedflag);
  end;

  if Field.FieldNo>0 then begin
    Result:=_dbfFile.GetFieldData(Field.FieldNo - 1,Field.DataType,ptr,Buffer);
  end else begin { calculated fields.... }
    Inc(PChar(Ptr), Field.Offset + GetRecordSize);
{$ifndef fpc}
    Result := Boolean(PChar(Ptr)[0]);
{$else}
    Result := (Pchar(ptr)[0]<>#0);
{$endif}    
    if Result and (Buffer <> nil) then
      Move(PChar(Ptr)[1], Buffer^, Field.DataSize);
  end;
end;


function TDbf.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; {override virtual abstract from TDataset}
var
  Acceptable : Boolean;
  prec:pDBFRecord;
begin
  prec:=pDBFRecord(Buffer);
  if _dbfFile.RecordCount < 1 then
    Result := grEOF
  else repeat
    result := grOk;
    case GetMode of
      gmCurrent :
        begin
          if prec^.BookmarkData.Recno=_PhysicalRecno then begin
            exit;    // try to fasten a bit...
          end;
        end;
      gmNext :
        begin
          if _curIndex<>nil then begin
            Acceptable:=_curIndex.Next;
          end else begin
            inc(_PhysicalRecno);
            Acceptable:=(_PhysicalRecno<_dbfFile.RecordCount);
          end;
          if Acceptable then begin
            result:= grOk;
          end else begin
            InternalLast;
            result:= grEOF
          end;
        end;
      gmPrior :
        begin
          if _curIndex<>nil then begin
            Acceptable:=_curIndex.Prev;
          end else begin
            dec(_PhysicalRecno);
            Acceptable:=(_PhysicalRecno>=0);
          end;
          if Acceptable then begin
            result:= grOk;
          end else begin
            InternalFirst;
            result:= grBOF
          end;
        end;
    end;
    if result=grOk then begin
      if _curIndex<>nil then _PhysicalRecno:=_CurIndex.GetRealRecNo;
      if (_PhysicalRecno>=_dbfFile.RecordCount)
        or (_PhysicalRecno<0) then begin
        result:=grError;
      end else begin
        _dbfFile.ReadRecord(_PhysicalRecno,@prec^.DeletedFlag);
        result:=grOk;
      end;
      if Result = grOK then begin
        ClearCalcFields(Buffer);
        GetCalcFields(Buffer);
        prec^.BookmarkFlag := bfCurrent;
        prec^.BookmarkData.Recno:=PhysicalRecno;
      end else if (Result = grError) and DoCheck then
          raise eBinaryDataSetError.Create ('GetRecord: Invalid record');
    end;
    Acceptable := (_ShowDeleted or (prec^.DeletedFlag = ' '))
      and _FilterRecord(Buffer);
    if (GetMode=gmCurrent) and Not Acceptable then Result := grError;
  until (Result <> grOK) or Acceptable;
end;

function TDbf.GetRecordSize: Word; {override virtual abstract from TDataset}
begin
  Result := _RecordDataSize; // data only
end;

procedure TDbf.InternalAddRecord(Buffer: Pointer; DoAppend: Boolean); {override virtual abstract from TDataset}
begin
end;

procedure TDbf.InternalClose; {override virtual abstract from TDataset}
begin
  _CloseFiles;

  // disconnect field objects
  BindFields(False);
  // destroy field object (if not persistent)
  if DefaultFields then
    DestroyFields;
end;

procedure TDbf.InternalDelete; {override virtual abstract from TDataset}
begin
//  CheckActive;
  pRecordHdr(ActiveBuffer)^.DeletedFlag := '*'; //_DataHdr.LastDeleted;
  _dbfFile.WriteRecord(_PhysicalRecNo,ActiveBuffer);
  Resync([]);
end;

procedure TDbf.InternalFirst; {override virtual abstract from TDataset}
begin
  if _dbfFile.RecordCount=0 then InternalLast
  else if _curindex=nil then _PhysicalRecno:=-1
  else _curIndex.First;
end;

procedure TDbf.InternalGotoBookmark(ABookmark: Pointer); {override virtual abstract from TDataset}
var
  RecInfo: TRecInfo;
begin
  RecInfo := TRecInfo(ABookmark^);
  if (RecInfo.Bookmark >= 0) and (RecInfo.Bookmark < _dbfFile.RecordCount) then begin
    _PhysicalRecno:=RecInfo.Bookmark;
  end else
    raise eBinaryDataSetError.Create ('Bookmark ' +
      IntToStr (RecInfo.Bookmark) + ' not found');
end;

procedure TDbf.InternalHandleException; {override virtual abstract from TDataset}
begin
{$ifndef fpc}
  Application.HandleException(Self);
{$endif}  
end;

procedure TDbf.InternalInitFieldDefs; {override virtual abstract from TDataset}
begin
  FieldDefs.Clear;
  with FieldDefs do
  begin
    if IsCursorOpen  then begin
      _dbfFile.CreateFieldDefs(FieldDefs);
    end else begin
      _OpenFiles(false);
      _dbfFile.CreateFieldDefs(FieldDefs);
      Close();
    end;
  end;
end;

procedure TDbf.InternalInitRecord(Buffer: PChar); {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
begin
  prec:=pDbfRecord(Buffer);
  prec^.BookmarkData.RecNo:=-1;
  prec^.BookmarkFlag:=TBookmarkFlag(0);
  fillchar(prec^.DeletedFlag,_RecordDataSize,' ');
end;

procedure TDbf.InternalLast; {override virtual abstract from TDataset}
begin
  if _curindex=nil then _PhysicalRecno:=_dbfFile.RecordCount
  else _curIndex.Last;
end;

procedure TDbf.InternalOpen; {override virtual abstract from TDataset}
begin
  _OpenFiles(false);
  // if there are no persistent field objects,
  InternalInitFieldDefs;
  // create the fields dynamically
  if DefaultFields then begin
    CreateFields;
  end;
  BindFields (True);
  // connect the TField objects with the actual fields

  InternalFirst;
end;

procedure TDbf.InternalPost; {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
  lIndex:TIndex;
  i:integer;
begin
  CheckActive;
  prec:=pDbfRecord(ActiveBuffer);
  prec^.DeletedFlag:=' ';

  if State = dsEdit then
  begin
    // replace data with new data
    if _indexes.Count>0 then begin
      _dbfFile.ReadRecord(_PhysicalRecno,_PrevBuffer);
      for i:=0 to _indexes.Count-1 do begin
        lindex:=TIndex(_indexes.Items[i]);
        lindex.Update(_PhysicalRecno,_PrevBuffer,@prec^.DeletedFlag);
      end;
    end;
  end else begin
    // append
    _PhysicalRecno:=_dbfFile._DataHdr.RecordCount;
    inc(_dbfFile._DataHdr.RecordCount);
    if _indexes.Count>0 then begin
      _dbfFile.ReadRecord(_PhysicalRecno,_PrevBuffer);
      for i:=0 to _indexes.Count-1 do begin
        lindex:=TIndex(_indexes.Items[i]);
        lindex.Insert(_PhysicalRecno,@prec^.DeletedFlag);
      end;
    end;
  end;
  _dbfFile.WriteRecord(_PhysicalRecno,@prec^.DeletedFlag);
end;


procedure TDbf.CreateTable; //(FieldDefs:TFieldDefs);
var
  ix:integer;
begin
  CheckInactive;
  //  InternalInitFieldDefs;
  if FieldDefs.Count = 0 then
  begin
    for Ix := 0 to FieldCount - 1 do
    begin
      with Fields[Ix] do
      begin
        if FieldKind = fkData then
          FieldDefs.Add(FieldName,DataType,Size,Required);
      end;
    end;
  end;
  _OpenFiles(true);
  try
    _dbfFile.DbfFile_CreateTable(FieldDefs);
  finally
    // close the file
    _CloseFiles;
  end;
end;

procedure TDbf.PackTable;
begin
  _dbfFile.dbfFile_PackTable;
  Resync([]);
end;


function TDbf.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; {override virtual}
var
  Memoi:array[1..32] of char;
  lBlob:TMyBlobFile;
begin
  lBlob:=TMyBlobFile.Create(Mode,Field);
  if _dbfFile.GetFieldData(Field.FieldNo-1, ftString,@pDbfRecord(ActiveBuffer)^.deletedflag,@Memoi[1]) then begin
    lBlob.MemoRecno:=StrToIntDef(Memoi,0);
    _dbtFile.ReadMemo(lBlob.MemoRecno,lBlob);
    lBlob.ReadSize:=lBlob.Size;
  end else lBlob.MemoRecno:=0;
  Result:=lBlob;
end;

{$ifdef DELPHI_3}
procedure TDbf.Translate(Src, Dest: PChar; ToOem: Boolean); {override virtual}
begin
  if (Src <> nil) and (Dest<>nil) then begin
    if ToOem then CharToOem(Src,Dest)
    else OemToChar(Src,Dest);
  end;
end;
{$else}
{$ifndef fpc}
function TDbf.Translate(Src, Dest: PChar; ToOem: Boolean): Integer; {override virtual}
begin
  if (Src <> nil) and (Dest<>nil) then begin
    if ToOem then CharToOem(Src,Dest)
    else OemToChar(Src,Dest);
    result:= StrLen(Dest);
  end else result:=0;
end;
{$else}
procedure TDbf.Translate(Src, Dest: PChar; ToOem: Boolean); {override virtual}
begin
end;
{$endif}
{$endif}

procedure TDbf.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[_dbfFile.RecordSize], CalcFieldsSize, 0);
end;

procedure TDbf.InternalSetToRecord(Buffer: PChar); {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
begin
  if Buffer=nil then exit;
  prec:=pDbfRecord(Buffer);
  _PhysicalRecno:=prec^.BookmarkData.RecNo;
  _ResyncIndexes(Buffer);
end;

procedure TDbf._ResyncIndexes(Buffer: PChar);
var
  i:integer;
  lindex:TIndex;
begin
  if _indexes.Count>0 then begin
    _dbfFile.ReadRecord(_PhysicalRecno,_PrevBuffer);
    for i:=0 to _indexes.Count-1 do begin
      lindex:=TIndex(_indexes.Items[i]);
      lindex.GotoKey(_physicalRecno,nil);
    end;
  end;
end;

function TDbf.IsCursorOpen: Boolean; {override virtual abstract from TDataset}
begin
  result:=_IsCursorOpen;
end;

procedure TDbf.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
begin
  prec:=pDbfRecord(Buffer);
  prec^.BookMarkFlag:=Value;
end;

procedure TDbf.SetBookmarkData(Buffer: PChar; Data: Pointer); {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
begin
  prec:=pDbfRecord(Buffer);
  prec^.BookMarkData:=pBookMarkData(Data)^;
end;

procedure TDbf.SetFieldData(Field: TField; Buffer: Pointer); {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
  dst:pointer;
begin
  if (Field.FieldNo >= 0) then begin
    prec:=pDbfRecord(ActiveBuffer);
    dst:=@prec^.DeletedFlag;
    _dbfFile.SetFieldData(Field.FieldNo - 1,Field.DataType,Buffer,Dst);
  end else begin    { ***** fkCalculated, fkLookup ***** }
    prec:=pDbfRecord(CalcBuffer);
    dst:=@prec^.DeletedFlag;
    Inc(integer(dst), GetRecordSize + Field.Offset);
    Boolean(dst^) := LongBool(Buffer);
    if Boolean(dst^) then begin
      Inc(integer(dst), 1);
      Move(Buffer^, dst^, Field.DataSize);
    end;
  end;     { end of ***** fkCalculated, fkLookup ***** }
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then begin
    DataEvent(deFieldChange, Longint(Field));
  end;
end;


// this function is just for the grid scrollbars
// it doesn't have to be perfectly accurate, but fast.
function TDbf.GetRecordCount: Integer; {override virtual}
begin
  if _curIndex=nil then begin
    result:=_dbfFile.RecordCount;
  end else begin
    result:=_curIndex.GuessRecordCount;
  end;
end;

// this function is just for the grid scrollbars
// it doesn't have to be perfectly accurate, but fast.
function TDbf.GetRecNo: Integer; {override virtual}
begin
  UpdateCursorPos;
  if _curIndex=nil then begin
    result:=_PhysicalRecno+1;
  end else begin
    result:=_curIndex.GuessRecNo;
  end;
end;

procedure TDbf.SetRecNo(Value: Integer); {override virual}
begin
  if _curIndex=nil then begin
    _PhysicalRecno:=Value-1;
  end else begin
    //result:=_curIndex.GuessRecNo;
  end;
  Resync([rmExact]);
end;

procedure TDBf.DeleteIndex(const AName: string);

begin
  // I must admit that is seems a bit expeditive.
  // but I does implement this method because TTable does
  DeleteFile(_GetPath + Name);
end;

procedure TDbf.CloseIndexFile(const IndexFileName: string);
var
  lindex:tindex;
begin
  lindex:=_GetIndex(IndexFileName);
  if lindex<>nil then begin
    lindex.Free;
    _indexes.Delete(_indexes.IndexOf(lindex));
    if _curindex = lindex then begin
      _curindex:=nil;
      resync([]);
    end;
  end;
end;

procedure TDbf.OpenIndexFile(AnIndexName:string);
var
  lIndexFile:TIndexFile;
  lIndex:TIndex;
begin
  lindex:=_GetIndex(IndexName);
  if lindex=nil then begin
    IndexName:=lowercase(_GetPath + IndexName);
    lIndexFile:=TIndexFile(GetPagedFile(IndexName));
    if lIndexFile=nil then begin
      lIndexFile:=TIndexFile.Create(IndexName,fmOpenReadWrite + fmShareDenyWrite);
    end;
    lIndex:=TIndex.Create(lIndexFile,0,false);
    _Indexes.Add(lIndex);
    lIndex.InitFieldDef(_DbfFile,lIndex._NdxHdr.KeyDesc);
  end;
end;

(*
procedure TDbfFile.DbfFile_PackTable;
var
begin
end;
*)
{$ifdef fpc}
procedure TDbf.AddIndex(const AnIndexName, IndexFields: String; Options: TIndexOptions);

begin
  AddIndex(indexName,IndexFields,options,'');  
end;
{$endif}

{$ifdef DELPHI_3}
procedure TDbf.AddIndex(const IndexName, Fields: String; Options: TIndexOptions);
var
  DescFields:string;
{$else}
{$ifndef fpc}
procedure TDbf.AddIndex(const IndexName, Fields: String; Options: TIndexOptions; const DescFields: String='');
var
{$else}
procedure TDbf.AddIndex(const AnIndexName, IndexFields: String; Options: TIndexOptions; const DescFields: String);
var
{$endif}
{$endif}
  lfilename:string;
  lIndexFile:TIndexFile;
  lIndex:TIndex;
  cur,thelast:integer;
begin
  lfilename:=lowercase(_GetPath+IndexName);
  lIndexFile:=TIndexFile(GetPagedFile(lfilename));
  if lIndexFile<>nil then exit;
  lIndexFile:=TIndexFile.Create(lfilename,fmCreate);
  lIndex:=TIndex.Create(lIndexFile,0,true);
{$ifndef fpc}
  lIndex.InitFieldDef(_DbfFile,Fields);
{$else}  
  lIndex.InitFieldDef(_DbfFile,IndexFields);
{$endif}  
  with lIndex._NdxHdr do begin
    startpage:=1;
    nbPage:=1;
    keyformat:=#0;
    keytype:='C';
    dummy:=$5800;
    keylen:=lindex._FieldLen;
    nbkey:=(512-8) div (lindex._FieldLen+8);
    keyreclen:=lindex._FieldLen+8;
    Unique:=0;
    KeyDesc[0]:=' ';
{$ifndef fpc}    
    StrLCopy(KeyDesc,PChar(UpperCase(Fields)),255);
{$else}    
    StrLCopy(KeyDesc,PChar(UpperCase(IndexFields)),255);
{$endif}    
  end;
  lindex._IndexFile._Seek(lindex._RootPage);
  lindex._IndexFile.Stream.Write(lindex._NdxHdr,SizeOf(lindex._NdxHdr));

  cur:=0;
  thelast:=_DbfFile.CalcRecordCount;

  while cur<thelast do begin
    _DbfFile.ReadRecord(cur, _PrevBuffer);
    lIndex.Insert(cur,_PrevBuffer);
    inc(cur);
  end;
  _Indexes.Add(lIndex);
end;
//==========================================================
//============ dbtfile
//==========================================================
constructor TDbtFile.Create(const FileName: string; Mode: Word; Ver:xBaseVersion);
begin
  inherited Create(FileName,Mode);
  _DbtVersion:=Ver;
  if mode = fmCreate then begin
    FillChar(_MemoHdr,sizeof(_MemoHdr),0);
  end else begin
    Stream.Position:=0;
    Stream.read(_MemoHdr,SizeOf(_MemoHdr));
  end;
  HeaderSize:=0;
  RecordSize:=_MemoHdr.BlockLen;

  if (RecordSize=0) or ((RecordSize mod 128)<>0) then begin
    _MemoHdr.BlockLen := $200;
    RecordSize := $200;
  end;
  // Can you tell me why the header of dbase3 memo contains 1024 and it 512 ?
  if _DbtVersion=xBaseIII then RecordSize:=512;
end;

procedure TDbtFile.ReadMemo(recno:Integer;Dst:TStream);
var
  Buff:array[0..511] of char;
  i,lsize:integer;
  finish:boolean;
  lastc:char;
begin
  if recno=0 then Exit;
  Stream.Position:= RecordSize * recno;
  if _DbtVersion >= xBaseIV then begin // dBase4 memofiles
    Stream.read(Buff[0],8);
    if (Buff[0]=#$ff) and  (Buff[1]=#$ff) and
      (Buff[2]=#$08) and (Buff[3]=#$00) then begin
          // dbase IV memo
      lsize:=(PInteger(@Buff[4])^)-8;
    end else begin
      lsize:=0;
    end;
    repeat
      if lsize>SizeOf(Buff) then begin
        Stream.read(Buff,SizeOf(Buff));
        Dst.Write(buff,SizeOf(Buff));
        Dec(lsize,SizeOf(Buff));
      end else if lsize>0 then begin
        Stream.read(Buff,lsize);
        Dst.Write(buff,lsize);
        lsize:=0;
      end;
    until lsize=0;
  end else begin
    finish:=False;
    Stream.read(Buff,SizeOf(Buff));
    lastc:=#0;
    repeat
      for i:=0 to SizeOf(Buff)-2 do begin
        if ((Buff[i]=#$1A) and
          ((Buff[i+1]=#$1A) or ((i=0) and (lastc=#$1A))))
          or (Buff[i]=#$0)
          then begin
          if i>0 then Dst.Write(buff,i);
          finish:=True;
          break;
        end;
      end;
      if finish then Break;
      Dst.Write(buff,512);
      lastc:=Buff[511];
      Stream.read(Buff,SizeOf(Buff));
    until finish;
  end;
  Dst.Seek(0,0);
end;

procedure TDbtFile.WriteMemo(var MemoRecno:Integer;ReadSize:Integer;Src:TStream);
var
  ByteBefore:Integer;
  ByteAfter:Integer;
  Buff:array[0..511] of char;
  i:Integer;
  c:Byte;
  Append:Boolean;
begin
  if _DbtVersion >= xBaseIV then begin // dBase4 memofiles
    ByteBefore:=8;
    ByteAfter:=0;
  end else begin // stupid files
    ByteBefore:=0;
    ByteAfter:=2;
  end;
  if Src.Size = 0 then begin
    MemoRecno:=0;
  end else begin
    if ((ByteBefore+Src.Size+ByteAfter+_MemoHdr.BlockLen-1) div _MemoHdr.BlockLen)
      <= ((ReadSize+_MemoHdr.BlockLen-1) div _MemoHdr.BlockLen)
      then begin
      Append:=false;
      //MemoRecno:=MemoRecno;
    end else begin
      Append:=True;
      MemoRecno:=_MemoHdr.NextBlock;
      if MemoRecno=0 then begin
        _MemoHdr.NextBlock:=1;
        MemoRecno:=1;
      end;
    end;
    Stream.Seek(_MemoHdr.BlockLen * MemoRecno,0);
    i:=Src.Position;
    Src.Seek(0,0);
    if ByteBefore=8 then begin
      i:=$0008ffff;
      Stream.Write(i,4);
      i:=Src.Size+ByteBefore+ByteAfter;
      Stream.Write(i,4);
    end;
    repeat
      i:=Src.Read(buff,512);
      if i=0 then break;
      Inc(_MemoHdr.NextBlock);
      Stream.Write(Buff,i);
    until i<512;
    if ByteAfter=2 then begin
      c:=$1A;
      Stream.Write(c,1);
      Stream.Write(c,1);
    end;
    if Append then begin
      Stream.Seek(0,0);
      Stream.Write(_MemoHdr,SizeOf(_MemoHdr))
    end;
  end;
end;

//==========================================================
//============ TIndexFile
//==========================================================
constructor TIndexFile.Create(const FileName: string; Mode: Word);
var
  ext:string;
  i:Integer;
begin
  inherited Create(FileName,Mode);
  HeaderSize:=0;
  RecordSize:=512;

  ext:=ExtractFileExt(FileName);
  if (ext='.mdx') then begin
    _IndexVersion:=xBaseIV;
    if Mode = fmCreate then begin
      FillChar(_MdxHdr,sizeof(_MdxHdr),0);
    end else begin
      Stream.read(_MdxHdr,SizeOf(_MdxHdr));
    end;
    for i:= 0 to _MdxHdr.TagUsed-1 do begin
//      Stream.Position :=544 + i * _MdxHdr.TagSize;
//      Stream.read(lMdxTag,SizeOf(rMdxTag));
//      lIndex:=TIndex.Create(Self,lMdxTag.pageno);
//      _Indexes.Add(lIndex);
//      if i=0 then lIndex.ReadPage(lIndex._NdxHdr.startpage);
    end;
  end else begin
    _IndexVersion:=xBaseIII;
(*
      _IndexFile._Seek(Pos);
      _IndexFile.Stream.Read(_NdxHdr,SizeOf(_NdxHdr));
      _Root:=TIndexPage.Create(Self);
      _Root.SetPageNo(_NdxHdr.startpage);
      lPos:=_Root;
      _nblevel:=1;
      repeat
        lPos.LocalFirst;
        if lPos.Entry._LowerPage=0 then break;
        inc(_nblevel);
    lChild:=TIndexPage.Create(Self);
    lChild._UpperLevel:=lPos;
    lPos._LowerLevel:=lChild;
    lChild.SetPageNo(lPos.Entry._LowerPage);
    lPos:=lChild;
  until false;

  _Spare:=TIndexPage.Create(Self);
//  _Field:=_IndexFile._Dbf.FindField(_NdxHdr.KeyDesc);
  First;
*)
  end;
end;

destructor TIndexFile.Destroy;
begin
  inherited;
end;

//==========================================================
//============ TIndexPage
//==========================================================
constructor TIndexPage.Create(Parent:TIndex);
begin
  _LowerLevel:=nil;
  _UpperLevel:=nil;
  _Index:=Parent;
  _PageNo:=-1;
  _EntryNo:=-1;
end;

destructor TIndexPage.Destroy;
begin
  if _LowerLevel<>nil then _LowerLevel.Free;
end;

function  TIndexPage.GetPEntry(EntryNo:integer):PNdxEntry;
begin
  Result:=PNdxentry(@_PageBuff.Entries[_Index._NdxHdr.keyreclen*entryno]);
end;

function  TIndexPage.LocalInsert(Recno:integer; Buffer:Pchar;LowerPage:integer):boolean;
var
  src,dst:pointer;
  siz:integer;
begin
  if _PageBuff.NbEntries < _Index._NdxHdr.nbkey then begin
    src:=Entry;
    dst:=GetPEntry(_EntryNo+1);
    siz:=(_PageBuff.NbEntries - _EntryNo)
      * _Index._NdxHdr.keyreclen + 8;
    Move(Src^, Dst^, Siz);
    inc(_PageBuff.NbEntries);
    SetEntry(Recno,Buffer,LowerPage);
    Write;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;


function  TIndexPage.LocalDelete:boolean;
var
  src,dst:pointer;
  siz:integer;
begin
  if _PageBuff.NbEntries >=0 then begin
    if _EntryNo<_PageBuff.NbEntries then begin
      src:=GetPEntry(_EntryNo+1);
      dst:=Entry;
      siz:=(_PageBuff.NbEntries - _EntryNo - 1)
        * _Index._NdxHdr.keyreclen + 8;
      Move(Src^, Dst^, Siz);
    end;
    dec(_PageBuff.NbEntries);
    Write;
    if ((_PageBuff.NbEntries=0) and (_lowerlevel=nil))
      or (_PageBuff.NbEntries<0) then begin
      if _UpperLevel<>nil then begin
        _UpperLevel.LocalDelete;
      end;
    end else if (_EntryNo>LastEntryNo) then begin
      SetEntryNo(LastEntryNo); // We just removed the last on this page.
      if (_UpperLevel<>nil)  then begin
        _UpperLevel.SetEntry(0,Entry^.CKey,_PageNo);
      end;
    end;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

function  TIndexPage.LastEntryNo:integer;
begin
  if (_LowerLevel=nil) then begin
    result := _PageBuff.NbEntries - 1;
  end else begin
    result := _PageBuff.NbEntries;
  end;
end;

procedure TIndexPage.LocalFirst;
begin
  SetEntryNo(0);
end;

procedure TIndexPage.LocalLast;
begin
  SetEntryNo(LastEntryNo);
end;

function TIndexPage.LocalPrev:boolean;
begin
  if _EntryNo>0 then begin
    SetEntryNo(_EntryNo-1);
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

function TIndexPage.LocalNext:boolean;
begin
  if (_EntryNo<LastEntryNo) then begin
    SetEntryNo(_EntryNo+1);
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

procedure TIndexPage.First;
begin
  LocalFirst;
  if (_LowerLevel<>nil) then LowerLevel.First;
end;

procedure TIndexPage.Last;
begin
  LocalLast;
  if (_LowerLevel<>nil) then LowerLevel.Last;
end;

function TIndexPage.Prev:boolean;
begin
  if (_LowerLevel<>nil) and LowerLevel.Prev then begin
    result:=true;
    exit;
  end;
  Result:=LocalPrev;
  if Result and (Entry^._LowerPage>0) then LowerLevel.Last;
end;

function TIndexPage.Next:boolean;
begin
  if (_LowerLevel<>nil) and LowerLevel.next then begin
    result:=true;
    exit;
  end;
  Result:=LocalNext;
  if Result and (Entry^._LowerPage>0) then LowerLevel.First;
end;


function TIndexPage.FindNearest(Recno:integer; Key:pchar):integer;
var
  cmpres:integer;
  v1,v2:double;
  p:TIndexPage;
begin
  Result:=-1;
  if @Key=nil then begin
    Exit;
  end;
  SetEntryNo(0);
  while _EntryNo<=_PageBuff.NbEntries do begin
    if _EntryNo=_PageBuff.NbEntries then break;
    if _Index._NdxHdr.keytype='C' then begin
      cmpres:=StrLIComp(PChar(Key),Entry^.CKey,_Index._FieldLen);
    end else begin
      // Numeric field... to do
      v1:=PDouble(Key)^;
      v2:=Entry^.NKey;
      if v1>v2 then cmpres:=1
      else if v1<v2 then cmpres:=-1
      else cmpres:=0;
    end;
    if cmpres=0 then begin
      if _LowerLevel=nil then begin
        if (Entry^.RecNo=Recno) then begin
          result:=0;
          Exit;
        end else if (Entry^.Recno>Recno) then begin
          result:=-1;
          Exit;
        end;
      end else begin
        p:=self;
        while p._LowerLevel<>nil do begin
          p:=p.LowerLevel;
          p.LocalLast;
        end;
        if (p.Entry^.Recno>=Recno) then begin
          result:=-1;
          Exit;
        end;
      end;
    end else if cmpres<0 then begin
      result:=-1;
      exit;
    end;
    SetEntryNo(_EntryNo+1);
  end;
  result:=1;
  Exit;
end;

procedure TIndexPage.SetEntry(Recno:Integer; key:PChar; LowerPage:integer);
begin
  assert((_EntryNo>=0) and (_EntryNo<=_PageBuff.NbEntries));
  if (_EntryNo=self._PageBuff.NbEntries) then begin
    if (_UpperLevel<>nil)  then begin
      _UpperLevel.SetEntry(0,key,Self._PageNo);
    end;
  end else begin
    if _Index._NdxHdr.keytype='C' then begin
      mymove(key,Entry^.CKey,_Index._NdxHdr.keylen);
    end else begin
      Entry^.NKey:=PDouble(key)^;
    end;
  end;
  Entry^.RecNo:=RecNo;
  Entry^._LowerPage:=LowerPage;
  Write;
end;
function TIndexPage.LowerLevel : TIndexPage;
begin
  if (_LowerLevel<>nil) and (_LowerLevel._PageNo<>Entry^._LowerPage) then begin
    _LowerLevel.SetPageNo(Entry^._LowerPage);
  end;
  result:=_LowerLevel;
end;

function TIndexPage.Insert(Recno:Integer; Buffer:PChar; LowerPage:integer):boolean;
var
  src,dst:PNdxEntry;
  siz:integer;
  split,old_entry:integer;
  lSpare:TIndexPage;
begin
  if not LocalInsert(recno,buffer,lowerpage) then begin
    // The entry is FULL so we will split this page
    // 1 - Check parent exist
    if _UpperLevel=nil then begin
      AddNewLevel;
    end;

    old_entry:=_EntryNo;
    split:=_EntryNo;
    if split < _Index._NdxHdr.nbkey div 2 then begin
      split:=_Index._NdxHdr.nbkey div 2;
    end;
    lSpare:=TIndexPage.Create(_Index);
    try
      // 2 - Create new page with first part
      inc(_Index._NdxHdr.nbPage);
      lSpare._PageNo:=_Index._NdxHdr.nbPage;
      _Index._IndexFile._Seek(_Index._RootPage);
      _Index._IndexFile.Stream.WriteBuffer (_Index._NdxHdr, SizeOf(_Index._NdxHdr));

      if _lowerlevel=nil then begin
        lSpare._PageBuff.NbEntries:=split;
      end else begin
        lSpare._PageBuff.NbEntries:=split-1;
      end;
      siz:=split*_Index._NdxHdr.keyreclen+8;
      src:=@_PageBuff.Entries;
      dst:=@lSpare._PageBuff.Entries;
      Move(src^,dst^,siz);
      lSpare.Write;

      // 3 - Keep only end-part in this page
      siz:=(_PageBuff.NbEntries-Split);
      _PageBuff.NbEntries:=siz;

      siz:=siz*_Index._NdxHdr.keyreclen+8;
      SetEntryNo(split);
      src:=Entry;
      SetEntryNo(0);
      dst:=Entry;
      Move(src^,dst^,siz);

      // 3 - Update upper level
      lSpare.SetEntryNo(split-1);
      _UpperLevel.Insert(0,lSpare.Entry^.CKey,lSpare._PageNo);

      // We just need to go on inserted record now

      if old_entry>=split then begin
        _UpperLevel.LocalNext;
        SetEntryNo(old_entry - split);
        LocalInsert(Recno,Buffer,LowerPage);
        lSpare.Write;
      end else begin
        lSpare.SetEntryNo(old_entry);
        lSpare.LocalInsert(Recno,Buffer,LowerPage);
        Write;
      end;
    finally
      lspare.free;
    end;
  end;
    Result:=true;
end;

function TIndexPage.Delete:boolean;
begin
  Result:=LocalDelete;
end;

procedure TIndexPage.SetPageNo(page:Integer);
begin
  if (_PageNo<>page) and (page>0) then begin
    _Index._IndexFile.ReadRecord(Page,@_PageBuff);
    _PageNo:=page;
    _EntryNo:=-1;
  end;
end;

procedure TIndexPage.AddNewLevel;
var
  lNewPage:TIndexPage;
begin
  lNewPage:=TIndexPage.Create(_Index);
  inc(_Index._NdxHdr.nbPage);
  lNewPage._PageNo:= _Index._NdxHdr.nbPage;
  _Index._NdxHdr.startpage:= _Index._NdxHdr.nbPage;
  _Index._IndexFile._Seek(_Index._RootPage);
  _Index._IndexFile.Stream.WriteBuffer (_Index._NdxHdr, SizeOf(_Index._NdxHdr));

  lNewPage._PageBuff.NbEntries:=0;
  lNewPage._UpperLevel:=nil;
  lNewPage._LowerLevel:=_Index._Root;
  lNewPage.SetEntryNo(0);
  lNewPage.SetEntry(0,nil,_PageNo);
  _Index._Root._UpperLevel:=lNewPage;
  _Index._Root:=lNewPage;
  lNewPage:=nil;
end;

procedure TIndexPage.Write;
begin
  _Index._IndexFile.WriteRecord(_PageNo,@_PageBuff);
end;

procedure TIndexPage.SetEntryNo(entryno:Integer);
begin
  if (_EntryNo<>entryno) then begin
    _EntryNo:=entryno;
    if _EntryNo>=0 then Entry:=PNdxentry(@_PageBuff.Entries[_Index._NdxHdr.keyreclen*entryno]);
  end;
end;

procedure TIndexPage.WritePage(Page:integer);
begin
  _Index._IndexFile.WriteRecord(Page,@_PageBuff);
end;

//==========================================================
//============ TIndex
//==========================================================
constructor TIndex.Create(Parent:TIndexFile; RootPage:integer;CreateIt:boolean);
var
  lPos:TIndexPage;
  lChild:TIndexPage;
begin
  _RootPage:=RootPage;
  _IndexFile:=Parent;
  //_IndexOrder:=TList.Create;
  if CreateIt then begin
    FillChar(_NdxHdr,sizeof(_NdxHdr),0);
    _NdxHdr.startpage:=1;
    _NdxHdr.nbPage:=2;
    _NdxHdr.keyformat:=#0;
    _NdxHdr.keytype:='C';

    _IndexFile._Seek(RootPage);
    _IndexFile.Stream.Write(_NdxHdr,SizeOf(_NdxHdr));
    _FieldPos := 0;
    _FieldLen := 0;
  end else begin
    _IndexFile._Seek(RootPage);
    _IndexFile.Stream.Read(_NdxHdr,SizeOf(_NdxHdr));
  end;

  _Root:=TIndexPage.Create(Self);
  _Root.SetPageNo(_NdxHdr.startpage);
  lPos:=_Root;
  _nblevel:=1;
  repeat
    lPos.LocalFirst;
    if lPos.Entry^._LowerPage=0 then break;
    inc(_nblevel);
    lChild:=TIndexPage.Create(Self);
    lChild._UpperLevel:=lPos;
    lPos._LowerLevel:=lChild;
    lChild.SetPageNo(lPos.Entry^._LowerPage);
    lPos:=lChild;
  until false;

  inc(_IndexFile._cntuse);
  First;
end;

destructor TIndex.Destroy;
begin
  _IndexFile.Release;
   _Root.Free;
end;


function TIndex.Find(Recno:integer; Buffer:PChar; var pPos:TIndexPage):integer;
var
  res:integer;
begin
  pPos:=_Root;
  repeat
    res:=pPos.FindNearest(Recno,Buffer);
    if res<>0 then begin
      if pPos.Entry^._LowerPage<>0 then begin
        pPos:=pPos.LowerLevel;
        res:=2;
      end;
    end;
  until res<>2;
  Result:=res;
end;

procedure TIndex.Update(Recno: integer; PrevBuffer,NewBuffer: PChar);
var
  lPos:TIndexPage;
begin
  if _FieldLen=0 then exit;

  inc(PrevBuffer,_FieldPos);
  inc(NewBuffer,_FieldPos);

  if StrLIComp(PrevBuffer,NewBuffer,_FieldLen)<>0 then begin
    Delete;
    Find(Recno+1,NewBuffer,lPos);
    lPos.Insert(Recno+1,NewBuffer,0);
  end;
end;

procedure TIndex.Insert(Recno:integer; Buffer:PChar);
var
  lPos:TIndexPage;
begin
  if _FieldLen=0 then exit;

  inc(Buffer,_FieldPos);

  Find(Recno+1,Buffer,lPos);
  lPos.Insert(Recno+1,Buffer,0);
end;

function TIndex.Delete:boolean;
var
  lPos:TIndexPage;
begin
  lpos:=_root;
  while lpos._LowerLevel<>nil do begin
    lPos:=lPos.LowerLevel;
  end;
  lPos.Delete;
  Result:=true;
end;


function TIndex.Pos:TIndexPage;
var
  p:TIndexPage;
begin
  p:=_Root;
  while p.Entry^._LowerPage>0 do begin
    p:=p.LowerLevel;
  end;
  result:=p;
end;

procedure TIndex.First;
begin
  _Root.First;
  dec(Pos._EntryNo);
end;

procedure TIndex.Last;
begin
  _Root.Last;
  inc(Pos._EntryNo);
end;

function TIndex.Prev:boolean;
begin
  result:=_Root.Prev;
end;

function TIndex.Next:boolean;
begin
  result:=_Root.Next;
end;

(*
procedure TIndex.SetRecNo(Value: Integer);
var
  pos:integer;
  p:TIndexPage;
  i:integer;
  ldiv:integer;
begin
  p:=_Root;
  ldiv:=1;
  while p.Entry^._LowerPage>0 do begin
    ldiv:=ldiv*(_NdxHdr.nbkey+1);
    p:=p._LowerLevel;
  end;
  pos:=value div ldiv;
  p:=_Root;
  while p.Entry^._LowerPage>0 do begin
    p._EntryNo:=pos;
    value:=value - pos * (_NdxHdr.nbkey+1);
    ldiv:=ldiv div (_NdxHdr.nbkey+1);
    pos:=value div ldiv;
    p:=p._LowerLevel;
  end;
{
  pos:=1;
  First;
  While pos<value do begin
    if Next = false then break;
    inc(pos);
  end;
}
end;
*)
function TIndex.GuessRecordCount: Integer;
var
  lPos:TIndexPage;
  nbrecord:integer;
begin
  // I just read first level and Guess an approximate record count...
  nbrecord:=_Root._PageBuff.NbEntries;
  lPos:=_Root.LowerLevel;
  while lpos<>nil do begin
    nbrecord:=nbrecord*(_NdxHdr.nbkey+1);
    lPos:=lPos.LowerLevel;
  end;
  result:=nbrecord;
end;


function TIndex.GuessRecNo:Integer;
var
  p:TIndexPage;
begin
  p:=_Root;
  result:=p._EntryNo;
  while p.Entry^._LowerPage>0 do begin
    p:=p.LowerLevel;
    Result:=Result*(_NdxHdr.nbkey+1) + p._EntryNo;
  end;
end;

function TIndex.GetRealRecNo:integer;
var
  ippos : TIndexPage;
begin
  ippos:=_Root;
  while ippos._LowerLevel<>nil do begin
    ippos:=pos.LowerLevel;
  end;
  if (ippos._EntryNo<0) or (ippos._EntryNo>=ippos._PageBuff.NbEntries) then Result:=-1
  else Result:=ippos.Entry^.RecNo-1;
end;

procedure TIndex.GotoKey(recno:integer; buffer:pchar);
begin
  // very temporary implementation
  // could definitely be a bit faster.
  _Root.First;
  repeat
    if self.Pos.Entry^.RecNo=(recno+1) then begin
      exit;
    end;
  until Next=false;
end;

procedure TIndex.InitFieldDef(dbfFile:TDbfFile;FieldDesc:string);
var
  FieldInfo:TMyFieldInfo;
begin
  FieldInfo:=DbfFile.GetFieldInfo(FieldDesc);
  if FieldInfo<>nil then begin
    _FieldPos:=FieldInfo.Offset;
    _FieldLen:=FieldInfo.Size;
  end;
end;

//==========================================================
//============ initialization
//==========================================================

{$ifndef fpc}
type

  TTableNameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure TTableNameProperty.Edit; {override;}
var
  FileOpen: TOpenDialog;
  Dbf: TDbf;
begin
  FileOpen := TOpenDialog.Create(Application);
  try
    with fileopen do begin
      Dbf:=GetComponent(0) as TDbf;
      Filename := Dbf.DesignTimePath + GetValue;
      Filter := 'Dbf table|*.dbf';
      if Execute then begin
        SetValue(ExtractFilename(Filename));
        Dbf.DesignTimePath:=ExtractFilePath(Filename);
      end;
    end;
  finally
    Fileopen.free;
  end;
end;

function TTableNameProperty.GetAttributes: TPropertyAttributes; {override;}
begin
  Result := [paDialog, paRevertable];
end;



type
  TRunTimePathProperty = class(TStringProperty)
  end;

  TDesignTimePathProperty = class(TStringProperty)
  end;

//==========================================================
//============ initialization
//==========================================================

procedure Register;
begin
  RegisterComponents('Exemples', [TDbf]);
  RegisterPropertyEditor(TypeInfo(string), TDbf, 'TableName', TTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDbf, 'RunTimePath', TRunTimePathProperty);
  RegisterPropertyEditor(TypeInfo(string), TDbf, 'DesignTimePath', TDesignTimePathProperty);
//  RegisterPropertyEditor(TypeInfo(TStrings), TDbf, 'IndexFiles', TIndexFilesProperty);
//  ShowMessage(ToolServices.GetProjectName);
end;
{$endif fpc}

initialization
    _PagedFiles := TList.Create;
    tDbf_TrimFields := true;

finalization
    _PagedFiles.free;

end.
