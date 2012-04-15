{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2012 by the Free Pascal development team

    Basic indexer
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
 
unit fpIndexer;

{$mode objfpc}{$H+}

{ $define LangDetect}

interface

uses
  Classes, SysUtils;

type
  TWordTokenType = (wtOr, wtAnd, wtWord);

  TWordToken = record
    Value: string;
    TokenType: TWordTokenType;
  end;

  TIgnoreListDef = class;

  { TWordParser }

  TWordParser = class
  private
    FCount: integer;
    FWildCardChar: char;
    WordList: array of TWordToken;

    procedure AddToken(AValue: string; ATokenType: TWordTokenType);
    function GetSearchWordQuery: string;
    function GetToken(index: integer): TWordToken;
    procedure SetCount(AValue: integer);
  public
    constructor Create(ASearchWords: string);

    property Count: integer read FCount write SetCount;
    property WildCardChar: char read FWildCardChar write FWildCardChar;
    property SearchWordQuery: string read GetSearchWordQuery;
    property Token[index: integer]: TWordToken read GetToken;
  end;

  TSearchOption = (soContains);
  TSearchOptions = set of TSearchOption;

  TSearchWordData = record
    Context: string;
    FileDate: TDateTime;
    Language: string;
    Position: int64;
    Rank: integer;
    SearchWord: string;
    URL: string;
  end;

  TFPSearch = class;

  { TCustomIndexDB }

  TCustomIndexDB = class(TComponent)
  public
    procedure CreateDB; virtual; abstract;
    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual;
    procedure CompactDB; virtual; abstract;
    procedure BeginTrans; virtual; abstract;
    procedure CommitTrans; virtual; abstract;
    procedure DeleteWordsFromFile(URL: string); virtual; abstract;
    procedure AddSearchData(ASearchData: TSearchWordData); virtual; abstract;
    procedure FindSearchData(SearchWord: TWordParser; FPSearch: TFPSearch; SearchOptions: TSearchOptions); virtual; abstract;
    procedure CreateIndexerTables; virtual; abstract;
  end;

  TDatabaseID = record
    Name: string;
    ID: integer;
  end;

  { ---------------------------------------------------------------------
    SQL-Based databases support
    ---------------------------------------------------------------------}

  TIndexTable = (itWords, itLanguages, itFiles, itMatches);
  TIndexIndex = (iiWords, iiMatches, iiLanguages, iiFiles);
  TIndexField = (ifWordsID, ifWordsWord,
    ifMatchesID, ifMatchesWordId, ifMatchesFileID, ifMatchesLanguageID,
    ifMatchesPosition, ifMatchesContext, ifLanguagesID, ifLanguagesName,
    ifFilesID, ifFilesURL, ifFilesReindex, ifFilesUpdated, ifFilesTimeStamp,
    ifFilesLanguageID);
  TIndexForeignKey = (ikFilesLanguage, ikMatchesWord, ikMatchesFile, ikMatchesLanguage);
  TIndexTables = set of TIndexTable;
  TIndexIndexes = set of TIndexIndex;
  TIndexFields = set of TIndexField;

const
  MaxContextLen = 255;
  TableFields: array[TIndexField] of TIndexTable =
    (itWords, itWords,
    itMatches, itMatches, itMatches, itMatches, itMatches, itMatches,
    itLanguages, itLanguages,
    itFiles, itFiles, itFiles, itFiles, itFiles, itFiles);

  DefaultTableNames: array[TIndexTable] of string = ('WORDS', 'FILELANGUAGES', 'FILENAMES', 'WORDMATCHES');
  DefaultIndexNames: array[TIndexIndex] of string = ('I_WORDS', 'I_WORDMATCHES', 'I_FILELANGUAGES', 'I_FILENAMES');
  DefaultFieldNames: array[TIndexField] of string = (
    'W_ID', 'W_WORD',
    'WM_ID', 'WM_WORD_FK', 'WM_FILE_FK', 'WM_LANGUAGE_FK', 'WM_POSITION', 'WM_CONTEXT',
    'FL_ID', 'FL_NAME',
    'FN_ID', 'FN_URL', 'FN_REINDEX', 'FN_UPDATED', 'FN_TIMESTAMP', 'FN_LANGUAGE_FK');
  ForeignKeyTables: array[TIndexForeignKey] of TIndexTable = (itFiles, itMatches, itMatches, itMatches);
  ForeignKeyTargets: array[TIndexForeignKey] of TIndexTable = (itLanguages, itWords, itFiles, itLanguages);
  ForeignKeyFields: array[TIndexForeignKey] of TIndexField = (ifFilesLanguageID, ifMatchesWordID, ifMatchesFileID, ifMatchesLanguageID);
  ForeignKeyTargetFields: array[TIndexForeignKey] of TIndexField = (ifLanguagesID, ifWordsID, ifFilesID, ifLanguagesID);
  DefaultForeignKeyNames: array[TIndexForeignKey] of string = ('R_FILES_LANGUAGE', 'R_MATCHES_WORD', 'R_MATCHES_FILE', 'R_MATCHES_LANGUAGE');
  IdFieldType = 'BIGINT NOT NULL';
  PrimaryFieldType = IdFieldType + ' PRIMARY KEY';
  PosFieldType = 'BIGINT';
  FlagFieldType = 'SMALLINT';
  TextFieldType = 'VARCHAR(100) NOT NULL';
  LargeTextFieldType = 'VARCHAR(255) NOT NULL';
  TimeStampFieldType = 'TIMESTAMP';
  DefaultFieldTypes: array[TIndexField] of string = (
    PrimaryFieldType, TextFieldType, PrimaryFieldType, IdFieldType, IdFieldType,
    IdFieldType, PosFieldType, LargeTextFieldType, PrimaryFieldType, TextFieldType,
    PrimaryFieldType, LargeTextFieldType, FlagFieldType, FlagFieldType, TimeStampFieldType,
    IdFieldType);

type

  { TSQLIndexDB }

  TSQLIndexDB = class(TCustomIndexDB)
  protected
    function CreateForeignKey(const ForeignKey: TIndexForeignKey; ForCreate: boolean = False): string;
    function CreateIndexSQL(const AIndexName, ATableName: string; const AFieldList: array of string): string; virtual;
    function CreateTableIndex(IndexType: TIndexIndex): string; virtual;
    function CreateTableSQL(const TableType: TIndexTable): string; virtual;
    function DeleteWordsSQL(UseParams: boolean = True): string; virtual;
    function DropTableSQl(TableType: TIndexTable): string; virtual;
    function GetFieldName(FieldType: TIndexField): string; virtual;
    function GetFieldType(FieldType: TIndexField): string; virtual;
    function GetForeignKeyName(ForeignKey: TIndexForeignKey): string; virtual;
    function GetIndexName(IndexType: TIndexIndex): string; virtual;
    function GetLanguageSQL(UseParams: boolean = True): string; virtual;
    function GetMatchSQL(SearchOptions: TSearchOptions; SearchWord: TWordParser; UseParams: boolean = True): string; virtual;
    function GetSearchFileSQL(UseParams: boolean = True): string; virtual;
    function GetSearchSQL(ATable: TIndexTable; IDField, SearchField: TINdexField; UseParams: boolean = True): string; virtual;
    function GetTableName(TableType: TIndexTable): string; virtual;
    function GetUrlSQL(UseParams: boolean = True): string; virtual;
    function GetWordSQL(UseParams: boolean = True): string; virtual;
    function InsertSQL(const TableType: TIndexTable; UseParams: boolean = True): string; virtual;
    procedure FinishCreateTable(const TableType: TIndexTable); virtual;
  protected
    class function AllowForeignKeyInTable: boolean; virtual;
    procedure Execute(const sql: string; ignoreErrors: boolean = True); virtual; abstract;
    function GetURLID(const URL: string; ATimeStamp: TDateTime; ALanguageID: int64; DoCreate: boolean = True): int64; virtual; abstract;
  public
    procedure CreateIndexerTables; override;
    procedure DeleteWordsFromFile(URL: string); override;
  end;

  TCustomFileReader = class;

  TOnSearchWordEvent = procedure(AReader: TCustomFileReader; var AWord: TSearchWordData) of object;
  { TCustomFileReader }

  TCustomFileReader = class
  private
    FCount: integer;
    FDetectLanguage: boolean;
    FIgnoreNumeric: boolean;
    FLanguage: string;
    FOnAdd: TOnSearchWordEvent;
    FSearchWord: array of TSearchWordData;
    FStream: TStream;
    FStreamPos: integer;
    FUseIgnoreList: boolean;
    FIgnoreListDef: TIgnoreListDef;
    FNoListFound: boolean;
    FTokenStartPos: integer;
    FContext: string;
    function GetCapacity: integer;
    function GetSearchWord(index: integer): TSearchWordData;
    procedure SetCapacity(AValue: integer);
    procedure SetStream(AValue: TStream);
    procedure SetStreamPos(AValue: integer);
  protected
    function AllowedToken(token: string): boolean; virtual;
    function GetToken: string; virtual;
    function GetContext: string;
    function AllowWord(var ASearchWord: TSearchWordData): boolean;
    procedure Add(var ASearchWord: TSearchWordData);
    procedure DoDetectLanguage;
    property Stream: TStream read FStream write SetStream;
    property StreamPos: integer read FStreamPos write SetStreamPos;
    property TokenStartPos: integer read FTokenStartPos;
  public
    constructor Create;
    destructor Destroy; override;
    property DetectLanguage: boolean read FDetectLanguage write FDetectLanguage;
    property Language: string read FLanguage write FLanguage;
    procedure LoadFromStream(FileStream: TStream); virtual;
    property SearchWord[index: integer]: TSearchWordData read GetSearchWord;
    property Count: integer read FCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property OnAddSearchWord: TOnSearchWordEvent read FOnAdd write FOnAdd;
    property UseIgnoreList: boolean read FUseIgnoreList write FUseIgnoreList;
    property IgnoreNumeric: boolean read FIgnoreNumeric write FIgnoreNumeric;
  end;

  TCustomFileReaderClass = class of TCustomFileReader;


  TAddWordStub = class(TObject)
  private
    FCount: int64;
    FURL: string;
    FDateTime: TDateTime;
    FDatabase: TCustomIndexDB;
  public
    constructor Create(const AURL: string; const ADateTime: TDateTime; ADatabase: TCustomIndexDB);
    procedure DoAddWord(AReader: TCustomFileReader; var AWord: TSearchWordData); virtual;
    property Count: int64 read FCount;
  end;

  { TFPIndexer }

  TIndexProgressEvent = procedure(Sender: TObject; const ACurrent, ACount: integer; const AURL: string) of object;

  TFPIndexer = class(TComponent)
  private
    FCommitFiles: boolean;
    FDatabase: TCustomIndexDB;
    FDetectLanguage: boolean;
    FErrorCount: int64;
    FExcludeFileMask: string;
    FFileMask: string;
    FIgnoreNumeric: boolean;
    FLanguage: string;
    FOnProgress: TIndexProgressEvent;
    FSearchPath: string;
    FSearchRecursive: boolean;
    FUseIgnoreList: boolean;
    ExcludeMaskPatternList: TStrings;
    MaskPatternList: TStrings;
    procedure SetDatabase(AValue: TCustomIndexDB);
    procedure SetExcludeFileMask(AValue: string);
    procedure SetFileMask(AValue: string);
    procedure SetSearchPath(AValue: string);
  protected
    procedure DoProgress(const ACurrent, ACount: integer; const URL: string); virtual;
    procedure SearchFiles(const PathName, FileName: string; const Recursive: boolean; AList: TStrings); virtual;
    procedure ExcludeFiles(const ExcludeMask: string; AList: TStrings); virtual;
  public
    constructor Create(AOwner: TComponent); overload;
    destructor Destroy; override;

    function IndexStream(const AURL: string; ADateTime: TDateTime; S: TStream; Reader: TCustomFileReader): int64; virtual;
    function IndexFile(AURL: string; AllowErrors: boolean; const ALanguage: string = ''): int64;
    function Execute(AllowErrors: boolean): int64;
    property ErrorCount: int64 read FErrorCount;
    property Language: string read FLanguage write FLanguage;
    property OnProgress: TIndexProgressEvent read FOnProgress write FOnProgress;
    property UseIgnoreList: boolean read FUseIgnoreList write FUseIgnoreList;
    property IgnoreNumeric: boolean read FIgnoreNumeric write FIgnoreNumeric;
    property CommitFiles: boolean read FCommitFiles write FCommitFiles;
  published
    property Database: TCustomIndexDB read FDatabase write SetDatabase;
    property ExcludeFileMask: string read FExcludeFileMask write SetExcludeFileMask;
    property FileMask: string read FFileMask write SetFileMask;
    property SearchPath: string read FSearchPath write SetSearchPath;
    property SearchRecursive: boolean read FSearchRecursive write FSearchRecursive;
    property DetectLanguage: boolean read FDetectLanguage write FDetectLanguage;
  end;

  { TFileReaderDef }

  TFileReaderDef = class(TCollectionItem)
  private
    FExtensions, FTypeName, FDefaultExt: string;
    FReader: TCustomFileReaderClass;
  public
    function HandlesExtension(const Ext: string): boolean; virtual;
    function CreateReader(const AURL: string): TCustomFileReader; virtual;
    procedure DisposeReader(AReader: TCustomFileReader); virtual;
    property Extensions: string read FExtensions write FExtensions;
    property TypeName: string read FTypeName write FTypeName;
    property DefaultExt: string read FDefaultExt write FDefaultExt;
    property Reader: TCustomFileReaderClass read FReader write FReader;
  end;

  { TFileReaderDefs }

  TFileReaderDefs = class(TCollection)
  private
    function GetD(AIndex: integer): TFileReaderDef;
    procedure SetD(AIndex: integer; AValue: TFileReaderDef);
  public
    function AddFileReader(const ATypeName: string): TFileReaderDef;
    function IndexOfTypeName(const ATypeName: string): integer;
    property Defs[AIndex: integer]: TFileReaderDef read GetD write SetD; default;
  end;

  { TFileHandlersManager }

  TFileHandlersManager = class
  private
    FData: TFileReaderDefs;
    function GetCount: integer;
    function GetData(const ATypeName: string): TFileReaderDef;
    function GetData(index: integer): TFileReaderDef;
    function GetDefExt(const TypeName: string): string;
    function GetExt(const TypeName: string): string;
    function GetReader(const TypeName: string): TCustomFileReaderClass;
    function GetTypeName(index: integer): string;
  protected
    function CreateFileReaderDefs: TFileReaderDefs; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDefsForExtension(const Extension: string; List: TStrings): integer;
    procedure RegisterFileReader(const ATypeName, TheExtensions: string; AReader: TCustomFileReaderClass);
    property Count: integer read GetCount;
    property DefaultExtension[const TypeName: string]: string read GetDefExt;
    property Extensions[const TypeName: string]: string read GetExt;
    property FileReader[const TypeName: string]: TCustomFileReaderClass read GetReader;
    property TypeNames[index: integer]: string read GetTypeName;
  end;

  { TFPSearch }

  TFPSearch = class (TComponent)
  private
    FCount: integer;
    FDatabase: TCustomIndexDB;
    FOptions: TSearchOptions;
    FRankedCount: integer;
    FSearchWord: TWordParser;
    ResultList: array of TSearchWordData;
    RankedList: array of TSearchWordData;
    function GetRankedResults(index: integer): TSearchWordData;
    function GetResults(index: integer): TSearchWordData;
    procedure SetDatabase(AValue: TCustomIndexDB);
    procedure RankResults;
    procedure SetRankedCount(AValue: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: int64;
    procedure AddResult(index: integer; AValue: TSearchWordData);
    property Count: integer read FCount;
    property RankedCount: integer read FRankedCount write SetRankedCount;
    property Results[index: integer]: TSearchWordData read GetResults;
    property RankedResults[index: integer]: TSearchWordData read GetRankedResults;
    procedure SetSearchWord(AValue: string);
  published
    property Database: TCustomIndexDB read FDatabase write SetDatabase;
    property Options: TSearchOptions read FOptions write FOptions;
    property SearchWord: TWordParser read FSearchWord;
  end;

  { TIgnoreListDef }

  TIgnoreListDef = class(TCollectionItem)
  private
    FLanguage: string;
    FList: TStrings;
    procedure SetStrings(AValue: TStrings);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure BeginLoading;
    procedure EndLoading;
    function IgnoreWord(const AWord: string): boolean;
    property List: TStrings read FList write SetStrings;
    property Language: string read FLanguage write FLanguage;
  end;

  { TIgnoreLists }

  TIgnoreLists = class(TCollection)
  private
    function getL(AIndex: integer): TIgnoreListDef;
    procedure SetL(AIndex: integer; AValue: TIgnoreListDef);
  public
    function IndexOfLanguage(const ALanguage: string): integer;
    function FindLanguage(const ALanguage: string): TIgnoreListDef;
    function LanguageByName(const ALanguage: string): TIgnoreListDef;
    function AddLanguage(const ALanguage: string): TIgnoreListDef;
    property Lists[AIndex: integer]: TIgnoreListDef read getL write SetL; default;
  end;

  { TIgnoreListManager }

  TIgnoreListManager = class(TComponent)
  private
    FLists: TIgnoreLists;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterIgnoreWords(const ALanguage: string; AList: TStrings);
    procedure LoadIgnoreWordsFromFile(const ALanguage, AFileName: string);
    property Lists: TIgnoreLists read FLists;
  end;

  EFPIndexer = class(Exception);

var
  FileHandlers: TFileHandlersManager;
  IgnoreListManager: TIgnoreListManager;

function DateToISO8601(DateTime: TDateTime): string;
function ISO8601ToDate(DateTime: string): TDateTime;

function QuoteString(S: string): string;

implementation

uses
  {$ifdef LangDetect}
     fpTextCat, Math,
  {$endif}
  fpmasks;     //please note that this is an LCL unit, should be moved to FCL afaic

resourcestring
  SErrNoSuchLanguage = 'Unknown language : "%s".';

function DateToISO8601(DateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd', DateTime) + 'T' +
            FormatDateTime('hh:mm:ss', DateTime)
end;

function ISO8601ToDate(DateTime: string): TDateTime;
begin
  Result := EncodeDate(StrToInt(copy(DateTime, 1, 4)),
                       StrToInt(copy(DateTime, 6, 2)),
                       StrToInt(copy(DateTime, 9, 2))) +
            EncodeTime(StrToInt(copy(DateTime, 12, 2)),
                       StrToInt(copy(DateTime, 15, 2)),
                       StrToInt(copy(DateTime, 18, 2)),
                       0);
end;

function QuoteString(S: string): string;
begin
  Result := '''' + S + '''';
end;

function CalcDefExt(TheExtensions: string): string;
var
  p: integer;
begin
  p := pos(';', TheExtensions);
  if p = 0 then
    Result := TheExtensions
  else
    Result := Copy(TheExtensions, 1, p - 1);
end;

{ TIgnoreListManager }

constructor TIgnoreListManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLists := TIgnoreLists.Create(TIgnoreListDef);
end;

destructor TIgnoreListManager.Destroy;
begin
  FreeAndNil(FLists);
  inherited Destroy;
end;

procedure TIgnoreListManager.RegisterIgnoreWords(const ALanguage: string; AList: TStrings);

var
  L: TIgnoreListDef;

begin
  L := FLists.FindLanguage(ALanguage);
  if (L = nil) then
  begin
    L := FLists.AddLanguage(ALanguage);
  end;
  L.BeginLoading;
  try
    L.List.AddStrings(AList);
  finally
    L.EndLoading;
  end;
end;

procedure TIgnoreListManager.LoadIgnoreWordsFromFile(const ALanguage, AFileName: string);

var
  L: TStringList;

begin
  L := TStringList.Create;
  try
    L.LoadFromFile(AFileName);
    RegisterIgnoreWords(ALanguage, L)
  finally
    L.Free;
  end;
end;

{ TIgnoreLists }

function TIgnoreLists.getL(AIndex: integer): TIgnoreListDef;
begin
  Result := TIgnoreListDef(Items[AIndex]);
end;

procedure TIgnoreLists.SetL(AIndex: integer; AValue: TIgnoreListDef);
begin
  Items[AIndex] := AValue;
end;

function TIgnoreLists.IndexOfLanguage(const ALanguage: string): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (CompareText(ALanguage, GetL(Result).Language) <> 0) do
    Dec(Result);
end;

function TIgnoreLists.FindLanguage(const ALanguage: string): TIgnoreListDef;

var
  i: integer;

begin
  I := IndexOfLanguage(ALanguage);
  if (I = -1) then
    Result := nil
  else
    Result := GetL(I);
end;

function TIgnoreLists.LanguageByName(const ALanguage: string): TIgnoreListDef;
begin
  Result := FindLanguage(Alanguage);
  if (Result = nil) then
    raise EFPIndexer.CreateFmt(SErrNoSuchLanguage, [ALanguage]);
end;

function TIgnoreLists.AddLanguage(const ALanguage: string): TIgnoreListDef;
begin
  Result := Add as TIgnoreListDef;
  Result.Language := ALanguage;
end;

{ TIgnoreListDef }

procedure TIgnoreListDef.SetStrings(AValue: TStrings);
begin
  if FList = AValue then
    exit;
  FList.Assign(AValue);
end;

constructor TIgnoreListDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FList := TStringList.Create;
end;

destructor TIgnoreListDef.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TIgnoreListDef.BeginLoading;
begin
  TStringList(FList).Sorted := False;
end;

procedure TIgnoreListDef.EndLoading;
begin
  TStringList(FList).Sorted := True;
end;

function TIgnoreListDef.IgnoreWord(const AWord: string): boolean;
begin
  Result := FList.IndexOf(AWord) <> -1;
end;

{ TCustomIndexDB }

procedure TCustomIndexDB.Disconnect;
begin
  // Do nothing
end;

function TFileReaderDef.HandlesExtension(const Ext: string): boolean;
begin
  Result := Pos(lowercase(ext) + ';', FExtensions + ';') <> 0;
end;

function TFileReaderDef.CreateReader(const AURL: string): TCustomFileReader;
begin
  Result := FReader.Create;
end;

procedure TFileReaderDef.DisposeReader(AReader: TCustomFileReader);
begin
  AReader.Free;
end;

{ TFileReaderDefs }

function TFileReaderDefs.GetD(AIndex: integer): TFileReaderDef;
begin
  Result := TFileReaderDef(Items[AIndex]);
end;

procedure TFileReaderDefs.SetD(AIndex: integer; AValue: TFileReaderDef);
begin
  Items[AIndex] := AValue;
end;

function TFileReaderDefs.AddFileReader(const ATypeName: string): TFileReaderDef;
begin
  Result := Add as TFileReaderDef;
  Result.FTypeName := ATypeName;
end;

function TFileReaderDefs.IndexOfTypeName(const ATypeName: string): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (CompareText(AtypeName, GetD(Result).TypeName) <> 0) do
    Dec(Result);
end;


{ TWordParser }

procedure TWordParser.AddToken(AValue: string; ATokenType: TWordTokenType);
begin
  Count := Count + 1;

  //insert an OR if two following tokens are of type wtWord
  if (FCount > 1) and (WordList[FCount - 2].TokenType = wtWord) and (ATokenType = wtWord) then
  begin
    WordList[FCount - 1].Value := 'or';
    WordList[FCount - 1].TokenType := wtOR;
    Count := Count + 1;
  end;

  WordList[FCount - 1].Value := AValue;
  WordList[FCount - 1].TokenType := ATokenType;
end;

function TWordParser.GetSearchWordQuery: string;
var
  s: string;
  i: integer;
begin
  s := '';
  for i := 0 to FCount - 1 do
    if i = FCount - 1 then
      s := S+WordList[i].Value
    else
      s := S+WordList[i].Value + ' ';

  //replace wildcard '*' with the SQL variant '%'
  Result := StringReplace(s, '*', WildCardChar, [rfReplaceAll, rfIgnoreCase]);
end;

function TWordParser.GetToken(index: integer): TWordToken;
begin
  if (index >= 0) and (index < FCount) then
    Result := WordList[index];

  Result.Value := StringReplace(Result.Value, '*', WildCardChar, [rfReplaceAll, rfIgnoreCase]);
end;

procedure TWordParser.SetCount(AValue: integer);
begin
  if FCount = AValue then
    Exit;
  FCount := AValue;

  SetLength(WordList, FCount);
end;

constructor TWordParser.Create(ASearchWords: string);
var
  list: TStringList;
  i: integer;
begin
  //erase list
  FCount := 0;

  FWildCardChar := '%';

  list := TStringList.Create;

  try
    list.Delimiter := ' ';
    list.StrictDelimiter := True;

    list.DelimitedText := LowerCase(ASearchWords);

    //create the search clause
    for i := 0 to list.Count - 1 do
    begin
      if list[i] = 'or' then
        AddToken('or', wtOR)
      else
      begin
        if list[i] = 'and' then
          AddToken('and', wtOR)
        else
          AddToken(QuoteString(list[i]), wtWord);
      end;
    end;
  finally
    FreeAndNil(list);
  end;
end;

{ TFPSearch }

procedure TFPSearch.SetDatabase(AValue: TCustomIndexDB);
begin
  if FDatabase = AValue then
    exit;
  FDatabase := AValue;
end;

procedure TFPSearch.RankResults;
var
  i: integer;
  best_value: TSearchWordData;
  best_j: integer;
  j: integer;

  procedure AddNewRankedItem(Data: TSearchWordData);
  begin
    //add item to ranked list
    RankedCount := RankedCount + 1;
    RankedList[FRankedCount - 1] := Data;
    RankedList[FRankedCount - 1].Rank := 1;
  end;

begin
  for i := 0 to FCount - 1 do
  begin
    if FRankedCount > 0 then
    begin
      if RankedList[FRankedCount - 1].URL <> ResultList[i].URL then
        AddNewRankedItem(ResultList[i])
      else
        RankedList[FRankedCount - 1].Rank := RankedList[FRankedCount - 1].Rank+ 1;
    end
    else
      AddNewRankedItem(ResultList[i]);
  end;

  //sort ranked list
  for i := 0 to FRankedCount - 2 do
  begin
    // Find the smallest remaining item.
    best_value := RankedList[i];
    best_j := i;
    for j := i + 1 to FRankedCount - 1 do
      if (RankedList[j].Rank > best_value.Rank) then
      begin
        best_value := RankedList[j];
        best_j := j;
      end;

    // Swap it into position.
    RankedList[best_j] := RankedList[i];
    RankedList[i] := best_value;
  end;
end;

procedure TFPSearch.SetRankedCount(AValue: integer);
begin
  if FRankedCount = AValue then
    Exit;

  FRankedCount := AValue;
  SetLength(RankedList, AValue);
end;

procedure TFPSearch.AddResult(index: integer; AValue: TSearchWordData);
begin
  //grow result list if needed
  if index >= Count then
  begin
    FCount := index;
    SetLength(ResultList, FCount + 1);
  end;

  ResultList[index] := AValue;
end;

procedure TFPSearch.SetSearchWord(AValue: string);
begin
  if Assigned(FSearchWord) then
    FreeAndNil(FSearchWord);

  FSearchWord := TWordParser.Create(AValue);
  FSearchWord.WildCardChar := '%';   //should come from DataBase
end;

function TFPSearch.GetResults(index: integer): TSearchWordData;
begin
  Result := ResultList[index];
end;

function TFPSearch.GetRankedResults(index: integer): TSearchWordData;
begin
  Result := RankedList[index];
end;

constructor TFPSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCount := 0;
  FRankedCount := 0;
end;

destructor TFPSearch.Destroy;
begin
  inherited Destroy;
end;

function TFPSearch.Execute: int64;
begin
  Result := 0;

  //reset previous searches
  FCount := 0;
  SetLength(ResultList, FCount);
  Database.Connect;
  Database.FindSearchData(SearchWord, Self, Options);
  Result := Count;

  //rank the results
  RankResults;
end;

{ TCustomFileReader }

function TCustomFileReader.GetSearchWord(index: integer): TSearchWordData;
begin
  Result := FSearchWord[index];
end;

function TCustomFileReader.GetCapacity: integer;
begin
  Result := Length(FSearchWord);
end;

procedure TCustomFileReader.SetCapacity(AValue: integer);
begin
  SetLength(FSearchWord, AValue);
end;

//a very basic tokenizer that only returns numeric and alphanumeric characters
function TCustomFileReader.GetToken: string;

var
  s: string;
  c: char;
begin
  Result := '';

  if not Assigned(Stream) then
    exit;

  try
    //writeln('pos:', Stream.Position, ' size:', Stream.Size);
    if Stream.Position >= Stream.Size - 1 then
      exit;
    c := Chr(Stream.ReadByte);
    //writeln('pos:', Stream.Position, ' size:', Stream.Size);
    if Stream.Position >= Stream.Size - 1 then
      exit;

    //eat all invalid characters
    while not (C in ['a'..'z', 'A'..'Z', '0'..'9']) and (Stream.Position < Stream.Size) do
      c := Chr(Stream.ReadByte);
    S := c;
    //now read all valid characters from stream and append
    FTokenStartPos := Stream.Position;
    c := Chr(Stream.ReadByte);
    while (c in ['a'..'z', 'A'..'Z', '0'..'9']) and (Stream.Position < Stream.Size) do
    begin
      s :=S+ c;
      c := chr(Stream.ReadByte);
    end;
    FContext := FContext + (' ' + S);
    if (Length(FContext) > MaxContextlen) then
      Delete(FContext, 1, Length(FContext) - MaxContextLen);
    Result := s;
  except
  end;
end;

function TCustomFileReader.GetContext: string;
begin
  Result := FContext;
end;

function TCustomFileReader.AllowWord(var ASearchWord: TSearchWordData): boolean;

var
  F: double;

begin
  Result := True;
  if FIgnoreNumeric and (Length(ASearchWord.SearchWord) < 20) then
    Result := not TryStrToFloat(ASearchWord.SearchWord, F);
  if Result and UseIgnoreList then
  begin
    if not Assigned(FIgnoreListDef) and not FNoListFound then
    begin
      if (Language <> '') then
      begin
        FIgnoreListDef := IgnoreListManager.Lists.FindLanguage(Language);
        FNoListFound := FIgnoreListDef = nil;
      end;
    end;
    if Assigned(FIgnoreListDef) then
    begin
      Result := not FIgnoreListDef.IgnoreWord(ASearchWord.SearchWord);
    end;
  end;
end;

function TCustomFileReader.AllowedToken(token: string): boolean;
begin
  Result := True;
end;

procedure TCustomFileReader.SetStream(AValue: TStream);
begin
  if FStream = AValue then
    Exit;
  FStream := AValue;
  StreamPos := 0;
  FStream.Seek(0, soFromBeginning);
  FContext := '';
end;

procedure TCustomFileReader.SetStreamPos(AValue: integer);
begin
  if FStreamPos = AValue then
    Exit;
  FStreamPos := AValue;
end;

procedure TCustomFileReader.Add(var ASearchWord: TSearchWordData);

var
  C: integer;

begin
  if not AllowWord(ASearchWord) then
    exit;
  Inc(FCount);
  if (FOnAdd <> nil) then
    FonAdd(Self, AsearchWord)
  else
  begin
    C := Capacity;
    if (FCount > C) then
      if (C < 10) then
        C := 10
      else
        Capacity := C + C div 2;
    FSearchWord[FCount - 1] := ASearchWord;
  end;
end;

procedure TCustomFileReader.DoDetectLanguage;
{$ifdef LangDetect}
var
  tc: TFPTextCat;
  i: integer;
  s: string = '';
{$endif}
begin
{$ifdef LangDetect}
  tc := TFPTextCat.Create;
  try
    for i := 0 to Min(1000, Count - 1) do
      s :=S+ FSearchWord[i].SearchWord + ' ';

    tc.LoadFromString(s);
    tc.Classify;
    FLanguage := tc.Language;
  finally
    FreeAndNil(tc);
  end;
{$endif}
end;

constructor TCustomFileReader.Create;
begin
  FCount := 0;
  FLanguage := 'unknown';
  FignoreNumeric := True;
end;

destructor TCustomFileReader.Destroy;
begin
  SetLength(FSearchWord, 0);
  inherited Destroy;
end;

procedure TCustomFileReader.LoadFromStream(FileStream: TStream);
begin
  Stream := FileStream;
  if (FOnAdd = nil) then
    Capacity := Stream.Size div 10;
  FNoListFound := False;
end;

{ TFileHandlersManager }

function TFileHandlersManager.GetReader(const TypeName: string): TCustomFileReaderClass;
var
  ih: TFileReaderDef;
begin
  ih := GetData(TypeName);
  if assigned(ih) then
    Result := ih.FReader
  else
    Result := nil;
end;

function TFileHandlersManager.GetExt(const TypeName: string): string;

var
  ih: TFileReaderDef;
begin
  ih := GetData(TypeName);
  if assigned(ih) then
    Result := ih.Extensions
  else
    Result := '';
end;

function TFileHandlersManager.GetDefExt(const TypeName: string): string;
var
  ih: TFileReaderDef;
begin
  ih := GetData(TypeName);
  if assigned(ih) then
    Result := ih.FDefaultExt
  else
    Result := '';
end;

function TFileHandlersManager.GetTypeName(index: integer): string;
var
  ih: TFileReaderDef;
begin
  ih := TFileReaderDef(FData[index]);
  Result := ih.FTypeName;
end;

function TFileHandlersManager.GetData(const ATypeName: string): TFileReaderDef;
var
  r: integer;
begin
  r := FData.IndexOfTypeName(ATypeName);
  if r >= 0 then
    Result := FData[r]
  else
    Result := nil;
end;

function TFileHandlersManager.GetData(index: integer): TFileReaderDef;
begin
  if (index >= 0) and (index < FData.Count) then
    Result := TFileReaderDef(FData[index])
  else
    Result := nil;
end;

function TFileHandlersManager.GetCount: integer;
begin
  Result := FData.Count;
end;

constructor TFileHandlersManager.Create;
begin
  inherited Create;
  FData := CreateFileReaderDefs;
end;

function TFileHandlersManager.CreateFileReaderDefs: TFileReaderDefs;
begin
  Result := TFileReaderDefs.Create(TFileReaderDef);
end;

destructor TFileHandlersManager.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

{$note function result is not set, convert to procedure?}
function TFileHandlersManager.GetDefsForExtension(const Extension: string; List: TStrings): integer;
var
  I: integer;
  D: TFileReaderDef;
begin
  for I := 0 to FData.Count - 1 do
  begin
    D := FData[i];
    if D.HandlesExtension(Extension) and (D.Reader <> nil) then
      List.AddObject(D.TypeName, D);
  end;
end;

procedure TFileHandlersManager.RegisterFileReader(const ATypeName, TheExtensions: string; AReader: TCustomFileReaderClass);
var
  ih: TFileReaderDef;
begin
  ih := GetData(ATypeName);
  if assigned(ih) then
  begin
    if assigned(ih.FReader) then
      raise EFPindexer.CreateFmt('File reader "%s" already registered', [ATypeName]);
  end
  else
  begin
    ih := FData.AddFileReader(ATypeName);
    with ih do
    begin
      Extensions := Lowercase(TheExtensions);
      DefaultExt := CalcDefExt(TheExtensions);
    end;
  end;
  ih.FReader := AReader;
end;

{ TAddWordStub }

constructor TAddWordStub.Create(const AURL: string; const ADateTime: TDateTime; ADatabase: TCustomIndexDB);
begin
  FURL := AURl;
  FDateTime := ADateTime;
  FDatabase := ADatabase;
  FCount := 0;
end;

procedure TAddWordStub.DoAddWord(AReader: TCustomFileReader; var AWord: TSearchWordData);
begin
  AWord.URL := FURL;
  AWord.FileDate := FDateTime;
  AWord.Language := AReader.Language;
  AWord.SearchWord := LowerCase(AWord.SearchWord);
  FDataBase.AddSearchData(AWord);
  Inc(FCount);
end;


{ TFPIndexer }

procedure TFPIndexer.SetDatabase(AValue: TCustomIndexDB);
begin
  if FDatabase = AValue then
    Exit;
  FDatabase := AValue;
end;

procedure TFPIndexer.SetExcludeFileMask(AValue: string);
begin
  if FExcludeFileMask = AValue then
    exit;

  FExcludeFileMask := AValue;
  ExcludeMaskPatternList.DelimitedText := FExcludeFileMask;
end;

procedure TFPIndexer.SearchFiles(const PathName, FileName: string; const Recursive: boolean; AList: TStrings);
var
  Rec: TSearchRec;
  Path: string;
begin
  Path := IncludeTrailingBackslash(PathName);
  try
    if FindFirst(Path + FileName, faAnyFile - faDirectory, Rec) = 0 then
      repeat
        AList.Add(Path + Rec.Name);
      until FindNext(Rec) <> 0;
  finally
    FindClose(Rec);
  end;
  if not Recursive then
    Exit;
  if FindFirst(Path + AllFilesMask, faDirectory, Rec) = 0 then
    try
      repeat
        if ((Rec.Attr and faDirectory) <> 0) and (Rec.Name <> '.') and (Rec.Name <> '..') then
          SearchFiles(Path + Rec.Name, FileName, True, AList);
      until FindNext(Rec) <> 0;
    finally
      FindClose(Rec);
    end;
end;

procedure TFPIndexer.ExcludeFiles(const ExcludeMask: string; AList: TStrings);
var
  i: integer;
begin
  for i := AList.Count - 1 downto 0 do
    if MatchesMask(AList[i], ExcludeMask) then
      AList.Delete(i);
end;

procedure TFPIndexer.SetFileMask(AValue: string);
begin
  if FFileMask = AValue then
    exit;

  FFileMask := AValue;
  MaskPatternList.DelimitedText := FFileMask;
end;

procedure TFPIndexer.SetSearchPath(AValue: string);
begin
  if FSearchPath = AValue then
    exit;
  FSearchPath := ExtractFilePath(ExpandFileName(IncludeTrailingPathDelimiter(AValue)));
end;

procedure TFPIndexer.DoProgress(const ACurrent, ACount: integer; const URL: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, ACurrent, ACount, URL);
end;

constructor TFPIndexer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ExcludeMaskPatternList := TStringList.Create;
  ExcludeMaskPatternList.StrictDelimiter := True;
  ExcludeMaskPatternList.Delimiter := ';';

  MaskPatternList := TStringList.Create;
  MaskPatternList.StrictDelimiter := True;
  MaskPatternList.Delimiter := ';';

  DetectLanguage := False;
  FIgnoreNumeric := True;
end;

destructor TFPIndexer.Destroy;
begin
  MaskPatternList.Clear;
  FreeAndNil(MaskPatternList);

  ExcludeMaskPatternList.Clear;
  FreeAndNil(ExcludeMaskPatternList);

  inherited Destroy;
end;

function TFPIndexer.IndexStream(const AURL: string; ADateTime: TDateTime; S: TStream; Reader: TCustomFileReader): int64;
var
  i: integer;
  Stub: TAddWordStub;
  AWord: TSearchWordData;
begin
  Result := 0;
  DataBase.DeleteWordsFromFile(AURL);
  // If reader must detect language, the stub cannot be used.
  if not DetectLanguage then
  begin
    Stub := TAddWordStub.Create(AURL, ADateTime, Database);
    try
      Reader.OnAddSearchWord := @Stub.DoAddWord;
      Reader.LoadFromStream(S);
      Result := Stub.Count;
    finally
      Stub.Free;
    end;
  end
  else
  begin
    Reader.LoadFromStream(S);
    for i := 0 to Reader.Count - 1 do
    begin
      AWord := Reader.SearchWord[i];
      AWord.URL := AURL;
      AWord.FileDate := ADateTime;
      AWord.Language := Reader.Language;
      AWord.SearchWord := LowerCase(AWord.SearchWord);
      FDataBase.AddSearchData(AWord);
      Inc(Result);
    end;
  end;
  if CommitFiles then
  begin
    Database.CommitTrans;
    Database.BeginTrans;
  end;
end;

function TFPIndexer.IndexFile(AURL: string; AllowErrors: boolean; const ALanguage: string): int64;
var
  e: string;
  i: integer;
  d: TFileReaderDef;
  reader: TCustomFileReader;
  fs: TFileStream;
  L: TStringList;
  DT: TDateTime;
begin
  Result := 0;
  if not FileExists(AURL) then
    raise EFPIndexer.Create('error: could not find file: ' + AURL);
  //get cleaned file extension
  E := LowerCase(ExtractFileExt(AURL));
  if (e <> '') and (e[1] = '.') then
    Delete(e, 1, 1);
  L := TStringList.Create;
  try
    FileHandlers.GetDefsForExtension(e, L);
    for I := 0 to L.Count - 1 do
    begin
      d := L.Objects[i] as TFileReaderDef;
      reader := D.CreateReader(AURL);
      try
        try
          Reader.IgnoreNumeric := True;
          if (ALanguage <> '') then
          begin
            Reader.Language := ALanguage;
            Reader.DetectLanguage := False;
          end
          else
            Reader.DetectLanguage := DetectLanguage;
          Reader.UseIgnoreList := UseIgnoreList;
          fs := TFileStream.Create(AURL, fmOpenRead);
          try
            DT := FileDateToDateTime(FileAge(AURL));
            Result := Result + IndexStream(AURL, DT, FS, Reader);
          finally
            fs.Free;
          end
        except
          On E: Exception do
            if not AllowErrors then
              raise
            else
            begin
              Inc(FErrorCount);
            end;
        end;
      finally
        D.DisposeReader(reader);
      end;
    end;
  finally
    L.Free;
  end;
end;

function TFPIndexer.Execute(AllowErrors: boolean): int64;
var
  m: integer;
  List: TStringList;
  url: string;
begin
  Result := 0;
  FErrorCount := 0;
  if not Assigned(FDatabase) then
    raise EFPIndexer.Create('database not assigned');
  if not DirectoryExists(SearchPath) then
    raise EFPIndexer.CreateFmt('Search path "%s" does not exist', [SearchPath]);
  Database.Connect;
  try
    // execute search for each mask pattern
    List := TStringList.Create;
    try
      for m := 0 to MaskPatternList.Count - 1 do
        SearchFiles(SearchPath, MaskPatternList[m], SearchRecursive, List);
      if (List.Count > 0) then
      begin
        List.Sort;
        DataBase.BeginTrans;
        for m := 0 to List.Count - 1 do
        begin
          URL := List[m];
          DoProgress(M, List.Count, URL);
          Result := Result + IndexFile(URL, AllowErrors, Language);
        end;
        {$note perform cleanup here on orphaned search words}
        DataBase.CommitTrans;
        Database.CompactDB;
      end;
    finally
      List.Free;
    end;
  finally
    Database.Disconnect;
  end;
end;

{ TSQLIndexDB }

procedure TSQLIndexDB.CreateIndexerTables;
var
  T: TIndexTable;
  I: TIndexIndex;
  k: TIndexForeignKey;
begin

  //create a new database
  BeginTrans;

  for t := low(TIndexTable) to High(TindexTable) do
    Execute(DropTableSQl(t));
  CommitTrans;

  BeginTrans;

  for t := low(TIndexTable) to High(TindexTable) do
    Execute(CreateTableSQl(t), False);
  CommitTrans;

  BeginTrans;

  for I := low(TIndexIndex) to High(TIndexIndex) do
    Execute(CreateTableIndex(i), False);

  CommitTrans;

  BeginTrans;

  if not AllowForeignKeyInTable then
    for k := low(TIndexForeignKey) to High(TIndexForeignKey) do
      Execute(CreateForeignKey(k), False);

  CommitTrans;

  BeginTrans;

  for t := low(TIndexTable) to High(TindexTable) do
    FinishCreateTable(t);

  CommitTrans;
end;

function TSQLIndexDB.GetTableName(TableType: TIndexTable): string;
begin
  Result := DefaultTableNames[TableType];
end;

function TSQLIndexDB.GetIndexName(IndexType: TIndexIndex): string;
begin
  Result := DefaultIndexNames[IndexType];
end;

function TSQLIndexDB.GetFieldName(FieldType: TIndexField): string;
begin
  Result := DefaultFieldNames[FieldType];
end;

function TSQLIndexDB.GetForeignKeyName(ForeignKey: TIndexForeignKey): string;
begin
  Result := DefaultForeignKeyNames[ForeignKey];
end;

function TSQLIndexDB.GetFieldType(FieldType: TIndexField): string;
begin
  Result := DefaultFieldTypes[FieldType];
end;

function TSQLIndexDB.DropTableSQl(TableType: TIndexTable): string;
begin
  Result := 'DROP TABLE ' + GetTableName(TableType);
end;

function TSQLIndexDB.CreateTableSQL(const TableType: TIndexTable): string;
var
  f: TIndexField;
  K: TIndexForeignKey;
begin
  for F := Low(TIndexField) to High(TIndexField) do
    if TableFields[F] = TableType then
    begin
      if (Result <> '') then
        Result := Result + ',' + sLineBreak;
      Result := Result + GetFieldName(f) + ' ' + GetFieldType(f);
    end;
  if AllowForeignKeyInTable then
    for K := Low(TIndexForeignKey) to High(TIndexForeignKey) do
      if (ForeignKeyTables[k] = TableType) then
      begin
        if (Result <> '') then
          Result := Result + ',' + sLineBreak;
        Result := Result + CreateForeignKey(k, True);
      end;
  Result := 'CREATE TABLE ' + GetTableName(TableType) + ' (' + Result + ')';
end;

function TSQLIndexDB.CreateForeignKey(const ForeignKey: TIndexForeignKey; ForCreate: boolean = False): string;
var
  STN, TTN, FKN, FKF, FTK: string;
begin
  STN := GetTableName(ForeignKeyTables[ForeignKey]);
  TTN := GetTableName(ForeignKeyTargets[ForeignKey]);
  FKN := GetForeignKeyName(Foreignkey);
  FKF := GetFieldName(ForeignKeyFields[ForeignKey]);
  FTK := GetFieldName(ForeignKeyTargetFields[ForeignKey]);
  if ForCreate then
    Result := Format('CONSTRAINT %S FOREIGN KEY (%s) REFERENCES %S(%s)', [FKN, FKF, TTN, FTK])
  else
    Result := Format('ALTER TABLE %s ADD CONSTRAINT %S FOREIGN KEY (%s) REFERENCES %S(%s)', [STN, FKN, FKF, TTN, FTK]);
end;

procedure TSQLIndexDB.FinishCreateTable(const TableType: TIndexTable);
begin
  // Do nothing
end;

function TSQLIndexDB.InsertSQL(const TableType: TIndexTable; UseParams: boolean = True): string;
var
  FL: string = '';
  VL: string = '';
  F: TIndexField;
begin
  for F := Low(TIndexField) to High(TIndexField) do
    if TableFields[F] = TableType then
    begin
      if (FL <> '') then
      begin
        FL := FL + ', ';
        VL := VL + ', ';
      end;
      FL := FL + GetfieldName(F);
      if UseParams then
        VL := VL + ':' + GetfieldName(F)
      else
        VL := VL + '%s';
    end;
  Result := Format('INSERT INTO %s (%s) VALUES (%s)', [GetTableName(TableType), FL, VL]);
end;

function TSQLIndexDB.CreateTableIndex(IndexType: TIndexIndex): string;
var
  TIN: string;
begin
  TIN := GetindexName(IndexType);
  case IndexType of
    iiWords: Result := CreateIndexSQL(TIN, GetTableName(itWords), [GetFieldName(ifWordsWord)]);
    iiFiles: Result := CreateIndexSQL(TIN, GetTableName(itFiles), [GetFieldName(ifFilesURL)]);
    iiLanguages: Result := CreateIndexSQL(TIN, GetTableName(itLanguages), [GetFieldName(ifLanguagesName)]);
  end;
end;

function TSQLIndexDB.CreateIndexSQL(const AIndexName, ATableName: string; const AFieldList: array of string): string;
var
  I: integer;
begin
  Result := 'CREATE UNIQUE INDEX ' + AIndexName + ' ON ' + ATableName + ' (';
  for I := Low(AFieldList) to High(AFieldList) do
  begin
    Result := Result + AFieldList[i];
    if I < High(AFieldList) then
      Result := Result + ',';
  end;
  Result := Result + ');';
end;

function TSQLIndexDB.GetUrlSQL(UseParams: boolean): string;
begin
  Result := GetSearchSQL(itFiles, ifFilesID, ifFilesURL, UseParams);
end;

function TSQLIndexDB.GetSearchSQL(ATable: TIndexTable; IDField, SearchField: TINdexField; UseParams: boolean = True): string;
var
  IDF, TN, URLF: string;
begin
  TN := GetTableName(ATable);
  IDF := GetFieldName(IDField);
  URLF := GetFieldName(SearchField);
  Result := 'SELECT %s from %s where (%s = ';
  Result := Format(Result, [IDF, TN, URLF]);
  if UseParams then
    Result := Result + ':' + URLF + ')'
  else
    Result := Result + '%s)';
end;

function TSQLIndexDB.GetLanguageSQL(UseParams: boolean = True): string;
begin
  Result := GetSearchSQL(itLanguages, ifLanguagesID, ifLanguagesName, UseParams);
end;

function TSQLIndexDB.GetWordSQL(UseParams: boolean = True): string;
begin
  Result := GetSearchSQL(itWords, ifWordsID, ifWordsWord, UseParams);
end;

function TSQLIndexDB.GetSearchFileSQL(UseParams: boolean = True): string;
begin
  Result := GetSearchSQL(itFiles, ifFilesID, ifFilesURL, UseParams);
end;

function TSQLIndexDB.DeleteWordsSQL(UseParams: boolean): string;
begin
  Result := Format('DELETE FROM %s WHERE (%s =', [GetTableName(itMatches), GetFieldName(ifMatchesFileID)]);
  if UseParams then
    Result := Result + ':' + GetFieldName(ifMatchesFileID) + ')'
  else
    Result := Result + '%d)';
end;

class function TSQLIndexDB.AllowForeignKeyInTable: boolean;
begin
  Result := False;
end;

function TSQLIndexDB.GetMatchSQL(SearchOptions: TSearchOptions; SearchWord: TWordParser; UseParams: boolean = True): string;
var
  WW, MN, FN, WN, LN: string;
  i: integer;
begin
  WW := getFieldName(ifWordsWord);
  Result := Format('SELECT %s, %s, %s, %s, %s, %s', [GetFieldName(ifMatchesPosition),
    GetFieldName(ifFilesURL), GetFieldName(ifMatchesContext),
    WW, GetFieldName(ifFilesTimeStamp), GetFieldName(ifLanguagesName)]);
  MN := GetTableName(itMatches);
  FN := getTableName(itFiles);
  WN := getTableName(itWords);
  LN := getTableName(itLanguages);
  Result := Result + Format(' FROM %s, %s ,%s, %s', [MN, FN, WN, LN]);
  Result := Result + Format(' WHERE (%s.%s=%s.%s)', [MN, getFieldName(ifMatchesWordID), WN, getFieldName(ifWordsID)]);
  Result := Result + Format(' AND (%s.%s=%s.%s)', [MN, getFieldName(ifMatchesFileID), FN, getFieldName(ifFilesID)]);
  Result := Result + Format(' AND (%s.%s=%s.%s)', [LN, getFieldName(ifLanguagesID), FN, getFieldName(ifFilesLanguageID)]);
  Result := Result + ' AND (';

  for i := 0 to SearchWord.Count - 1 do
  begin
    if SearchWord.Token[i].TokenType = wtWord then
    begin
      Result := Result + Format('(%s.%s ', [WN, WW]);
      if (soContains in SearchOptions) then
        Result := Result + 'Like '
      else
        Result := Result + '= ';
      if UseParams then
        Result := Result + ':' + WW + IntToStr(i) + ')'
      else
        Result := Result + Format('%s)', [SearchWord.Token[i].Value]);
    end
    else
      Result := Result + Format(' %s ', [SearchWord.Token[i].Value]);
  end;
  Result := Result + Format(') ORDER BY %s', [GetFieldName(ifFilesURL)]);
end;

procedure TSQLIndexDB.DeleteWordsFromFile(URL: string);
var
  FID: integer;
begin
  FID := GetURLID(URL, 0, -1, False);

  if (FID <> -1) then
    Execute(Format(DeleteWordsSQL(False), [FID]), False);
end;

initialization
  FileHandlers := TFileHandlersManager.Create;
  IgnoreListManager := TIgnoreListManager.Create(nil);

finalization
  FreeAndNil(IgnoreListManager);
  FreeAndNil(FileHandlers);
end.

