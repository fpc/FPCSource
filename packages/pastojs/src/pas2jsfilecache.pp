{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018  Mattias Gaertner  mattias@freepascal.org

    Pascal to Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Abstract:
    TPas2jsFileResolver extends TFileResolver and searches source files.
}
unit Pas2jsFileCache;

{$mode objfpc}{$H+}

{$i pas2js_defines.inc}

interface

uses
  Classes, SysUtils, AVL_Tree,
  PScanner, PasResolver, FPPJsSrcMap,
  Pas2jsLogger, Pas2jsFileUtils, Pas2JsFiler;

const // Messages
  nIncludeSearch = 201; sIncludeSearch = 'Include file search: %s';
  nUnitSearch = 202; sUnitSearch = 'Unitsearch: %s';
  nSearchingFileFound = 203; sSearchingFileFound = 'Searching file: %s... found';
  nSearchingFileNotFound = 204; sSearchingFileNotFound = 'Searching file: %s... not found';
  nDuplicateFileFound = 205; sDuplicateFileFound = 'Duplicate file found: "%s" and "%s"';
  nCustomJSFileNotFound = 206; sCustomJSFileNotFound = 'custom JS file not found: "%s"';

type
  EPas2jsFileCache = class(Exception);

type
  TPas2jsFileAgeTime = longint;
  TPas2jsFileAttr = longint;
  TPas2jsFileSize = int64;
  TPas2jsSearchFileCase = (
    sfcDefault,
    sfcCaseSensitive,
    sfcCaseInsensitive
  );

  TPas2jsCachedDirectories = class;

  TPas2jsCachedDirectoryEntry = class
  public
    Name: string;
    Time: TPas2jsFileAgeTime; // modification time
    Attr: TPas2jsFileAttr;
    Size: TPas2jsFileSize;
  end;

  { TPas2jsCachedDirectory }

  TPas2jsCachedDirectory = class
  private
    FChangeStamp: TChangeStamp;
    FPath: string;
    FEntries: TFPList; // list of TPas2jsCachedDirectoryEntry
    FPool: TPas2jsCachedDirectories;
    FRefCount: integer;
    FSorted: boolean;
    function GetEntries(Index: integer): TPas2jsCachedDirectoryEntry; inline;
    procedure SetSorted(const AValue: boolean);
  protected
    procedure DoReadDir; virtual;
  public
    constructor Create(aPath: string; aPool: TPas2jsCachedDirectories);
    destructor Destroy; override;
    function Count: integer;
    procedure Clear;
    property ChangeStamp: TChangeStamp read FChangeStamp write FChangeStamp;// set on Update to Pool.ChangeStamp
    function NeedsUpdate: boolean; inline;
    procedure Update;
    procedure Reference;
    procedure Release;
    property RefCount: integer read FRefCount;
    function Add(const Name: string; Time: TPas2jsFileAgeTime;
      Attr: TPas2jsFileAttr; Size: TPas2jsFileSize): TPas2jsCachedDirectoryEntry;
    function FindFile(const ShortFilename: string;
                      const FileCase: TPas2jsSearchFileCase): string;
    function FileAge(const ShortFilename: string): TPas2jsFileAgeTime;
    function FileAttr(const ShortFilename: string): TPas2jsFileAttr;
    function FileSize(const ShortFilename: string): TPas2jsFileSize;
    function IndexOfFileCaseInsensitive(const ShortFilename: String): integer;
    function IndexOfFileCaseSensitive(const ShortFilename: String): integer;
    function IndexOfFile(const ShortFilename: String): integer; inline;
    function CountSameNamesCaseInsensitive(Index: integer): integer;
    procedure GetSameNamesCaseInsensitive(Index: integer; List: TStrings);
    property Entries[Index: integer]: TPas2jsCachedDirectoryEntry read GetEntries; default;
    procedure GetFiles(var Files: TStrings;
      IncludeDirs: boolean = true // add faDirectory as well
      ); // returns relative file names
    procedure CheckConsistency;
    procedure WriteDebugReport;
    property Path: string read FPath; // with trailing path delimiter
    property Pool: TPas2jsCachedDirectories read FPool;
    property Sorted: boolean read FSorted write SetSorted; // descending, sort first case insensitive, then sensitive
  end;

  TReadDirectoryEvent = function(Dir: TPas2jsCachedDirectory): boolean of object;// true = skip default function

  { TPas2jsCachedDirectories }

  TPas2jsCachedDirectories = class
  private
    FChangeStamp: TChangeStamp;
    FDirectories: TAVLTree;// tree of TPas2jsCachedDirectory sorted by Directory
    FWorkingDirectory: string;
  private
    FOnReadDirectory: TReadDirectoryEvent;
    type
      TFileInfo = record
        Filename: string;
        DirPath: string;
        ShortFilename: string;
        Dir: TPas2jsCachedDirectory;
      end;
    function GetFileInfo(var Info: TFileInfo): boolean;
    procedure SetWorkingDirectory(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    property ChangeStamp: TChangeStamp read FChangeStamp;
    procedure Invalidate; inline;
    procedure Clear;
    function DirectoryExists(Filename: string): boolean;
    function FileExists(Filename: string): boolean;
    function FileExistsI(var Filename: string): integer; // returns number of found files
    function FileAge(Filename: string): TPas2jsFileAgeTime;
    function FileAttr(Filename: string): TPas2jsFileAttr;
    function FileSize(Filename: string): TPas2jsFileSize;
    function FindDiskFilename(const Filename: string;
                              {%H-}SearchCaseInsensitive: boolean = false): string; // using Pascal case insensitivity, not UTF-8
    procedure GetListing(const aDirectory: string; var Files: TStrings;
        IncludeDirs: boolean = true // add faDirectory as well
        ); // returns relative file names
    function GetDirectory(const Directory: string;
                      CreateIfNotExists: boolean = true;
                      DoReference: boolean = true): TPas2jsCachedDirectory;
    property WorkingDirectory: string read FWorkingDirectory write SetWorkingDirectory; // used for relative filenames, contains trailing path delimiter
    property OnReadDirectory: TReadDirectoryEvent read FOnReadDirectory write FOnReadDirectory;
  end;

type
  TP2jsFileCacheOption = (
    caoShowFullFilenames,
    caoShowTriedUsedFiles,
    caoAllJSIntoMainJS,
    caoSearchLikeFPC
    );
  TP2jsFileCacheOptions = set of TP2jsFileCacheOption;
const
  DefaultPas2jsFileCacheOptions = [];
  p2jsfcoCaption: array[TP2jsFileCacheOption] of string = (
    // only used by experts, no need for resourcestrings
    'Show full filenames',
    'Show tried/used files',
    'Combine all JavaScript into main file',
    'Search files like FPC'
    );

  EncodingBinary = 'Binary';
type
  TPas2jsFilesCache = class;
  TPas2jsCachedFile = class;

  { TPas2jsFileResolver }

  TPas2jsFileResolver = class(TFileResolver)
  private
    FCache: TPas2jsFilesCache;
  public
    constructor Create(aCache: TPas2jsFilesCache); reintroduce;
    function FindIncludeFile(const aFilename: string): TLineReader; override;
    function FindIncludeFileName(const aFilename: string): String; reintroduce;
    function FindSourceFile(const aFilename: string): TLineReader; override;
    function FindUnitFileName(const aUnitname, InFilename: string; out IsForeign: boolean): String;
    function FindUnitJSFileName(const aUnitFilename: string): String;
    function FindCustomJSFileName(const aFilename: string): String;
    function FileExistsLogged(const Filename: string): boolean;
    function FileExistsILogged(var Filename: string): integer;
    function SearchLowUpCase(var Filename: string): boolean; virtual;
    property Cache: TPas2jsFilesCache read FCache;
  end;

  { TPas2jsFileLineReader }

  TPas2jsFileLineReader = class(TLineReader)
  private
    FCachedFile: TPas2jsCachedFile;
    FIsEOF: boolean;
    FLineNumber: integer;
    FSource: string;
    FSrcPos: PChar;
  public
    constructor Create(const AFilename: string); override;
    constructor Create(aFile: TPas2jsCachedFile); reintroduce;
    function IsEOF: Boolean; override;
    function ReadLine: string; override;
    property LineNumber: integer read FLineNumber;
    property CachedFile: TPas2jsCachedFile read FCachedFile;
    property Source: string read FSource;
    property SrcPos: PChar read FSrcPos;
  end;

  { TPas2jsCachedFile }

  TPas2jsCachedFile = class
  private
    FCache: TPas2jsFilesCache;
    FChangeStamp: TChangeStamp;
    FFileEncoding: string;
    FFilename: string;
    FLastErrorMsg: string;
    FLoaded: boolean;
    FLoadedFileAge: longint;
    FSource: string;
    FCacheStamp: TChangeStamp; // Cache.ResetStamp when file was loaded
    function GetIsBinary: boolean; inline;
  public
    constructor Create(aCache: TPas2jsFilesCache; const aFilename: string); reintroduce;
    function Load(RaiseOnError: boolean; Binary: boolean = false): boolean;
    function CreateLineReader(RaiseOnError: boolean): TPas2jsFileLineReader;
    property IsBinary: boolean read GetIsBinary;
    property FileEncoding: string read FFileEncoding;
    property Filename: string read FFilename;
    property Source: string read FSource; // UTF-8 without BOM or Binary
    property Cache: TPas2jsFilesCache read FCache;
    property ChangeStamp: TChangeStamp read FChangeStamp;// changed when Source changed
    property Loaded: boolean read FLoaded; // Source valid, but may contain an old version
    property LastErrorMsg: string read FLastErrorMsg;
    property LoadedFileAge: longint read FLoadedFileAge;// only valid if Loaded=true
  end;

  TPas2jsReadFileEvent = function(aFilename: string; var aSource: string): boolean of object;
  TPas2jsWriteFileEvent = procedure(aFilename: string; Source: string) of object;

  TPas2jsCachedFilesState = (
    cfsMainJSFileResolved
    );
  TPas2jsFileCacheStates = set of TPas2jsCachedFilesState;

  TPas2jsSearchPathKind = (
    spkPath,      // e.g. unitpaths, includepaths
    spkIdentifier // e.g. namespaces, trailing - means remove
    );

  { TPas2jsFilesCache }

  TPas2jsFilesCache = class
  private
    FBaseDirectory: string;
    FDefaultOutputPath: string;
    FDirectoryCache: TPas2jsCachedDirectories;
    FFiles: TAVLTree; // tree of TPas2jsCachedFile sorted for Filename
    FForeignUnitPaths: TStringList;
    FForeignUnitPathsFromCmdLine: integer;
    FIncludePaths: TStringList;
    FIncludePathsFromCmdLine: integer;
    FInsertFilenames: TStringList;
    FLog: TPas2jsLogger;
    FMainJSFile: string;
    FMainJSFileResolved: string; // only valid if cfsMainJSFileResolved in FStates
    FMainSrcFile: string;
    FMainSrcFileShort: string;
    FNamespaces: TStringList;
    FNamespacesFromCmdLine: integer;
    FOnReadFile: TPas2jsReadFileEvent;
    FOnWriteFile: TPas2jsWriteFileEvent;
    FOptions: TP2jsFileCacheOptions;
    FPrecompileFormat: TPas2JSPrecompileFormat;
    FReadLineCounter: SizeInt;
    FResetStamp: TChangeStamp;
    FSrcMapBaseDir: string;
    FStates: TPas2jsFileCacheStates;
    FUnitOutputPath: string;
    FUnitPaths: TStringList;
    FUnitPathsFromCmdLine: integer;
    function GetAllJSIntoMainJS: Boolean;
    function GetSearchLikeFPC: boolean;
    function GetShowFullFilenames: boolean;
    function GetShowTriedUsedFiles: boolean;
    procedure RegisterMessages;
    procedure SetAllJSIntoMainJS(AValue: Boolean);
    procedure SetBaseDirectory(AValue: string);
    function AddSearchPaths(const Paths: string; Kind: TPas2jsSearchPathKind;
      FromCmdLine: boolean; var List: TStringList; var CmdLineCount: integer): string;
    procedure SetDefaultOutputPath(AValue: string);
    procedure SetMainJSFile(AValue: string);
    procedure SetOptions(AValue: TP2jsFileCacheOptions);
    procedure SetSearchLikeFPC(const AValue: boolean);
    procedure SetShowFullFilenames(const AValue: boolean);
    procedure SetShowTriedUsedFiles(const AValue: boolean);
    procedure SetSrcMapBaseDir(const AValue: string);
    procedure SetUnitOutputPath(AValue: string);
    procedure SetOption(Flag: TP2jsFileCacheOption; Enable: boolean);
  protected
    function ReadFile(Filename: string; var Source: string): boolean; virtual;
    procedure FindMatchingFiles(Mask: string; MaxCount: integer; Files: TStrings);// find files, matching * and ?
  public
    constructor Create(aLog: TPas2jsLogger);
    destructor Destroy; override;
    procedure Reset;
    function AddIncludePaths(const Paths: string; FromCmdLine: boolean; out ErrorMsg: string): boolean;
    function AddNamespaces(const Paths: string; FromCmdLine: boolean; out ErrorMsg: string): boolean;
    function AddUnitPaths(const Paths: string; FromCmdLine: boolean; out ErrorMsg: string): boolean;
    function AddSrcUnitPaths(const Paths: string; FromCmdLine: boolean; out ErrorMsg: string): boolean;
    function CreateResolver: TPas2jsFileResolver;
    function FormatPath(const aPath: string): string;
    function GetResolvedMainJSFile: string;
    function FindFile(Filename: string): TPas2jsCachedFile;
    function LoadFile(Filename: string; Binary: boolean = false): TPas2jsCachedFile;
    function NormalizeFilename(const Filename: string; RaiseOnError: boolean): string;
    procedure InsertCustomJSFiles(aWriter: TPas2JSMapper);
    function IndexOfInsertJSFilename(const aFilename: string): integer;
    procedure AddInsertJSFilename(const aFilename: string);
    procedure RemoveInsertJSFilename(const aFilename: string);
    procedure GetListing(const aDirectory: string; var Files: TStrings;
                         FullPaths: boolean = true);
    procedure RaiseDuplicateFile(aFilename: string);
    procedure SaveToFile(ms: TMemoryStream; Filename: string);
    function ExpandDirectory(const Filename, BaseDir: string): string;
  public
    property AllJSIntoMainJS: Boolean read GetAllJSIntoMainJS write SetAllJSIntoMainJS;
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory; // includes trailing pathdelim
    property MainOutputPath: string read FDefaultOutputPath write SetDefaultOutputPath; // includes trailing pathdelim
    property DirectoryCache: TPas2jsCachedDirectories read FDirectoryCache;
    property ForeignUnitPaths: TStringList read FForeignUnitPaths;
    property ForeignUnitPathsFromCmdLine: integer read FForeignUnitPathsFromCmdLine;
    property IncludePaths: TStringList read FIncludePaths;
    property IncludePathsFromCmdLine: integer read FIncludePathsFromCmdLine;
    property InsertFilenames: TStringList read FInsertFilenames;
    property Log: TPas2jsLogger read FLog;
    property MainJSFile: string read FMainJSFile write SetMainJSFile;
    property MainSrcFileShort: string read FMainSrcFileShort write FMainSrcFileShort;
    property MainSrcFile: string read FMainSrcFile write FMainSrcFile;
    property Namespaces: TStringList read FNamespaces;
    property NamespacesFromCmdLine: integer read FNamespacesFromCmdLine;
    property Options: TP2jsFileCacheOptions read FOptions write SetOptions default DefaultPas2jsFileCacheOptions;
    property PrecompileFormat: TPas2JSPrecompileFormat read FPrecompileFormat write FPrecompileFormat;
    property ReadLineCounter: SizeInt read FReadLineCounter write FReadLineCounter;
    property ResetStamp: TChangeStamp read FResetStamp;
    property SearchLikeFPC: boolean read GetSearchLikeFPC write SetSearchLikeFPC;
    property ShowFullPaths: boolean read GetShowFullFilenames write SetShowFullFilenames;
    property ShowTriedUsedFiles: boolean read GetShowTriedUsedFiles write SetShowTriedUsedFiles;
    property SrcMapBaseDir: string read FSrcMapBaseDir write SetSrcMapBaseDir; // includes trailing pathdelim
    property UnitOutputPath: string read FUnitOutputPath write SetUnitOutputPath; // includes trailing pathdelim
    property UnitPaths: TStringList read FUnitPaths;
    property UnitPathsFromCmdLine: integer read FUnitPathsFromCmdLine;
    property OnReadFile: TPas2jsReadFileEvent read FOnReadFile write FOnReadFile;
    property OnWriteFile: TPas2jsWriteFileEvent read FOnWriteFile write FOnWriteFile;
  end;

function CompareFilenameWithCachedFile(Filename, CachedFile: Pointer): integer;
function CompareCachedFiles(File1, File2: Pointer): integer;
function ComparePas2jsCacheDirectories(Dir1, Dir2: Pointer): integer;
function CompareAnsiStringWithDirectoryCache(Path, DirCache: Pointer): integer;
function ComparePas2jsDirectoryEntries(Entry1, Entry2: Pointer): integer;
function CompareFirstCaseInsThenSensitive(const s, h: string): integer;

// UTF-8 helper functions
function ConvertTextToUTF8(const Src: string; var SrcEncoding: string): string;
function GuessEncoding(const Src: string): string;
function HasUTF8BOM(const s: string): boolean;
function RemoveUTFBOM(const s: string): string;

implementation

function CompareFilenameWithCachedFile(Filename, CachedFile: Pointer): integer;
var
  Cache: TPas2jsCachedFile absolute CachedFile;
begin
  Result:=CompareFilenames(AnsiString(Filename),Cache.Filename);
end;

function CompareCachedFiles(File1, File2: Pointer): integer;
var
  Cache1: TPas2jsCachedFile absolute File1;
  Cache2: TPas2jsCachedFile absolute File2;
begin
  Result:=CompareFilenames(Cache1.Filename,Cache2.Filename);
end;

function ComparePas2jsCacheDirectories(Dir1, Dir2: Pointer): integer;
var
  Directory1: TPas2jsCachedDirectory absolute Dir1;
  Directory2: TPas2jsCachedDirectory absolute Dir2;
begin
  Result:=CompareFilenames(Directory1.Path,Directory2.Path);
end;

function CompareAnsiStringWithDirectoryCache(Path, DirCache: Pointer): integer;
var
  Directory: TPas2jsCachedDirectory absolute DirCache;
begin
  Result:=CompareFilenames(AnsiString(Path),Directory.Path);
end;

function ComparePas2jsDirectoryEntries(Entry1, Entry2: Pointer): integer;
var
  E1: TPas2jsCachedDirectoryEntry absolute Entry1;
  E2: TPas2jsCachedDirectoryEntry absolute Entry2;
begin
  Result:=CompareFirstCaseInsThenSensitive(E1.Name,E2.Name);
end;

function CompareFirstCaseInsThenSensitive(const s, h: string): integer;
begin
  Result:=CompareText(s,h);
  if Result<>0 then exit;
  Result:=CompareStr(s,h);
end;

function ConvertTextToUTF8(const Src: string; var SrcEncoding: string): string;
var
  p: PChar;
  NormSrcEncoding: String;
begin
  Result:=Src;
  if SrcEncoding='' then
    SrcEncoding:=GuessEncoding(Src);
  if Result='' then exit;
  NormSrcEncoding:=NormalizeEncoding(SrcEncoding);
  if NormSrcEncoding=NormalizeEncoding(EncodingUTF8) then
  begin
    p:=PChar(Result);
    if (p^=#$EF) and (p[1]=#$BB) and (p[2]=#$BF) then
    begin
      // cut out UTF-8 BOM
      Delete(Result,1,3);
    end;
  end else if (NormSrcEncoding=EncodingSystem)
      or (NormSrcEncoding=GetDefaultTextEncoding) then
  begin
    Result:=SystemCPToUTF8(Result);
  end else
    EPas2jsFileCache.Create('invalid encoding "'+SrcEncoding+'"');
end;

function GuessEncoding(const Src: string): string;
var
  p: PChar;
  l: SizeInt;
  i: Integer;
begin
  if Src='' then exit(EncodingUTF8);

  if HasUTF8BOM(Src) then
    // UTF-8 BOM
    exit(EncodingUTF8);

  // try UTF-8 (this includes ASCII)
  l:=length(Src);
  p:=PChar(Src);
  repeat
    if ord(p^)<128 then
    begin
      // ASCII
      if (p^=#0) and (p-PChar(Src)>=l) then
        exit(EncodingUTF8);
      inc(p);
    end else begin
      i:=UTF8CharacterStrictLength(p);
      if i=0 then
        break;
      inc(p,i);
    end;
  until false;

  // check binary
  p:=PChar(Src);
  repeat
    case p^ of
    #0:
      if (p-PChar(Src)>=l) then
        break
      else
        exit(EncodingBinary);
    #1..#8,#11,#14..#31:
      exit(EncodingBinary);
    end;
    inc(p);
  until false;

  // use system encoding
  Result:=GetDefaultTextEncoding;
end;

function HasUTF8BOM(const s: string): boolean;
var
  p: PChar;
begin
  if s='' then exit(false);
  p:=PChar(s);
  Result:=(p^=#$EF) and (p[1]=#$BB) and (p[2]=#$BF);
end;

function RemoveUTFBOM(const s: string): string;
begin
  Result:=s;
  if not HasUTF8BOM(Result) then exit;
  Delete(Result,1,3);
end;

{ TPas2jsCachedDirectory }

// inline
function TPas2jsCachedDirectory.IndexOfFile(const ShortFilename: String
  ): integer;
begin
  {$IFDEF CaseInsensitiveFilenames}
  Result:=IndexOfFileCaseInsensitive(ShortFilename);
  {$ELSE}
  Result:=IndexOfFileCaseSensitive(ShortFilename);
  {$ENDIF}
end;

// inline
function TPas2jsCachedDirectory.GetEntries(Index: integer
  ): TPas2jsCachedDirectoryEntry;
begin
  Result:=TPas2jsCachedDirectoryEntry(FEntries[Index]);
end;

// inline
function TPas2jsCachedDirectory.NeedsUpdate: boolean;
begin
  Result:=(Pool.ChangeStamp<>FChangeStamp) or (FChangeStamp=InvalidChangeStamp);
end;

procedure TPas2jsCachedDirectory.SetSorted(const AValue: boolean);
begin
  if FSorted=AValue then Exit;
  FSorted:=AValue;
  if not FSorted then exit;
  FEntries.Sort(@ComparePas2jsDirectoryEntries); // sort descending
end;

procedure TPas2jsCachedDirectory.DoReadDir;
var
  Info: TUnicodeSearchRec;
begin
  if Assigned(Pool.OnReadDirectory) then
    if Pool.OnReadDirectory(Self) then exit;

  // Note: do not add a 'if not DirectoryExists then exit'.
  // This will not work on automounted directories. You must use FindFirst.
  if FindFirst(UnicodeString(Path+AllFilesMask),faAnyFile,Info)=0 then
  begin
    repeat
      // check if special file
      if (Info.Name='.') or (Info.Name='..') or (Info.Name='')
      then
        continue;
      // add file
      Add(String(Info.Name),Info.Time,Info.Attr,Info.Size);
    until FindNext(Info)<>0;
  end;
  FindClose(Info);
end;

constructor TPas2jsCachedDirectory.Create(aPath: string;
  aPool: TPas2jsCachedDirectories);
begin
  FRefCount:=1;
  FPath:=IncludeTrailingPathDelimiter(aPath);
  FEntries:=TFPList.Create;
  FPool:=aPool;
end;

destructor TPas2jsCachedDirectory.Destroy;
begin
  Clear;
  FreeAndNil(FEntries);
  inherited Destroy;
end;

function TPas2jsCachedDirectory.Count: integer;
begin
  Result:=FEntries.Count;
end;

procedure TPas2jsCachedDirectory.Clear;
var
  i: Integer;
begin
  for i:=0 to FEntries.Count-1 do
    TObject(FEntries[i]).Free;
  FEntries.Clear;
  FSorted:=true;
end;

procedure TPas2jsCachedDirectory.Update;
begin
  if not NeedsUpdate then exit;
  Clear;
  DoReadDir;
  FChangeStamp:=Pool.ChangeStamp;
  Sorted:=true;
  {$IFDEF VerbosePas2JSDirCache}
  writeln('TPas2jsCachedDirectories.Update "',Path,'" Count=',Count);
  CheckConsistency;
  {$ENDIF}
end;

procedure TPas2jsCachedDirectory.Reference;
begin
  inc(FRefCount);
end;

procedure TPas2jsCachedDirectory.Release;
begin
  if FRefCount<1 then
    raise Exception.Create('TPas2jsCachedDirectory.Release [20180126090800] "'+Path+'"');
  dec(FRefCount);
  if FRefCount=0 then Free;
end;

function TPas2jsCachedDirectory.Add(const Name: string;
  Time: TPas2jsFileAgeTime; Attr: TPas2jsFileAttr; Size: TPas2jsFileSize
  ): TPas2jsCachedDirectoryEntry;
begin
  Result:=TPas2jsCachedDirectoryEntry.Create;
  Result.Name:=Name;
  Result.Time:=Time;
  Result.Attr:=Attr;
  Result.Size:=Size;
  FEntries.Add(Result);
  FSorted:=false;
end;

function TPas2jsCachedDirectory.FindFile(const ShortFilename: string;
  const FileCase: TPas2jsSearchFileCase): string;
var
  i: Integer;
begin
  case FileCase of
    sfcCaseSensitive: i:=IndexOfFileCaseSensitive(ShortFilename);
    sfcCaseInsensitive: i:=IndexOfFileCaseInsensitive(ShortFilename);
  else
    i:=IndexOfFile(ShortFilename);
  end;
  if i>=0 then
    Result:=Entries[i].Name
  else
    Result:='';
end;

function TPas2jsCachedDirectory.FileAge(const ShortFilename: string
  ): TPas2jsFileAgeTime;
var
  i: Integer;
begin
  i:=IndexOfFile(ShortFilename);
  if i>=0 then
    Result:=Entries[i].Time
  else
    Result:=-1;
end;

function TPas2jsCachedDirectory.FileAttr(const ShortFilename: string
  ): TPas2jsFileAttr;
var
  i: Integer;
begin
  i:=IndexOfFile(ShortFilename);
  if i>=0 then
    Result:=Entries[i].Attr
  else
    Result:=0;
end;

function TPas2jsCachedDirectory.FileSize(const ShortFilename: string
  ): TPas2jsFileSize;
var
  i: Integer;
begin
  i:=IndexOfFile(ShortFilename);
  if i>=0 then
    Result:=Entries[i].Size
  else
    Result:=-1;
end;

function TPas2jsCachedDirectory.IndexOfFileCaseInsensitive(
  const ShortFilename: String): integer;
var
  l, r, cmp, m: Integer;
  Entry: TPas2jsCachedDirectoryEntry;
begin
  Sorted:=true;
  l:=0;
  r:=Count-1;
  while l<=r do begin
    m:=(l+r) shr 1;
    Entry:=Entries[m];
    cmp:=CompareText(Entry.Name,ShortFilename);
    if cmp>0 then
      r:=m-1
    else if cmp<0 then
      l:=m+1
    else
      exit(m);
  end;
  Result:=-1;
end;

function TPas2jsCachedDirectory.IndexOfFileCaseSensitive(
  const ShortFilename: String): integer;
var
  l, r, cmp, m: Integer;
  Entry: TPas2jsCachedDirectoryEntry;
begin
  Sorted:=true;
  l:=0;
  r:=Count-1;
  while l<=r do begin
    m:=(l+r) shr 1;
    Entry:=Entries[m];
    cmp:=CompareFirstCaseInsThenSensitive(Entry.Name,ShortFilename);
    if cmp>0 then
      r:=m-1
    else if cmp<0 then
      l:=m+1
    else
      exit(m);
  end;
  Result:=-1;
end;

function TPas2jsCachedDirectory.CountSameNamesCaseInsensitive(Index: integer
  ): integer;
var
  i: Integer;
  Filename: String;
begin
  Filename:=Entries[Index].Name;
  Result:=1;
  for i:=Index-1 downto 0 do
  begin
    if CompareText(Entries[i].Name,Filename)<>0 then break;
    inc(Result);
  end;
  for i:=Index+1 to Count-1 do
  begin
    if CompareText(Entries[i].Name,Filename)<>0 then break;
    inc(Result);
  end;
end;

procedure TPas2jsCachedDirectory.GetSameNamesCaseInsensitive(Index: integer;
  List: TStrings);
var
  i: Integer;
  Filename: String;
begin
  Filename:=Entries[Index].Name;
  List.Add(Filename);
  for i:=Index-1 downto 0 do
  begin
    if CompareText(Entries[i].Name,Filename)<>0 then break;
    List.Add(Entries[i].Name);
  end;
  for i:=Index+1 to Count-1 do
  begin
    if CompareText(Entries[i].Name,Filename)<>0 then break;
    List.Add(Entries[i].Name);
  end;
end;

procedure TPas2jsCachedDirectory.GetFiles(var Files: TStrings;
  IncludeDirs: boolean);
var
  i: Integer;
  Entry: TPas2jsCachedDirectoryEntry;
begin
  if Files=nil then
    Files:=TStringList.Create;
  if (Self=nil) or (Path='') then exit;
  Update;
  for i:=0 to Count-1 do begin
    Entry:=Entries[i];
    if IncludeDirs or ((Entry.Attr and faDirectory)=0) then
      Files.Add(Entry.Name);
  end;
end;

procedure TPas2jsCachedDirectory.CheckConsistency;
{AllowWriteln}

  procedure E(Msg: string);
  begin
    WriteDebugReport;
    writeln('TPas2jsCachedDirectory.CheckConsistency Failed for "',Path,'": '+Msg);
  end;

var
  i, cmp, j: Integer;
  Entry, LastEntry: TPas2jsCachedDirectoryEntry;
begin
  if Path<>IncludeTrailingPathDelimiter(Path) then
    E('Path<>IncludeTrailingPathDelimiter(Path)');
  LastEntry:=nil;
  for i:=0 to Count-1 do begin
    Entry:=Entries[i];
    if (Entry.Name='') or (Entry.Name='.') or (Entry.Name='..') then
      E('invalid entry "'+Entry.Name+'"');
    if (Entry.Size<0) then
      E('invalid size "'+Entry.Name+'" '+IntToStr(Entry.Size));
    if Sorted then
    begin
      if (LastEntry<>nil) then
      begin
        if LastEntry.Name=Entry.Name then
          E('duplicate "'+Entry.Name+'"');
        cmp:=CompareText(LastEntry.Name,Entry.Name);
        if cmp>0 then
          E('sorted wrong case insensitive "'+LastEntry.Name+'" "'+Entry.Name+'"');
        if (cmp=0) and (CompareStr(LastEntry.Name,Entry.Name)>0) then
          E('sorted wrong case sensitive "'+LastEntry.Name+'" "'+Entry.Name+'"');
      end;
      j:=IndexOfFileCaseSensitive(Entry.Name);
      if i<>j then
        E('IndexOfFileCaseSensitive failed "'+Entry.Name+'" expected '+IntToStr(i)+', but was '+IntToStr(j));
    end;
    LastEntry:=Entry;
  end;
  {AllowWriteln-}
end;

procedure TPas2jsCachedDirectory.WriteDebugReport;
var
  i: Integer;
  Entry: TPas2jsCachedDirectoryEntry;
begin
  {AllowWriteln}
  writeln('TPas2jsCachedDirectory.WriteDebugReport Count=',Count,' Path="',Path,'"');
  for i:=0 to Count-1 do begin
    Entry:=Entries[i];
    writeln(i,' "',Entry.Name,'" Size=',Entry.Size,' Time=',DateTimeToStr(FileDateToDateTime(Entry.Time)),' Dir=',faDirectory and Entry.Attr>0);
  end;
  {AllowWriteln-}
end;

{ TPas2jsCachedDirectories }

function TPas2jsCachedDirectories.GetFileInfo(var Info: TFileInfo): boolean;
begin
  Info.Filename:=ChompPathDelim(ResolveDots(Info.Filename));
  if Info.Filename='' then exit(false);
  if not FilenameIsAbsolute(Info.Filename) then
    Info.Filename:=WorkingDirectory+Info.Filename;
  Info.ShortFilename:=ExtractFilename(Info.Filename);
  Info.DirPath:=ExtractFilePath(Info.Filename);
  if (Info.ShortFilename<>'') and (Info.ShortFilename<>'.') and (Info.ShortFilename<>'..')
  then
  begin
    Info.Dir:=GetDirectory(Info.DirPath,true,false);
  end else begin
    Info.Dir:=nil;
  end;
  Result:=true;
end;

procedure TPas2jsCachedDirectories.SetWorkingDirectory(const AValue: string);
begin
  FWorkingDirectory:=IncludeTrailingPathDelimiter(ResolveDots(AValue));
end;

constructor TPas2jsCachedDirectories.Create;
begin
  IncreaseChangeStamp(FChangeStamp);
  FDirectories:=TAVLTree.Create(@ComparePas2jsCacheDirectories);
end;

destructor TPas2jsCachedDirectories.Destroy;
begin
  Clear;
  FreeAndNil(FDirectories);
  inherited Destroy;
end;

procedure TPas2jsCachedDirectories.Invalidate;
begin
  IncreaseChangeStamp(FChangeStamp);
end;

procedure TPas2jsCachedDirectories.Clear;
var
  Node: TAVLTreeNode;
  Dir: TPas2jsCachedDirectory;
begin
  Node:=FDirectories.FindLowest;
  while Node<>nil do begin
    Dir:=TPas2jsCachedDirectory(Node.Data);
    if Dir.FRefCount<>1 then
      raise Exception.Create('TPas2jsCachedDirectories.Clear [20180126090807] "'+Dir.Path+'" '+IntToStr(Dir.FRefCount));
    Dir.Release;
    Node.Data:=nil;
    Node:=FDirectories.FindSuccessor(Node);
  end;
  FDirectories.Clear;
end;

function TPas2jsCachedDirectories.DirectoryExists(Filename: string): boolean;
var
  Info: TFileInfo;
begin
  Info.Filename:=Filename;
  if not GetFileInfo(Info) then exit(false);
  if Info.Dir<>nil then
    Result:=(Info.Dir.FileAttr(Info.ShortFilename) and faDirectory)>0
  else
    Result:=SysUtils.DirectoryExists(Info.Filename);
end;

function TPas2jsCachedDirectories.FileExists(Filename: string): boolean;
var
  Info: TFileInfo;
begin
  Info.Filename:=Filename;
  if not GetFileInfo(Info) then exit(false);
  if Info.Dir<>nil then
    Result:=Info.Dir.IndexOfFile(Info.ShortFilename)>=0
  else
    Result:=SysUtils.FileExists(Info.Filename);
end;

function TPas2jsCachedDirectories.FileExistsI(var Filename: string): integer;
var
  Info: TFileInfo;
  i: Integer;
begin
  Result:=0;
  Info.Filename:=Filename;
  if not GetFileInfo(Info) then exit;
  if Info.Dir=nil then
  begin
    if SysUtils.FileExists(Info.Filename) then
      Result:=1;
  end
  else
  begin
    i:=Info.Dir.IndexOfFileCaseInsensitive(Info.ShortFilename);
    if i<0 then
      exit;
    Filename:=Info.Dir.Path+Info.Dir[i].Name;
    Result:=Info.Dir.CountSameNamesCaseInsensitive(i);
  end;
end;

function TPas2jsCachedDirectories.FileAge(Filename: string): TPas2jsFileAgeTime;
var
  Info: TFileInfo;
begin
  Info.Filename:=Filename;
  if GetFileInfo(Info) and (Info.Dir<>nil) then
    Result:=Info.Dir.FileAge(Info.ShortFilename)
  else
    Result:=-1;
end;

function TPas2jsCachedDirectories.FileAttr(Filename: string): TPas2jsFileAttr;
var
  Info: TFileInfo;
begin
  Info.Filename:=Filename;
  if GetFileInfo(Info) and (Info.Dir<>nil) then
    Result:=Info.Dir.FileAttr(Info.ShortFilename)
  else
    Result:=0;
end;

function TPas2jsCachedDirectories.FileSize(Filename: string): TPas2jsFileSize;
var
  Info: TFileInfo;
begin
  Info.Filename:=Filename;
  if GetFileInfo(Info) and (Info.Dir<>nil) then
    Result:=Info.Dir.FileSize(Info.ShortFilename)
  else
    Result:=-1;
end;

function TPas2jsCachedDirectories.FindDiskFilename(const Filename: string;
  SearchCaseInsensitive: boolean): string;
var
  ADirectory: String;
  Cache: TPas2jsCachedDirectory;
  DiskShortFilename: String;
begin
  Result:=ChompPathDelim(ResolveDots(Filename));
  if Result='' then exit;
  //debugln(['TPas2jsCachedDirectories.FindDiskFilename Filename=',Result]);
  {$IF defined(NotLiteralFilenames) or defined(CaseInsensitiveFilenames)}
  {$ELSE}
  if (not SearchCaseInsensitive) then exit;
  {$ENDIF}
  ADirectory:=ExtractFilePath(Result);
  if ADirectory=Result then
    exit; // root directory, e.g. / under Linux or C: under Windows
  if SearchCaseInsensitive then
    // search recursively all directory parts
    ADirectory:=IncludeTrailingPathDelimiter(FindDiskFilename(ADirectory,true));
  Cache:=GetDirectory(ADirectory,true,false);
  //debugln(['TPas2jsCachedDirectories.FindDiskFilename Dir=',Cache.Directory]);
  Result:=ExtractFileName(Result);
  DiskShortFilename:=Cache.FindFile(Result,sfcCaseInsensitive);
  //debugln(['TPas2jsCachedDirectories.FindDiskFilename DiskShortFilename=',DiskShortFilename]);
  if DiskShortFilename<>'' then Result:=DiskShortFilename;
  Result:=Cache.Path+Result;
end;

procedure TPas2jsCachedDirectories.GetListing(const aDirectory: string;
  var Files: TStrings; IncludeDirs: boolean);
begin
  GetDirectory(aDirectory,true,false).GetFiles(Files,IncludeDirs);
end;

function TPas2jsCachedDirectories.GetDirectory(const Directory: string;
  CreateIfNotExists: boolean; DoReference: boolean): TPas2jsCachedDirectory;
var
  Dir: String;
  Node: TAVLTreeNode;
begin
  Dir:=ResolveDots(Directory);
  if not FilenameIsAbsolute(Dir) then
    Dir:=WorkingDirectory+Dir;
  Dir:=IncludeTrailingPathDelimiter(Dir);
  Node:=FDirectories.FindKey(Pointer(Dir),@CompareAnsiStringWithDirectoryCache);
  if Node<>nil then
  begin
    Result:=TPas2jsCachedDirectory(Node.Data);
    if DoReference then
      Result.Reference;
    Result.Update;
  end else if DoReference or CreateIfNotExists then
  begin
    {$IFDEF VerbosePas2JSDirCache}
    writeln('TPas2jsCachedDirectories.GetDirectory "',Dir,'"');
    {$ENDIF}
    Result:=TPas2jsCachedDirectory.Create(Dir,Self);
    FDirectories.Add(Result);
    if DoReference then
      Result.Reference;
    Result.Update;
  end else
    Result:=nil;
end;

{ TPas2jsFileLineReader }

constructor TPas2jsFileLineReader.Create(const AFilename: string);
begin
  raise Exception.Create('TPas2jsFileLineReader.Create [20180126090825] no cache "'+AFilename+'"');
end;

constructor TPas2jsFileLineReader.Create(aFile: TPas2jsCachedFile);
begin
  inherited Create(aFile.Filename);
  FCachedFile:=aFile;
  FSource:=aFile.Source;
  FSrcPos:=PChar(FSource);
  FIsEOF:=FSource='';
end;

function TPas2jsFileLineReader.IsEOF: Boolean;
begin
  Result:=FIsEOF;
end;

function TPas2jsFileLineReader.ReadLine: string;
var
  p: PChar;

  procedure GetLine;
  var
    l: SizeInt;
  begin
    l:=p-FSrcPos;
    SetLength(Result,l);
    if l>0 then
      Move(FSrcPos^,Result[1],l);
    FSrcPos:=p;
    inc(FLineNumber);
    if (CachedFile<>nil) and (CachedFile.Cache<>nil) then
      inc(CachedFile.Cache.FReadLineCounter);
    //writeln('GetLine "',Result,'"');
  end;

var
  c: Char;
begin
  if FIsEOF then exit('');
  p:=FSrcPos;
  repeat
    c:=p^;
    case c of
    #0:
      if p-PChar(FSource)=length(FSource) then
      begin
        FIsEOF:=true;
        GetLine;
        exit;
      end;
    #10,#13:
      begin
        GetLine;
        inc(p);
        if (p^ in [#10,#13]) and (p^<>c) then inc(p);
        if (p^=#0) and (p-PChar(FSource)=length(FSource)) then
          FIsEOF:=true;
        FSrcPos:=p;
        exit;
      end;
    end;
    inc(p);
  until false;
  Result:='';
end;

{ TPas2jsCachedFile }

// inline
function TPas2jsCachedFile.GetIsBinary: boolean;
begin
  Result:=FFileEncoding=EncodingBinary;
end;

constructor TPas2jsCachedFile.Create(aCache: TPas2jsFilesCache;
  const aFilename: string);
begin
  FChangeStamp:=InvalidChangeStamp;
  FCache:=aCache;
  FCacheStamp:=Cache.ResetStamp;
  FFilename:=aFilename;
end;

function TPas2jsCachedFile.Load(RaiseOnError: boolean; Binary: boolean
  ): boolean;

  procedure Err(const ErrorMsg: string);
  begin
    FLastErrorMsg:=ErrorMsg;
    if RaiseOnError then
      raise EPas2jsFileCache.Create(FLastErrorMsg);
  end;

var
  NewSource: string;
begin
  {$IFDEF VerboseFileCache}
  writeln('TPas2jsCachedFile.Load START "',Filename,'" Loaded=',Loaded);
  {$ENDIF}
  if Loaded then
  begin
    // already loaded, check if it still valid
    if (Cache.ResetStamp=FCacheStamp) then
    begin
      // nothing changed
      Result:=FLastErrorMsg='';
      if (not Result) and RaiseOnError then
        raise EPas2jsFileCache.Create(FLastErrorMsg);
      exit;
    end;
    {$IFDEF VerboseFileCache}
    writeln('TPas2jsCachedFile.Load CHECK FILEAGE "',Filename,'"');
    {$ENDIF}
    if LoadedFileAge=Cache.DirectoryCache.FileAge(Filename) then
      exit(true);
  end;
  {$IFDEF VerboseFileCache}
  writeln('TPas2jsCachedFile.Load FIRST or RELOAD ',Filename,' Loaded=',Loaded);
  {$ENDIF}
  // needs (re)load
  Result:=false;
  if not Cache.DirectoryCache.FileExists(Filename) then
  begin
    Err('File not found "'+Filename+'"');
    exit;
  end;
  if Cache.DirectoryCache.DirectoryExists(Filename) then
  begin
    Err('File is a directory "'+Filename+'"');
    exit;
  end;
  NewSource:='';
  if not Cache.ReadFile(Filename,NewSource) then exit;
  {$IFDEF VerboseFileCache}
  writeln('TPas2jsCachedFile.Load ENCODE ',Filename,' FFileEncoding=',FFileEncoding);
  {$ENDIF}
  if Binary then
  begin
    FSource:=NewSource;
    FFileEncoding:=EncodingBinary;
  end else
    FSource:=ConvertTextToUTF8(NewSource,FFileEncoding);
  FLoaded:=true;
  FCacheStamp:=Cache.ResetStamp;
  FLoadedFileAge:=Cache.DirectoryCache.FileAge(Filename);
  {$IFDEF VerboseFileCache}
  writeln('TPas2jsCachedFile.Load END ',Filename,' FFileEncoding=',FFileEncoding);
  {$ENDIF}
end;

function TPas2jsCachedFile.CreateLineReader(RaiseOnError: boolean
  ): TPas2jsFileLineReader;
begin
  if not Load(RaiseOnError) then
    exit(nil);
  Result:=TPas2jsFileLineReader.Create(Self);
end;

{ TPas2jsFileResolver }

constructor TPas2jsFileResolver.Create(aCache: TPas2jsFilesCache);
begin
  inherited Create;
  FCache:=aCache;
end;

function TPas2jsFileResolver.FindIncludeFile(const aFilename: string): TLineReader;
var
  Filename: String;
begin
  Result:=nil;
  Filename:=FindIncludeFileName(aFilename);
  if Filename='' then exit;
  try
    Result := TFileLineReader.Create(Filename);
  except
    // error is shown in the scanner, which has the context information
  end;
end;

function TPas2jsFileResolver.FindIncludeFileName(const aFilename: string): String;

  function SearchCasedInIncPath(const Filename: string): string;
  var
    i: Integer;
  begin
    // file name is relative
    // first search in the same directory as the unit
    if BaseDirectory<>'' then
      begin
      Result:=BaseDirectory+Filename;
      if SearchLowUpCase(Result) then exit;
      end;
    // then search in include path
    for i:=0 to Cache.IncludePaths.Count-1 do begin
      Result:=Cache.IncludePaths[i]+Filename;
      if SearchLowUpCase(Result) then exit;
    end;
    Result:='';
  end;

var
  Filename : string;
begin
  Result := '';

  // convert pathdelims to system
  Filename:=SetDirSeparators(aFilename);
  if Cache.ShowTriedUsedFiles then
    Cache.Log.LogMsgIgnoreFilter(nIncludeSearch,[Filename]);

  if FilenameIsAbsolute(Filename) then
  begin
    Result:=Filename;
    if not SearchLowUpCase(Result) then
      Result:='';
    exit;
  end;

  // search with the given file extension (even if no ext)
  Result:=SearchCasedInIncPath(Filename);
  if Result<>'' then exit;

  if ExtractFileExt(Filename)='' then
  begin
    // search with the default file extensions
    Result:=SearchCasedInIncPath(Filename+'.inc');
    if Result<>'' then exit;
    Result:=SearchCasedInIncPath(Filename+'.pp');
    if Result<>'' then exit;
    Result:=SearchCasedInIncPath(Filename+'.pas');
    if Result<>'' then exit;
  end;
end;

function TPas2jsFileResolver.FindSourceFile(const aFilename: string): TLineReader;
var
  CurFilename: String;
  Found: Boolean;
  i: Integer;
begin
  Result:=nil;
  CurFilename:=aFilename;
  if StrictFileCase or Cache.SearchLikeFPC then
    Found:=Cache.DirectoryCache.FileExists(CurFilename)
  else
  begin
    i:=Cache.DirectoryCache.FileExistsI(CurFilename);
    Found:=i=1;
    if i>1 then
      Cache.RaiseDuplicateFile(CurFilename);
  end;
  if not Found then
    raise EFileNotFoundError.Create(aFilename)
  else
    Result:=Cache.LoadFile(CurFilename).CreateLineReader(false);
end;

function TPas2jsFileResolver.FindUnitFileName(const aUnitname,
  InFilename: string; out IsForeign: boolean): String;

  function SearchInDir(Dir: string; var Filename: string): boolean;
  // search in Dir for pp, pas, p times given case, lower case, upper case
  begin
    Filename:=Dir+aUnitname+'.pp';
    if SearchLowUpCase(Filename) then exit(true);
    Filename:=Dir+aUnitname+'.pas';
    if SearchLowUpCase(Filename) then exit(true);
    Filename:=Dir+aUnitname+'.p';
    if SearchLowUpCase(Filename) then exit(true);
    Result:=false;
  end;

var
  i: Integer;
  aFilename: String;
begin
  Result:='';
  IsForeign:=false;

  if InFilename<>'' then
  begin
    aFilename:=SetDirSeparators(InFilename);
    Result:=ResolveDots(aFilename);
    if FilenameIsAbsolute(Result) then
    begin
      if SearchLowUpCase(Result) then exit;
    end else
    begin
      Result:=ResolveDots(BaseDirectory+Result);
      if SearchLowUpCase(Result) then exit;
    end;
    exit('');
  end;

  // first search in foreign unit paths
  IsForeign:=true;
  for i:=0 to Cache.ForeignUnitPaths.Count-1 do
    if SearchInDir(Cache.ForeignUnitPaths[i],Result) then
    begin
      IsForeign:=true;
      exit;
    end;

  // then in BaseDirectory
  IsForeign:=false;
  if SearchInDir(BaseDirectory,Result) then exit;

  // finally search in unit paths
  for i:=0 to Cache.UnitPaths.Count-1 do
    if SearchInDir(Cache.UnitPaths[i],Result) then exit;

  Result:='';
end;

function TPas2jsFileResolver.FindUnitJSFileName(const aUnitFilename: string
  ): String;
begin
  Result:='';
  if aUnitFilename='' then exit;
  if Cache.AllJSIntoMainJS then
  begin
    Result:=Cache.GetResolvedMainJSFile;
  end else begin
    if Cache.UnitOutputPath<>'' then
      Result:=Cache.UnitOutputPath+ChangeFileExt(ExtractFileName(aUnitFilename),'.js')
    else if Cache.MainOutputPath<>'' then
      Result:=Cache.MainOutputPath+ChangeFileExt(ExtractFileName(aUnitFilename),'.js')
    else
      Result:=ChangeFileExt(aUnitFilename,'.js');
  end;
end;

function TPas2jsFileResolver.FindCustomJSFileName(const aFilename: string
  ): String;

  function SearchInDir(const Dir: string): boolean;
  var
    CurFilename: String;
  begin
    CurFilename:=Dir+aFilename;
    Result:=FileExistsLogged(CurFilename);
    if Result then
      FindCustomJSFileName:=CurFilename;
  end;

var
  i: Integer;
begin
  Result:='';

  if FilenameIsAbsolute(aFilename) then
    begin
    Result:=aFilename;
    if not FileExistsLogged(Result) then
      Result:='';
    exit;
    end;

  if ExtractFilePath(aFilename)<>'' then
    begin
    Result:=ExpandFileNameUTF8(aFilename,BaseDirectory);
    if not FileExistsLogged(Result) then
      Result:='';
    exit;
    end;

  // first search in foreign unit paths
  for i:=0 to Cache.ForeignUnitPaths.Count-1 do
    if SearchInDir(Cache.ForeignUnitPaths[i]) then
      exit;

  // then in BaseDirectory
  if SearchInDir(BaseDirectory) then exit;

  // finally search in unit paths
  for i:=0 to Cache.UnitPaths.Count-1 do
    if SearchInDir(Cache.UnitPaths[i]) then exit;

  Result:='';
end;

function TPas2jsFileResolver.FileExistsLogged(const Filename: string): boolean;
begin
  Result:=Cache.DirectoryCache.FileExists(Filename);
  if Cache.ShowTriedUsedFiles then
    if Result then
      Cache.Log.LogMsgIgnoreFilter(nSearchingFileFound,[Cache.FormatPath(Filename)])
    else
      Cache.Log.LogMsgIgnoreFilter(nSearchingFileNotFound,[Cache.FormatPath(Filename)]);
end;

function TPas2jsFileResolver.FileExistsILogged(var Filename: string): integer;
begin
  Result:=Cache.DirectoryCache.FileExistsI(Filename);
  if Cache.ShowTriedUsedFiles then
    if Result>0 then
      Cache.Log.LogMsgIgnoreFilter(nSearchingFileFound,[Cache.FormatPath(Filename)])
    else
      Cache.Log.LogMsgIgnoreFilter(nSearchingFileNotFound,[Cache.FormatPath(Filename)]);
end;

function TPas2jsFileResolver.SearchLowUpCase(var Filename: string): boolean;
var
  i: Integer;
{$IFNDEF CaseInsensitiveFilenames}
  CasedFilename: String;
{$ENDIF}
begin
  if StrictFileCase or Cache.SearchLikeFPC then
  begin
    if FileExistsLogged(Filename) then
      exit(true);
    if StrictFileCase then
      exit(false);
    {$IFNDEF CaseInsensitiveFilenames}
    // FPC like search:
    // first as written, then lowercase, then uppercase
    CasedFilename:=ExtractFilePath(Filename)+LowerCase(ExtractFileName(Filename));
    if (Filename<>CasedFilename) and FileExistsLogged(CasedFilename) then
    begin
      Filename:=CasedFilename;
      exit(true);
    end;
    CasedFilename:=ExtractFilePath(Filename)+UpperCase(ExtractFileName(Filename));
    if (Filename<>CasedFilename) and FileExistsLogged(CasedFilename) then
    begin
      Filename:=CasedFilename;
      exit(true);
    end;
    {$ENDIF}
  end else
  begin
    // search case insensitive
    i:=FileExistsILogged(Filename);
    if i=1 then exit(true);
    if i>1 then
      Cache.RaiseDuplicateFile(Filename);
  end;
  Result:=false;
end;

{ TPas2jsFilesCache }

procedure TPas2jsFilesCache.RegisterMessages;
begin
  Log.RegisterMsg(mtInfo,nIncludeSearch,sIncludeSearch);
  Log.RegisterMsg(mtInfo,nUnitSearch,sUnitSearch);
  Log.RegisterMsg(mtInfo,nSearchingFileFound,sSearchingFileFound);
  Log.RegisterMsg(mtInfo,nSearchingFileNotFound,sSearchingFileNotFound);
  Log.RegisterMsg(mtFatal,nDuplicateFileFound,sDuplicateFileFound);
  Log.RegisterMsg(mtFatal,nCustomJSFileNotFound,sCustomJSFileNotFound);
end;

function TPas2jsFilesCache.GetAllJSIntoMainJS: Boolean;
begin
  Result:=caoAllJSIntoMainJS in FOptions;
end;

function TPas2jsFilesCache.GetSearchLikeFPC: boolean;
begin
  Result:=caoSearchLikeFPC in FOptions;
end;

function TPas2jsFilesCache.GetShowFullFilenames: boolean;
begin
  Result:=caoShowFullFilenames in FOptions;
end;

function TPas2jsFilesCache.GetShowTriedUsedFiles: boolean;
begin
  Result:=caoShowTriedUsedFiles in FOptions;
end;

procedure TPas2jsFilesCache.SetAllJSIntoMainJS(AValue: Boolean);
begin
  SetOption(caoAllJSIntoMainJS,AValue);
end;

procedure TPas2jsFilesCache.SetBaseDirectory(AValue: string);
begin
  AValue:=Pas2jsFileUtils.ExpandDirectory(AValue);
  if FBaseDirectory=AValue then Exit;
  FBaseDirectory:=AValue;
  DirectoryCache.WorkingDirectory:=BaseDirectory;
end;

function TPas2jsFilesCache.AddSearchPaths(const Paths: string;
  Kind: TPas2jsSearchPathKind; FromCmdLine: boolean; var List: TStringList;
  var CmdLineCount: integer): string;
// cmd line paths are added in front of the cfg paths
// cmd line paths are added in order, cfg paths are added in reverse order
// multi paths separated by semicolon are added in order
// duplicates are removed
var
  Added: Integer;

  function Add(aPath: string): boolean;
  var
    Remove: Boolean;
    i: Integer;
  begin
    Remove:=false;
    // search duplicate
    case Kind of
    spkPath:
      begin
        i:=List.Count-1;
        while (i>=0) and (CompareFilenames(aPath,List[i])<>0) do dec(i);
      end;
    spkIdentifier:
      begin
        if aPath[length(aPath)]='-' then
        begin
          Delete(aPath,length(aPath),1);
          Remove:=true;
        end;
        if not IsValidIdent(aPath,true,true) then
        begin
          AddSearchPaths:=aPath;
          exit(false);
        end;
        i:=List.Count-1;
        while (i>=0) and (CompareText(aPath,List[i])<>0) do dec(i);
      end;
    end;

    if Remove then
    begin
      // remove
      if i>=0 then
      begin
        List.Delete(i);
        if CmdLineCount>i then dec(CmdLineCount);
      end;
      exit(true);
    end;

    if FromCmdLine then
    begin
      // from cmdline: append in order to the cmdline params, in front of cfg params
      if i>=0 then
      begin
        if i<=CmdLineCount then exit(true);
        List.Delete(i);
      end;
      List.Insert(CmdLineCount,aPath);
      inc(CmdLineCount);
    end else begin
      // from cfg: append in reverse order to the cfg params, behind cmdline params
      if i>=0 then
      begin
        if i<=CmdLineCount+Added then exit(true);
        List.Delete(i);
      end;
      List.Insert(CmdLineCount+Added,aPath);
      inc(Added);
    end;
    Result:=true;
  end;

var
  aPath: String;
  p, i: integer;
  aPaths: TStringList;
begin
  Result:='';
  p:=1;
  Added:=0;
  aPaths:=TStringList.Create;
  try
    while p<=length(Paths) do begin
      aPath:=GetNextDelimitedItem(Paths,';',p);
      if aPath='' then continue;
      if Kind=spkPath then
      begin
        aPath:=ExpandDirectory(aPath,BaseDirectory);
        if aPath='' then continue;
      end;
      aPaths.Clear;
      FindMatchingFiles(aPath,1000,aPaths);
      if aPaths.Count=0 then
      begin
        if not Add(aPath) then exit;
      end else begin
        for i:=0 to aPaths.Count-1 do
          if not Add(aPaths[i]) then exit;
      end;
    end;
  finally
    aPaths.Free;
  end;
end;

procedure TPas2jsFilesCache.SetDefaultOutputPath(AValue: string);
begin
  AValue:=ExpandDirectory(AValue,BaseDirectory);
  if FDefaultOutputPath=AValue then Exit;
  FDefaultOutputPath:=AValue;
end;

procedure TPas2jsFilesCache.SetMainJSFile(AValue: string);
begin
  if FMainJSFile=AValue then Exit;
  FMainJSFile:=AValue;
end;

procedure TPas2jsFilesCache.SetOptions(AValue: TP2jsFileCacheOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
end;

procedure TPas2jsFilesCache.SetSearchLikeFPC(const AValue: boolean);
begin
  SetOption(caoSearchLikeFPC,AValue);
end;

procedure TPas2jsFilesCache.SetShowFullFilenames(const AValue: boolean);
begin
  SetOption(caoShowFullFilenames,AValue);
end;

procedure TPas2jsFilesCache.SetShowTriedUsedFiles(const AValue: boolean);
begin
  SetOption(caoShowTriedUsedFiles,AValue);
end;

procedure TPas2jsFilesCache.SetSrcMapBaseDir(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=ExpandDirectory(AValue,BaseDirectory);
  if FSrcMapBaseDir=NewValue then Exit;
  FSrcMapBaseDir:=NewValue;
end;

procedure TPas2jsFilesCache.SetUnitOutputPath(AValue: string);
begin
  AValue:=ExpandDirectory(AValue,BaseDirectory);
  if FUnitOutputPath=AValue then Exit;
  FUnitOutputPath:=AValue;
end;

procedure TPas2jsFilesCache.SetOption(Flag: TP2jsFileCacheOption; Enable: boolean
  );
begin
  if Enable then
    Include(FOptions,Flag)
  else
    Exclude(FOptions,Flag);
  if Flag in [caoAllJSIntoMainJS] then
    Exclude(FStates,cfsMainJSFileResolved);
end;

function TPas2jsFilesCache.ReadFile(Filename: string; var Source: string
  ): boolean;
var
  ms: TMemoryStream;
begin
  Result:=false;
  try
    if Assigned(OnReadFile) then
      Result:=OnReadFile(Filename,Source);
    if Result then
      Exit;
    ms:=TMemoryStream.Create;
    try
      ms.LoadFromFile(Filename);
      SetLength(Source,ms.Size);
      ms.Position:=0;
      if Source<>'' then
        ms.Read(Source[1],length(Source));
      Result:=true;
    finally
      ms.Free;
    end;
  except
    on E: Exception do begin
      EPas2jsFileCache.Create('Error reading file "'+Filename+'": '+E.Message);
    end;
  end;
end;

procedure TPas2jsFilesCache.FindMatchingFiles(Mask: string; MaxCount: integer;
  Files: TStrings);

  procedure TooMany(id: int64);
  begin
    raise EListError.Create('found too many files "'+Mask+'". Max='+IntToStr(MaxCount)+' ['+IntToStr(id)+']');
  end;

  procedure Find(aMask: string; p: integer);
  var
    Dir: TPas2jsCachedDirectory;
    StartP, i: Integer;
    CurMask, Filename: String;
    Entry: TPas2jsCachedDirectoryEntry;
  begin
    while p<=length(aMask) do begin
      if aMask[p] in ['*','?'] then
      begin
        while (p>1) and not (aMask[p-1] in AllowDirectorySeparators) do dec(p);
        Dir:=DirectoryCache.GetDirectory(LeftStr(aMask,p-1),true,false);
        StartP:=p;
        while (p<=length(aMask)) and not (aMask[p] in AllowDirectorySeparators) do
          inc(p);
        CurMask:=copy(aMask,StartP,p-StartP);
        for i:=0 to Dir.Count-1 do begin
          Entry:=Dir.Entries[i];
          if (Entry.Name='') or (Entry.Name='.') or (Entry.Name='..') then continue;
          if not MatchGlobbing(CurMask,Entry.Name) then continue;
          Filename:=Dir.Path+Entry.Name;
          if p>length(aMask) then
          begin
            // e.g. /path/unit*.pas
            if Files.Count>=MaxCount then
              TooMany(20180126091916);
            Files.Add(Filename);
          end else begin
            // e.g. /path/sub*path/...
            Find(Filename+copy(aMask,p,length(aMask)),length(Filename)+1);
          end;
        end;
        exit;
      end;
      inc(p);
    end;
    // mask has no placeholder -> search directly
    if DirectoryCache.FileExists(aMask) then
    begin
      if Files.Count>=MaxCount then
        TooMany(20180126091913);
      Files.Add(aMask);
    end;
  end;

begin
  Mask:=ResolveDots(Mask);
  Find(Mask,1);
end;

constructor TPas2jsFilesCache.Create(aLog: TPas2jsLogger);
begin
  inherited Create;
  FResetStamp:=InvalidChangeStamp;
  FLog:=aLog;
  FOptions:=DefaultPas2jsFileCacheOptions;
  FIncludePaths:=TStringList.Create;
  FInsertFilenames:=TStringList.Create;
  FForeignUnitPaths:=TStringList.Create;
  FNamespaces:=TStringList.Create;
  FUnitPaths:=TStringList.Create;
  FFiles:=TAVLTree.Create(@CompareCachedFiles);
  FDirectoryCache:=TPas2jsCachedDirectories.Create;
  RegisterMessages;
end;

destructor TPas2jsFilesCache.Destroy;
begin
  FLog:=nil;
  FFiles.FreeAndClear;
  FreeAndNil(FDirectoryCache);
  FreeAndNil(FFiles);
  FreeAndNil(FInsertFilenames);
  FreeAndNil(FIncludePaths);
  FreeAndNil(FForeignUnitPaths);
  FreeAndNil(FNamespaces);
  FreeAndNil(FUnitPaths);
  inherited Destroy;
end;

procedure TPas2jsFilesCache.Reset;
begin
  IncreaseChangeStamp(FResetStamp);
  FDirectoryCache.Invalidate;
  // FFiles: TAVLTree; keep data, files are checked against LoadedFileAge
  FOptions:=DefaultPas2jsFileCacheOptions;
  FMainJSFile:='';
  FMainJSFileResolved:='';
  FMainSrcFile:='';
  FBaseDirectory:='';
  FSrcMapBaseDir:='';
  FUnitOutputPath:='';
  FReadLineCounter:=0;
  FForeignUnitPaths.Clear;
  FForeignUnitPathsFromCmdLine:=0;
  FUnitPaths.Clear;
  FUnitPathsFromCmdLine:=0;
  FIncludePaths.Clear;
  FIncludePathsFromCmdLine:=0;
  FInsertFilenames.Clear;
  FStates:=FStates-[cfsMainJSFileResolved];
  FNamespaces.Clear;
  FNamespacesFromCmdLine:=0;
  FPrecompileFormat:=nil;
  FSrcMapBaseDir:='';
  // FOnReadFile: TPas2jsReadFileEvent; keep
  // FOnWriteFile: TPas2jsWriteFileEvent; keep
end;

function TPas2jsFilesCache.AddIncludePaths(const Paths: string;
  FromCmdLine: boolean; out ErrorMsg: string): boolean;
begin
  ErrorMsg:=AddSearchPaths(Paths,spkPath,FromCmdLine,FIncludePaths,FIncludePathsFromCmdLine);
  Result:=ErrorMsg='';
end;

function TPas2jsFilesCache.AddNamespaces(const Paths: string;
  FromCmdLine: boolean; out ErrorMsg: string): boolean;
begin
  ErrorMsg:=AddSearchPaths(Paths,spkIdentifier,FromCmdLine,FNamespaces,FNamespacesFromCmdLine);
  Result:=ErrorMsg='';
end;

function TPas2jsFilesCache.AddUnitPaths(const Paths: string;
  FromCmdLine: boolean; out ErrorMsg: string): boolean;
begin
  ErrorMsg:=AddSearchPaths(Paths,spkPath,FromCmdLine,FUnitPaths,FUnitPathsFromCmdLine);
  Result:=ErrorMsg='';
end;

function TPas2jsFilesCache.AddSrcUnitPaths(const Paths: string;
  FromCmdLine: boolean; out ErrorMsg: string): boolean;
begin
  ErrorMsg:=AddSearchPaths(Paths,spkPath,FromCmdLine,FForeignUnitPaths,FForeignUnitPathsFromCmdLine);
  Result:=ErrorMsg='';
end;

function TPas2jsFilesCache.CreateResolver: TPas2jsFileResolver;
begin
  Result := TPas2jsFileResolver.Create(Self);
  Result.UseStreams:=false;
  Result.BaseDirectory:=BaseDirectory; // beware: will be changed by Scanner.OpenFile
end;

function TPas2jsFilesCache.FormatPath(const aPath: string): string;
begin
  Result:=aPath;
  if (Result='') or (BaseDirectory='') then exit;
  if FilenameIsAbsolute(aPath) then
  begin
    if not ShowFullPaths then
    begin
      if BaseDirectory=LeftStr(Result,length(BaseDirectory)) then
        Delete(Result,1,length(BaseDirectory));
    end;
  end else begin
    if ShowFullPaths then
      Result:=BaseDirectory+Result;
  end;
end;

function TPas2jsFilesCache.GetResolvedMainJSFile: string;
begin
  if not (cfsMainJSFileResolved in FStates) then
  begin
    if MainJSFile='.' then
      FMainJSFileResolved:=''
    else begin
      FMainJSFileResolved:=MainJSFile;
      if FMainJSFileResolved<>'' then
      begin
        // has option -o
        if ExtractFilePath(FMainJSFileResolved)='' then
        begin
          // -o<FileWithoutPath>
          if MainOutputPath<>'' then
            FMainJSFileResolved:=MainOutputPath+FMainJSFileResolved
          else if UnitOutputPath<>'' then
            FMainJSFileResolved:=UnitOutputPath+FMainJSFileResolved;
        end;
      end else begin
        // no option -o
        FMainJSFileResolved:=ChangeFileExt(MainSrcFile,'.js');
        if MainOutputPath<>'' then
        begin
          // option -FE and no -o => put into MainOutputPath
          FMainJSFileResolved:=MainOutputPath+ExtractFilename(FMainJSFileResolved)
        end else if UnitOutputPath<>'' then
        begin
          // option -FU and no -o => put into UnitOutputPath
          FMainJSFileResolved:=UnitOutputPath+ExtractFilename(FMainJSFileResolved)
        end else begin
          // no -FU and no -o => put into source directory
        end;
      end;
    end;
    Include(FStates,cfsMainJSFileResolved);
  end;
  Result:=FMainJSFileResolved;
end;

function TPas2jsFilesCache.FindFile(Filename: string): TPas2jsCachedFile;
var
  Node: TAVLTreeNode;
begin
  Filename:=NormalizeFilename(Filename,true);
  Node:=FFiles.FindKey(Pointer(Filename),@CompareFilenameWithCachedFile);
  if Node=nil then
    exit(nil);
  Result:=TPas2jsCachedFile(Node.Data);
end;

function TPas2jsFilesCache.LoadFile(Filename: string; Binary: boolean
  ): TPas2jsCachedFile;
var
  Node: TAVLTreeNode;
begin
  Filename:=NormalizeFilename(Filename,true);
  Node:=FFiles.FindKey(Pointer(Filename),@CompareFilenameWithCachedFile);
  if Node=nil then
  begin
    // new file
    Result:=TPas2jsCachedFile.Create(Self,Filename);
    FFiles.Add(Result);
  end else begin
    Result:=TPas2jsCachedFile(Node.Data);
  end;
  Result.Load(true,Binary);
end;

function TPas2jsFilesCache.NormalizeFilename(const Filename: string;
  RaiseOnError: boolean): string;
begin
  Result:=Filename;
  if ExtractFilename(Result)='' then
    if RaiseOnError then
      raise EFileNotFoundError.Create('invalid file name "'+Filename+'"');
  Result:=ExpandFileNameUTF8(Result,BaseDirectory);
  if (ExtractFilename(Result)='') or not FilenameIsAbsolute(Result) then
    if RaiseOnError then
      raise EFileNotFoundError.Create('invalid file name "'+Filename+'"');
end;

procedure TPas2jsFilesCache.InsertCustomJSFiles(aWriter: TPas2JSMapper);
var
  i: Integer;
  Filename: String;
  FileResolver: TPas2jsFileResolver;
  aFile: TPas2jsCachedFile;
begin
  if InsertFilenames.Count=0 then exit;
  FileResolver:=CreateResolver;
  try
    for i:=0 to InsertFilenames.Count-1 do begin
      Filename:=FileResolver.FindCustomJSFileName(ResolveDots(InsertFilenames[i]));
      if Filename='' then
      begin
        Log.LogMsg(nCustomJSFileNotFound,[InsertFilenames[i]]);
        raise EFileNotFoundError.Create('');
      end;
      aFile:=LoadFile(Filename);
      if aFile.Source='' then continue;
      aWriter.WriteFile(aFile.Source,Filename);
    end
  finally
    FileResolver.Free;
  end;
end;

function TPas2jsFilesCache.IndexOfInsertJSFilename(const aFilename: string
  ): integer;
var
  i: Integer;
begin
  for i:=0 to FInsertFilenames.Count-1 do
    if CompareFilenames(aFilename,InsertFilenames[i])=0 then
      exit(i);
  Result:=-1;
end;

procedure TPas2jsFilesCache.AddInsertJSFilename(const aFilename: string);
begin
  if IndexOfInsertJSFilename(aFilename)<0 then
    InsertFilenames.Add(aFilename);
end;

procedure TPas2jsFilesCache.RemoveInsertJSFilename(const aFilename: string);
var
  i: Integer;
begin
  i:=IndexOfInsertJSFilename(aFilename);
  if i>=0 then
    InsertFilenames.Delete(i);
end;

procedure TPas2jsFilesCache.GetListing(const aDirectory: string;
  var Files: TStrings; FullPaths: boolean);
begin
  DirectoryCache.GetDirectory(aDirectory,true,false).GetFiles(Files,FullPaths);
end;

procedure TPas2jsFilesCache.RaiseDuplicateFile(aFilename: string);

  procedure E(const File1, File2: string);
  begin
    raise EPas2jsFileCache.Create(SafeFormat(sDuplicateFileFound,[File1,File2]));
  end;

var
  Dir: TPas2jsCachedDirectory;
  i: Integer;
  List: TStringList;
  ShortFilename: String;
begin
  Dir:=DirectoryCache.GetDirectory(ExtractFilePath(aFilename),true,false);
  ShortFilename:=ExtractFilename(aFilename);
  i:=Dir.IndexOfFileCaseSensitive(ShortFilename);
  if i<0 then
    E(aFilename,'?');
  List:=TStringList.Create;
  try
    Dir.GetSameNamesCaseInsensitive(i,List);
    if List.Count<2 then
      E(aFilename,'?');
    E(Dir.Path+List[0],List[1]);
  finally
    List.Free;
  end;
end;

procedure TPas2jsFilesCache.SaveToFile(ms: TMemoryStream; Filename: string);
var
  s: string;
  l: Int64;
  i: Integer;
begin
  if Assigned(OnWriteFile) then
  begin
    l:=ms.Size-ms.Position;
    if l>0 then
    begin
      s:='';
      SetLength(s,l);
      ms.Read(s[1],l);
    end
    else
      s:='';
    OnWriteFile(Filename,s);
  end else
  begin
    try
      ms.SaveToFile(Filename);
    except
      on E: Exception do begin
        i:=GetLastOSError;
        if i<>0 then
          Log.LogPlain('Note: '+SysErrorMessage(i));
        if not SysUtils.DirectoryExists(ChompPathDelim(ExtractFilePath(Filename))) then
          Log.LogPlain('Note: file cache inconsistency: folder does not exist "'+ChompPathDelim(ExtractFilePath(Filename))+'"');
        if SysUtils.FileExists(Filename) and not FileIsWritable(Filename) then
          Log.LogPlain('Note: file is not writable "'+Filename+'"');
        raise;
      end;
    end;
  end;
end;

function TPas2jsFilesCache.ExpandDirectory(const Filename, BaseDir: string
  ): string;
begin
  if Filename='' then exit('');
  if BaseDir<>'' then
    Result:=ExpandFileNameUTF8(Filename,BaseDir)
  else
    Result:=ExpandFileNameUTF8(Filename,BaseDirectory);
  if Result='' then exit;
  Result:=IncludeTrailingPathDelimiter(Result);
end;

end.

