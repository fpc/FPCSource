{ Author: Mattias Gaertner  2017  mattias@freepascal.org

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
  Pas2jsLogger, Pas2jsFileUtils;

const // Messages
  nIncludeSearch = 201; sIncludeSearch = 'Include file search: %s';
  nUnitSearch = 202; sUnitSearch = 'Unitsearch: %s';
  nSearchingFileFound = 203; sSearchingFileFound = 'Searching file: %s... found';
  nSearchingFileNotFound = 204; sSearchingFileNotFound = 'Searching file: %s... not found';

type
  EPas2jsFileCache = class(Exception);

type
  TP2jsFileCacheOption = (
    caoShowFullFilenames,
    caoShowTriedUsedFiles,
    caoAllJSIntoMainJS
    );
  TP2jsFileCacheOptions = set of TP2jsFileCacheOption;
const
  DefaultPas2jsFileCacheOptions = [];
  p2jsfcoCaption: array[TP2jsFileCacheOption] of string = (
    // only used by experts, no need for resourcestrings
    'Show full filenames',
    'Show tried/used files',
    'Combine all JavaScript into main file'
    );

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
  public
    constructor Create(aCache: TPas2jsFilesCache; const aFilename: string); reintroduce;
    function Load(RaiseOnError: boolean): boolean;
    function CreateLineReader(RaiseOnError: boolean): TPas2jsFileLineReader;
    property FileEncoding: string read FFileEncoding;
    property Filename: string read FFilename;
    property Source: string read FSource; // UTF-8 without BOM
    property Cache: TPas2jsFilesCache read FCache;
    property ChangeStamp: TChangeStamp read FChangeStamp;// changed when Source changed
    property Loaded: boolean read FLoaded; // Source valid, but may contain an old version
    property LastErrorMsg: string read FLastErrorMsg;
    property LoadedFileAge: longint read FLoadedFileAge;// only valid if Loaded=true
  end;

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
    FNamespaces: TStringList;
    FNamespacesFromCmdLine: integer;
    FOptions: TP2jsFileCacheOptions;
    FReadLineCounter: SizeInt;
    FResetStamp: TChangeStamp;
    FSrcMapBaseDir: string;
    FStates: TPas2jsFileCacheStates;
    FUnitOutputPath: string;
    FUnitPaths: TStringList;
    FUnitPathsFromCmdLine: integer;
    function GetAllJSIntoMainJS: Boolean;
    function GetShowFullFilenames: boolean;
    function GetShowTriedUsedFiles: boolean;
    procedure RegisterMessages;
    procedure SetAllJSIntoMainJS(AValue: Boolean);
    procedure SetBaseDirectory(AValue: string);
    function AddSearchPaths(const Paths: string; Kind: TPas2jsSearchPathKind;
      FromCmdLine: boolean; var List: TStringList; var CmdLineCount: integer): string;
    procedure SetMainJSFile(AValue: string);
    procedure SetOptions(AValue: TP2jsFileCacheOptions);
    procedure SetShowFullFilenames(AValue: boolean);
    procedure SetShowTriedUsedFiles(AValue: boolean);
    procedure SetSrcMapBaseDir(const AValue: string);
    procedure SetUnitOutputPath(AValue: string);
    procedure SetOption(Flag: TP2jsFileCacheOption; Enable: boolean);
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
    function LoadTextFile(Filename: string): TPas2jsCachedFile;
    function NormalizeFilename(const Filename: string; RaiseOnError: boolean): string;
    procedure InsertCustomJSFiles(aWriter: TPas2JSMapper);
    function IndexOfInsertFilename(const aFilename: string): integer;
    procedure AddInsertFilename(const aFilename: string);
    procedure RemoveInsertFilename(const aFilename: string);
  public
    property AllJSIntoMainJS: Boolean read GetAllJSIntoMainJS write SetAllJSIntoMainJS;
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory; // includes trailing pathdelim
    property ForeignUnitPaths: TStringList read FForeignUnitPaths;
    property ForeignUnitPathsFromCmdLine: integer read FForeignUnitPathsFromCmdLine;
    property IncludePaths: TStringList read FIncludePaths;
    property IncludePathsFromCmdLine: integer read FIncludePathsFromCmdLine;
    property InsertFilenames: TStringList read FInsertFilenames;
    property Log: TPas2jsLogger read FLog;
    property MainJSFile: string read FMainJSFile write SetMainJSFile;
    property MainSrcFile: string read FMainSrcFile write FMainSrcFile;
    property Namespaces: TStringList read FNamespaces;
    property NamespacesFromCmdLine: integer read FNamespacesFromCmdLine;
    property Options: TP2jsFileCacheOptions read FOptions write SetOptions default DefaultPas2jsFileCacheOptions;
    property ReadLineCounter: SizeInt read FReadLineCounter write FReadLineCounter;
    property ResetStamp: TChangeStamp read FResetStamp;
    property SrcMapBaseDir: string read FSrcMapBaseDir write SetSrcMapBaseDir; // includes trailing pathdelim
    property ShowFullPaths: boolean read GetShowFullFilenames write SetShowFullFilenames;
    property ShowTriedUsedFiles: boolean read GetShowTriedUsedFiles write SetShowTriedUsedFiles;
    property UnitOutputPath: string read FUnitOutputPath write SetUnitOutputPath; // includes trailing pathdelim
    property UnitPaths: TStringList read FUnitPaths;
    property UnitPathsFromCmdLine: integer read FUnitPathsFromCmdLine;
  end;

function CompareFilenameWithCachedFile(Filename, CachedFile: Pointer): integer;
function CompareCachedFiles(File1, File2: Pointer): integer;
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
  if NormSrcEncoding=NormalizeEncoding(EncodingUTF8) then begin
    p:=PChar(Result);
    if (p^=#$EF) and (p[1]=#$BB) and (p[2]=#$BF) then begin
      // cut out UTF-8 BOM
      Delete(Result,1,3);
    end;
  end else if (NormSrcEncoding=EncodingSystem)
      or (NormSrcEncoding=GetDefaultTextEncoding) then begin
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
    if ord(p^)<128 then begin
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

{ TPas2jsFileLineReader }

constructor TPas2jsFileLineReader.Create(const AFilename: string);
var
  ms: TMemoryStream;
  NewSource, FileEncoding: string;
begin
  inherited Create(AFilename);
  ms:=TMemoryStream.Create;
  try
    try
      ms.LoadFromFile(Filename);
      SetLength(NewSource,ms.Size);
      ms.Position:=0;
      if NewSource<>'' then
        ms.Read(NewSource[1],length(NewSource));
    except
      on E: Exception do begin
        EPas2jsFileCache.Create('Error reading file "'+Filename+'": '+E.Message);
        exit;
      end;
    end;
  finally
    ms.Free;
  end;
  FileEncoding:='';
  FSource:=ConvertTextToUTF8(NewSource,FileEncoding);
  FSrcPos:=PChar(FSource);
  FIsEOF:=FSource='';
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
      if p-PChar(FSource)=length(FSource) then begin
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

constructor TPas2jsCachedFile.Create(aCache: TPas2jsFilesCache;
  const aFilename: string);
begin
  FChangeStamp:=InvalidChangeStamp;
  FCache:=aCache;
  FCacheStamp:=Cache.ResetStamp;
  FFilename:=aFilename;
end;

function TPas2jsCachedFile.Load(RaiseOnError: boolean): boolean;

  procedure Err(const ErrorMsg: string);
  begin
    FLastErrorMsg:=ErrorMsg;
    if RaiseOnError then
      raise EPas2jsFileCache.Create(FLastErrorMsg);
  end;

var
  ms: TMemoryStream;
  NewSource: string;
begin
  {$IFDEF VerboseFileCache}
  writeln('TPas2jsCachedFile.Load START "',Filename,'" Loaded=',Loaded);
  {$ENDIF}
  if Loaded then begin
    // already loaded, check if it still valid
    if (Cache.ResetStamp=FCacheStamp) then begin
      // nothing changed
      Result:=FLastErrorMsg='';
      if (not Result) and RaiseOnError then
        raise EPas2jsFileCache.Create(FLastErrorMsg);
      exit;
    end;
    {$IFDEF VerboseFileCache}
    writeln('TPas2jsCachedFile.Load CHECK FILEAGE "',Filename,'"');
    {$ENDIF}
    if LoadedFileAge=FileAge(Filename) then
      exit(true);
  end;
  {$IFDEF VerboseFileCache}
  writeln('TPas2jsCachedFile.Load RELOAD ',Filename,' Loaded=',Loaded);
  {$ENDIF}
  // needs (re)load
  Result:=false;
  if not FileExists(Filename) then begin
    Err('File not found "'+Filename+'"');
    exit;
  end;
  if DirectoryExists(Filename) then begin
    Err('File is a directory "'+Filename+'"');
    exit;
  end;
  ms:=TMemoryStream.Create;
  try
    try
      ms.LoadFromFile(Filename);
      SetLength(NewSource,ms.Size);
      ms.Position:=0;
      if NewSource<>'' then
        ms.Read(NewSource[1],length(NewSource));
    except
      on E: Exception do begin
        Err('Error reading file "'+Filename+'": '+E.Message);
        exit;
      end;
    end;
  finally
    ms.Free;
  end;
  {$IFDEF VerboseFileCache}
  writeln('TPas2jsCachedFile.Load ENCODE ',Filename,' FFileEncoding=',FFileEncoding);
  {$ENDIF}
  FSource:=ConvertTextToUTF8(NewSource,FFileEncoding);
  FLoaded:=true;
  FCacheStamp:=Cache.ResetStamp;
  FLoadedFileAge:=FileAge(Filename);
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
    Result := TFileLineReader.Create(Filename); // ToDo: 1. convert encoding to UTF-8, 2. use cache
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
    for i:=0 to IncludePaths.Count-1 do begin
      Result:=IncludePaths[i]+Filename;
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

  if FilenameIsAbsolute(Filename) then begin
    Result:=Filename;
    if not SearchLowUpCase(Result) then
      Result:='';
    exit;
  end;

  // search with the given file extension (even if no ext)
  Result:=SearchCasedInIncPath(Filename);
  if Result<>'' then exit;

  if ExtractFileExt(Filename)='' then begin
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
begin
  Result:=nil;
  if not FileExists(aFilename) then
    raise EFileNotFoundError.Create(aFilename)
  else
    Result:=Cache.LoadTextFile(aFilename).CreateLineReader(false);
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
begin
  Result:='';

  if InFilename<>'' then begin
    Cache.Log.LogMsgIgnoreFilter(nSearchingFileNotFound,['not yet implemented "in" '+Cache.FormatPath(InFilename)])
    // ToDo
  end;

  // first search in foreign unit paths
  IsForeign:=true;
  for i:=0 to Cache.ForeignUnitPaths.Count-1 do
    if SearchInDir(Cache.ForeignUnitPaths[i],Result) then begin
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
  if Cache.AllJSIntoMainJS then begin
    Result:=Cache.GetResolvedMainJSFile;
  end else begin
    if Cache.UnitOutputPath<>'' then
      Result:=Cache.UnitOutputPath+ChangeFileExt(ExtractFileName(aUnitFilename),'.js')
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
  Result:=FileExists(Filename);
  if Cache.ShowTriedUsedFiles then
    if Result then
      Cache.Log.LogMsgIgnoreFilter(nSearchingFileFound,[Cache.FormatPath(Filename)])
    else
      Cache.Log.LogMsgIgnoreFilter(nSearchingFileNotFound,[Cache.FormatPath(Filename)]);
end;

function TPas2jsFileResolver.SearchLowUpCase(var Filename: string): boolean;
{$IFNDEF CaseInsensitiveFilenames}
var
  CasedFilename: String;
{$ENDIF}
begin
  if FileExistsLogged(Filename) then
    exit(true);
  if StrictFileCase then
    exit(false);
  {$IFNDEF CaseInsensitiveFilenames}
  CasedFilename:=ExtractFilePath(Filename)+LowerCase(ExtractFileName(Filename));
  if (Filename<>CasedFilename) and FileExistsLogged(CasedFilename) then begin
    Filename:=CasedFilename;
    exit(true);
  end;
  CasedFilename:=ExtractFilePath(Filename)+UpperCase(ExtractFileName(Filename));
  if (Filename<>CasedFilename) and FileExistsLogged(CasedFilename) then begin
    Filename:=CasedFilename;
    exit(true);
  end;
  {$ENDIF}
  Result:=false;
end;

{ TPas2jsFilesCache }

procedure TPas2jsFilesCache.RegisterMessages;
begin
  Log.RegisterMsg(mtInfo,nIncludeSearch,sIncludeSearch);
  Log.RegisterMsg(mtInfo,nUnitSearch,sUnitSearch);
  Log.RegisterMsg(mtInfo,nSearchingFileFound,sSearchingFileFound);
  Log.RegisterMsg(mtInfo,nSearchingFileNotFound,sSearchingFileNotFound);
end;

function TPas2jsFilesCache.GetAllJSIntoMainJS: Boolean;
begin
  Result:=caoAllJSIntoMainJS in FOptions;
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
  AValue:=ExpandDirectory(AValue);
  if FBaseDirectory=AValue then Exit;
  FBaseDirectory:=AValue;
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
        if aPath[length(aPath)]='-' then begin
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

    if Remove then begin
      // remove
      if i>=0 then begin
        List.Delete(i);
        if CmdLineCount>i then dec(CmdLineCount);
      end;
      exit(true);
    end;

    if FromCmdLine then begin
      // from cmdline: append in order to the cmdline params, in front of cfg params
      if i>=0 then begin
        if i<=CmdLineCount then exit(true);
        List.Delete(i);
      end;
      List.Insert(CmdLineCount,aPath);
      inc(CmdLineCount);
    end else begin
      // from cfg: append in reverse order to the cfg params, behind cmdline params
      if i>=0 then begin
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
        aPath:=ExpandDirectory(aPath);
      if (aPath='') then continue;
      aPaths.Clear;
      FindMatchingFiles(aPath,1000,aPaths);
      if aPaths.Count=0 then begin
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

procedure TPas2jsFilesCache.SetShowFullFilenames(AValue: boolean);
begin
  SetOption(caoShowFullFilenames,AValue);
end;

procedure TPas2jsFilesCache.SetShowTriedUsedFiles(AValue: boolean);
begin
  SetOption(caoShowTriedUsedFiles,AValue);
end;

procedure TPas2jsFilesCache.SetSrcMapBaseDir(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=ExpandDirectory(AValue);
  if FSrcMapBaseDir=NewValue then Exit;
  FSrcMapBaseDir:=NewValue;
end;

procedure TPas2jsFilesCache.SetUnitOutputPath(AValue: string);
begin
  AValue:=ExpandDirectory(AValue);
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
  RegisterMessages;
end;

destructor TPas2jsFilesCache.Destroy;
begin
  FLog:=nil;
  FFiles.FreeAndClear;
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
  FOptions:=DefaultPas2jsFileCacheOptions;
  FMainJSFile:='';
  FMainSrcFile:='';
  FBaseDirectory:='';
  FSrcMapBaseDir:='';
  FUnitOutputPath:='';
  FReadLineCounter:=0;
  FForeignUnitPaths.Clear;
  FUnitPaths.Clear;
  FIncludePaths.Clear;
  FStates:=FStates-[cfsMainJSFileResolved];
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
  if FilenameIsAbsolute(aPath) then begin
    if not ShowFullPaths then begin
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
  if not (cfsMainJSFileResolved in FStates) then begin
    if MainJSFile='.' then
      FMainJSFileResolved:=''
    else begin
      FMainJSFileResolved:=MainJSFile;
      if FMainJSFileResolved='' then begin
        // no option -o
        if UnitOutputPath<>'' then begin
          // option -FU and no -o => put into UnitOutputPath
          FMainJSFileResolved:=UnitOutputPath+ChangeFileExt(ExtractFilename(MainSrcFile),'.js')
        end else begin
          // no -FU and no -o => put into source directory
          FMainJSFileResolved:=ChangeFileExt(MainSrcFile,'.js');
        end;
      end else begin
        // has option -o
        if (ExtractFilePath(FMainJSFileResolved)='') and (UnitOutputPath<>'') then
          FMainJSFileResolved:=UnitOutputPath+FMainJSFileResolved;
      end;
    end;
    Include(FStates,cfsMainJSFileResolved);
  end;
  Result:=FMainJSFileResolved;
end;

function TPas2jsFilesCache.LoadTextFile(Filename: string): TPas2jsCachedFile;
var
  Node: TAVLTreeNode;
begin
  Filename:=NormalizeFilename(Filename,true);
  Node:=FFiles.FindKey(Pointer(Filename),@CompareFilenameWithCachedFile);
  if Node=nil then begin
    // new file
    Result:=TPas2jsCachedFile.Create(Self,Filename);
    FFiles.Add(Result);
  end else begin
    Result:=TPas2jsCachedFile(Node.Data);
  end;
  Result.Load(true);
end;

function TPas2jsFilesCache.NormalizeFilename(const Filename: string;
  RaiseOnError: boolean): string;
begin
  Result:=Filename;
  if ExtractFilename(Result)='' then
    if RaiseOnError then
      raise EFileNotFoundError.Create('invalid file name "'+Filename+'"');
  Result:=ExpandFileNameUTF8(Result);
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
        raise EFileNotFoundError.Create('invalid custom JS file name "'+InsertFilenames[i]+'"');
      aFile:=LoadTextFile(Filename);
      if aFile.Source='' then continue;
      aWriter.WriteFile(aFile.Source,Filename);
    end
  finally
    FileResolver.Free;
  end;
end;

function TPas2jsFilesCache.IndexOfInsertFilename(const aFilename: string
  ): integer;
var
  i: Integer;
begin
  for i:=0 to FInsertFilenames.Count-1 do
    if CompareFilenames(aFilename,InsertFilenames[i])=0 then
      exit(i);
  Result:=-1;
end;

procedure TPas2jsFilesCache.AddInsertFilename(const aFilename: string);
begin
  if IndexOfInsertFilename(aFilename)<0 then
    InsertFilenames.Add(aFilename);
end;

procedure TPas2jsFilesCache.RemoveInsertFilename(const aFilename: string);
var
  i: Integer;
begin
  i:=IndexOfInsertFilename(aFilename);
  if i>=0 then
    InsertFilenames.Delete(i);
end;

end.

