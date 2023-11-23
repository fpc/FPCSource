unit System.IOUtils;
{
   This file is part of the Free Pascal run time library.
    Copyright (c) 2022 the Free Pascal development team

   FPC/Lazarus Replacement for IOUtils from Delphi 10.4
   Initially written 2022 by Dirk Jansen, completed by Michael Van Canneyt

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

{$MODE OBJFPC}
{$H+}
{ $IFDEF VER_3_2}
{$modeswitch nestedprocvars}
{ $ELSE}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{ $ENDIF}
{$modeswitch arrayoperators}

interface

uses
  Classes, SysUtils, Types, streamex;

type

  TPathPrefixType = (pptNoPrefix, pptExtended, pptExtendedUNC);
  TSearchOption    = (soTopDirectoryOnly, soAllDirectories);
  TFileMode        = (fmCreateNew, fmCreate, fmOpen, fmOpenOrCreate,
                      fmTruncate, fmAppend);
  TFileAccess      = (faRead, faWrite, faReadWrite);
  TFileShare       = (fsNone, fsRead, fsWrite, fsReadWrite);
  {$IF DEFINED(WINDOWS)}
    TFileAttribute = (faReadOnly, faHidden, faSystem, faDirectory, faArchive,
                      faDevice, faNormal, faTemporary, faSparseFile,
                      faReparsePoint, faCompressed, faOffline,
                      faNotContentIndexed, faEncrypted, faSymLink) ;
  {$ELSEIF DEFINED(UNIX)}
    TFileAttribute = (faNamedPipe, faCharacterDevice, faDirectory, faBlockDevice,
                      faNormal, faSymLink, faSocket, faWhiteout,
                      faOwnerRead, faOwnerWrite, faOwnerExecute,
                      faGroupRead, faGroupWrite, faGroupExecute,
                      faOthersRead, faOthersWrite, faOthersExecute,
                      faUserIDExecution, faGroupIDExecution, faStickyBit);
  {$ELSE}
    TFileAttribute = (faReadOnly, faHidden, faSystem, faDirectory, faArchive,
                      faNormal, faSymLink);

  {$ENDIF}

  TFileAttributes = set of TFileAttribute;

  TFilterPredicate       = function(const aPath: string; const SearchRec: TSearchRec): Boolean;
{$IFDEF VER3_2}
  TFilterPredicateLocal = function(const aPath: string; const SearchRec: TSearchRec): Boolean is nested;
{$ELSE}
  TFilterPredicateLocal = reference to function(const aPath: string; const SearchRec: TSearchRec): Boolean;
{$ENDIF}
  TFilterPredicateObject = function(const aPath: string; const SearchRec: TSearchRec): Boolean of object;

  { TDirectory }

type
  TDirectory = class

  protected
    class function GetFilesAndDirectories(const aPath, aSearchPattern: string;const aSearchOption: TSearchOption; const SearchAttributes: TFileAttributes; const aPredicate: TFilterPredicateLocal): TStringDynArray;  overload;
    class function GetFilesAndDirectories(const aPath, aSearchPattern: string;const aSearchOption: TSearchOption; const SearchAttributes: TFileAttributes; const aPredicate: TFilterPredicateObject): TStringDynArray;  overload;
    class function GetFilesAndDirectories(const aPath, aSearchPattern: string;const aSearchOption: TSearchOption; const SearchAttributes: TFileAttributes; const aPredicate: TFilterPredicate): TStringDynArray;  overload;
  public
    class procedure Copy(const SourceDirName, DestDirName: string);
    class procedure CreateDirectory(const aPath: string);
    class procedure Delete(const aPath: string); overload;
    class procedure Delete(const aPath: string; const Recursive: Boolean); overload;
    class function Exists(const aPath: string; FollowLink: Boolean = True): Boolean;
    class function GetAttributes(const aPath: string; FollowLink: Boolean = True): TFileAttributes;
    class function GetCurrentDirectory: string;
    class procedure SetCurrentDirectory(const aPath: string);
    class function GetLogicalDrives: TStringDynArray;
    //class function GetCreationTime(const aPath: string): TDateTime;
    //class function GetCreationTimeUtc(const aPath: string): TDateTime;
    //class function GetLastAccessTime(const aPath: string): TDateTime;
    //class function GetLastAccessTimeUtc(const aPath: string): TDateTime;
    //class function GetLastWriteTime(const aPath: string): TDateTime;
    //class function GetLastWriteTimeUtc(const aPath: string): TDateTime;
    class procedure SetAttributes(const aPath: string; const Attributes: TFileAttributes);
    //class procedure SetCreationTime(const aPath: string; const CreationTime: TDateTime);
    //class procedure SetCreationTimeUtc(const aPath: string; const CreationTime: TDateTime);
    //class procedure SetLastAccessTime(const aPath: string; const LastAccessTime: TDateTime);
    //class procedure SetLastAccessTimeUtc(const aPath: string; const LastAccessTime: TDateTime);
    //class procedure SetLastWriteTime(const aPath: string; const LastWriteTime: TDateTime);
    //class procedure SetLastWriteTimeUtc(const aPath: string; const LastWriteTime: TDateTime);
    class function GetParent(const aPath: string): string;
    class function GetDirectories(const aPath: string): TStringDynArray; overload;
    class function GetDirectories(const aPath: string; const aPredicate: TFilterPredicateLocal): TStringDynArray; overload;
    class function GetDirectories(const aPath: string; const aPredicate: TFilterPredicateObject): TStringDynArray; overload;
    class function GetDirectories(const aPath: string; const aPredicate: TFilterPredicate): TStringDynArray; overload;
    class function GetDirectories(const aPath, aSearchPattern: string): TStringDynArray; overload;
    class function GetDirectories(const aPath, aSearchPattern: string; const aPredicate: TFilterPredicateLocal): TStringDynArray; overload;
    class function GetDirectories(const aPath, aSearchPattern: string; const aPredicate: TFilterPredicateObject): TStringDynArray; overload;
    class function GetDirectories(const aPath, aSearchPattern: string; const aPredicate: TFilterPredicate): TStringDynArray; overload;
    class function GetDirectories(const aPath, aSearchPattern: string; const aSearchOption: TSearchOption): TStringDynArray; overload;
    class function GetDirectories(const aPath, aSearchPattern: string; const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateLocal): TStringDynArray; overload;
    class function GetDirectories(const aPath, aSearchPattern: string; const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateObject): TStringDynArray; overload;
    class function GetDirectories(const aPath, aSearchPattern: string; const aSearchOption: TSearchOption; const aPredicate: TFilterPredicate): TStringDynArray; overload;
    class function GetDirectories(const aPath: string; const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateLocal): TStringDynArray; overload;
    class function GetDirectories(const aPath: string; const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateObject): TStringDynArray; overload;
    class function GetDirectories(const aPath: string; const aSearchOption: TSearchOption; const aPredicate: TFilterPredicate): TStringDynArray; overload;
    //class function GetDirectoryRoot(const aPath: string): string; { TODO -odj : UNC => \\Servername\Freigabe, sonst c:\, d:\ usw. }
    class function GetFiles(const aPath: string): TStringDynArray; overload;
    class function GetFiles(const aPath: string; const aPredicate: TFilterPredicateLocal): TStringDynArray; overload;
    class function GetFiles(const aPath: string; const aPredicate: TFilterPredicateObject): TStringDynArray; overload;
    class function GetFiles(const aPath: string; const aPredicate: TFilterPredicate): TStringDynArray; overload;
    class function GetFiles(const aPath, aSearchPattern: string): TStringDynArray; overload;
    class function GetFiles(const aPath, aSearchPattern: string; const aPredicate: TFilterPredicateLocal): TStringDynArray; overload;
    class function GetFiles(const aPath, aSearchPattern: string; const aPredicate: TFilterPredicateObject): TStringDynArray; overload;
    class function GetFiles(const aPath, aSearchPattern: string; const aPredicate: TFilterPredicate): TStringDynArray; overload;
    class function GetFiles(const aPath, aSearchPattern: string; const aSearchOption: TSearchOption): TStringDynArray; overload;
    class function GetFiles(const aPath, aSearchPattern: string;const aSearchOption: TSearchOption;const aPredicate: TFilterPredicateLocal): TStringDynArray; overload;
    class function GetFiles(const aPath, aSearchPattern: string;const aSearchOption: TSearchOption;const aPredicate: TFilterPredicateObject): TStringDynArray; overload;
    class function GetFiles(const aPath, aSearchPattern: string;const aSearchOption: TSearchOption;const aPredicate: TFilterPredicate): TStringDynArray; overload;
    class function GetFiles(const aPath: string;const aSearchOption: TSearchOption;const aPredicate: TFilterPredicateLocal): TStringDynArray; overload;
    class function GetFiles(const aPath: string;const aSearchOption: TSearchOption;const aPredicate: TFilterPredicateObject): TStringDynArray; overload;
    class function GetFiles(const aPath: string;const aSearchOption: TSearchOption;const aPredicate: TFilterPredicate): TStringDynArray; overload;
    class function GetFileSystemEntries(const aPath: string): TStringDynArray;overload;
    class function GetFileSystemEntries(const aPath: string; const aPredicate: TFilterPredicateLocal): TStringDynArray; overload;
    class function GetFileSystemEntries(const aPath: string; const aPredicate: TFilterPredicateObject): TStringDynArray; overload;
    class function GetFileSystemEntries(const aPath: string; const aPredicate: TFilterPredicate): TStringDynArray; overload;
    class function GetFileSystemEntries(const aPath, aSearchPattern: string): TStringDynArray; overload;
    class function GetFileSystemEntries(const aPath, aSearchPattern: string; const aPredicate: TFilterPredicateLocal): TStringDynArray; overload;
    class function GetFileSystemEntries(const aPath, aSearchPattern: string; const aPredicate: TFilterPredicateObject): TStringDynArray; overload;
    class function GetFileSystemEntries(const aPath, aSearchPattern: string; const aPredicate: TFilterPredicate): TStringDynArray; overload;
    class function GetFileSystemEntries(const aPath: string; const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateLocal): TStringDynArray; overload;
    class function GetFileSystemEntries(const aPath: string; const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateObject): TStringDynArray; overload;
    class function GetFileSystemEntries(const aPath: string; const aSearchOption: TSearchOption; const aPredicate: TFilterPredicate): TStringDynArray; overload;
    Class Procedure ForAllEntries(const aPath, aPattern: string; const aBefore, aAfter: TFilterPredicateLocal; aRecursive: Boolean);
    class function IsEmpty(const aPath: string): Boolean;
    class function IsRelativePath(const aPath: string): Boolean;
    class procedure Move(const SourceDirName, DestDirName: string);
  end;

  { TPath }

  TPath = class
  private
    class var FAltDirectorySeparatorChar: Char;
    class var FDirectorySeparatorChar: Char;
    class var FExtensionSeparatorChar: Char;
    class var FPathSeparator: Char;
    class var FVolumeSeparatorChar: Char;
    class var FInvalidPathChars : TCharArray;
    class var FinvalidFileNameChars : TCharArray;
    // FNMatch is case sensitive!
    class function FNMatch(const Pattern, Name: string): Boolean;
    class function IntGetPathRoot(const aPath: string): string;
    // Return position of first char after \\?\(UNC). Optionally return prefixtype.
    class function SkipExtendedPrefix(const aPath: string; out Prefix: TPathPrefixType): SizeInt;
    class function SkipExtendedPrefix(const aPath: String): SizeInt;
  public
    class constructor Create;
    class function IsValidPathChar(const AChar: Char): Boolean;
    class function IsValidFileNameChar(const AChar: Char): Boolean;
    class function HasValidPathChars(const aPath: string; const UseWildcards: Boolean = false): Boolean; inline;
    class function HasValidPathChars(const aPath: string; out Index: Integer; const UseWildcards: Boolean = false): Boolean;
    class function HasValidFileNameChars(const FileName: string; const UseWildcards: Boolean = False): Boolean; inline;
    class function HasValidFileNameChars(const FileName: string; out Index: Integer; const UseWildcards: Boolean = False): Boolean;
    class function GetExtendedPrefix(const aPath: string): TPathPrefixType;
    class function IsDriveRooted(const aPath: string): Boolean;
    class function IsExtendedPrefixed(const aPath: string): Boolean;
    class function IsRelativePath(const aPath: string): Boolean;
    class function IsUNCPath(const aPath: string): Boolean;
    class function IsUNCRooted(const aPath: string): Boolean;
    class function GetGUIDFileName(const UseSeparator: Boolean = False): string;
    class function DriveExists(const aPath: string): Boolean;
    class function MatchesPattern(const FileName, Pattern: string; const CaseSensitive: Boolean): Boolean;
    class function ChangeExtension(const aPath, Extension: string): string;
    class function Combine(const Path1, Path2: string; const ValidateParams: Boolean = True): string;
    class function Combine(const Path1, Path2, Path3: string; const ValidateParams: Boolean = True): string;
    class function Combine(const Path1, Path2, Path3, Path4: string; const ValidateParams: Boolean = True): string;
    class function Combine(const Paths: array of string; const ValidateParams: Boolean = True): string;
    class function GetDirectoryName(FileName: string): string;
    class function GetExtension(const FileName: string): string;
    class function GetFileName(const FileName: string): string;
    class function GetFileNameWithoutExtension(const FileName: string): string;
    class function GetFullPath(const aPath: string): string;
    class function GetInvalidFileNameChars: TCharArray;
    class function GetInvalidPathChars: TCharArray;
    class function GetPathRoot(const aPath: string): string;
    class function GetRandomFileName: string;
    class function GetTempFileName: string;
    class function GetTempPath: string;
    class function GetHomePath: string;
    class function GetDocumentsPath: string;
    class function GetDesktopPath: string;
    class function GetSharedDocumentsPath: string;
    class function GetLibraryPath: string;
    class function GetAppPath: string;
    class function GetCachePath: string;
    class function GetPublicPath: string;
    class function GetPicturesPath: string;
    class function GetSharedPicturesPath: string;
    class function GetCameraPath: string;
    class function GetSharedCameraPath: string;
    class function GetMusicPath: string;
    class function GetSharedMusicPath: string;
    class function GetMoviesPath: string;
    class function GetSharedMoviesPath: string;
    class function GetAlarmsPath: string;
    class function GetSharedAlarmsPath: string;
    class function GetDownloadsPath: string;
    class function GetSharedDownloadsPath: string;
    class function GetRingtonesPath: string;
    class function GetSharedRingtonesPath: string;
    class function GetTemplatesPath: string;
    class function GetAttributes(const aPath: string; aFollowLink: Boolean = True): TFileAttributes;
    class procedure SetAttributes(const aPath: string; const aAttributes: TFileAttributes);
    class function HasExtension(const aPath: string): Boolean;
    class function IsPathRooted(const aPath: string): Boolean;
    class property ExtensionSeparatorChar: Char read FExtensionSeparatorChar;
    class property AltDirectorySeparatorChar: Char read FAltDirectorySeparatorChar;
    class property DirectorySeparatorChar: Char read FDirectorySeparatorChar;
    class property PathSeparator: Char read FPathSeparator;
    class property VolumeSeparatorChar: Char read FVolumeSeparatorChar;
  end;

  { TFile }

  TFile = class
  private
    class function DetectFileEncoding(const aPath: String; out BOMLength: Integer
      ): TEncoding;
    class procedure GetFileTimestamps(const aFilename: TFileName; var aCreate, aWrite, aAccess: TDateTime; IsUTC : Boolean);
  public
    class function IntegerToFileAttributes(const Attributes: Integer): TFileAttributes;
    class function FileAttributesToInteger(const Attributes: TFileAttributes): Integer;
    class function Create(const aPath: string): TFileStream; overload;
    class function Create(const aPath: string; const BufferSize: Integer): TFileStream; overload;
    Class function OpenOrCreate(const aPath: string) : TFileStream;
    class procedure AppendAllText(const aPath, aContents: string); overload;
    class procedure AppendAllText(const aPath, Contents: string; const Encoding: TEncoding); overload;
    class function AppendText(const aPath: string): TStreamWriter;
    class procedure Copy(const SourceFileName, DestFileName: string); overload;
    class procedure Copy(const SourceFileName, DestFileName: string; const Overwrite: Boolean); overload;
    class function CreateSymLink(const Link, Target: string): Boolean;
    class function CreateText(const aPath: string): TStreamWriter;
    class procedure Delete(const aPath: string);
//{$IFDEF MSWINDOWS}
//    class procedure Decrypt(const aPath: string);
//    class procedure Encrypt(const aPath: string);
//{$ENDIF MSWINDOWS}
    class function Exists(const aPath: string; FollowLink: Boolean = True): Boolean;
    class function GetAttributes(const aPath: string; FollowLink: Boolean = True): TFileAttributes;
    class function GetCreationTime(const aPath: string): TDateTime;
    class function GetCreationTimeUtc(const aPath: string): TDateTime;
    class function GetLastAccessTime(const aPath: string): TDateTime;
    class function GetLastAccessTimeUtc(const aPath: string): TDateTime;
    class function GetLastWriteTime(const aPath: string): TDateTime;
    class function GetLastWriteTimeUtc(const aPath: string): TDateTime;
    class function GetSymLinkTarget(const aFileName: string; var SymLinkRec: TSymLinkRec): Boolean; overload;
    class function GetSymLinkTarget(const aFileName: string; var TargetName: RawByteString): Boolean; overload;
    class function GetSymLinkTarget(const aFileName: Unicodestring; var TargetName: UnicodeString): Boolean; overload;
    class procedure Move(SourceFileName, DestFileName: string);
    class function Open(const aPath: string; const aMode: TFileMode): TFileStream; overload;
    class function Open(const aPath: string; const aMode: TFileMode; const aAccess: TFileAccess): TFileStream; overload;
    class function Open(const aPath: string; const aMode: TFileMode; const aAccess: TFileAccess; const aShare: TFileShare): TFileStream; overload;
    class function OpenRead(const aPath: string): TFileStream;
    class function OpenText(const aPath: string): TStreamReader;
    class function OpenWrite(const aPath: string): TFileStream;
    class function ReadAllBytes(const aPath: string): TBytes;
    class function ReadAllLines(const aPath: string): TStringDynArray; overload;
    class function ReadAllLines(const aPath: string; const aEncoding: TEncoding): TStringDynArray; overload;
    class function ReadAllText(const aPath: string): string; overload;
    class function ReadAllText(const aPath: string; const aEncoding: TEncoding): string; overload;
    class procedure Replace(const aSource, aDestination, aBackup: string); overload;
{$IFDEF MSWINDOWS}
    class procedure Replace(const aSource, aDestination, aBackup: string; const aIgnoreMetadataErrors: Boolean); overload;
{$ENDIF MSWINDOWS}
    class procedure SetAttributes(const aPath: string; const aAttributes: TFileAttributes);
//    class procedure SetCreationTime(const aPath: string; const CreationTime: TDateTime);
//    class procedure SetCreationTimeUtc(const aPath: string; const CreationTime: TDateTime);
//    class procedure SetLastAccessTime(const aPath: string; const LastAccessTime: TDateTime);
//    class procedure SetLastAccessTimeUtc(const aPath: string; const LastAccessTime: TDateTime);
//    class procedure SetLastWriteTime(const aPath: string; const LastWriteTime: TDateTime);
//    class procedure SetLastWriteTimeUtc(const aPath: string; const LastWriteTime: TDateTime);
    class procedure WriteAllBytes(const aPath: string; const aBytes: TBytes);
    class procedure WriteAllLines(const aPath: string; const aContents: TStringDynArray); overload;
    class procedure WriteAllLines(const aPath: string; const aContents: TStringDynArray; const aEncoding: TEncoding); overload;
    class procedure WriteAllText(const aPath, aContents: string); overload;
    class procedure WriteAllText(const aPath, aContents: string; const aEncoding: TEncoding); overload;
  end;

implementation

uses
  {$IfDef MSWINDOWS}
    windows, WinDirs,
  {$EndIf}
  {$IfDef WINCE}
    windows,
  {$EndIf}
  {$IfDef UNIX}
    BaseUnix,
  {$EndIf}
   DateUtils
  ;

ResourceString
  SErrFileExists = 'File "%s" already exists';
  SErrFileNotFound  = 'File "%s" does not exist';
  SErrInvalidCharsInPath = 'Filename "%s" contains invalid characters.' ;
  SErrEmptyPath = 'Error: aPath is empty.'  ;

  {$IfDef Unix}
  errStatFailed = 'Fstat for %a failed. Err.No.: %d';
  {$EndIf}

{$IFDEF MSWINDOWS}
Const
  WinAttrs : Array[TFileAttribute] of Integer =
     (FILE_ATTRIBUTE_READONLY, FILE_ATTRIBUTE_HIDDEN, FILE_ATTRIBUTE_SYSTEM,
      FILE_ATTRIBUTE_DIRECTORY,FILE_ATTRIBUTE_ARCHIVE, FILE_ATTRIBUTE_DEVICE,
      FILE_ATTRIBUTE_NORMAL, FILE_ATTRIBUTE_TEMPORARY,FILE_ATTRIBUTE_SPARSE_FILE,
      FILE_ATTRIBUTE_REPARSE_POINT, FILE_ATTRIBUTE_COMPRESSED, FILE_ATTRIBUTE_OFFLINE,
      FILE_ATTRIBUTE_NOT_CONTENT_INDEXED, FILE_ATTRIBUTE_ENCRYPTED,FILE_ATTRIBUTE_REPARSE_POINT);
{$ENDIF}
{$IFDEF WINCE}
  { Missing attributes are put to zero }
Const
  WinAttrs : Array[TFileAttribute] of Integer =
     (FILE_ATTRIBUTE_READONLY, FILE_ATTRIBUTE_HIDDEN, FILE_ATTRIBUTE_SYSTEM,
      FILE_ATTRIBUTE_DIRECTORY,FILE_ATTRIBUTE_ARCHIVE, 0{FILE_ATTRIBUTE_DEVICE},
      FILE_ATTRIBUTE_NORMAL, FILE_ATTRIBUTE_TEMPORARY,FILE_ATTRIBUTE_SPARSE_FILE,
      FILE_ATTRIBUTE_REPARSE_POINT, FILE_ATTRIBUTE_COMPRESSED, FILE_ATTRIBUTE_OFFLINE,
      FILE_ATTRIBUTE_NOT_CONTENT_INDEXED, FILE_ATTRIBUTE_ENCRYPTED,FILE_ATTRIBUTE_REPARSE_POINT);
{$ENDIF}

{$IFDEF WINDOWS}
function FileAttributesToFlags(const Attributes: TFileAttributes): Integer;

var
  A : TFileAttribute;
begin
  Result:=0;
  For a in TFileAttributes do
    if a in Attributes then
      Result:=Result+WinAttrs[a];
end;

function FlagsToFileAttributes(const Flags : Integer) : TFileAttributes;

var
  A : TFileAttribute;
begin
  Result:=[];
  For a in TFileAttributes do
    if (Flags and WinAttrs[a])<>0 then
      Include(Result,a);
end;
{$ENDIF}

{$IFDEF unix}

Const
  UnixModes : Array[TFileAttribute] of Integer =
    (
    0,0,0,0, // 0 means it can't be set
    0,0,0,0,
    S_IRUSR, S_IWUSR,S_IXUSR,
    S_IRGRP,S_IWGRP, S_IXGRP,
    S_IROTH,S_IWOTH,S_IXOTH,
    S_ISUID,S_ISGID,0 {S_IVTX}
    );

function FileAttributesToMode(const Attributes: TFileAttributes): Integer;

var
  A : TFileAttribute;
begin
  Result:=0;
  For a in TFileAttributes do
    if a in Attributes then
      Result:=Result+UnixModes[a];
end;

function ModeToFileAttributes(const Flags : Integer) : TFileAttributes;

var
  A : TFileAttribute;
begin
  Result:=[];
  For a in TFileAttributes do
    if (Flags and UnixModes[a])<>0 then
      Include(Result,a);
end;
{$ENDIF}


function UTCtoLocal(const UTCDateTime: TDateTime): TDateTime;
begin
  Result:=Sysutils.UniversalTimeToLocal(UTCDateTime,GetLocalTimeOffset);
 end;

{ TPath }

class constructor TPath.Create;
var
  C : Char;
begin
  FAltDirectorySeparatorChar:=#0;
  For C in AllowDirectorySeparators do
    if (C<>System.DirectorySeparator) and (FAltDirectorySeparatorChar=#0) then
      FAltDirectorySeparatorChar:=C;
  FExtensionSeparatorChar   := System.ExtensionSeparator;
  FDirectorySeparatorChar   := System.DirectorySeparator;
  FPathSeparator            := System.PathSeparator;

  if Length(DriveSeparator)>0 then
  begin
    {$ifdef UNIX}
    FVolumeSeparatorChar     := DriveSeparator[1]
    {$else}
    FVolumeSeparatorChar     := DriveSeparator
    {$endif}
  end
  else
    FVolumeSeparatorChar    :=#0;
end;

class function TPath.IsValidPathChar(const AChar: Char): Boolean;


begin
  Result:=(Ord(aChar)<32);
{$IFNDEF UNIX}
  Result:=Result or CharInSet(aChar,[ '"', '<', '>', '|']);
{$ENDIF}
  Result:=Not Result;
end;

class function TPath.IsValidFileNameChar(const AChar: Char): Boolean;
begin
  Result:=(Ord(aChar)<32);
{$IFNDEF UNIX}
  Result:=Result or CharInSet(aChar,[ '"', '*', '/', ':', '<', '>', '?', '\', '|']);
{$ELSE}
  Result:=Result or CharInSet(aChar,[ '/', '~']) ;
{$ENDIF}
  Result:=Not Result;
end;

class function TPath.SkipExtendedPrefix(const aPath: String): SizeInt;

Var
  P : TPathPrefixType;

begin
  Result:=SkipExtendedPrefix(aPath,P);
end;

class function TPath.SkipExtendedPrefix(const aPath: string; out Prefix: TPathPrefixType): SizeInt;

Const
   pPrefix = '\\?\';
   UNCPrefix = pPrefix +'UNC\';
   LenPrefix = Length(pPrefix);
   lenUNCPrefix = Length(UNCPrefix);

begin
  Prefix:=GetExtendedPrefix(aPath);
  case Prefix of
  TPathPrefixType.pptExtended:
    Result:=LenPrefix+1;
  TPathPrefixType.pptExtendedUNC:
    Result:=LenUNCPrefix+1;
  else
    Result:=1;
  end;
end;

class function TPath.HasValidPathChars(const aPath: string;
  const UseWildcards: Boolean): Boolean;
var
  dummy: Integer;
begin
  Result:=TPath.HasValidPathChars(aPath, dummy, UseWildcards);
end;

class function TPath.HasValidPathChars(const aPath: string;
  out Index: integer; const UseWildcards: Boolean): Boolean;
var
  P: PChar;
  S,I,Len: Integer;
  C : Char;
  CheckWC : Boolean;
begin
  Len:=Length(aPath);
  if Len=0 then
    Exit(True);
  Result:=False;
  CheckWC:=not UseWildcards;
  P:=PChar(aPath);
  S:=SkipExtendedPrefix(aPath);
  Inc(P,S-1);
  for I:=S to Len do
    begin
    Index:=i;
    C:=P^;
    if CheckWC and (CharInSet(C,['?','*'])) then
      exit;
    if not IsValidPathChar(C) then
      exit;
    Inc(P);
    end;
  Result:=True;
end;

class function TPath.HasValidFileNameChars(const FileName: string;
  const UseWildcards: Boolean): Boolean;
var
  dummy: Integer;
begin
  Result:=HasValidFileNameChars(FileName, dummy, UseWildCards);
end;

class function TPath.HasValidFileNameChars(const FileName: string;
  out Index: Integer; const UseWildcards: Boolean): Boolean;
var
  P: PChar;
  S,I,Len: Integer;
  C : Char;
  CheckWC : Boolean;
begin
  Len:=Length(FileName);
  if Len=0 then
    Exit(True);
  Result:=False;
  CheckWC:=not UseWildcards;
  P:=PChar(FileName);
  S:=SkipExtendedPrefix(FileName);
  Inc(P,S-1);
  for I:=S to Len do
    begin
    Index:=I;
    C:=P^;
    if CheckWC and (CharInSet(C,['?','*'])) then
      exit;
    if not IsValidFileNameChar(C) then
      exit;
    Inc(P);
    end;
  Result:=True;
end;

class function TPath.GetExtendedPrefix(const aPath: string): TPathPrefixType;
begin
  Result:=TPathPrefixType.pptNoPrefix;
{$IFDEF MSWINDOWS}
  if aPath.ToUpper.StartsWith(PathDelim + PathDelim + '?' + PathDelim + 'UNC' + PathDelim) then
    Result:=TPathPrefixType.pptExtendedUNC
  else if aPath.StartsWith(PathDelim + PathDelim + '?' + PathDelim) then
    Result:=TPathPrefixType.pptExtended
{$ENDIF}
end;

class function TPath.IsDriveRooted(const aPath: string): Boolean;
begin
  {$IfDef MSWINDOWS}
    Result:=(Length(aPath) > 1) and
              (aPath[1] in ['a'..'z', 'A'..'Z']) and
              (aPath[2] = ':');
  {$Else}
    Result:=False;
  {$EndIf}
end;

class function TPath.IsExtendedPrefixed(const aPath: string): Boolean;
begin
{$IfDef MSWINDOWS}
  Result:=aPath.StartsWith(PathSeparator + PathSeparator + '?' + PathDelim);
{$Else}
  Result:=False;
{$EndIf}

end;

class function TPath.IsRelativePath(const aPath: string): Boolean;
begin
  Result:=(not aPath.StartsWith(PathDelim)) and
            (not TPath.IsDriveRooted(aPath)) and
            (not TPath.IsUNCRooted(aPath));
end;

class function TPath.IsUNCPath(const aPath: string): Boolean;
begin
  {$IfDef MSWINDOWS}
    Result:=IsUNCRooted(aPath) and HasValidPathChars(aPath,False)
  {$Else}
    Result:=False;
  {$EndIf}
end;

class function TPath.IsUNCRooted(const aPath: string): Boolean;
begin
  {$IfDef MSWINDOWS}
  Result:=False;
  if (Length(aPath)>=3) and (Copy(aPath,1,2)='//') then
    If (aPath[3]='?') then
      Result:=GetExtendedPrefix(aPath) = TPathPrefixType.pptExtendedUNC
    else
      Result:=IsValidPathChar(aPath[3]);
  {$Else}
    Result:=False;
  {$EndIf}
end;

class function TPath.GetGUIDFileName(const UseSeparator: Boolean): string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result:=GUIDToString(Guid);
  if not UseSeparator then
    Result:=StringReplace(Result, '-', '', [rfReplaceAll]);
  Result:=Copy(Result, 2, Length(Result) - 2);
end;

class function TPath.DriveExists(const aPath: string): Boolean;
begin
  Result:=False;
  {$IfDef MSWINDOWS}
    try
      case GetDriveType(PChar(ExtractFileDrive(aPath))) of
        DRIVE_REMOVABLE,
        DRIVE_FIXED,
        DRIVE_REMOTE,
        DRIVE_CDROM,
        DRIVE_RAMDISK: Result:=True;
      end;
    except
      { no exception }
    end;
  {$EndIf}
end;

{ assumes that pattern and name have the same code page }
class function TPath.FNMatch(const Pattern, Name: string): Boolean;
Var
  LenPat,LenName : longint;

  function NameUtf8CodePointLen(index: longint): longint;
    var
      MaxLookAhead: longint;
    begin
      MaxLookAhead:=LenName-Index+1;
      { abs so that in case of an invalid sequence, we count this as one
        codepoint }
      NameUtf8CodePointLen:=abs(Utf8CodePointLen(pansichar(@Name[index]),MaxLookAhead,true));
      { if the sequence was incomplete, use the incomplete sequence as
        codepoint }
      if NameUtf8CodePointLen=0 then
        NameUtf8CodePointLen:=MaxLookAhead;
    end;

    procedure GoToLastByteOfUtf8CodePoint(var j: longint);
      begin
        inc(j,NameUtf8CodePointLen(j)-1);
      end;

  { input:
      i: current position in pattern (start of utf-8 code point)
      j: current position in name (start of utf-8 code point)
      update_i_j: should i and j be changed by the routine or not

    output:
      i: if update_i_j, then position of last matching part of code point in
         pattern, or first non-matching code point in pattern. Otherwise the
         same value as on input.
      j: if update_i_j, then position of last matching part of code point in
         name, or first non-matching code point in name. Otherwise the
         same value as on input.
      result: true if match, false if no match
  }
  function CompareUtf8CodePoint(var i,j: longint; update_i_j: boolean): Boolean;
    var
      bytes,
      new_i,
      new_j: longint;
    begin
      bytes:=NameUtf8CodePointLen(j);
      new_i:=i;
      new_j:=j;
      { ensure that a part of an UTF-8 codepoint isn't interpreted
        as '*' or '?' }
      repeat
        dec(bytes);
        Result:=
          (new_j<=LenName) and
          (new_i<=LenPat) and
          (Pattern[new_i]=Name[new_j]);
        inc(new_i);
        inc(new_j);
      until not(Result) or
            (bytes=0);
      if update_i_j then
        begin
          i:=new_i;
          j:=new_j;
        end;
    end;


  Function DoFNMatch(i,j:longint):Boolean;
  Var
    UTF8, Found : boolean;
  Begin
    Found:=true;
    { ensure that we don't skip partial characters in UTF-8-encoded strings }
    UTF8:=StringCodePage(Name)=CP_UTF8;
    While Found and (i<=LenPat) Do
     Begin
       Case Pattern[i] of
        '?' :
          begin
            Found:=(j<=LenName);
            if UTF8 then
              GoToLastByteOfUtf8CodePoint(j);
          end;
        '*' : Begin
              {find the next character in pattern, different of ? and *}
                while Found do
                  begin
                    inc(i);
                    if i>LenPat then
                      Break;
                    case Pattern[i] of
                      '*' : ;
                      '?' : begin
                              if j>LenName then
                                begin
                                  DoFNMatch:=false;
                                  Exit;
                                end;
                              if UTF8 then
                                GoToLastByteOfUtf8CodePoint(j);
                              inc(j);
                            end;
                      else
                        Found:=false;
                      end;
                 end;
                Assert((i>LenPat) or ( (Pattern[i]<>'*') and (Pattern[i]<>'?') ));
                { Now, find in name the character which i points to, if the * or
                  ? wasn't the last character in the pattern, else, use up all
                  the chars in name }
                Found:=false;
                if (i<=LenPat) then
                  begin
                    repeat
                      {find a letter (not only first !) which maches pattern[i]}
                      if UTF8 then
                        begin
                          while (j<=LenName) and
                                ((name[j]<>pattern[i]) or
                                 not CompareUtf8CodePoint(i,j,false)) do
                            begin
                              GoToLastByteOfUtf8CodePoint(j);
                              inc(j);
                            end;
                        end
                      else
                        begin
                          while (j<=LenName) and (name[j]<>pattern[i]) do
                            inc (j);
                        end;
                      if (j<LenName) then
                        begin
                          { while positions i/j have already been checked, in
                            case of UTF-8 we have to ensure that we don't split
                            a code point. Otherwise we can skip over comparing
                            the same characters twice }
                          if DoFnMatch(i+ord(not UTF8),j+ord(not UTF8)) then
                            begin
                              i:=LenPat;
                              j:=LenName;{we can stop}
                              Found:=true;
                              Break;
                            end
                          { We didn't find one, need to look further }
                          else
                            begin
                              if UTF8 then
                                GoToLastByteOfUtf8CodePoint(j);
                              inc(j);
                            end;
                        end
                      else if j=LenName then
                        begin
                          Found:=true;
                          Break;
                        end;
                      { This 'until' condition must be j>LenName, not j>=LenName.
                        That's because when we 'need to look further' and
                        j = LenName then loop must not terminate. }
                    until (j>LenName);
                  end
                else
                  begin
                    j:=LenName;{we can stop}
                    Found:=true;
                  end;
              end;
        #128..#255:
          begin
            Found:=(j<=LenName) and (pattern[i]=name[j]);
            if Found and UTF8 then
              begin
                { ensure that a part of an UTF-8 codepoint isn't matched with
                  '*' or '?' }
                Found:=CompareUtf8CodePoint(i,j,true);
                { at this point, either Found is false (and we'll stop), or
                  both pattern[i] and name[j] are the end of the current code
                  point and equal }
              end
          end
       else {not a wildcard character in pattern}
         Found:=(j<=LenName) and (pattern[i]=name[j]);
       end;
       inc(i);
       inc(j);
     end;
    DoFnMatch:=Found and (j>LenName);
  end;

Begin {start FNMatch}
  LenPat:=Length(Pattern);
  LenName:=Length(Name);
  FNMatch:=DoFNMatch(1,1);
End;

class function TPath.MatchesPattern(const FileName, Pattern: string;
  const CaseSensitive: Boolean): Boolean;

Var
  lFile,lPattern : String;

begin
  lFile:=FileName;
  lPattern:=Pattern;
  if not CaseSensitive then
    begin
    lFile:=LowerCase(lFile);
    lPattern:=LowerCase(lPattern);
    end;
  if not HasValidFileNameChars(FileName, False) then
    raise EArgumentException.CreateFmt(SErrInvalidCharsInPath, [FileName]);
  Result:=(Pattern=AllFilesMask) or FNMatch(lPattern,lFile);
end;

class function TPath.ChangeExtension(const aPath, Extension: string): string;
begin
  Result:=ChangeFileExt(aPath, Extension);
end;

class function TPath.Combine(const Path1, Path2: string; const ValidateParams : Boolean = True): string;
begin
  if (Path1='') or (Path2='') then
    begin
    if Path1='' then
      Result:=Path2
    else
      Result:=Path1
    end
  else
    begin
    if not TPath.HasValidPathChars(Path1,False) then
      Raise EArgumentException.CreateFmt(SErrInvalidCharsInPath,[Path1]);
    if not TPath.HasValidPathChars(Path2,False) then
      Raise EArgumentException.CreateFmt(SErrInvalidCharsInPath,[Path2]);
    Result:=ConcatPaths([Path1, Path2]);
    end;
end;

class function TPath.Combine(const Path1, Path2, Path3 : string; const ValidateParams : Boolean = True): string;

begin
  Result:=Combine([Path1,Path2,Path3],ValidateParams);
end;

class function TPath.Combine(const Path1, Path2, Path3,Path4 : string; const ValidateParams : Boolean = True): string;

begin
  Result:=Combine([Path1,Path2,Path3,Path4],ValidateParams);
end;

class function TPath.Combine(const Paths: array of string; const ValidateParams: Boolean = True): string;
  function AppendPathDelim(const Path: string): string;
  begin
    if (Path = '') or (Path[Length(Path)] in AllowDirectorySeparators)
    {$ifdef mswindows}
      //don't add a PathDelim to e.g. 'C:'
      or ((Length(Path) = 2) and (Path[2] = ':') and (UpCase(Path[1]) in ['A'..'Z']))
    {$endif}
    then
      Result:=Path
    else
      Result:=Path + DirectorySeparator;
  end;
var
  i: Integer;
  Path: String;
begin
  if ValidateParams then
    for i := Low(Paths) to High(Paths) do
      if not TPath.HasValidPathChars(Paths[i], False) then
        Raise EInOutArgumentException.CreateFmt(SErrInvalidCharsInPath,[Paths[i]],Path[i]);
  Result := '';
  for i := High(Paths) downto Low(Paths) do
  begin
    Path := Paths[i];
    if (Path <> '') then
    begin
      if (Result <> '') then
        Path := AppendPathDelim(Path);
      Result := Path + Result;
      if not TPath.IsRelativePath(Result) then
        Exit;
    end;
  end;
end;

class function TPath.GetDirectoryName(FileName: string): string;
begin
  Result:=ExcludeTrailingPathDelimiter(ExtractFileDir(FileName));
end;

class function TPath.GetExtension(const FileName: string): string;
begin
  Result:=ExtractFileExt(FileName);
end;

class function TPath.GetFileName(const FileName: string): string;
begin
  Result:=ExtractFileName(FileName);
end;

class function TPath.GetFileNameWithoutExtension(const FileName: string
  ): string;
begin
  Result:=ChangeFileExt(ExtractFileName(FileName), '');
end;

class function TPath.GetFullPath(const aPath: string): string;
begin
  Result:=ExpandFileName(aPath);
end;

class function TPath.GetInvalidFileNameChars: TCharArray;

Const
   ExtraChars : Array of Char
   {$IFNDEF UNIX}
     = ('"', '*', '/', ':', '<', '>', '?', '\', '|');
   {$ELSE}
     = ( '/', '~');
   {$ENDIF}

Var
  I : Integer;

begin
  if Length(FInvalidFileNameChars)=0 then
    begin
    SetLength(FInvalidFileNameChars,32+Length(ExtraChars));
    For I:=0 to 31 do
      FInvalidFileNameChars[i]:=Char(I);
    For I:=0 to Length(ExtraCHars)-1 do
      FInvalidFileNameChars[32+I]:=ExtraChars[i];
    end;
  Result:=FInvalidFilenameChars;
end;

class function TPath.GetInvalidPathChars: TCharArray;

Const
   {$IFDEF UNIX}
   ExtraChars : Array of Char = ( '"', '<', '>', '|' );
   {$ELSE}
   ExtraChars : Array of Char = ();
   {$ENDIF}
Var
  I : Integer;

begin
  if Length(FInvalidPathChars)=0 then
    begin
    SetLength(FInvalidPathChars,32+Length(ExtraChars));
    For I:=0 to 31 do
      FInvalidPathChars[i]:=Char(I);
    For I:=0 to Length(ExtraCHars)-1 do
      FInvalidPathChars[32+I]:=ExtraChars[i];
    end;
  Result:=FInvalidPathChars;

end;

class function TPath.GetPathRoot(const aPath: string): string;
begin
  if Trim(aPath) = '' then
    raise EInOutError.Create(SErrEmptyPath);
  if not HasValidPathChars(aPath, True) then
    raise EInOutError.CreateFmt(SErrInvalidCharsInPath, [aPath]);

  Result:=IntGetPathRoot(aPath);
end;

{$IF DEFINED(Unix)}
class function TPath.IntGetPathRoot(const aPath: string): string;

begin
  if (aPath <> '') and (aPath.Chars[0] = PathDelim) then
    Result:=PathDelim
  else
    Result:='';
end;
{$ELSEIF DEFINED(WINDOWS)}
class function TPath.IntGetPathRoot(const aPath: string): string;

var
  lPath: string;
  NeedSeparator: Boolean;
  aPos, ResLen, len: Integer;
  PPT: TPathPrefixType;

begin
  lPath:=SetDirSeparators(aPath);
  len:=Length(lPath);
  NeedSeparator:=False;
  aPos:=SkipExtendedPrefix(lPath, PPT);
  ResLen:=0;
  if IsDriveRooted(lPath) then  // Drive letter
    begin
    ResLen:=aPos+1;
    NeedSeparator:=(Len > 2) and (lPath[aPos]=PathDelim);
    if NeedSeparator and (PPT in [TPathPrefixType.pptExtended, TPathPrefixType.pptExtendedUNC]) then
      NeedSeparator:=False;
    end
  else if IsUNCRooted(aPath) then // UNC aPath
    begin
      aPos:=Pos(PathDelim,lPath,3);
      if aPos > 0 then
        begin
        ResLen:=apos;
        aPos:=Pos(PathDelim,aPath,aPos+1);
        if aPos>0 then
          ResLen:=aPos-1
        end
      else
        ResLen:=Len;
    end
  else if aPos<=Len then
    begin
    ResLen:= Ord(lPath[aPos]=PathDelim);
    if aPos<Len then
      ResLen:=ResLen+Ord(lPath[aPos+1]=PathDelim);
    end;
  Result:=Copy(lPath,1,ResLen);
  if NeedSeparator then
    Result:=Result+PathDelim;
end;
{$ELSEIF DEFINED(HASAMIGA)}
class function TPath.IntGetPathRoot(const aPath: string): string;
begin
  if Pos(DriveSeparator, aPath) > 0 then
    Result := Copy(aPath, 1, Pos(DriveSeparator, aPath))
  else
    Result := '';
end;
{$ELSE}
class function TPath.IntGetPathRoot(const aPath: string): string;
begin
  Result:='';
end;
{$ENDIF}


class function TPath.GetRandomFileName: string;

Const
  SDigits = '01234567890';
  SUppers = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  SLowers = 'abcdefghijklmnopqrstuvwxyz';

  SelectChars : Array[0..2] of string = (SDigits,SUppers,SLowers);
  SelectLengths : Array[0..2] of integer = (Length(SDigits),Length(SUppers),Length(SLowers));
  FNLen = 12;
  ExtLen = 3;
  DotAt = FNLen-ExtLen;

var
  C,I: Byte;

begin
  Result:=''; // DO NOT LOCALIZE
  SetLength(Result,FNLen);
  for i:=1 to FNLen do
    if i <> DotAt then
      begin
      C:=Random(3);
      Result[I]:=SelectChars[C][1+Random(SelectLengths[C])];
      end
    else
      Result[i]:=ExtensionSeparatorChar;
end;

class function TPath.GetTempFileName: string;
begin
  Result:=SysUtils.GetTempFileName;
end;

class function TPath.GetTempPath: string;
begin
  Result:=IncludeTrailingPathDelimiter(SysUtils.GetTempDir);
end;

class function TPath.GetHomePath: string;
begin
  Result:=SysUtils.GetUserDir;
end;

{$ifdef UNIX}
type
  TSpecialDir = (sdDesktop, sdDocuments, sdDownloads, sdMusic, sdPictures, sdPublic, sdTemplates, sdVideos);
{$IFNDEF darwin}
function GetSpecialDir(const AType: TSpecialDir): string;

const
  Names : array[TSpecialDir] of string
        = ('DESKTOP', 'DOCUMENTS', 'DOWNLOAD',  'MUSIC', 'PICTURES', 'PUBLICSHARE', 'TEMPLATES', 'VIDEOS');

var
  cfg,varname: string;
  L: TStringList;
begin
  Result := '';
  // XDG variable name
  varName:=Format('XDG_%s_DIR',[Names[AType]]);
  Cfg:=GetEnvironmentVariable('XDG_CONFIG_HOME');
  if (Cfg='') then
    Cfg:=GetUserDir+'.config/user-dirs.dirs'
  else
    CFG:=CFG+'user-dirs.dirs';
  if not FileExists(Cfg) then
    Exit;
  L:=TStringList.Create;
  try
    L.LoadFromFile(Cfg);
    Result:=AnsiDequotedStr(L.Values[VarName],'"');
  finally
    FreeAndNil(L);
  end;
  Result:=StringReplace(Result,'$HOME', ExcludeTrailingPathDelimiter(GetUserDir), [rfIgnoreCase]);
end;
{$else}
function GetSpecialDir(const AType: TSpecialDir): string;

begin
  // Todo
  Result:='';
end;
{$endif}
{$endif}

class function TPath.GetDocumentsPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_PERSONAL, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdDocuments);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$ENDIF}
end;

class function TPath.GetDesktopPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_DESKTOPDIRECTORY, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdDesktop);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetSharedDocumentsPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_COMMON_DOCUMENTS, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdPublic);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetLibraryPath: string;
begin
{$IFDEF UNIX}
  Result := GetCurrentDir;
{$ELSE}
  Result:=ExtractFilePath(ParamStr(0));
{$ENDIF}
end;

class function TPath.GetAppPath: string;
begin
  Result:=ExtractFilePath(ParamStr(0));
end;

class function TPath.GetCachePath: string;
begin
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetUserDir+'.cache'; // Check darwin
    {$ELSE}
    Result:=SysUtils.GetTempDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetPublicPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_COMMON_APPDATA, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdPublic);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetPicturesPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_MYPICTURES, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdPictures);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetSharedPicturesPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_COMMON_PICTURES, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdPublic);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetCameraPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_MYPICTURES, False);
  {$EndIf}
end;

class function TPath.GetSharedCameraPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_COMMON_PICTURES, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdPublic);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetMusicPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_MYMUSIC, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdMusic);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetSharedMusicPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_COMMON_MUSIC, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdPublic);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetMoviesPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_MYVIDEO, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdVideos);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetSharedMoviesPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_COMMON_VIDEO, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdPublic);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetAlarmsPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_MYMUSIC, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdMusic);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetSharedAlarmsPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_COMMON_MUSIC, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdPublic);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetDownloadsPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdDownloads);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetSharedDownloadsPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_COMMON_APPDATA, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdPublic);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetRingtonesPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_MYMUSIC, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdMusic);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetSharedRingtonesPath: string;
begin
  Result:='';
  {$IfDef MSWINDOWS}
    Result:=GetWindowsSpecialDir(CSIDL_COMMON_MUSIC, False);
  {$ELSE}
    {$IFDEF UNIX}
      Result:=GetSpecialDir(sdPublic);
    {$ELSE}
      Result:=GetUserDir;
    {$ENDIF}
  {$EndIf}
end;

class function TPath.GetTemplatesPath: string;

begin
  Result:='';
{$IfDef MSWINDOWS}
  Result:=GetWindowsSpecialDir(CSIDL_PERSONAL, False);
{$ELSE}
  {$IFDEF UNIX}
    Result:=GetSpecialDir(sdTemplates);
  {$ELSE}
    Result:=GetUserDir;
  {$ENDIF}
{$EndIf}

end;

class function TPath.GetAttributes(const aPath: string; aFollowLink: Boolean
  ): TFileAttributes;
begin
  Result:=TFile.GetAttributes(aPath, aFollowLink);
end;

class procedure TPath.SetAttributes(const aPath: string;
  const aAttributes: TFileAttributes);
begin
  TFile.SetAttributes(aPath, aAttributes);
end;

class function TPath.HasExtension(const aPath: string): Boolean;
begin
  Result:=not (aPath = ChangeFileExt(aPath, ''));
end;

class function TPath.IsPathRooted(const aPath: string): Boolean;
begin
  Result:=aPath.StartsWith(PathSeparator) or TPath.IsDriveRooted(aPath);
end;

{ TFile }

class procedure TFile.GetFileTimestamps(const aFilename: TFileName; var aCreate, aWrite, aAccess: TDateTime; IsUTC: Boolean);

var
  DateTime: TDateTimeInfoRec;

begin
   if FileGetDateTimeInfo(aFileName,DateTime) then
     begin
     aCreate:=DateTime.CreationTime;
     aWrite:=DateTime.TimeStamp;
     aAccess:=DateTime.LastAccessTime;
     if isUTC then
       begin
       aCreate:=LocalTimeToUniversal(aCreate);
       aWrite:=LocalTimeToUniversal(aWrite);
       aAccess:=LocalTimeToUniversal(aAccess);
       end;
     end
   else
     raise EInOutError.CreateFmt(SErrFileNotFound, [aFileName]);
end;

class function TFile.IntegerToFileAttributes(const Attributes: Integer
  ): TFileAttributes;
  procedure AddIfSet(var FileAttribs: TFileAttributes; const BitValue: Integer; FileAttrib: TFileAttribute);
  begin
    if (Attributes and BitValue) = BitValue then
      FileAttribs:=FileAttribs + [FileAttrib];
  end;
begin
  {$If Defined(UNIX)}
    // Assume full mode
    Result:=ModeToFileAttributes(Attributes);
  {$ElseIf Defined(WINDOWS)}
    // Assume all flags
    Result:=FlagsToFileAttributes(Attributes);
  {$Else}
    { Attributes supported by TSearchRec}
    Result:=[];
    AddIfSet(Result, SysUtils.faDirectory,         TFileAttribute.faDirectory);
    AddIfSet(Result, SysUtils.faSymLink{%H-},      TFileAttribute.faSymLink);
    AddIfSet(Result, SysUtils.faNormal,            TFileAttribute.faNormal);
    AddIfSet(Result, SysUtils.faDirectory,  TFileAttribute.faDirectory);
    AddIfSet(Result, SysUtils.faSymLink{%H-},    TFileAttribute.faSymLink);
    AddIfSet(Result, SysUtils.faHidden{%H-},     TFileAttribute.faHidden);
    AddIfSet(Result, SysUtils.faSysFile{%H-},    TFileAttribute.faSystem);
    AddIfSet(Result, SysUtils.faArchive,    TFileAttribute.faArchive);
  {$EndIf}
end;

{$IFDEF UNIX}
// We need full mode here, not just what TSearchRec has to offer
function FileGetAttr(const FN: string; FollowLink: Boolean): Integer;
var
  st: tstat;
  Res : Integer;

begin
  Result:=0;
  if FollowLink then
    Res:=fpstat(FN,st)
  else
    Res:=fplstat(FN, st);
  if Res=0 then
    Result := st.st_mode
end;
{$ENDIF UNIX}


class function TFile.FileAttributesToInteger(const Attributes: TFileAttributes
  ): Integer;
  procedure AddIfSet(var AttribValue: Integer; const BitValue: Integer; FileAttrib: TFileAttribute);
  begin
    if FileAttrib in Attributes then
      AttribValue:=AttribValue or BitValue;
  end;
begin
  Result:=0;
  {$IFDEF UNIX}
  // Assume full mode
  Result:=FileAttributesToMode(Attributes);
  {$ELSE}
    {$IFDEF WINDOWS}
      // Assume all flags
      Result:=FileAttributesToFlags(Attributes);
    {$ELSE}
       // Assume attrs as in TSearchRec
      AddIfSet(Result, SysUtils.faDirectory,  TFileAttribute.faDirectory);
      AddIfSet(Result, SysUtils.faSymLink{%H-},    TFileAttribute.faSymLink);
      AddIfSet(Result, SysUtils.faNormal,     TFileAttribute.faNormal);
      AddIfSet(Result, SysUtils.faReadOnly,   TFileAttribute.faReadOnly);
      AddIfSet(Result, SysUtils.faHidden{%H-},     TFileAttribute.faHidden);
      AddIfSet(Result, SysUtils.faSysFile{%H-},    TFileAttribute.faSystem);
      AddIfSet(Result, SysUtils.faArchive,    TFileAttribute.faArchive);
    {$EndIf}
  {$EndIf}
end;

class function TFile.Create(const aPath: string): TFileStream;
begin
  Result:=Create(aPath,0);
end;

class function TFile.Create(const aPath: string; const BufferSize: Integer
  ): TFileStream;
begin
  Result:=TFileStream.Create(aPath,Classes.fmCreate);
end;

class function TFile.OpenOrCreate(const aPath: string): TFileStream;
begin
  If Exists(aPath) then
    Result:=Open(aPath,fmOpen)
  else
    Result:=Create(aPath);
end;

class function TFile.DetectFileEncoding(const aPath: String; out
  BOMLength: Integer): TEncoding;

Var
  B : TBytes;

begin
  B:=[];
  Result:=TEncoding.Default;
  With TFileStream.Create(aPath,fmOpenRead or fmShareDenyWrite) do
    try
      SetLength(B,4);
      if Read(B[0],4)<2 then
        Exit;
      BOMLength:=TEncoding.GetBufferEncoding(B, Result);
    finally
      Free;
    end;
end;

class procedure TFile.AppendAllText(const aPath, aContents: string);

Var
  Encoding : TEncoding;
  BOMLength : Integer;

begin
  if FileExists(aPath) then
    Encoding:=DetectFileEncoding(aPath,BOMlength)
  else
    Encoding:=TENcoding.Default;
  AppendAllText(aPath,aContents,Encoding);
end;

class procedure TFile.AppendAllText(const aPath, Contents: string;
  const Encoding: TEncoding);

Var
  B : TBytes;
  F : TFileStream;

begin
  F:=OpenOrCreate(aPath);
  try
    {$IF SIZEOF(CHAR)=1}
    B:=Encoding.GetAnsiBytes(Contents);
    {$ELSE}
    B:=Encoding.GetBytes(Contents);
    {$ENDIF}
    F.Seek(0,soEnd);
    F.WriteBuffer(B[0],Length(B));
  finally
    F.Free;
  end;
end;

class function TFile.AppendText(const aPath: string): TStreamWriter;
begin
  Result:=TStreamWriter.Create(aPath,True)
end;

type
  TCopyFileFlag = (
    cffOverwriteFile,
    cffCreateDestDirectory,
    cffPreserveTime
    );
  TCopyFileFlags = set of TCopyFileFlag;


function CopyFile(const SrcFilename, DestFilename: string; Flags: TCopyFileFlags; ExceptionOnError: Boolean): boolean;
var
  SrcHandle: THandle;
  DestHandle: THandle;
  Buffer: array[1..4096] of byte;
  ReadCount, WriteCount, TryCount: LongInt;
begin
  Result:=False;
  // check overwrite
  if (not (cffOverwriteFile in Flags)) and FileExists(DestFileName) then
    exit;
  // check directory
  if (cffCreateDestDirectory in Flags)
     and (not DirectoryExists(ExtractFilePath(DestFileName)))
     and (not ForceDirectories(ExtractFilePath(DestFileName))) then
    exit;
  TryCount:=0;
  While TryCount <> 3 Do Begin
    SrcHandle:=FileOpen(SrcFilename, fmOpenRead or fmShareDenyWrite);
    if THandle(SrcHandle)=feInvalidHandle then Begin
      Inc(TryCount);
      Sleep(10);
    End
    Else Begin
      TryCount:=0;
      Break;
    End;
  End;
  If TryCount > 0 Then
  begin
    if ExceptionOnError then
      raise EFOpenError.CreateFmt({SFOpenError}'Unable to open file "%s"', [SrcFilename])
    else
      exit;
  end;
  try
    DestHandle:=FileCreate(DestFileName);
    if (THandle(DestHandle)=feInvalidHandle) then
    begin
      if ExceptionOnError then
        raise EFCreateError.CreateFmt({SFCreateError}'Unable to create file "%s"',[DestFileName])
      else
        Exit;
    end;
    try
      repeat
        ReadCount:=FileRead(SrcHandle,Buffer[1],High(Buffer));
        if ReadCount<=0 then break;
        WriteCount:=FileWrite(DestHandle,Buffer[1],ReadCount);
        if WriteCount<ReadCount then
        begin
          if ExceptionOnError then
            raise EWriteError.CreateFmt({SFCreateError}'Unable to write to file "%s"',[DestFileName])
          else
            Exit;
        end;
      until false;
    finally
      FileClose(DestHandle);
    end;
    if (cffPreserveTime in Flags) then
      FileSetDate(DestFilename, FileGetDate(SrcHandle));
    Result:=True;
  finally
    FileClose(SrcHandle);
  end;
end;


class procedure TFile.Copy(const SourceFileName, DestFileName: string);
begin
   CopyFile(SourceFileName, DestFileName, [cffPreserveTime],True);
end;

class procedure TFile.Copy(const SourceFileName, DestFileName: string;
  const Overwrite: Boolean);
begin
  if Overwrite then
    CopyFile(SourceFileName, DestFileName, [cffOverwriteFile, cffPreserveTime],True)
  else
    CopyFile(SourceFileName, DestFileName, [cffPreserveTime],True);
end;

class function TFile.CreateSymLink(const Link, Target: string): Boolean;
begin
{$IFDEF UNIX}
  Result:=fpLink(Target,Link)=0;
{$ELSE}
  Result:=False;
{$ENDIF}
end;

class function TFile.CreateText(const aPath: string): TStreamWriter;
begin
  Result:=TStreamWriter.Create(aPath,False);
end;

class procedure TFile.Delete(const aPath: string);
begin
  SysUtils.DeleteFile(aPath);
end;

class function TFile.Exists(const aPath: string; FollowLink: Boolean): Boolean;
begin
  Result:=FileExists(aPath, FollowLink);
end;

class function TFile.GetAttributes(const aPath: string; FollowLink: Boolean
  ): TFileAttributes;
begin
  Result:=IntegerToFileAttributes(FileGetAttr(aPath{$ifdef unix},Followlink{$endif}));
end;

class function TFile.GetCreationTime(const aPath: string): TDateTime;

var
  Dummy1, Dummy2: TDateTime;
begin
  Result:=MinDateTime;
  Dummy1:=MinDateTime;
  Dummy2:=MinDateTime;
  GetFileTimestamps(aPath, Result, Dummy1, Dummy2, False);
end;

class function TFile.GetCreationTimeUtc(const aPath: string): TDateTime;
var
  Dummy1, Dummy2: TDateTime;
begin
  Result:=MinDateTime;
  Dummy1:=MinDateTime;
  Dummy2:=MinDateTime;
  GetFileTimestamps(aPath, Result, Dummy1, Dummy2, True);
end;

class function TFile.GetLastAccessTime(const aPath: string): TDateTime;
var
  Dummy1, Dummy2: TDateTime;
begin
  Result:=MinDateTime;
  Dummy1:=MinDateTime;
  Dummy2:=MinDateTime;
  GetFileTimestamps(aPath, Dummy1, Dummy2, Result, False);
end;

class function TFile.GetLastAccessTimeUtc(const aPath: string): TDateTime;
var
  Dummy1, Dummy2: TDateTime;
begin
  Result:=MinDateTime;
  Dummy1:=MinDateTime;
  Dummy2:=MinDateTime;
  GetFileTimestamps(aPath, Dummy1, Dummy2, Result,True);
end;

class function TFile.GetLastWriteTime(const aPath: string): TDateTime;
var
  Dummy1, Dummy2: TDateTime;
begin
  Result:=MinDateTime;
  Dummy1:=MinDateTime;
  Dummy2:=MinDateTime;
  GetFileTimestamps(aPath, Dummy1, Result, Dummy2, False);
end;

class function TFile.GetLastWriteTimeUtc(const aPath: string): TDateTime;
var
  Dummy1, Dummy2: TDateTime;
begin
  Result:=MinDateTime;
  Dummy1:=MinDateTime;
  Dummy2:=MinDateTime;
  GetFileTimestamps(aPath, Dummy1, Result, Dummy2,True);
end;

class function TFile.GetSymLinkTarget(const aFileName: string;
  var SymLinkRec: TSymLinkRec): Boolean;
begin
  Result:=FileGetSymLinkTarget(aFileName,SymLinkRec);
end;

class function TFile.GetSymLinkTarget(const aFileName: string;
  var TargetName: RawByteString): Boolean;
begin
  Result:=FileGetSymLinkTarget(aFileName,TargetName);
end;

class function TFile.GetSymLinkTarget(const aFileName: Unicodestring;
  var TargetName: UnicodeString): Boolean;
begin
  Result:=FileGetSymLinkTarget(aFileName,TargetName);
end;

class procedure TFile.Move(SourceFileName, DestFileName: string);
begin
  if FileExists(DestFileName) then
    raise EInOutError.CreateFmt(SerrFileExists, [DestFileName]);
  if RenameFile(SourceFileName, DestFileName) then
    Exit;
  Copy(SourceFileName, DestFileName);
  if FileExists(DestFileName) and FileExists(SourceFileName) then
    Delete(SourceFileName);
end;

class function TFile.Open(const aPath: string; const aMode: TFileMode
  ): TFileStream;
begin
  Result:=Open(aPath,aMode,faReadWrite)
end;

class function TFile.Open(const aPath: string; const aMode: TFileMode;
  const aAccess: TFileAccess): TFileStream;
begin
  Result:=Open(aPath, aMode, aAccess, TFileShare.fsNone);
end;

class function TFile.Open(const aPath: string; const aMode: TFileMode;
  const aAccess: TFileAccess; const aShare: TFileShare): TFileStream;

Const
 // faRead, faWrite, faReadWrite
   AccessModes : Array[TFileAccess] of Word = (SysUtils.fmOpenRead, SysUtils.fmOpenWrite,fmOpenReadWrite)  ;
   // fsNone, fsRead, fsWrite, fsReadWrite
   ShareModes : Array[TFileShare] of word = (fmShareExclusive, fmShareDenyRead, fmShareDenyWrite,fmShareDenyNone);

Var
  acMode,sMode,fMode : Word;

begin
  acMode:=AccessModes[aAccess];
  sMode:=ShareModes[aShare];
  fMode:=acMode or sMode;
  case aMode of
  TFileMode.fmCreateNew :
    begin
    if Exists(aPath) then
      Raise EInOutError.CreateFmt(SErrFileExists,[aPath]);
    Result:=TFileStream.Create(aPath,fMode);
    end;
  TFileMode.fmCreate:
    Result:=TFileStream.Create(aPath, Classes.fmCreate or sMode);
  TFileMode.fmOpen:
    begin
    if Exists(aPath) then
      Raise EInOutError.CreateFmt(SErrFileNotFound,[aPath]);
    Result:=TFileStream.Create(aPath,fMode);
    end;
  TFileMode.fmOpenOrCreate:
    begin
    if Exists(aPath) then
      Result:=TFileStream.Create(aPath,fMode)
    else
      Result:=TFileStream.Create(aPath,Classes.fmCreate or sMode);
    end;
  TFileMode.fmTruncate:
    begin
    if not Exists(aPath) then
      raise EInoutError.CreateFmt(SErrFileNotFound, [aPath]);
    Result:=TFileStream.Create(aPath,fMode);
    Result.Size:=0;
    end;
  TFileMode.fmAppend:
    begin
    if Exists(aPath) then
    begin
      Result:=TFileStream.Create(aPath, fMode);
      Result.Seek(0,soEnd);
    end
    else
      Result:=TFileStream.Create(aPath, Classes.fmCreate or sMode);
    end;
  end;
end;

class function TFile.OpenRead(const aPath: string): TFileStream;
begin
  Result:=TFileStream.Create(aPath, fmOpenRead or fmShareDenyWrite);
end;

class function TFile.OpenText(const aPath: string): TStreamReader;
begin

end;

class function TFile.OpenWrite(const aPath: string): TFileStream;
begin
  Result:=TFileStream.Create(aPath,fmOpenWrite);
end;

class function TFile.ReadAllBytes(const aPath: string): TBytes;

begin
  Result:=[];
  With OpenRead(aPath) do
    try
       SetLength(Result,Size);
       ReadBuffer(Result,0);
    finally
      Free;
    end;
end;

class function TFile.ReadAllLines(const aPath: string): TStringDynArray;

Var
  aBOMLength : Integer;

begin
  Result:=ReadAllLines(aPath,DetectFileEncoding(aPath,aBomLength));
end;

class function TFile.ReadAllLines(const aPath: string; const aEncoding: TEncoding
  ): TStringDynArray;
begin
  With TStringList.Create do
    try
      LoadFromFile(aPath,aEncoding);
      Result:=ToStringArray;
    finally
      Free;
    end;
end;

class function TFile.ReadAllText(const aPath: string): string;

var
  aBOMLength : Integer;

begin
  Result:=ReadAllText(aPath,DetectFIleEncoding(aPath,aBOMLength));
end;

class function TFile.ReadAllText(const aPath: string; const aEncoding: TEncoding
  ): string;

Var
  B : TBytes;

begin
  B:=ReadAllBytes(aPath);
{$if sizeof(char)=1}
  Result:=aEncoding.GetAnsiString(B);
{$else}
  Result:=aEncoding.GetString(B);
{$endif}
end;

{$IFDEF MSWINDOWS}

function ReplaceFileA(lpReplacedFileName, lpReplacementFileName, lpBackupFileName: LPCSTR; dwReplaceFlags: DWORD; lpExclude: LPVOID; lpReserved: LPVOID): BOOL; stdcall; external 'kernel32' name 'ReplaceFileA';
function ReplaceFileW(lpReplacedFileName, lpReplacementFileName, lpBackupFileName: LPCWSTR; dwReplaceFlags: DWORD; lpExclude: LPVOID; lpReserved: LPVOID): BOOL; stdcall; external 'kernel32' name 'ReplaceFileW';

class procedure TFile.Replace(const aSource, aDestination, aBackup: string; const aIgnoreMetadataErrors: Boolean); overload;

var
  lBackup,lDest,lSrc : String;
  ReplaceFlags : DWord;

begin
  lDest:=ExpandFileName(aDestination);
  lSrc:=ExpandFileName(aSource);
  lBackup:=ExpandFileName(aBackup);
  ReplaceFlags:=REPLACEFILE_WRITE_THROUGH;
  if aIgnoreMetadataErrors then
      ReplaceFlags:=ReplaceFlags or REPLACEFILE_IGNORE_MERGE_ERRORS;
  ReplaceFileA(PChar(lDest),PChar(lSrc),PChar(lBackup),ReplaceFlags,nil,nil);
end;
{$ENDIF MSWINDOWS}


class procedure TFile.Replace(const aSource, aDestination,
  aBackup: string);
var
  lBackup,lDest,lSrc : String;

begin
  lDest:=ExpandFileName(aDestination);
  lSrc:=ExpandFileName(aSource);
  lBackup:=ExpandFileName(aBackup);
  if CopyFile(lDest,lBackup,[],False) then
    if CopyFile(lSrc,lDest,[cffOverwriteFile],False) then
      Delete(lSrc);
end;

class procedure TFile.SetAttributes(const aPath: string;
  const aAttributes: TFileAttributes);
begin
{$ifdef unix}
  fpCHmod(aPath,FileAttributesToInteger(aAttributes));
{$else}
  SysUtils.FileSetAttr(aPath, FileAttributesToInteger(aAttributes));
{$endif}
end;

class procedure TFile.WriteAllBytes(const aPath: string; const aBytes: TBytes);
begin
  With Create(aPath) do
    try
      WriteBuffer(aBytes,Length(aBytes));
    finally
      Free
    end;
end;

class procedure TFile.WriteAllLines(const aPath: string;
  const aContents: TStringDynArray);
begin
  WriteAllLines(aPath,aContents,TEncoding.UTF8);
end;

class procedure TFile.WriteAllLines(const aPath: string;
  const aContents: TStringDynArray; const aEncoding: TEncoding);
var
  L : TStringList;
begin
  L:=TStringList.Create;
  try
    L.SetStrings(aContents);
    L.SaveToFile(aPath,aEncoding);
  finally
    L.Free;
  end;

end;

class procedure TFile.WriteAllText(const aPath, aContents: string);
begin
   WriteAllText(aPath,aContents,TEncoding.UTF8);
end;

class procedure TFile.WriteAllText(const aPath, aContents: string;
  const aEncoding: TEncoding);
begin
{$IF SIZEOF(CHAR)=1}
  WriteAllBytes(aPath,aEncoding.GetAnsiBytes(aContents));
{$ELSE}
  WriteAllBytes(aPath,aEncoding.GetBytes(aContents));
{$ENDIF}
end;

{ TDirectory }

class function TDirectory.GetFilesAndDirectories(const aPath,
  aSearchPattern: string; const aSearchOption: TSearchOption;
  const SearchAttributes: TFileAttributes;
  const aPredicate: TFilterPredicateLocal): TStringDynArray;

  function FilterPredicate(const aPath: string; const SearchRec: TSearchRec): Boolean;
  begin
    Result:=(SearchRec.Name <> '.') and (SearchRec.Name <> '..');
    if Result and Assigned(aPredicate) then
      Result:=aPredicate(aPath, SearchRec);
  end;
var
  SearchRec: TSearchRec;
  IntPath:   TFileName;
begin
  IntPath     :=IncludeTrailingPathDelimiter(aPath);
  Result      :=[];
  if (FindFirst(IntPath + aSearchPattern, TFile.FileAttributesToInteger(SearchAttributes), SearchRec) = 0) then
    repeat
      if (aSearchOption = soAllDirectories) and ((SearchRec.Attr and SysUtils.faDirectory) <> 0) then
        Result:=Result + GetFilesAndDirectories(IntPath + SearchRec.Name, aSearchPattern, aSearchOption, SearchAttributes, aPredicate)
      else if FilterPredicate(aPath, SearchRec) then
        Result:=Result + [IntPath + SearchRec.Name];
    until FindNext(SearchRec) <> 0;
  SysUtils.FindClose(SearchRec);
end;

class function TDirectory.GetFilesAndDirectories(const aPath,
  aSearchPattern: string; const aSearchOption: TSearchOption;
  const SearchAttributes: TFileAttributes;
  const aPredicate: TFilterPredicateObject): TStringDynArray;

  function DoFilterPredicate(const aPath: string; const SearchRec: TSearchRec): Boolean;
  begin
    Result:=aPredicate(aPath, SearchRec);
  end;

begin
  Result:=GetFilesAndDirectories(aPath, aSearchPattern, aSearchOption, SearchAttributes,@DoFilterPredicate);
end;

class function TDirectory.GetFilesAndDirectories(const aPath,
  aSearchPattern: string; const aSearchOption: TSearchOption;
const SearchAttributes: TFileAttributes; const aPredicate: TFilterPredicate
  ): TStringDynArray;
  function DoFilterPredicate(const aPath: string; const SearchRec: TSearchRec): Boolean;
  begin
    Result:=aPredicate(aPath, SearchRec);
  end;
begin
  Result:=GetFilesAndDirectories(aPath, aSearchPattern, aSearchOption, SearchAttributes, @DoFilterPredicate);
end;

class procedure TDirectory.Copy(const SourceDirName, DestDirName: string);
begin
  CopyFile(SourceDirName, DestDirName,[],True);
end;

class procedure TDirectory.CreateDirectory(const aPath: string);
begin
  ForceDirectories(aPath);
end;

function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
const
  //Don't follow symlinks on *nix, just delete them
  DeleteMask = faAnyFile {$ifdef unix} or sysutils.faSymLink{%H-} {$endif unix};
var
  FileInfo: TSearchRec;
  CurSrcDir: String;
  CurFilename: String;
begin
  Result:=false;
  CurSrcDir:=ExpandFileName(DirectoryName);
  if FindFirst(CurSrcDir+AllFilesMask,DeleteMask,FileInfo)=0 then
    Try
      repeat
        // check if special file
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
          continue;
        CurFilename:=CurSrcDir+FileInfo.Name;
        if ((FileInfo.Attr and sysutils.faDirectory)>0)
           {$ifdef unix} and ((FileInfo.Attr and sysutils.faSymLink{%H-})=0) {$endif unix} then begin
          if not DeleteDirectory(CurFilename,false) then exit;
        end else begin
          if not Sysutils.DeleteFile(CurFilename) then exit;
        end;
      until Sysutils.FindNext(FileInfo)<>0;
    finally
      Sysutils.FindClose(FileInfo);
    end;
  if (not OnlyChildren) and (not RemoveDir(CurSrcDir)) then exit;
  Result:=true;
end;


class procedure TDirectory.Delete(const aPath: string);
begin
  RemoveDir(aPath);
end;



class procedure TDirectory.Delete(const aPath: string; const Recursive: Boolean);
begin
  if Recursive then
    DeleteDirectory(aPath, False)
  else
    TDirectory.Delete(aPath);
end;

class function TDirectory.Exists(const aPath: string; FollowLink: Boolean
  ): Boolean;
begin
  Result:=DirectoryExists(aPath, FollowLink);
end;

class function TDirectory.GetAttributes(const aPath: string; FollowLink: Boolean
  ): TFileAttributes;
begin
  Result:=TFile.GetAttributes(aPath, FollowLink);
end;

class function TDirectory.GetCurrentDirectory: string;
begin
  Result:=GetCurrentDir;
end;

class procedure TDirectory.SetCurrentDirectory(const aPath: string);
begin
  ChDir(aPath);
end;

class function TDirectory.GetLogicalDrives: TStringDynArray;
{$IfDef WINDOWS}
var
  i: Char;
{$EndIf}
begin
  Result:=[];
  {$IfDef WINDOWS}
    for i:='A' to 'Z' do
      if DirectoryExists(i + ':\') then
        Result:=Result + [i + ':\'];
  {$EndIf}
end;

class procedure TDirectory.SetAttributes(const aPath: string;
  const Attributes: TFileAttributes);
begin
  TFile.SetAttributes(aPath, Attributes);
end;

class function TDirectory.GetParent(const aPath: string): string;
begin
  Result:=ExpandFileName(IncludeTrailingPathDelimiter(aPath) + '..');
end;

class function TDirectory.GetDirectories(const aPath: string): TStringDynArray;
begin
  Result:=GetDirectories(aPath, '*');
end;

class function TDirectory.GetDirectories(const aPath: string;
  const aPredicate: TFilterPredicateLocal): TStringDynArray;
begin
  Result:=GetDirectories(aPath, '*', aPredicate);
end;

class function TDirectory.GetDirectories(const aPath: string;
  const aPredicate: TFilterPredicateObject): TStringDynArray;
begin
  Result:=GetDirectories(aPath, '*', aPredicate);
end;

class function TDirectory.GetDirectories(const aPath: string;
  const aPredicate: TFilterPredicate): TStringDynArray;
begin
  Result:=GetDirectories(aPath, '*', aPredicate);
end;

class function TDirectory.GetDirectories(const aPath, aSearchPattern: string
  ): TStringDynArray;
begin
  Result:=GetDirectories(aPath, aSearchPattern, TFilterPredicateLocal(nil));
end;

class function TDirectory.GetDirectories(const aPath, aSearchPattern: string;
  const aPredicate: TFilterPredicateLocal): TStringDynArray;
begin
  Result:=GetDirectories(aPath, aSearchPattern, soTopDirectoryOnly, aPredicate);
end;

class function TDirectory.GetDirectories(const aPath, aSearchPattern: string;
  const aPredicate: TFilterPredicateObject): TStringDynArray;
begin
  Result:=GetDirectories(aPath, aSearchPattern, soTopDirectoryOnly, aPredicate);
end;

class function TDirectory.GetDirectories(const aPath, aSearchPattern: string;
  const aPredicate: TFilterPredicate): TStringDynArray;
begin
  Result:=GetDirectories(aPath, aSearchPattern, soTopDirectoryOnly, aPredicate);
end;

class function TDirectory.GetDirectories(const aPath, aSearchPattern: string;
  const aSearchOption: TSearchOption): TStringDynArray;
begin
  Result:=GetDirectories(aPath, aSearchPattern, aSearchOption, TFilterPredicateLocal(nil));
end;

class function TDirectory.GetDirectories(const aPath, aSearchPattern: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateLocal
  ): TStringDynArray;
begin
  Result:=GetFilesAndDirectories(aPath, aSearchPattern, aSearchOption, [faDirectory], aPredicate);
end;

class function TDirectory.GetDirectories(const aPath, aSearchPattern: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateObject
  ): TStringDynArray;
begin
  Result:=GetFilesAndDirectories(aPath, aSearchPattern, aSearchOption, [faDirectory], aPredicate);
end;

class function TDirectory.GetDirectories(const aPath, aSearchPattern: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicate
  ): TStringDynArray;
begin
  Result:=GetFilesAndDirectories(aPath, aSearchPattern, aSearchOption, [faDirectory], aPredicate);
end;

class function TDirectory.GetDirectories(const aPath: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateLocal
  ): TStringDynArray;
begin
  Result:=GetDirectories(aPath, '*', aSearchOption, aPredicate);
end;

class function TDirectory.GetDirectories(const aPath: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateObject
  ): TStringDynArray;
begin
  Result:=GetDirectories(aPath, '*', aSearchOption, aPredicate);
end;

class function TDirectory.GetDirectories(const aPath: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicate
  ): TStringDynArray;
begin
  Result:=GetDirectories(aPath, '*', aSearchOption, aPredicate);
end;

class function TDirectory.GetFiles(const aPath: string): TStringDynArray;
begin
  Result:=GetFiles(aPath, '*');
end;

class function TDirectory.GetFiles(const aPath: string;
  const aPredicate: TFilterPredicateLocal): TStringDynArray;
begin
  Result:=GetFiles(aPath, '*', soTopDirectoryOnly, aPredicate);
end;

class function TDirectory.GetFiles(const aPath: string;
  const aPredicate: TFilterPredicateObject): TStringDynArray;
begin
  Result:=GetFiles(aPath, '*', soTopDirectoryOnly, aPredicate);
end;

class function TDirectory.GetFiles(const aPath: string;
  const aPredicate: TFilterPredicate): TStringDynArray;
begin
  Result:=GetFiles(aPath, '*', soTopDirectoryOnly, aPredicate);
end;

class function TDirectory.GetFiles(const aPath, aSearchPattern: string
  ): TStringDynArray;
begin
  Result:=GetFiles(aPath, aSearchPattern, soTopDirectoryOnly, TFilterPredicateLocal(nil));
end;

class function TDirectory.GetFiles(const aPath, aSearchPattern: string;
  const aPredicate: TFilterPredicateLocal): TStringDynArray;
begin
  Result:=GetFiles(aPath, aSearchPattern, soTopDirectoryOnly, aPredicate);
end;

class function TDirectory.GetFiles(const aPath, aSearchPattern: string;
  const aPredicate: TFilterPredicateObject): TStringDynArray;
begin
  Result:=GetFiles(aPath, aSearchPattern, soTopDirectoryOnly, aPredicate);
end;

class function TDirectory.GetFiles(const aPath, aSearchPattern: string;
  const aPredicate: TFilterPredicate): TStringDynArray;
begin
  Result:=GetFiles(aPath, aSearchPattern, soTopDirectoryOnly, aPredicate);
end;

class function TDirectory.GetFiles(const aPath, aSearchPattern: string;
  const aSearchOption: TSearchOption): TStringDynArray;
begin
  Result:=GetFiles(aPath, aSearchPattern, aSearchOption, TFilterPredicateLocal(nil));
end;

class function TDirectory.GetFiles(const aPath, aSearchPattern: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateLocal
  ): TStringDynArray;
begin
  Result:=GetFilesAndDirectories(aPath, aSearchPattern, aSearchOption,
                                   TFile.IntegerToFileAttributes(faAnyFile) - [faDirectory],
                                   aPredicate);
end;

class function TDirectory.GetFiles(const aPath, aSearchPattern: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateObject
  ): TStringDynArray;
begin
  Result:=GetFiles(aPath, aSearchPattern, aSearchOption, aPredicate);
end;

class function TDirectory.GetFiles(const aPath, aSearchPattern: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicate
  ): TStringDynArray;
begin
  Result:=GetFiles(aPath, aSearchPattern, aSearchOption, aPredicate);
end;

class function TDirectory.GetFiles(const aPath: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateLocal
  ): TStringDynArray;
begin
  Result:=GetFiles(aPath, '*', aSearchOption, aPredicate);
end;

class function TDirectory.GetFiles(const aPath: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateObject
  ): TStringDynArray;
begin
  Result:=GetFiles(aPath, '*', aSearchOption, aPredicate);
end;

class function TDirectory.GetFiles(const aPath: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicate
  ): TStringDynArray;
begin
  Result:=GetFiles(aPath, '*', aSearchOption, aPredicate);
end;

class function TDirectory.GetFileSystemEntries(const aPath: string
  ): TStringDynArray;
begin
  Result:=GetFileSystemEntries(aPath, '*');
end;

class function TDirectory.GetFileSystemEntries(const aPath: string;
  const aPredicate: TFilterPredicateLocal): TStringDynArray;
begin
  Result:=GetFileSystemEntries(aPath, '*', aPredicate);
end;

class function TDirectory.GetFileSystemEntries(const aPath: string;
  const aPredicate: TFilterPredicateObject): TStringDynArray;
begin
  Result:=GetFileSystemEntries(aPath, '*', aPredicate);
end;

class function TDirectory.GetFileSystemEntries(const aPath: string;
  const aPredicate: TFilterPredicate): TStringDynArray;
begin
  Result:=GetFileSystemEntries(aPath, '*', aPredicate);
end;

class function TDirectory.GetFileSystemEntries(const aPath, aSearchPattern: string
  ): TStringDynArray;
begin
  Result:=GetFileSystemEntries(aPath, aSearchPattern, TFilterPredicateLocal(nil));
end;

class function TDirectory.GetFileSystemEntries(const aPath,
  aSearchPattern: string; const aPredicate: TFilterPredicateLocal
  ): TStringDynArray;
begin
  Result:=GetFilesAndDirectories(aPath, aSearchPattern, soTopDirectoryOnly, TFile.IntegerToFileAttributes(faAnyFile), aPredicate);

end;

class function TDirectory.GetFileSystemEntries(const aPath,
  aSearchPattern: string; const aPredicate: TFilterPredicateObject
  ): TStringDynArray;
begin
  Result:=GetFilesAndDirectories(aPath, aSearchPattern, soTopDirectoryOnly, TFile.IntegerToFileAttributes(faAnyFile), aPredicate);
end;

class function TDirectory.GetFileSystemEntries(const aPath,
  aSearchPattern: string; const aPredicate: TFilterPredicate): TStringDynArray;
begin
  Result:=GetFilesAndDirectories(aPath, aSearchPattern,  soTopDirectoryOnly, TFile.IntegerToFileAttributes(faAnyFile), aPredicate);
end;

class function TDirectory.GetFileSystemEntries(const aPath: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateLocal
  ): TStringDynArray;
begin
  Result:=GetFilesAndDirectories(aPath, '*', aSearchOption, TFile.IntegerToFileAttributes(faAnyFile),aPredicate);
end;

class function TDirectory.GetFileSystemEntries(const aPath: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicateObject
  ): TStringDynArray;
begin
  Result:=GetFilesAndDirectories(aPath, '*', aSearchOption, TFile.IntegerToFileAttributes(faAnyFile),aPredicate);
end;

class function TDirectory.GetFileSystemEntries(const aPath: string;
  const aSearchOption: TSearchOption; const aPredicate: TFilterPredicate
  ): TStringDynArray;
begin
  Result:=GetFilesAndDirectories(aPath, '*', aSearchOption, TFile.IntegerToFileAttributes(faAnyFile),aPredicate);
end;

class procedure TDirectory.ForAllEntries(const aPath, aPattern: string; const aBefore, aAfter: TFilterPredicateLocal; aRecursive: Boolean);
var
  Handle: Boolean;
  Continue: Boolean;
  Info: TSearchRec;
  lPath : String;
begin
  Handle:=True;
  lPath:=IncludeTrailingPathDelimiter(aPath);
  if FindFirst(lPath+AllFilesMask,faAnyFile,Info)<>0 then
    exit;
  try
    repeat
      Handle:=TPath.MatchesPattern(Info.Name,aPattern,System.FileNameCaseSensitive);
      if Handle and Assigned(aBefore) then
        Continue:=aBefore(aPath,Info);
      if Continue then
        begin
        if aRecursive and Info.IsDirectory and not Info.IsCurrentOrParentDir then
           ForAllEntries(lPath+Info.Name,aPattern,aBefore,aAfter,aRecursive);
        if Handle and Assigned(aAfter) then
          Continue:=aAfter(lPath,Info);
        end;
    until (SysUtils.FindNext(Info)<>0) or not Continue;
  finally
    SysUtils.FindClose(Info);
  end;
end;

class function TDirectory.IsEmpty(const aPath: string): Boolean;
var
  sr: TSearchRec;
begin
  Result:=True;
  if (FindFirst(aPath, faAnyFile, sr) = 0) then
    repeat
      Result:=(sr.Name = '.') or (sr.Name = '..');
    until Result and (FindNext(sr) = 0);
  SysUtils.FindClose(sr);
end;

class function TDirectory.IsRelativePath(const aPath: string): Boolean;
begin
  Result:=TPath.IsRelativePath(aPath);
end;

Function SpecialDir(Const Info : TSearchRec) : Boolean;

begin
  Result:=(Info.Attr and SysUtils.faDirectory <> 0) and (Info.Name='.') or (Info.Name='..');
end;

class procedure TDirectory.Move(const SourceDirName, DestDirName: string);

Var
  lSource,lDest : String;

  function DoCreateDestDir(const aPath: string; const aInfo: TSearchRec): Boolean;
  var
    lPath: string;
  begin
    Result:=True;
    if (Not aInfo.IsDirectory) or aInfo.IsCurrentOrParentDir then
      exit;
    lPath:=lDest;
    if SameFileName(aPath,SourceDirName) then
      lPath:=IncludeTrailingPathDelimiter(lPath+ExtractRelativePath(lSource,aPath));
    lPath:=lPath+aInfo.Name;
    CreateDir(lPath);
  end;

  function DoMoveSrcToDest(const aPath: string; const aInfo: TSearchRec): Boolean;
  var
    lSrc,lDestF: string;
  begin
    Result:=True;
    if aInfo.IsCurrentOrParentDir then
      exit;
    if aInfo.IsDirectory then
      begin
        lSrc:=TPath.Combine(aPath,aInfo.Name);
{$IFDEF WINDOWS}
        FileSetAttr(lSrc,SysUtils.faNormal);
{$ENDIF}
        RemoveDir(lSrc);
      end
    else
      begin
        lSrc:=TPath.Combine(aPath, aInfo.Name);
        lDestF:=lDest;
        if SameFileName(aPath,SourceDirName) then
          lDestF:=IncludeTrailingPathDelimiter(lDestF+ExtractRelativePath(lSource,aPath));
        lDestF:=lDest+aInfo.Name;
{$IFDEF WINDOWS}
        FileSetAttr(lSrc,SysUtils.faNormal);
{$ENDIF WINDOWS}
        RenameFile(lSrc,lDestF);
{$IFDEF WINDOWS}
        FileSetAttr(lDestf,aInfo.Attr);
{$ENDIF WINDOWS}
      end;
  end;


begin
  lSource:=IncludeTrailingPathDelimiter(SourceDirName);
  lDest:=IncludeTrailingPathDelimiter(DestDirName);
  ForceDirectories(DestDirName);
  ForAllEntries(lSource,allFilesMask,@DoCreateDestDir,@DoMoveSrcToDest,True);
  RemoveDir(SourceDirName);
end;

end.

