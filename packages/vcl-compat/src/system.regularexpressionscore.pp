{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2023 The Free Pascal team

    Delphi-compatible Regular expressions unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  Note that the original Delphi unit (by Jan Goyvaerts) uses PCRE1,
  but this unit uses PCRE2. The string type depends on how the packages were
  compiled.
}

unit System.RegularExpressionsCore;

{$MODE OBJFPC}
{$H+}

interface

{.$DEFINE USEWIDESTRING} // uncomment if you want to force widestring...

// We cannot detect the char size before the uses clause is parsed, it will return 1, the compiler default.
// So we need a define here, maybe a compiler switch is needed to set the default size (-Sw ?) which would allow to set the default type.
// The detection here is based on the assumption that the dotted units use widestring...
{$IFDEF FPC_DOTTEDUNITS}
{$DEFINE USEWIDESTRING}
{$ENDIF}


uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Contnrs, {$IFNDEF USEWIDESTRING} Api.PCRE2_8 {$ELSE} Api.PCRE2_16 {$ENDIF}, System.CTypes, System.RegularExpressionsConsts;
{$ELSE}
  SysUtils, Classes, Contnrs, {$IFNDEF USEWIDESTRING} libpcre2_8 {$ELSE} libpcre2_16 {$ENDIF}, ctypes, System.RegularExpressionsConsts;
{$ENDIF}

const
  MAX_SUBEXPRESSIONS = 99;

type
  {$IFDEF USEWIDESTRING}
  TREString = UnicodeString;
  {$ElSE}
  TREString = AnsiString;
  {$ENDIF}
  TREStringDynArray = Array of TREString;

  TPerlRegExOption = (preCaseLess,preMultiLine,preSingleLine,preExtended,preAnchored,preUnGreedy,preNoAutoCapture,
                      preAllowEmptyClass, preAltBSUX, preAltCircumFlex, preAltVerbNames,
                      preDollarEndOnly, preDupNames, preEndAnchored, preFirstLine, preLiteral, preMatchInvalidUTF,
                      preMatchUnsetBackRef, preNeverBackslashC, preNoAutoPossess, preNoDotStarAnchor, preNoStartOptimize,
                      preNoUTFCheck, preUseOffsetLimit);

  TPerlRegExOptions = set of TPerlRegExOption;

  TPerlRegExStateItem = (preNotBOL,preNotEOL,preNotEmpty);
  TPerlRegExState = set of TPerlRegExStateItem;

  TPerlRegExReplaceEvent = procedure(Sender: TObject; var ReplaceWith: TREString) of object;

  { TPerlRegEx }

  TPerlRegEx = class
  Private
    Type
      TTransformation = (tNone,tLowerCase,tUpperCase,tFirstCap,tInitialCap);
      TMatchResult = (mrFound,mrNotFound,mrAfterStop);
    class function TransForm(aTransform: TTransformation; const S: TREString): TREString;
  private
  {$IFDEF USEWIDESTRING}
    FCode : Ppcre2_code_16;
  {$ELSE}
    FCode : Ppcre2_code_8;
  {$ENDIF}
    FOnMatch: TNotifyEvent;
    FOnReplace: TPerlRegExReplaceEvent;
    FOptions: TPerlRegExOptions;
    FRegEx: TREString;
    FState: TPerlRegExState;
    FStart,
    FStop: Integer;
    FStudied: Boolean;
    FResultVector : Psize_t;
    FResultCount : Cardinal;
    FMatchData : ppcre2_match_data;
    FModifiedSubject,
    FSubject: TREString;
    FSubjectLength : cuint32;
    FNameCount : cuint32;
    FNameTable : PCRE2_SPTR;
    FNameEntrySize : cuint32;
    FLastModifiedEnd: SizeInt;
    FReplacement : TREString;
    FStoredGroups: array of TREString;
    FCrLFIsNewLine,
    FIsUtf : Boolean;
    Procedure CheckMatch; inline;
    function DoMatch(Opts: CUInt32): TMatchResult;
    function GetBackRefIndex(const Ref: TREString; var I: Integer): Integer;
    function GetCompiled: Boolean;
    function GetFoundMatch: Boolean; inline;
    function GetGroupCount: Integer;
    function GetGroupLengths(aIndex: Integer): Integer;
    function GetGroupOffsets(aIndex: Integer): Integer;
    function GetGroups(aIndex: Integer): TREString;
    function GetMatchedLength: Integer;
    function GetMatchedOffset: Integer;
    function GetMatchedText: TREString;
    function GetModifiedSubject: TREString;
    function GetNamedGroup(const aName : TREString): TREString;
    procedure GetNamedGroupInfo;
    function GetNames(aIndex : Integer): TREString;
    function GetPCREErrorMsg(ErrorNr: Integer): TREString;
    function GetResultString(aIndex: Integer): TREString;
    function GetStart: Integer;
    function GetStop: Integer;
    function GetSubject: TREString;
    function GetSubjectLeft: TREString;
    function GetSubjectRight: TREString;
    function MakeOptions(aOptions: TPerlRegExOptions): Integer;
    procedure SetOptions(aValue: TPerlRegExOptions);
    procedure SetRegEx(const aValue: TREString);
    procedure SetReplacement(const aValue: TREString);
    procedure SetStart(aValue: Integer);
    procedure SetStop(aValue: Integer);
    procedure SetSubject(const aValue: TREString);
  protected
    procedure FreeCodeData;
    procedure FreeMatchData;
    procedure CleanUp; virtual;
    procedure ClearStoredGroups;
    function FirstOffset : Cardinal;
    function FirstLength : Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    // Use this to escape special characters.
    class function EscapeRegExChars(const aString: TREString): TREString;
    // Compile the regex.
    procedure Compile;
    // Study regex (may result in faster execution);
    procedure Study;
    // Try to match, starting at beginning. Returns true if a match was found.
    function Match: Boolean;
    // Try to match again, starting previous match end. Returns true if a new match was found.
    function MatchAgain: Boolean;
    // Replace current match in Subject with ComputeReplacement. Returns computed replacement
    function Replace: TREString;
    // Replace all matches in Subject with their ComputeReplacement. Returns true if a match was found.
    function ReplaceAll: Boolean;
    // Compute replacement text.
    function ComputeReplacement: TREString;
    // Store groups for faster access.
    procedure StoreGroups;
    // Index in groups of name.
    function NamedGroup(const aName: TREString): Integer;
    // Split subject TREString based on regex. aStrings will contain everything outside the matches.
    procedure Split(const aStrings: TStrings; aLimit: Integer = 0);
    // Split subject TREString based on regex. Result will contain everything outside the matches.
    function Split(aLimit: Integer = 0) : TREStringDynArray;
    // Split subject TREString based on regex, but include matches in result.
    procedure SplitCapture(const aStrings: TStrings; aLimit: Integer); overload;
    // Split subject TREString based on regex, but include matches in result.
    // if aoffset is > 1 then everything till offset is put in the first TREString.
    procedure SplitCapture(const aStrings: TStrings; aLimit: Integer; aOffset : Integer); overload;
    // Same with result in array
    function SplitCapture(aLimit: Integer; aOffset : Integer) : TREStringDynArray; overload;
    // Was the regex compiled ?
    property Compiled: Boolean read GetCompiled;
    // Match found ?
    property FoundMatch: Boolean read GetFoundMatch;
    // Did study ?
    property Studied: Boolean read FStudied;
    // Fast access, group 0.
    property MatchedText: TREString read GetMatchedText;
    property MatchedLength: Integer read GetMatchedLength;
    property MatchedOffset: Integer read GetMatchedOffset;
    // Minimum search position, 1-based.
    property Start: Integer read GetStart write SetStart;
    // Maximum search position, 1-based.
    property Stop: Integer read GetStop write SetStop;
    property State: TPerlRegExState read FState write FState;
    // Group count.
    property GroupCount: Integer read GetGroupCount;
    // Group Texts. Index 0 - GroupCount. 0 is whole matched text. on original search text.
    property Groups[aIndex: Integer]: TREString read GetGroups;
    // Group lengths & Offsets. Index 0 - GroupCount. 0 is whole matched text, on original search text.
    property GroupLengths[aIndex: Integer]: Integer read GetGroupLengths;
    property GroupOffsets[aIndex: Integer]: Integer read GetGroupOffsets;
    // Named access to groups.
    property NamedGroups[aName : TREString] : TREString Read GetNamedGroup;
    // Names available in current match.
    Property NameCount : Cardinal Read FNameCount;
    Property Names[aIndex : Integer] : TREString Read GetNames;
    // Subject TREString. Will be modified by replace !
    property Subject: TREString read GetModifiedSubject write SetSubject;
    // Original subject TREString. Not modified by replace !
    property OriginalSubject: TREString read FSubject write SetSubject;
    // Left of original subject.
    property SubjectLeft: TREString read GetSubjectLeft;
    // Right of original subject.
    property SubjectRight: TREString read GetSubjectRight;
  public
    // Set options.
    property Options: TPerlRegExOptions read FOptions write SetOptions;
    // The regular expression
    property RegEx: TREString read FRegEx write SetRegEx;
    // The replacement expression.
    property Replacement: TREString read FReplacement write SetReplacement;
    // Called on every match.
    property OnMatch: TNotifyEvent read FOnMatch write FOnMatch;
    // Set this to modify the computed replacement text.
    property OnReplace: TPerlRegExReplaceEvent read FOnReplace write FOnReplace;
  end;

                                                                             
  TRegExStudyOption = (preJIT, preJITPartialHard, preJITPartialSoft);
  TRegExStudyOptions = set of TRegExStudyOption;

  { TPerlRegExList }

  TPerlRegExList = class
  private
    FMatch: TPerlRegEx;
    FList : TFPObjectList;
    FStart : Integer;
    FStop : Integer;
    FSubject : TREString;
    function GetCount: Integer;
    function GetOwnsRegex: Boolean;
    function GetRegEx(aIndex: Integer): TPerlRegEx;
    function GetStart: Integer;
    function GetStop: Integer;
    function GetSubject: TREString;
    procedure SetRegEx(aIndex: Integer; aValue: TPerlRegEx);
    procedure SetStart(aValue: Integer);
    procedure SetStop(aValue: Integer);
    procedure SetSubject(aValue: TREString);
  protected
    procedure UpdateRegEx(const aRegEx: TPerlRegEx);
  public
    constructor Create(OwnsRegex : Boolean);
    destructor Destroy; override;
  public
    function Add(const aRegEx: TPerlRegEx): Integer;
    procedure Clear;
    procedure Delete(aIndex: Integer);
    function IndexOf(const aRegEx: TPerlRegEx): Integer;
    procedure Insert(aIndex: Integer; const aRegEx: TPerlRegEx);
  public
    function Match: Boolean;
    function MatchAgain: Boolean;
    property RegEx[aIndex: Integer]: TPerlRegEx read GetRegEx write SetRegEx;
    property Count: Integer read GetCount;
    property Subject: TREString read GetSubject write SetSubject;
    property Start: Integer read GetStart write SetStart;
    property Stop: Integer read GetStop write SetStop;
    property MatchedRegEx: TPerlRegEx read FMatch;
    Property OwnsRegex : Boolean Read GetOwnsRegex;
  end;

  ERegularExpressionError = class(Exception);

// Todo: move to strutils ?
Function InitialCaps(const S : TREString) : TREString;

implementation

{$IFNDEF USEWIDESTRING}
function GetStrLen(p : PAnsiChar; len : Integer) : AnsiString;

var
  L : Integer;

begin
  Result:='';
  L:=StrLen(P);
  if L>Len then
    L:=Len;
  SetLength(Result,L);
  if L>0 then
    Move(P^,Result[1],L);
end;
{$ELSE}
function GetStrLen(p : PWideChar; len : Integer) : UnicodeString;

var
  L : Integer;

begin
  Result:='';
  L:=StrLen(P);
  if L>Len then
    L:=Len;
  SetLength(Result,L);
  if Len>0 then
    Move(P^,Result[1],L*2);
end;
{$ENDIF}

Function InitialCaps(const S : TREString) : TREString;

const
  NonWord = [#0..'&', '(', '*', '+', ',', '-', '.', '?', '<', '[', '{', #$B7];

var
  L : TREString;
  Len,Last,I : Integer;
  Upper : Boolean;

begin
  L:=LowerCase(S);
  Len:=Length(L);
  Last:=1;
  I:=1;
  Upper:=True;
  Result:='';
  While I<=Len do
    begin
    if L[i] in NonWord then
      Upper:=True
    else if Upper then
      begin
      if I>Last then
        Result:=Result+Copy(L,Last,I-Last);
      Result:=Result+UpperCase(L[i]);
      inc(I);
      Last:=I;
      Upper:=False;
      end;
    Inc(i);
    end;
  Result:=Result+Copy(L,Last,I-Last);
end;


{ TPerlRegEx }


function TPerlRegEx.GetFoundMatch: Boolean;
begin
  Result:=FResultCount>0;
end;

function TPerlRegEx.GetCompiled: Boolean;
begin
  Result:=Assigned(FCode);
end;

procedure TPerlRegEx.CheckMatch;
begin
  if not FoundMatch then
    raise ERegularExpressionError.Create(SRegExMatchRequired);
end;

function TPerlRegEx.GetGroupCount: Integer;
begin
  CheckMatch;
  Result:=FResultCount-1;
end;

function TPerlRegEx.GetGroupLengths(aIndex: Integer): Integer;

begin
  CheckMatch;
  Result:=FResultVector[2*aIndex+1]-FResultVector[2*aIndex];
end;

function TPerlRegEx.GetGroupOffsets(aIndex: Integer): Integer;
begin
  CheckMatch;
  Result:=FResultVector[2*aIndex]+1;
end;

function TPerlRegEx.GetResultString(aIndex: Integer): TREString;

var
  astart,aLength : Ptrint;

begin
  // Writeln('AIndex ',aIndex,' ',FResultCount);
  aStart:=FResultVector[2*aIndex];
  aLength:=FResultVector[2*aIndex+1]-aStart;
  inc(aStart); // 1-based
  Result:=Copy(FSubject,AStart,aLength);
end;

function TPerlRegEx.GetGroups(aIndex: Integer): TREString;

begin
  CheckMatch;
  if Length(FStoredGroups)>0 then
    Result:=FStoredGroups[aIndex]
  else
    Result:=GetResultString(aIndex);
end;

function TPerlRegEx.GetMatchedLength: Integer;

begin
  Result:=GetGroupLengths(0)
end;

function TPerlRegEx.GetMatchedOffset: Integer;
begin
  Result:=GetGroupOffsets(0);
end;

function TPerlRegEx.GetMatchedText: TREString;
begin
  Result:=GetResultString(0)
end;

function TPerlRegEx.GetModifiedSubject: TREString;
begin
  Result:=FModifiedSubject;
end;


function TPerlRegEx.GetNamedGroup(const aName: TREString): TREString;

var
  Idx : integer;

begin
  Result:='';
  Idx:=NamedGroup(aName);
  if Idx<>-1 then
    Result:=Groups[Idx];
end;

function TPerlRegEx.GetStart: Integer;
begin
  Result:=FStart+1;
end;

function TPerlRegEx.GetStop: Integer;
begin
  Result:=FStop+1;
end;

function TPerlRegEx.GetSubject: TREString;
begin
  Result:=FSubject;
end;

function TPerlRegEx.GetSubjectLeft: TREString;
begin
  // Resultvector is 0 based
  Result:=Copy(FSubject,1,FResultVector[0]);
end;

function TPerlRegEx.GetSubjectRight: TREString;
var
  SPos : Integer;
begin
  SPos:=FResultVector[1]; // 0-based
  Result:=Copy(FSubject,SPos+1,FSubjectLength-SPos);
end;

procedure TPerlRegEx.SetOptions(aValue: TPerlRegExOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
  CleanUp; // Need to reset...
end;

procedure TPerlRegEx.SetRegEx(const aValue: TREString);
begin
  if FRegEx=AValue then Exit;
  FRegEx:=aValue;
end;

procedure TPerlRegEx.SetReplacement(const aValue: TREString);
begin
  FReplacement:=AValue;
end;

procedure TPerlRegEx.SetStart(aValue: Integer);
begin
  FStart:=aValue-1;
end;

procedure TPerlRegEx.SetStop(aValue: Integer);
begin
  if FStop=aValue-1 then Exit;
  FStop:=aValue-1;
end;

procedure TPerlRegEx.SetSubject(const aValue: TREString);
begin
  FSubject:=aValue;
  FSubjectLength:=Length(FSubject);
  FModifiedSubject:=aValue;
  CleanUp;
  FStart:=0;
  FStop:=Length(FSubject);
end;

procedure TPerlRegEx.CleanUp;
begin
  FreeMatchData;
  FreeCodeData;
  ClearStoredGroups;
  FResultCount:=0;
  FResultVector:=Nil;
  FLastModifiedEnd:=0;
end;

procedure TPerlRegEx.ClearStoredGroups;
begin
  SetLength(FStoredGroups,0);
end;

function TPerlRegEx.FirstOffset: Cardinal;
begin
  Result:=FResultVector[0];
end;

function TPerlRegEx.FirstLength: Cardinal;
begin
  Result:=FResultVector[1]-FResultVector[0];
end;

constructor TPerlRegEx.Create;
begin
  if not libpcre28loaded then
    Loadlibpcre28;
end;

destructor TPerlRegEx.Destroy;
begin
  inherited Destroy;
end;

class function TPerlRegEx.EscapeRegExChars(const aString: TREString): TREString;

Const
  NeedEscape = ['\','[',']','^','$','.','|','?','*','+','-','(',')','{','}','&','<','>'];

var
  I : Integer;
  PSrc,PDest,PStart : PChar;

begin
  Result:='';
  SetLength(Result,2*Length(aString));
  PSrc:=PChar(aString);
  PDest:=PChar(Result);
  PStart:=PDest;
  for I:=1 to Length(aString) do
    begin
    if PSrc^=#0 then
      begin
      PDest^:='\';
      Inc(PDest);
      PDest^:='0';
      end
    else if CharInSet(PSrc^,NeedEscape) then
      begin
      PDest^:='\';
      Inc(PDest);
      PDest^:=PSrc^;
      end
    else
      PDest^:=PSrc^;
    Inc(PSrc);
    Inc(PDest);
    end;
  SetLength(Result,(PDest-PStart));
end;

function TPerlRegEx.MakeOptions(aOptions: TPerlRegExOptions): Integer;

  Procedure AddOption(aOpt : TPerlRegExOption; aValue : cuint32);

  begin
    if aOpt in AOptions then
      Result:=Result or aValue;
  end;

begin
  Result:=PCRE2_NEWLINE_ANY or PCRE2_UTF;
  AddOption(preCaseLess,PCRE2_CASELESS);
  AddOption(preMultiLine,PCRE2_MULTILINE);
  AddOption(preSingleLine,PCRE2_DOTALL);
  AddOption(preExtended,PCRE2_EXTENDED);
  AddOption(preAnchored,PCRE2_ANCHORED);
  AddOption(preUnGreedy,PCRE2_UNGREEDY);
  AddOption(preNoAutoCapture,PCRE2_NO_AUTO_CAPTURE);
  AddOption(preAllowEmptyClass,PCRE2_ALLOW_EMPTY_CLASS);
  AddOption(preAltBSUX,PCRE2_ALT_BSUX);
  AddOption(preAltCircumFlex,PCRE2_ALT_CIRCUMFLEX);
  AddOption(preAltVerbNames,PCRE2_ALT_VERBNAMES);
  AddOption(preDollarEndOnly,PCRE2_DOLLAR_ENDONLY);
  AddOption(preDupNames,PCRE2_DUPNAMES);
  AddOption(preEndAnchored,PCRE2_ENDANCHORED);
  AddOption(preFirstLine,PCRE2_FIRSTLINE);
  AddOption(preLiteral,PCRE2_LITERAL);
  AddOption(preMatchInvalidUTF,PCRE2_MATCH_INVALID_UTF);
  AddOption(preMatchUnsetBackRef,PCRE2_MATCH_UNSET_BACKREF);
  AddOption(preNeverBackslashC,PCRE2_NEVER_BACKSLASH_C);
  AddOption(preNoAutoPossess,PCRE2_NO_AUTO_POSSESS);
  AddOption(preNoDotStarAnchor,PCRE2_NO_DOTSTAR_ANCHOR);
  AddOption(preNoStartOptimize,PCRE2_NO_START_OPTIMIZE);
  // maybe we should enable by default ?
  AddOption(preNoUTFCheck,PCRE2_NO_UTF_CHECK);
  AddOption(preUseOffsetLimit,PCRE2_USE_OFFSET_LIMIT);
//  AddOption(preUTF,PCRE2_UTF);

end;

function TPerlRegEx.GetPCREErrorMsg(ErrorNr: Integer): TREString;

var
  Buffer : Array[0..255] of ansichar;

begin
  pcre2_get_error_message(ErrorNr,@Buffer,SizeOf(Buffer));
  Result:=strpas(@Buffer);
end;

procedure TPerlRegEx.Compile;

var
  ErrorNr: Integer;
  ErrorPos: Integer;

begin
  if (FRegEx='') then
    raise ERegularExpressionError.CreateRes(@SRegExMissingExpression);
  CleanUp;
  FCode:=pcre2_compile(TPCRE2_SPTR8(FRegEx),Length(FRegEx),MakeOptions(FOptions),@ErrorNr,@ErrorPos,Nil);
  if (FCode=nil) then
    raise ERegularExpressionError.CreateFmt(SRegExExpressionError,[ErrorPos+1,GetPCREErrorMsg(ErrorNr)]);
  FMatchData:=pcre2_match_data_create_from_pattern(FCode,Nil);

end;

procedure TPerlRegEx.Study;
begin

end;

procedure TPerlRegEx.FreeMatchData;

var
  Data : ppcre2_match_data;

begin
  if FMatchData=Nil then exit;
  Data:=FMatchData;
  FMatchData:=Nil;
  pcre2_match_data_free(Data);
  FResultVector:=Nil;
end;

procedure TPerlRegEx.FreeCodeData;

var
  {$IFDEF USEWIDESTRING}
  Data : Ppcre2_code_16;
  {$ELSE}
  Data : Ppcre2_code_8;
  {$ENDIF}

begin
  if (FCode=Nil) then
    exit;
  Data:=FCode;
  FCode:=Nil;
  pcre2_code_free(Data);
end;

procedure TPerlRegEx.GetNamedGroupInfo;


begin
  FNameEntrySize:=0;
  FNameTable:=Nil;
  pcre2_pattern_info(
    FCode,                 (* the compiled pattern *)
    PCRE2_INFO_NAMECOUNT,  (* get the number of named substrings *)
    @FNameCount);          (* where to put the answer *)
  if (FNameCount = 0) then
    Exit;
  pcre2_pattern_info(
    FCode,                    (* the compiled pattern *)
    PCRE2_INFO_NAMETABLE,     (* address of the table *)
    @FNameTable);             (* where to put the answer *)
  pcre2_pattern_info(
    FCODE,                       (* the compiled pattern *)
    PCRE2_INFO_NAMEENTRYSIZE, (* size of each entry in the table *)
    @FNameEntrySize);
end;

function TPerlRegEx.GetNames(aIndex : Integer): TREString;
var
  Ptr : PCRE2_SPTR;
  N,I : Integer;
  tblName : TREString;

begin
  Ptr:=FNameTable;
  if (aIndex<0) or (aIndex>FNameCount) then
    Raise ERegularExpressionError.CreateFmt(SErrInvalidNameIndex,[aIndex,FNameCount]);
  for i:=0 to aIndex-1 do
    Inc(Ptr,FNameEntrySize);
{$IFDEF USEWIDESTRING}
  n:=ord(ptr[0]);
  Result:=GetStrLen((Ptr+1),FNameEntrySize-2);
{$ELSE}
  n:=(ord(ptr[0]) shl 8) or ord(ptr[1]);
  Result:=GetStrLen((Ptr+2),FNameEntrySize-3);
{$ENDIF}
end;

function TPerlRegEx.Match: Boolean;

var
  newline,option_bits : cuint32;

begin
  Result:=False;
  ClearStoredGroups;
  if not Compiled then
    Compile;
  FMatchData:=pcre2_match_data_create_from_pattern(FCode,Nil);
  Result:=DoMatch(0)=mrFound;
  if Result  then
    begin
    pcre2_pattern_info(FCode,PCRE2_INFO_ALLOPTIONS, @option_bits);
    FIsUtf:=((option_bits and PCRE2_UTF) <> 0);
    pcre2_pattern_info(FCode,PCRE2_INFO_NEWLINE,@newline);
    FCrLFIsNewLine:= (newline=PCRE2_NEWLINE_ANY) or
                     (newline=PCRE2_NEWLINE_CRLF) or
                     (newline=PCRE2_NEWLINE_ANYCRLF);
    end;
end;


function TPerlRegEx.DoMatch(Opts : CUInt32): TMatchResult;

var
  len,rc : cInt;
  S : TREString;

begin
  Result:=mrNotFound;
{$IF SIZEOF(CHAR)=2}
  rc:=pcre2_match_w(
{$ELSE}
  rc:=pcre2_match(
{$ENDIF}
    FCode,                   (* the compiled pattern *)
    PChar(FSubject),         (* the subject TREString *)
    FSubjectLength,         (* the length of the subject *)
    FStart,                  (* start at offset 0 in the subject *)
    Opts,                    (* default options *)
    FMatchData,              (* block for storing the result *)
    Nil);
  if (rc <= 0) then
    begin
    FreeMatchData;
    FreeCodeData;
    if (rc=PCRE2_ERROR_NOMATCH) then
      Exit(mrNotFound)
    else if (rc = 0) then
      raise ERegularExpressionError.CreateFmt(SRegExMatchError,[SErrRegexOvectorTooSmall])
    else
      raise ERegularExpressionError.CreateFmt(SRegExMatchError,[GetPCREErrorMsg(rc)]);
    end;
  Result:=mrFound;
  FResultCount:=rc;
  FResultVector:=pcre2_get_ovector_pointer(FMatchData);
  if FResultVector[0]>FStop then
    Exit(mrAfterStop);
  {For i:=0 to FResultCount-1 do
    Writeln(I,': ',FResultVector[2*I],' - ',FResultVector[2*I+1]);}
  if (FResultVector[0]>FResultVector[1]) then
    begin
    Len:=integer(FResultVector[0]-FResultVector[1]);
    S:=Copy(FSubject,FResultVector[1],Len);
    FreeMatchData;
    FreeCodeData;
    raise ERegularExpressionError.CreateFmt(SRegExMatcStartAfterEnd,[S]);
    end;
  // Next should start after current
  FStart:=FResultVector[1];
  GetNamedGroupInfo;
  if Assigned(OnMatch) then
   OnMatch(Self);
end;

function TPerlRegEx.MatchAgain: Boolean;

var
  StartChar,Opts : cuint32;
begin
  Result:=False;
  Opts:=0;
  // Special case, empty TREString.
  if (FResultVector[0]=FResultVector[1]) then
    begin
    if (FResultVector[0]>=FSubjectLength) then
      Exit;
    Opts:=PCRE2_NOTEMPTY_ATSTART or PCRE2_ANCHORED;
    end
  else
    begin
    // Check whether start empty
    Startchar:=pcre2_get_startchar(FMatchData);
    if (FStart<=Startchar) then
      begin
      (* Reached end of subject.   *)
      if (startchar>=FSubjectLength) then
        Exit;
      (* Advance by one character. *)
      FStart:=StartChar+1;
      (* If UTF-8, it may be more than one code unit. *)
      if FIsUtf then
        begin
        While (FStart<FSubjectLength) do
          begin
          if ((Ord(Subject[FStart+1]) and $c0)<>$80) then
            Exit;
          Inc(FStart);
          end;
        end;
      end;
    end;
  // If we're behind stop, exit at once.
  Case DoMatch(Opts) of
    mrAfterStop : Exit(False);
    mrNotFound : Result:=False;
    mrFound: Result:=True;
  end;
  (*
    This time, a result of NOMATCH isn't an error. If the value in 'options'
    is zero, it just means we have found all possible matches, so the loop ends.
    Otherwise, it means we have failed to find a non-empty-TREString match at a
    point where there was a previous empty-TREString match. In this case, we do what
    Perl does: advance the matching position by one character, and continue. We
    do this by setting the 'end of previous match' offset, because that is picked
    up at the top of the loop as the point at which to start again.

    There are two complications: (a) When CRLF is a valid newline sequence, and
    the current position is just before it, advance by an extra byte. (b)
    Otherwise we must ensure that we skip an entire UTF character if we are in
    UTF mode.
  *)
  While not Result do
    begin
    if Opts=0 then
      Break;
    FResultVector[1]:=FStart+1;             (* Advance one code unit *)
    if FCrLFIsNewLine and                   (* If CRLF is a newline & *)
       (FStart<FSubjectLength-2) and        (* we are at CRLF *)
       (FSubject[FStart+1]=#13) and
       (FSubject[Fstart+2]=#10) then
       inc(FResultVector[1])                (* Advance by one more. *)
    else if (FIsUtf) then                     (* Otherwise, ensure we advance a whole UTF-8 character. *)
      begin
      while (FResultVector[1]<FSubjectLength-1) do
        begin
        if ((Ord(subject[FResultVector[1]]) and $c0) <> $80) then
          break;
        inc(FResultVector[1]);
        end;
      end;
    Case DoMatch(Opts) of
      mrAfterStop :
        begin
        Result:=False;
        Break;
        end;
      mrNotFound : Result:=False;
      mrFound: Result:=True;
    end;
    end;
end;


function TPerlRegEx.Replace: TREString;

var
  NewSubject,Tmp : TREString;

begin
  CheckMatch;
  Result:=ComputeReplacement;
  if Assigned(OnReplace) then
    OnReplace(Self, Result);
  Tmp:=Result;
  if FLastModifiedEnd=0 then
    FLastModifiedEnd:=GetMatchedOffset-1;
  NewSubject:=Copy(FModifiedSubject,1,FLastModifiedEnd)+Tmp;
  FLastModifiedEnd:=Length(NewSubject)+1;
  tmp:=GetSubjectRight;
  FModifiedSubject:=NewSubject+tmp;
  ClearStoredGroups;
end;

function TPerlRegEx.ReplaceAll: Boolean;
begin
  Result:=Match;
  if Not Result then
    exit;
  repeat
    Replace;
  until not MatchAgain;
end;

function IsAlphaAndUnderline(const C: Char): Boolean;

Const
  allowed = ['A'..'Z', 'a'..'z', '_'];

begin
  Result:=CharInSet(C,Allowed);
end;

function IsNumeric(const C: Char): Boolean;

Const
  allowed = ['0'..'9'];

begin
  Result:=CharInSet(C,Allowed);
end;


{ Return values:
  >=0 : group number.
  -1 : whole subject.
  -2 : Left of match.
  -3 : Right of match.
  -99  : invalid.
  On return, I is the index of the next character to process.
}

function TPerlRegEx.GetBackRefIndex(const Ref: TREString; var I: Integer): Integer;

var
  Len,P,N,Group : Integer;

begin
  Len:=Length(Ref);
  Group:=-99;
  Case Ref[I] of
  '0'..'9':
    begin
    Group:=Ord(Ref[i])-Ord('0');
    Inc(I);
    // Only consume as much integers as there are groups.
    // So if there are 15 groups then $16 -> $1 + literal 6.
    While (I<=Len) and (Ref[i] in ['0'..'9']) do
      begin
      N:=(Group*10)+Ord(Ref[i])-Ord('0');
      if N>GroupCount then
        Break;
      Group:=N;
      Inc(I);
      end;
    end;
  '{':
    begin
    Inc(I);
    if (Ref[I] in ['0'..'9']) then
      // \{123}
      begin
      Group:=0;
      while (I<Len) and IsNumeric(Ref[I]) do
        begin
        Group:=(Group*10)+Ord(Ref[i])-Ord('0');
        Inc(I);
        end;
      if (I>Len) or (Ref[I]<>'}') then
        Group:=-99
      else
        Inc(I);
      end
    else
      // \{named}
      begin
      P:=I;
      while (I<Len) and IsAlphaAndUnderline(Ref[I]) do
        Inc(I);
      if (I>Len) or (Ref[I]<>'}') then
        Group:=-99
      else
        begin
        Group:=NamedGroup(Copy(Ref,P,I-P));
        if Group=-1 then
          group:=-99;
        Inc(I);
        end
      end;
    end;
  '_': // Whole subject
    begin
    Group:=-1;
    Inc(I);
    end;
  '&': // \& or $& (whole regex match)
    begin
    Group:=0;
    Inc(I);
    end;
  '+': // Last group
    begin
    Group:=GroupCount;
    Inc(I);
    end;
  '`': // Subject to left of match.
    begin
    Group:=-2;
    inc(I);
    end;
  #39:  // Subject to right of match.
    begin
    Group:=-3;
    inc(I);
    end
  end;
  Result:=Group;
end;

class function TPerlRegEx.TransForm(aTransform: TTransformation; const S: TREString): TREString;

begin
  Case aTransform of
    tFirstCap : Result:=UpperCase(Copy(S,1,1))+LowerCase(Copy(S,2,Length(S)-1));
    tInitialCap : Result:=InitialCaps(S);
    tUpperCase : Result:=UpperCase(S);
    tLowerCase : Result:=LowerCase(S);
  else
    Result:=S;
  end;
end;

function TPerlRegEx.ComputeReplacement: TREString;

var
  Res : TREString;
  Len : Integer;

  Procedure AddToResult(aStart,aNext : Integer); inline;

  begin
    Res:=Res+Copy(FReplacement,aStart,aNext-aStart);
  end;

  Procedure AddNamedGroup(const aName : TREString); inline;

  begin
    Res:=Res+NamedGroups[aName];
  end;

  Function AddBackRef(aTransform : TTransformation; I : Integer) : Integer;

  var
    P,N,Group : Integer;

  begin
    Group:=GetBackRefIndex(FReplacement,I);
    Case Group of
      -99 : ; // invalid
      -1 : Res:=Res+TransForm(aTransform,FSubject);
      -2 : Res:=Res+TransForm(aTransform,SubjectLeft);
      -3 : Res:=Res+TransForm(aTransform,SubjectRight);
    else
      if Group<=GroupCount then
        Res:=Res+TransForm(aTransform,Groups[Group]);
    end;
    Result:=I;
  end;

var
  I, P, Last : Integer;
  updatelast : boolean;

begin
  Len:=Length(FReplacement);
  if Len=0 then
    Exit('');
  I:=1;
  Last:=1;
  while I<=Len do
    begin
    case FReplacement[I] of
    '\':
        begin
        if (I=Len) then
          raise ERegularExpressionError.CreateFmt(SRegExIndexOutOfBounds,[I]);
        AddToResult(Last,I);
        Inc(I);
        UpdateLast:=True;
        case FReplacement[I] of
          '$', '\':
            begin
            Inc(I);
            AddToResult(I-1,I);
            end;
          'g':
            begin
            if (I+2<Len) and (FReplacement[I+1] = '<') then
              begin
              Inc(I,2); // First char
              P:=I;
              while (I<Len) and IsAlphaAndUnderline(FReplacement[I]) do
                Inc(I);
              // We should now be on closing >
              if (I<=Len) and (FReplacement[I]='>') then
                begin
                AddNamedGroup(Copy(FReplaceMent,P,I-P));
                Inc(I);
                Last:=I;
                end
              else
                begin
                I:=I+2; // Skip everything.
                UpdateLast:=False
                end;
              end
            else
              UpdateLast:=False;
            end;
          'l','L' : I:=AddBackRef(tLowerCase,I);
          'u','U' : I:=AddBackRef(tLowerCase,I);
          'f','F' : I:=AddBackRef(tFirstCap,I);
          'i','I' : I:=AddBackRef(tInitialCap,I);
        else
          I:=AddBackRef(tNone,I);
        end;
        if UpdateLast then
          Last:=I;
      end;
    '$':
      begin
      if I=Len then
        raise ERegularExpressionError.CreateFmt(SRegExIndexOutOfBounds,[I]);
      AddToResult(Last,I);
      Inc(I);
      if FReplacement[I]='$' then
        begin
        AddToResult(Last,I);
        Inc(I);
        end
      else
        I:=AddBackRef(tNone,I);
       Last:=I;
       end;
    else // Case
      Inc(I);
    end;
    end;
  if I>Last then
    AddToResult(Last,I);
  Result:=Res;
end;

procedure TPerlRegEx.StoreGroups;

var
  I : Integer;

begin
  CheckMatch;
  SetLength(FStoredGroups,GroupCount+1);
  For I:=0 to GroupCount do
    FStoredGroups[i]:=GetResultString(I);
end;

function TPerlRegEx.NamedGroup(const aName: TREString): Integer;

var
  Ptr : PCRE2_SPTR;
  N,I : Integer;
  tblName : TREString;

begin
  Ptr:=FNameTable;
  for i:=0 to FNameCount-1 do
    begin
{$IFDEF USEWIDESTRING}
    n:=ord(ptr[0]);
    tblName:=GetStrLen((Ptr+1),FNameEntrySize-2);
{$ELSE}
    n:=(ord(ptr[0]) shl 8) or ord(ptr[1]);
    tblName:=GetStrLen((Ptr+2),FNameEntrySize-3);
{$ENDIF}
    if SameText(TblName,aName) then
      Exit(n);
    Inc(Ptr,FNameEntrySize);
    end ;
  Result:=-1;
end;

procedure TPerlRegEx.Split(const aStrings: TStrings; aLimit: Integer);

var
  NewStart,LastEnd,Matches: Integer;

begin
  if Not Assigned(aStrings) then
    raise ERegularExpressionError.Create(SRegExStringsRequired);
  if (aLimit=1) or not Match then
    begin
    aStrings.Add(Subject);
    Exit;
    end;
  LastEnd:=0; // Last match pos
  Matches:=1;
  repeat
    NewStart:=FirstOffset; // Start of current match
    aStrings.Add(Copy(Subject,LastEnd+1,NewStart-LastEnd)); // Copy everything since last match.
    Inc(Matches);
    LastEnd:=NewStart+MatchedLength; // update last match pos.
  until ((aLimit>1) and (Matches>=aLimit)) or not MatchAgain;
  aStrings.Add(TREString(Copy(FSubject,LastEnd+1,FSubjectLength -LastEnd)));
end;

function TPerlRegEx.Split(aLimit: Integer): TREStringDynArray;
var
  L: TStrings;
  I : integer;

begin
  L:=TStringList.Create;
  try
    Split(L,aLimit);
    // We cannot use L.ToStringArray, because the string type may differ :/
    SetLength(Result,L.Count);
    For I:=0 to L.Count-1 do
      Result[I]:=L[I];
  finally
    L.Free;
  end;
end;

procedure TPerlRegEx.SplitCapture(const aStrings: TStrings; aLimit: Integer);

begin
  SplitCapture(aStrings,aLimit,1);
end;

procedure TPerlRegEx.SplitCapture(const aStrings: TStrings; aLimit: Integer; aOffset: Integer);

var
  NewStart,LastEnd,Matches: Integer;
  DoCopy : Boolean;

begin
  if Not Assigned(aStrings) then
    raise ERegularExpressionError.Create(SRegExStringsRequired);
  if (aLimit=1) or not Match then
    begin
    aStrings.Add(Subject);
    Exit;
    end;
  Dec(aOffset);
  if (aOffset>0) then
    Dec(aLimit);
  LastEnd:=0; // Last match pos
  Matches:=1;
  repeat
    NewStart:=FirstOffset; // Start of current match
    DoCopy:=(NewStart>aOffset);
    if DoCopy then
      begin
      aStrings.Add(Copy(Subject,LastEnd+1,NewStart-LastEnd)); // Copy everything since last match.
      if GroupCount > 0 then
        aStrings.Add(Groups[GroupCount]);
      Inc(Matches);
      LastEnd:=NewStart+MatchedLength; // update last match pos.
      end;
  until ((aLimit>1) and (Matches>=aLimit)) or not MatchAgain;
  aStrings.Add(TREString(Copy(FSubject,LastEnd+1,FSubjectLength-LastEnd)));
end;

function TPerlRegEx.SplitCapture(aLimit: Integer; aOffset: Integer): TREStringDynArray;

var
  L: TStrings;
  I : integer;

begin
  L:=TStringList.Create;
  try
    SplitCapture(L,aLimit,aOffset);
    // We cannot use L.ToStringArray, because the string type may differ :/
    SetLength(Result,L.Count);
    For I:=0 to L.Count-1 do
      Result[I]:=L[I];
  finally
    L.Free;
  end;
end;

{ TPerlRegExList }

function TPerlRegExList.GetCount: Integer;
begin
  Result:=FList.Count;
end;

function TPerlRegExList.GetOwnsRegex: Boolean;
begin
  Result:=FList.OwnsObjects;
end;

function TPerlRegExList.GetRegEx(aIndex: Integer): TPerlRegEx;
begin
  Result:=TPerlRegEx(Flist[aIndex])
end;

function TPerlRegExList.GetStart: Integer;
begin
  Result:=FStart;
end;

function TPerlRegExList.GetStop: Integer;
begin
  Result:=FStop;
end;

function TPerlRegExList.GetSubject: TREString;
begin
  Result:=FSubject;
end;

procedure TPerlRegExList.SetRegEx(aIndex: Integer; aValue: TPerlRegEx);
begin
  FList[aIndex]:=aValue;
end;

procedure TPerlRegExList.SetStart(AValue: Integer);
var
  I : Integer;
begin
  if AValue=FStart then exit;
  FStart:=aValue;
  For I:=0 to Count-1 do
    RegEx[I].Start:=aValue;
end;

procedure TPerlRegExList.SetStop(AValue: Integer);
var
  I : Integer;
begin
  if AValue=FStart then exit;
  FStop:=aValue;
  For I:=0 to Count-1 do
    RegEx[I].Stop:=aValue;
end;

procedure TPerlRegExList.SetSubject(aValue: TREString);
var
  I: Integer;
begin
  if aValue=FSUbject then exit;
  FSubject:=aValue;
  for I:=Count-1 downto 0 do
    RegEx[I].Subject:=Subject;
  FMatch:=nil;
end;

procedure TPerlRegExList.UpdateRegEx(const aRegEx: TPerlRegEx);
begin
  aRegEx.Subject:=FSubject;
  ARegEx.Start:=FStart;
  ARegEx.Stop:=FStop;
end;

constructor TPerlRegExList.Create(OwnsRegex: Boolean);
begin
  FList:=TFPObjectList.Create(OwnsRegex);
end;

destructor TPerlRegExList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TPerlRegExList.Add(const aRegEx: TPerlRegEx): Integer;
begin
  Result:=FList.Add(aRegEx);
  UpdateRegEx(aRegEx);
end;

procedure TPerlRegExList.Clear;
begin
  FList.Clear;
end;

procedure TPerlRegExList.Delete(aIndex: Integer);
begin
  FList.Delete(aIndex);
end;

function TPerlRegExList.IndexOf(const aRegEx: TPerlRegEx): Integer;
begin
  Result:=FList.IndexOf(aRegex);
end;

procedure TPerlRegExList.Insert(aIndex: Integer; const aRegEx: TPerlRegEx);
begin
  FList.Insert(aIndex,aRegex);
end;

function TPerlRegExList.Match: Boolean;
begin
  SetStart(1);
  FMatch:=nil;
  Result:=MatchAgain;
end;

function TPerlRegExList.MatchAgain: Boolean;

var
  PRE : TPerlRegEx;
  I,StartAt,Current: Integer;

begin
  // Determine start position
  if not Assigned(FMatch) then
    StartAt:=Start
  else
    With FMatch do
      StartAt:=0;  // MVC todo {InternalGetMatchedOffset+InternalGetMatchedLength};
  FMatch:=nil;
  Current:=-1;
  // Check all regexes for new closest match.
  I:=0;
  While (I<Count) and (Current>StartAt) do
    begin
    PRE:=RegEx[I];
    // Should we search this regex again ?
    if (not PRE.FoundMatch) or (PRE.FirstOffset<StartAt) then
      begin
      PRE.Start:=StartAt;
      PRE.MatchAgain;
      end;
    // New first position found ?
    if PRE.FoundMatch and ((FMatch=Nil) or (PRE.FirstOffset<Current)) then
      begin
      Current:=Pre.FirstOffset;
      FMatch:=PRE;
      end;
    Inc(I);
    end;
  Result:=Current<>-1;
end;

end.
