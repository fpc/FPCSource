unit wasm.pcrebridge;

{$mode ObjFPC}{$H+}

interface

uses
  ctypes, sysutils, wasm.regexp.objects;

{$IF SIZEOF(CHAR)=2}
{$DEFINE STRING_IS_UNICODE}
{$ENDIF}

const
  PCRE2_NEWLINE_ANY         = 1;
  PCRE2_UTF                 = 1 shl 1;
  PCRE2_CASELESS            = 1 shl 2;
  PCRE2_MULTILINE           = 1 shl 3;
  PCRE2_DOTALL              = 1 shl 4;
  PCRE2_EXTENDED            = 1 shl 5;
  PCRE2_ANCHORED            = 1 shl 6;
  PCRE2_UNGREEDY            = 1 shl 7;
  PCRE2_NO_AUTO_CAPTURE     = 1 shl 8;
  PCRE2_ALLOW_EMPTY_CLASS   = 1 shl 9;
  PCRE2_ALT_BSUX            = 1 shl 10;
  PCRE2_ALT_CIRCUMFLEX      = 1 shl 11;
  PCRE2_ALT_VERBNAMES       = 1 shl 12;
  PCRE2_DOLLAR_ENDONLY      = 1 shl 13;
  PCRE2_DUPNAMES            = 1 shl 14;
  PCRE2_ENDANCHORED         = 1 shl 15;
  PCRE2_FIRSTLINE           = 1 shl 16;
  PCRE2_LITERAL             = 1 shl 17;
  PCRE2_MATCH_INVALID_UTF   = 1 shl 18;
  PCRE2_MATCH_UNSET_BACKREF = 1 shl 19;
  PCRE2_NEVER_BACKSLASH_C   = 1 shl 20;
  PCRE2_NO_AUTO_POSSESS     = 1 shl 21;
  PCRE2_NO_DOTSTAR_ANCHOR   = 1 shl 22;
  PCRE2_NO_START_OPTIMIZE   = 1 shl 23;
  PCRE2_NO_UTF_CHECK        = 1 shl 24;
  PCRE2_USE_OFFSET_LIMIT    = 1 shl 25;

  PCRE2_ERROR_NOMATCH = -(1);
  PCRE2_ERROR_PARTIAL = -(2);
  PCRE2_ERROR_WASM    = -(200);

  PCRE2_INFO_ALLOPTIONS    = 0;
  PCRE2_INFO_NAMECOUNT     = 1;
  PCRE2_INFO_NAMETABLE     = 2;
  PCRE2_INFO_NEWLINE       = 3;
  PCRE2_INFO_NAMEENTRYSIZE = 4;

  PCRE2_NEWLINE_CR = 1;
  PCRE2_NEWLINE_LF = 2;
  PCRE2_NEWLINE_CRLF = 3;

  PCRE2_NEWLINE_ANYCRLF = 5;
  PCRE2_NEWLINE_NUL = 6;
  PCRE2_BSR_UNICODE = 1;
  PCRE2_BSR_ANYCRLF = 2;

  PCRE2_NOTEMPTY_ATSTART = 1;


Type
  tsize_t = csize_t;
  Psize_t = ^tsize_t;
  PCRE2_SIZE = tsize_t;
  PTsize_t = ^Tsize_t;
  PPTsize_t = ^PTsize_t;
  tuint32_t = cardinal;
  Tint32_t = cuint32;
  Tcint = cint;

  { TPCREWasmRegExp }

  TPCREWasmRegExp = Class(TWasmRegExp)
    matchindex : Integer;
    lastRes : TRegExpResult;
    haveres : Boolean;
    lasterror : String;
    OVector : Array of Tsize_t;
    FNamesTable : String;
    FNamesTableEntrySize : Integer;
  private
    procedure CreateNamesTable;
    function GetCaptureIndex(aStartIndex, aStopIndex: Integer): Integer;
    function HandleExec(aOffset: TSize_t; const S: String): Tcint;
  end;

  TPCRE2_SPTR8 = PAnsichar;
  Ppcre2_code_8 = TPCREWasmRegExp;
  PTpcre2_code_8 = Ppcre2_code_8;
  PTpcre2_compile_context_8 = Pointer;
  PTpcre2_match_data_8 = TPCREWasmRegExp;
  PTpcre2_general_context_8 = Pointer;
  PTpcre2_match_context_8 = Pointer;
  ppcre2_match_data = PTpcre2_match_data_8;
  PCRE2_SPTR = PAnsiChar;
  PTPCRE2_UCHAR8 = PAnsiChar;



function pcre2_compile(RegExp: TPCRE2_SPTR8; RegexLen: Tsize_t; Opts: Tuint32_t; ErrorNr: Pcint; ErrorPos: Psize_t; Context: PTpcre2_compile_context_8):Ppcre2_code_8;
procedure pcre2_code_free(Regexp:PTpcre2_code_8);

function pcre2_match_w(RegExp:PTpcre2_code_8; aSubject: TPCRE2_SPTR8; aSubjectLen: Tsize_t; aOffset: Tsize_t; Opts: Tuint32_t; MatchData: PTpcre2_match_data_8; aContext: PTpcre2_match_context_8):Tcint;
function pcre2_match(RegExp:PTpcre2_code_8; aSubject: TPCRE2_SPTR8; aSubjectLen: Tsize_t; aOffset: Tsize_t; Opts: Tuint32_t; MatchData: PTpcre2_match_data_8; aContext: PTpcre2_match_context_8):Tcint;
function pcre2_match_data_create_from_pattern(RegExp: PTpcre2_code_8; aContext: PTpcre2_general_context_8):PTpcre2_match_data_8;
procedure pcre2_match_data_free(aMatchData:PTpcre2_match_data_8);

function pcre2_pattern_info(RegExp:PTpcre2_code_8; Info: Tuint32_t; Res: pointer):Tcint;

function pcre2_get_ovector_pointer (match: PTpcre2_match_data_8): Psize_t;
function pcre2_get_startchar(match: PTpcre2_match_data_8): Tsize_t;
function pcre2_get_error_message(ErrNo:Tcint; aString :PTPCRE2_UCHAR8; aStrlen:Tsize_t):Tcint;

function libpcre28loaded : Boolean;
procedure Loadlibpcre28;


implementation

var
  gLastError : String;

function OptsToFlags(Opts : Tuint32_t) : TRegexpFlags;

  Procedure Check(PerlFlag :Tuint32_t; RegexFlag :TRegexpFlag);
  begin
    if (Opts and PerlFlag)<>0 then
      Include(Result,RegexFlag);
  end;

begin
  Result:=[rfIndices,rfGlobal];
  Check(PCRE2_MULTILINE,rfMultiLine);
  Check(PCRE2_CASELESS,rfIgnoreCase);
  Check(PCRE2_UTF,rfUnicode);
  Check(PCRE2_DOTALL,rfDotAll);
  (* The rest is not supported *)
end;

function pcre2_compile (RegExp:TPCRE2_SPTR8; RegexLen:Tsize_t; Opts :Tuint32_t; ErrorNr :Pcint; ErrorPos:Psize_t; Context :PTpcre2_compile_context_8):Ppcre2_code_8;

var
  F : TRegexpFlags ;
  S : String;

begin
  Result:=Nil;
  SetLength(S,RegexLen);
  Move(Regexp^,S[1],SizeOf(Char)*RegexLen);
  F:=OptsToFlags(Opts);
  try
    Result:=TPCREWasmRegExp.Create(S,F);
  except
    on E : Exception do
      begin
      ErrorNr^:=PCRE2_ERROR_WASM; // Does not exist (yet)
      gLastError:=E.Message;
      ErrorPos^:=0;
      end;
  end;
end;

procedure pcre2_code_free(Regexp:PTpcre2_code_8);

var
  RE : TPCREWasmRegExp absolute Regexp;

begin
  RE.Free;
end;

function pcre2_match_data_create_from_pattern(RegExp:PTpcre2_code_8; aContext :PTpcre2_general_context_8):PTpcre2_match_data_8;

begin
  Result:=Regexp;
end;

function pcre2_pattern_info(RegExp: PTpcre2_code_8; Info: Tuint32_t; Res: pointer): Tcint;

var
  RE : TPCREWasmRegExp absolute Regexp;

begin
  result:=0;
  case info of
    PCRE2_INFO_NAMECOUNT :
      Pcuint32(Res)^:=Length(RE.lastRes.Groups);
    PCRE2_INFO_NAMETABLE :
      PPChar(Res)^:=PChar(RE.FNamesTable);
    PCRE2_INFO_NAMEENTRYSIZE :
      Pcuint32(Res)^:=RE.FNamesTableEntrySize;
    PCRE2_INFO_ALLOPTIONS :
      Pcuint32(Res)^:=PCRE2_UTF;
    PCRE2_INFO_NEWLINE :
      Pcuint32(Res)^:=PCRE2_NEWLINE_ANY;
  else
    Result:=PCRE2_ERROR_WASM;
  end;
end;


procedure pcre2_match_data_free(aMatchData: PTpcre2_match_data_8);
begin
  // Do nothing
end;

function TPCREWasmRegExp.GetCaptureIndex(aStartIndex,aStopIndex : Integer) : Integer;

begin
  Result:=Length(lastRes.Matches)-1;
  While (Result>=0) and Not lastRes.Matches[Result].MatchPos(aStartIndex,aStopIndex) do
    Dec(Result);
end;

procedure TPCREWasmRegExp.CreateNamesTable;

Const
  ExtraSize = SizeOf(Word);
  {$IFDEF STRING_IS_UNICODE}
  CharOffset = 1;
  {$ELSE}
  CharOffset = 2;
  {$ENDIF}

var
  i, tmp, entrylen,len : Integer;
  N: UTF8String;
  NS : String;
  CaptureIdx : Word;
  CaptureOffset,NameOffset : integer;

begin
  FNamesTableEntrySize:=0;
  FNamesTable:='';
  Len:=Length(lastRes.Groups);
  if Len=0 then exit;
  entryLen:=Length(lastRes.Groups[0].Name);
  For I:=1 to Len-1 do
    begin
    tmp:=Length(lastRes.Groups[i].Name);
    if tmp>Entrylen then
      Entrylen:=Tmp;
    end;
  EntryLen:=EntryLen+ExtraSize*2;
  FNamesTableEntrySize:=EntryLen;
  SetLength(FNamesTable,Len*EntryLen);
  FillChar(FNamesTable[1],Length(FNamesTable)*SizeOf(Char),0);
  For I:=0 to Len-1 do
    begin
    N:=lastRes.Groups[i].Name;
    {$IFDEF STRING_IS_UNICODE}
    NS:=UTF8Decode(N);
    {$ELSE}
    NS:=N;
    {$ENDIF}
    CaptureIdx:=GetCaptureIndex(lastRes.Groups[i].StartIndex,lastRes.Groups[i].StopIndex);
    CaptureIdx:=NtoBE(CaptureIdx);
    CaptureOffset:=1+(I*EntryLen);
    NameOffset:=1+CharOffset+(I*EntryLen);
    Move(CaptureIdx,FNamesTable[CaptureOffset],SizeOf(Word));
    Move(NS[1],FNamesTable[NameOffset],Length(NS)*SizeOf(Char));
    end;
end;

function TPCREWasmRegExp.HandleExec(aOffset : TSize_t;Const S : String) : Tcint;

var
  i, len : Integer;

begin
  try
    LastIndex:=aOffset;
    lastRes:=Exec(S);
    len:=Length(lastRes.Matches);
    if len=0 then
      Result:=PCRE2_ERROR_NOMATCH
    else
      Result:=len;
    SetLength(OVector,Len*2);
    For i:=0 to len-1 do
      begin
      OVector[I*2]:=LastRes.Matches[i].StartIndex-1;
      OVector[(I*2)+1]:=LastRes.Matches[i].StopIndex-1;
      end;
    Len:=Length(lastRes.Groups);
    if Len>0 then
      CreateNamesTable;
  except
    Result:=PCRE2_ERROR_WASM;
  end;
end;

function pcre2_match_w(RegExp: PTpcre2_code_8; aSubject: TPCRE2_SPTR8; aSubjectLen: Tsize_t; aOffset: Tsize_t; Opts: Tuint32_t;
  MatchData: PTpcre2_match_data_8; aContext: PTpcre2_match_context_8): Tcint;

var
  RE : TPCREWasmRegExp absolute Regexp;
  S : String;
  {$IFNDEF STRING_IS_UNICODE}
  US : UnicodeString;
  {$ENDIF}

begin
  {$IFDEF STRING_IS_UNICODE}
  S:='';
  SetLength(S,aSubjectLen);
  if aSubjectLen>0 then
    Move(aSubject^,S[1],aSubjectLen*SizeOf(Char));
  {$ELSE}
  US:='';
  SetLength(US,aSubjectLen);
  if aSubjectLen>0 then
    Move(aSubject^,US[1],aSubjectLen* SizeOf(Char));
  S:=UTF8Encode(US);
  {$ENDIF}
  Result:=Re.HandleExec(aOffset,S);
end;

function pcre2_match(RegExp: PTpcre2_code_8; aSubject: TPCRE2_SPTR8; aSubjectLen: Tsize_t; aOffset: Tsize_t; Opts: Tuint32_t;
  MatchData: PTpcre2_match_data_8; aContext: PTpcre2_match_context_8): Tcint;

var
  RE : TPCREWasmRegExp absolute Regexp;
  {$IFDEF STRING_IS_UNICODE}
  RS : UnicodeString;
  {$ENDIF}
  S : String;

begin
  {$IFDEF STRING_IS_UNICODE}
  Res:='';
  SetLength(RS,aSubjectLen);
  if aSubjectLen>0 then
    Move(aSubject^,RS[1],aSubjectLen* SizeOf(Char));
  S:=UTF8Decode(RS);
  {$ELSE}
  S:='';
  SetLength(S,aSubjectLen);
  if aSubjectLen>0 then
    Move(aSubject^,S[1],aSubjectLen*SizeOf(Char));
  {$ENDIF}
  Result:=Re.HandleExec(aOffset,S);
end;


function pcre2_get_ovector_pointer(match: PTpcre2_match_data_8): Psize_t;
var
  RE : TPCREWasmRegExp absolute match;
begin
  Result:=PSize_t(Re.OVector);
end;

function pcre2_get_startchar(match: PTpcre2_match_data_8): Tsize_t;
var
  RE : TPCREWasmRegExp absolute match;
begin
  if (Length(Re.lastRes.Matches)>0) then
    Result:=Re.LastRes.Matches[0].StartIndex
  else
    Result:=Length(Re.lastRes.Input);
end;

function pcre2_get_error_message(ErrNo: Tcint; aString: PTPCRE2_UCHAR8; aStrlen: Tsize_t): Tcint;

var
  Err : RawByteString;

begin
  Err:=Format('Unknown error %d',[ErrNo]);
  Result:=Length(Err);
  if (Result>aStrLen) then
    Result:=aStrLen;
  if Result>0 then
    Move(Err[1],aString^,Result);
end;

function libpcre28loaded : Boolean;

begin
  Result:=True;
end;

procedure Loadlibpcre28;
begin
  //
end;



end.

