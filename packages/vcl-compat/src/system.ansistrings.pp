unit System.AnsiStrings;

{$mode objfpc}
{$H+}
{$Macro ON}

interface

uses
  {$ifdef FPC_DOTTEDUNITS}
  System.SysUtils, System.StrUtils;
  {$else FPC_DOTTEDUNITS}
  SysUtils, StrUtils;
  {$endif FPC_DOTTEDUNITS}

const
  EmptyAnsiStr: AnsiString = '';
  NullAnsiStr: PAnsiString = @EmptyAnsiStr;

  WordDelimiters: set of Byte = [0..255] -
    [Ord('a')..Ord('z'), Ord('A')..Ord('Z'), Ord('1')..Ord('9'), Ord('0')];

{Type
  TFilenameCaseMatch = SysUtils.TFil;}

function AdjustLineBreaks(const S: AnsiString; Style: TTextLineBreakStyle): AnsiString; overload;
function AnsiCompareFileName(const S1, S2: AnsiString): Integer; inline;
function AnsiCompareStr(const S1, S2: AnsiString): Integer; inline; overload;
function AnsiCompareText(const S1, S2: AnsiString): Integer; inline; overload;
function AnsiContainsStr(const AText, ASubText: AnsiString): Boolean; overload;
function AnsiContainsText(const AText, ASubText: AnsiString): Boolean; overload;
function AnsiDequotedStr(const S: AnsiString; AQuote: AnsiChar): AnsiString; overload;
function AnsiEndsStr(const ASubText, AText: AnsiString): Boolean; overload;
function AnsiEndsText(const ASubText, AText: AnsiString): Boolean; overload;
function AnsiExtractQuotedStr(var Src: PAnsiChar; Quote: AnsiChar): AnsiString; overload;
function AnsiFormatBuf(var Buffer; BufLen: Cardinal; const Format; FmtLen: Cardinal; const Args: array of const): Cardinal; overload;
function AnsiFormatBuf(var Buffer; BufLen: Cardinal; const Format; FmtLen: Cardinal; const Args: array of const;  const AFormatSettings: TFormatSettings): Cardinal; overload;
function AnsiIndexStr(const AText: AnsiString; const AValues: array of AnsiString): Integer; overload;
function AnsiIndexText(const AText: AnsiString; const AValues: array of AnsiString): Integer; overload;
function AnsiLastChar(const S: AnsiString): PAnsiChar; overload;
function AnsiLeftStr(const AText: AnsiString; const ACount: Integer): AnsiString; overload;
function AnsiLowerCase(const S: AnsiString): AnsiString; overload;
function AnsiLowerCaseFileName(const S: AnsiString): AnsiString; overload; deprecated 'Use AnsiLowerCase instead';
function AnsiMatchStr(const AText: AnsiString; const AValues: array of AnsiString): Boolean; overload;
function AnsiMatchText(const AText: AnsiString; const AValues: array of AnsiString): Boolean; overload;
function AnsiMidStr(const AText: AnsiString; const AStart, ACount: Integer): AnsiString; overload;
function AnsiPos(const Substr, S: AnsiString): Integer; overload;
function AnsiQuotedStr(const S: AnsiString; Quote: AnsiChar): AnsiString; overload;
function AnsiReplaceStr(const AText, AFromText, AToText: AnsiString): AnsiString; overload;
function AnsiReplaceText(const AText, AFromText, AToText: AnsiString): AnsiString; overload;
function AnsiReverseString(const AText: AnsiString): AnsiString; overload;
function AnsiRightStr(const AText: AnsiString; const ACount: Integer): AnsiString; overload;
function AnsiSameStr(const S1, S2: AnsiString): Boolean; inline; overload;
function AnsiSameText(const S1, S2: AnsiString): Boolean; inline; overload;
function AnsiStartsStr(const ASubText, AText: AnsiString): Boolean; overload;
function AnsiStartsText(const ASubText, AText: AnsiString): Boolean; overload;
function AnsiStrAlloc(Size: Cardinal): PAnsiChar;
function AnsiStrComp(S1, S2: PAnsiChar): Integer; inline; overload;
function AnsiStrIComp(S1, S2: PAnsiChar): Integer; inline; overload;
function AnsiStrLastChar(P: PAnsiChar): PAnsiChar; overload;{$IFNDEF POSIX} inline; {$ENDIF}
function AnsiStrLComp(S1, S2: PAnsiChar; MaxLen: Cardinal): Integer; overload;
function AnsiStrLIComp(S1, S2: PAnsiChar; MaxLen: Cardinal): Integer; overload;
function AnsiStrLower(Str: PAnsiChar): PAnsiChar; overload;{$IFDEF MSWINDOWS} inline; {$ENDIF}
function AnsiStrPos(Str, SubStr: PAnsiChar): PAnsiChar; overload;
function AnsiStrRScan(Str: PAnsiChar; Chr: AnsiChar): PAnsiChar; overload;
function AnsiStrScan(Str: PAnsiChar; Chr: AnsiChar): PAnsiChar; overload;
function AnsiStrUpper(Str: PAnsiChar): PAnsiChar; overload;{$IFDEF MSWINDOWS} inline; {$ENDIF}
function AnsiUpperCase(const S: AnsiString): AnsiString; overload;
function AnsiUpperCaseFileName(const S: AnsiString): AnsiString; overload; deprecated 'Use AnsiUpperCase instead';
procedure AppendStr(var Dest: AnsiString; const S: AnsiString); deprecated;
procedure AssignStr(var P: PAnsiString; const S: AnsiString); deprecated;
function ByteToCharIndex(const S: AnsiString; Index: Integer): Integer; overload; inline; deprecated 'Use ElementToCharIndex.';
function ByteToCharLen(const S: AnsiString; MaxLen: Integer): Integer; overload; inline; deprecated 'Use ElementToCharLen';
function ByteType(const S: AnsiString; Index: Integer): TMbcsByteType; overload;
function ChangeFileExt(const FileName, Extension: AnsiString): AnsiString; overload;
function ChangeFilePath(const FileName, Path: AnsiString): AnsiString; overload;
function CharLength(const S: AnsiString; Index: Integer): Integer; overload;
function CharToByteIndex(const S: AnsiString; Index: Integer): Integer; overload; inline; deprecated 'Use CharToElementIndex.';
function CharToByteLen(const S: AnsiString; MaxLen: Integer): Integer; overload; inline; deprecated 'Use CharToElementLen.';
function CharToElementIndex(const S: AnsiString; Index: Integer): Integer; overload;
function CharToElementLen(const S: AnsiString; MaxLen: Integer): Integer; overload;
function CompareStr(const S1, S2: AnsiString): Integer; overload;
function CompareStr(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions): Integer; overload;
function CompareText(const S1, S2: AnsiString): Integer; overload;
function CompareText(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions): Integer; overload;
function ContainsStr(const AText, ASubText: AnsiString): Boolean; inline; overload;
function ContainsText(const AText, ASubText: AnsiString): Boolean; inline; overload;
procedure DisposeStr(P: PAnsiString); deprecated;
function DupeString(const AText: AnsiString; ACount: Integer): AnsiString; overload;
function ElementToCharIndex(const S: AnsiString; Index: Integer): Integer; overload;
function ElementToCharLen(const S: AnsiString; MaxLen: Integer): Integer; overload;
function EndsStr(const ASubText, AText: AnsiString): Boolean; inline; overload;
function EndsText(const ASubText, AText: AnsiString): Boolean; inline; overload;
function ExcludeTrailingBackslash(const S: AnsiString): AnsiString; platform;
function ExcludeTrailingPathDelimiter(const S: AnsiString): AnsiString; overload;
function ExpandFileNameCase(const FileName: AnsiString;  out MatchFound: TFilenameCaseMatch): AnsiString; overload;
function ExpandFileName(const FileName: AnsiString): AnsiString; overload;
function ExpandUNCFileName(const FileName: AnsiString): AnsiString; overload;
function ExtractFileDir(const FileName: AnsiString): AnsiString; overload;
function ExtractFileDrive(const FileName: AnsiString): AnsiString; overload;
function ExtractFileExt(const FileName: AnsiString): AnsiString; overload;
function ExtractFileName(const FileName: AnsiString): AnsiString; overload;
function ExtractFilePath(const FileName: AnsiString): AnsiString; overload;
function ExtractRelativePath(const BaseName, DestName: AnsiString): AnsiString; overload;
function ExtractShortPathName(const FileName: AnsiString): AnsiString; overload;
function FloatToText(BufferArg: PAnsiChar; const Value; ValueType: TFloatValue;  Format: TFloatFormat; Precision, Digits: Integer; const AFormatSettings: TFormatSettings): Integer; overload;
function FloatToText(BufferArg: PAnsiChar; const Value; ValueType: TFloatValue;  Format: TFloatFormat; Precision, Digits: Integer): Integer; overload; inline;
function FloatToTextFmt(Buf: PAnsiChar; const Value; ValueType: TFloatValue; Format: PAnsiChar; const AFormatSettings: TFormatSettings): Integer; overload;
function FloatToTextFmt(Buf: PAnsiChar; const Value; ValueType: TFloatValue; Format: PAnsiChar): Integer; overload; inline;
procedure FmtStr(var aResult: AnsiString; const Format: AnsiString; const Args: array of const; const AFormatSettings: TFormatSettings); overload;
procedure FmtStr(var aResult: AnsiString; const Format: AnsiString; const Args: array of const); overload;
function FormatBuf(var Buffer; BufLen: Cardinal; const Format; FmtLen: Cardinal; const Args: array of const): Cardinal; overload;
function FormatBuf(var Buffer; BufLen: Cardinal; const Format; FmtLen: Cardinal; const Args: array of const; const AFormatSettings: TFormatSettings): Cardinal; overload;
function Format(const aFormat: AnsiString; const Args: array of const): AnsiString; overload;
function Format(const aFormat: AnsiString; const Args: array of const;  const AFormatSettings: TFormatSettings): AnsiString; overload;
function IncludeTrailingBackslash(const S: AnsiString): AnsiString; platform;
function IncludeTrailingPathDelimiter(const S: AnsiString): AnsiString; overload;
function IndexStr(const AText: AnsiString; const AValues: array of AnsiString): Integer; overload;
function IndexText(const AText: AnsiString; const AValues: array of AnsiString): Integer; overload;
function IsDelimiter(const Delimiters, S: AnsiString; Index: Integer): Boolean; overload;
function IsPathDelimiter(const S: AnsiString; Index: Integer): Boolean; overload;
function IsValidIdent(const Ident: AnsiString; AllowDots: Boolean; StrictDots : Boolean = False): Boolean;
function LastDelimiter(const Delimiters, S: AnsiString): Integer; overload;
function LeftBStr(const AText: AnsiString; const AByteCount: Integer): AnsiString;
function LeftStr(const AText: AnsiString; const ACount: Integer): AnsiString; overload;
function LowerCase(const S: AnsiString): AnsiString; overload;
function LowerCase(const S: AnsiString; LocaleOptions: TLocaleOptions): AnsiString; overload; inline;
function MatchStr(const AText: AnsiString; const AValues: array of AnsiString): Boolean; overload;
function MatchText(const AText: AnsiString; const AValues: array of AnsiString): Boolean; overload;
function MidBStr(const AText: AnsiString; const AByteStart, AByteCount: Integer): AnsiString;
function MidStr(const AText: AnsiString; const AStart, ACount: Integer): AnsiString; overload;
function NewStr(const S: AnsiString): PAnsiString; deprecated;
function NextCharIndex(const S: AnsiString; Index: Integer): Integer; overload;
function PosEx(const SubStr, S: AnsiString; Offset: Integer = 1): Integer; inline; overload;
function QuotedStr(const S: AnsiString): AnsiString; overload;
function RandomFrom(const AValues: array of AnsiString): AnsiString; overload;
function ReplaceStr(const AText, AFromText, AToText: AnsiString): AnsiString; inline; overload;
function ReplaceText(const AText, AFromText, AToText: AnsiString): AnsiString; inline; overload;
function ReverseString(const AText: AnsiString): AnsiString; overload;
function RightBStr(const AText: AnsiString; const AByteCount: Integer): AnsiString;
function RightStr(const AText: AnsiString; const ACount: Integer): AnsiString; overload;
function SameFileName(const S1, S2: AnsiString): Boolean; inline; overload;
function SameStr(const S1, S2: AnsiString): Boolean; overload;
function SameStr(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions): Boolean; overload;
function SameText(const S1, S2: AnsiString): Boolean; overload;
function SameText(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions): Boolean; overload;
function SearchBuf(Buf: PAnsiChar; BufLen: Integer; SelStart, SelLength: Integer; SearchString: AnsiString; Options: TStringSearchOptions = [soDown]): PAnsiChar; overload;
function StartsStr(const ASubText, AText: AnsiString): Boolean; inline; overload;
function StartsText(const ASubText, AText: AnsiString): Boolean; inline; overload;
function StrBufSize(const Str: PAnsiChar): Cardinal; overload;
function StrByteType(Str: PAnsiChar; Index: Cardinal): TMbcsByteType; overload;
function StrCat(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar; overload;
function StrCharLength(const Str: PAnsiChar): Integer; overload;
function StrComp(const Str1, Str2: PAnsiChar): Integer; overload;
function StrCopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar; overload;
procedure StrDispose(Str: PAnsiChar); overload;
function StrECopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar; overload;
function StrEnd(const Str: PAnsiChar): PAnsiChar; overload;
function StrFmt(Buffer, Format: PAnsiChar; const Args: array of const;  const AFormatSettings: TFormatSettings): PAnsiChar; overload;
function StrFmt(Buffer, Format: PAnsiChar; const Args: array of const): PAnsiChar; overload;
function StrIComp(const Str1, Str2: PAnsiChar): Integer; overload;
function StringReplace(const Source, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString; overload;
function StrLCat(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar; overload;
function StrLComp(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer; overload;
function StrLCopy(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar; overload;
function StrLen(const Str: PAnsiChar): Cardinal; overload;{$IFNDEF LEGACYSTRLEN} inline; {$ENDIF}
function StrLFmt(Buffer: PAnsiChar; MaxBufLen: Cardinal; Format: PAnsiChar; const Args: array of const;  const AFormatSettings: TFormatSettings): PAnsiChar; overload;
function StrLFmt(Buffer: PAnsiChar; MaxBufLen: Cardinal; Format: PAnsiChar; const Args: array of const): PAnsiChar; overload;
function StrLIComp(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer; overload;
function StrLower(Str: PAnsiChar): PAnsiChar; overload;
function StrMove(Dest: PAnsiChar; const Source: PAnsiChar; Count: Cardinal): PAnsiChar; overload;
function StrNew(const Str: PAnsiChar): PAnsiChar; overload;
function StrNextChar(const Str: PAnsiChar): PAnsiChar; inline; overload;
function StrPas(const Str: PAnsiChar): AnsiString; overload;
function StrPCopy(Dest: PAnsiChar; const Source: AnsiString): PAnsiChar; overload;
function StrPLCopy(Dest: PAnsiChar; const Source: AnsiString; MaxLen: Cardinal): PAnsiChar; overload;
function StrPos(const Str1, Str2: PAnsiChar): PAnsiChar; overload;
function StrRScan(const Str: PAnsiChar; Chr: AnsiChar): PAnsiChar; overload;
function StrScan(const Str: PAnsiChar; Chr: AnsiChar): PAnsiChar; overload;
function StrUpper(Str: PAnsiChar): PAnsiChar; overload;
function StuffString(const AText: AnsiString; AStart, ALength: Cardinal; const ASubText: AnsiString): AnsiString; overload;
function TextPos(Str, SubStr: PAnsiChar): PAnsiChar; overload;
function TextToFloat(Buffer: PAnsiChar; var Value;  ValueType: TFloatValue): Boolean; overload; inline;
function TextToFloat(Buffer: PAnsiChar; var Value; ValueType: TFloatValue; const AFormatSettings: TFormatSettings): Boolean; overload;
function Trim(const S: AnsiString): AnsiString; overload;
function TrimLeft(const S: AnsiString): AnsiString; overload;
function TrimRight(const S: AnsiString): AnsiString; overload;
function UpperCase(const S: AnsiString): AnsiString; overload;
function UpperCase(const S: AnsiString; LocaleOptions: TLocaleOptions): AnsiString; overload; inline;

implementation

{$IF SIZEOF(SIZEINT)>SIZEOF(INTEGER)}
Function DoCapSizeInt(SI : SizeInt) : Integer; inline;

begin
  if (SI<0) then
    result:=-1
  else if (SI>0) then
    result:=1
  else
    result:=0;
end;
{$DEFINE CAPSIZEINT:=DoCapSizeInt}
{$ELSE}
{$DEFINE CAPSIZEINT:=}
{$ENDIF}


function AdjustLineBreaks(const S: AnsiString; Style: TTextLineBreakStyle): AnsiString;
var
  Source,Dest: PAnsiChar;
  DestLen: Integer;
  I,J,L: Longint;

begin
  Source:=Pointer(S);
  L:=Length(S);
  DestLen:=L;
  I:=1;
  while (I<=L) do
    begin
    case S[i] of
      #10: if (Style=tlbsCRLF) then
               Inc(DestLen);
      #13: if (Style=tlbsCRLF) then
             if (I<L) and (S[i+1]=#10) then
               Inc(I)
             else
               Inc(DestLen)
             else if (I<L) and (S[I+1]=#10) then
               Dec(DestLen);
    end;
    Inc(I);
    end;
  if (DestLen=L) then
    Result:=S
  else
    begin
    SetLength(Result, DestLen);
    FillChar(Result[1],DestLen,0);
    Dest := Pointer(Result);
    J:=0;
    I:=0;
    While I<L do
      case Source[I] of
        #10: begin
             if Style=tlbsCRLF then
               begin
               Dest[j]:=#13;
               Inc(J);
              end;
             Dest[J] := #10;
             Inc(J);
             Inc(I);
             end;
        #13: begin
             if Style=tlbsCRLF then
               begin
               Dest[j] := #13;
               Inc(J);
               end;
             Dest[j]:=#10;
             Inc(J);
             Inc(I);
             if Source[I]=#10 then
               Inc(I);
             end;
      else
        Dest[j]:=Source[i];
        Inc(J);
        Inc(I);
      end;
    end;
end;

function AnsiCompareStr(const S1, S2: AnsiString): Integer;
begin
  result:=CAPSIZEINT(widestringmanager.CompareStrAnsiStringProc(s1,s2));
end;


function AnsiCompareText(const S1, S2: AnsiString): Integer;
begin
  result:=CAPSIZEINT(widestringmanager.CompareTextAnsiStringProc(s1,s2));
end;

function AnsiCompareFileName(const S1, S2: AnsiString): Integer;
begin
  If FileNameCaseSensitive then
    Result:=AnsiCompareStr(S1,S2) // Compare case sensitive
  else
    Result:=AnsiCompareText(S1,S2); // Compare case insensitive. No MBCS yet.
end;


function AnsiContainsStr(const AText, ASubText: AnsiString): Boolean;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.AnsiContainsStr(aText,aSubText);
end;

function AnsiContainsText(const AText, ASubText: AnsiString): Boolean;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.AnsiContainsText(aText,aSubText);
end;

function AnsiDequotedStr(const S: AnsiString; AQuote: AnsiChar): AnsiString;

var
  p : PAnsiChar;
begin
  p:=PAnsiChar(Pointer(s)); // work around CONST. Ansiextract is safe for nil
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.AnsiExtractquotedStr(P,AQuote);
end;

function AnsiEndsStr(const ASubText, AText: AnsiString): Boolean;

begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.AnsiEndsStr(aSubText,aText);
end;

function AnsiEndsText(const ASubText, AText: AnsiString): Boolean;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.AnsiEndsText(aSubText,aText);
end;

function AnsiExtractQuotedStr(var Src: PAnsiChar; Quote: AnsiChar): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.AnsiExtractQuotedStr(Src,Quote);
end;

function AnsiFormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const): Cardinal;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.FormatBuf(Buffer,BufLen,Format,FmtLen,Args);
end;

function AnsiFormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const;
  const AFormatSettings: TFormatSettings): Cardinal;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.FormatBuf(Buffer,BufLen,Format,FmtLen,Args,AFormatSettings);
end;

function AnsiIndexStr(const AText: AnsiString;
  const AValues: array of AnsiString): Integer;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.AnsiIndexStr(aText,aValues);
end;

function AnsiIndexText(const AText: AnsiString;
  const AValues: array of AnsiString): Integer;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.AnsiIndexText(aText,aValues);
end;

function AnsiLastChar(const S: AnsiString): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.AnsiLastChar(S);
end;

function AnsiLeftStr(const AText: AnsiString; const ACount: Integer
  ): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.LeftStr(aText,aCount);
end;

function AnsiLowerCase(const S: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.LowerCase(S);
end;

function AnsiLowerCaseFileName(const S: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.LowerCase(S);
end;

function AnsiMatchStr(const AText: AnsiString;
  const AValues: array of AnsiString): Boolean;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.AnsiMatchStr(aText,aValues);
end;

function AnsiMatchText(const AText: AnsiString;
  const AValues: array of AnsiString): Boolean;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.AnsiMatchText(aText,aValues);
end;

function AnsiMidStr(const AText: AnsiString; const AStart, ACount: Integer
  ): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.MidStr(aText,aStart,aCount);
end;

function AnsiPos(const Substr, S: AnsiString): Integer;
begin
  Result:=System.Pos(Substr,S);
end;

function AnsiQuotedStr(const S: AnsiString; Quote: AnsiChar): AnsiString;

var
  i, j, count: integer;

begin
  Result:='';
  count:=length(s);
  i:=0;
  j:=0;
  while (i<count) do
    begin
    Inc(I);
    if (S[i]=Quote) then
      begin
      Result:=Result+Copy(S,1+j,i-j)+Quote;
      j:=i;
      end ;
    end ;
  if i<>j then
    Result:=Result+copy(S,1+j,i-j);
  Result:=Result+Quote;
end;

function AnsiReplaceStr(const AText, AFromText, AToText: AnsiString
  ): AnsiString;
begin
  Result := StringReplace(AText,AFromText,AToText,[rfReplaceAll]);
end;

function AnsiReplaceText(const AText, AFromText, AToText: AnsiString
  ): AnsiString;
begin
  Result := StringReplace(AText,AFromText,AToText,[rfReplaceAll,rfIgnoreCase]);
end;

function AnsiReverseString(const AText: AnsiString): AnsiString;
var
  i,j : SizeInt;
begin
  result:='';
  j:=Length(atext);
  setLength(Result,j);
  i:=1;
  while (i<=j) do
    begin
    Result[i]:=atext[j-i+1];
    inc(i);
    end;
end;

function AnsiRightStr(const AText: AnsiString; const ACount: Integer
  ): AnsiString;
begin
  Result:=RightStr(aText,aCount);
end;

function AnsiSameStr(const S1, S2: AnsiString): Boolean;
begin
  Result:=SameStr(S1,S2);
end;

function AnsiSameText(const S1, S2: AnsiString): Boolean;
begin
  Result:=SameText(S1,S2);
end;

function AnsiStartsStr(const ASubText, AText: AnsiString): Boolean;
begin
  if (Length(AText) >= Length(ASubText)) and (ASubText <> '') then
    Result := StrLComp(PChar(ASubText), PAnsiChar(AText), Length(ASubText)) = 0
  else
    Result := (AsubText='');
end;

function AnsiStartsText(const ASubText, AText: AnsiString): Boolean;
begin
  Result:=(ASubText = '') or AnsiSameText(LeftStr(AText,Length(ASubText)), ASubText);
end;

function AnsiStrAlloc(Size: Cardinal): PAnsiChar;
begin
  Result:=StrAlloc(Size);
end;

function AnsiStrComp(S1, S2: PAnsiChar): Integer;
begin
  result:=CAPSIZEINT(widestringmanager.StrCompAnsiStringProc(s1,s2));
end;

function AnsiStrIComp(S1, S2: PAnsiChar): Integer;
begin
  result:=CAPSIZEINT(widestringmanager.StrICompAnsiStringProc(s1,s2));
end;

function AnsiStrLastChar(P: PAnsiChar): PAnsiChar;
begin
  result:=StrEnd(P);
  Dec(Result);
end;

function AnsiStrLComp(S1, S2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
  result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Sysutils.AnsiStrLComp(S1,S2,MaxLen);
end;

function AnsiStrLIComp(S1, S2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
  result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Sysutils.AnsiStrLIComp(S1,S2,MaxLen);
end;

function AnsiStrLower(Str: PAnsiChar): PAnsiChar;
begin
  result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Sysutils.AnsiStrLower(Str);
end;

function AnsiStrPos(Str, SubStr: PAnsiChar): PAnsiChar;
begin
  result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Sysutils.AnsiStrPos(Str,SubStr);
end;

function AnsiStrRScan(Str: PAnsiChar; Chr: AnsiChar): PAnsiChar;
begin
  result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Sysutils.AnsiStrRScan(Str,Chr);
end;

function AnsiStrScan(Str: PAnsiChar; Chr: AnsiChar): PAnsiChar;
begin
  result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Sysutils.AnsiStrScan(Str,Chr);
end;

function AnsiStrUpper(Str: PAnsiChar): PAnsiChar;
begin
  result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Sysutils.AnsiStrUpper(Str);
end;

function AnsiUpperCase(const S: AnsiString): AnsiString;
begin
  result:=widestringmanager.UpperAnsiStringProc(s);
end;

function AnsiUpperCaseFileName(const S: AnsiString): AnsiString;
begin
  result:=AnsiUpperCase(S);
end;

procedure AppendStr(var Dest: AnsiString; const S: AnsiString);
begin
  Dest:=Dest+S;
end;

procedure AssignStr(var P: PAnsiString; const S: AnsiString);
begin
  P^:=S;
end;

function ByteToCharIndex(const S: AnsiString; Index: Integer): Integer;
begin
  if (Index>0) and (Index<=Length(S)) then
    result:=Index
  else
    Result:=0;
end;

function ByteToCharLen(const S: AnsiString; MaxLen: Integer): Integer;
begin
  result:=Length(S);
  if Result>MaxLen then
    Result:=MaxLen;
end;

function ByteType(const S: AnsiString; Index: Integer): TMbcsByteType;
begin
  if (Index>0) and (Index<=Length(S)) then
    Result:=mbSingleByte
  else
    Result:=mbSingleByte;
end;

function ChangeFileExt(const FileName, Extension: AnsiString): AnsiString;
begin
  result:=AnsiString({$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Sysutils.ChangeFileExt(RawByteString(FileName),RawByteString(Extension)));
end;

function ChangeFilePath(const FileName, Path: AnsiString): AnsiString;
begin
  Result:=IncludeTrailingPathDelimiter(Path)+ExtractFileName(FileName);
end;

function CharLength(const S: AnsiString; Index: Integer): Integer;
begin
  if (Index>0) and (Index<=Length(S)) then
    Result:=1
  else
    Result:=0;
end;

function CharToByteIndex(const S: AnsiString; Index: Integer): Integer;
begin
  if (Index>0) and (Index<=Length(S)) then
    Result:=Index
  else
    Result:=0;
end;

function CharToByteLen(const S: AnsiString; MaxLen: Integer): Integer;
begin
  result:=Length(S);
  if Result>MaxLen then
    Result:=MaxLen;
end;

function CharToElementIndex(const S: AnsiString; Index: Integer): Integer;
begin
  if (Index>0) and (Index<=Length(S)) then
    Result:=Index
  else
    Result:=0;
end;

function CharToElementLen(const S: AnsiString; MaxLen: Integer): Integer;
begin
  result:=Length(S);
  if Result>MaxLen then
    Result:=MaxLen;
end;

function CompareStr(const S1, S2: AnsiString): Integer;

var
  Count, Count1,Count2 : Integer;

begin
  Count1 := Length(S1);
  Count2 := Length(S2);
  if Count1>Count2 then
    Count:=Count2
  else
    Count:=Count1;
  result:=CompareMemRange(Pointer(S1),Pointer(S2),Count);
  if result=0 then
    // CAPSIZEINT is no-op if Sizeof(Sizeint)<=SizeOF(Integer)
    result:=CAPSIZEINT(Count1-Count2);
end;

function CompareStr(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions
  ): Integer;
begin
  case LocaleOptions of
    loInvariantLocale: Result:=CompareStr(S1,S2);
    loUserLocale: Result:=AnsiCompareStr(S1,S2);
  end;
end;

function CompareText(const S1, S2: AnsiString): Integer;
var
  i, count, count1, count2: sizeint;
  Chr1, Chr2: byte;
  P1, P2: PChar;
begin
  Count1 := Length(S1);
  Count2 := Length(S2);
  if (Count1>Count2) then
    Count := Count2
  else
    Count := Count1;
  i := 0;
  if count>0 then
    begin
      P1 := @S1[1];
      P2 := @S2[1];
      while i < Count do
        begin
          Chr1 := byte(p1^);
          Chr2 := byte(p2^);
          if Chr1 <> Chr2 then
            begin
              if Chr1 in [97..122] then
                dec(Chr1,32);
              if Chr2 in [97..122] then
                dec(Chr2,32);
              if Chr1 <> Chr2 then
                Break;
            end;
          Inc(P1); Inc(P2); Inc(I);
        end;
    end;
  if i < Count then
    result := Chr1-Chr2
  else
    // CAPSIZEINT is no-op if Sizeof(Sizeint)<=SizeOF(Integer)
    result:=CAPSIZEINT(Count1-Count2);
end;

function CompareText(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions
  ): Integer;
begin
  case LocaleOptions of
    loInvariantLocale: Result:=CompareText(S1,S2);
    loUserLocale: Result:=AnsiCompareText(S1,S2);
  end;
end;

function ContainsStr(const AText, ASubText: AnsiString): Boolean;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.AnsiContainsStr(aText,aSubText);
end;

function ContainsText(const AText, ASubText: AnsiString): Boolean;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.AnsiContainsText(aText,aSubText);
end;

procedure DisposeStr(P: PAnsiString);
begin
  if (P <> nil) and (P^ <> '') then
    Dispose(P);
end;

function DupeString(const AText: AnsiString; ACount: Integer): AnsiString;

var
  ResLen, Rp, ToCopy: SizeInt;

begin
  if (AText = '') or (ACount <= 0) then
    Exit('');
  if ACount = 1 then
    Exit(AText);

  Rp := Length(AText);
  ResLen := ACount * Rp;
  SetLength(Result, ResLen);
  Move(Pointer(AText)^, Pointer(Result)^, Rp * SizeOf(AText[1]));

  repeat
    ToCopy := ResLen - Rp;
    if Rp < ToCopy then
      ToCopy := Rp;
    Move(Pointer(Result)^, PAnsiChar(Pointer(Result))[Rp], ToCopy * SizeOf(AText[1]));
    Inc(Rp, ToCopy);
  until Rp = ResLen;
end;

function ElementToCharIndex(const S: AnsiString; Index: Integer): Integer;
begin
  if (Index>0) and (Index<=Length(S)) then
    Result:=Index
  else
    Result:=0;
end;

function ElementToCharLen(const S: AnsiString; MaxLen: Integer): Integer;
begin
  Result:=Length(S);
  if Result>MaxLen then
    Result:=MaxLen;
end;

function EndsStr(const ASubText, AText: AnsiString): Boolean;
begin
  Result:=Length(AText) >= Length(ASubText);
  if Result then
    Result := StrLComp(PAnsiChar(ASubText), PAnsiChar(AText)+Length(AText)-Length(ASubText),Length(ASubText))=0
end;

function EndsText(const ASubText, AText: AnsiString): Boolean;
begin
  Result:=Length(AText) >= Length(ASubText);
  if Result then
    Result := StrLIComp(PAnsiChar(ASubText), PAnsiChar(AText)+Length(AText)-Length(ASubText),Length(ASubText))=0
end;

function ExcludeTrailingBackslash(const S: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.ExcludeLeadingPathDelimiter(S);
end;

function ExcludeTrailingPathDelimiter(const S: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.ExcludeLeadingPathDelimiter(S);
end;

function ExpandFileNameCase(const FileName: AnsiString; out
  MatchFound: TFilenameCaseMatch): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.ExpandFileNameCase(FileName,MatchFound);
end;

function ExpandFileName(const FileName: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.ExpandFileName(FileName);
end;

function ExpandUNCFileName(const FileName: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.ExpandUNCFileName(FileName);
end;

function ExtractFileDir(const FileName: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.ExtractFileDir(FileName);
end;

function ExtractFileDrive(const FileName: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.ExtractFileDrive(FileName);
end;

function ExtractFileExt(const FileName: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.ExtractFileExt(FileName);
end;

function ExtractFileName(const FileName: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.ExtractFileName(FileName);
end;

function ExtractFilePath(const FileName: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.ExtractFilePath(FileName);
end;

function ExtractRelativePath(const BaseName, DestName: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.ExtractRelativePath(BaseName,DestName);
end;

Type
  PReal = ^Real;

function FloatToText(BufferArg: PAnsiChar; const Value; ValueType: TFloatValue;
  Format: TFloatFormat; Precision, Digits: Integer;
  const AFormatSettings: TFormatSettings): Integer;

Var
  E : Extended;

begin
  Case ValueType of
    fvComp : E:=PComp(@Value)^;
    fvExtended : E:=PExtended(@Value)^;
    fvDouble: E:=PDouble(@Value)^;
    fvReal: E:=PReal(@Value)^;
    fvSingle : E:=PSingle(@Value)^;
    fvCurrency : E:=PCurrency(@Value)^;
  end;
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.FloatToText(BufferArg,E,Format,Precision,Digits,AFormatSettings);
end;

function FloatToText(BufferArg: PAnsiChar; const Value; ValueType: TFloatValue;
  Format: TFloatFormat; Precision, Digits: Integer): Integer;
begin
  Result:=FloatToText(BufferArg,Value,ValueType,Format,Precision,Digits,DefaultFormatSettings);
end;

function FloatToTextFmt(Buf: PAnsiChar; const Value; ValueType: TFloatValue;
  Format: PAnsiChar; const AFormatSettings: TFormatSettings): Integer;
Var
  E : Extended;

begin
  Case ValueType of
    fvComp : E:=PComp(@Value)^;
    fvExtended : E:=PExtended(@Value)^;
    fvDouble: E:=PDouble(@Value)^;
    fvReal: E:=PReal(@Value)^;
    fvSingle : E:=PSingle(@Value)^;
    fvCurrency : E:=PCurrency(@Value)^;
  end;
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.FloatToTextFmt(Buf,E,Format,aFormatSettings);
end;

function FloatToTextFmt(Buf: PAnsiChar; const Value; ValueType: TFloatValue;
  Format: PAnsiChar): Integer;
begin
  Result:=FloatToTextFmt(Buf,Value,ValueType,Format,DefaultFormatSettings);
end;

procedure FmtStr(var aResult: AnsiString; const Format: AnsiString;
  const Args: array of const; const AFormatSettings: TFormatSettings);
var
  S : String;
begin
  S:=aResult;
  {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.FmtStr(S,Format,Args,aFormatSettings);
  aResult:=S;
end;

procedure FmtStr(var aResult: AnsiString; const Format: AnsiString;
  const Args: array of const);
var
  S : String;
begin
  S:=aResult;
  {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.FmtStr(S,Format,Args,DefaultFormatSettings);
  aResult:=S;
end;

function FormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const): Cardinal;

begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.FormatBuf(Buffer,BufLen,Format,FmtLen,args);
end;

function FormatBuf(var Buffer; BufLen: Cardinal; const Format;
  FmtLen: Cardinal; const Args: array of const;
  const AFormatSettings: TFormatSettings): Cardinal;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.FormatBuf(Buffer,BufLen,Format,FmtLen,Args, aFormatSettings);
end;

function Format(const aFormat: AnsiString; const Args: array of const): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.Format(aFormat,Args);
end;

function Format(const aFormat: AnsiString; const Args: array of const;
  const AFormatSettings: TFormatSettings): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.Format(aFormat,Args,aFormatSettings);
end;

function IncludeTrailingBackslash(const S: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.IncludeTrailingBackslash(S);
end;

function IncludeTrailingPathDelimiter(const S: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.IncludeTrailingPathDelimiter(S);
end;

function IndexStr(const AText: AnsiString; const AValues: array of AnsiString
  ): Integer;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.IndexStr(aText,aValues);
end;

function IndexText(const AText: AnsiString; const AValues: array of AnsiString
  ): Integer;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.IndexText(aText,aValues);
end;

function IsDelimiter(const Delimiters, S: AnsiString; Index: Integer): Boolean;
begin
  Result:=False;
  If (Index>0) and (Index<=Length(S)) then
    Result:=Pos(S[Index],Delimiters)<>0; // Note we don't do MBCS yet
end;

function IsPathDelimiter(const S: AnsiString; Index: Integer): Boolean;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.IsPathDelimiter(S,Index);
end;

function IsValidIdent(const Ident: AnsiString; AllowDots: Boolean; StrictDots : Boolean = False): Boolean;

const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNum = Alpha + ['0'..'9'];
  Dot = '.';
var
  First: Boolean;
  I, Len: Integer;
begin
  Len := Length(Ident);
  if Len < 1 then
    Exit(False);
  First := True;
  for I := 1 to Len do
  begin
    if First then
    begin
      Result := Ident[I] in Alpha;
      First := False;
    end
    else if AllowDots and (Ident[I] = Dot) then
    begin
      if StrictDots then
      begin
        Result := I < Len;
        First := True;
      end;
    end
    else
      Result := Ident[I] in AlphaNum;
    if not Result then
      Break;
  end;
end;

function LastDelimiter(const Delimiters, S: AnsiString): Integer;
var
  chs: Set of AnsiChar;
  I: SizeInt;

begin
  chs := [];
  for I := 1 to Length(Delimiters) do
    Include(chs, Delimiters[I]);
  Result:=Length(S);
  While (Result>0) and not (S[Result] in chs) do
    Dec(Result);
end;

function LeftBStr(const AText: AnsiString; const AByteCount: Integer
  ): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.LeftBStr(aText,aByteCount);
end;

function LeftStr(const AText: AnsiString; const ACount: Integer): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.LeftStr(aText,aCount);
end;

function LowerCase(const S: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.LowerCase(S);
end;

function LowerCase(const S: AnsiString; LocaleOptions: TLocaleOptions): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.LowerCase(S,LocaleOptions);
end;

function MatchStr(const AText: AnsiString; const AValues: array of AnsiString
  ): Boolean;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.MatchStr(aText,aValues);
end;

function MatchText(const AText: AnsiString; const AValues: array of AnsiString
  ): Boolean;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.MatchText(aText,aValues);
end;

function MidBStr(const AText: AnsiString; const AByteStart, AByteCount: Integer
  ): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.MidBStr(aText,aByteStart,aByteCount);
end;

function MidStr(const AText: AnsiString; const AStart, ACount: Integer
  ): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.MidStr(aText,aStart,aCount);
end;

function NewStr(const S: AnsiString): PAnsiString;
begin
  if (S='') then
   Result:=nil
  else
   begin
     new(result);
     if (Result<>nil) then
       Result^:=s;
   end;
end;

function NextCharIndex(const S: AnsiString; Index: Integer): Integer;
begin
  if Index<Length(S) then
    Result:=Index+1
  else
    Result:=0;
end;

function PosEx(const SubStr, S: AnsiString; Offset: Integer): Integer;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.PosEx(SubStr,S,Offset);
end;

function QuotedStr(const S: AnsiString): AnsiString;

begin
  Result:=AnsiQuotedStr(S,'"');
end;

function RandomFrom(const AValues: array of AnsiString): AnsiString;
begin
  if high(AValues)=-1 then exit('');
  result:=Avalues[random(High(AValues)+1)];
end;

function ReplaceStr(const AText, AFromText, AToText: AnsiString): AnsiString;
begin
  Result:= {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StringReplace(AText,AFromText,AToText,[rfReplaceAll]);
end;

function ReplaceText(const AText, AFromText, AToText: AnsiString): AnsiString;
begin
  Result:= {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StringReplace(AText,AFromText,AToText,[rfReplaceAll,rfIgnoreCase]);
end;

function ReverseString(const AText: AnsiString): AnsiString;


begin
  Result:=AnsiReverseString(aText);
end;

function RightBStr(const AText: AnsiString; const AByteCount: Integer
  ): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.RightBStr(aText,aByteCount);
end;

function RightStr(const AText: AnsiString; const ACount: Integer): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.RightStr(aText,aCount);
end;

function SameFileName(const S1, S2: AnsiString): Boolean;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.SameFileName(S1,S2);
end;

function SameStr(const S1, S2: AnsiString): Boolean;
begin
  Result:=CompareStr(S1,S2)=0;
end;

function SameStr(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions
  ): Boolean;
begin
  Result:=CompareStr(S1,S2,LocaleOptions)=0;
end;

function SameText(const S1, S2: AnsiString): Boolean;
begin
  Result:=CompareText(S1,S2)=0;
end;

function SameText(const S1, S2: AnsiString; LocaleOptions: TLocaleOptions
  ): Boolean;
begin
  Result:=CompareText(S1,S2,LocaleOptions)=0;
end;

function SearchBuf(Buf: PAnsiChar; BufLen: Integer; SelStart,
  SelLength: Integer; SearchString: AnsiString; Options: TStringSearchOptions
  ): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}StrUtils.SearchBuf(Buf,BufLen,SelStart,SelLength,SearchString,Options);
end;

function StartsStr(const ASubText, AText: AnsiString): Boolean;
begin
  if (Length(AText) >= Length(ASubText)) and (ASubText <> '') then
    Result := StrLComp(PAnsiChar(ASubText), PAnsiChar(AText), Length(ASubText)) = 0
  else
    Result := (AsubText='');
end;

function StartsText(const ASubText, AText: AnsiString): Boolean;
begin
  Result:=AnsiStartsText(aSubText,aText);
end;

function StrBufSize(const Str: PAnsiChar): Cardinal;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrBufSize(Str);
end;

function StrByteType(Str: PAnsiChar; Index: Cardinal): TMbcsByteType;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrByteType(Str,Index);
end;

function StrCat(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrCat(Dest,Source);
end;

function StrCharLength(const Str: PAnsiChar): Integer;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrCharLength(Str);
end;

function StrComp(const Str1, Str2: PAnsiChar): Integer;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrComp(Str1,Str2);
end;

function StrCopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrCopy(Dest,Source);
end;

procedure StrDispose(Str: PAnsiChar);
begin
  {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrDispose(Str);
end;

function StrECopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrECopy(Dest,Source);
end;

function StrEnd(const Str: PAnsiChar): PAnsiChar;

begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrEnd(Str);
end;

function StrLen(const Str: PAnsiChar): Cardinal;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrLen(Str);
end;

function StrFmt(Buffer, Format: PAnsiChar; const Args: array of const;
  const AFormatSettings: TFormatSettings): PAnsiChar;

Var
  Len : Integer;
begin
  Len:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.FormatBuf(Buffer^,Maxint,Format^,strlen(Format),args,aFormatSettings);
  Buffer[Len]:=#0;
  Result:=Buffer;
end;

function StrFmt(Buffer, Format: PAnsiChar; const Args: array of const): PAnsiChar;

Var
  Len : Integer;
begin
  Len:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.FormatBuf(Buffer^,Maxint,Format^,strlen(Format),args);
  Buffer[Len]:=#0;
  Result:=Buffer;
end;

function StrIComp(const Str1, Str2: PAnsiChar): Integer;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrIComp(Str1,Str2);
end;

function StringReplace(const Source, OldPattern, NewPattern: AnsiString;
  Flags: TReplaceFlags): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StringReplace(Source,OldPattern,NewPattern,Flags);
end;

function StrLCat(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal
  ): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrLCat(Dest,Source,MaxLen);
end;

function StrLComp(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrLComp(str1,str2,MaxLen);
end;

function StrLCopy(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal
  ): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrLCopy(Dest,Source,MaxLen);
end;


function StrLFmt(Buffer: PAnsiChar; MaxBufLen: Cardinal; Format: PAnsiChar;
  const Args: array of const; const AFormatSettings: TFormatSettings
  ): PAnsiChar;
var
  Len : integer;

begin
  Len:=FormatBuf(Buffer^,MaxBufLen,Format^,strlen(Format),args,aFormatSettings);
  Buffer[Len]:=#0;
  Result:=Buffer;
end;

function StrLFmt(Buffer: PAnsiChar; MaxBufLen: Cardinal; Format: PAnsiChar;
  const Args: array of const): PAnsiChar;
begin
  Result:=StrLFmt(Buffer,MaxBufLen,Format,Args,DefaultFormatSettings);
end;

function StrLIComp(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrLIComp(Str1,Str2,MaxLen);
end;

function StrLower(Str: PAnsiChar): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrLower(Str);
end;

function StrMove(Dest: PAnsiChar; const Source: PAnsiChar; Count: Cardinal
  ): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrMove(Dest,Source,Count);
end;

function StrNew(const Str: PAnsiChar): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrNew(Str);
end;

function StrNextChar(const Str: PAnsiChar): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrNextChar(Str);
end;

function StrPas(const Str: PAnsiChar): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrNextChar(Str);
end;

function StrPCopy(Dest: PAnsiChar; const Source: AnsiString): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrPCopy(Dest,Source);
end;

function StrPLCopy(Dest: PAnsiChar; const Source: AnsiString; MaxLen: Cardinal
  ): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrPLCopy(Dest,Source,MaxLen);
end;

function StrPos(const Str1, Str2: PAnsiChar): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrPos(Str1,Str2);
end;

function StrRScan(const Str: PAnsiChar; Chr: AnsiChar): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrRScan(Str,Chr);
end;

function StrScan(const Str: PAnsiChar; Chr: AnsiChar): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrScan(Str,Chr);
end;

function StrUpper(Str: PAnsiChar): PAnsiChar;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.StrUpper(Str);
end;

function StuffString(const AText: AnsiString; AStart, ALength: Cardinal;  const ASubText: AnsiString): AnsiString;

var i,j,k : SizeUInt;

begin
  Result:='';
  j:=length(ASubText);
  i:=length(AText);
  if AStart>i then
    aStart:=i+1;
  k:=i+1-AStart;
  if ALength> k then
    ALength:=k;
  SetLength(Result,i+j-ALength);
  move (AText[1],result[1],AStart-1);
  move (ASubText[1],result[AStart],j);
  move (AText[AStart+ALength], Result[AStart+j],i+1-AStart-ALength);
end;

function TextPos(Str, SubStr: PAnsiChar): PAnsiChar;

var
  R,L,LS : PAnsiChar;
begin
  Result:=Nil;
  L:=nil;
  LS:=System.AnsiStrings.StrLower(System.AnsiStrings.StrNew(SubStr));
  try
    L:=System.AnsiStrings.StrLower(System.AnsiStrings.StrNew(Str));
    R:=System.AnsiStrings.StrPos(L,LS);
    if Assigned(R) then
      Result:=PAnsiChar(Str+(R-LS));
  finally
    StrDispose(L);
    StrDispose(LS);
  end;
end;

function TextToFloat(Buffer: PAnsiChar; var Value; ValueType: TFloatValue
  ): Boolean;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.TextToFloat(Buffer,Value,ValueType);
end;

function TextToFloat(Buffer: PAnsiChar; var Value; ValueType: TFloatValue;
  const AFormatSettings: TFormatSettings): Boolean;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.TextToFloat(Buffer,Value,ValueType,aFormatSettings);
end;

function Trim(const S: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.Trim(S);
end;

function TrimLeft(const S: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.TrimLeft(S);
end;

function TrimRight(const S: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.TrimRight(S);
end;

function UpperCase(const S: AnsiString): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.UpperCase(S);
end;

function UpperCase(const S: AnsiString; LocaleOptions: TLocaleOptions
  ): AnsiString;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.UpperCase(S,LocaleOptions);
end;

function ExtractShortPathName(const FileName: AnsiString): AnsiString; overload;

begin
  Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.ExtractShortPathName(FileName);
end;

end.
