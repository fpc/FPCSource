{
    This file is part of the Free Component Library

    JSON source lexical scanner
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

unit jsonscanner;

interface

uses SysUtils, Classes;

resourcestring
  SErrInvalidCharacter = 'Invalid character at line %d, pos %d: ''%s''';
  SUnterminatedComment = 'Unterminated comment at line %d, pos %d: ''%s''';
  SErrOpenString = 'string exceeds end of line %d';

type

  TJSONToken = (
    tkEOF,
    tkWhitespace,
    tkString,
    tkNumber,
    tkTrue,
    tkFalse,
    tkNull,
    // Simple (one-character) tokens
    tkComma,                 // ','
    tkColon,                 // ':'
    tkCurlyBraceOpen,        // '{'
    tkCurlyBraceClose,       // '}'
    tkSquaredBraceOpen,       // '['
    tkSquaredBraceClose,      // ']'
    tkIdentifier,            // Any Javascript identifier
    tkComment,
    tkUnknown
    );

  EScannerError = class(EParserError);

  TJSONOption = (joUTF8,joStrict,joComments,joIgnoreTrailingComma);
  TJSONOptions = set of TJSONOption;

Const
  DefaultOptions = [joUTF8];

Type

  { TJSONScanner }

  TJSONScanner = class
  private
    FSource: RawByteString;
    FCurPos : PAnsiChar; // Position inside total string
    FCurRow: Integer;
    FCurToken: TJSONToken;
    FCurTokenString: string;
    FCurLine: string;
    FTokenStr:  PAnsiChar; // position inside FCurLine
    FOptions : TJSONOptions;
    function GetCurColumn: Integer; inline;
    function GetO(AIndex: TJSONOption): Boolean;
    procedure SetO(AIndex: TJSONOption; AValue: Boolean);
  protected
    procedure Error(const Msg: string);overload;
    procedure Error(const Msg: string;  Const Args: array of const);overload;
    function DoFetchToken: TJSONToken; inline;
  public
    constructor Create(Source : TStream; AUseUTF8 : Boolean = True); overload; deprecated 'use options form instead';
    constructor Create(Source: TStream; AOptions: TJSONOptions); overload;
    constructor Create(const aSource : RawByteString; AUseUTF8 : Boolean = True); overload; deprecated  'use options form instead';
    constructor Create(const aSource: RawByteString; AOptions: TJSONOptions); overload;
    function FetchToken: TJSONToken;


    property CurLine: string read FCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;

    property CurToken: TJSONToken read FCurToken;
    property CurTokenString: string read FCurTokenString;
    // Use strict JSON: " for strings, object members are strings, not identifiers
    Property Strict : Boolean Index joStrict Read GetO Write SetO ; deprecated 'use options instead';
    // if set to TRUE, then strings will be converted to UTF8 ansistrings, not system codepage ansistrings.
    Property UseUTF8 : Boolean index joUTF8 Read GetO Write SetO; deprecated 'Use options instead';
    // Parsing options
    Property Options : TJSONOptions Read FOptions Write FOptions;
  end;

const
  TokenInfos: array[TJSONToken] of string = (
    'EOF',
    'Whitespace',
    'String',
    'Number',
    'True',
    'False',
    'Null',
    ',',
    ':',
    '{',
    '}',
    '[',
    ']',
    'identifier',
    'comment',
    ''
  );


implementation

constructor TJSONScanner.Create(Source : TStream; AUseUTF8 : Boolean = True);

Var
  O : TJSONOptions;

begin
  O:=DefaultOptions;
  if AUseUTF8 then
    Include(O,joUTF8)
  else
    Exclude(O,joUTF8);
  Create(Source,O);
end;

constructor TJSONScanner.Create(Source: TStream; AOptions: TJSONOptions);

Var
  S : RawByteString;

begin
  S:='';
  SetLength(S,Source.Size);
  if Length(S)>0 then
    Source.ReadBuffer(S[1],Length(S));
  Create(S,AOptions)
end;

constructor TJSONScanner.Create(const aSource : RawByteString; AUseUTF8 : Boolean = True);
Var
  O : TJSONOptions;

begin
  O:=DefaultOptions;
  if AUseUTF8 then
    Include(O,joUTF8)
  else
    Exclude(O,joUTF8);
  Create(aSource,O);
end;

constructor TJSONScanner.Create(const aSource: RawByteString; AOptions: TJSONOptions);
begin
  FSource:=aSource;
  FCurPos:=PAnsiChar(FSource);
  FOptions:=AOptions;
end;

function TJSONScanner.FetchToken: TJSONToken;
  
begin
  Result:=DoFetchToken;
end;

procedure TJSONScanner.Error(const Msg: string);
begin
  raise EScannerError.Create(Msg);
end;

procedure TJSONScanner.Error(const Msg: string; const Args: array of const);
begin
  raise EScannerError.CreateFmt(Msg, Args);
end;

function TJSONScanner.DoFetchToken: TJSONToken;

  function FetchLine: Boolean;

  var
    PEOL : PAnsiChar;
    Len : integer;

  begin
    Result:=(FCurPos<>Nil) and (FCurPos^<>#0);
    if Result then
      begin
      FTokenStr:=FCurPos;
      While Not (FCurPos^ in [#0,#10,#13]) do
        Inc(FCurPos);
      PEOL:=FCurPos;
      if (FCurPos^<>#0) then
        begin
        if (FCurPos^=#13) and (FCurPos[1]=#10) then
          Inc(FCurPos); // Skip CR-LF
        Inc(FCurPos); // To start of next line
        Inc(FCurRow); // Increase line index
        end;
      Len:=PEOL-FTokenStr;
      SetLength(FCurLine,Len);
      if Len>0 then
        Move(FTokenStr^,FCurLine[1],Len);
      FTokenStr:=PAnsiChar(FCurLine);
      end
    else             
      begin
      FCurLine:='';
      FTokenStr:=nil;
      end;
  end;

var
  TokenStart: PChar;
  it : TJSONToken;
  I : Integer;
  OldLength, SectionLength,  tstart,tcol, u1,u2: Integer;
  C , c2: char;
  S : String;
  IsStar,EOC: Boolean;

  Procedure MaybeAppendUnicode;

  Var
    u : UTF8String;

  begin
  // if there is a leftover \u, append
  if (u1<>0) then
    begin
    if (joUTF8 in Options) or (DefaultSystemCodePage=CP_UTF8) then
      U:=Utf8Encode(WideString(WideChar(u1))) // ToDo: use faster function
    else
      U:=String(WideChar(u1)); // WideChar converts the encoding. Should it warn on loss?
    FCurTokenString:=FCurTokenString+U;
    OldLength:=Length(FCurTokenString);
    u1:=0;
    end;
  end;


begin
  if FTokenStr = nil then
    if not FetchLine then
      begin
      Result := tkEOF;
      FCurToken := Result;
      exit;
      end;

  FCurTokenString := '';

  case FTokenStr[0] of
    #0:         // Empty line
      begin
      FetchLine;
      Result := tkWhitespace;
      end;
    #9, ' ':
      begin
      Result := tkWhitespace;
      repeat
        Inc(FTokenStr);
        if FTokenStr[0] = #0 then
          if not FetchLine then
          begin
            FCurToken := Result;
            exit;
          end;
      until not (FTokenStr[0] in [#9, ' ']);
      end;
    '"','''':
      begin
        C:=FTokenStr[0];
        If (C='''') and (joStrict in Options) then
          Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
        Inc(FTokenStr);
        TokenStart := FTokenStr;
        OldLength := 0;
        FCurTokenString := '';
        u1:=0;
        while not (FTokenStr[0] in [#0,C]) do
          begin
          if (FTokenStr[0]='\') then
            begin
            // Save length
            SectionLength := FTokenStr - TokenStart;
            Inc(FTokenStr);
            // Read escaped token
            Case FTokenStr[0] of
              '"' : S:='"';
              '''' : S:='''';
              't' : S:=#9;
              'b' : S:=#8;
              'n' : S:=#10;
              'r' : S:=#13;
              'f' : S:=#12;
              '\' : S:='\';
              '/' : S:='/';
              'u' : begin
                    S:='0000';
                    u2:=0;
                    For I:=1 to 4 do
                      begin
                      Inc(FTokenStr);
                      c2:=FTokenStr^;
                      Case c2 of
                        '0'..'9': u2:=u2*16+ord(c2)-ord('0');
                        'A'..'F': u2:=u2*16+ord(c2)-ord('A')+10;
                        'a'..'f': u2:=u2*16+ord(c2)-ord('a')+10;
                      else
                        Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
                      end;
                      end;
                    // ToDo: 4-bytes UTF16
                    if u1<>0 then
                      begin
                      if (joUTF8 in Options) or (DefaultSystemCodePage=CP_UTF8) then
                        S:=Utf8Encode(WideString(WideChar(u1)+WideChar(u2))) // ToDo: use faster function
                      else
                        S:=String(WideChar(u1)+WideChar(u2)); // WideChar converts the encoding. Should it warn on loss?
                      u1:=0;
                      end
                    else
                      begin
                      S:='';
                      u1:=u2;
                      end
                    end;
              #0  : Error(SErrOpenString,[FCurRow]);
            else
              Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
            end;
            I:=Length(S);
            if (SectionLength+I>0) then
              begin
              // If length=1, we know it was not \uXX, but u1 can be nonzero, and we must first append it.
              // example: \u00f8\"
              if I=1 then
                MaybeAppendUnicode;
              SetLength(FCurTokenString, OldLength + SectionLength+Length(S));
              if SectionLength > 0 then
                Move(TokenStart^, FCurTokenString[OldLength + 1], SectionLength);
              if I>0 then
                Move(S[1],FCurTokenString[OldLength + SectionLength+1],i);
              Inc(OldLength, SectionLength+Length(S));
              end;
            // Next char
            TokenStart := FTokenStr+1;
            end
          else
            MaybeAppendUnicode;
          if FTokenStr[0] = #0 then
            Error(SErrOpenString,[FCurRow]);
          Inc(FTokenStr);
          end;
        if FTokenStr[0] = #0 then
          Error(SErrOpenString,[FCurRow]);
        MaybeAppendUnicode;
        SectionLength := FTokenStr - TokenStart;
        SetLength(FCurTokenString, OldLength + SectionLength);
        if SectionLength > 0 then
          Move(TokenStart^, FCurTokenString[OldLength + 1], SectionLength);
        Inc(FTokenStr);
        Result := tkString;
      end;
    ',':
      begin
        Inc(FTokenStr);
        Result := tkComma;
      end;
    '0'..'9','.','-':
      begin
        TokenStart := FTokenStr;
        while true do
        begin
          Inc(FTokenStr);
          case FTokenStr[0] of
            '.':
              begin
                if FTokenStr[1] in ['0'..'9', 'e', 'E'] then
                begin
                  Inc(FTokenStr);
                  repeat
                    Inc(FTokenStr);
                  until not (FTokenStr[0] in ['0'..'9', 'e', 'E','-','+']);
                end;
                break;
              end;
            '0'..'9': ;
            'e', 'E':
              begin
                Inc(FTokenStr);
                if FTokenStr[0] in ['-','+']  then
                  Inc(FTokenStr);
                while FTokenStr[0] in ['0'..'9'] do
                  Inc(FTokenStr);
                break;
              end;
          else
            if not (FTokenStr[0] in [#0,'}',']',',',#9,' ']) then
               Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
            break;
          end;
        end;
        SectionLength := FTokenStr - TokenStart;
        FCurTokenString:='';
        SetString(FCurTokenString, TokenStart, SectionLength);
        If (FCurTokenString[1]='.') then
          FCurTokenString:='0'+FCurTokenString;
        Result := tkNumber;
      end;
    ':':
      begin
        Inc(FTokenStr);
        Result := tkColon;
      end;
    '{':
      begin
        Inc(FTokenStr);
        Result := tkCurlyBraceOpen;
      end;
    '}':
      begin
        Inc(FTokenStr);
        Result := tkCurlyBraceClose;
      end;  
    '[':
      begin
        Inc(FTokenStr);
        Result := tkSquaredBraceOpen;
      end;
    ']':
      begin
        Inc(FTokenStr);
        Result := tkSquaredBraceClose;
      end;
    '/' :
      begin
      if Not (joComments in Options) then
        Error(SErrInvalidCharacter, [CurRow,CurCOlumn,FTokenStr[0]]);
      TokenStart:=FTokenStr;
      Inc(FTokenStr);
      Case FTokenStr[0] of
        '/' : begin
              SectionLength := Length(FCurLine)- (FTokenStr - PChar(FCurLine));
              Inc(FTokenStr);
              FCurTokenString:='';
              SetString(FCurTokenString, FTokenStr, SectionLength);
              Fetchline;
              end;
        '*' :
          begin
          IsStar:=False;
          Inc(FTokenStr);
          TokenStart:=FTokenStr;
          Repeat
            if (FTokenStr[0]=#0) then
              begin
              SectionLength := (FTokenStr - TokenStart);
              S:='';
              SetString(S, TokenStart, SectionLength);
              FCurtokenString:=FCurtokenString+S;
              if not fetchLine then
                Error(SUnterminatedComment, [CurRow,CurCOlumn,FTokenStr[0]]);
              TokenStart:=FTokenStr;
              end;
            IsStar:=FTokenStr[0]='*';
            Inc(FTokenStr);
            EOC:=(isStar and (FTokenStr[0]='/'));
          Until EOC;
          if EOC then
            begin
            SectionLength := (FTokenStr - TokenStart-1);
            S:='';
            SetString(S, TokenStart, SectionLength);
            FCurtokenString:=FCurtokenString+S;
            Inc(FTokenStr);
            end;
          end;
      else
        Error(SErrInvalidCharacter, [CurRow,CurCOlumn,FTokenStr[0]]);
      end;
      Result:=tkComment;
      end;
    'a'..'z','A'..'Z','_':
      begin
        tstart:=CurRow;
        Tcol:=CurColumn;
        TokenStart := FTokenStr;
        repeat
          Inc(FTokenStr);
        until not (FTokenStr[0] in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
        SectionLength := FTokenStr - TokenStart;
        FCurTokenString:='';
        SetString(FCurTokenString, TokenStart, SectionLength);
        for it := tkTrue to tkNull do
          if CompareText(CurTokenString, TokenInfos[it]) = 0 then
            begin
            Result := it;
            FCurToken := Result;
            exit;
            end;
        if (joStrict in Options) then
          Error(SErrInvalidCharacter, [tStart,tcol,TokenStart[0]])
        else
          Result:=tkIdentifier;
      end;
  else
    Error(SErrInvalidCharacter, [CurRow,CurColumn,FTokenStr[0]]);
  end;

  FCurToken := Result;
end;

function TJSONScanner.GetCurColumn: Integer;
begin
  Result := FTokenStr - PChar(CurLine);
end;

function TJSONScanner.GetO(AIndex: TJSONOption): Boolean;
begin
  Result:=AIndex in FOptions;
end;

procedure TJSONScanner.SetO(AIndex: TJSONOption; AValue: Boolean);
begin
  If AValue then
    Include(Foptions,AIndex)
  else
    Exclude(Foptions,AIndex)
end;

end.
