{
    This file is part of the Free Component Library

    ECMAScript (JavaScript) source lexical scanner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}

{$IFNDEF FPC_DOTTEDUNITS}
unit JSScanner;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, Js.Token;
{$ELSE FPC_DOTTEDUNITS}
uses SysUtils, Classes, jstoken;
{$ENDIF FPC_DOTTEDUNITS}

Type
  TECMAVersion = (ecma5,ecma2015,ecma2021);

Const
  // TJSToken is the maximum known set of keywords.
  // Here we specify what keywords are not keywords for 'older' versions.
  NonJSKeywords : Array [TECMAVersion] of TJSTokens
    = (
        [tjsAwait, tjsClass, tjsConst,tjsDebugger,tjsEnum, tjsExport, tjsExtends, tjsImport, tjsLet, tjsSUPER, tjsYield], //ecma5
        [], // ecma2015
        [] // ecma2022
      );

resourcestring
  SErrInvalidCharacter = 'Invalid character ''%s''';
  SErrUnTerminatedString = 'string exceeds end of line';
  SErrExpectedEllipsis = 'Expected ellipsis, got ..';
  SErrIncludeFileNotFound = 'Could not find include file ''%s''';
  SErrIfXXXNestingLimitReached = 'Nesting of $IFxxx too deep';
  SErrInvalidPPElse = '$ELSE without matching $IFxxx';
  SErrInvalidPPEndif = '$ENDIF without matching $IFxxx';
  SInvalidHexadecimalNumber = 'Invalid decimal number';
  SErrInvalidNonEqual = 'Syntax Error: != or !== expected';
  SErrInvalidRegularExpression = 'Syntax error in regular expression: / expected, got: %s';

Type
  TJSScannerString = UTF8String;

  { TLineReader }

  TLineReader = class
  private
    FLastLF: TJSScannerString;
  public
    function IsEOF: Boolean; virtual; abstract;
    function ReadLine: TJSScannerString; virtual; abstract;
    Property LastLF : TJSScannerString Read FLastLF Write FLastLF;
  end;

  { TStreamLineReader }

  TStreamLineReader = class(TLineReader)
  private
    FStream : TStream;
    Buffer : Array[0..1024] of Byte;
    FBufPos,
    FBufLen : Integer;
    procedure FillBuffer;
  public
    Constructor Create(AStream : TStream);
    function IsEOF: Boolean; override;
    function ReadLine: TJSScannerString; override;
  end;

  TFileLineReader = class(TLineReader)
  private
    FTextFile: Text;
    FileOpened: Boolean;
  public
    constructor Create(const AFilename: TJSScannerString);
    destructor Destroy; override;
    function IsEOF: Boolean; override;
    function ReadLine: TJSScannerString; override;
  end;

  EJSScannerError       = class(Exception);

  { TJSScanner }

  TJSScanner = class
  private
    FDisableRShift: Boolean;
    FECMAVersion: TECMAVersion;
    FIsTypeScript: Boolean;
    FReturnComments: Boolean;
    FReturnWhiteSpace: Boolean;
    FSourceFile: TLineReader;
    FSourceFilename: String;
    FCurRow: Integer;
    FCurToken: TJSToken;
    FCurTokenString: TJSScannerString;
    FCurLine: TJSScannerString;
    FWasMultilineString: Boolean;
    TokenStr: PAnsiChar;
    FWasEndOfLine : Boolean;
    FSourceStream : TStream;
    FOwnSourceFile : Boolean;
    FNonKeyWords : TJSTokens;
    function CommentDiv: TJSToken;
    function DoIdentifier : TJSToken;
    function DoMultiLineComment: TJSToken;
    function DoNumericLiteral: TJSToken;
    function DoSingleLineComment: TJSToken;
    function DoStringLiteral: TJSToken;
    function DoWhiteSpace: TJSToken;
    function FetchLine: Boolean;
    function GetCurColumn: Integer;
    function ReadUnicodeEscape: WideChar;
    Function ReadRegex : TJSToken;
    procedure SetECMAVersion(AValue: TECMAVersion);
    procedure SetIsTypeScript(AValue: Boolean);
  protected
    procedure Error(const Msg: TJSScannerString);overload;
    procedure Error(const Msg: TJSScannerString; Args: array of Const);overload;
  public
    constructor Create(ALineReader: TLineReader; aECMAVersion : TECMAVersion = ecma5);
    constructor Create(AStream : TStream; aECMAVersion : TECMAVersion = ecma5; const aFileName : TJSScannerString = '');
    destructor Destroy; override;
    procedure OpenFile(const AFilename: TJSScannerString);
    Function FetchRegexprToken: TJSToken;
    Function FetchToken: TJSToken;
    Function IsEndOfLine : Boolean;
    Property WasEndOfLine : Boolean Read FWasEndOfLine;
    Property ReturnComments : Boolean Read FReturnComments Write FReturnComments;
    Property ReturnWhiteSpace : Boolean Read FReturnWhiteSpace Write FReturnWhiteSpace;
    property SourceFile: TLineReader read FSourceFile;
    property CurFilename: String read FSourceFilename;
    property CurLine: TJSScannerString read FCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;
    property CurToken: TJSToken read FCurToken;
    property CurTokenString: UTF8String read FCurTokenString;
    property ECMAVersion : TECMAVersion Read FECMAVersion Write SetECMAVersion;
    Property IsTypeScript : Boolean Read FIsTypeScript Write SetIsTypeScript;
    Property DisableRShift : Boolean Read FDisableRShift Write FDisableRShift;
    Property WasMultilineString : Boolean Read FWasMultilineString;
  end;


implementation


constructor TFileLineReader.Create(const AFilename: TJSScannerString);
begin
  inherited Create;
  Assign(FTextFile, AFilename);
  Reset(FTextFile);
  FileOpened := true;
end;

destructor TFileLineReader.Destroy;
begin
  if FileOpened then
    Close(FTextFile);
  inherited Destroy;
end;

function TFileLineReader.IsEOF: Boolean;
begin
  Result := EOF(FTextFile);
end;

function TFileLineReader.ReadLine: TJSScannerString;
begin
  ReadLn(FTextFile, Result);
end;

constructor TJSScanner.Create(ALineReader: TLineReader; aECMAVersion: TECMAVersion);
begin
  inherited Create;
  FSourceFile := ALineReader;
  ECMAVersion:=aECMAVersion;
  FNonKeyWords:=NonJSKeywords[aECMAVersion];
end;

constructor TJSScanner.Create(AStream: TStream; aECMAVersion: TECMAVersion; Const aFileName : TJSScannerString = '');
begin
  FSourceStream:=ASTream;
  FOwnSourceFile:=True;
  FSourceFilename:=aFilename;
  Create(TStreamLineReader.Create(AStream),aECMAVersion);
end;

destructor TJSScanner.Destroy;
begin
  If FOwnSourceFile then
    FSourceFile.Free;
  inherited Destroy;
end;

procedure TJSScanner.OpenFile(const AFilename: TJSScannerString);
begin
  FSourceFile := TFileLineReader.Create(AFilename);
  FSourceFilename := AFilename;
end;

Function TJSScanner.FetchRegexprToken: TJSToken;
begin
  if (CurToken in [tjsDiv,tjsDivEq]) then
    Result:=ReadRegEx
  else
    Result:=CurToken
end;


procedure TJSScanner.Error(const Msg: TJSScannerString);
begin
  raise EJSScannerError.Create(Msg);
end;

procedure TJSScanner.Error(const Msg: TJSScannerString; Args: array of Const);
begin
  raise EJSScannerError.CreateFmt(Msg, Args);
end;

function TJSScanner.FetchLine: Boolean;
begin
  if FSourceFile.IsEOF then
  begin
    FCurLine := '';
    TokenStr := nil;
    Result := false;
  end else
  begin
    FCurLine := FSourceFile.ReadLine;
    TokenStr := PAnsiChar(CurLine);
    Result := true;
    Inc(FCurRow);
    FWasEndofLine:=True;
  end;
end;

function TJSScanner.DoWhiteSpace : TJSToken;

begin
  Result:=tjsWhitespace;
  repeat
    Inc(TokenStr);
    if TokenStr[0] = #0 then
      if not FetchLine then
       begin
       FCurToken := Result;
       exit;
       end;
  until not (TokenStr[0] in [#9, ' ']);
end;

function TJSScanner.DoSingleLineComment : TJSToken;

Var
  TokenStart : PAnsiChar;
  Len : Integer;

begin
  Inc(TokenStr);
  TokenStart := TokenStr;
  while TokenStr[0] <> #0 do
     Inc(TokenStr);
  Len:=TokenStr-TokenStart;
  SetLength(FCurTokenString, Len);
  if (Len>0) then
    Move(TokenStart^,FCurTokenString[1],Len);
  Result := tjsComment;
end;

function TJSScanner.DoMultiLineComment : TJSToken;

Var
  TokenStart : PAnsiChar;
  Len,OLen : Integer;
  PrevToken : AnsiChar;

begin
  Inc(TokenStr);
  TokenStart := TokenStr;
  FCurTokenString := '';
  OLen:= 0;
  PrevToken:=#0;
  while Not ((TokenStr[0]='/') and (PrevToken='*')) do
    begin
    if (TokenStr[0]=#0) then
      begin
      Len:=TokenStr-TokenStart+1;
      SetLength(FCurTokenString,OLen+Len);
      if Len>1 then
        Move(TokenStart^,FCurTokenString[OLen+1],Len-1);
      Inc(OLen,Len);
      FCurTokenString[OLen]:=#10;
      if not FetchLine then
        begin
        Result := tjsEOF;
        FCurToken := Result;
        exit;
        end;
      TokenStart := TokenStr;
      PrevToken:=#0;
      end
    else
      begin
      PrevToken:=TokenStr[0];
      Inc(TokenStr);
      end;
    end;
  Len:=TokenStr-TokenStart-1; // -1 for *
  SetLength(FCurTokenString, Olen+Len);
  if (Len>0) then
    Move(TokenStart^, FCurTokenString[Olen + 1], Len);
  Inc(TokenStr);
  Result := tjsComment;
end;

function TJSScanner.CommentDiv : TJSToken;

begin
  FCurTokenString := '';
  Inc(TokenStr);
  if (TokenStr[0] = '/') then       // Single-line comment
    Result:=DoSingleLineComment
  else if (TokenStr[0]='*') then
    Result:=DoMultiLineComment
  else if (TokenStr[0] = '=') then       // Single-line comment
    begin
    Result:=tjsDiveQ;
    Inc(TokenStr)
    end
  else
    Result:=tjsDiv;
end;

function TJSScanner.ReadUnicodeEscape: WideChar;

Var
  S : TJSScannerString;
  I : Integer;

begin
  S:='0000';
  For I:=1 to 4 do
    begin
    Inc(TokenStr);
    Case TokenStr[0] of
      '0'..'9','A'..'F','a'..'f' :
        S[i]:=Upcase(TokenStr[0]);
    else
      Error(SErrInvalidCharacter, [TokenStr[0]]);
    end;
    end;
  // Takes care of conversion... This needs improvement !!
  Result:=WideChar(StrToInt('$'+S));
end;

Function TJSScanner.ReadRegex: TJSToken;

Var
  CC : Boolean; // Character class
  Done : Boolean;
  CL,L : Integer;
  TokenStart : PAnsiChar;

begin
  if (CurToken<>tjsDivEq) then
    FCurTokenString := '/'
  else
    FCurTokenString := '/=';
  CL:=Length(FCurTokenString);
  Inc(TokenStr);
  TokenStart:=TokenStr;
  Done:=False;
  CC:=False;
  While Not Done do
    begin
    Case TokenStr[0] of
      #0 : Done:=True;
      '/' : Done:=Not CC;
      '\' : begin
            Inc(TokenStr);
            Done:=TokenStr=#0;
            end;
      '[' : CC:=True;
      ']' : CC:=False;
    end;
    if not Done then
      Inc(TokenStr);
    end;
  If (TokenStr[0]<>'/') then
    Error(SErrInvalidRegularExpression, [TokenStr[0]]);
  repeat
    Inc(TokenStr);
  until not (TokenStr[0] in ['A'..'Z', 'a'..'z', '0'..'9', '_','$']);
  L:=(TokenStr-TokenStart);
  SetLength(FCurTokenString,CL+L);
  Move(TokenStart^,FCurTokenString[CL+1],L);
  Result:=tjsRegEx;
end;

procedure TJSScanner.SetECMAVersion(AValue: TECMAVersion);
begin
  if FECMAVersion=AValue then Exit;
  FECMAVersion:=AValue;
  FNonKeyWords:=NonJSKeywords[aValue];
end;

procedure TJSScanner.SetIsTypeScript(AValue: Boolean);
begin
  if FIsTypeScript=AValue then Exit;
  FIsTypeScript:=AValue;
  if True then
    ecmaversion:=ecma2021;
end;

function TJSScanner.DoStringLiteral: TJSToken;

Var
  Delim : AnsiChar;
  TokenStart : PAnsiChar;
  Len,OLen: Integer;
  S : UTF8String;

  Procedure AddToString;

  begin
    Len := TokenStr - TokenStart;
    SetLength(FCurTokenString, OLen + Len);
    if Len > 0 then
      Move(TokenStart^, FCurTokenString[OLen+1], Len);
    OLen:=OLen+Len;
  end;

begin
  Delim:=TokenStr[0];
  Inc(TokenStr);
  TokenStart := TokenStr;
  OLen := 0;
  FCurTokenString := '';
  while not (TokenStr[0] in [#0,Delim]) do
    begin
    if (TokenStr[0]='\') then
      begin
      // Save length
      Len := TokenStr - TokenStart;
      Inc(TokenStr);
      // Read escaped token
      Case TokenStr[0] of
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
              S:=UTF8Encode(ReadUniCodeEscape);
              end;
        #0  : Error(SErrUnterminatedString);
      else
        Error(SErrInvalidCharacter, [TokenStr[0]]);
      end;
      SetLength(FCurTokenString, OLen + Len+1+Length(S));
      if Len > 0 then
        Move(TokenStart^, FCurTokenString[OLen + 1], Len);
      Move(S[1],FCurTokenString[OLen + Len+1],Length(S));
      Inc(OLen, Len+Length(S));
      // Next AnsiChar
      // Inc(TokenStr);
      TokenStart := TokenStr+1;
      end;
    Inc(TokenStr);
    if TokenStr[0] = #0 then
      begin
      if Delim<>'`' then
        Error(SErrUnterminatedString)
      else
        begin
        AddToString;
        FCurTokenString:=FCurTokenString+FSourceFile.LastLF;
        oLen:=oLen+Length(FSourceFile.LastLF);
        if Not FetchLine then
          Error(SErrUnterminatedString);
        TokenStart:=TokenStr;
        end
      end;
    end;
  if TokenStr[0] = #0 then
    Error(SErrUnterminatedString);
  AddToString;
  Inc(TokenStr);
  Result := tjsString;
end;

function TJSScanner.DoNumericLiteral :TJSToken;

Var
  TokenStart : PAnsiChar;
  Len : Integer;

begin
  TokenStart := TokenStr;
  while true do
    begin
    Inc(TokenStr);
    case TokenStr[0] of
      'x':
        If (TokenStart[0]='0') and ((TokenStr-TokenStart)=1) then
          begin
          Inc(TokenStr);
          while Upcase(TokenStr[0]) in ['0'..'9','A'..'F'] do
            Inc(TokenStr);
          Break;  
          end
        else
          Error(SInvalidHexadecimalNumber);
      '.':
        begin
          if TokenStr[1] in ['0'..'9', 'e', 'E'] then
          begin
            Inc(TokenStr);
            repeat
              Inc(TokenStr);
            until not (TokenStr[0] in ['0'..'9', 'e', 'E','-','+']);
          end;
          break;
        end;
      '0'..'9': ;
      'e', 'E':
        begin
          Inc(TokenStr);
          if TokenStr[0] in ['-','+']  then
            Inc(TokenStr);
          while TokenStr[0] in ['0'..'9'] do
            Inc(TokenStr);
          break;
        end;
      else
        break;
    end;
  end;
  Len:=TokenStr-TokenStart;
  Setlength(FCurTokenString, Len);
  if (Len>0) then
    Move(TokenStart^,FCurTokenString[1],Len);
  Result := tjsNumber;
end;

function TJSScanner.DoIdentifier : TJSToken;

Var
  TokenStart:PAnsiChar;
  Len : Integer;
  I : TJSToken;

begin
  Result:=tjsIdentifier;
  TokenStart := TokenStr;
  repeat
    Inc(TokenStr);
    //If (TokenStr[0]='\') and (TokenStr[1]='u') then
  until not (TokenStr[0] in ['A'..'Z', 'a'..'z', '0'..'9', '_','$']);
  Len:=(TokenStr-TokenStart);
  SetLength(FCurTokenString,Len);
  if Len > 0 then
    Move(TokenStart^,FCurTokenString[1],Len);
  // Check if this is a keyword or identifier
  // !!!: Optimize this!
  for i:=FirstKeyword to Lastkeyword do
    if Not (I in FNonKeyWords) then
      if (CurTokenString=TokenInfos[i]) then
        begin
        Result := i;
        FCurToken := Result;
        exit;
        end;
end;

Function TJSScanner.FetchToken: TJSToken;

begin
  if not (FCurtoken in [tjsWhiteSpace,tjsComment]) then
    FWasEndOfLine:=False;
  Repeat
    if TokenStr = nil then
      begin
      if not FetchLine then
        begin
        Result := tjsEOF;
        FCurToken := Result;
        exit;
        end;
      end;
    //CurPos:=TokenStr;
    FCurTokenString := '';
    case TokenStr[0] of
      #0:         // Empty line
        begin
        FetchLine;
        Result := tjsWhitespace;
        end;
      '/' :
         Result:=CommentDiv;
      #9, ' ':
         Result := DoWhiteSpace;
      '''','"','`':
         begin
         FWasMultilineString:=(TokenStr[0]='`');
         Result:=DoStringLiteral;
         end;
      '0'..'9':
         Result:=DoNumericLiteral;
     '&':
         begin
         Inc(TokenStr);
         If Tokenstr[0]='&' then
           begin
           Inc(TokenStr);
           Result := tjsAndAnd;
           end
         else If Tokenstr[0]='=' then
           begin
           Inc(TokenStr);
           Result := tjsAndEQ;
           end
         else
           Result := tjsAnd;
         end;
     '%':
         begin
         Inc(TokenStr);
         If Tokenstr[0]='=' then
           begin
           Inc(TokenStr);
           Result := tjsModEq;
           end
         else
           Result := tjsMod;
         end;
     '^':
         begin
         Inc(TokenStr);
         If (TokenStr[0]='=') then
           begin
           Result:=tjsXorEq;
           Inc(tokenStr)
           end
         else
           result:=tjsXOR;
         end;
     '|':
         begin
         Inc(TokenStr);
         If Tokenstr[0]='|' then
           begin
           Inc(TokenStr);
           Result := tjsOROR;
           end
         else If Tokenstr[0]='=' then
             begin
             Inc(TokenStr);
             Result := tjsOREQ;
             end
         else
           Result := tjsOR;
         end;
    '(':
      begin
      Inc(TokenStr);
      Result := tjsBraceOpen;
      end;
    ')':
      begin
      Inc(TokenStr);
      Result := tjsBraceClose;
      end;
    '*':
      begin
      Inc(TokenStr);
      If (TokenStr[0]='=') then
        begin
        Inc(TokenStr);
        Result := tjsMulEq;
        end
      else if (TokenStr[0]='*') then
        begin
        Inc(TokenStr);
        Result := tjsPower;
        end
      else
        Result := tjsMul;
      end;
    '+':
      begin
      Inc(TokenStr);
      If (TokenStr[0]='=') then
        begin
        Inc(TokenStr);
        Result := tjsPlusEq;
        end
      else If (TokenStr[0]='+') then
        begin
        Inc(TokenStr);
        Result := tjsPlusPlus;
        end
      else
        Result := tjsPlus;
      end;
    ',':
      begin
        Inc(TokenStr);
        Result := tjsComma;
      end;
    '-':
      begin
      Inc(TokenStr);
      If (TokenStr[0]='=') then
        begin
        Inc(TokenStr);
        Result:=tjsMinusEq
        end
      else If (TokenStr[0]='-') then
        begin
        Inc(TokenStr);
        Result:=tjsMinusMinus
        end
      else if (TokenStr[0] in ['0'..'9']) then
        begin
        Result:=DoNumericLiteral;
        If (Result=tjsNumber) then
          FCurTokenString:='-'+FCurTokenString;
        end
      else
        Result := tjsMinus;
      end;
    '.':
      begin
      Inc(TokenStr);
      if (TokenStr[0] in ['0'..'9']) then
        begin
        Result:=DoNumericLiteral;
        If (Result=tjsNumber) then
          FCurTokenString:='0.'+FCurTokenString;
         end
      else if TokenStr[0] = '.' then
        begin
        Inc(TokenStr);
        if TokenStr[0]='.' then
          begin
          Inc(TokenStr);
          Result:=tjsEllipsis
          end
        else
          Error(SerrExpectedEllipsis);
        end
      else
        Result := tjsDot;
      end;
    ':':
      begin
      Inc(TokenStr);
      Result := tjsColon;
      end;
    '?':
      begin
      Inc(TokenStr);
      Result := tjsConditional;
      end;
    ';':
      begin
      Inc(TokenStr);
      Result := tjsSemicolon;
      end;
    '<':
      begin
      Inc(TokenStr);
      if TokenStr[0] = '=' then
        begin
        Inc(TokenStr);
        Result := tjsLE;
        end
      else if TokenStr[0] = '<' then
        begin
        Inc(TokenStr);
        if (TokenStr[0] = '=') then
          begin
          Inc(TokenStr);
          Result := tjsLShiftEq;
          end
        else
          Result := tjsLShift;
        end
      else
        Result := tjsLT;
      end;
    '=':
      begin
      Inc(TokenStr);
      if (TokenStr[0]='=') then
        begin
        Inc(TokenStr);
        If (TokenStr[0]='=') then
          begin
          Inc(TokenStr);
          Result:=tjsSEQ;
          end
        else
          Result:=tjsEQ;
        end
      else if (TokenStr[0]='>') then
        begin
        Inc(TokenStr);
        Result:=tjsArrow;
        end
      else
        Result := tjsAssign;
      end;
    '!':
      begin
      Inc(TokenStr);
      if (TokenStr[0]='=') then
        begin
        Inc(TokenStr);
        If (TokenStr[0]='=') then
          begin
          Inc(TokenStr);
          Result:=tjsSNE;
          end
        else
          Result:=tjsNE;
        end
      else
        Result:=tjsNot;// Error(SErrInvalidNonEqual);
      end;
    '~':
      begin
      Inc(TokenStr);
      Result:=tjsInv;
      end;
    '>':
      begin
      Inc(TokenStr);
      if TokenStr[0] = '=' then
	begin
        Inc(TokenStr);
        Result:=tjsGE;
        end
      else if (TokenStr[0] = '>') and Not DisableRShift then
  	begin
        Inc(TokenStr);
        if (TokenStr[0] = '>') then
       	  begin
          Inc(TokenStr);
          if (TokenStr[0] = '=') then
            begin
            Inc(TokenStr);
            Result:=tjsURSHIFTEQ;
            end
          else
            Result:=tjsURSHIFT;
          end
        else if (TokenStr[0] = '=') then
          begin
          Inc(TokenStr);
          Result:=tjsRSHIFTEq;
          end
        else
          Result:=tjsRSHIFT;
        end
      else
        Result := tjsGT;
      end;
    '[':
      begin
      Inc(TokenStr);
      Result := tJSSquaredBraceOpen;
      end;
    ']':
      begin
      Inc(TokenStr);
      Result := tJSSquaredBraceClose;
      end;
    '{':
      begin
      Inc(TokenStr);
      Result := tJSCurlyBraceOpen;
      end;
    '}':
      begin
      Inc(TokenStr);
      Result := tJSCurlyBraceClose;
      end;
    #$C2:
      begin
      // Non-breaking space in UTF8
      if TokenStr[1]=#$A0 then
        begin
        Inc(TokenStr);
        Inc(TokenStr);
        Result:=tjsWhiteSpace;
        end
      else
        Result:=DoIdentifier;
      end;
   else
     Result:=DoIdentifier;
   end; // Case
  Until (Not (Result in [tjsComment,tjsWhitespace])) or
        ((Result=tjsComment) and ReturnComments) or
        ((Result=tjsWhiteSpace) and ReturnWhiteSpace);
end;

Function TJSScanner.IsEndOfLine: Boolean;
begin
  Result:=(TokenStr=Nil) or (TokenStr[0] in [#0,#10,#13]);
end;

function TJSScanner.GetCurColumn: Integer;
begin
  Result := TokenStr - PAnsiChar(CurLine);
end;

{ TStreamLineReader }

constructor TStreamLineReader.Create(AStream: TStream);
begin
  FStream:=AStream;
  FBufPos:=0;
  FBufLen:=0;
end;

function TStreamLineReader.IsEOF: Boolean;
begin
  Result:=(FBufPos>=FBufLen);
  If Result then
    begin
    FillBuffer;
    Result:=(FBufLen=0);
    end;
end;

procedure TStreamLineReader.FillBuffer;

begin
  FBufLen:=FStream.Read(Buffer,SizeOf(Buffer)-1);
  Buffer[FBufLen]:=0;
  FBufPos:=0;
end;

function TStreamLineReader.ReadLine: TJSScannerString;

Var
  FPos,OLen,Len: Integer;
  PRun : PByte;

begin
  Result:='';
  FPos:=FBufPos;
  Repeat
    PRun:=@Buffer[FBufPos];
    While (FBufPos<FBufLen) and Not (PRun^ in [10,13]) do
      begin
      Inc(PRun);
      Inc(FBufPos);
      end;
    If (FBufPos=FBufLen) then
      begin
      Len:=FBufPos-FPos;
      If (Len>0) then
        begin
        Olen:=Length(Result);
        SetLength(Result,OLen+Len);
        Move(Buffer[FPos],Result[OLen+1],Len);
        end;
      FillBuffer;
      FPos:=FBufPos;
      end;
  until (FBufPos=FBufLen) or (PRun^ in [10,13]);
  Len:=FBufPos-FPos;
  If (Len>0) then
    begin
    Olen:=Length(Result);
    SetLength(Result,OLen+Len);
    Move(Buffer[FPos],Result[OLen+1],Len)
    end;
  If (PRun^ in [10,13]) and (FBufPos<FBufLen) then
    begin
    LastLF:=PAnsiChar(PRun)^;
    Inc(FBufPos);
    // Check #13#10
    If (PRun^=13) then
      begin
      If (FBufPos=FBufLen) then
        FillBuffer;
      If (FBufPos<FBufLen) and (Buffer[FBufpos]=10) then
        begin
        LastLF:=LastLF+#10;
        Inc(FBufPos);
        end;
      end;
    end;
end;

end.
