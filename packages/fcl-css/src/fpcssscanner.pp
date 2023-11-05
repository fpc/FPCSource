{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2022- by Michael Van Canneyt (michael@freepascal.org)

    This file contains CSS scanner and tokenizer

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit fpCSSScanner;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpCss.Tree;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, fpCSSTree;
{$ENDIF FPC_DOTTEDUNITS}

Type
  TCSSToken =  (
    ctkUNKNOWN,
    ctkEOF,
    ctkWHITESPACE,
    ctkCOMMENT,
    ctkSEMICOLON,
    ctkLPARENTHESIS,
    ctkRPARENTHESIS,
    ctkLBRACE,
    ctkRBRACE,
    ctkLBRACKET,
    ctkRBRACKET,
    ctkCOMMA,
    ctkEQUALS,
    ctkAND,
    ctkTILDE,
    ctkTILDEEQUAL,
    ctkPLUS,
    ctkCOLON,
    ctkDOUBLECOLON,
    ctkDOT,
    ctkDIV,
    ctkGT,
    ctkGE,
    ctkLT,
    ctkLE,
    ctkPERCENTAGE,
    ctkMINUS,
    ctkSTAR,
    ctkSTAREQUAL,
    ctkINTEGER,
    ctkFLOAT,
    ctkHASH,
    ctkSTRING,
    ctkIDENTIFIER,
    ctkATKEYWORD,
    ctkURL,
    ctkBADURL,
    ctkIMPORTANT,
    ctkCLASSNAME,
    ctkFUNCTION,
    ctkPSEUDO,
    ctkPSEUDOFUNCTION,
    ctkSQUARED,
    ctkSQUAREDEQUAL,
    ctkUNICODERANGE,
    ctkPIPE,
    ctkPIPEEQUAL,
    ctkDOLLAR,
    ctkDOLLAREQUAL,
    ctkINVALID
   );
  TCSSTokens = Set of TCSSToken;

  TCSSString = UTF8String;

resourcestring
  SErrInvalidCharacter = 'Invalid character ''%s''';
  SErrOpenString = 'String exceeds end of line';
  SErrIncludeFileNotFound = 'Could not find include file ''%s''';
  SInvalidHexadecimalNumber = 'Invalid decimal number';
  SErrUnknownCharacter = 'Unknown character: %s';

Type
  ECSSScanner = Class(ECSSException);

  TLineReader = class
  public
    function IsEOF: Boolean; virtual; abstract;
    function ReadLine: TCSSString; virtual; abstract;
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
    function ReadLine: TCSSString; override;
  end;

  TFileLineReader = class(TLineReader)
  private
    FTextFile: Text;
    FileOpened: Boolean;
  public
    constructor Create(const AFilename: TCSSString);
    destructor Destroy; override;
    function IsEOF: Boolean; override;
    function ReadLine: TCSSString; override;
  end;

  { TCSSScanner }

  TCSSScannerOption = (csoExtendedIdentifiers,csoReturnComments,csoReturnWhiteSpace);
  TCSSScannerOptions = set of TCSSScannerOption;
  TCSSScannerWarnEvent = procedure(Sender: TObject; Msg: string) of object;

  TCSSScanner = class
  private
    FDisablePseudo: Boolean;
    FOnWarn: TCSSScannerWarnEvent;
    FOptions: TCSSScannerOptions;
    FSourceFile: TLineReader;
    FSourceFilename: TCSSString;
    FCurRow: Integer;
    FCurToken: TCSSToken;
    FCurTokenString: TCSSString;
    FCurLine: TCSSString;
    TokenStr: PAnsiChar;
    FSourceStream : TStream;
    FOwnSourceFile : Boolean;
    function DoHash: TCSSToken;
    function DoIdentifierLike : TCSSToken;
    function DoInvalidChars : TCSSToken;
    function DoMultiLineComment: TCSSToken;
    function CommentDiv: TCSSToken;
    function DoNumericLiteral: TCSSToken;
    function DoSingleLineComment: TCSSToken;
    function DoStringLiteral: TCSSToken;
    function DoWhiteSpace: TCSSToken;
    function EatBadURL: TCSSToken;
    Function DoUnicodeRange : TCSSTOKEN;
    function FetchLine: Boolean;
    function GetCurColumn: Integer;
    function GetReturnComments: Boolean;
    function GetReturnWhiteSpace: Boolean;
    function ReadUnicodeEscape: WideChar;
    procedure SetReturnComments(AValue: Boolean);
    procedure SetReturnWhiteSpace(AValue: Boolean);
    class function UnknownCharToStr(C: AnsiChar): TCSSString;
  protected
    procedure DoError(const Msg: TCSSString; Args: array of const); overload;
    procedure DoError(const Msg: TCSSString); overload;
    function DoFetchToken: TCSSToken; virtual;
  public
    constructor Create(ALineReader: TLineReader);
    constructor Create(AStream : TStream);
    destructor Destroy; override;
    procedure OpenFile(const AFilename: TCSSString);
    Function FetchToken: TCSSToken;
    function IsUTF8BOM: boolean;
    Property ReturnComments : Boolean Read GetReturnComments Write SetReturnComments;
    Property ReturnWhiteSpace : Boolean Read GetReturnWhiteSpace Write SetReturnWhiteSpace;
    Property Options : TCSSScannerOptions Read FOptions Write FOptions;
    property SourceFile: TLineReader read FSourceFile;
    property CurFilename: TCSSString read FSourceFilename;
    property CurLine: TCSSString read FCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;
    property CurToken: TCSSToken read FCurToken;
    property CurTokenString: TCSSString read FCurTokenString;
    property DisablePseudo : Boolean Read FDisablePseudo Write FDisablePseudo;
    property OnWarn: TCSSScannerWarnEvent read FOnWarn write FOnWarn;
  end;

function SafeFormat(const Fmt: string; const Args: array of const): string;

implementation

Const
  Alpha = ['A'..'Z','a'..'z'];
  Num   = ['0'..'9'];
  AlNum = Alpha+Num;
  AlNumIden = Alpha+Num+['-'];
  WhiteSpace = [' ',#9];
  WhiteSpaceEx = WhiteSpace+[#0];

type
  TMessageArgs = array of string;

procedure CreateMsgArgs(var MsgArgs: TMessageArgs; const Args: array of const);
var
  i: Integer;
  A : AnsiString;
  U : UnicodeString;
  {$ifdef pas2js}
  v: jsvalue;
  {$endif}
begin
  SetLength(MsgArgs, High(Args)-Low(Args)+1);
  for i:=Low(Args) to High(Args) do
    {$ifdef pas2js}
    begin
    v:=Args[i];
    if isBoolean(v) then
      MsgArgs[i] := BoolToStr(Boolean(v))
    else if isString(v) then
      MsgArgs[i] := String(v)
    else if isNumber(v) then
      begin
      if IsInteger(v) then
        MsgArgs[i] := str(NativeInt(v))
      else
        MsgArgs[i] := str(double(v));
      end
    else
      MsgArgs[i]:='';
    end;
    {$else}
    case Args[i].VType of
      vtInteger:      MsgArgs[i] := IntToStr(Args[i].VInteger);
      vtBoolean:      MsgArgs[i] := BoolToStr(Args[i].VBoolean);
      vtChar:         MsgArgs[i] := Args[i].VChar;
      {$ifndef FPUNONE}
      vtExtended:     ; //  Args[i].VExtended^;
      {$ENDIF}
      vtString:       MsgArgs[i] := Args[i].VString^;
      vtPointer:      ; //  Args[i].VPointer;
      vtPChar:        MsgArgs[i] := Args[i].VPChar;
      vtObject:       ; //  Args[i].VObject;
      vtClass:        ; //  Args[i].VClass;
      vtWideChar:
        begin
        U:=Args[i].VWideChar;
        MsgArgs[i] := String(U);
        end;
      vtPWideChar:
        begin
        U:=Args[i].VPWideChar;
        MsgArgs[i] := String(U);
        end;
      vtAnsiString:
        begin
        A:=AnsiString(Args[i].VAnsiString);
        MsgArgs[i]:=A;
        end;
      vtCurrency:     ; //  Args[i].VCurrency^);
      vtVariant:      ; //  Args[i].VVariant^);
      vtInterface:    ; //  Args[i].VInterface^);
      vtWidestring:
        begin
        U:=WideString(Args[i].VWideString);
        MsgArgs[i] := String(U);
        end;
      vtInt64:        MsgArgs[i] := IntToStr(Args[i].VInt64^);
      vtQWord:        MsgArgs[i] := IntToStr(Args[i].VQWord^);
      vtUnicodeString:
        begin
        U:=UnicodeString(Args[i].VUnicodeString);
        MsgArgs[i] := String(U);
        end;
    end;
    {$endif}
end;

function SafeFormat(const Fmt: string; const Args: array of const): string;
var
  MsgArgs: TMessageArgs;
  i: Integer;
begin
  try
    Result:=Format(Fmt,Args);
  except
    Result:='';
    MsgArgs:=nil;
    CreateMsgArgs(MsgArgs,Args);
    for i:=0 to length(MsgArgs)-1 do
      begin
      if i>0 then
        Result:=Result+',';
      Result:=Result+MsgArgs[i];
      end;
    Result:='{'+Fmt+'}['+Result+']';
  end;
end;

constructor TFileLineReader.Create(const AFilename: TCSSString);
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

function TFileLineReader.ReadLine: TCSSString;
begin
  ReadLn(FTextFile, Result);
end;

constructor TCSSScanner.Create(ALineReader: TLineReader);
begin
  inherited Create;
  FSourceFile := ALineReader;
end;

constructor TCSSScanner.Create(AStream: TStream);
begin
  FSourceStream:=ASTream;
  FOwnSourceFile:=True;
  Create(TStreamLineReader.Create(AStream));
end;

destructor TCSSScanner.Destroy;
begin
  If FOwnSourceFile then
    FSourceFile.Free;
  inherited Destroy;
end;

procedure TCSSScanner.OpenFile(const AFilename: TCSSString);
begin
  FSourceFile := TFileLineReader.Create(AFilename);
  FSourceFilename := AFilename;
end;

function TCSSScanner.FetchLine: Boolean;
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
  end;
end;

function TCSSScanner.DoWhiteSpace : TCSSToken;

begin
  Result:=ctkWhitespace;
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

function TCSSScanner.DoSingleLineComment : TCSSToken;

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
  Result := ctkComment;
end;

function TCSSScanner.DoMultiLineComment : TCSSToken;

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
        Result := ctkEOF;
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
  Result := ctkComment;
end;

function TCSSScanner.CommentDiv : TCSSToken;

begin
  FCurTokenString := '';
  Inc(TokenStr);
  if (TokenStr[0] = '/') then       // Single-line comment
    Result:=DoSingleLineComment
  else if (TokenStr[0]='*') then
    Result:=DoMultiLineComment
  else
    Result:=ctkDiv;
end;

function TCSSScanner.ReadUnicodeEscape: WideChar;

const
  Hex = ['0'..'9','A'..'F','a'..'f' ];

Var
  S : TCSSString;
  I : Integer;
  HaveHex : Boolean;

begin
  S:='';
  I:=1;
  Repeat
    S:=S+Upcase(TokenStr[0]);
    HaveHex:=TokenStr[1] in Hex;
    if HaveHex then
      Inc(TokenStr);
    Inc(I);
  Until (I>4) or not HaveHex;
  // Takes care of conversion... This needs improvement !!
  Result:=WideChar(StrToInt('$'+S));
end;

procedure TCSSScanner.SetReturnComments(AValue: Boolean);
begin
  if AValue then
    Include(FOptions,csoReturnComments)
  else
    Exclude(FOptions,csoReturnComments)
end;

procedure TCSSScanner.SetReturnWhiteSpace(AValue: Boolean);
begin
  if AValue then
    Include(FOptions,csoReturnWhiteSpace)
  else
    Exclude(FOptions,csoReturnWhiteSpace)
end;


function TCSSScanner.DoStringLiteral: TCSSToken;

Var
  Delim : AnsiChar;
  TokenStart : PAnsiChar;
  Len,OLen: Integer;
  S : TCSSString;

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
        'a'..'f',
        'A'..'F',
        '0'..'9':
              begin
              S:=UTF8Encode(ReadUniCodeEscape);
              end;
        #0  : DoError(SErrOpenString);
      else
        DoError(SErrInvalidCharacter, [TokenStr[0]]);
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
    if TokenStr[0] = #0 then
      DoError(SErrOpenString);
    Inc(TokenStr);
    end;
  if TokenStr[0] = #0 then
    DoError(SErrOpenString);
  Len := TokenStr - TokenStart;
  SetLength(FCurTokenString, OLen + Len);
  if Len > 0 then
    Move(TokenStart^, FCurTokenString[OLen+1], Len);
  Inc(TokenStr);
  Result := ctkSTRING;
end;

function TCSSScanner.DoNumericLiteral :TCSSToken;

Var
  TokenStart : PAnsiChar;
  Len : Integer;
  isEscape : Boolean;

begin
  Result := ctkINTEGER;
  isEscape:=TokenStr[0]='\';
  if IsEscape then
    Inc(TokenStr);
  TokenStart := TokenStr;
  while true do
    begin
    Inc(TokenStr);
    case TokenStr[0] of
      '.':
        if IsEscape then
          Break
        else
          begin
            Result := ctkFLOAT;
            if TokenStr[1] in ['0'..'9'] then
            begin
              Inc(TokenStr);
              repeat
                Inc(TokenStr);
              until not (TokenStr[0] in ['0'..'9']);
            end;
            break;
          end;
      '0'..'9': ;
      else
        break;
    end;
  end;
  Len:=TokenStr-TokenStart;
  Setlength(FCurTokenString, Len);
  if (Len>0) then
  Move(TokenStart^,FCurTokenString[1],Len);
  if IsEscape then
    begin
    Result:=ctkString;
    FCurTokenString:=AnsiChar(StrToInt(FCurTokenString));
    end;
end;

function TCSSScanner.DoHash :TCSSToken;

Var
  TokenStart : PAnsiChar;
  Len : Integer;

begin
  Result := ctkHASH;
  TokenStart := TokenStr;
  Inc(TokenStr);
  while (TokenStr[0]<>'#') and (TokenStr[0] in AlNumIden) do
    inc(TokenStr);
  Len:=TokenStr-TokenStart;
  Setlength(FCurTokenString, Len);
  if (Len>0) then
  Move(TokenStart^,FCurTokenString[1],Len);
end;


function TCSSScanner.EatBadURL: TCSSToken;

var
  TokenStart : PAnsiChar;
  C : AnsiChar;
  len,oldlen : integer;

begin
  Result:=ctkURL;
  While not (TokenStr[0] in [#0,')']) do
    begin
    TokenStart:=TokenStr;
    While not (TokenStr[0] in [#0,')']) do
      begin
      C:=TokenStr[0];
      if (Ord(C)<=Ord(' ')) or (Ord(C)>127) then
        Result:=ctkBADURL;
      inc(TokenStr);
      end;
    Len:=TokenStr-TokenStart;
    oldLen:=Length(FCurTokenString);
    Setlength(FCurTokenString, OldLen+Len);
    if (Len>0) then
      Move(TokenStart^,FCurTokenString[OldLen+1],Len);
    if TokenStr[0]=#0 then
      if not FetchLine then
        Exit(ctkEOF);
    end;
end;

function TCSSScanner.DoUnicodeRange: TCSSTOKEN;
Var
  TokenStart:PAnsiChar;
  Len : Integer;
  Tokens : Set of AnsiChar;

begin
  Tokens:= ['A'..'F', 'a'..'f', '0'..'9', '-'];
  Result:=ctkUNICODERANGE;
  TokenStart := TokenStr;
  Inc(TokenStr,2); // U+
  repeat
    if (TokenStr[0]='-') then
      Tokens:=Tokens-['-'];
    Inc(TokenStr);
    //If (TokenStr[0]='\') and (TokenStr[1]='u') then
  until not (TokenStr[0] in Tokens);
  Len:=(TokenStr-TokenStart);
  SetLength(FCurTokenString,Len);
  if Len > 0 then
    Move(TokenStart^,FCurTokenString[1],Len);

end;

class function TCSSScanner.UnknownCharToStr(C: AnsiChar): TCSSString;

begin
  if C=#0 then
    Result:='EOF'
  else if (C in WhiteSpace) then
    Result:='#'+IntToStr(Ord(C))
  else
    Result:='"'+C+'"';
end;

function TCSSScanner.DoIdentifierLike : TCSSToken;

Var
  TokenStart:PAnsiChar;
  Len,oLen : Integer;
  IsEscape,IsAt, IsPseudo, IsFunc : Boolean;

begin
  Result:=ctkIDENTIFIER;
  TokenStart := TokenStr;
  IsPseudo:=False;
  IsAt:=TokenStr[0]='@';
  IsFunc:=false;
  For Len:=1 to 2 do
    if TokenStr[0]=':' then
      begin
      IsPseudo:=True;
      Inc(TokenStr);
      end;
  Repeat
    if not (TokenStr[0]='\') then
      repeat
        Inc(TokenStr);
        //If (TokenStr[0]='\') and (TokenStr[1]='u') then
      until not (TokenStr[0] in ['A'..'Z', 'a'..'z', '0'..'9', '_','-']);
    IsEscape:=TokenStr[0]='\';
    if IsEscape then
      begin
      if ((TokenStr[0] in WhiteSpace) or (TokenStr[0]=#0))  then
        DoError(SErrUnknownCharacter ,[UnknownCharToStr(TokenStr[0])])
      end
    else if not IsAt then
      begin
      IsFunc:=TokenStr[0]='(';
      if IsFunc then
        Inc(TokenStr);
      end;
    Len:=(TokenStr-TokenStart);
    oLen:=Length(FCurTokenString);
    SetLength(FCurTokenString,Olen+Len);
    if Len > 0 then
      Move(TokenStart^,FCurTokenString[Olen+1],Len);
    if IsEscape then
      Inc(TokenStr);
    TokenStart := TokenStr;
  until Not IsEscape;
  // Some specials
  if (CurTokenString[1]='.') and not IsFunc then
    Result:=ctkCLASSNAME
  else if isAt then
    Result:=ctkATKEYWORD
  else if CurTokenString='!important' then
    Result:=ctkIMPORTANT
  else if (CurtokenString='url(') then
    begin
    Result:=ctkURL;
    If TokenStr[0] in ['"',''''] then
      DoStringLiteral
    else
      begin
      result:=EatBadURL;
      end;
    If (result<>ctkEOF) and (TokenStr[0] in [')']) then
      Inc(TokenStr);
    end
  else if IsPseudo then
    begin
    if IsFunc then
      Result:=ctkPSEUDOFUNCTION
    else
      Result:=ctkPSEUDO;
    end
  else if IsFunc then
    Result:=ctkFUNCTION;
end;

function TCSSScanner.DoInvalidChars: TCSSToken;
var
  TokenStart: PAnsiChar;
  Len: SizeUInt;
begin
  Result:=ctkINVALID;
  TokenStart := TokenStr;
  repeat
    writeln('TCSSScanner.DoInvalidChars ',hexstr(ord(TokenStr^),2));
    Inc(TokenStr);
  until (TokenStr[0] in [#0,#9,#10,#13,#32..#127]);
  Len:=TokenStr-TokenStart;
  SetLength(FCurTokenString,Len);
  if Len > 0 then
    Move(TokenStart^,FCurTokenString[1],Len);
end;

function TCSSScanner.FetchToken: TCSSToken;

var
  CanStop : Boolean;

begin
  Repeat
    Result:=DoFetchToken;
    if (Result=ctkINVALID) and IsUTF8BOM then
      CanStop:=false
    else
      CanStop:=(Not (Result in [ctkComment,ctkWhiteSpace]))
             or ((ReturnComments and (Result=ctkComment))
                  or
                 (ReturnWhiteSpace and (Result=ctkWhiteSpace))
                )
  Until CanStop;
end;

function TCSSScanner.IsUTF8BOM: boolean;
begin
  Result:=(length(FCurTokenString)=3)
      and (FCurTokenString[1]=#$EF)
      and (FCurTokenString[2]=#$BB)
      and (FCurTokenString[3]=#$BF);
end;

function TCSSScanner.DoFetchToken: TCSSToken;


  Procedure CharToken(aToken : TCSSToken);

  begin
    FCurTokenString:=TokenStr[0];
    Inc(TokenStr);
    Result:=aToken;
  end;

  Procedure TwoCharsToken(aToken : TCSSToken);

  begin
    FCurTokenString:=TokenStr[0]+TokenStr[1];
    Inc(TokenStr,2);
    Result:=aToken;
  end;

begin
  if TokenStr = nil then
    begin
    if not FetchLine then
      begin
      Result := ctkEOF;
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
      Result := ctkWhitespace;
      end;
    '''','"':
       Result:=DoStringLiteral;
    '/' :
       Result:=CommentDiv;
    #9, ' ':
       Result := DoWhiteSpace;
    '#':
       Result:=DoHash;
    '\':
       begin
       if TokenStr[1] in ['0'..'9'] then
         Result:=DoNumericLiteral
       else
         begin
         if (TokenStr[1] in WhiteSpace) or (TokenStr[1]=#0) then
           DoError(SErrUnknownCharacter ,[UnknownCharToStr(TokenStr[1])])
         else
           Result:=DoIdentifierLike
         end;
       end;
    '0'..'9':
       Result:=DoNumericLiteral;
    '&': CharToken(ctkAnd);
    '{': CharToken( ctkLBRACE);
    '}': CharToken(ctkRBRACE);
    '*': if TokenStr[1]='=' then
           TwoCharsToken(ctkSTAREQUAL)
         else if (csoExtendedIdentifiers in Options) and (TokenStr[1] in AlNumIden) then
           Result:=DoIdentifierLike
         else
           CharToken(ctkSTAR);
    '^':
      if TokenStr[1]='=' then
        TwoCharsToken(ctkSQUAREDEQUAL)
      else
        CharToken(ctkSQUARED);
    ',': CharToken(ctkCOMMA);
    '~':
      if TokenStr[1]='=' then
        TwoCharsToken(ctkTILDEEQUAL)
      else
        CharToken(ctkTILDE);
    '|':
      if TokenStr[1]='=' then
        TwoCharsToken(ctkPIPEEQUAL)
      else
        CharToken(ctkPIPE);
    '$':
      if TokenStr[1]='=' then
        TwoCharsToken(ctkDOLLAREQUAL)
      else
        CharToken(ctkDOLLAR);
    ';': CharToken(ctkSEMICOLON);
    '@': Result:=DoIdentifierLike;
    ':':
      begin
      if DisablePseudo then
        CharToken(ctkCOLON)
      else if (TokenStr[1]=':') then
        begin
        if (TokenStr[2] in AlNumIden) then
          Result:=DoIdentifierLike
        else
          Result:=ctkDoubleCOLON
        end
      else if (TokenStr[1] in AlNumIden) then
        Result:=DoIdentifierLike
      else
        CharToken(ctkCOLON);
      end;
    '.':
      begin
      if (TokenStr[1] in AlNum) then
        Result:=Self.DoIdentifierLike
      else
        CharToken(ctkDOT);
      end;
    '>':
      if TokenStr[1]='=' then
        TwoCharsToken(ctkGE)
      else
        CharToken(ctkGT);
    '<':
      if TokenStr[1]='=' then
        TwoCharsToken(ctkLE)
      else
        CharToken(ctkLT);
    '(': CharToken(ctkLPARENTHESIS);
    ')': CharToken(ctkRPARENTHESIS);
    '[': CharToken(ctkLBRACKET);
    ']': CharToken(ctkRBRACKET);
    '=': CharToken(ctkEQUALS);
    '-':
      begin
      if (TokenStr[1] in ['0'..'9']) then
        Result:=DoNumericLiteral
      else if Not (TokenStr[1] in WhiteSpaceEx) then
        Result:=DoIdentifierLike
      else
        CharToken(ctkMINUS);
      end;
    '+': CharToken(ctkPLUS);
    '%': CharToken(ctkPERCENTAGE);
    '_','!',
    'a'..'z',
    'A'..'Z':
       begin
       if (TokenStr[0] in ['u','U']) and (TokenStr[1]='+') then
         Result:=DoUnicodeRange
       else
         Result:=DoIdentifierLike;
       end;
  else
    writeln('TCSSScanner.DoFetchToken ',Ord(TokenStr[0]));
    If Ord(TokenStr[0])>127 then
      Result:=DoInvalidChars
    else
      DoError(SErrUnknownCharacter ,['"'+TokenStr[0]+'"']);

  end; // Case
end;

procedure TCSSScanner.DoError(const Msg: TCSSString; Args: array of const);
begin
  DoError(Format(Msg,Args));
end;

procedure TCSSScanner.DoError(const Msg: TCSSString);

Var
  S : TCSSString;

begin
  S:=Format('Error at (%d,%d): ',[CurRow,CurColumn])+Msg;
  Raise ECSSScanner.Create(S);
end;

function TCSSScanner.GetCurColumn: Integer;
begin
  if (TokenStr=Nil) or (Length(CurLine)=0) then
    Result:=0
  else
    Result := TokenStr - PAnsiChar(CurLine);
end;

function TCSSScanner.GetReturnComments: Boolean;
begin
  Result:=(csoReturnComments in FOptions);
end;

function TCSSScanner.GetReturnWhiteSpace: Boolean;
begin
  Result:=(csoReturnWhiteSpace in FOptions);
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

function TStreamLineReader.ReadLine: TCSSString;

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
    Inc(FBufPos);
    // Check #13#10
    If (PRun^=13) then
      begin
      If (FBufPos=FBufLen) then
        FillBuffer;
      If (FBufPos<FBufLen) and (Buffer[FBufpos]=10) then
        Inc(FBufPos);
      end;
    end;
end;

end.

