{ ********************************************************************* 
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2016 Michael Van Canneyt.
       
    Javascript minifier
            
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
                   
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
                                
  **********************************************************************}
unit jswriter;

{$mode objfpc}{$H+}
{ $DEFINE DEBUGJSWRITER}

interface

uses
  SysUtils, jstoken, jsbase, jstree;

Type
  TTextWriter = class;

  TTextWriterWriting = procedure(Sender: TTextWriter) of object;

  { TTextWriter }

  TTextWriter = Class(TObject)
  private
    FCurElement: TJSElement;
    FCurLine: integer;
    FCurColumn: integer;
    FOnWriting: TTextWriterWriting;
  protected
    Function DoWrite(Const S : AnsiString) : Integer; virtual; abstract;
    Function DoWrite(Const S : UnicodeString) : Integer; virtual; abstract;
    Procedure Writing; // called before adding new characters
  Public
    // All functions return the number of bytes copied to output stream.
    constructor Create;
    Function Write(Const S : UnicodeString) : Integer;
    Function Write(Const S : AnsiString) : Integer;
    Function WriteLn(Const S : AnsiString) : Integer;
    Function Write(Const Fmt : AnsiString; Args : Array of const) : Integer;
    Function WriteLn(Const Fmt : AnsiString; Args : Array of const) : Integer;
    Function Write(Const Args : Array of const) : Integer;
    Function WriteLn(Const Args : Array of const) : Integer;
    Property CurLine: integer read FCurLine write FCurLine;
    Property CurColumn: integer read FCurColumn write FCurColumn;// char index, not codepoint
    Property CurElement: TJSElement read FCurElement write FCurElement;
    Property OnWriting: TTextWriterWriting read FOnWriting write FOnWriting;
  end;

  { TFileWriter }

  TFileWriter = Class(TTextWriter)
  Protected
    FFile : Text;
    FFileName : String;
    Function DoWrite(Const S : AnsiString) : Integer; override;
    Function DoWrite(Const S : UnicodeString) : Integer; override;
  Public
    Constructor Create(Const AFileNAme : String);
    Destructor Destroy; override;
    Procedure Flush;
    Procedure Close;
    Property FileName : String Read FFileName;
  end;

  { TBufferWriter }

  TBytes = Array of byte;
  TBufferWriter = Class(TTextWriter)
  private
    FBufPos,
    FCapacity: Cardinal;
    FBuffer : TBytes;
    function GetAsAnsistring: AnsiString;
    function GetBuffer: Pointer;
    function GetBufferLength: Integer;
    function GetCapacity: Cardinal;
    function GetUnicodeString: UnicodeString;
    procedure SetCapacity(AValue: Cardinal);
  Protected
    Function DoWrite(Const S : AnsiString) : integer; override;
    Function DoWrite(Const S : UnicodeString) : integer; override;
  Public
    Constructor Create(Const ACapacity : Cardinal);
    Procedure SaveToFile(Const AFileName : String);
    Property Buffer : Pointer Read GetBuffer;
    Property BufferLength : Integer Read GetBufferLength;
    Property Capacity : Cardinal Read GetCapacity Write SetCapacity;
    Property AsAnsistring : AnsiString Read GetAsAnsistring;
    Property AsUnicodeString : UnicodeString Read GetUnicodeString;
  end;

  TJSEscapeQuote = (
    jseqSingle,
    jseqDouble,
    jseqBoth
    );

  { TJSWriter }

  TWriteOption = (woCompact,
                  woUseUTF8,
                  woTabIndent,
                  woEmptyStatementAsComment,
                  woQuoteElementNames,
                  woCompactArrayLiterals,
                  woCompactObjectLiterals,
                  woCompactArguments);
  TWriteOptions = Set of TWriteOption;

  TJSWriter = Class
  private
    FCurIndent : Integer;
    FFreeWriter : Boolean;
    FIndentChar : Char;
    FIndentSize: Byte;
    FLinePos : Integer;
    FOptions: TWriteOptions;
    FSkipCurlyBrackets : Boolean;
    FSkipRoundBrackets : Boolean;
    FWriter: TTextWriter;
    function GetUseUTF8: Boolean;
    procedure SetOptions(AValue: TWriteOptions);
  Protected
    // Helper routines
    Procedure Error(Const Msg : String);
    Procedure Error(Const Fmt : String; Args : Array of const);
    Procedure WriteIndent; // inline;
    Procedure Write(Const U : UnicodeString);
    Procedure Write(Const S : AnsiString);
    Procedure WriteLn(Const S : AnsiString);
    Procedure WriteLn(Const U : UnicodeString);
    // one per type of statement
    Procedure WriteValue(V : TJSValue);  virtual;
    Procedure WriteRegularExpressionLiteral(El: TJSRegularExpressionLiteral);
    Procedure WriteVariableStatement(El: TJSVariableStatement);
    Procedure WriteEmptyBlockStatement(El: TJSEmptyBlockStatement); virtual;
    Procedure WriteEmptyStatement(El: TJSEmptyStatement);virtual;
    Procedure WriteLiteral(El: TJSLiteral);virtual;
    Procedure WriteArrayLiteral(El: TJSArrayLiteral);virtual;
    Procedure WriteObjectLiteral(El: TJSObjectLiteral);virtual;
    Procedure WriteMemberExpression(El: TJSMemberExpression);virtual;
    Procedure WriteCallExpression(El: TJSCallExpression);virtual;
    Procedure WriteSwitchStatement(El: TJSSwitchStatement);virtual;
    Procedure WriteUnary(El: TJSUnary);virtual;
    Procedure WriteAssignStatement(El: TJSAssignStatement);virtual;
    Procedure WriteForInStatement(El: TJSForInStatement);virtual;
    Procedure WriteWhileStatement(El: TJSWhileStatement);virtual;
    Procedure WriteForStatement(El: TJSForStatement);virtual;
    Procedure WriteIfStatement(El: TJSIfStatement);virtual;
    Procedure WriteSourceElements(El: TJSSourceElements);virtual;
    Procedure WriteStatementList(El: TJSStatementList);virtual;
    Procedure WriteTryStatement(El: TJSTryStatement);virtual;
    Procedure WriteVarDeclaration(El: TJSVarDeclaration);virtual;
    Procedure WriteWithStatement(El: TJSWithStatement);virtual;
    Procedure WriteVarDeclarationList(El: TJSVariableDeclarationList);virtual;
    Procedure WriteConditionalExpression(El: TJSConditionalExpression);virtual;
    Procedure WriteFunctionBody(El: TJSFunctionBody);virtual;
    Procedure WriteFunctionDeclarationStatement(El: TJSFunctionDeclarationStatement);virtual;
    Procedure WriteLabeledStatement(El: TJSLabeledStatement);virtual;
    Procedure WriteReturnStatement(El: TJSReturnStatement);virtual;
    Procedure WriteTargetStatement(El: TJSTargetStatement);virtual;
    Procedure WriteFuncDef(FD: TJSFuncDef);virtual;
    Procedure WritePrimaryExpression(El: TJSPrimaryExpression);virtual;
    Procedure WriteBinary(El: TJSBinary);virtual;
    Function IsEmptyStatement(El: TJSElement): boolean;
    Function HasLineEnding(El: TJSElement): boolean;
  Public
    Function EscapeString(const S: TJSString; Quote: TJSEscapeQuote = jseqDouble): TJSString;
    Constructor Create(AWriter : TTextWriter);
    Constructor Create(Const AFileName : String);
    Destructor Destroy; override;
    Procedure WriteJS(El : TJSElement);
    Procedure Indent;
    Procedure Undent;
    Property Writer : TTextWriter Read FWriter;
    Property Options : TWriteOptions Read FOptions Write SetOptions;
    Property IndentSize : Byte Read FIndentSize Write FIndentSize;
    Property UseUTF8 : Boolean Read GetUseUTF8;
  end;
  EJSWriter = Class(Exception);

Function UTF16ToUTF8(const S: UnicodeString): string;

implementation

Resourcestring
  SErrUnknownJSClass = 'Unknown javascript element class : %s';
  SErrNilNode = 'Nil node in Javascript';

function HexDump(p: PChar; Count: integer): string;
var
  i: Integer;
begin
  Result:='';
  for i:=0 to Count-1 do
    Result:=Result+HexStr(ord(p[i]),2);
end;

function UTF16ToUTF8(const S: UnicodeString): string;
begin
  Result:=UTF8Encode(S);
  // prevent UTF8 codepage appear in the strings - we don't need codepage
  // conversion magic
  SetCodePage(RawByteString(Result), CP_ACP, False);
end;

{ TBufferWriter }

function TBufferWriter.GetBufferLength: Integer;
begin
  Result:=FBufPos;
end;

function TBufferWriter.GetAsAnsistring: AnsiString;
begin
  SetLength(Result,BufferLength);
  if (BufferLength>0) then
    Move(FBuffer[0],Result[1],BufferLength);
end;

function TBufferWriter.GetBuffer: Pointer;
begin
  Result:=Pointer(FBuffer);
end;

function TBufferWriter.GetCapacity: Cardinal;
begin
  Result:=Length(FBuffer);
end;

function TBufferWriter.GetUnicodeString: UnicodeString;

Var
  SL : Integer;

begin
  SL:=BufferLength div SizeOf(UnicodeChar); // Silently ignores last byte
  SetLength(Result,SL);
  if (SL>0) then
    Move(FBuffer[0],Result[1],SL*SizeOf(UnicodeChar));
end;

procedure TBufferWriter.SetCapacity(AValue: Cardinal);
begin
  if FCapacity=AValue then Exit;
  SetLength(FBuffer,AValue);
  if (FBufPos>Capacity) then
    FBufPos:=Capacity;
end;

Function TBufferWriter.DoWrite(Const S: AnsiString): integer;

Var
  DesLen,MinLen : Integer;

begin
  Result:=Length(S)*SizeOf(Char);
  if Result=0 then exit;
  MinLen:=Result+FBufPos;
  If (MinLen>Capacity) then
    begin
    DesLen:=(FCapacity*5) div 4;
    if DesLen>MinLen then
      MinLen:=DesLen;
    Capacity:=MinLen;
    end;
  Move(S[1],FBuffer[FBufPos],Result);
  FBufPos:=FBufPos+Result;
end;

Function TBufferWriter.DoWrite(Const S: UnicodeString): integer;

Var
  DesLen,MinLen : Integer;

begin
  Result:=Length(S)*SizeOf(UnicodeChar);
  if Result=0 then exit;
  MinLen:=Result+FBufPos;
  If (MinLen>Capacity) then
    begin
    DesLen:=(FCapacity*5) div 4;
    if DesLen>MinLen then
      MinLen:=DesLen;
    Capacity:=MinLen;
    end;
  Move(S[1],FBuffer[FBufPos],Result);
  FBufPos:=FBufPos+Result;
end;

Constructor TBufferWriter.Create(Const ACapacity: Cardinal);
begin
  inherited Create;
  Capacity:=ACapacity;
end;

Procedure TBufferWriter.SaveToFile(Const AFileName: String);

Var
  F : File;

begin
  Assign(F,AFileName);
  Rewrite(F,1);
  try
    BlockWrite(F,FBuffer[0],FBufPos);
  finally
    Close(F);
  end;
end;

{ TJSWriter }

procedure TJSWriter.SetOptions(AValue: TWriteOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
  If woTabIndent in Foptions then
    FIndentChar:=#9
  else
    FIndentChar:=' ';
end;

function TJSWriter.GetUseUTF8: Boolean;
begin
  Result:=(woUseUTF8 in Options)
end;

procedure TJSWriter.Error(const Msg: String);
begin
  Raise EJSWriter.Create(Msg);
end;

procedure TJSWriter.Error(const Fmt: String; Args: array of const);
begin
  Raise EJSWriter.CreateFmt(Fmt,Args);
end;

procedure TJSWriter.WriteIndent;

begin
  If (FLinePos=0) then
    FLinePos:=Writer.Write(StringOfChar(FIndentChar,FCurIndent));
end;

procedure TJSWriter.Indent;
begin
  Inc(FCurIndent,FIndentSIze);
end;

procedure TJSWriter.Undent;
begin
  if (FCurIndent>=FIndentSIze) then
    Dec(FCurIndent,FIndentSIze)
  else
    FCurIndent:=0;
end;

procedure TJSWriter.Write(const U: UnicodeString);

Var
  S : String;

begin
  WriteIndent;
  if UseUTF8 then
    begin
    S:=UTF16ToUTF8(U);
    FLinePos:=FLinePos+Writer.Write(S);
    end
  else
    FLinePos:=FLinePos+Writer.Write(U);
end;

procedure TJSWriter.Write(const S: AnsiString);
begin
  if Not (woUseUTF8 in Options) then
    Write(UnicodeString(S))
  else
    begin
    WriteIndent;
    FLinePos:=FLinePos+Writer.Write(S);
    end;
end;

procedure TJSWriter.WriteLn(const S: AnsiString);
begin
  if Not (woUseUTF8 in Options) then
    Writeln(UnicodeString(S))
  else
    begin
    WriteIndent;
    Writer.WriteLn(S);
    FLinePos:=0;
    end;
end;

procedure TJSWriter.WriteLn(const U: UnicodeString);
Var
  S : String;

begin
  if UseUTF8 then
    begin
    S:=UTF16ToUTF8(U);
    Writeln(S);
    end
  else
    begin
    WriteIndent;
    FLinePos:=FLinePos+Writer.Write(U);
    FLinePos:=0;
    end;
end;

function TJSWriter.EscapeString(const S: TJSString; Quote: TJSEscapeQuote
  ): TJSString;

Var
  I,J,L : Integer;
  P : TJSPChar;
  R: TJSString;

begin
  I:=1;
  J:=1;
  R:='';
  L:=Length(S);
  P:=TJSPChar(S);
  While I<=L do
    begin
    if (P^ in [#0..#31,'"','''','/','\']) then
      begin
      R:=R+Copy(S,J,I-J);
      Case P^ of
        '\' : R:=R+'\\';
        '/' : R:=R+'\/';
        '"' : if Quote=jseqSingle then R:=R+'"' else R:=R+'\"';
        '''': if Quote=jseqDouble then R:=R+'''' else R:=R+'\''';
        #0..#7,#11,#14..#31: R:=R+'\x'+TJSString(hexStr(ord(P^),2));
        #8  : R:=R+'\b';
        #9  : R:=R+'\t';
        #10 : R:=R+'\n';
        #12 : R:=R+'\f';
        #13 : R:=R+'\r';
      end;
      J:=I+1;
      end;
    Inc(I);
    Inc(P);
    end;
  R:=R+Copy(S,J,I-1);
  Result:=R;
end;

procedure TJSWriter.WriteValue(V: TJSValue);
const
  TabWidth = 4;

  function GetLineIndent(var p: PWideChar): integer;
  var
    h: PWideChar;
  begin
    h:=p;
    Result:=0;
    repeat
      case h^ of
      #0: break;
      #9: Result:=Result+(TabWidth-Result mod TabWidth);
      ' ': inc(Result);
      else break;
      end;
      inc(h);
    until false;
    p:=h;
  end;

  function SkipToNextLineStart(p: PWideChar): PWideChar;
  begin
    repeat
      case p^ of
      #0: break;
      #10,#13:
        begin
        if (p[1] in [#10,#13]) and (p^<>p[1]) then
          inc(p,2)
        else
          inc(p);
        break;
        end
      else inc(p);
      end;
    until false;
    Result:=p;
  end;

Var
  S : String;
  JS: TJSString;
  p, StartP: PWideChar;
  MinIndent, CurLineIndent: Integer;
begin
  if V.CustomValue<>'' then
    begin
    JS:=V.CustomValue;
    if JS='' then exit;

    p:=SkipToNextLineStart(PWideChar(JS));
    if p^=#0 then
      begin
      // simple value
      Write(JS);
      exit;
      end;

    // multi line value

    // find minimum indent
    MinIndent:=-1;
    repeat
      CurLineIndent:=GetLineIndent(p);
      if (MinIndent<0) or (MinIndent>CurLineIndent) then
        MinIndent:=CurLineIndent;
      p:=SkipToNextLineStart(p);
    until p^=#0;

    // write value lines indented
    p:=PWideChar(JS);
    GetLineIndent(p); // the first line is already indented, skip
    repeat
      StartP:=p;
      p:=SkipToNextLineStart(StartP);
      Write(copy(JS,StartP-PWideChar(JS)+1,p-StartP));
      if p^=#0 then break;
      CurLineIndent:=GetLineIndent(p);
      Write(StringOfChar(FIndentChar,FCurIndent+CurLineIndent-MinIndent));
    until false;

    exit;
    end;
  Case V.ValueType of
    jstUNDEFINED : S:='undefined';
    jstNull : s:='null';
    jstBoolean : if V.AsBoolean then s:='true' else s:='false';
    jstString :
      begin
      JS:=V.AsString;
      if Pos('"',JS)>0 then
        JS:=''''+EscapeString(JS,jseqSingle)+''''
      else
        JS:='"'+EscapeString(JS,jseqDouble)+'"';
      Write(JS);
      exit;
      end;
    jstNumber :
      if Frac(V.AsNumber)=0 then // this needs to be improved
        Str(Round(V.AsNumber),S)
      else
        Str(V.AsNumber,S);
    jstObject : ;
    jstReference : ;
    JSTCompletion : ;
  end;
  Write(S);
end;

constructor TJSWriter.Create(AWriter: TTextWriter);
begin
  FWriter:=AWriter;
  FIndentChar:=' ';
  FOptions:=[woUseUTF8];
end;

constructor TJSWriter.Create(const AFileName: String);
begin
  Create(TFileWriter.Create(AFileName));
  FFreeWriter:=True;
end;

destructor TJSWriter.Destroy;
begin
  If FFreeWriter then
    begin
    FWriter.Free;
    FWriter:=Nil;
    end;
  inherited Destroy;
end;

procedure TJSWriter.WriteFuncDef(FD: TJSFuncDef);

Var
  C : Boolean;
  I : Integer;
  A: TJSElement;

begin
  C:=(woCompact in Options);
  Write('function ');
  If (FD.Name<>'') then
    Write(FD.Name);
  Write('(');
  if Assigned(FD.Params) then
    For I:=0 to FD.Params.Count-1 do
      begin
      write(FD.Params[i]);
      if I<FD.Params.Count-1 then
        if C then Write(',') else Write (', ');
      end;
  Write(') {');
  if Not (C or FD.IsEmpty) then
    begin
    Writeln('');
    Indent;
    end;
  if Assigned(FD.Body) then
    begin
    FSkipCurlyBrackets:=True;
    //writeln('TJSWriter.WriteFuncDef '+FD.Body.ClassName);
    WriteJS(FD.Body);
    A:=FD.Body.A;
    If (Assigned(A))
        and (not (A is TJSStatementList))
        and (not (A is TJSSourceElements))
        and (not (A is TJSEmptyBlockStatement))
    then
      if C then
        Write('; ')
      else
        Writeln(';');
    end;
  if C then
    Write('}')
  else
    begin
    Undent;
    Write('}'); // do not writeln
    end;
end;

procedure TJSWriter.WriteEmptyBlockStatement(El: TJSEmptyBlockStatement);
begin
  if El=nil then ;
  if woCompact in Options then
    Write('{}')
  else
    begin
    Writeln('{');
    Write('}');
    end;
end;

procedure TJSWriter.WriteEmptyStatement(El: TJSEmptyStatement);
begin
  if El=nil then ;
  if woEmptyStatementAsComment in Options then
    Write('/* Empty statement */')
end;

procedure TJSWriter.WriteRegularExpressionLiteral(
  El: TJSRegularExpressionLiteral);

begin
  Write('/');
  Write(EscapeString(El.Pattern.AsString,jseqBoth));
  Write('/');
  If Assigned(El.PatternFlags) then
    Write(EscapeString(El.PatternFlags.AsString,jseqBoth));
end;

procedure TJSWriter.WriteLiteral(El: TJSLiteral);
begin
  WriteValue(El.Value);
end;

procedure TJSWriter.WritePrimaryExpression(El: TJSPrimaryExpression);

begin
  if El is TJSPrimaryExpressionThis then
    Write('this')
  else if El is TJSPrimaryExpressionIdent then
    Write(TJSPrimaryExpressionIdent(El).Name)
  else
    Error(SErrUnknownJSClass,[El.ClassName]);
end;

procedure TJSWriter.WriteArrayLiteral(El: TJSArrayLiteral);

Var
  Chars : Array[Boolean] of string[2] = ('[]','()');

Var
  i,C : Integer;
  isArgs,WC , MultiLine: Boolean;
  BC : String[2];

begin
  isArgs:=El is TJSArguments;
  BC:=Chars[isArgs];
  C:=El.Elements.Count-1;
  if C=-1 then
    begin
    Write(bc);
    Exit;
    end;
  WC:=(woCompact in Options) or
      ((Not isArgs) and (woCompactArrayLiterals in Options)) or
      (isArgs and (woCompactArguments in Options)) ;
  MultiLine:=(not WC) and (C>4);
  if MultiLine then
    begin
    Writeln(Copy(BC,1,1));
    Indent;
    end
  else
    Write(Copy(BC,1,1));
  For I:=0 to C do
    begin
    FSkipRoundBrackets:=true;
    WriteJS(El.Elements[i].Expr);
    if I<C then
      if WC then
        Write(',')
      else if MultiLine then
        Writeln(',')
      else
        Write(', ');
    end;
  if MultiLine then
    begin
    Writeln('');
    Undent;
    end;
  Write(Copy(BC,2,1));
end;


procedure TJSWriter.WriteObjectLiteral(El: TJSObjectLiteral);


Var
  i,C : Integer;
  QE,WC : Boolean;
  S : TJSString;

begin
  C:=El.Elements.Count-1;
  QE:=(woQuoteElementNames in Options);
  if C=-1 then
    begin
    Write('{}');
    Exit;
    end;
  WC:=(woCompact in Options) or (woCompactObjectLiterals in Options);
  if WC then
    Write('{')
  else
    begin
    Writeln('{');
    Indent;
    end;
  For I:=0 to C do
   begin
   S:=El.Elements[i].Name;
   if QE or not IsValidJSIdentifier(S) then
     S:='"'+S+'"';
   Write(S+': ');
   Indent;
   FSkipRoundBrackets:=true;
   WriteJS(El.Elements[i].Expr);
   if I<C then
     if WC then Write(', ') else Writeln(',');
   Undent;
   end;
  FSkipRoundBrackets:=false;
  if not WC then
    begin
    Writeln('');
    Undent;
    end;
  Write('}');
end;

procedure TJSWriter.WriteMemberExpression(El: TJSMemberExpression);

var
  MExpr: TJSElement;
  Args: TJSArguments;
begin
  if El is TJSNewMemberExpression then
    Write('new ');
  MExpr:=El.MExpr;
  if (MExpr is TJSPrimaryExpression)
      or (MExpr is TJSDotMemberExpression)
      or (MExpr is TJSBracketMemberExpression)
      // Note: new requires brackets in this case: new (a())()
      or ((MExpr is TJSCallExpression) and not (El is TJSNewMemberExpression))
      or (MExpr is TJSLiteral) then
    WriteJS(MExpr)
  else
    begin
    Write('(');
    WriteJS(MExpr);
    Write(')');
    end;
  if El is TJSDotMemberExpression then
    begin
    write('.');
    Write(TJSDotMemberExpression(El).Name);
    end
  else if El is TJSBracketMemberExpression then
    begin
    write('[');
    FSkipRoundBrackets:=true;
    WriteJS(TJSBracketMemberExpression(El).Name);
    FSkipRoundBrackets:=false;
    write(']');
    end
  else if (El is TJSNewMemberExpression) then
    begin
    Args:=TJSNewMemberExpression(El).Args;
    if Assigned(Args) then
      begin
      Writer.CurElement:=Args;
      WriteArrayLiteral(Args);
      end
    else
      Write('()');
    end;
end;

procedure TJSWriter.WriteCallExpression(El: TJSCallExpression);

begin
  WriteJS(El.Expr);
  if Assigned(El.Args) then
    begin
    Writer.CurElement:=El.Args;
    WriteArrayLiteral(El.Args);
    end
  else
    Write('()');
end;

procedure TJSWriter.WriteUnary(El: TJSUnary);

Var
  S : String;

begin
  FSkipRoundBrackets:=false;
  S:=El.PreFixOperator;
  if (S<>'') then
    Write(S);
  WriteJS(El.A);
  if (S='') then
    begin
    S:=El.PostFixOperator;
    if (S<>'') then
      Write(S);
    end;
end;

procedure TJSWriter.WriteStatementList(El: TJSStatementList);

Var
  C : Boolean;
  B : Boolean;
  LastEl: TJSElement;

begin
  //write('TJSWriter.WriteStatementList '+BoolToStr(FSkipCurlyBrackets,true));
  //if El.A<>nil then write(' El.A='+El.A.ClassName) else write(' El.A=nil');
  //if El.B<>nil then write(' El.B='+El.B.ClassName) else write(' El.B=nil');
  //writeln(' ');

  C:=(woCompact in Options);
  B:= Not FSkipCurlyBrackets;
  if B then
    begin
    Write('{');
    Indent;
    if not C then writeln('');
    end;
  if not IsEmptyStatement(El.A) then
    begin
    WriteJS(El.A);
    LastEl:=El.A;
    if Assigned(El.B) then
      begin
      if not (LastEl is TJSStatementList) then
        begin
        if C then
          Write('; ')
        else
          Writeln(';');
        end;
      FSkipCurlyBrackets:=True;
      WriteJS(El.B);
      LastEl:=El.B;
      end;
    if (not C) and not (LastEl is TJSStatementList) then
      writeln(';');
    end
  else if Assigned(El.B) then
    begin
    WriteJS(El.B);
    if (not C) and not (El.B is TJSStatementList) then
      writeln(';');
    end;
  if B then
    begin
    Undent;
    Write('}'); // do not writeln
    end;
end;

procedure TJSWriter.WriteWithStatement(El: TJSWithStatement);
begin
   Write('with (');
   FSkipRoundBrackets:=true;
   WriteJS(El.A);
   FSkipRoundBrackets:=false;
   if (woCompact in Options) then
     Write(') ')
   else
     WriteLn(')');
   Indent;
   WriteJS(El.B);
   Undent;
end;

procedure TJSWriter.WriteVarDeclarationList(El: TJSVariableDeclarationList);

begin
  WriteJS(El.A);
  If Assigned(El.B) then
    begin
    Write(', ');
    WriteJS(El.B);
    end;
end;

procedure TJSWriter.WriteBinary(El: TJSBinary);

Var
  S : AnsiString;
  AllowCompact, WithBrackets: Boolean;
begin
  {$IFDEF VerboseJSWriter}
  System.writeln('TJSWriter.WriteBinary SkipRoundBrackets=',FSkipRoundBrackets);
  {$ENDIF}
  WithBrackets:=not FSkipRoundBrackets;
  if WithBrackets then
    Write('(');
  FSkipRoundBrackets:=false;
  WriteJS(El.A);
  AllowCompact:=False;
  if (El is TJSBinaryExpression) then
    begin
    S:=TJSBinaryExpression(El).OperatorString;
    AllowCompact:=TJSBinaryExpression(El).AllowCompact;
    end;
  If Not (AllowCompact and (woCompact in Options)) then
    S:=' '+S+' ';
  Write(S);
  WriteJS(El.B);
  if WithBrackets then
    Write(')');
end;

function TJSWriter.IsEmptyStatement(El: TJSElement): boolean;
begin
  if (El=nil) then
    exit(true);
  if (El.ClassType=TJSEmptyStatement) and not (woEmptyStatementAsComment in Options) then
    exit(true);
  Result:=false;
end;

function TJSWriter.HasLineEnding(El: TJSElement): boolean;
begin
  if El<>nil then
    begin
    if (El.ClassType=TJSStatementList) or (El.ClassType=TJSSourceElements) then
      exit(true);
    end;
  Result:=false;
end;

procedure TJSWriter.WriteConditionalExpression(El: TJSConditionalExpression);

begin
  write('(');
  WriteJS(El.A);
  write(' ? ');
  WriteJS(El.B);
  write(' : ');
  WriteJS(El.C);
  write(')');
end;

procedure TJSWriter.WriteAssignStatement(El: TJSAssignStatement);

Var
  S : AnsiString;
begin
  WriteJS(El.LHS);
  S:=El.OperatorString;
  If Not (woCompact in Options) then
    S:=' '+S+' ';
  Write(s);
  FSkipRoundBrackets:=true;
  WriteJS(El.Expr);
  FSkipRoundBrackets:=false;
end;

procedure TJSWriter.WriteVarDeclaration(El: TJSVarDeclaration);

begin
  Write(El.Name);
  if Assigned(El.Init) then
    begin
    Write(' = ');
    FSkipRoundBrackets:=true;
    WriteJS(El.Init);
    FSkipRoundBrackets:=false;
    end;
end;

procedure TJSWriter.WriteIfStatement(El: TJSIfStatement);

var
  HasBTrue, C, HasBFalse, BTrueNeedBrackets: Boolean;
begin
  C:=woCompact in Options;
  Write('if (');
  FSkipRoundBrackets:=true;
  WriteJS(El.Cond);
  FSkipRoundBrackets:=false;
  Write(')');
  If Not C then
    Write(' ');
  HasBTrue:=not IsEmptyStatement(El.BTrue);
  HasBFalse:=not IsEmptyStatement(El.BFalse);
  if HasBTrue then
    begin
    // Note: the 'else' needs {} in front
    BTrueNeedBrackets:=HasBFalse and not (El.BTrue is TJSStatementList)
      and not (El.BTrue is TJSEmptyBlockStatement);
    if BTrueNeedBrackets then
      if C then
        Write('{')
      else
        begin
        Writeln('{');
        Indent;
        end;
    WriteJS(El.BTrue);
    if BTrueNeedBrackets then
      if C then
        Write('}')
      else
        begin
        Undent;
        Writeln('}');
        end;
    end;
  if HasBFalse then
    begin
    if not HasBTrue then
      begin
      if C then
        Write('{}')
      else
        Writeln('{}');
      end
    else
      Write(' ');
    Write('else ');
    WriteJS(El.BFalse)
    end;
end;

procedure TJSWriter.WriteForInStatement(El: TJSForInStatement);

begin
  Write('for (');
  if Assigned(El.LHS) then
    WriteJS(El.LHS);
  Write(' in ');
  if Assigned(El.List) then
    WriteJS(El.List);
  Write(') ');
  if Assigned(El.Body) then
    WriteJS(El.Body);
end;

procedure TJSWriter.WriteForStatement(El: TJSForStatement);

begin
  Write('for (');
  if Assigned(El.Init) then
    WriteJS(El.Init);
  Write('; ');
  if Assigned(El.Cond) then
    begin
    FSkipRoundBrackets:=true;
    WriteJS(El.Cond);
    FSkipRoundBrackets:=false;
    end;
  Write('; ');
  if Assigned(El.Incr) then
    WriteJS(El.Incr);
  Write(') ');
  if Assigned(El.Body) then
    WriteJS(El.Body);
end;

procedure TJSWriter.WriteWhileStatement(El: TJSWhileStatement);


begin
  if El is TJSDoWhileStatement then
    begin
    Write('do ');
    if Assigned(El.Body) then
      begin
      FSkipCurlyBrackets:=false;
      WriteJS(El.Body);
      end;
    Write(' while (');
    If Assigned(El.Cond) then
      begin
      FSkipRoundBrackets:=true;
      WriteJS(EL.Cond);
      FSkipRoundBrackets:=false;
      end;
    Write(')');
    end
  else
    begin
    Write('while (');
    If Assigned(El.Cond) then
      begin
      FSkipRoundBrackets:=true;
      WriteJS(EL.Cond);
      FSkipRoundBrackets:=false;
      end;
    Write(') ');
    if Assigned(El.Body) then
      WriteJS(El.Body);
    end;
end;

procedure TJSWriter.WriteSwitchStatement(El: TJSSwitchStatement);

Var
  C : Boolean;

  Procedure WriteCaseLabel(L : TJSString);

  begin
    Write(l);
  end;

Var
  I : Integer;
  EC : TJSCaseElement;

begin
  C:=(woCompact in Options);
  Write('switch (');
  If Assigned(El.Cond) then
    begin
    FSkipRoundBrackets:=true;
    WriteJS(El.Cond);
    FSkipRoundBrackets:=false;
    end;
  if C then
    Write(') {')
  else
    Writeln(') {');
  For I:=0 to El.Cases.Count-1 do
    begin
    EC:=El.Cases[i];
    if EC=El.TheDefault then
      Write('default')
    else
      begin
      Write('case ');
      FSkipRoundBrackets:=true;
      WriteJS(EC.Expr);
      FSkipRoundBrackets:=false;
      end;
    if Assigned(EC.Body) then
      begin
      FSkipCurlyBrackets:=true;
      If C then
        Write(': ')
      else
        Writeln(':');
      Indent;
      WriteJS(EC.Body);
      Undent;
      if (EC.Body is TJSStatementList) or (EC.Body is TJSEmptyBlockStatement) then
        begin
        if C then
          begin
          if I<El.Cases.Count-1 then
            Write(' ');
          end
        else
          Writeln('');
        end
      else if C then
        Write('; ')
      else
        Writeln(';');
      end
    else
      begin
      if C then
        Write(': ')
      else
        Writeln(':');
      end;
    end;
  Write('}');
end;

procedure TJSWriter.WriteTargetStatement(El: TJSTargetStatement);

Var
  TN : TJSString;

begin
  TN:=El.TargetName;
  if (El is TJSForStatement) then
    WriteForStatement(TJSForStatement(El))
  else if (El is TJSSwitchStatement) then
    WriteSwitchStatement(TJSSwitchStatement(El))
  else if (El is TJSForInStatement) then
    WriteForInStatement(TJSForInStatement(El))
  else if El is TJSWhileStatement then
    WriteWhileStatement(TJSWhileStatement(El))
  else if (El is TJSContinueStatement) then
    begin
    if (TN<>'') then
      Write('continue '+TN)
    else
      Write('continue');
    end
  else if (El is TJSBreakStatement) then
    begin
   if (TN<>'') then
      Write('break '+TN)
    else
      Write('break');
    end
  else
    Error('Unknown target statement class: "%s"',[El.ClassName])
end;

procedure TJSWriter.WriteReturnStatement(El: TJSReturnStatement);

begin
  if El.Expr=nil then
    Write('return')
  else
    begin
    Write('return ');
    FSkipRoundBrackets:=true;
    WriteJS(El.Expr);
    FSkipRoundBrackets:=false;
    end;
end;

procedure TJSWriter.WriteLabeledStatement(El: TJSLabeledStatement);
begin
  if Assigned(El.TheLabel) then
    begin
    Write(El.TheLabel.Name);
    if woCompact in Options then
      Write(': ')
    else
      Writeln(':');
    end;
  // Target ??
  WriteJS(El.A);
end;

procedure TJSWriter.WriteTryStatement(El: TJSTryStatement);

Var
  C : Boolean;

begin
  C:=woCompact in Options;
  Write('try {');
  if not IsEmptyStatement(El.Block) then
    begin
    if Not C then writeln('');
    FSkipCurlyBrackets:=True;
    Indent;
    WriteJS(El.Block);
    if (Not C) and (not (El.Block is TJSStatementList)) then writeln('');
    Undent;
    end;
  Write('}');
  If (El is TJSTryCatchFinallyStatement) or (El is TJSTryCatchStatement) then
    begin
    Write(' catch');
    if El.Ident<>'' then Write(' ('+El.Ident+')');
    If C then
      Write(' {')
    else
      Writeln(' {');
    if not IsEmptyStatement(El.BCatch) then
      begin
      FSkipCurlyBrackets:=True;
      Indent;
      WriteJS(El.BCatch);
      Undent;
      if (Not C) and (not (El.BCatch is TJSStatementList)) then writeln('');
      end;
    Write('}');
    end;
  If (El is TJSTryCatchFinallyStatement) or (El is TJSTryFinallyStatement) then
    begin
    If C then
      Write(' finally {')
    else
      Writeln(' finally {');
    if not IsEmptyStatement(El.BFinally) then
      begin
      Indent;
      FSkipCurlyBrackets:=True;
      WriteJS(El.BFinally);
      Undent;
      if (Not C) and (not (El.BFinally is TJSStatementList)) then writeln('');
      end;
    Write('}');
    end;
end;

procedure TJSWriter.WriteFunctionBody(El: TJSFunctionBody);

begin
  //writeln('TJSWriter.WriteFunctionBody '+El.A.ClassName+' FSkipBrackets='+BoolToStr(FSkipCurlyBrackets,'true','false'));
  if not IsEmptyStatement(El.A) then
    WriteJS(El.A);
end;

procedure TJSWriter.WriteFunctionDeclarationStatement(
  El: TJSFunctionDeclarationStatement);

begin
  if Assigned(El.AFunction) then
    WriteFuncDef(El.AFunction);
end;

procedure TJSWriter.WriteSourceElements(El: TJSSourceElements);

Var
  C : Boolean;

  Procedure WriteElements(Elements: TJSElementNodes);
  Var
    I : Integer;
    E : TJSElement;
  begin
    if Elements=nil then exit;
    For I:=0 to Elements.Count-1 do
      begin
      E:=Elements.Nodes[i].Node;
      WriteJS(E);
      if Not C then
        WriteLn(';')
      else
        if I<Elements.Count-1 then
          Write('; ')
        else
          Write(';')
      end;
  end;

begin
  C:=(woCompact in Options);
  WriteElements(El.Vars);
  WriteElements(El.Functions);
  WriteElements(El.Statements);
end;

procedure TJSWriter.WriteVariableStatement(El: TJSVariableStatement);

begin
  Write('var ');
  WriteJS(El.A);
end;

procedure TJSWriter.WriteJS(El: TJSElement);
var
  LastWritingEl: TJSElement;
begin
{$IFDEF DEBUGJSWRITER}
  if (EL<>Nil) then
    system.Writeln('WriteJS : ',EL.ClassName)
  else
    system.Writeln('WriteJS : El = Nil');
{$ENDIF}
  LastWritingEl:=Writer.CurElement;
  Writer.CurElement:=El;
  if (El is TJSEmptyBlockStatement ) then
    WriteEmptyBlockStatement(TJSEmptyBlockStatement(El))
  else if (El is TJSEmptyStatement) then
    WriteEmptyStatement(TJSEmptyStatement(El))
  else if (El is TJSLiteral) then
    WriteLiteral(TJSLiteral(El))
  else if (El is TJSPrimaryExpression) then
    WritePrimaryExpression(TJSPrimaryExpression(El))
  else if (El is TJSArrayLiteral) then
    WriteArrayLiteral(TJSArrayLiteral(El))
  else if (El is TJSObjectLiteral) then
    WriteObjectLiteral(TJSObjectLiteral(El))
  else if (El is TJSMemberExpression) then
    WriteMemberExpression(TJSMemberExpression(El))
  else if (El is TJSRegularExpressionLiteral) then
    WriteRegularExpressionLiteral(TJSRegularExpressionLiteral(El))
  else if (El is TJSCallExpression) then
    WriteCallExpression(TJSCallExpression(El))
  else if (El is TJSLabeledStatement) then // Before unary
    WriteLabeledStatement(TJSLabeledStatement(El))
  else if (El is TJSFunctionBody) then // Before unary
    WriteFunctionBody(TJSFunctionBody(El))
  else if (El is TJSVariableStatement) then // Before unary
    WriteVariableStatement(TJSVariableStatement(El))
  else if (El is TJSUNary) then
    WriteUnary(TJSUnary(El))
  else if (El is TJSVariableDeclarationList) then
    WriteVarDeclarationList(TJSVariableDeclarationList(El)) // Must be before binary
  else if (El is TJSStatementList) then
    WriteStatementList(TJSStatementList(El)) // Must be before binary
  else if (El is TJSWithStatement) then
    WriteWithStatement(TJSWithStatement(El)) // Must be before binary
  else if (El is TJSBinary) then
    WriteBinary(TJSBinary(El))
  else if (El is TJSConditionalExpression) then
    WriteConditionalExpression(TJSConditionalExpression(El))
  else if (El is TJSAssignStatement) then
    WriteAssignStatement(TJSAssignStatement(El))
  else if (El is TJSVarDeclaration) then
    WriteVarDeclaration(TJSVarDeclaration(El))
  else if (El is TJSIfStatement) then
    WriteIfStatement(TJSIfStatement(El))
  else if (El is TJSTargetStatement) then
    WriteTargetStatement(TJSTargetStatement(El))
  else if (El is TJSReturnStatement) then
    WriteReturnStatement(TJSReturnStatement(El))
  else if (El is TJSTryStatement) then
    WriteTryStatement(TJSTryStatement(El))
  else if (El is TJSFunctionDeclarationStatement) then
    WriteFunctionDeclarationStatement(TJSFunctionDeclarationStatement(El))
  else if (El is TJSSourceElements) then
    WriteSourceElements(TJSSourceElements(El))
  else if El=Nil then
    Error(SErrNilNode)
  else
    Error(SErrUnknownJSClass,[El.ClassName]);
//  Write('/* '+El.ClassName+' */');
  FSkipCurlyBrackets:=False;
  Writer.CurElement:=LastWritingEl;
end;

{ TFileWriter }

Function TFileWriter.DoWrite(Const S: AnsiString) : Integer;
begin
  Result:=Length(S);
  system.Write(FFile,S);
end;

Function TFileWriter.DoWrite(Const S: UnicodeString) : Integer;
begin
  Result:=Length(S)*SizeOf(UnicodeChar);
  system.Write(FFile,S);
end;

Constructor TFileWriter.Create(Const AFileNAme: String);
begin
  inherited Create;
  FFileName:=AFileName;
  Assign(FFile,AFileName);
  Rewrite(FFile);
end;

Destructor TFileWriter.Destroy;
begin
  Close;
  Inherited;
end;

Procedure TFileWriter.Flush;
begin
  system.Flush(FFile);
end;

Procedure TFileWriter.Close;
begin
  system.Close(FFile);
end;

{ TTextWriter }

procedure TTextWriter.Writing;
begin
  if Assigned(OnWriting) then
    OnWriting(Self);
end;

constructor TTextWriter.Create;
begin
  FCurLine:=1;
  FCurColumn:=1;
end;

function TTextWriter.Write(const S: UnicodeString): Integer;
var
  p: PWideChar;
  c: WideChar;
begin
  if S='' then exit;
  Writing;
  Result:=DoWrite(S);
  p:=PWideChar(S);
  repeat
    c:=p^;
    case c of
    #0:
      if p-PWideChar(S)=length(S)*2 then
        break
      else
        inc(FCurColumn);
    #10,#13:
      begin
      FCurColumn:=1;
      inc(FCurLine);
      inc(p);
      if (p^ in [#10,#13]) and (c<>p^) then inc(p);
      continue;
      end;
    else
      // ignore low/high surrogate, CurColumn is char index, not codepoint
      inc(FCurColumn);
    end;
    inc(p);
  until false;
end;

function TTextWriter.Write(const S: AnsiString): Integer;
var
  p: PChar;
  c: Char;
begin
  if S='' then exit;
  Writing;
  Result:=DoWrite(S);
  p:=PChar(S);
  repeat
    c:=p^;
    case c of
    #0:
      if p-PChar(S)=length(S) then
        break
      else
        inc(FCurColumn);
    #10,#13:
      begin
      FCurColumn:=1;
      inc(FCurLine);
      inc(p);
      if (p^ in [#10,#13]) and (c<>p^) then inc(p);
      continue;
      end;
    else
      // ignore UTF-8 multibyte chars, CurColumn is char index, not codepoint
      inc(FCurColumn);
    end;
    inc(p);
  until false;
end;

function TTextWriter.WriteLn(const S: AnsiString): Integer;
begin
  Result:=Write(S)+Write(sLineBreak);
end;

function TTextWriter.Write(const Fmt: AnsiString;
  Args: array of const): Integer;

begin
  Result:=Write(Format(Fmt,Args));
end;

function TTextWriter.WriteLn(const Fmt: AnsiString;
  Args: array of const): Integer;
begin
  Result:=WriteLn(Format(Fmt,Args));
end;

function TTextWriter.Write(const Args: array of const): Integer;

Var
  I : Integer;
  V : TVarRec;
  S : String;
  U : UnicodeString;


begin
  Result:=0;
  For I:=Low(Args) to High(Args) do
    begin
    V:=Args[i];
    S:='';
    U:='';
    case V.VType of
       vtInteger       : Str(V.VInteger,S);
       vtBoolean       : if V.VBoolean then s:='true' else s:='false';
       vtChar          : s:=V.VChar;
       vtWideChar      : U:=V.VWideChar;
       vtExtended      : Str(V.VExtended^,S);
       vtString        : S:=V.VString^;
       vtPChar         : S:=V.VPChar;
       vtPWideChar     : U:=V.VPWideChar;
       vtAnsiString    : S:=PChar(V.VAnsiString);
       vtCurrency      : Str(V.VCurrency^,S);
       vtVariant       : S:=V.VVariant^;
       vtWideString    : U:=PWideChar(V.VWideString);
       vtInt64         : Str(V.VInt64^,S);
       vtUnicodeString : U:=PWideChar(V.VUnicodeString);
       vtQWord         : Str(V.VQWord^,S);
    end;
    if (U<>'') then
      Result:=Result+Write(u)
    else if (S<>'') then
      Result:=Result+Write(s);
    end;
end;

function TTextWriter.WriteLn(const Args: array of const): Integer;
begin
  Result:=Write(Args)+Writeln('');
end;

end.

