unit jswriter;

{$mode objfpc}{$H+}
{ $DEFINE DEBUGJSWRITER}

interface

uses
  {Classes, } SysUtils, jstoken, jsbase, jstree;

Type

  { TTextWriter }

  TTextWriter = Class(TObject)
  protected
    Function DoWrite(Const S : AnsiString) : Integer; virtual; abstract;
    Function DoWrite(Const S : UnicodeString) : Integer; virtual; abstract;
  Public
    // All functions return the numberof bytes copied to output stream.
    Function Write(Const S : UnicodeString) : Integer;
    Function Write(Const S : AnsiString) : Integer;
    Function WriteLn(Const S : AnsiString) : Integer;
    Function Write(Const Fmt : AnsiString; Args : Array of const) : Integer;
    Function WriteLn(Const Fmt : AnsiString; Args : Array of const) : Integer;
    Function Write(Const Args : Array of const) : Integer;
    Function WriteLn(Const Args : Array of const) : Integer;
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
    FLinePos : Integer;
    FIndentSize: Byte;
    FIndentChar : Char;
    FOptions: TWriteOptions;
    FWriter: TTextWriter;
    FFreeWriter : Boolean;
    FSkipBrackets : Boolean;
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
    Procedure WriteVariableStatement(el: TJSVariableStatement);
    Procedure WriteEmptyBlockStatement(El: TJSEmptyBlockStatement); virtual;
    Procedure WriteEmptyStatement(El: TJSEmptyStatement);virtual;
    Procedure WriteLiteral(El: TJSLiteral);virtual;
    Procedure WriteArrayLiteral(El: TJSArrayLiteral);virtual;
    Procedure WriteObjectLiteral(El: TJSObjectLiteral);virtual;
    Procedure WriteMemberExpression(el: TJSMemberExpression);virtual;
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
    Procedure WriteTryStatement(el: TJSTryStatement);virtual;
    Procedure WriteVarDeclaration(El: TJSVarDeclaration);virtual;
    Procedure WriteWithStatement(El: TJSWithStatement);virtual;
    Procedure WriteVarDeclarationList(El: TJSVariableDeclarationList);virtual;
    Procedure WriteConditionalExpression(El: TJSConditionalExpression);virtual;
    Procedure WriteFunctionBody(El: TJSFunctionBody);virtual;
    Procedure WriteFunctionDeclarationStatement(El: TJSFunctionDeclarationStatement);virtual;
    Procedure WriteLabeledStatement(El: TJSLabeledStatement);virtual;
    Procedure WriteReturnStatement(EL: TJSReturnStatement);virtual;
    Procedure WriteTargetStatement(El: TJSTargetStatement);virtual;
    Procedure WriteFuncDef(FD: TJSFuncDef);virtual;
    Procedure WritePrimaryExpression(El: TJSPrimaryExpression);virtual;
    Procedure WriteBinary(El: TJSBinary);virtual;
  Public
    Function EscapeString(const S: TJSString): String;
    Function JSStringToStr(const S: TJSString): string;
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
  EJSWriter = CLass(Exception);

implementation

Resourcestring
  SErrUnknownJSClass = 'Unknown javascript element class : %s';
  SErrNilNode = 'Nil node in Javascript';

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
  MinLen:=Result+FBufPos;
  If (MinLen>Capacity) then
    begin
    DesLen:=Round(FCapacity*1.25);
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
  MinLen:=Result+FBufPos;
  If (MinLen>Capacity) then
    begin
    DesLen:=Round(FCapacity*1.25);
    if DesLen>MinLen then
      MinLen:=DesLen;
    Capacity:=MinLen;
    end;
  Move(S[1],FBuffer[FBufPos],Result);
  FBufPos:=FBufPos+Result;
end;

Constructor TBufferWriter.Create(Const ACapacity: Cardinal);
begin
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
  S : UTF8String;

begin
  WriteIndent;
  if UseUTF8 then
    begin
    S:=UTF8Encode(U);
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
  S : UTF8String;

begin
  if UseUTF8 then
    begin
    S:=UTF8Encode(U);
    Writeln(S);
    end
  else
    begin
    WriteIndent;
    FLinePos:=FLinePos+Writer.Write(U);
    FLinePos:=0;
    end;
end;

function TJSWriter.EscapeString(const S: TJSString): String;

Var
  I,J,L : Integer;
  P : PWideChar;

begin
  I:=1;
  J:=1;
  Result:='';
  L:=Length(S);
  P:=PWideChar(S);
  While I<=L do
    begin
    if (P^ in ['"','/','\',#8,#9,#10,#12,#13]) then
      begin
      Result:=Result+JSStringToStr(Copy(S,J,I-J));
      Case P^ of
        '\' : Result:=Result+'\\';
        '/' : Result:=Result+'\/';
        '"' : Result:=Result+'\"';
        #8  : Result:=Result+'\b';
        #9  : Result:=Result+'\t';
        #10 : Result:=Result+'\n';
        #12 : Result:=Result+'\f';
        #13 : Result:=Result+'\r';
      end;
      J:=I+1;
      end;
    Inc(I);
    Inc(P);
    end;
  Result:=Result+JSStringToStr(Copy(S,J,I-1));
end;

function TJSWriter.JSStringToStr(const S: TJSString): string;
begin
  if UseUTF8 then
    Result:=UTF8Encode(S)
  else
    Result:=String(S);
end;

procedure TJSWriter.WriteValue(V: TJSValue);

Var
  S : String;
begin
  Case V.ValueType of
     jstUNDEFINED : S:='undefined';
     jstNull : s:='null';
     jstBoolean : if V.AsBoolean then s:='true' else s:='false';
     jstString : S:='"'+EscapeString(V.AsString)+'"';
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
    FSkipBrackets:=True;
    //writeln('TJSWriter.WriteFuncDef '+FD.Body.ClassName);
    WriteJS(FD.Body);
    If (Assigned(FD.Body.A))
    and (not (FD.Body.A is TJSStatementList))
    and (not (FD.Body.A is TJSSourceElements))
    and (not (FD.Body.A is TJSEmptyBlockStatement))
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
  Write(EscapeString(El.Pattern.AsString));
  Write('/');
  If Assigned(El.PatternFlags) then
    Write(EscapeString(El.PatternFlags.AsString));
end;

procedure TJSWriter.WriteLiteral(El: TJSLiteral);
begin
  WriteValue(el.Value);
end;

procedure TJSWriter.WritePrimaryExpression(El: TJSPrimaryExpression);

begin
  if El is TJSPrimaryExpressionThis then
    Write('this')
  else if El is TJSPrimaryExpressionIdent then
    Write(TJSPrimaryExpressionIdent(El).Name);
end;

procedure TJSWriter.WriteArrayLiteral(El: TJSArrayLiteral);



Var
  Chars : Array[Boolean] of string[2] = ('[]','()');

Var
  i,C : Integer;
  isArgs,WC : Boolean;
  BC : String[2];

begin
  isArgs:=el is TJSArguments;
  BC:=Chars[isArgs];
  C:=EL.Elements.Count-1;
  if C=-1 then
    begin
    if isArgs then
      Write(bc)
    else
      Write(bc);
    Exit;
    end;
  WC:=(woCompact in Options) or
      ((Not isArgs) and (woCompactArrayLiterals in Options)) or
      (isArgs and (woCompactArguments in Options)) ;
  if WC then
    Write(Copy(BC,1,1))
  else
    begin
    Writeln(Copy(BC,1,1));
    Indent;
    end;
  For I:=0 to C do
   begin
   WriteJS(EL.Elements[i].Expr);
   if I<C then
     if WC then Write(', ') else Writeln(',')
   end;
  if not WC then
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
  C:=EL.Elements.Count-1;
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
   S:=EL.Elements[i].Name;
   if QE then
     S:='"'+S+'"';
   Write(S+': ');
   Indent;
   WriteJS(EL.Elements[i].Expr);
   if I<C then
     if WC then Write(', ') else Writeln(',');
   Undent;
   end;
  if not WC then
    begin
    Writeln('');
    Undent;
    end;
  Write('}');
end;

procedure TJSWriter.WriteMemberExpression(el: TJSMemberExpression);

begin
  if el is TJSNewMemberExpression then
    Write('new ');
  WriteJS(el.MExpr);
  if el is TJSDotMemberExpression then
    begin
    write('.');
    Write(TJSDotMemberExpression(el).Name);
    end
  else if el is TJSBracketMemberExpression then
    begin
    write('[');
    WriteJS(TJSBracketMemberExpression(el).Name);
    write(']');
    end
  else if (el is TJSNewMemberExpression) then
    begin
    if (Assigned(TJSNewMemberExpression(el).Args)) then
      WriteArrayLiteral(TJSNewMemberExpression(el).Args)
    else
      Write('()');
    end;
end;

procedure TJSWriter.WriteCallExpression(El: TJSCallExpression);

begin
  WriteJS(El.Expr);
  if Assigned(El.Args) then
    WriteArrayLiteral(EL.Args)
  else
    Write('()');
end;

procedure TJSWriter.WriteUnary(El: TJSUnary);

Var
  S : String;

begin
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
  //write('TJSWriter.WriteStatementList '+BoolToStr(FSkipBrackets,true));
  //if El.A<>nil then write(' El.A='+El.A.ClassName) else write(' El.A=nil');
  //if El.B<>nil then write(' El.B='+El.B.ClassName) else write(' El.B=nil');
  //writeln(' ');

  C:=(woCompact in Options);
  B:= Not FSkipBrackets;
  if B then
    begin
    Write('{');
    Indent;
    if not C then writeln('');
    end;
  if Assigned(El.A) and (El.A.ClassType<>TJSEmptyBlockStatement) then
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
      FSkipBrackets:=True;
      WriteJS(El.B);
      LastEl:=El.B;
      end;
    if (not C) and not (LastEl is TJSStatementList) then
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
   WriteJS(EL.A);
   if (woCompact in Options) then
     Write(') ')
   else
     WriteLn(')');
   Indent;
   WriteJS(EL.B);
   Undent;
end;

procedure TJSWriter.WriteVarDeclarationList(El: TJSVariableDeclarationList);

begin
  WriteJS(EL.A);
  If Assigned(EL.B) then
    begin
    Write(', ');
    WriteJS(EL.B);
    end;
end;

procedure TJSWriter.WriteBinary(El: TJSBinary);

Var
  S : AnsiString;
  AllowCompact : Boolean;
begin
  Write('(');
  WriteJS(EL.A);
  AllowCompact:=False;
  if (el is TJSBinaryExpression) then
    begin
    S:=TJSBinaryExpression(El).OperatorString;
    AllowCompact:=TJSBinaryExpression(El).AllowCompact;
    end;
  If Not (AllowCompact and (woCompact in Options)) then
    S:=' '+S+' ';
  Write(S);
  WriteJS(EL.B);
  Write(')');
end;

procedure TJSWriter.WriteConditionalExpression(El: TJSConditionalExpression);

begin
  write('(');
  WriteJS(EL.A);
  write(' ? ');
  WriteJS(EL.B);
  write(' : ');
  WriteJS(EL.C);
  write(')');
end;

procedure TJSWriter.WriteAssignStatement(El: TJSAssignStatement);

Var
  S : AnsiString;
begin
  WriteJS(EL.LHS);
  S:=El.OperatorString;
  If Not (woCompact in Options) then
      S:=' '+S+' ';
  Write(s);
  WriteJS(EL.Expr);
end;

procedure TJSWriter.WriteVarDeclaration(El: TJSVarDeclaration);

begin
  Write(EL.Name);
  if Assigned(EL.Init) then
    begin
    Write(' = ');
    WriteJS(EL.Init);
    end;
end;

procedure TJSWriter.WriteIfStatement(El: TJSIfStatement);

begin
  Write('if (');
  WriteJS(EL.Cond);
  Write(') ');
  WriteJS(El.BTrue);
  if Assigned(El.BFalse) then
    begin
    Write(' else ');
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
    WriteJS(El.Cond);
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
      FSkipBrackets:=false;
      WriteJS(El.Body);
      end;
    Write(' while (');
    If Assigned(El.Cond) then
      WriteJS(EL.Cond);
    Write(')');
    end
  else
    begin
    Write('while (');
    If Assigned(El.Cond) then
      WriteJS(EL.Cond);
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
    WriteJS(EL.Cond);
  if C then
    Write(') {')
  else
    Writeln(') {');
  For I:=0 to EL.Cases.Count-1 do
    begin
    EC:=EL.Cases[i];
    if EC=EL.TheDefault then
      Write('default')
    else
      begin
      Write('case ');
      WriteJS(EC.Expr);
      end;
    If C then
      Write(': ')
    else
      Writeln(':');
    if Assigned(EC.Body) then
      begin
      WriteJS(EC.Body);
      if C then
        begin
        if Not ((EC.Body is TJSStatementList) or (EC.Body is TJSEmptyBlockStatement)) then
          write('; ')
        end
      else
        Writeln('');
      end;
    end;
  Write('}');
end;

procedure TJSWriter.WriteTargetStatement(El: TJSTargetStatement);

Var
  TN : TJSString;

begin
  TN:=EL.TargetName;
  if (El is TJSForStatement) then
    WriteForStatement(TJSForStatement(El))
  else if (El is TJSSwitchStatement) then
    WriteSwitchStatement(TJSSwitchStatement(El))
  else if (El is TJSForInStatement) then
    WriteForInStatement(TJSForInStatement(El))
  else if EL is TJSWhileStatement then
    WriteWhileStatement(TJSWhileStatement(El))
  else if (EL is TJSContinueStatement) then
    begin
    if (TN<>'') then
      Write('continue '+TN)
    else
      Write('continue');
    end
  else if (EL is TJSBreakStatement) then
    begin
   if (TN<>'') then
      Write('break '+TN)
    else
      Write('break');
    end
  else
    Error('Unknown target statement class: "%s"',[EL.ClassName])
end;

procedure TJSWriter.WriteReturnStatement(EL: TJSReturnStatement);

begin
  Write('return ');
  WriteJS(EL.Expr);
end;

procedure TJSWriter.WriteLabeledStatement(El: TJSLabeledStatement);
begin
  if Assigned(EL.TheLabel) then
    begin
    Write(EL.TheLabel.Name);
    if woCompact in Options then
      Write(': ')
    else
      Writeln(':');
    end;
  // Target ??
  WriteJS(EL.A);
end;

procedure TJSWriter.WriteTryStatement(el: TJSTryStatement);

Var
  C : Boolean;

begin
  C:=woCompact in Options;
  Write('try {');
  if Not C then writeln('');
  FSkipBrackets:=True;
  Indent;
  WriteJS(El.Block);
  Undent;
  If C then
    Write('} ')
  else
    begin
    Writeln('');
    Writeln('}');
    end;
  If (El is TJSTryCatchFinallyStatement) or (El is TJSTryCatchStatement) then
    begin
    Write('catch ('+El.Ident);
    If C then
      Write(') {')
    else
      Writeln(') {');
    Indent;
    WriteJS(EL.BCatch);
    Undent;
    If C then
      if (El is TJSTryCatchFinallyStatement) then
        Write('} ')
      else
        Write('}')
    else
      begin
      Writeln('');
      Writeln('}');
      end;
    end;
  If (El is TJSTryCatchFinallyStatement) or (El is TJSTryFinallyStatement) then
    begin
    If C then
      Write('finally {')
    else
      Writeln('finally {');
    Indent;
    WriteJS(EL.BFinally);
    Undent;
    If C then
      Write('}')
    else
      begin
      Writeln('');
      Writeln('}');
      end;
    end;
end;

procedure TJSWriter.WriteFunctionBody(El: TJSFunctionBody);

begin
  //writeln('TJSWriter.WriteFunctionBody '+El.A.ClassName+' FSkipBrackets='+BoolToStr(FSkipBrackets,'true','false'));
  if Assigned(El.A) and (not (El.A is TJSEmptyBlockStatement)) then
    WriteJS(El.A);
end;

procedure TJSWriter.WriteFunctionDeclarationStatement(
  El: TJSFunctionDeclarationStatement);

begin
  if Assigned(EL.AFunction) then
    WriteFuncDef(EL.AFunction);
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

procedure TJSWriter.WriteVariableStatement(el: TJSVariableStatement);

begin
  Write('var ');
  WriteJS(EL.A);
end;

procedure TJSWriter.WriteJS(El: TJSElement);
begin
{$IFDEF DEBUGJSWRITER}
  if (EL<>Nil) then
    system.Writeln('WriteJS : ',EL.ClassName)
  else
    system.Writeln('WriteJS : El = Nil');
{$ENDIF}
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
  FSkipBrackets:=False;
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

Function TTextWriter.Write(Const S: UnicodeString) : Integer;
begin
  Result:=DoWrite(S);
end;

Function TTextWriter.Write(Const S: String) : integer;
begin
  Result:=DoWrite(S);
end;

Function TTextWriter.WriteLn(Const S: String) : Integer;
begin
  Result:=DoWrite(S)+DoWrite(sLineBreak);
end;

Function TTextWriter.Write(Const Fmt: String; Args: Array of const) : Integer;

begin
  Result:=DoWrite(Format(Fmt,Args));
end;

Function TTextWriter.WriteLn(Const Fmt: String; Args: Array of const) : integer;
begin
  Result:=WriteLn(Format(Fmt,Args));
end;

Function TTextWriter.Write(Const Args: Array of const) : Integer;

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
      Result:=Result+write(s);
    end;
end;

Function TTextWriter.WriteLn(Const Args: Array of const) : integer;
begin
  Result:=Write(Args)+Writeln('');
end;

end.

