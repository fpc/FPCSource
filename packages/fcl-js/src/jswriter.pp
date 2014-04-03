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
                  woQuoteElementNames);
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
    Procedure WriteFunctionBody(el: TJSFunctionBody);virtual;
    Procedure WriteFunctionDeclarationStatement(El: TJSFunctionDeclarationStatement);virtual;
    Procedure WriteLabeledStatement(El: TJSLabeledStatement);virtual;
    Procedure WriteReturnStatement(EL: TJSReturnStatement);virtual;
    Procedure WriteTargetStatement(El: TJSTargetStatement);virtual;
    Procedure WriteFuncDef(FD: TJSFuncDef);virtual;
    Procedure WritePrimaryExpression(El: TJSPrimaryExpression);virtual;
    Procedure WriteBinary(El: TJSBinary);virtual;
  Public
    Class Function EscapeString(const S: TJSString): TJSString;
    Constructor Create(AWriter : TTextWriter);
    Constructor Create(Const AFileName : String);
    Destructor Destroy; override;
    Procedure WriteJS(El : TJSElement);
    Procedure Indent;
    Procedure Undent;
    Property Writer : TTextWriter Read FWriter;
    Property options : TWriteOptions Read FOptions Write SetOptions;
    Property IndentSize : Byte Read FIndentSize Write FIndentSize;
    Property UseUTF8 : Boolean Read GetUseUTF8;
  end;
  EJSWriter = CLass(Exception);

implementation

Resourcestring
  SErrUnknownJSClass = 'Unknown javascript element class : %s';

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

Procedure TJSWriter.Error(Const Msg: String);
begin
  Raise EJSWriter.Create(Msg);
end;

Procedure TJSWriter.Error(Const Fmt: String; Args: Array of const);
begin
  Raise EJSWriter.CreateFmt(Fmt,Args);
end;

Procedure TJSWriter.WriteIndent;

begin
  If (FLinePos=0) then
    FLinePos:=Writer.Write(StringOfChar(FIndentChar,FCurIndent));
end;

Procedure TJSWriter.Indent;
begin
  Inc(FCurIndent,FIndentSIze);
end;

Procedure TJSWriter.Undent;
begin
  if (FCurIndent>=FIndentSIze) then
    Dec(FCurIndent,FIndentSIze)
  else
    FCurIndent:=0;
end;

Procedure TJSWriter.Write(Const U: UnicodeString);

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

Procedure TJSWriter.Write(Const S: AnsiString);
begin
  if Not (woUseUTF8 in Options) then
    Write(UnicodeString(S))
  else
    begin
    WriteIndent;
    FLinePos:=FLinePos+Writer.Write(S);
    end;
end;

Procedure TJSWriter.WriteLn(Const S: AnsiString);
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

Procedure TJSWriter.WriteLn(Const U: UnicodeString);
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

Class Function TJSWriter.EscapeString(const S : TJSString) : TJSString;

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
    if (AnsiChar(P^) in ['"','/','\',#8,#9,#10,#12,#13]) then
      begin
      Result:=Result+Copy(S,J,I-J);
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
  Result:=Result+Copy(S,J,I-1);
end;

Procedure TJSWriter.WriteValue(V: TJSValue);

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

Constructor TJSWriter.Create(AWriter: TTextWriter);
begin
  FWriter:=AWriter;
  FIndentChar:=' ';
  FOptions:=[woUseUTF8];
end;

Constructor TJSWriter.Create(Const AFileName: String);
begin
  Create(TFileWriter.Create(AFileName));
  FFreeWriter:=True;
end;

Destructor TJSWriter.Destroy;
begin
  If FFreeWriter then
    begin
    FWriter.Free;
    FWriter:=Nil;
    end;
  inherited Destroy;
end;

Procedure TJSWriter.WriteFuncDef(FD: TJSFuncDef);

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
    indent;
    end;
  if Assigned(FD.Body) then
    begin
    FSkipBrackets:=True;
    WriteJS(FD.Body);
    If not (FD.Body.A is TJSStatementList) then
      if C then
        Write('; ')
      else
        Writeln(';');
    end;
  if C then
    Write('}')
  else
    begin
    undent;
    Writeln('}');
    end;
end;

Procedure TJSWriter.WriteEmptyBlockStatement(El: TJSEmptyBlockStatement);
begin
  if woCompact in Options then
    Write('{}')
  else
    begin
    Writeln('{');
    Write('}');
    end;
end;

Procedure TJSWriter.WriteEmptyStatement(El: TJSEmptyStatement);
begin
  if woEmptyStatementAsComment in options then
    Write('/* Empty statement */')
end;

Procedure TJSWriter.WriteRegularExpressionLiteral(El: TJSRegularExpressionLiteral);

begin
  Write('/');
  Write(EscapeString(EL.Pattern.AsString));
  Write('/');
  If Assigned(EL.PatternFlags) then
    Write(EscapeString(EL.PatternFlags.AsString));
end;

Procedure TJSWriter.WriteLiteral(El: TJSLiteral);
begin
  WriteValue(el.Value);
end;

Procedure TJSWriter.WritePrimaryExpression(El: TJSPrimaryExpression);

begin
  if El is TJSPrimaryExpressionThis then
    Write('this')
  else if el is TJSPrimaryExpressionIdent then
    Write(TJSPrimaryExpressionIdent(El).Name);
end;

Procedure TJSWriter.WriteArrayLiteral(El : TJSArrayLiteral);



Var
  Chars : Array[Boolean] of string[2] = ('[]','()');

Var
  i,C : Integer;
  WC : Boolean;
  BC : String[2];
begin
  BC:=Chars[El is TJSArguments];
  C:=EL.Elements.Count-1;
  if C=-1 then
    begin
    if el is TJSArguments then
      Write(bc)
    else
      Write(bc);
    Exit;
    end;
  WC:=(woCompact in Options);
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


Procedure TJSWriter.WriteObjectLiteral(El : TJSObjectLiteral);


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
  WC:=(woCompact in Options);
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

Procedure TJSWriter.WriteMemberExpression(el : TJSMemberExpression);

Var
  I : integer;
  A : TJSArguments;
begin
  if el is TJSNewMemberExpression then
    Write('new ');
  WriteJS(el.mexpr);
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

Procedure TJSWriter.WriteCallExpression(El : TJSCallExpression);

Var
  I : integer;
  A : TJSArguments;
begin
  WriteJS(El.Expr);
  if Assigned(El.Args) then
    WriteArrayLiteral(EL.Args)
  else
    Write('()');
end;

Procedure TJSWriter.WriteUnary(El : TJSUnary);

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

Procedure TJSWriter.WriteStatementList(El : TJSStatementList);

Var
  C : Boolean;
  B : Boolean;

begin
  C:=(woCompact in Options);
  B:= Not FSkipBrackets;
  if B then
    begin
    Write('{');
    if not C then writeln('');
    end;
  if Assigned(EL.A) then
    begin
    WriteJS(EL.A);
    if Assigned(EL.B) then
      begin
      if C then
        Write('; ')
      else
        Writeln(';');
      FSkipBrackets:=True;
      WriteJS(EL.B);
      end;
    if not C then writeln(';');
    end;
  if B then
    begin
    Write('}');
    if not C then writeln('');
    end;
end;

Procedure TJSWriter.WriteWithStatement(El : TJSWithStatement);
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

Procedure TJSWriter.WriteVarDeclarationList(El : TJSVariableDeclarationList);

begin
  WriteJS(EL.A);
  If Assigned(EL.B) then
    begin
    Write(', ');
    WriteJS(EL.B);
    end;
end;

Procedure TJSWriter.WriteBinary(El : TJSBinary);

Var
  S : AnsiString;
  B : Boolean;
  T : TJSToken;

begin
  Write('(');
  WriteJS(EL.A);
  B:=False;
  if (el is TJSBinaryExpression) then
    begin
    S:=TJSBinaryExpression(El).OperatorString;
    B:=TJSBinaryExpression(El).AllowCompact;
    end;
  If Not (B and (woCompact in Options)) then
    S:=' '+S+' ';
  Write(s);
  WriteJS(EL.B);
  Write(')');
end;

Procedure TJSWriter.WriteConditionalExpression(El : TJSConditionalExpression);

begin
  write('(');
  WriteJS(EL.A);
  write(' ? ');
  WriteJS(EL.B);
  write(' : ');
  WriteJS(EL.C);
  write(')');
end;

Procedure TJSWriter.WriteAssignStatement(El : TJSAssignStatement);

Var
  S : AnsiString;
  T : TJSToken;
begin
  WriteJS(EL.LHS);
  S:=El.OperatorString;
  If Not (woCompact in Options) then
      S:=' '+S+' ';
  Write(s);
  WriteJS(EL.Expr);
end;

Procedure TJSWriter.WriteVarDeclaration(El : TJSVarDeclaration);

begin
  Write(EL.Name);
  if Assigned(EL.Init) then
    begin
    Write(' = ');
    WriteJS(EL.Init);
    end;
end;

Procedure TJSWriter.WriteIfStatement(El : TJSIfStatement);

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

Procedure TJSWriter.WriteForInStatement(El : TJSForInStatement);

begin
  Write('for (');
  if Assigned(El.LHS) then
    WriteJS(El.LHS);
  Write(' in ');
  if Assigned(El.List) then
    WriteJS(El.List);
  Write(') ');
  if Assigned(El.body) then
    WriteJS(El.Body);
end;

Procedure TJSWriter.WriteForStatement(El : TJSForStatement);

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
  if Assigned(El.body) then
    WriteJS(El.Body);
end;

Procedure TJSWriter.WriteWhileStatement(El : TJSWhileStatement);


begin
  if El is TJSDoWhileStatement then
    begin
    Write('do ');
    if Assigned(El.body) then
      WriteJS(El.Body);
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
    if Assigned(El.body) then
      WriteJS(El.Body);
    end;
end;

Procedure TJSWriter.WriteSwitchStatement(El : TJSSwitchStatement);

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

Procedure TJSWriter.WriteTargetStatement(El : TJSTargetStatement);

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

Procedure TJSWriter.WriteReturnStatement(EL: TJSReturnStatement);

begin
  Write('return ');
  WriteJS(EL.Expr);
end;

Procedure TJSWriter.WriteLabeledStatement(El : TJSLabeledStatement);
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

Procedure TJSWriter.WriteTryStatement(el :TJSTryStatement);

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

Procedure TJSWriter.WriteFunctionBody(el : TJSFunctionBody);

begin
  if Assigned(EL.A) then
    WriteJS(EL.A);
end;

Procedure TJSWriter.WriteFunctionDeclarationStatement(El : TJSFunctionDeclarationStatement);

begin
  if Assigned(EL.AFunction) then
    WriteFuncDef(EL.AFunction);
end;

Procedure TJSWriter.WriteSourceElements(El :TJSSourceElements);

Var
  I : Integer;
  C : Boolean;
  E : TJSElement;

begin
  C:=(woCompact in Options);
  For I:=0 to EL.Statements.Count-1 do
    begin
    E:=EL.Statements.Nodes[i].Node;
    WriteJS(E);
    if Not C then
      WriteLn(';')
    else
      if I<EL.Statements.Count-1 then
        Write('; ')
      else
        Write(';')
    end;
end;


Procedure TJSWriter.WriteVariableStatement(el : TJSVariableStatement);

begin
  Write('var ');
  WriteJS(EL.A);
end;

Procedure TJSWriter.WriteJS(El: TJSElement);
begin
{$IFDEF DEBUGJSWRITER}
  if (EL<>Nil) then
    system.Writeln('WriteJS : ',EL.ClassName)
  else
    system.Writeln('WriteJS : El = Nil');
{$ENDIF}
  if (El is TJSEmptyBlockStatement ) then
    WriteEmptyBlockStatement(TJSEmptyBlockStatement(el))
  else if (El is TJSEmptyStatement) then
    WriteEmptyStatement(TJSEmptyStatement(el))
  else if (el is TJSLiteral) then
    WriteLiteral(TJSLiteral(el))
  else if (el is TJSPrimaryExpression) then
    WritePrimaryExpression(TJSPrimaryExpression(el))
  else if (el is TJSArrayLiteral) then
    WriteArrayLiteral(TJSArrayLiteral(el))
  else if (el is TJSObjectLiteral) then
    WriteObjectLiteral(TJSObjectLiteral(el))
  else if (el is TJSMemberExpression) then
    WriteMemberExpression(TJSMemberExpression(el))
  else if (el is TJSRegularExpressionLiteral) then
    WriteRegularExpressionLiteral(TJSRegularExpressionLiteral(El))
  else if (el is TJSCallExpression) then
    WriteCallExpression(TJSCallExpression(el))
  else if (el is TJSLabeledStatement) then // Before unary
    WriteLabeledStatement(TJSLabeledStatement(el))
  else if (el is TJSFunctionBody) then // Before unary
    WriteFunctionBody(TJSFunctionBody(el))
  else if (el is TJSVariableStatement) then // Before unary
    WriteVariableStatement(TJSVariableStatement(el))
  else if (el is TJSUNary) then
    WriteUnary(TJSUnary(el))
  else if (el is TJSVariableDeclarationList) then
    WriteVarDeclarationList(TJSVariableDeclarationList(el)) // Must be before binary
  else if (el is TJSStatementList) then
    WriteStatementList(TJSStatementList(el)) // Must be before binary
  else if (el is TJSWithStatement) then
    WriteWithStatement(TJSWithStatement(El)) // Must be before binary
  else if (el is TJSBinary) then
    WriteBinary(TJSBinary(el))
  else if (el is TJSConditionalExpression) then
    WriteConditionalExpression(TJSConditionalExpression(el))
  else if (el is TJSAssignStatement) then
    WriteAssignStatement(TJSAssignStatement(el))
  else if (el is TJSVarDeclaration) then
    WriteVarDeclaration(TJSVarDeclaration(el))
  else if (el is TJSIfStatement) then
    WriteIfStatement(TJSIfStatement(el))
  else if (el is TJSTargetStatement) then
    WriteTargetStatement(TJSTargetStatement(el))
  else if (el is TJSReturnStatement) then
    WriteReturnStatement(TJSReturnStatement(el))
  else if (el is TJSTryStatement) then
    WriteTryStatement(TJSTryStatement(el))
  else if (el is TJSFunctionDeclarationStatement) then
    WriteFunctionDeclarationStatement(TJSFunctionDeclarationStatement(el))
  else if (el is TJSSourceElements) then
    WriteSourceElements(TJSSourceElements(el))
  else
    Error(SErrUnknownJSClass,[El.ClassName]);
//  Write('/* '+EL.ClassName+' */');
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

