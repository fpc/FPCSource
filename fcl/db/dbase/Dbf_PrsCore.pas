unit Dbf_PrsCore;

{force CR/LF fix}

{--------------------------------------------------------------
| TCustomExpressionParser
|
| - contains core expression parser
|---------------------------------------------------------------}

interface

{$I Dbf_Common.inc}

uses
  SysUtils,
  Classes,
  Dbf_Common,
  Dbf_PrsSupp,
  Dbf_PrsDef;

{$define ENG_NUMBERS}

// ENG_NUMBERS will force the use of english style numbers 8.1 instead of 8,1
//   (if the comma is your decimal separator)
// the advantage is that arguments can be separated with a comma which is
// fairly common, otherwise there is ambuigity: what does 'var1,8,4,4,5' mean?
// if you don't define ENG_NUMBERS and DecimalSeparator is a comma then
// the argument separator will be a semicolon ';'

type

  TCustomExpressionParser = class(TObject)
  private
    FHexChar: Char;
    FArgSeparator: Char;
    FDecimalSeparator: Char;
    FOptimize: Boolean;
    FConstantsList: TOCollection;
    FLastRec: PExpressionRec;
    FCurrentRec: PExpressionRec;
    FExpResult: PChar;
    FExpResultPos: PChar;
    FExpResultSize: Integer;

    procedure ParseString(AnExpression: string; DestCollection: TExprCollection);
    function  MakeTree(Expr: TExprCollection; FirstItem, LastItem: Integer): PExpressionRec;
    procedure MakeLinkedList(var ExprRec: PExpressionRec; Memory: PPChar;
        MemoryPos: PPChar; MemSize: PInteger);
    procedure Check(AnExprList: TExprCollection);
    procedure CheckArguments(ExprRec: PExpressionRec);
    procedure RemoveConstants(var ExprRec: PExpressionRec);
    function ResultCanVary(ExprRec: PExpressionRec): Boolean;
  protected
    FWordsList: TSortedCollection;

    function MakeRec: PExpressionRec; virtual;
    procedure FillExpressList; virtual; abstract;
    procedure HandleUnknownVariable(VarName: string); virtual; abstract;

    procedure CompileExpression(AnExpression: string);
    procedure EvaluateCurrent;
    procedure ReplaceExprWord(OldExprWord, NewExprWord: TExprWord); virtual;
    procedure DisposeList(ARec: PExpressionRec);
    procedure DisposeTree(ExprRec: PExpressionRec);
    function CurrentExpression: string; virtual; abstract;
    function GetResultType: TExpressionType; virtual;

    property CurrentRec: PExpressionRec read FCurrentRec write FCurrentRec;
    property LastRec: PExpressionRec read FLastRec write FLastRec;
    property ExpResult: PChar read FExpResult;
    property ExpResultPos: PChar read FExpResultPos write FExpResultPos;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddReplaceExprWord(AExprWord: TExprWord);
    procedure DefineFloatVariable(AVarName: string; AValue: PDouble);
    procedure DefineIntegerVariable(AVarName: string; AValue: PInteger);
//    procedure DefineSmallIntVariable(AVarName: string; AValue: PSmallInt);
{$ifdef SUPPORT_INT64}
    procedure DefineLargeIntVariable(AVarName: string; AValue: PLargeInt);
{$endif}
    procedure DefineDateTimeVariable(AVarName: string; AValue: PDateTimeRec);
    procedure DefineBooleanVariable(AVarName: string; AValue: PBoolean);
    procedure DefineStringVariable(AVarName: string; AValue: PPChar);
    procedure DefineStringVariableFixedLen(AVarName: string; AValue: PPChar; ALength: Integer);
    procedure DefineFunction(AFunctName, AShortName, ADescription, ATypeSpec: string;
        AMinFunctionArg: Integer; AResultType: TExpressionType; AFuncAddress: TExprFunc);
    procedure ReplaceFunction(OldName: string; AFunction: TObject);
    procedure Evaluate(AnExpression: string);
    function AddExpression(AnExpression: string): Integer;
    procedure ClearExpressions; virtual;
//    procedure GetGeneratedVars(AList: TList);
    procedure GetFunctionNames(AList: TStrings);
    function GetFunctionDescription(AFunction: string): string;
    property HexChar: Char read FHexChar write FHexChar;
    property ArgSeparator: Char read FArgSeparator write FArgSeparator;
    property Optimize: Boolean read FOptimize write FOptimize;
    property ResultType: TExpressionType read GetResultType;


    //if optimize is selected, constant expressions are tried to remove
    //such as: 4*4*x is evaluated as 16*x and exp(1)-4*x is repaced by 2.17 -4*x
  end;


implementation

{ TCustomExpressionParser }

constructor TCustomExpressionParser.Create;
begin
  inherited;

  FHexChar := '$';
{$IFDEF ENG_NUMBERS}
  FDecimalSeparator := '.';
  FArgSeparator := ',';
{$ELSE}
  FDecimalSeparator := DecimalSeparator;
  if DecimalSeparator = ',' then
    FArgSeparator := ';'
  else
    FArgSeparator := ',';
{$ENDIF}
  FConstantsList := TOCollection.Create;
  FWordsList := TExpressList.Create;
  GetMem(FExpResult, ArgAllocSize);
  FExpResultPos := FExpResult;
  FExpResultSize := ArgAllocSize;
  FOptimize := true;
  FillExpressList;
end;

destructor TCustomExpressionParser.Destroy;
begin
  ClearExpressions;
  FreeMem(FExpResult);
  FConstantsList.Free;
  FWordsList.Free;

  inherited;
end;

procedure TCustomExpressionParser.CompileExpression(AnExpression: string);
var
  ExpColl: TExprCollection;
  ExprTree: PExpressionRec;
begin
  if Length(AnExpression) > 0 then
  begin
    ExprTree := nil;
    ExpColl := TExprCollection.Create;
    try
      //    FCurrentExpression := anExpression;
      ParseString(AnExpression, ExpColl);
      Check(ExpColl);
      ExprTree := MakeTree(ExpColl, 0, ExpColl.Count - 1);
      FCurrentRec := nil;
      CheckArguments(ExprTree);
      if Optimize then
        RemoveConstants(ExprTree);
      // all constant expressions are evaluated and replaced by variables
      FCurrentRec := nil;
      FExpResultPos := FExpResult;
      MakeLinkedList(ExprTree, @FExpResult, @FExpResultPos, @FExpResultSize);
    except
      on E: Exception do
      begin
        DisposeTree(ExprTree);
        ExpColl.Free;
        raise;
      end;
    end;
    ExpColl.Free;
  end;
end;

procedure TCustomExpressionParser.CheckArguments(ExprRec: PExpressionRec);
var
  TempExprWord: TExprWord;
  I, error: Integer;
  foundAltFunc: Boolean;
begin
  with ExprRec^ do
  begin
    repeat
      I := 0;
      error := 0;
      foundAltFunc := false;
      while (I < ExprWord.MaxFunctionArg) and (ArgList[I] <> nil) and (error = 0) do
      begin
        // test subarguments first
        CheckArguments(ArgList[I]);

        // test if correct type
        if (ArgList[I].ExprWord.ResultType <> ExprCharToExprType(ExprWord.TypeSpec[I+1])) then
          error := 2;

        // goto next argument
        Inc(I);
      end;

      // test if enough parameters passed; I = num args user passed
      if (error = 0) and (I < ExprWord.MinFunctionArg) then
        error := 1;

      // test if too many parameters passed
      if (error = 0) and (I > ExprWord.MaxFunctionArg) then
        error := 3;

      // error occurred?
      if error <> 0 then
      begin
        // see if we can find another function
        I := FWordsList.IndexOf(ExprWord);
        // check if not last function
        if I < FWordsList.Count - 1 then
        begin
          TempExprWord := FWordsList.Items[I+1];
          if FWordsList.Compare(FWordsList.KeyOf(ExprWord), FWordsList.KeyOf(TempExprWord)) = 0 then
          begin
            ExprWord := TempExprWord;
            Oper := ExprWord.ExprFunc;
            foundAltFunc := true;
          end;
        end;
      end;
    until (error = 0) or not foundAltFunc;

    // fatal error?
    case error of
      1: raise EParserException.Create('Function or operand has too few arguments');
      2: raise EParserException.Create('Argument type mismatch');
      3: raise EParserException.Create('Function or operand has too many arguments');
    end;
  end;
end;

function TCustomExpressionParser.ResultCanVary(ExprRec: PExpressionRec):
  Boolean;
var
  I: Integer;
begin
  with ExprRec^ do
  begin
    Result := ExprWord.CanVary;
    if not Result then
      for I := 0 to ExprWord.MaxFunctionArg - 1 do
        if ResultCanVary(ArgList[I]) then
        begin
          Result := true;
          Exit;
        end
  end;
end;

procedure TCustomExpressionParser.RemoveConstants(var ExprRec: PExpressionRec);
var
  I: Integer;
begin
  if not ResultCanVary(ExprRec) then
  begin
    if not ExprRec^.ExprWord.IsVariable then
    begin
      // reset current record so that make list generates new
      FCurrentRec := nil;
      FExpResultPos := FExpResult;
      MakeLinkedList(ExprRec, @FExpResult, @FExpResultPos, @FExpResultSize);

      try
        // compute result
        EvaluateCurrent;

        // make new record to store constant in
        ExprRec := MakeRec;

        // check result type
        with ExprRec^ do
        begin
          case ResultType of
            etBoolean: ExprWord := TBooleanConstant.Create(EmptyStr, PBoolean(FExpResult)^);
            etFloat: ExprWord := TFloatConstant.CreateAsDouble(EmptyStr, PDouble(FExpResult)^);
            etString: ExprWord := TStringConstant.Create(FExpResult);
          end;

          // fill in structure
          Oper := ExprWord.ExprFunc;
          Args[0] := ExprWord.AsPointer;
          FConstantsList.Add(ExprWord);
        end;
      finally
        DisposeList(FCurrentRec);
        FCurrentRec := nil;
      end;
    end;
  end else
    with ExprRec^ do
    begin
      for I := 0 to ExprWord.MaxFunctionArg - 1 do
        if ArgList[I] <> nil then
          RemoveConstants(ArgList[I]);
    end;
end;

procedure TCustomExpressionParser.DisposeTree(ExprRec: PExpressionRec);
var
  I: Integer;
begin
  if ExprRec <> nil then
  begin
    with ExprRec^ do
    begin
      if ExprWord <> nil then
        for I := 0 to ExprWord.MaxFunctionArg - 1 do
          DisposeTree(ArgList[I]);
      if Res <> nil then
        Res.Free;
    end;
    Dispose(ExprRec);
  end;
end;

procedure TCustomExpressionParser.DisposeList(ARec: PExpressionRec);
var
  TheNext: PExpressionRec;
  I: Integer;
begin
  if ARec <> nil then
    repeat
      TheNext := ARec.Next;
      if ARec.Res <> nil then
        ARec.Res.Free;
      I := 0;
      while ARec.ArgList[I] <> nil do
      begin
        FreeMem(ARec.Args[I]);
        Inc(I);
      end;
      Dispose(ARec);
      ARec := TheNext;
    until ARec = nil;
end;

procedure TCustomExpressionParser.MakeLinkedList(var ExprRec: PExpressionRec;
  Memory: PPChar; MemoryPos: PPChar; MemSize: PInteger);
var
  I: Integer;
begin
  // test function type
  if @ExprRec^.ExprWord.ExprFunc = nil then
  begin
    // special 'no function' function
    // indicates no function is present -> we can concatenate all instances
    // we don't create new arguments...these 'fall' through
    // use destination as we got it
    I := 0;
    while ExprRec^.ArgList[I] <> nil do
    begin
      // convert arguments to list
      MakeLinkedList(ExprRec^.ArgList[I], Memory, MemoryPos, MemSize);
      // goto next argument
      Inc(I);
    end;
    // don't need this record anymore
    Dispose(ExprRec);
    ExprRec := nil;
  end else begin
    // inc memory pointer so we know if we are first
    ExprRec^.ResetDest := MemoryPos^ = Memory^;
    Inc(MemoryPos^);
    // convert arguments to list
    I := 0;
    while ExprRec^.ArgList[I] <> nil do
    begin
      // save variable type for easy access
      ExprRec^.ArgsType[I] := ExprRec^.ArgList[I].ExprWord.ResultType;
      // check if we need to copy argument, variables in general do not
      // need copying, except for fixed len strings which are not
      // null-terminated
//      if ExprRec^.ArgList[I].ExprWord.NeedsCopy then
//      begin
        // get memory for argument
        GetMem(ExprRec^.Args[I], ArgAllocSize);
        ExprRec^.ArgsPos[I] := ExprRec^.Args[I];
        ExprRec^.ArgsSize[I] := ArgAllocSize;
        MakeLinkedList(ExprRec^.ArgList[I], @ExprRec^.Args[I], @ExprRec^.ArgsPos[I],
            @ExprRec^.ArgsSize[I]);
//      end else begin
        // copy reference
//        ExprRec^.Args[I] := ExprRec^.ArgList[I].Args[0];
//        ExprRec^.ArgsPos[I] := ExprRec^.Args[I];
//        ExprRec^.ArgsSize[I] := 0;
//        FreeMem(ExprRec^.ArgList[I]);
//        ExprRec^.ArgList[I] := nil;
//      end;

      // goto next argument
      Inc(I);
    end;

    // link result to target argument
    ExprRec^.Res := TDynamicType.Create(Memory, MemoryPos, MemSize);

    // link to next operation
    if FCurrentRec = nil then
    begin
      FCurrentRec := ExprRec;
      FLastRec := ExprRec;
    end else begin
      FLastRec.Next := ExprRec;
      FLastRec := ExprRec;
    end;
  end;
end;

function TCustomExpressionParser.MakeTree(Expr: TExprCollection; 
  FirstItem, LastItem: Integer): PExpressionRec;

{
- This is the most complex routine, it breaks down the expression and makes
  a linked tree which is used for fast function evaluations
- it is implemented recursively
}

var
  I, IArg, IStart, IEnd, lPrec, brCount: Integer;
  redundantBrackets: boolean;
  ExprWord: TExprWord;
begin
  // remove redundant brackets
  repeat
    redundantBrackets := false;
    if (TExprWord(Expr.Items[FirstItem]).ResultType = etLeftBracket) and
        (TExprWord(Expr.Items[LastItem]).ResultType = etRightBracket) then
    begin
      Inc(FirstItem);
      Dec(LastItem);
      redundantBrackets := true;
    end;
  until not redundantBrackets;

  // check for empty range
  if LastItem < FirstItem then
  begin
    Result := nil;
    exit;
  end;

  // get new record
  Result := MakeRec;

  // simple constant, variable or function?
  if LastItem = FirstItem then
  begin
    Result.ExprWord := TExprWord(Expr.Items[FirstItem]);
    Result.Oper := @Result.ExprWord.ExprFunc;
    if Result.ExprWord.IsVariable then
    begin
      // copy pointer to variable
      Result.Args[0] := Result.ExprWord.AsPointer;
      // is this a fixed length string variable?
      if Result.ExprWord.FixedLen >= 0 then
      begin
        // store length as second parameter
        Result.Args[1] := PChar(Result.ExprWord.LenAsPointer);
      end;
    end;
    exit;
  end;

  // no...more complex, find operator with lowest precedence
  brCount := 0;
  IArg := 0;
  IEnd := FirstItem-1;
  lPrec := -1;
  for I := FirstItem to LastItem do
  begin
    ExprWord := TExprWord(Expr.Items[I]);
    if (brCount = 0) and ExprWord.IsOperator and (TFunction(ExprWord).OperPrec > lPrec) then
    begin
      IEnd := I;
      lPrec := TFunction(ExprWord).OperPrec;
    end;
    case ExprWord.ResultType of
      etLeftBracket: Inc(brCount);
      etRightBracket: Dec(brCount);
    end;
  end;

  // operator found ?
  if IEnd >= FirstItem then
  begin
    // save operator
    Result.ExprWord := TExprWord(Expr.Items[IEnd]);
    Result.Oper := Result.ExprWord.ExprFunc;
    // recurse into left part if present
    if IEnd > FirstItem then
    begin
      Result.ArgList[IArg] := MakeTree(Expr, FirstItem, IEnd-1);
      Inc(IArg);
    end;
    // recurse into right part if present
    if IEnd < LastItem then
      Result.ArgList[IArg] := MakeTree(Expr, IEnd+1, LastItem);
  end else 
  if TExprWord(Expr.Items[FirstItem]).IsFunction then 
  begin
    // save function
    Result.ExprWord := TExprWord(Expr.Items[FirstItem]);
    Result.Oper := Result.ExprWord.ExprFunc;
    // parse function arguments
    IEnd := FirstItem + 1;
    IStart := IEnd;
    brCount := 0;
    if TExprWord(Expr.Items[IEnd]).ResultType = etLeftBracket then
    begin
      // opening bracket found, first argument expression starts at next index
      Inc(brCount);
      Inc(IStart);
      while (IEnd < LastItem) and (brCount <> 0) do
      begin
        Inc(IEnd);
        case TExprWord(Expr.Items[IEnd]).ResultType of
          etLeftBracket: Inc(brCount);
          etComma:
            if brCount = 1 then
            begin
              // argument separation found, build tree of argument expression
              Result.ArgList[IArg] := MakeTree(Expr, IStart, IEnd-1);
              Inc(IArg);
              IStart := IEnd + 1;
            end;
          etRightBracket: Dec(brCount);
        end;
      end;

      // parse last argument
      Result.ArgList[IArg] := MakeTree(Expr, IStart, IEnd-1);
    end;
  end else
    raise EParserException.Create('Operator/function missing');
end;

procedure TCustomExpressionParser.ParseString(AnExpression: string; DestCollection: TExprCollection);
var
  isConstant: Boolean;
  I, I1, I2, Len, DecSep: Integer;
  W, S: string;
  TempWord: TExprWord;

  procedure ReadConstant(AnExpr: string; isHex: Boolean);
  begin
    isConstant := true;
    while (I2 <= Len) and ((AnExpr[I2] in ['0'..'9']) or
      (isHex and (AnExpr[I2] in ['a'..'f', 'A'..'F']))) do
      Inc(I2);
    if I2 <= Len then
    begin
      if AnExpr[I2] = FDecimalSeparator then
      begin
        Inc(I2);
        while (I2 <= Len) and (AnExpr[I2] in ['0'..'9']) do
          Inc(I2);
      end;
      if (I2 <= Len) and (AnExpr[I2] = 'e') then
      begin
        Inc(I2);
        if (I2 <= Len) and (AnExpr[I2] in ['+', '-']) then
          Inc(I2);
        while (I2 <= Len) and (AnExpr[I2] in ['0'..'9']) do
          Inc(I2);
      end;
    end;
  end;

  procedure ReadWord(AnExpr: string);
  var
    OldI2: Integer;
    constChar: Char;
  begin
    isConstant := false;
    I1 := I2;
    while (I1 < Len) and (AnExpr[I1] = ' ') do
      Inc(I1);
    I2 := I1;
    if I1 <= Len then
    begin
      if AnExpr[I2] = HexChar then
      begin
        Inc(I2);
        OldI2 := I2;
        ReadConstant(AnExpr, true);
        if I2 = OldI2 then
        begin
          isConstant := false;
          while (I2 <= Len) and (AnExpr[I2] in ['a'..'z', 'A'..'Z', '_', '0'..'9']) do
            Inc(I2);
        end;
      end
      else if AnExpr[I2] = FDecimalSeparator then
        ReadConstant(AnExpr, false)
      else
        case AnExpr[I2] of
          '''', '"':
            begin
              isConstant := true;
              constChar := AnExpr[I2];
              Inc(I2);
              while (I2 <= Len) and (AnExpr[I2] <> constChar) do
                Inc(I2);
              if I2 <= Len then
                Inc(I2);
            end;
          'a'..'z', 'A'..'Z', '_':
            begin
              while (I2 <= Len) and (AnExpr[I2] in ['a'..'z', 'A'..'Z', '_', '0'..'9']) do
                Inc(I2);
            end;
          '>', '<':
            begin
              if (I2 <= Len) then
                Inc(I2);
              if AnExpr[I2] in ['=', '<', '>'] then
                Inc(I2);
            end;
          '=':
            begin
              if (I2 <= Len) then
                Inc(I2);
              if AnExpr[I2] in ['<', '>', '='] then
                Inc(I2);
            end;
          '&':
            begin
              if (I2 <= Len) then
                Inc(I2);
              if AnExpr[I2] in ['&'] then
                Inc(I2);
            end;
          '|':
            begin
              if (I2 <= Len) then
                Inc(I2);
              if AnExpr[I2] in ['|'] then
                Inc(I2);
            end;
          ':':
            begin
              if (I2 <= Len) then
                Inc(I2);
              if AnExpr[I2] = '=' then
                Inc(I2);
            end;
          '!':
            begin
              if (I2 <= Len) then
                Inc(I2);
              if AnExpr[I2] = '=' then //support for !=
                Inc(I2);
            end;
          '+':
            begin
              Inc(I2);
              if (AnExpr[I2] = '+') and FWordsList.Search(PChar('++'), I) then
                Inc(I2);
            end;
          '-':
            begin
              Inc(I2);
              if (AnExpr[I2] = '-') and FWordsList.Search(PChar('--'), I) then
                Inc(I2);
            end;
          '^', '/', '\', '*', '(', ')', '%', '~', '$':
            Inc(I2);
          '0'..'9':
            ReadConstant(AnExpr, false);
        else
          begin
            Inc(I2);
          end;
        end;
    end;
  end;

begin
  I2 := 1;
  S := Trim(AnExpression);
  Len := Length(S);
  repeat
    ReadWord(S);
    W := Trim(Copy(S, I1, I2 - I1));
    if isConstant then
    begin
      if W[1] = HexChar then
      begin
        // convert hexadecimal to decimal
        W[1] := '$';
        W := IntToStr(StrToInt(W));
      end;
      if (W[1] = '''') or (W[1] = '"') then
        TempWord := TStringConstant.Create(W)
      else begin
        DecSep := Pos(FDecimalSeparator, W);
        if (DecSep > 0) then
        begin
{$IFDEF ENG_NUMBERS}
          // we'll have to convert FDecimalSeparator into DecimalSeparator
          // otherwise the OS will not understand what we mean
          W[DecSep] := DecimalSeparator;
{$ENDIF}
          TempWord := TFloatConstant.Create(W, W)
        end else begin
          TempWord := TIntegerConstant.Create(StrToInt(W));
        end;
      end;
      DestCollection.Add(TempWord);
      FConstantsList.Add(TempWord);
    end
    else if Length(W) > 0 then
      if FWordsList.Search(PChar(W), I) then
      begin
        DestCollection.Add(FWordsList.Items[I])
      end else begin
        // unknown variable -> fire event
        HandleUnknownVariable(W);
        // try to search again
        if FWordsList.Search(PChar(W), I) then
        begin
          DestCollection.Add(FWordsList.Items[I])
        end else begin
          raise EParserException.Create('Unknown variable '''+W+''' found.');
        end;
      end;
  until I2 > Len;
end;

procedure TCustomExpressionParser.Check(AnExprList: TExprCollection);
var
  I, J, K, L: Integer;
begin
  AnExprList.Check;
  with AnExprList do
  begin
    I := 0;
    while I < Count do
    begin
      {----CHECK ON DOUBLE MINUS OR DOUBLE PLUS----}
      if ((TExprWord(Items[I]).Name = '-') or
        (TExprWord(Items[I]).Name = '+'))
        and ((I = 0) or
        (TExprWord(Items[I - 1]).ResultType = etComma) or
        (TExprWord(Items[I - 1]).ResultType = etLeftBracket) or
        (TExprWord(Items[I - 1]).IsOperator and (TExprWord(Items[I - 1]).MaxFunctionArg
        = 2))) then
      begin
        {replace e.g. ----1 with +1}
        if TExprWord(Items[I]).Name = '-' then
          K := -1
        else
          K := 1;
        L := 1;
        while (I + L < Count) and ((TExprWord(Items[I + L]).Name = '-')
          or (TExprWord(Items[I + L]).Name = '+')) and ((I + L = 0) or
          (TExprWord(Items[I + L - 1]).ResultType = etComma) or
          (TExprWord(Items[I + L - 1]).ResultType = etLeftBracket) or
          (TExprWord(Items[I + L - 1]).IsOperator and (TExprWord(Items[I + L -
          1]).MaxFunctionArg = 2))) do
        begin
          if TExprWord(Items[I + L]).Name = '-' then
            K := -1 * K;
          Inc(L);
        end;
        if L > 0 then
        begin
          Dec(L);
          for J := I + 1 to Count - 1 - L do
            Items[J] := Items[J + L];
          Count := Count - L;
        end;
        if K = -1 then
        begin
          if FWordsList.Search(pchar('-@'), J) then
            Items[I] := FWordsList.Items[J];
        end
        else if FWordsList.Search(pchar('+@'), J) then
          Items[I] := FWordsList.Items[J];
      end;
      {----CHECK ON DOUBLE NOT----}
      if (TExprWord(Items[I]).Name = 'not')
        and ((I = 0) or
        (TExprWord(Items[I - 1]).ResultType = etLeftBracket) or
        TExprWord(Items[I - 1]).IsOperator) then
      begin
        {replace e.g. not not 1 with 1}
        K := -1;
        L := 1;
        while (I + L < Count) and (TExprWord(Items[I + L]).Name = 'not') and ((I
          + L = 0) or
          (TExprWord(Items[I + L - 1]).ResultType = etLeftBracket) or
          TExprWord(Items[I + L - 1]).IsOperator) do
        begin
          K := -K;
          Inc(L);
        end;
        if L > 0 then
        begin
          if K = 1 then
          begin //remove all
            for J := I to Count - 1 - L do
              Items[J] := Items[J + L];
            Count := Count - L;
          end
          else
          begin //keep one
            Dec(L);
            for J := I + 1 to Count - 1 - L do
              Items[J] := Items[J + L];
            Count := Count - L;
          end
        end;
      end;
      {-----MISC CHECKS-----}
      if (TExprWord(Items[I]).IsVariable) and ((I < Count - 1) and
        (TExprWord(Items[I + 1]).IsVariable)) then
        raise EParserException.Create('Missing operator between '''+TExprWord(Items[I]).Name+''' and '''+TExprWord(Items[I]).Name+'''');
      if (TExprWord(Items[I]).ResultType = etLeftBracket) and ((I >= Count - 1) or
        (TExprWord(Items[I + 1]).ResultType = etRightBracket)) then
        raise EParserException.Create('Empty brackets ()');
      if (TExprWord(Items[I]).ResultType = etRightBracket) and ((I < Count - 1) and
        (TExprWord(Items[I + 1]).ResultType = etLeftBracket)) then
        raise EParserException.Create('Missing operator between )(');
      if (TExprWord(Items[I]).ResultType = etRightBracket) and ((I < Count - 1) and
        (TExprWord(Items[I + 1]).IsVariable)) then
        raise EParserException.Create('Missing operator between ) and constant/variable');
      if (TExprWord(Items[I]).ResultType = etLeftBracket) and ((I > 0) and
        (TExprWord(Items[I - 1]).IsVariable)) then
        raise EParserException.Create('Missing operator between constant/variable and (');

      {-----CHECK ON INTPOWER------}
      if (TExprWord(Items[I]).Name = '^') and ((I < Count - 1) and
          (TExprWord(Items[I + 1]).ClassType = TIntegerConstant)) then
        if FWordsList.Search(PChar('^@'), J) then
          Items[I] := FWordsList.Items[J]; //use the faster intPower if possible
      Inc(I);
    end;
  end;
end;

procedure TCustomExpressionParser.EvaluateCurrent;
var
  TempRec: PExpressionRec;
begin
  if FCurrentRec <> nil then
  begin
    // get current record
    TempRec := FCurrentRec;
    // execute list
    repeat
      with TempRec^ do
      begin
        // do we need to reset pointer?
        if ResetDest then
          Res.MemoryPos^ := Res.Memory^;

        Oper(TempRec);

        // goto next
        TempRec := Next;
      end;
    until TempRec = nil;
  end;
end;

procedure TCustomExpressionParser.DefineFunction(AFunctName, AShortName, ADescription, ATypeSpec: string;
  AMinFunctionArg: Integer; AResultType: TExpressionType; AFuncAddress: TExprFunc);
begin
  AddReplaceExprWord(TFunction.Create(AFunctName, AShortName, ATypeSpec, AMinFunctionArg, AResultType, AFuncAddress, ADescription));
end;

procedure TCustomExpressionParser.DefineIntegerVariable(AVarName: string; AValue: PInteger);
begin
  AddReplaceExprWord(TIntegerVariable.Create(AVarName, AValue));
end;

{
procedure TCustomExpressionParser.DefineSmallIntVariable(AVarName: string; AValue: PSmallInt);
begin
  AddReplaceExprWord(TSmallIntVariable.Create(AVarName, AValue));
end;
}

{$ifdef SUPPORT_INT64}

procedure TCustomExpressionParser.DefineLargeIntVariable(AVarName: string; AValue: PLargeInt);
begin
  AddReplaceExprWord(TLargeIntVariable.Create(AVarName, AValue));
end;

{$endif}

procedure TCustomExpressionParser.DefineDateTimeVariable(AVarName: string; AValue: PDateTimeRec);
begin
  AddReplaceExprWord(TDateTimeVariable.Create(AVarName, AValue));
end;

procedure TCustomExpressionParser.DefineBooleanVariable(AVarName: string; AValue: PBoolean);
begin
  AddReplaceExprWord(TBooleanVariable.Create(AVarName, AValue));
end;

procedure TCustomExpressionParser.DefineFloatVariable(AVarName: string; AValue: PDouble);
begin
  AddReplaceExprWord(TFloatVariable.Create(AVarName, AValue));
end;

procedure TCustomExpressionParser.DefineStringVariable(AVarName: string; AValue: PPChar);
begin
  DefineStringVariableFixedLen(AVarName, AValue, -1);
end;

procedure TCustomExpressionParser.DefineStringVariableFixedLen(AVarName: string; AValue: PPChar; ALength: Integer);
begin
  AddReplaceExprWord(TStringVariable.Create(AVarName, AValue, ALength));
end;

{
procedure TCustomExpressionParser.GetGeneratedVars(AList: TList);
var
  I: Integer;
begin
  AList.Clear;
  with FWordsList do
    for I := 0 to Count - 1 do
    begin
      if TObject(Items[I]).ClassType = TGeneratedVariable then
        AList.Add(Items[I]);
    end;
end;
}

function TCustomExpressionParser.GetResultType: TExpressionType;
begin
  Result := etUnknown;
  if FCurrentRec <> nil then
  begin
    //LAST operand should be boolean -otherwise If(,,) doesn't work
    while (FLastRec^.Next <> nil) do
      FLastRec := FLastRec^.Next;
    if FLastRec.ExprWord <> nil then
      Result := FLastRec.ExprWord.ResultType;
  end;
end;

procedure TCustomExpressionParser.ReplaceExprWord(OldExprWord, NewExprWord: TExprWord);
var
  J: Integer;
  Rec: PExpressionRec;
  p, pnew: pointer;
begin
  if OldExprWord.MaxFunctionArg <> NewExprWord.MaxFunctionArg then
    raise Exception.Create('Cannot replace variable/function MaxFunctionArg doesn''t match');

  p := OldExprWord.AsPointer;
  pnew := NewExprWord.AsPointer;
  Rec := FCurrentRec;
  repeat
    if (Rec.ExprWord = OldExprWord) then
    begin
      Rec.ExprWord := NewExprWord;
      Rec.Oper := NewExprWord.ExprFunc;
    end;
    if p <> nil then
      for J := 0 to Rec.ExprWord.MaxFunctionArg - 1 do
        if Rec.Args[J] = p then
          Rec.Args[J] := pnew;
    Rec := Rec.Next;
  until Rec = nil;
end;

function TCustomExpressionParser.MakeRec: PExpressionRec;
var
  I: Integer;
begin
  New(Result);
  Result.Oper := nil;
  Result.AuxData := nil;
  for I := 0 to MaxArg - 1 do
  begin
    Result.Args[I] := nil;
    Result.ArgsPos[I] := nil;
    Result.ArgsSize[I] := 0;
    Result.ArgsType[I] := etUnknown;
    Result.ArgList[I] := nil;
  end;
  Result.Res := nil;
  Result.Next := nil;
  Result.ExprWord := nil;
  Result.ResetDest := false;
end;

procedure TCustomExpressionParser.Evaluate(AnExpression: string);
begin
  if Length(AnExpression) > 0 then
  begin
    AddExpression(AnExpression);
    EvaluateCurrent;
  end;
end;

function TCustomExpressionParser.AddExpression(AnExpression: string): Integer;
begin
  if Length(AnExpression) > 0 then
  begin
    Result := 0;
    CompileExpression(AnExpression);
  end else
    Result := -1;
  //CurrentIndex := Result;
end;

procedure TCustomExpressionParser.ReplaceFunction(OldName: string; AFunction:
  TObject);
var
  I: Integer;
begin
  // clearing only allowed when expression is not present
  if (AFunction = nil) and (FCurrentRec <> nil) then
    raise Exception.Create('Cannot undefine function/variable while expression present');

  if FWordsList.Search(PChar(OldName), I) then
  begin
    // if no function specified, then no need to replace!
    if AFunction <> nil then
      ReplaceExprWord(FWordsList.Items[I], TExprWord(AFunction));
    FWordsList.AtFree(I);
  end;
  if AFunction <> nil then
    FWordsList.Add(AFunction);
end;

procedure TCustomExpressionParser.ClearExpressions;
begin
  DisposeList(FCurrentRec);
  FCurrentRec := nil;
  FLastRec := nil;
end;

procedure TCustomExpressionParser.AddReplaceExprWord(AExprWord: TExprWord);
var
  IOldVar: Integer;
begin
  if FWordsList.Search(PChar(AExprWord.Name), IOldVar) then
  begin
    ReplaceExprWord(FWordsList.Items[IOldVar], AExprWord);
    FWordsList.AtFree(IOldVar);
    FWordsList.Add(AExprWord);
  end
  else
    FWordsList.Add(AExprWord);
end;

function TCustomExpressionParser.GetFunctionDescription(AFunction: string):
  string;
var
  S: string;
  p, I: Integer;
begin
  S := AFunction;
  p := Pos('(', S);
  if p > 0 then
    S := Copy(S, 1, p - 1);
  if FWordsList.Search(pchar(S), I) then
    Result := TExprWord(FWordsList.Items[I]).Description
  else
    Result := EmptyStr;
end;

procedure TCustomExpressionParser.GetFunctionNames(AList: TStrings);
var
  I, J: Integer;
  S: string;
begin
  with FWordsList do
    for I := 0 to Count - 1 do
      with TExprWord(FWordsList.Items[I]) do
        if Length(Description) > 0 then
        begin
          S := Name;
          if MaxFunctionArg > 0 then
          begin
            S := S + '(';
            for J := 0 to MaxFunctionArg - 2 do
              S := S + ArgSeparator;
            S := S + ')';
          end;
          AList.Add(S);
        end;
end;

end.

