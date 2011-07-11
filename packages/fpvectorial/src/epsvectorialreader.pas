{
Reads EPS files

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho

Documentation: http://www.tailrecursive.org/postscript/postscript.html
}
unit epsvectorialreader;

{$mode objfpc}{$H+}

{.$define FPVECTORIALDEBUG_PATHS}
{.$define FPVECTORIALDEBUG_COLORS}
{.$define FPVECTORIALDEBUG_ROLL}

interface

uses
  Classes, SysUtils, Math, contnrs,
  fpimage, fpcanvas,
  fpvectorial, fpvutils;

type
  TPSTokenType = (ttComment, ttFloat);

  TPSTokens = TFPList;// TPSToken;

  TPSToken = class
    StrValue: string;
    FloatValue: double;
    IntValue: Integer;
    BoolValue: Boolean;
    Line: Integer; // To help debugging
    function Duplicate: TPSToken; virtual;
  end;

  TCommentToken = class(TPSToken)
  end;

  { TProcedureToken }

  TProcedureToken = class(TPSToken)
    Levels: Integer; // Used to count groups inside groups and find the end of a top-level group
    Childs: TPSTokens;
    Parsed: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TETType = (ettNamedElement, ettOperand, ettOperator);

  { TExpressionToken }

  TExpressionToken = class(TPSToken)
    ETType: TETType;
    function IsExpressionOperand: Boolean;
    procedure PrepareFloatValue;
    function Duplicate: TPSToken; override;
  end;

  TPostScriptScannerState = (ssSearchingToken, ssInComment, ssInDefinition, ssInGroup, ssInExpressionElement);

  { TGraphicState }

  TGraphicState = class
  public
    Color: TFPColor;
    TranslateX, TranslateY: Double;
    function Duplicate: TGraphicState;
  end;

  { TPSTokenizer }

  TPSTokenizer = class
  public
    Tokens: TPSTokens;
    constructor Create;
    destructor Destroy; override;
    procedure ReadFromStream(AStream: TStream);
    procedure DebugOut();
    function IsValidPostScriptChar(AChar: Byte): Boolean;
    function IsPostScriptSpace(AChar: Byte): Boolean;
    function IsEndOfLine(ACurChar: Byte; AStream: TStream): Boolean;
  end;

  { TvEPSVectorialReader }

  TvEPSVectorialReader = class(TvCustomVectorialReader)
  private
    Stack: TObjectStack;
    GraphicStateStack: TObjectStack; // TGraphicState
    Dictionary: TStringList;
    ExitCalled: Boolean;
    CurrentGraphicState: TGraphicState;
    //
    procedure RunPostScript(ATokens: TPsTokens; AData: TvVectorialDocument);
    //
    procedure ExecuteProcedureToken(AToken: TProcedureToken; AData: TvVectorialDocument);
    procedure ExecuteOperatorToken(AToken: TExpressionToken; AData: TvVectorialDocument);
    function  ExecuteArithmeticAndMathOperator(AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
    function  ExecutePathConstructionOperator(AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
    function  ExecuteGraphicStateOperatorsDI(AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
    function  ExecuteGraphicStateOperatorsDD(AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
    function  ExecuteDictionaryOperators(AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
    function  ExecuteMiscellaneousOperators(AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
    function  ExecuteStackManipulationOperator(AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
    function  ExecuteControlOperator(AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
    function  ExecutePaintingOperator(AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
    function  ExecuteDeviceSetupAndOutputOperator(AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
    function  ExecuteArrayOperator(AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
    function  ExecuteStringOperator(AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
    //
    procedure PostScriptCoordsToFPVectorialCoords(AParam1, AParam2: TPSToken; var APosX, APosY: Double);
    procedure DictionarySubstituteOperator(ADictionary: TStringList; var ACurToken: TPSToken);
  public
    { General reading methods }
    Tokenizer: TPSTokenizer;
    constructor Create; override;
    Destructor Destroy; override;
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

var
  FPointSeparator: TFormatSettings;

{ TGraphicState }

function TGraphicState.Duplicate: TGraphicState;
begin
  Result := TGraphicState(Self.ClassType.Create);
  Result.Color := Color;
  Result.TranslateX := TranslateX;
  Result.TranslateY := TranslateY;
end;

{ TPSToken }

function TPSToken.Duplicate: TPSToken;
begin
  Result := TPSToken(Self.ClassType.Create);
  Result.StrValue := StrValue;
  Result.FloatValue := FloatValue;
  Result.IntValue := IntValue;
  Result.Line := Line;
end;

{ TProcedureToken }

constructor TProcedureToken.Create;
begin
  inherited Create;

  Childs := TPSTokens.Create;
end;

destructor TProcedureToken.Destroy;
begin
  Childs.Free;

  inherited Destroy;
end;

{ TExpressionToken }

function TExpressionToken.IsExpressionOperand: Boolean;
begin
  if StrValue = '' then Exit(False);
  Result := StrValue[1] in ['0'..'9','-'];
end;

procedure TExpressionToken.PrepareFloatValue;
begin
  //if not IsExpressionOperand() then Exit;
  if ETType <> ettOperand then Exit; // faster, because this field should already be filled

  FloatValue := StrToFloat(StrValue, FPointSeparator);
end;

function TExpressionToken.Duplicate: TPSToken;
begin
  Result:=inherited Duplicate;
  TExpressionToken(Result).ETType := ETType;
end;

{$DEFINE FPVECTORIALDEBUG}

{ TPSTokenizer }

constructor TPSTokenizer.Create;
begin
  inherited Create;
  Tokens := TPSTokens.Create;
end;

destructor TPSTokenizer.Destroy;
begin
  Tokens.Free;
  inherited Destroy;
end;

{@@ Rules for parsing PostScript files:

* Coments go from the first occurence of % outside a line to the next new line
* The only accepted characters are printable ASCII ones, plus spacing ASCII chars
  See IsValidPostScriptChar about that
}
procedure TPSTokenizer.ReadFromStream(AStream: TStream);
var
  i: Integer;
  CurChar: Char;
  CurLine: Integer = 1;
  State: TPostScriptScannerState = ssSearchingToken;
  CommentToken: TCommentToken;
  ProcedureToken: TProcedureToken;
  ExpressionToken: TExpressionToken;
  Len: Integer;
  lIsEndOfLine: Boolean;
begin
  while AStream.Position < AStream.Size do
  begin
    CurChar := Char(AStream.ReadByte());
//    {$ifdef FPVECTORIALDEBUG}
//    WriteLn(Format('Obtained token %s', [CurChar]));
//    {$endif}
    if not IsValidPostScriptChar(Byte(CurChar)) then
      raise Exception.Create('[TPSTokenizer.ReadFromStream] Invalid char: ' + IntToHex(Byte(CurChar), 2));

    lIsEndOfLine := IsEndOfLine(Byte(CurChar), AStream);
    if lIsEndOfLine then Inc(CurLine);

    case State of
      { Searching for a token }
      ssSearchingToken:
      begin
        if CurChar = '%' then
        begin
          CommentToken := TCommentToken.Create;
          CommentToken.Line := CurLine;
          State := ssInComment;
//          {$ifdef FPVECTORIALDEBUG}
//          WriteLn(Format('Starting Comment at Line %d', [CurLine]));
//          {$endif}
        end
        else if CurChar = '{' then
        begin
          ProcedureToken := TProcedureToken.Create;
          ProcedureToken.Levels := 1;
          ProcedureToken.Line := CurLine;
          State := ssInGroup;
        end
        else if CurChar in ['a'..'z','A'..'Z','0'..'9','-','/'] then
        begin
          ExpressionToken := TExpressionToken.Create;
          ExpressionToken.Line := CurLine;
          if CurChar = '/' then
            ExpressionToken.ETType := ettNamedElement
          else
          begin
            ExpressionToken.StrValue := CurChar;
            if ExpressionToken.IsExpressionOperand() then
              ExpressionToken.ETType := ettOperand
            else
              ExpressionToken.ETType := ettOperator;
          end;
          State := ssInExpressionElement;
        end
        else if lIsEndOfLine then Continue
        else if IsPostScriptSpace(Byte(CurChar)) then Continue
        else
          raise Exception.Create(Format('[TPSTokenizer.ReadFromStream] Unexpected char while searching for token: $%s in Line %d',
           [IntToHex(Byte(CurChar), 2), CurLine]));
      end;

      { Passing by comments }
      ssInComment:
      begin
        CommentToken.StrValue := CommentToken.StrValue + CurChar;
        if lIsEndOfLine then
        begin
          Tokens.Add(CommentToken);
          State := ssSearchingToken;
//          {$ifdef FPVECTORIALDEBUG}
//          WriteLn(Format('Adding Comment "%s" at Line %d', [CommentToken.StrValue, CurLine]));
//          {$endif}
        end;
      end; // ssInComment

      // Starts at { and ends in }, passing over nested groups
      ssInGroup:
      begin
        if (CurChar = '{') then ProcedureToken.Levels := ProcedureToken.Levels + 1;
        if (CurChar = '}') then ProcedureToken.Levels := ProcedureToken.Levels - 1;

        if ProcedureToken.Levels = 0 then
        begin
          Tokens.Add(ProcedureToken);
          State := ssSearchingToken;
        end
        else
          ProcedureToken.StrValue := ProcedureToken.StrValue + CurChar;
      end;

      // Goes until a space comes, or {
      ssInExpressionElement:
      begin
        if IsPostScriptSpace(Byte(CurChar)) or (CurChar = '{') then
        begin
          ExpressionToken.PrepareFloatValue();
          Tokens.Add(ExpressionToken);
          State := ssSearchingToken;
          if (CurChar = '{') then AStream.Seek(-1, soFromCurrent);
        end
        else
          ExpressionToken.StrValue := ExpressionToken.StrValue + CurChar;
      end;

    end; // case
  end; // while

  // If the stream finished, there might be a token still being built
  // so lets finish it
  if State = ssInExpressionElement then
  begin
    Tokens.Add(ExpressionToken);
  end;
end;

procedure TPSTokenizer.DebugOut();
var
  i: Integer;
  Token: TPSToken;
begin
  for i := 0 to Tokens.Count - 1 do
  begin
    Token := TPSToken(Tokens.Items[i]);

    if Token is TCommentToken then
    begin
      WriteLn(Format('TCommentToken StrValue=%s', [Token.StrValue]));
    end
    else if Token is TProcedureToken then
    begin
      WriteLn(Format('TProcedureToken StrValue=%s', [Token.StrValue]));
    end
    else if Token is TExpressionToken then
    begin
      WriteLn(Format('TExpressionToken StrValue=%s', [Token.StrValue]));
    end;
  end;
end;

{@@ Valid PostScript Chars:

All printable ASCII: a..zA..Z0..9 plus punctuation

Plus the following white spaces
000 00 0 Null (nul)
011 09 9 Tab (tab)
012 0A 10 Line feed (LF)
014 0C 12 Form feed (FF)
015 0D 13 Carriage return (CR)
040 20 32 Space (SP)
}
function TPSTokenizer.IsValidPostScriptChar(AChar: Byte): Boolean;
begin
  Result := ((AChar > 32) and (AChar < 127)) or (AChar in [0, 9, 10, 12, 13, 32]);
end;

function TPSTokenizer.IsPostScriptSpace(AChar: Byte): Boolean;
begin
  Result := AChar in [0, 9, 10, 12, 13, 32];
end;

function TPSTokenizer.IsEndOfLine(ACurChar: Byte; AStream: TStream): Boolean;
var
  HasNextChar: Boolean = False;
  NextChar: Byte;
begin
  Result := False;

  if ACurChar = 13 then
  begin
    if AStream.Position < AStream.Size then
    begin
      HasNextChar := True;
      NextChar := AStream.ReadByte();
      if NextChar <> 10 then AStream.Seek(-1, soFromCurrent); // Go back if it wasnt a #13#10
      Exit(True);
    end;
  end;

  if ACurChar = 10 then Result := True;
end;

{$ifndef Windows}
{$define FPVECTORIALDEBUG}
{$endif}

{ TvEPSVectorialReader }

procedure TvEPSVectorialReader.RunPostScript(ATokens: TPsTokens;
  AData: TvVectorialDocument);
var
  i: Integer;
  CurToken: TPSToken;
begin
  {$ifdef FPVECTORIALDEBUG_CODEFLOW}
  WriteLn('[TvEPSVectorialReader.RunPostScript] START');
  {$endif}
  if ExitCalled then
  begin
    {$ifdef FPVECTORIALDEBUG_CODEFLOW}
    WriteLn('[TvEPSVectorialReader.RunPostScript] ExitCalled');
    {$endif}
    Exit;
  end;
  for i := 0 to ATokens.Count - 1 do
  begin
    CurToken := TPSToken(ATokens.Items[i]);

    if CurToken is TCommentToken then
    begin
      {$ifdef FPVECTORIALDEBUG_CODEFLOW}
      WriteLn(Format('[TvEPSVectorialReader.RunPostScript] Type: TCommentToken Token: %s', [CurToken.StrValue]));
      {$endif}
//      ProcessCommentToken(CurToken as TCommentToken, AData);
      Continue;
    end;

    if CurToken is TProcedureToken then
    begin
      {$ifdef FPVECTORIALDEBUG_CODEFLOW}
      WriteLn(Format('[TvEPSVectorialReader.RunPostScript] Type: TProcedureToken Token: %s', [CurToken.StrValue]));
      {$endif}
      Stack.Push(CurToken);
      Continue;
    end;

    if CurToken is TExpressionToken then
    begin
      {$ifdef FPVECTORIALDEBUG_CODEFLOW}
      WriteLn(Format('[TvEPSVectorialReader.RunPostScript] Type: TExpressionToken Token: %s', [CurToken.StrValue]));
      {$endif}

      if TExpressionToken(CurToken).ETType <> ettOperator then
      begin
        Stack.Push(CurToken);
        Continue;
      end;

      // Now we need to verify if the operator should be substituted in the dictionary
      DictionarySubstituteOperator(Dictionary, CurToken);

      if CurToken is TProcedureToken then ExecuteProcedureToken(TProcedureToken(CurToken), AData)
      else ExecuteOperatorToken(TExpressionToken(CurToken), AData);

      if ExitCalled then Break;
    end;
  end;
  {$ifdef FPVECTORIALDEBUG_CODEFLOW}
  WriteLn('[TvEPSVectorialReader.RunPostScript] END');
  {$endif}
end;

procedure TvEPSVectorialReader.ExecuteProcedureToken(AToken: TProcedureToken;
  AData: TvVectorialDocument);
var
  ProcTokenizer: TPSTokenizer;
  lStream: TMemoryStream;
  lOldTokens: TPSTokens;
  i: Integer;
begin
  {$ifdef FPVECTORIALDEBUG_CODEFLOW}
  WriteLn('[TvEPSVectorialReader.ExecuteProcedureToken] START');
  {$endif}
  if ExitCalled then
  begin
    {$ifdef FPVECTORIALDEBUG_CODEFLOW}
    WriteLn('[TvEPSVectorialReader.ExecuteProcedureToken] ExitCalled');
    {$endif}
    Exit;
  end;

  if not AToken.Parsed then
  begin
    ProcTokenizer := TPSTokenizer.Create;
    lStream := TMemoryStream.Create;
    try
      // Copy the string to a Stream
      for i := 1 to Length(AToken.StrValue) do
        lStream.WriteByte(Byte(AToken.StrValue[i]));

      // Change the Tokens so that it writes directly to AToken.Childs
      lOldTokens := ProcTokenizer.Tokens;
      ProcTokenizer.Tokens := AToken.Childs;

      // Now parse the procedure code
      lStream.Position := 0;
      ProcTokenizer.ReadFromStream(lStream);

      // Recover the old tokens for usage in .Free
      ProcTokenizer.Tokens := lOldTokens;
    finally
      lStream.Free;
      ProcTokenizer.Free;
    end;

    AToken.Parsed := True;
  end;

  // Now run the procedure
  RunPostScript(AToken.Childs, AData);
  {$ifdef FPVECTORIALDEBUG_CODEFLOW}
  WriteLn('[TvEPSVectorialReader.ExecuteProcedureToken] END');
  {$endif}
end;

procedure TvEPSVectorialReader.ExecuteOperatorToken(AToken: TExpressionToken;
  AData: TvVectorialDocument);
var
  Param1, Param2: TPSToken;
begin
  if AToken.StrValue = '' then raise Exception.Create('[TvEPSVectorialReader.ProcessExpressionToken] Empty operator');

  if ExecuteDictionaryOperators(AToken, AData) then Exit;

  if ExecuteArithmeticAndMathOperator(AToken, AData) then Exit;

  if ExecutePathConstructionOperator(AToken, AData) then Exit;

  if ExecuteGraphicStateOperatorsDI(AToken, AData) then Exit;

  if ExecuteGraphicStateOperatorsDD(AToken, AData) then Exit;

  if ExecuteControlOperator(AToken, AData) then Exit;

  if ExecuteStackManipulationOperator(AToken, AData) then Exit;

  if ExecuteMiscellaneousOperators(AToken, AData) then Exit;

  if ExecutePaintingOperator(AToken, AData) then Exit;

  if ExecuteDeviceSetupAndOutputOperator(AToken, AData) then Exit;

  if ExecuteArrayOperator(AToken, AData) then Exit;

  if ExecuteStringOperator(AToken, AData) then Exit;

  // If we got here, there the command not yet implemented
  raise Exception.Create(Format('[TvEPSVectorialReader.ProcessExpressionToken] Unknown PostScript Command "%s" in Line %d',
    [AToken.StrValue, AToken.Line]));

{  File Operators

  filename access file file Open named file with specified access
  datasrc|datatgt dict
  param1 … paramn filtername filter file Establish filtered file
  file closefile – Close file
  file read int true Read one character from file
  or false
  file int write – Write one character to file
  file string readhexstring substring bool Read hexadecimal numbers from file into
  string
  file string writehexstring – Write string to file as hexadecimal
  file string readstring substring bool Read string from file
  file string writestring – Write string to file
  file string readline substring bool Read line from file into string
  file token any true Read token from file
  or false
  file bytesavailable int Return number of bytes available to read
  – flush – Send buffered data to standard output file
  file flushfile – Send buffered data or read to EOF
  file resetfile – Discard buffered characters
  file status bool Return status of file (true = valid)
  filename status pages bytes referenced created true
  or false Return information about named file
  filename run – Execute contents of named file
  – currentfile file Return file currently being executed
  filename deletefile – Delete named file
  filename1 filename2 renamefile – Rename file filename1 to filename2
  template proc scratch filenameforall – Execute proc for each file name matching
  template
  file position setfileposition – Set file to specified position
  file fileposition position Return current position in file
  string print – Write string to standard output file
  any = – Write text representation of any to standard
  output file
  any == – Write syntactic representation of any to
  standard output file
  any1 … anyn stack any1 … anyn Print stack nondestructively using =
  any1 … anyn pstack any1 … anyn Print stack nondestructively using ==
  obj tag printobject – Write binary object to standard output file,
  using tag
  file obj tag writeobject – Write binary object to file, using tag
  int setobjectformat – Set binary object format (0 = disable,
  1 = IEEE high, 2 = IEEE low, 3 = native
  high, 4 = native low)
  – currentobjectformat int Return binary object format
}
{ Resource Operators

  key instance category defineresource instance Register named resource instance in category
  key category undefineresource – Remove resource registration
  key category findresource instance Return resource instance identified by key in
  category
  renderingintent findcolorrendering name bool Select CIE-based color rendering dictionary
  by rendering intent
  key category resourcestatus status size true Return status of resource instance
  or false
  template proc scratch category resourceforall – Enumerate resource instances in category
}
{ Virtual Memory Operators

  – save save Create VM snapshot
  save restore – Restore VM snapshot
  bool setglobal – Set VM allocation mode (false = local,
  true = global)
  – currentglobal bool Return current VM allocation mode
  any gcheck bool Return true if any is simple or in global VM,
  false if in local VM
  bool1 password startjob bool2 Start new job that will alter initial VM if
  bool1 is true
  index any defineuserobject – Define user object associated with index
  index execuserobject – Execute user object associated with index
  index undefineuserobject – Remove user object associated with index
  – UserObjects array Return current UserObjects array defined in
  userdict
}
{ Errors

  configurationerror setpagedevice or setdevparams request
  cannot be satisfied
  dictfull No more room in dictionary
  dictstackoverflow Too many begin operators
  dictstackunderflow Too many end operators
  execstackoverflow Executive stack nesting too deep
  handleerror Called to report error information
  interrupt External interrupt request (for example,
  Control-C)
  invalidaccess Attempt to violate access attribute
  invalidexit exit not in loop
  invalidfileaccess Unacceptable access string
  invalidfont Invalid Font resource name or font or
  CIDFont dictionary
  invalidrestore Improper restore
  ioerror Input/output error
  limitcheck Implementation limit exceeded
  nocurrentpoint Current point undefined
  rangecheck Operand out of bounds
  stackoverflow Operand stack overflow
  stackunderflow Operand stack underflow
  syntaxerror PostScript language syntax error
  timeout Time limit exceeded
  typecheck Operand of wrong type
  undefined Name not known
  undefinedfilename File not found
  undefinedresource Resource instance not found
  undefinedresult Overflow, underflow, or meaningless result
  unmatchedmark Expected mark not on stack
  unregistered Internal error
  VMerror Virtual memory exhausted
}
end;

{ Operand Stack Manipulation Operators

  any pop –                    Discard top element
  any1 any2 exch ==> any2 any1 Exchange top two elements
  any dup ==> any any          Duplicate top element
  any1 … anyn n copy any1 … anyn any1 … anyn
                               Duplicate top n elements
  anyn … any0 n index anyn … any0 anyn
                               Duplicate arbitrary element
  anyn-1 … any0 n j roll any(j-1) mod n … any0 anyn-1 … anyj mod n
                               Roll n elements up j times
  any1 … anyn clear            Discard all elements
  any1 … anyn count any1 … anyn n
                               Count elements on stack
  – mark mark                  Push mark on stack
  mark obj1 … objn cleartomark –
                               Discard elements down through mark
  mark obj1 … objn counttomark mark obj1 … objn n
                               Count elements down to mark
}
function TvEPSVectorialReader.ExecuteStackManipulationOperator(
  AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
var
  Param1, Param2, NewToken: TPSToken;
  lIndexN, lIndexJ: Integer;
  lTokens: array of TPSToken;
  i: Integer;
begin
  Result := False;

  // Discard top element
  if AToken.StrValue = 'pop' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Exit(True);
  end;
  // Exchange top two elements
  if AToken.StrValue = 'exch' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Param2 := TPSToken(Stack.Pop);
    Stack.Push(Param2);
    Stack.Push(Param1);
    Exit(True);
  end;
  // Duplicate top element
  if AToken.StrValue = 'dup' then
  begin
    Param1 := TPSToken(Stack.Pop);
    NewToken := Param1.Duplicate();
    Stack.Push(Param1);
    Stack.Push(NewToken);
    Exit(True);
  end;
  // Duplicate arbitrary element
  if AToken.StrValue = 'index' then
  begin
    Param1 := TPSToken(Stack.Pop);
    lIndexN := Round(Param1.FloatValue);
    SetLength(lTokens, lIndexN+1);

    // Unroll all elements necessary

    for i := 0 to lIndexN do
    begin
      lTokens[i] := TPSToken(Stack.Pop);
      Param2 := lTokens[i];
      if Param2 = nil then
      begin
        // raise Exception.Create('[TvEPSVectorialReader.ExecuteStackManipulationOperator] nil element poped in operator index');
        Exit(True);
      end;
    end;

    // Duplicate the disired token

    NewToken := lTokens[lIndexN].Duplicate();

    // Roll them back

    for i := lIndexN downto 0 do
    begin
      Stack.Push(lTokens[i]);
    end;

    // Roll the duplicated element too

    Stack.Push(NewToken);

    Exit(True);
  end;
  // anyn-1 … any0 n j roll any(j-1) mod n … any0 anyn-1 … anyj mod n
  //
  // performs a circular shift of the objects anyn-1 through any0 on the operand stack
  // by the amount j. Positive j indicates upward motion on the stack, whereas negative
  // j indicates downward motion.
  // n must be a nonnegative integer and j must be an integer. roll first removes these
  // operands from the stack; there must be at least n additional elements. It then performs
  // a circular shift of these n elements by j positions.
  // If j is positive, each shift consists of removing an element from the top of the stack
  // and inserting it between element n - 1 and element n of the stack, moving all in8.2
  // tervening elements one level higher on the stack. If j is negative, each shift consists
  // of removing element n - 1 of the stack and pushing it on the top of the stack,
  // moving all intervening elements one level lower on the stack.
  //
  // Examples    N J
  // (a) (b) (c) 3 -1 roll => (b) (c) (a)
  // (a) (b) (c) 3 1 roll  => (c) (a) (b)
  // (a) (b) (c) 3 0 roll  => (a) (b) (c)
  if AToken.StrValue = 'roll' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Param2 := TPSToken(Stack.Pop);
    lIndexJ := Round(Param1.FloatValue);
    lIndexN := Round(Param2.FloatValue);

    {$ifdef FPVECTORIALDEBUG_ROLL}
    WriteLn(Format('[TvEPSVectorialReader] roll: N=%d J=%d', [lIndexN, lIndexJ]));
    {$endif}

    if lIndexN < 0 then raise Exception.Create('[TvEPSVectorialReader.ExecuteStackManipulationOperator] rool operator: n must be positive');

    if lIndexJ = 0 then Exit;

    SetLength(lTokens, lIndexN);

    // Unroll all elements necessary

    for i := 0 to lIndexN-1 do
    begin
      lTokens[i] := TPSToken(Stack.Pop());
      Param2 := lTokens[i];
      if Param2 = nil then
      begin
        // raise Exception.Create('[TvEPSVectorialReader.ExecuteStackManipulationOperator] nil element poped in operator index');
        Exit(True);
      end;
    end;

    // Roll them back

    if lIndexJ > 0 then
    begin
      for i := lIndexJ-1 downto 0 do
      begin
        Stack.Push(lTokens[i]);
      end;
      for i := lIndexN-1 downto lIndexJ do
      begin
        Stack.Push(lTokens[i]);
      end;
    end
    else
    begin
      lIndexJ := -lIndexJ;

      for i := lIndexN-lIndexJ-1 downto 0 do
      begin
        Stack.Push(lTokens[i]);
      end;
      for i := lIndexN-1 downto lIndexN-lIndexJ do
      begin
        Stack.Push(lTokens[i]);
      end;
    end;

    Exit(True);
  end;
end;

{  Control Operators

  any exec –          Execute arbitrary object
  bool proc if –      Execute proc if bool is true
  bool proc1 proc2 ifelse –
                      Execute proc1 if bool is true, proc2 if false
  initial increment limit proc for –
                      Execute proc with values from initial by steps
                      of increment to limit
  int proc repeat –   Execute proc int times
  proc loop –         Execute proc an indefinite number of times
  – exit –            Exit innermost active loop
  – stop –            Terminate stopped context
  any stopped bool    Establish context for catching stop
  – countexecstack int Count elements on execution stack
  array execstack subarray Copy execution stack into array
  – quit – Terminate interpreter
  – start – Executed at interpreter startup
  Type, Attribute, and Conversion Operators
  any type name Return type of any
  any cvlit any Make object literal
  any cvx any Make object executable
  any xcheck bool     Test executable attribute
  array|packedarray|file|string executeonly array|packedarray|file|string
  Reduce access to execute-only
  array|packedarray|dict|file|string noaccess array|packedarray|dict|file|string
  Disallow any access
  array|packedarray|dict|file|string readonly array|packedarray|dict|file|string
  Reduce access to read-only
  array|packedarray|dict|file|string rcheck bool Test read access
  array|packedarray|dict|file|string wcheck bool Test write access
  num|string cvi int Convert to integer
  string cvn name Convert to name
  num|string cvr real Convert to real
  num radix string cvrs substring Convert with radix to string
  any string cvs substring Convert to string
}
function TvEPSVectorialReader.ExecuteControlOperator(AToken: TExpressionToken;
  AData: TvVectorialDocument): Boolean;
var
  Param1, Param2, Param3, Param4: TPSToken;
  NewToken: TExpressionToken;
  FloatCounter: Double;
begin
  Result := False;

  // Execute proc if bool is true
  if AToken.StrValue = 'if' then
  begin
    Param1 := TPSToken(Stack.Pop); // proc
    Param2 := TPSToken(Stack.Pop); // bool

    if not (Param1 is TProcedureToken) then
      raise Exception.Create(Format('[TvEPSVectorialReader.ExecuteControlOperator] The operator if requires a procedure. Error at line %d', [AToken.Line]));

    if Param2.BoolValue then ExecuteProcedureToken(TProcedureToken(Param1), AData);

    Exit(True);
  end;
  // Execute proc1 if bool is true, proc2 if false
  if AToken.StrValue = 'ifelse' then
  begin
    Param1 := TPSToken(Stack.Pop); // proc2
    Param2 := TPSToken(Stack.Pop); // proc1
    Param3 := TPSToken(Stack.Pop); // bool

    if not (Param1 is TProcedureToken) then
      raise Exception.Create(Format('[TvEPSVectorialReader.ExecuteControlOperator] The operator ifelse requires a procedure. Error at line %d', [AToken.Line]));
    if not (Param2 is TProcedureToken) then
      raise Exception.Create(Format('[TvEPSVectorialReader.ExecuteControlOperator] The operator ifelse requires a procedure. Error at line %d', [AToken.Line]));

    if Param3.BoolValue then ExecuteProcedureToken(TProcedureToken(Param2), AData)
    else ExecuteProcedureToken(TProcedureToken(Param1), AData);

    Exit(True);
  end;
  // Exit innermost active loop
  if AToken.StrValue = 'exit' then
  begin
    ExitCalled := True;

    Exit(True);
  end;
  // Establish context for catching stop
  if AToken.StrValue = 'stopped' then
  begin
    Param1 := TPSToken(Stack.Pop);

    if not (Param1 is TProcedureToken) then
      raise Exception.Create(Format('[TvEPSVectorialReader.ExecuteControlOperator] The operator stopped requires a procedure. Error at line %d', [AToken.Line]));

    ExecuteProcedureToken(TProcedureToken(Param1), AData);

    NewToken := TExpressionToken.Create;
    NewToken.ETType := ettOperand;
    NewToken.BoolValue := False;
    NewToken.StrValue := 'false';
    Stack.Push(NewToken);

    Exit(True);
  end;
  // Execute proc an indefinite number of times
  if AToken.StrValue = 'loop' then
  begin
    Param1 := TPSToken(Stack.Pop);

    if not (Param1 is TProcedureToken) then
      raise Exception.Create(Format('[TvEPSVectorialReader.ExecuteControlOperator] The operator loop requires a procedure. Error at line %d', [AToken.Line]));

    while True do
    begin
      ExecuteProcedureToken(TProcedureToken(Param1), AData);

      if ExitCalled then
      begin
        ExitCalled := False;
        Break;
      end;
    end;

    Exit(True);
  end;
  // initial increment limit proc for
  if AToken.StrValue = 'for' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Param2 := TPSToken(Stack.Pop);
    Param3 := TPSToken(Stack.Pop);
    Param4 := TPSToken(Stack.Pop);

    if not (Param1 is TProcedureToken) then
      raise Exception.Create(Format('[TvEPSVectorialReader.ExecuteControlOperator] The operator for requires a procedure. Error at line %d', [AToken.Line]));

    FloatCounter := Param4.FloatValue;
    while FloatCounter < Param2.FloatValue do
    begin
      ExecuteProcedureToken(TProcedureToken(Param1), AData);

      FloatCounter := FloatCounter + Param3.FloatValue;
    end;

    Exit(True);
  end;
  // tests whether the operand has the executable or the literal attribute, returning true
  // if it is executable or false if it is literal
  if AToken.StrValue = 'xcheck' then
  begin
    Param1 := TPSToken(Stack.Pop);

    NewToken := TExpressionToken.Create;
    NewToken.ETType := ettOperand;
    NewToken.BoolValue := (Param1 is TProcedureToken) or
      ((Param1 is TExpressionToken) and (TExpressionToken(Param1).ETType = ettOperator));
    if NewToken.BoolValue then NewToken.StrValue := 'true'
    else NewToken.StrValue := 'false';
    Stack.Push(NewToken);

    Exit(True);
  end;
end;

{  Painting Operators

  – erasepage –   Paint current page white
  – stroke –      Draw line along current path
  – fill –        Fill current path with current color
  – eofill –      Fill using even-odd rule
  x y width height rectstroke – Define rectangular path and stroke
  x y width height matrix rectstroke – Define rectangular path, concatenate matrix,
  and stroke
  numarray|numstring rectstroke – Define rectangular paths and stroke
  numarray|numstring matrix rectstroke – Define rectangular paths, concatenate
  matrix, and stroke
  x y width height rectfill – Fill rectangular path
  numarray|numstring rectfill – Fill rectangular paths
  userpath ustroke – Interpret and stroke userpath
  userpath matrix ustroke – Interpret userpath, concatenate matrix, and
  stroke
  userpath ufill – Interpret and fill userpath
  userpath ueofill – Fill userpath using even-odd rule
  dict shfill – Fill area defined by shading pattern
  dict image – Paint any sampled image
  width height bits/sample matrix datasrc image – Paint monochrome sampled image
  width height bits/comp matrix
  datasrc0 … datasrcncomp-1 multi ncomp colorimage – Paint color sampled image
  dict imagemask – Paint current color through mask
  width height polarity matrix datasrc imagemask – Paint current color through mask
  Insideness-Testing Operators
  x y infill bool Test whether (x, y) would be painted by fill
  userpath infill bool Test whether pixels in userpath would be
  painted by fill
  x y ineofill bool Test whether (x, y) would be painted by eofill
  userpath ineofill bool Test whether pixels in userpath would be
  painted by eofill
  x y userpath inufill bool Test whether (x, y) would be painted by ufill
  of userpath
  userpath1 userpath2 inufill bool Test whether pixels in userpath1 would be
  painted by ufill of userpath2
  x y userpath inueofill bool Test whether (x, y) would be painted by
  ueofill of userpath
  userpath1 userpath2 inueofill bool Test whether pixels in userpath1 would be
  painted by ueofill of userpath2
  x y instroke bool Test whether (x, y) would be painted by
  stroke
  x y userpath inustroke bool Test whether (x, y) would be painted by
  ustroke of userpath
  x y userpath matrix inustroke bool Test whether (x, y) would be painted by
  ustroke of userpath
  userpath1 userpath2 inustroke bool Test whether pixels in userpath1 would be
  painted by ustroke of userpath2
  userpath1 userpath2 matrix inustroke bool Test whether pixels in userpath1 would be
  painted by ustroke of userpath2
  Form and Pattern Operators
  pattern matrix makepattern pattern’ Create pattern instance from prototype
  pattern setpattern – Install pattern as current color
  comp1 … compn pattern setpattern – Install pattern as current color
  form execform – Paint form
}
function TvEPSVectorialReader.ExecutePaintingOperator(AToken: TExpressionToken;
  AData: TvVectorialDocument): Boolean;
var
  Param1, Param2: TPSToken;
begin
  Result := False;

  if AToken.StrValue = 'stroke' then
  begin
    Exit(True);
  end;

  if AToken.StrValue = 'eofill' then
  begin
    AData.SetBrushStyle(bsDiagCross);

    Exit(True);
  end;
end;

{ Device Setup and Output Operators

  – showpage – Transmit and reset current page
  – copypage – Transmit current page
  dict setpagedevice – Install page-oriented output device
  – currentpagedevice dict Return current page device parameters
  – nulldevice – Install no-output device
  Glyph and Font Operators
  key font|cidfont definefont font|cidfont Register font|cidfont in Font resource
  category
  key name|string|dict array composefont font Register composite font dictionary created
  from CMap and array of CIDFonts or fonts
  key undefinefont – Remove Font resource registration
  key findfont font|cidfont Return Font resource instance identified by
  key
  font|cidfont scale scalefont font¢|cidfont¢ Scale font|cidfont by scale to produce
  font¢|cidfont¢
  font|cidfont matrix makefont font¢|cidfont¢ Transform font|cidfont by matrix to produce
  font¢|cidfont¢
  font|cidfont setfont – Set font or CIDFont in graphics state
  – rootfont font|cidfont Return last set font or CIDFont
  – currentfont font|cidfont Return current font or CIDFont, possibly a
  descendant of rootfont
  key scale|matrix selectfont – Set font or CIDFont given name and
  transform
  string show – Paint glyphs for string in current font
  ax ay string ashow – Add (ax , ay) to width of each glyph while
  showing string
  cx cy char string widthshow – Add (cx , cy) to width of glyph for char while
  showing string
  cx cy char ax ay string awidthshow – Combine effects of ashow and widthshow
  string numarray|numstring xshow – Paint glyphs for string using x widths in
  numarray|numstring
  string numarray|numstring xyshow – Paint glyphs for string using x and y widths
  in numarray|numstring
  string numarray|numstring yshow – Paint glyphs for string using y widths in
  numarray|numstring
  name|cid glyphshow – Paint glyph for character identified by
  name|cid
  string stringwidth wx wy Return width of glyphs for string in current
  font
  proc string cshow – Invoke character mapping algorithm and
  call proc
  proc string kshow – Execute proc between characters shown from
  string
  – FontDirectory dict Return dictionary of Font resource instances
  – GlobalFontDirectory dict Return dictionary of Font resource instances
  in global VM
  – StandardEncoding array Return Adobe standard font encoding vector
  – ISOLatin1Encoding array Return ISO Latin-1 font encoding vector
  key findencoding array Find encoding vector
  wx wy llx lly urx ury setcachedevice – Declare cached glyph metrics
  w0x w0y llx lly urx ury
  w1x w1y vx vy setcachedevice2 – Declare cached glyph metrics
  wx wy setcharwidth – Declare uncached glyph metrics
  Interpreter Parameter Operators
  dict setsystemparams – Set systemwide interpreter parameters
  – currentsystemparams dict Return systemwide interpreter parameters
  dict setuserparams – Set per-context interpreter parameters
  – currentuserparams dict Return per-context interpreter parameters
  string dict setdevparams – Set parameters for input/output device
  string currentdevparams dict Return device parameters
  int vmreclaim – Control garbage collector
  int setvmthreshold – Control garbage collector
  – vmstatus level used maximum
  Report VM status
  – cachestatus bsize bmax msize mmax csize cmax blimit
  Return font cache status and parameters
  int setcachelimit – Set maximum bytes in cached glyph
  mark size lower upper setcacheparams – Set font cache parameters
  – currentcacheparams mark size lower upper
  Return current font cache parameters
  mark blimit setucacheparams – Set user path cache parameters
  – ucachestatus mark bsize bmax rsize rmax blimit
  Return user path cache status and
  parameters
}
function TvEPSVectorialReader.ExecuteDeviceSetupAndOutputOperator(
  AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
var
  Param1, Param2: TPSToken;
begin
  Result := False;

  if AToken.StrValue = 'showpage' then
  begin
    Exit(True);
  end;
end;

{ Array Operators

  int array array Create array of length int
  – [ mark Start array construction
  mark obj0 … objn-1 ] array End array construction
  array length int Return number of elements in array
  array index get any Return array element indexed by index
  array index any put – Put any into array at index
  array index count getinterval subarray Return subarray of array starting at index for
  count elements
  array1 index array2|packedarray2 putinterval – Replace subarray of array1 starting at index
  by array2|packedarray2
  any0 … anyn-1 array astore array Pop elements from stack into array
  array aload any0 … anyn-1 array Push all elements of array on stack
  array1 array2 copy subarray2 Copy elements of array1 to initial subarray of
  array2
  array proc forall – Execute proc for each element of array
  Packed Array Operators
  any0 … anyn-1 n packedarray packedarray Create packed array consisting of n elements
  from stack
  bool setpacking – Set array packing mode for { … } syntax
  (true = packed array)
  – currentpacking bool Return array packing mode
  packedarray length int Return number of elements in packedarray
  packedarray index get any Return packedarray element indexed by index
  packedarray index count getinterval subarray Return subarray of packedarray starting at
  index for count elements
  packedarray aload any0 … anyn-1 packedarray
  Push all elements of packedarray on stack
  packedarray1 array2 copy subarray2 Copy elements of packedarray1 to initial
  subarray of array2
  packedarray proc forall – Execute proc for each element of packedarray
}
function TvEPSVectorialReader.ExecuteArrayOperator(AToken: TExpressionToken;
  AData: TvVectorialDocument): Boolean;
begin
  Result := False;

end;

{ String Operators

  int string string Create string of length int
  string length int Return number of elements in string
  string index get int Return string element indexed by index
  string index int put – Put int into string at index
  string index count getinterval substring Return substring of string starting at index
  for count elements
  string1 index string2 putinterval – Replace substring of string1 starting at index
  by string2
  string1 string2 copy substring2 Copy elements of string1 to initial substring
  of string2
  string proc forall – Execute proc for each element of string
  string seek anchorsearch post match true Search for seek at start of string
  or string false
  string seek search post match pre true Search for seek in string
  or string false
  string token post any true Read token from start of string
  or false
  Relational, Boolean, and Bitwise Operators
  any1 any2 eq bool Test equal
  any1 any2 ne bool Test not equal
  num1|str1 num2|str2 ge bool Test greater than or equal
  num1|str1 num2|str2 gt bool Test greater than
  num1|str1 num2|str2 le bool Test less than or equal
  num1|str1 num2|str2 lt bool Test less than
  bool1|int1 bool2|int2 and bool3|int3 Perform logical|bitwise and
  bool1|int1 not bool2|int2 Perform logical|bitwise not
  bool1|int1 bool2|int2 or bool3|int3 Perform logical|bitwise inclusive or
  bool1|int1 bool2|int2 xor bool3|int3 Perform logical|bitwise exclusive or
  – true true Return boolean value true
  – false false Return boolean value false
  int1 shift bitshift int2 Perform bitwise shift of int1 (positive is left)
}
function TvEPSVectorialReader.ExecuteStringOperator(AToken: TExpressionToken;
  AData: TvVectorialDocument): Boolean;
var
  Param1, Param2: TPSToken;
  NewToken: TExpressionToken;
begin
  Result := False;

  // any1 any2 ne bool Test not equal
  if AToken.StrValue = 'ne' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Param2 := TPSToken(Stack.Pop);

    NewToken := TExpressionToken.Create;
    NewToken.ETType := ettOperand;
    NewToken.BoolValue := Param1.StrValue = Param2.StrValue;
    if NewToken.BoolValue then NewToken.StrValue := 'true'
    else NewToken.StrValue := 'false';
    Stack.Push(NewToken);

    Exit(True);
  end;
  // num1 num2 lt bool
  // string1 string2 lt bool
  // pops two objects from the operand stack and pushes true if the first operand is less
  // than the second, or false otherwise. If both operands are numbers, lt compares
  // their mathematical values. If both operands are strings, lt compares them element
  // by element, treating the elements as integers in the range 0 to 255, to determine
  // whether the first string is lexically less than the second. If the operands are of
  // other types or one is a string and the other is a number, a typecheck error occurs.
  if AToken.StrValue = 'lt' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Param2 := TPSToken(Stack.Pop);

    NewToken := TExpressionToken.Create;
    NewToken.ETType := ettOperand;
    NewToken.BoolValue := Param1.FloatValue > Param2.FloatValue;
    if NewToken.BoolValue then NewToken.StrValue := 'true'
    else NewToken.StrValue := 'false';
    Stack.Push(NewToken);

    Exit(True);
  end;
end;

{  Arithmetic and Math Operators

  num1 num2 add sum        Return num1 plus num2
  num1 num2 div quotient   Return num1 divided by num2
  int1 int2 idiv quotient  Return int1 divided by int2
  int1 int2 mod remainder  Return remainder after dividing int1 by int2
  num1 num2 mul product    Return num1 times num2
  num1 num2 sub difference Return num1 minus num2
  num1 abs num2            Return absolute value of num1
  num1 neg num2            Return negative of num1
  num1 ceiling num2        Return ceiling of num1
  num1 floor num2          Return floor of num1
  num1 round num2          Round num1 to nearest integer
  num1 truncate num2       Remove fractional part of num1
  num sqrt real            Return square root of num
  num den atan angle       Return arctangent of num/den in degrees
  angle cos real           Return cosine of angle degrees
  angle sin real           Return sine of angle degrees
  base exponent exp real   Raise base to exponent power
  num ln real              Return natural logarithm (base e)
  num log real             Return common logarithm (base 10)
  – rand int               Generate pseudo-random integer
  int srand –              Set random number seed
  – rrand int              Return random number seed
}
function TvEPSVectorialReader.ExecuteArithmeticAndMathOperator(
  AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
var
  Param1, Param2: TPSToken;
begin
  Result := False;

  // Division
  // Param2 Param1 div ==> Param2 div Param1
  if AToken.StrValue = 'div' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Param2 := TPSToken(Stack.Pop);
    Param1.FloatValue := Param2.FloatValue / Param1.FloatValue;
    Param1.StrValue := '00'; // Just to mark it as a number
    Stack.Push(Param1);
    Exit(True);
  end;

  // Param2 Param1 mul ==> Param2 mul Param1
  if AToken.StrValue = 'mul' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Param2 := TPSToken(Stack.Pop);
    Param1.FloatValue := Param2.FloatValue * Param1.FloatValue;
    Param1.StrValue := '00'; // Just to mark it as a number
    Stack.Push(Param1);
    Exit(True);
  end;
  // num1 num2 sub difference Return num1 minus num2
  if AToken.StrValue = 'sub' then
  begin
    Param1 := TPSToken(Stack.Pop); // num2
    Param2 := TPSToken(Stack.Pop); // num1
    Param1.FloatValue := Param2.FloatValue - Param1.FloatValue;
    Param1.StrValue := '00'; // Just to mark it as a number
    Stack.Push(Param1);
    Exit(True);
  end;
end;

{ Path Construction Operators

  – newpath –              Initialize current path to be empty
  – currentpoint x y       Return current point coordinates
  x y moveto –             Set current point to (x, y)
  dx dy rmoveto –          Perform relative moveto
  x y lineto –             Append straight line to (x, y)
  dx dy rlineto –          Perform relative lineto
  x y r angle1 angle2 arc – Append counterclockwise arc
  x y r angle1 angle2 arcn – Append clockwise arc
  x1 y1 x2 y2 r arct –     Append tangent arc
  x1 y1 x2 y2 r arcto xt1 yt1 xt2 yt2 Append tangent arc
  x1 y1 x2 y2 x3 y3 curveto – Append Bézier cubic section
  dx1 dy1 dx2 dy2 dx3 dy3 rcurveto – Perform relative curveto
  – closepath –            Connect subpath back to its starting point
  – flattenpath –          Convert curves to sequences of straight lines
  – reversepath –          Reverse direction of current path
  – strokepath –           Compute outline of stroked path
  userpath ustrokepath – Compute outline of stroked userpath
  userpath matrix ustrokepath – Compute outline of stroked userpath
  string bool charpath – Append glyph outline to current path
  userpath uappend – Interpret userpath and append to current
  path
  – clippath – Set current path to clipping path
  llx lly urx ury setbbox – Set bounding box for current path
  – pathbbox llx lly urx ury Return bounding box of current path
  move line curve close pathforall – Enumerate current path
  bool upath userpath Create userpath for current path; include
  ucache if bool is true
  – initclip – Set clipping path to device default
  – clip – Clip using nonzero winding number rule
  – eoclip – Clip using even-odd rule
  x y width height rectclip – Clip with rectangular path
  numarray|numstring rectclip – Clip with rectangular paths
  – ucache – Declare that user path is to be cached
}
function TvEPSVectorialReader.ExecutePathConstructionOperator(
  AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
var
  Param1, Param2, Param3, Param4, Param5, Param6: TPSToken;
  PosX, PosY, PosX2, PosY2, PosX3, PosY3, BaseX, BaseY: Double;
begin
  Result := False;

  //
  if AToken.StrValue = 'newpath' then
  begin
    {$ifdef FPVECTORIALDEBUG_PATHS}
    WriteLn('[TvEPSVectorialReader.ExecutePathConstructionOperator] newpath');
    {$endif}
    AData.EndPath();
    AData.StartPath();

    AData.SetPenColor(CurrentGraphicState.Color);

    Exit(True);
  end;
  // Param2 Param1 moveto ===> moveto(X=Param2, Y=Param1);
  if AToken.StrValue = 'moveto' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Param2 := TPSToken(Stack.Pop);
    PostScriptCoordsToFPVectorialCoords(Param1, Param2, PosX, PosY);
    PosX := PosX + CurrentGraphicState.TranslateX;
    PosY := PosY + CurrentGraphicState.TranslateY;
    {$ifdef FPVECTORIALDEBUG_PATHS}
    WriteLn(Format('[TvEPSVectorialReader.ExecutePathConstructionOperator] moveto %f, %f', [PosX, PosY]));
    {$endif}
    AData.AddMoveToPath(PosX, PosY);
    Exit(True);
  end;
  // Absolute LineTo
  if AToken.StrValue = 'lineto' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Param2 := TPSToken(Stack.Pop);
    PostScriptCoordsToFPVectorialCoords(Param1, Param2, PosX, PosY);
    PosX := PosX + CurrentGraphicState.TranslateX;
    PosY := PosY + CurrentGraphicState.TranslateY;
    {$ifdef FPVECTORIALDEBUG_PATHS}
    WriteLn(Format('[TvEPSVectorialReader.ExecutePathConstructionOperator] lineto %f, %f', [PosX, PosY]));
    {$endif}
    AData.AddLineToPath(PosX, PosY);
    Exit(True);
  end;
  // Relative LineTo
  if AToken.StrValue = 'rlineto' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Param2 := TPSToken(Stack.Pop);
    PostScriptCoordsToFPVectorialCoords(Param1, Param2, PosX, PosY);
    AData.GetCurrenPathPenPos(BaseX, BaseY);
    PosX := PosX + CurrentGraphicState.TranslateX;
    PosY := PosY + CurrentGraphicState.TranslateY;
    {$ifdef FPVECTORIALDEBUG_PATHS}
    WriteLn(Format('[TvEPSVectorialReader.ExecutePathConstructionOperator] rlineto %f, %f', [BaseX + PosX, BaseY + PosY]));
    {$endif}
    AData.AddLineToPath(BaseX + PosX, BaseY + PosY);
    Exit(True);
  end;
  // dx1 dy1 dx2 dy2 dx3 dy3 rcurveto –
  // (relative curveto) appends a section of a cubic Bézier curve to the current path in
  // the same manner as curveto. However, the operands are interpreted as relative
  // displacements from the current point rather than as absolute coordinates. That is,
  // rcurveto constructs a curve between the current point (x0, y0) and the endpoint
  // (x0 + dx3, y0 + dy3), using (x0 + dx1, y0 + dy1) and (x0 + dx2, y0 + dy2) as the Bézier
  // control points. In all other respects, the behavior of rcurveto is identical to that of
  // curveto.
  if AToken.StrValue = 'rcurveto' then
  begin
    Param1 := TPSToken(Stack.Pop); // dy3
    Param2 := TPSToken(Stack.Pop); // dx3
    Param3 := TPSToken(Stack.Pop); // dy2
    Param4 := TPSToken(Stack.Pop); // dx2
    Param5 := TPSToken(Stack.Pop); // dy1
    Param6 := TPSToken(Stack.Pop); // dx1
    PostScriptCoordsToFPVectorialCoords(Param5, Param6, PosX, PosY);
    PostScriptCoordsToFPVectorialCoords(Param3, Param4, PosX2, PosY2);
    PostScriptCoordsToFPVectorialCoords(Param1, Param2, PosX3, PosY3);
    AData.GetCurrenPathPenPos(BaseX, BaseY);
    BaseX := BaseX + CurrentGraphicState.TranslateX;
    BaseY := BaseY + CurrentGraphicState.TranslateY;
    {$ifdef FPVECTORIALDEBUG_PATHS}
    WriteLn(Format('[TvEPSVectorialReader.ExecutePathConstructionOperator] translate %f, %f',
      [CurrentGraphicState.TranslateX, CurrentGraphicState.TranslateY]));
    WriteLn(Format('[TvEPSVectorialReader.ExecutePathConstructionOperator] rcurveto from %f, %f via %f, %f %f, %f to %f, %f',
      [BaseX, BaseY, BaseX + PosX, BaseY + PosY, BaseX + PosX2, BaseY + PosY2, BaseX + PosX3, BaseY + PosY3]));
    {$endif}
    AData.AddBezierToPath(BaseX + PosX, BaseY + PosY, BaseX + PosX2, BaseY + PosY2, BaseX + PosX3, BaseY + PosY3);
    Exit(True);
  end;
  // x y r angle1 angle2 arc – Append counterclockwise arc
  if AToken.StrValue = 'arc' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Param2 := TPSToken(Stack.Pop);
    Param3 := TPSToken(Stack.Pop);
    Param4 := TPSToken(Stack.Pop);
    Param5 := TPSToken(Stack.Pop);
    PostScriptCoordsToFPVectorialCoords(Param4, Param5, PosX, PosY);
//    {$ifdef FPVECTORIALDEBUG}
//    WriteLn(Format('[TvEPSVectorialReader.ExecutePathConstructionOperator] rcurveto %f, %f', [BaseX + PosX, BaseY + PosY]));
//    {$endif}
//    AData.AddBezierToPath(BaseX + PosX, BaseY + PosY, BaseX + PosX2, BaseY + PosY2, BaseX + PosX3, BaseY + PosY3);
    {$ifdef FPVECTORIALDEBUG_PATHS}
    WriteLn(Format('[TvEPSVectorialReader.ExecutePathConstructionOperator] arc %f, %f', [PosX, PosY]));
    {$endif}
    Exit(True);
  end;
  // – eoclip – Clip using even-odd rule
  if AToken.StrValue = 'eoclip' then
  begin
    Exit(True);
  end
end;

{  Graphics State Operators (Device-Independent)

  – gsave –                    Push graphics state
  – grestore –                 Pop graphics state
  – clipsave –                 Push clipping path
  – cliprestore –              Pop clipping path
  – grestoreall –              Pop to bottommost graphics state
  – initgraphics –             Reset graphics state parameters
  – gstate gstate              Create graphics state object
  gstate setgstate –           Set graphics state from gstate
  gstate currentgstate gstate  Copy current graphics state into gstate
  num setlinewidth –           Set line width
  – currentlinewidth num       Return current line width
  int setlinecap –             Set shape of line ends for stroke (0 = butt,
                               1 = round, 2 = square)
  – currentlinecap int         Return current line cap
  int setlinejoin –            Set shape of corners for stroke (0 = miter,
                               1 = round, 2 = bevel)
  – currentlinejoin int Return current line join
  num setmiterlimit – Set miter length limit
  – currentmiterlimit num Return current miter limit
  bool setstrokeadjust – Set stroke adjustment (false = disable,
  true = enable)
  – currentstrokeadjust bool Return current stroke adjustment
  array offset setdash – Set dash pattern for stroking
  – currentdash array offset Return current dash pattern
  array|name setcolorspace – Set color space
  – currentcolorspace array Return current color space
  comp1 … compn setcolor – Set color components
  pattern setcolor – Set colored tiling pattern as current color
  comp1 … compn pattern setcolor – Set uncolored tiling pattern as current color
  – currentcolor comp1 … compn Return current color components
  num setgray – Set color space to DeviceGray and color to
  specified gray value (0 = black, 1 = white)
  – currentgray num Return current color as gray value
  hue saturation brightness sethsbcolor – Set color space to DeviceRGB and color to
  specified hue, saturation, brightness
  – currenthsbcolor hue saturation brightness
  Return current color as hue, saturation,
  brightness
  red green blue setrgbcolor – Set color space to DeviceRGB and color to
                               specified red, green, blue
  – currentrgbcolor red green blue Return current color as red, green, blue
  cyan magenta yellow black setcmykcolor – Set color space to DeviceCMYK and color to
  specified cyan, magenta, yellow, black
  – currentcmykcolor cyan magenta yellow black
  Return current color as cyan, magenta,
  yellow, black
}
function TvEPSVectorialReader.ExecuteGraphicStateOperatorsDI(
  AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
var
  Param1, Param2, Param3: TPSToken;
  lRed, lGreen, lBlue: Double;
  lGraphicState: TGraphicState;
begin
  Result := False;

  //
  if AToken.StrValue = 'gsave' then
  begin
    GraphicStateStack.Push(CurrentGraphicState.Duplicate());
    {$ifdef FPVECTORIALDEBUG_PATHS}
    WriteLn('[TvEPSVectorialReader.ExecuteGraphicStateOperatorsDI] gsave');
    {$endif}
    Exit(True);
  end;
  //
  if AToken.StrValue = 'grestore' then
  begin
    lGraphicState := TGraphicState(GraphicStateStack.Pop());
    if lGraphicState = nil then raise Exception.Create('[TvEPSVectorialReader.ExecuteGraphicStateOperatorsDI] grestore: call to grestore without corresponding gsave');
    CurrentGraphicState.Free;
    CurrentGraphicState := lGraphicState;
    {$ifdef FPVECTORIALDEBUG_PATHS}
    WriteLn('[TvEPSVectorialReader.ExecuteGraphicStateOperatorsDI] grestore');
    {$endif}
    Exit(True);
  end;
  //
  if AToken.StrValue = 'setlinewidth' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Exit(True);
  end;
  //
  if AToken.StrValue = 'setlinejoin' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Exit(True);
  end;
  // red green blue setrgbcolor –
  // sets the current color space in the graphics state to DeviceRGB and the current color
  // to the component values specified by red, green, and blue. Each component
  // must be a number in the range 0.0 to 1.0. If any of the operands is outside this
  // range, the nearest valid value is substituted without error indication.
  if AToken.StrValue = 'setrgbcolor' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Param2 := TPSToken(Stack.Pop);
    Param3 := TPSToken(Stack.Pop);

    lRed := EnsureRange(Param3.FloatValue, 0, 1);
    lGreen := EnsureRange(Param2.FloatValue, 0, 1);
    lBlue := EnsureRange(Param1.FloatValue, 0, 1);

    CurrentGraphicState.Color.Red := Round(lRed * $FFFF);
    CurrentGraphicState.Color.Green := Round(lGreen * $FFFF);
    CurrentGraphicState.Color.Blue := Round(lBlue * $FFFF);
    CurrentGraphicState.Color.alpha := alphaOpaque;

    AData.SetPenColor(CurrentGraphicState.Color);

    {$ifdef FPVECTORIALDEBUG_COLORS}
    WriteLn(Format('[TvEPSVectorialReader.ExecuteGraphicStateOperatorsDI] setrgbcolor r=%f g=%f b=%f',
      [Param3.FloatValue, Param2.FloatValue, Param1.FloatValue]));
    {$endif}

    Exit(True);
  end;
end;

{  Graphics State Operators (Device-Dependent)

  halftone sethalftone – Set halftone dictionary
  – currenthalftone halftone
  Return current halftone dictionary
  frequency angle proc setscreen – Set gray halftone screen by frequency, angle,
  and spot function
  frequency angle halftone setscreen – Set gray halftone screen from halftone
  dictionary
  – currentscreen frequency angle proc|halftone
  Return current gray halftone screen
  redfreq redang redproc|redhalftone
  greenfreq greenang greenproc|greenhalftone
  bluefreq blueang blueproc|bluehalftone
  grayfreq grayang grayproc|grayhalftone setcolorscreen – Set all four halftone screens
  – currentcolorscreen redfreq redang redproc|redhalftone
  greenfreq greenang greenproc|greenhalftone
  bluefreq blueang blueproc|bluehalftone
  grayfreq grayang grayproc|grayhalftone
  Return all four halftone screens
  proc settransfer – Set gray transfer function
  – currenttransfer proc
  Return current gray transfer function
  redproc greenproc blueproc grayproc setcolortransfer – Set all four transfer functions
  – currentcolortransfer redproc greenproc blueproc grayproc
  Return current transfer functions
  proc setblackgeneration – Set black-generation function
  – currentblackgeneration proc
  Return current black-generation function
  proc setundercolorremoval – Set undercolor-removal function
  – currentundercolorremoval proc
  Return current undercolor-removal
  function
  dict setcolorrendering – Set CIE-based color rendering dictionary
  – currentcolorrendering dict
  Return current CIE-based color rendering
  dictionary
  num setflat – Set flatness tolerance
  – currentflat num Return current flatness
  bool setoverprint – Set overprint parameter
  – currentoverprint bool Return current overprint parameter
  num setsmoothness – Set smoothness parameter
  – currentsmoothness num Return current smoothness parameter
  Coordinate System and Matrix Operators
  – matrix matrix Create identity matrix
  – initmatrix – Set CTM to device default
  matrix identmatrix matrix Fill matrix with identity transform
  matrix defaultmatrix matrix Fill matrix with device default matrix
  matrix currentmatrix matrix Fill matrix with CTM
  matrix setmatrix –       Replace CTM by matrix
  tx ty translate –        Translate user space by (tx , ty)
  tx ty matrix translate matrix Define translation by (tx , ty)
  sx sy scale – Scale user space by sx and sy
  sx sy matrix scale matrix Define scaling by sx and sy
  angle rotate – Rotate user space by angle degrees
  angle matrix rotate matrix Define rotation by angle degrees
  matrix concat – Replace CTM by matrix ´ CTM
  matrix1 matrix2 matrix3 concatmatrix matrix3 Fill matrix3 with matrix1 ´ matrix2
  x y transform x¢ y¢ Transform (x, y) by CTM
  x y matrix transform x¢ y¢ Transform (x, y) by matrix
  dx dy dtransform dx¢ dy¢ Transform distance (dx, dy) by CTM
  dx dy matrix dtransform dx¢ dy¢ Transform distance (dx, dy) by matrix
  x¢ y¢ itransform x y Perform inverse transform of (x¢, y¢) by
  CTM
  x¢ y¢ matrix itransform x y Perform inverse transform of (x¢, y¢) by
  matrix
  dx¢ dy¢ idtransform dx dy Perform inverse transform of distance
  (dx¢, dy¢) by CTM
  dx¢ dy¢ matrix idtransform dx dy Perform inverse transform of distance
  (dx¢, dy¢) by matrix
  matrix1 matrix2 invertmatrix matrix2 Fill matrix2 with inverse of matrix1
}
function TvEPSVectorialReader.ExecuteGraphicStateOperatorsDD(
  AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
var
  Param1, Param2: TPSToken;
begin
  Result := False;

  //
  if AToken.StrValue = 'scale' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Param2 := TPSToken(Stack.Pop);
    Exit(True);
  end;
  // tx ty translate –        Translate user space by (tx , ty)
  if AToken.StrValue = 'translate' then
  begin
    Param1 := TPSToken(Stack.Pop); // ty
    Param2 := TPSToken(Stack.Pop); // tx

    if Param2 = nil then Exit(True);

    {$ifdef FPVECTORIALDEBUG_PATHS}
    WriteLn(Format('[TvEPSVectorialReader.ExecuteGraphicStateOperatorsDI] translate %f, %f', [Param2.FloatValue, Param1.FloatValue]));
    {$endif}

    CurrentGraphicState.TranslateX := Param2.FloatValue;
    CurrentGraphicState.TranslateY := Param1.FloatValue;

    Exit(True);
  end;
  //
  if AToken.StrValue = 'rotate' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Exit(True);
  end;
end;

{  Dictionary Operators

  int dict dict Create dictionary with capacity for int
  elements
  – << mark             Start dictionary construction
  mark key1 value1 … keyn valuen >> dict
                        End dictionary construction
  dict length int       Return number of entries in dict
  dict maxlength int    Return current capacity of dict
  dict begin –          Push dict on dictionary stack
  – end –               Pop current dictionary off dictionary stack
  key value def –       Associate key and value in current dictionary
  key load value        Search dictionary stack for key and return
                        associated value
  key value store –     Replace topmost definition of key
  dict key get any      Return value associated with key in dict
  dict key value put –  Associate key with value in dict
  dict key undef –      Remove key and its value from dict
  dict key known bool Test whether key is in dict
  key where dict true   Find dictionary in which key is defined
             or false
  dict1 dict2 copy dict2 Copy contents of dict1 to dict2
  dict proc forall – Execute proc for each entry in dict
  – currentdict dict Return current dictionary
  – errordict dict Return error handler dictionary
  – $error dict Return error control and status dictionary
  – systemdict dict Return system dictionary
  – userdict dict Return writeable dictionary in local VM
  – globaldict dict Return writeable dictionary in global VM
  – statusdict dict Return product-dependent dictionary
  – countdictstack int Count elements on dictionary stack
  array dictstack subarray Copy dictionary stack into array
  – cleardictstack – Pop all nonpermanent dictionaries off
  dictionary stack
}
function TvEPSVectorialReader.ExecuteDictionaryOperators(
  AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
var
  Param1, Param2: TPSToken;
  NewToken: TExpressionToken;
begin
  Result := False;

  // Adds a dictionary definition
  if AToken.StrValue = 'def' then
  begin
    Param1 := TPSToken(Stack.Pop);
    Param2 := TPSToken(Stack.Pop);
    Dictionary.AddObject(Param2.StrValue, Param1);
    Exit(True);
  end;

  // Can be ignored
  if AToken.StrValue = 'load' then
  begin
    Exit(True);
  end;

  // Find dictionary in which key is defined
  if AToken.StrValue = 'where' then
  begin
    Param1 := TPSToken(Stack.Pop);
    NewToken := TExpressionToken.Create;
    NewToken.ETType := ettOperand;
    NewToken.BoolValue := False;
    Stack.Push(NewToken);
    Exit(True);
  end;
end;

{  Miscellaneous Operators

  proc bind proc Replace operator names in proc with
  operators; perform idiom recognition
  – null null Push null on stack
  – version string Return interpreter version
  – realtime int Return real time in milliseconds
  – usertime int Return execution time in milliseconds
  – languagelevel int Return LanguageLevel
  – product string Return product name
  – revision int Return product revision level
  – serialnumber int Return machine serial number
  – executive – Invoke interactive executive
  bool echo – Turn echoing on or off
  – prompt – Executed when ready for interactive input
}
function TvEPSVectorialReader.ExecuteMiscellaneousOperators(
  AToken: TExpressionToken; AData: TvVectorialDocument): Boolean;
begin
  Result := False;

  // Just a hint for more efficient parsing, we can ignore
  if AToken.StrValue = 'bind' then
  begin
    Exit(True);
  end;
end;

procedure TvEPSVectorialReader.PostScriptCoordsToFPVectorialCoords(AParam1,
  AParam2: TPSToken; var APosX, APosY: Double);
begin
  APosX := AParam2.FloatValue;
  APosY := AParam1.FloatValue;
end;

procedure TvEPSVectorialReader.DictionarySubstituteOperator(
  ADictionary: TStringList; var ACurToken: TPSToken);
var
  lIndex: Integer;
  SubstituteToken, NewToken: TPSToken;
begin
  lIndex := ADictionary.IndexOf(ACurToken.StrValue);
  if lIndex >= 0 then
  begin
    SubstituteToken := TPSToken(ADictionary.Objects[lIndex]);

    if SubstituteToken is TExpressionToken then
    begin
      ACurToken.StrValue := SubstituteToken.StrValue;
      ACurToken.FloatValue := SubstituteToken.FloatValue;
    end
    else if SubstituteToken is TProcedureToken then
    begin
      ACurToken := SubstituteToken;
    end;
    if ACurToken.StrValue = '' then raise Exception.Create('[TvEPSVectorialReader.DictionarySubstituteOperator] The Dictionary substitution resulted in an empty value');
  end;
end;

constructor TvEPSVectorialReader.Create;
begin
  inherited Create;

  FPointSeparator := SysUtils.DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := ',';

  Tokenizer := TPSTokenizer.Create;
  Stack := TObjectStack.Create;
  GraphicStateStack := TObjectStack.Create;
  Dictionary := TStringList.Create;
  CurrentGraphicState := TGraphicState.Create;
end;

destructor TvEPSVectorialReader.Destroy;
begin
  Tokenizer.Free;
  Stack.Free;
  GraphicStateStack.Free;
  Dictionary.Free;
  CurrentGraphicState.Free;

  inherited Destroy;
end;

procedure TvEPSVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
begin
  Tokenizer.ReadFromStream(AStream);
//  Tokenizer.DebugOut();

  // Make sure we have at least one path
  AData.StartPath();

  RunPostScript(Tokenizer.Tokens, AData);

  // Make sure we have at least one path
  AData.EndPath();
end;

initialization

  RegisterVectorialReader(TvEPSVectorialReader, vfEncapsulatedPostScript);

end.

