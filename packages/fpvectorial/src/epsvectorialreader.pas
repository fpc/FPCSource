{
Reads EPS files

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho

}
unit epsvectorialreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  fpvectorial, fpimage, fpvutils;

type
  TPSTokenType = (ttComment, ttFloat);

  TPSTokens = TFPList;// TPSToken;

  TPSToken = class
    StrValue: string;
    FloatValue: double;
    IntValue: Integer;
    Childs: TPSTokens;
  end;

  TCommentToken = class(TPSToken)
  end;

  TDefinitionToken = class(TPSToken)
  end;

  TExpressionToken = class(TPSToken)
  end;

  TPostScriptScannerState = (ssSearchingToken, ssInComment, ssInDefinition, ssInExpressionElement);

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
    FPointSeparator: TFormatSettings;
  public
    { General reading methods }
    Tokenizer: TPSTokenizer;
    constructor Create; override;
    Destructor Destroy; override;
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

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
  DefinitionToken: TDefinitionToken;
  ExpressionToken: TExpressionToken;
  Len: Integer;
  lIsEndOfLine: Boolean;
begin
  while AStream.Position < AStream.Size do
  begin
    CurChar := Char(AStream.ReadByte());
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
          State := ssInComment;
        end
        else if CurChar = '/' then
        begin
          DefinitionToken := TDefinitionToken.Create;
          State := ssInDefinition;
        end
        else if CurChar in ['a'..'z'] + ['A'..'Z'] + ['0'..'9'] then
        begin
          ExpressionToken := TExpressionToken.Create;
          State := ssInExpressionElement;
        end
        else if lIsEndOfLine then Continue
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
        end;
      end; // ssInComment

      // Dictionary definitions end in "def"
      ssInDefinition:
      begin
        DefinitionToken.StrValue := DefinitionToken.StrValue + CurChar;
        Len := Length(DefinitionToken.StrValue);
        if Len >= 3 then
        begin
          if (DefinitionToken.StrValue[Len-2] = 'd') and (DefinitionToken.StrValue[Len-1] = 'e') and (DefinitionToken.StrValue[Len] = 'f') then
          begin
            Tokens.Add(DefinitionToken);
            State := ssSearchingToken;
          end;
        end;
      end;

      // Goes until a space comes
      ssInExpressionElement:
      begin
        if IsPostScriptSpace(Byte(CurChar)) then
        begin
          Tokens.Add(ExpressionToken);
          State := ssSearchingToken;
        end
        else
          ExpressionToken.StrValue := ExpressionToken.StrValue + CurChar;
      end;

    end; // case
  end; // while
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
    else if Token is TDefinitionToken then
    begin
      WriteLn(Format('TDefinitionToken StrValue=%s', [Token.StrValue]));
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

constructor TvEPSVectorialReader.Create;
begin
  inherited Create;

  Tokenizer := TPSTokenizer.Create;
end;

destructor TvEPSVectorialReader.Destroy;
begin
  Tokenizer.Free;
  inherited Destroy;
end;

procedure TvEPSVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
begin
  Tokenizer.ReadFromStream(AStream);
  Tokenizer.DebugOut();
end;

initialization

  RegisterVectorialReader(TvEPSVectorialReader, vfEncapsulatedPostScript);

end.

