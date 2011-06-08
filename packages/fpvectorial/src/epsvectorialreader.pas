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
  fpvectorial, fpimage;

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

  TPostScriptScannerState = (ssSearchingToken, ssInComment);

  { TPSTokenizer }

  TPSTokenizer = class
  public
    Tokens: TPSTokens;
    constructor Create;
    destructor Destroy; override;
    procedure ReadFromStream(AStream: TStream);
    procedure DebugOut();
    function IsValidPostScriptChar(AChar: Byte): Boolean;
  end;

  { TvEPSFVectorialReader }

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
  State: TPostScriptScannerState = ssSearchingToken;
  CommentToken: TCommentToken;
begin
  while AStream.Position < AStream.Size do
  begin
    CurChar := Char(AStream.ReadByte());
    if not IsValidPostScriptChar(Byte(CurChar)) then
      raise Exception.Create('[TPSTokenizer.ReadFromStream] Invalid char: ' + IntToHex(Byte(CurChar), 2));

    case State of
      { Searching for a token }
      ssSearchingToken:
      begin
        case CurChar of
          '%':
          begin
            CommentToken := TCommentToken.Create;
            State := ssInComment;
          end;
        end;

      end;

      { Passing by comments }
      ssInComment:
      begin
        CommentToken.StrValue := CommentToken.StrValue + CurChar;

        case CurChar of
          #13:
          begin
            // Check if this is a Windows-style #13#10 line end marker by getting one more char
            if AStream.ReadByte() <> 10 then AStream.Seek(-1, soFromCurrent); // Go back if it wasnt a #13#10

            Tokens.Add(CommentToken);
            State := ssSearchingToken;
          end;
          #10:
          begin
            Tokens.Add(CommentToken);
            State := ssSearchingToken;
          end;
        end; // case
      end; // ssInComment

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

