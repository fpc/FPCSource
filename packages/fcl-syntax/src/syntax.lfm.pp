{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Lazarus and Delphi text form file (lfm, dfm) syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.lfm;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.SysUtils, syntax.highlighter;
  {$ELSE}
  Types, SysUtils, syntax.highlighter;
  {$ENDIF}

type

  { TLfmSyntaxHighlighter }

  TLfmSyntaxHighlighter = class(TSyntaxHighlighter)
  private
    FSource: string;
    FPos: integer;
    function Ch(aOffset: integer = 0): char;
    function TakeWhile(const aAllowed: TSysCharSet): string;
  protected
    class procedure CheckCategories;
    class procedure RegisterDefaultCategories; override;
    class function GetLanguages: TStringDynArray; override;
  public
    class var CategoryLfm: integer;
    constructor Create; override;
  protected
    // Split aSource in tokens
    procedure DoExecute(const aSource: string); override;
  end;

implementation

const
  LfmNameChars = ['A'..'Z', 'a'..'z', '_', '0'..'9'];
  LfmDigits = ['0'..'9'];
  LfmHexDigits = ['0'..'9', 'A'..'F', 'a'..'f'];

function TLfmSyntaxHighlighter.Ch(aOffset: integer): char;

begin
  if FPos + aOffset <= Length(FSource) then
    Result := FSource[FPos + aOffset]
  else
    Result := #0;
end;


function TLfmSyntaxHighlighter.TakeWhile(const aAllowed: TSysCharSet): string;

var
  lStart: integer;
begin
  lStart := FPos;
  while (FPos <= Length(FSource)) and (FSource[FPos] in aAllowed) do
    Inc(FPos);
  Result := SourceText(FSource, lStart, FPos - lStart);
end;


class procedure TLfmSyntaxHighlighter.CheckCategories;

begin
  if CategoryLfm = 0 then
    RegisterDefaultCategories;
end;


class procedure TLfmSyntaxHighlighter.RegisterDefaultCategories;

begin
  CategoryLfm := RegisterCategory('LFM');
  inherited RegisterDefaultCategories;
end;


class function TLfmSyntaxHighlighter.GetLanguages: TStringDynArray;

begin
  Result := ['lfm', 'dfm', 'xfm', 'fmx'];
end;


constructor TLfmSyntaxHighlighter.Create;

begin
  inherited Create;
  CheckCategories;
  DefaultCategory := CategoryLfm;
end;


procedure TLfmSyntaxHighlighter.DoExecute(const aSource: string);

var
  lWord, lLower: string;
  lLineStart, lAfterObject, lInValue: boolean;
  lStart, lDepth: integer;
begin
  CheckCategories;
  if aSource = '' then
    Exit;
  FSource := aSource;
  FPos := 1;
  // lLineStart: only whitespace so far on this line
  lLineStart := True;
  // lAfterObject: after object, inherited or inline come the name and the class
  lAfterObject := False;
  lInValue := False;
  // lDepth: nesting of ( ) value lists
  lDepth := 0;
  while FPos <= Length(FSource) do
    case Ch of
      #10, #13:
        begin
        AddToken(TakeWhile([#10, #13]), shDefault);
        lLineStart := True;
        lAfterObject := False;
        if lDepth = 0 then
          lInValue := False;
        end;
      ' ', #9:
        AddToken(TakeWhile([' ', #9]), shDefault);
      '''':
        begin
        lStart := FPos;
        Inc(FPos);
        while FPos <= Length(FSource) do
          begin
          if FSource[FPos] in [#10, #13] then
            Break;
          if FSource[FPos] = '''' then
            begin
            if Ch(1) = '''' then
              Inc(FPos, 2)
            else
              begin
              Inc(FPos);
              Break;
              end;
            end
          else
            Inc(FPos);
          end;
        AddSourceToken(FSource, lStart, FPos - lStart, shStrings);
        lLineStart := False;
        end;
      '#':
        begin
        // #13 and #$0A
        lStart := FPos;
        Inc(FPos);
        if Ch = '$' then
          begin
          Inc(FPos);
          TakeWhile(LfmHexDigits);
          end
        else
          TakeWhile(LfmDigits);
        AddSourceToken(FSource, lStart, FPos - lStart, shCharacters);
        lLineStart := False;
        end;
      '{':
        begin
        // Binary data in hexadecimal
        lStart := FPos;
        while (FPos <= Length(FSource)) and (FSource[FPos] <> '}') do
          Inc(FPos);
        if FPos <= Length(FSource) then
          Inc(FPos);
        AddSourceToken(FSource, lStart, FPos - lStart, shComment);
        lLineStart := False;
        end;
      '0'..'9', '$', '-', '+':
        if (Ch in ['-', '+']) and not (Ch(1) in LfmDigits + ['.', '$']) then
          begin
          AddSourceToken(FSource, FPos, 1, shOperator);
          Inc(FPos);
          lLineStart := False;
          end
        else
          begin
          lStart := FPos;
          if Ch in ['-', '+'] then
            Inc(FPos);
          if Ch = '$' then
            begin
            Inc(FPos);
            TakeWhile(LfmHexDigits);
            end
          else
            begin
            TakeWhile(LfmDigits);
            if (Ch = '.') and (Ch(1) in LfmDigits) then
              begin
              Inc(FPos);
              TakeWhile(LfmDigits);
              end;
            if (Ch in ['e', 'E']) and (Ch(1) in LfmDigits + ['-', '+']) then
              begin
              Inc(FPos, 2);
              TakeWhile(LfmDigits);
              end;
            end;
          AddSourceToken(FSource, lStart, FPos - lStart, shNumbers);
          lLineStart := False;
          end;
      'A'..'Z', 'a'..'z', '_':
        begin
        lWord := TakeWhile(LfmNameChars + ['.']);
        lLower := LowerCase(lWord);
        if lLineStart and ((lLower = 'object') or (lLower = 'inherited') or (lLower = 'inline')) then
          begin
          AddToken(lWord, shKeyword);
          lAfterObject := True;
          end
        else if lLineStart and ((lLower = 'end') or (lLower = 'item')) then
          AddToken(lWord, shKeyword)
        else if lAfterObject then
          AddToken(lWord, shAttribute)
        else if lLineStart and (lDepth = 0) and not lInValue then
          AddToken(lWord, shKey)
        else if (lLower = 'true') or (lLower = 'false') or (lLower = 'nil') then
          AddToken(lWord, shKeyword)
        else
          AddToken(lWord, shDefault);
        lLineStart := False;
        end;
      '=':
        begin
        AddToken('=', shOperator);
        Inc(FPos);
        lInValue := True;
        lLineStart := False;
        end;
      '(', '<', '[':
        begin
        AddSourceToken(FSource, FPos, 1, shSymbol);
        if Ch = '(' then
          Inc(lDepth);
        Inc(FPos);
        lLineStart := False;
        end;
      ')', '>', ']':
        begin
        AddSourceToken(FSource, FPos, 1, shSymbol);
        if (Ch = ')') and (lDepth > 0) then
          Dec(lDepth);
        Inc(FPos);
        lLineStart := False;
        end;
    else
      AddSourceToken(FSource, FPos, 1, shSymbol);
      Inc(FPos);
      lLineStart := False;
    end;
end;

initialization
  TLfmSyntaxHighlighter.Register;
end.
