{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    SQL syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.sql;

interface

uses
  types, syntax.highlighter;

type
  // String escaping modes for SQL
  TSqlStringEscapeMode = (
    semBackslash,  // Backslash escaping: 'I\'m here'
    semDoubled     // Doubled character escaping: 'I''m here' (Firebird, standard SQL)
  );

  { TSqlSyntaxHighlighter }

  TSqlSyntaxHighlighter = class(TSyntaxHighlighter)
  private
    FSource: string;
    FPos: integer;
    FStringEscapeMode: TSqlStringEscapeMode;
  protected
    procedure ProcessSingleQuoteString(var endPos: integer);
    procedure ProcessDoubleQuoteString(var endPos: integer);
    procedure ProcessSingleLineComment(var endPos: integer);
    procedure ProcessMultiLineComment(var endPos: integer);
    procedure ProcessNumber(var endPos: integer);
    function CheckForKeyword(var endPos: integer): boolean;
    function IsWordChar(ch: char): boolean;
    function IsHexChar(ch: char): boolean;
    class procedure CheckCategories;
    class procedure RegisterDefaultCategories; override;
    class function GetLanguages : TStringDynarray; override;
  public
    constructor Create; override;
    class var
      CategorySQL : Integer;
    function Execute(const Source: string): TSyntaxTokenArray; override;
    property StringEscapeMode: TSqlStringEscapeMode read FStringEscapeMode write FStringEscapeMode;
  end;

const
  MaxKeywordLength = 20;
  MaxKeyword = 113;

  SqlKeywordTable: array[0..MaxKeyword] of string = (
    // Basic SQL keywords
    'SELECT', 'FROM', 'WHERE', 'INSERT', 'UPDATE', 'DELETE', 'CREATE', 'DROP', 'ALTER',
    'TABLE', 'DATABASE', 'INDEX', 'VIEW', 'PROCEDURE', 'FUNCTION', 'TRIGGER',
    // Data types
    'INTEGER', 'INT', 'BIGINT', 'SMALLINT', 'DECIMAL', 'NUMERIC', 'FLOAT', 'REAL', 'DOUBLE',
    'VARCHAR', 'CHAR', 'TEXT', 'BLOB', 'CLOB', 'DATE', 'TIME', 'TIMESTAMP', 'BOOLEAN',
    // Constraints and modifiers
    'PRIMARY', 'FOREIGN', 'KEY', 'REFERENCES', 'CONSTRAINT', 'UNIQUE', 'NOT', 'NULL',
    'DEFAULT', 'CHECK', 'AUTO_INCREMENT', 'IDENTITY',
    // Joins and set operations
    'JOIN', 'INNER', 'LEFT', 'RIGHT', 'FULL', 'OUTER', 'CROSS', 'ON', 'USING',
    'UNION', 'INTERSECT', 'EXCEPT', 'MINUS',
    // Clauses and operators
    'AND', 'OR', 'IN', 'EXISTS', 'BETWEEN', 'LIKE', 'IS', 'AS', 'DISTINCT', 'ALL', 'ANY', 'SOME',
    'ORDER', 'BY', 'GROUP', 'HAVING', 'LIMIT', 'OFFSET', 'TOP',
    // Functions and aggregates
    'COUNT', 'SUM', 'AVG', 'MIN', 'MAX', 'CASE', 'WHEN', 'THEN', 'ELSE', 'END',
    'CAST', 'CONVERT', 'COALESCE', 'NULLIF',
    // Transaction control
    'BEGIN', 'COMMIT', 'ROLLBACK', 'TRANSACTION', 'SAVEPOINT',
    // Privileges and security
    'GRANT', 'REVOKE', 'ROLE', 'USER', 'PRIVILEGES',
    // Conditional and flow control
    'IF', 'ELSIF', 'ELSEIF', 'WHILE', 'FOR', 'LOOP', 'DECLARE', 'SET',
    // Schema operations
    'SCHEMA', 'CATALOG', 'DOMAIN', 'SEQUENCE'
  );

function DoSqlHighlighting(const Source: string): TSyntaxTokenArray;
function DoSqlHighlighting(const Source: string; EscapeMode: TSqlStringEscapeMode): TSyntaxTokenArray;

implementation

uses
  SysUtils;

{ TSqlSyntaxHighlighter }

procedure TSqlSyntaxHighlighter.ProcessSingleQuoteString(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos); // Skip opening quote

  while FPos <= Length(FSource) do
  begin
    if FSource[FPos] = '''' then
    begin
      if FStringEscapeMode = semDoubled then
      begin
        // Standard SQL doubled quote escaping
        if (FPos < Length(FSource)) and (FSource[FPos + 1] = '''') then
          Inc(FPos, 2) // Skip escaped quote
        else
        begin
          Inc(FPos); // Skip closing quote
          break;
        end;
      end
      else
      begin
        // Single quote always ends the string in backslash mode
        Inc(FPos);
        break;
      end;
    end
    else if (FStringEscapeMode = semBackslash) and (FSource[FPos] = '\') then
    begin
      if FPos < Length(FSource) then
        Inc(FPos); // Skip escaped character
      Inc(FPos);
    end
    else
      Inc(FPos);
  end;

  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shStrings);
end;

procedure TSqlSyntaxHighlighter.ProcessDoubleQuoteString(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos); // Skip opening quote

  while FPos <= Length(FSource) do
  begin
    if FSource[FPos] = '"' then
    begin
      if FStringEscapeMode = semDoubled then
      begin
        // Standard SQL doubled quote escaping
        if (FPos < Length(FSource)) and (FSource[FPos + 1] = '"') then
          Inc(FPos, 2) // Skip escaped quote
        else
        begin
          Inc(FPos); // Skip closing quote
          break;
        end;
      end
      else
      begin
        // Double quote always ends the string in backslash mode
        Inc(FPos);
        break;
      end;
    end
    else if (FStringEscapeMode = semBackslash) and (FSource[FPos] = '\') then
    begin
      if FPos < Length(FSource) then
        Inc(FPos); // Skip escaped character
      Inc(FPos);
    end
    else
      Inc(FPos);
  end;

  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shStrings);
end;

procedure TSqlSyntaxHighlighter.ProcessSingleLineComment(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos, 2); // Skip '--'

  // Process until end of line
  while (FPos <= Length(FSource)) and (FSource[FPos] <> #10) and (FSource[FPos] <> #13) do
    Inc(FPos);

  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shComment);
end;

procedure TSqlSyntaxHighlighter.ProcessMultiLineComment(var endPos: integer);
var
  startPos: integer;
begin
  startPos := FPos;
  Inc(FPos, 2); // Skip the opening /*

  while FPos < Length(FSource) do
  begin
    if (FSource[FPos] = '*') and (FSource[FPos + 1] = '/') then
    begin
      Inc(FPos, 2);
      break;
    end;
    Inc(FPos);
  end;

  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shComment);
end;

procedure TSqlSyntaxHighlighter.ProcessNumber(var endPos: integer);
var
  startPos: integer;
  hasDecimalPoint: boolean;
begin
  startPos := FPos;
  hasDecimalPoint := False;

  // Handle numbers (including decimals and scientific notation)
  while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9']) do
    Inc(FPos);

  // Handle decimal point
  if (FPos <= Length(FSource)) and (FSource[FPos] = '.') and not hasDecimalPoint then
  begin
    hasDecimalPoint := True;
    Inc(FPos);
    while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9']) do
      Inc(FPos);
  end;

  // Handle scientific notation (E or e)
  if (FPos <= Length(FSource)) and (FSource[FPos] in ['E', 'e']) then
  begin
    Inc(FPos);
    if (FPos <= Length(FSource)) and (FSource[FPos] in ['+', '-']) then
      Inc(FPos);
    while (FPos <= Length(FSource)) and (FSource[FPos] in ['0'..'9']) do
      Inc(FPos);
  end;

  endPos := FPos - 1;
  AddToken(Copy(FSource, startPos, endPos - startPos + 1), shNumbers);
end;

function TSqlSyntaxHighlighter.CheckForKeyword(var endPos: integer): boolean;
var
  i, j: integer;
  keyword, ukeyword: string;
begin
  Result := False;
  i := 0;

  while (FPos + i <= Length(FSource)) and (i < MaxKeywordLength) and
    IsWordChar(FSource[FPos + i]) do
    Inc(i);

  keyword := Copy(FSource, FPos, i);
  ukeyword := UpperCase(keyword);

  for j := 0 to MaxKeyword do
    if SqlKeywordTable[j] = ukeyword then
    begin
      Result := True;
      break;
    end;

  if Result then
  begin
    Inc(FPos, i);
    endPos := FPos - 1;
    AddToken(keyword, shKeyword);
  end;
end;

function TSqlSyntaxHighlighter.IsWordChar(ch: char): boolean;
begin
  Result := ch in ['a'..'z', 'A'..'Z', '0'..'9', '_'];
end;

function TSqlSyntaxHighlighter.IsHexChar(ch: char): boolean;
begin
  Result := ch in ['0'..'9', 'A'..'F', 'a'..'f'];
end;

class procedure TSqlSyntaxHighlighter.CheckCategories;
begin
  if CategorySQL = 0 then
    RegisterDefaultCategories;
end;

class procedure TSqlSyntaxHighlighter.RegisterDefaultCategories;
begin
  CategorySQL := RegisterCategory('SQL');
end;

class function TSqlSyntaxHighlighter.GetLanguages: TStringDynarray;
begin
  Result := ['sql', 'mysql', 'postgresql', 'sqlite', 'firebird', 'oracle', 'mssql', 'tsql'];
end;

constructor TSqlSyntaxHighlighter.Create;
begin
  inherited Create;
  CheckCategories;
  DefaultCategory := CategorySQL;
  FStringEscapeMode := semDoubled; // Default to standard SQL escaping
end;

function TSqlSyntaxHighlighter.Execute(const Source: string): TSyntaxTokenArray;
var
  lLen, endPos, startPos: integer;
  ch: char;
begin
  Result := Nil;
  CheckCategories;
  lLen := Length(Source);
  if lLen = 0 then
    Exit;
  FSource := Source;
  FTokens.Reset;
  FPos := 1;
  EndPos := 0;

  while FPos <= lLen do
  begin
    ch := FSource[FPos];
    case ch of
    '''':
      ProcessSingleQuoteString(endPos);
    '"':
      ProcessDoubleQuoteString(endPos);
    '-':
      begin
      if (FPos < Length(FSource)) and (FSource[FPos + 1] = '-') then
        ProcessSingleLineComment(endPos)
      else
      begin
        AddToken('-', shOperator);
        endPos := FPos;
        Inc(FPos);
      end;
      end;
    '/':
      begin
      if (FPos < Length(FSource)) and (FSource[FPos + 1] = '*') then
        ProcessMultiLineComment(endPos)
      else
      begin
        AddToken('/', shOperator);
        endPos := FPos;
        Inc(FPos);
      end;
      end;
    '0'..'9':
      ProcessNumber(endPos);
    '$': // Hexadecimal numbers (some SQL dialects)
      begin
      startPos := FPos;
      Inc(FPos);
      while (FPos <= Length(FSource)) and IsHexChar(FSource[FPos]) do
        Inc(FPos);
      endPos := FPos - 1;
      AddToken(Copy(FSource, startPos, endPos - startPos + 1), shNumbers);
      end;
    'a'..'z', 'A'..'Z', '_':
      begin
      if not CheckForKeyword(endPos) then
      begin
        startPos := FPos;
        while (FPos <= Length(FSource)) and IsWordChar(FSource[FPos]) do
          Inc(FPos);
        endPos := FPos - 1;
        AddToken(Copy(FSource, startPos, endPos - startPos + 1), shDefault);
      end;
      end;
    '(', ')', '[', ']', '{', '}', ';', ',':
      begin
      AddToken(ch, shSymbol);
      endPos := FPos;
      Inc(FPos);
      end;
    '=', '<', '>', '!', '+', '*', '%', '&', '|', '^', '~':
      begin
      startPos := FPos;
      // Handle multi-character operators
      if ch = '<' then
      begin
        if (FPos < Length(FSource)) and (FSource[FPos + 1] in ['=', '>', '<']) then
          Inc(FPos);
      end
      else if ch = '>' then
      begin
        if (FPos < Length(FSource)) and (FSource[FPos + 1] in ['=', '<']) then
          Inc(FPos);
      end
      else if ch = '!' then
      begin
        if (FPos < Length(FSource)) and (FSource[FPos + 1] = '=') then
          Inc(FPos);
      end;
      Inc(FPos);
      endPos := FPos - 1;
      AddToken(Copy(FSource, startPos, endPos - startPos + 1), shOperator);
      end;
    ' ', #9, #10, #13:
      begin
      startPos := FPos;
      while (FPos <= Length(FSource)) and (FSource[FPos] in [' ', #9, #10, #13]) do
        Inc(FPos);
      endPos := FPos - 1;
      AddToken(Copy(FSource, startPos, endPos - startPos + 1), shDefault);
      end;
    else
      AddToken(ch, shInvalid);
      endPos := FPos;
      Inc(FPos);
    end;
    if FPos = endPos then Inc(FPos);
  end;
  Result := FTokens.GetTokens;
end;

function DoSqlHighlighting(const Source: string): TSyntaxTokenArray;
var
  highlighter: TSqlSyntaxHighlighter;
begin
  highlighter := TSqlSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(Source);
  finally
    highlighter.Free;
  end;
end;

function DoSqlHighlighting(const Source: string; EscapeMode: TSqlStringEscapeMode): TSyntaxTokenArray;
var
  highlighter: TSqlSyntaxHighlighter;
begin
  highlighter := TSqlSyntaxHighlighter.Create;
  try
    highlighter.StringEscapeMode := EscapeMode;
    Result := highlighter.Execute(Source);
  finally
    highlighter.Free;
  end;
end;

initialization
  TSqlSyntaxHighlighter.Register;
end.