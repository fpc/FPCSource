{
    This file is part of the Free Pascal packages library.
    Copyright (c) 2008 by Joost van der Sluis, member of the
    Free Pascal development team

    Compatibility unit for the old regexpr unit.

    Renaming to OldRegExpr after insertion of the newer
    RegExpr unit by Andrey V. Sorokin in 2011-08.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit OldRegExpr;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}
{$H-}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Regex;
{$ELSE FPC_DOTTEDUNITS}
uses
  Regex;
{$ENDIF FPC_DOTTEDUNITS}

type
   tregexprflag = (
     ref_singleline,
     {** This indicates that a start of line is either the
         start of the pattern or a linebreak. }
     ref_multiline,
     {** The match will be done in a case-insensitive way
          according to US-ASCII character set. }
     ref_caseinsensitive);
   tregexprflags = set of tregexprflag;

   TRegExprEngine = TRegexEngine;

function GenerateRegExprEngine(regexpr : PAnsiChar;flags : tregexprflags;var RegExprEngine: TRegExprEngine): boolean;
function GenerateRegExprEngine(regexpr : PAnsiChar;flags : tregexprflags): TREGExprEngine;
procedure DestroyRegExprEngine(var regexpr : TRegExprEngine);

function RegExprPos(RegExprEngine : TRegExprEngine;p : PAnsiChar;var index,len : integer) : boolean;
function RegExprReplaceAll(RegExprEngine : TRegExprEngine;const src,newstr : ansistring;var dest : ansistring) : sizeint;

function RegExprEscapeStr (const S : AnsiString) : AnsiString;

implementation

function GenerateRegExprEngine(regexpr: PAnsiChar; flags: tregexprflags;
  var RegExprEngine: TRegExprEngine): boolean;
var ErrorPos  : Integer;
    ErrorCode : TRegexError;

begin
  RegExprEngine := TRegExprEngine.Create(regexpr);
  if ref_multiline in flags then RegExprEngine.MultiLine:=True;
  if ref_caseinsensitive in flags then RegExprEngine.IgnoreCase:=True;
  Result := RegExprEngine.Parse(ErrorPos,ErrorCode);
end;

function GenerateRegExprEngine(regexpr: PAnsiChar; flags: tregexprflags
  ): TREGExprEngine;

var r: TRegExprEngine;

begin
  GenerateRegExprEngine(regexpr,flags,r);
  GenerateRegExprEngine:=r;
end;

procedure DestroyRegExprEngine(var regexpr: TRegExprEngine);
begin
  if regexpr <> nil then
    regexpr.Free;
  regexpr := nil;
end;

function RegExprPos(RegExprEngine: TRegExprEngine; p: PAnsiChar; var index,
  len: integer): boolean;
begin
  Len := 1;
  Result := RegExprEngine.MatchString(p,index,len);
  Len := Len - index;
  Dec(Index);
  if not Result then
    begin
      index := -1;
      len := 0;
    end;
end;

function RegExprReplaceAll(RegExprEngine: TRegExprEngine; const src,
  newstr: ansistring; var dest: ansistring): sizeint;
begin
  result := RegExprEngine.ReplaceAllString(src,newstr,Dest);
end;

function RegExprEscapeStr(const S: AnsiString): AnsiString;
var
  i, len   : integer;
  s1: AnsiString;
begin
  RegExprEscapeStr:= '';
  s1:='';
  if (S = '') then
   exit;

  len := Length (S);

  for i := 1 to len do
    begin
      if (S [i] in ['(','|', '.', '*', '?', '^', '$', '-', '[', '{', '}', ']', ')', '\']) then
        begin
          s1 := s1 + '\';
        end;

      s1 := s1 + S[i];
    end;
  RegExprEscapeStr:=s1;
end;

end.

