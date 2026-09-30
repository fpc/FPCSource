{
    Copyright (c) 1998-2000 by Florian Klaempfl

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}


unit scanbase;
{$H+}
{$modeswitch result}
{$modeswitch out}

interface

uses
  h2plexlib, h2ptypes;




var
   infile : string;
   outfile : text;
   c : char;
   aktspace : string;
   block_type : tblocktype;
   commentstr: string;

const
   in_define : boolean = false;
   { True if define spans to the next line }
   cont_line : boolean = false;
   { 1 after define; 2 after the ID to print the first separating space }
   in_space_define : byte = 0;
   arglevel : longint = 0;

   {> 1 = ifdef level in a ifdef C++ block
      1 = first level in an ifdef block
      0 = not in an ifdef block
     -1 = in else part of ifdef block, process like we weren't in the block
          but skip the incoming end.
    > -1 = ifdef sublevel in an else block.
   }
   cplusblocklevel : LongInt = 0;


procedure internalerror(i : integer);


function NotInCPlusBlock : Boolean; inline;
procedure skip_until_eol;
procedure commenteof;
procedure copy_until_eol;
procedure HandleMultiLineComment;
procedure HandleSingleLineComment;
Procedure CheckLongString;
Procedure HandleContinuation;
Procedure HandleEOL;
Procedure HandleWhiteSpace;
Procedure HandleIdentifier;
Procedure HandleLongInteger;
Procedure HandleHexLongInteger;
Procedure HandleNumber;
Procedure HandleDeref;
Procedure HandleCallingConvention(aCC : Integer);
Procedure HandlePalmPilotCallingConvention;
Procedure HandleIllegalCharacter;
// Skips the parenthesized argument of __attribute__, __declspec or __asm__.
Procedure HandleSkipParenthesized;
// Counts the brace nesting: aOpen for {, otherwise for }.
Procedure HandleBrace(aOpen : Boolean);
// Returns the defines met inside braces to the input, after a semicolon outside braces.
Procedure HandleSemicolon;

// Preprocessor routines...

Procedure HandlePreProcIfDef;
Procedure HandlePreProcIf;
Procedure HandlePreProcElse;
Procedure HandlePreProcElIf;
Procedure HandlePreProcEndif;
Procedure HandlePreProcUndef;
Procedure HandlePreProcInclude;
Procedure HandlePreProcLineInfo;
Procedure HandlePreProcPragma;
Procedure HandlePreProcDefine;
Procedure HandlePreProcError;
Procedure HandlePreProcStripConditional(isEnd : Boolean);
Procedure EnterCplusPlus;
// Returns the C preprocessor condition aText as FPC $if expression, or aText without comments when it cannot be translated.
function TranslateCondition(const aText : string) : string;

procedure openInputfile;

const
   newline = #10;

implementation

uses
   SysUtils,Classes,h2poptions,h2pconst,h2pCpp;

const
  MaxPackDepth = 32;

var
  CondText : string;
  CondPos : integer;
  CondToken : string;
  CondOk : boolean;
  // Record alignment of #pragma pack, and the alignments saved by pack(push).
  PackCurrent : string = 'C';
  PackStack : array[1..MaxPackDepth] of string;
  PackDepth : integer = 0;
  // Brace nesting outside defines, and the text of the defines met inside braces.
  BraceDepth : integer = 0;
  PendingDefines : AnsiString = '';

procedure openInputfile;

begin
  if Preprocess then
    assign(yyinput, PreprocessInput(inputfilename))
  else
    assign(yyinput, inputfilename);
  {$I-}
  reset(yyinput);
  {$I+}
  if ioresult<>0 then
  begin
   writeln('file ',inputfilename,' not found!');
   halt(1);
  end;
end;

procedure internalerror(i : integer);
  begin
     writeln('Internal error ',i,' in line ',yylineno);
     halt(1);
  end;


procedure commenteof;
  begin
     writeln('unexpected EOF inside comment at line ',yylineno);
  end;


procedure copy_until_eol;
  begin
    c:=get_char;
    while c<>newline do
     begin
       write(outfile,c);
       c:=get_char;
     end;
  end;


procedure skip_until_eol;
  begin
    c:=get_char;
    while c<>newline do
     c:=get_char;
  end;



function NotInCPlusBlock : Boolean; inline;

begin
  NotInCPlusBlock := cplusblocklevel < 1;
end;


// Skips the rest of the line inside a C++ block, and returns true when it did.
function SkipInCPlusBlock : Boolean;

begin
  Result:=not NotInCPlusBlock;
  if Result then
    skip_until_eol;
end;



procedure HandleMultiLineComment;

begin
  if SkipInCPlusBlock then
    exit;
  if not stripcomment then
    write(outfile,aktspace,'{');
  repeat
    c:=get_char;
    case c of
       '*' :
         begin
           c:=get_char;
           if c='/' then
            begin
              if not stripcomment then
               write(outfile,' }');
              c:=get_char;
              if c=newline then
                writeln(outfile);
              unget_char(c);
              flush(outfile);
              exit;
            end
           else
            begin
              if not stripcomment then
               write(outfile,'*');
              unget_char(c)
            end;
          end;
        newline :
          begin
            if not stripcomment then
             begin
               writeln(outfile);
               write(outfile,aktspace);
             end;
          end;
        { Don't write this thing out, to
          avoid nested comments.
        }
      '{','}' :
          begin
          end;
        #0 :
          begin
          commenteof;
          if not stripcomment then
            writeln(outfile,' }');
          exit;
          end;
        else
          if not stripcomment then
           write(outfile,c);
    end;
  until false;
end;

procedure HandleSingleLineComment;

begin
  if SkipInCPlusBlock then
    exit;

  commentstr:='';
  if (in_define) and not (stripcomment) then
  begin
     commentstr:='{';
  end
  else
  If not stripcomment then
    write(outfile,aktspace,'{');

  repeat
    c:=get_char;
    case c of
      newline, #0 :
        begin
          unget_char(c);
          if not stripcomment then
            begin
              if in_define then
                begin
                  commentstr:=commentstr+' }';
                end
              else
                begin
                  write(outfile,' }');
                  writeln(outfile);
                end;
            end;
          flush(outfile);
          exit;
        end;
      { Don't write this comment out,
        to avoid nested comment problems
      }
      '{','}' :
          begin
          end;
      else
        if not stripcomment then
          begin
            if in_define then
             begin
               commentstr:=commentstr+c;
             end
            else
              write(outfile,c);
          end;
    end;
  until false;
end;

Procedure CheckLongString;

begin
  if SkipInCPlusBlock then
    exit;
  if win32headers then
    return(CSTRING)
  else
    return(256);
end;

// Returns the integer literal aText without its suffixes U and L.
function WithoutIntegerSuffix(const aText : string) : string;

begin
  Result:=aText;
  while (Length(Result)>1) and (Result[Length(Result)] in ['u','U','l','L']) do
    SetLength(Result,Length(Result)-1);
end;


// Reads the characters up to the end of the line or file; c is the newline or #0 after them.
function ReadRestOfLine : AnsiString;

begin
  Result:='';
  c:=get_char;
  while (c<>newline) and (c<>#0) do
    begin
    Result:=Result+c;
    c:=get_char;
    end;
end;


Procedure HandleLongInteger;

begin
  if SkipInCPlusBlock then
    exit;
  if (length(yytext)>1) and (yytext[1]='0') and (yytext[2] in ['0'..'7']) then
    begin
       delete(yytext,1,1);
       yytext:='&'+yytext;
    end;
  yytext:=WithoutIntegerSuffix(yytext);
  return(NUMBER);
end;

Procedure HandleHexLongInteger;

begin
  if SkipInCPlusBlock then
    exit;
  (* handle pre- and postfixes *)
  if copy(yytext,1,2)='0x' then
    begin
       delete(yytext,1,2);
       yytext:='$'+yytext;
    end;
  yytext:=WithoutIntegerSuffix(yytext);
  return(NUMBER);
end;

procedure HandleNumber;

var
  lPos : integer;

begin
  if SkipInCPlusBlock then
    exit;
  if yytext[length(yytext)] in ['F','f','L','l'] then
    Delete(yytext,length(yytext),1);
  if yytext[1]='.' then
    yytext:='0'+yytext;
  lPos:=pos('.',yytext);
  if (lPos>0) and ((lPos=length(yytext)) or not (yytext[lPos+1] in ['0'..'9'])) then
    Insert('0',yytext,lPos+1);
  return(NUMBER);
end;

Procedure HandleDeref;

begin
  if SkipInCPlusBlock then
    exit;
  if in_define then
    return(DEREF)
  else
    return(256);
end;

// Returns the rest of the directive line, with lines continued by a backslash joined.
function ReadDirectiveLine : string;

var
  lLine : string;

begin
  lLine:='';
  repeat
    lLine:=lLine+ReadRestOfLine;
    lLine:=TrimRight(lLine);
    if (c=#0) or (lLine='') or (lLine[Length(lLine)]<>'\') then
      break;
    lLine[Length(lLine)]:=' ';
  until false;
  Result:=lLine;
end;


// Returns aText without C comments.
function StripComments(const aText : string) : string;

var
  lPos : integer;

begin
  Result:='';
  lPos:=1;
  while lPos<=Length(aText) do
    if Copy(aText,lPos,2)='/*' then
      begin
      lPos:=lPos+2;
      while (lPos<=Length(aText)) and (Copy(aText,lPos,2)<>'*/') do
        Inc(lPos);
      lPos:=lPos+2;
      Result:=Result+' ';
      end
    else if Copy(aText,lPos,2)='//' then
      break
    else
      begin
      Result:=Result+aText[lPos];
      Inc(lPos);
      end;
end;


// Reads the next token of CondText into CondToken; an empty token marks the end.
procedure NextCondToken;

var
  lStart : integer;
  lTwo : string;

begin
  while (CondPos<=Length(CondText)) and (CondText[CondPos] in [' ',#9]) do
    Inc(CondPos);
  CondToken:='';
  if CondPos>Length(CondText) then
    exit;
  lStart:=CondPos;
  if CondText[CondPos] in ['0'..'9'] then
    begin
    while (CondPos<=Length(CondText)) and (CondText[CondPos] in ['0'..'9','A'..'Z','a'..'z','.']) do
      Inc(CondPos);
    end
  else if CondText[CondPos] in ['A'..'Z','a'..'z','_'] then
    begin
    while (CondPos<=Length(CondText)) and (CondText[CondPos] in ['0'..'9','A'..'Z','a'..'z','_']) do
      Inc(CondPos);
    end
  else
    begin
    lTwo:=Copy(CondText,CondPos,2);
    if (lTwo='&&') or (lTwo='||') or (lTwo='==') or (lTwo='!=') or (lTwo='<=') or (lTwo='>=')
       or (lTwo='<<') or (lTwo='>>') then
      Inc(CondPos,2)
    else
      Inc(CondPos);
    end;
  CondToken:=Copy(CondText,lStart,CondPos-lStart);
end;


// Returns the C number aNumber in Pascal notation.
function ConvertCondNumber(const aNumber : string) : string;

var
  lIndex : integer;
  lOctal : boolean;

begin
  Result:=aNumber;
  if pos('.',Result)>0 then
    exit;
  Result:=WithoutIntegerSuffix(Result);
  if (Length(Result)>2) and (Result[1]='0') and (Result[2] in ['x','X']) then
    Result:='$'+Copy(Result,3,Length(Result)-2)
  else if (Length(Result)>1) and (Result[1]='0') then
    begin
    lOctal:=true;
    for lIndex:=2 to Length(Result) do
      lOctal:=lOctal and (Result[lIndex] in ['0'..'7']);
    if lOctal then
      Result:='&'+Copy(Result,2,Length(Result)-1);
    end;
end;


// Returns the C precedence of the binary operator aOp, 0 for no binary operator.
function CondPrecedence(const aOp : string) : integer;

begin
  case aOp of
    '||' : Result:=1;
    '&&' : Result:=2;
    '|' : Result:=3;
    '^' : Result:=4;
    '&' : Result:=5;
    '==','!=' : Result:=6;
    '<','<=','>','>=' : Result:=7;
    '<<','>>' : Result:=8;
    '+','-' : Result:=9;
    '*','/','%' : Result:=10;
  else
    Result:=0;
  end;
end;


// Returns the Pascal operator for the C binary operator aOp.
function CondPascalOp(const aOp : string) : string;

begin
  case aOp of
    '||','|' : Result:=' or ';
    '&&','&' : Result:=' and ';
    '^' : Result:=' xor ';
    '==' : Result:=' = ';
    '!=' : Result:=' <> ';
    '<<' : Result:=' shl ';
    '>>' : Result:=' shr ';
    '/' : Result:=' div ';
    '%' : Result:=' mod ';
  else
    Result:=' '+aOp+' ';
  end;
end;


function ParseCondExpr(aMinPrecedence : integer; out aComposite : boolean) : string; forward;


// Parses a primary condition expression: number, name, defined(name) or parenthesized expression.
function ParseCondPrimary(out aComposite : boolean) : string;

var
  lInner : boolean;

begin
  aComposite:=false;
  Result:='';
  if CondToken='(' then
    begin
    NextCondToken;
    Result:='('+ParseCondExpr(1,lInner)+')';
    CondOk:=CondOk and (CondToken=')');
    NextCondToken;
    end
  else if CondToken='defined' then
    begin
    NextCondToken;
    if CondToken='(' then
      begin
      NextCondToken;
      Result:='defined('+CondToken+')';
      NextCondToken;
      CondOk:=CondOk and (CondToken=')');
      NextCondToken;
      end
    else
      begin
      Result:='defined('+CondToken+')';
      CondOk:=CondOk and (CondToken<>'') and (CondToken[1] in ['A'..'Z','a'..'z','_']);
      NextCondToken;
      end;
    end
  else if (CondToken<>'') and (CondToken[1] in ['A'..'Z','a'..'z','_']) then
    begin
    Result:=CondToken;
    NextCondToken;
    (* a macro call cannot be translated *)
    CondOk:=CondOk and (CondToken<>'(');
    end
  else if (CondToken<>'') and (CondToken[1] in ['0'..'9']) then
    begin
    Result:=ConvertCondNumber(CondToken);
    NextCondToken;
    end
  else
    CondOk:=false;
end;


// Parses a unary condition expression.
function ParseCondUnary(out aComposite : boolean) : string;

var
  lOperand : string;
  lInner : boolean;

begin
  aComposite:=false;
  if (CondToken='!') or (CondToken='~') or (CondToken='-') or (CondToken='+') then
    begin
    Result:=CondToken;
    NextCondToken;
    lOperand:=ParseCondUnary(lInner);
    if lInner then
      lOperand:='('+lOperand+')';
    if (Result='!') or (Result='~') then
      Result:='not '+lOperand
    else if Result='-' then
      Result:='-'+lOperand
    else
      Result:=lOperand;
    end
  else
    Result:=ParseCondPrimary(aComposite);
end;


// Parses binary condition operators of at least precedence aMinPrecedence; aComposite is set for a binary result.
function ParseCondExpr(aMinPrecedence : integer; out aComposite : boolean) : string;

var
  lOp, lRight : string;
  lPrecedence : integer;
  lRightComposite : boolean;

begin
  Result:=ParseCondUnary(aComposite);
  while CondOk do
    begin
    lPrecedence:=CondPrecedence(CondToken);
    if (lPrecedence=0) or (lPrecedence<aMinPrecedence) then
      break;
    lOp:=CondToken;
    NextCondToken;
    lRight:=ParseCondExpr(lPrecedence+1,lRightComposite);
    if aComposite then
      Result:='('+Result+')';
    if lRightComposite then
      lRight:='('+lRight+')';
    Result:=Result+CondPascalOp(lOp)+lRight;
    aComposite:=true;
    end;
end;


function TranslateCondition(const aText : string) : string;

var
  lText : string;
  lComposite : boolean;

begin
  lText:=Trim(StripComments(aText));
  CondText:=lText;
  CondPos:=1;
  CondOk:=true;
  NextCondToken;
  Result:=ParseCondExpr(1,lComposite);
  if not CondOk or (CondToken<>'') or (Result='') then
    Result:=lText;
end;


// Writes the directive {$aKeyword aText} and ends the current section.
procedure WriteDirective(const aKeyword, aText : string);

begin
  writeln(outfile,'{$',aKeyword,aText,'}');
  block_type:=bt_no;
  flush(outfile);
end;


// Enters a nested #if or #ifdef; returns true when its directive is written, outside a skipped C++ block.
function EnterCondition : boolean;

begin
  Result:=cplusblocklevel<=0;
  if cplusblocklevel>0 then
    Inc(cplusblocklevel)
  else if cplusblocklevel<0 then
    Dec(cplusblocklevel);
end;


// Enters the #else or #elif branch; returns true when its directive is written, and switches a C++ block.
function EnterElseBranch : boolean;

begin
  Result:=(cplusblocklevel<-1) or (cplusblocklevel=0);
  case cplusblocklevel of
    1 : cplusblocklevel:=-1;
    -1 : cplusblocklevel:=1;
  end;
end;


Procedure HandlePreProcIfDef;

begin
  if EnterCondition then
    begin
    writeln(outfile,'{$ifdef ',Trim(StripComments(ReadDirectiveLine)),'}');
    flush(outfile);
    end;
end;

Procedure HandlePreProcElse;

begin
  if EnterElseBranch then
    WriteDirective('else','');
end;

Procedure HandlePreProcEndif;

begin
  case cplusblocklevel of
    0 : WriteDirective('endif','');
    -1 : cplusblocklevel:=0;
  else
    if cplusblocklevel>0 then
      Dec(cplusblocklevel)
    else
      Inc(cplusblocklevel);
  end;
end;

Procedure HandlePreProcElif;

begin
  if EnterElseBranch then
    WriteDirective('elseif ',TranslateCondition(ReadDirectiveLine));
end;

Procedure HandlePreProcUndef;

begin
  write(outfile,'{$undef');
  copy_until_eol;
  writeln(outfile,'}');
  flush(outfile);
end;

Procedure HandlePreProcInclude;

var
  lText : AnsiString;

begin
  if SkipInCPlusBlock then
    exit;
  lText:=Trim(ReadRestOfLine);
  if (lText<>'') and (lText[1]='<') and (pos('>',lText)>0) then
    lText:=copy(lText,1,pos('>',lText))
  else if (lText<>'') and (lText[1]='"') and (pos('"',copy(lText,2,length(lText)))>0) then
    lText:=copy(lText,1,pos('"',copy(lText,2,length(lText)))+1);
  if (lText<>'') and (lText[1]='<') then
    begin
    if not stripinfo then
      writeln(outfile,'(* #include ',lText,' ignored *)');
    end
  else
    writeln(outfile,'{$include ',lText,'}');
  flush(outfile);
  block_type:=bt_no;
end;

Procedure HandlePreProcIf;

var
  lText : string;

begin
  if not EnterCondition then
    exit;
  lText:=ReadDirectiveLine;
  (* #ifndef has no rule of its own and comes here *)
  if Copy(lText,1,4)='ndef' then
    WriteDirective('ifndef ',Trim(StripComments(Copy(lText,5,Length(lText)-4))))
  else
    WriteDirective('if ',TranslateCondition(lText));
end;

Procedure HandlePreProcLineInfo;

var
  lLine, lError : longint;

begin
  if NotInCPlusBlock then
    (* preprocessor line info *)
    repeat
      c:=get_char;
      case c of
        newline :
          begin
            unget_char(c);
            (* the next line is line number lLine of its file *)
            val(Trim(copy(yytext,2,length(yytext)-1)),lLine,lError);
            if (lError=0) and (lLine>0) then
              yylineno:=lLine-1;
            exit;
          end;
        #0 :
          exit;
      end;
    until false
  else
    skip_until_eol;
end;

// Writes the $PACKRECORDS directive for #pragma pack with the arguments aArgs; returns false for invalid arguments.
function HandlePragmaPack(const aArgs : AnsiString) : boolean;

var
  lArgs : TStringList;
  lValue : AnsiString;
  i, lNumber : longint;

begin
  lArgs:=TStringList.Create;
  lArgs.StrictDelimiter:=true;
  lArgs.CommaText:=aArgs;
  for i:=0 to lArgs.Count-1 do
    lArgs[i]:=Trim(lArgs[i]);
  lValue:='';
  if (lArgs.Count>0) and TryStrToInt(lArgs[lArgs.Count-1],lNumber) then
    lValue:=IntToStr(lNumber);
  Result:=true;
  if (lArgs.Count=0) or ((lArgs.Count=1) and (lArgs[0]='')) then
    PackCurrent:='C'
  else if lArgs[0]='push' then
    begin
    if PackDepth<MaxPackDepth then
      begin
      inc(PackDepth);
      PackStack[PackDepth]:=PackCurrent;
      end;
    if lValue<>'' then
      PackCurrent:=lValue;
    end
  else if lArgs[0]='pop' then
    begin
    if PackDepth>0 then
      begin
      PackCurrent:=PackStack[PackDepth];
      dec(PackDepth);
      end
    else
      PackCurrent:='C';
    if lValue<>'' then
      PackCurrent:=lValue;
    end
  else if (lArgs.Count=1) and (lValue<>'') then
    PackCurrent:=lValue
  else
    Result:=false;
  lArgs.Free;
  if Result then
    writeln(outfile,'{$PACKRECORDS ',PackCurrent,'}');
end;


procedure HandlePreProcPragma;

var
  lText : AnsiString;
  lOpen, lClose : integer;
  lPack : boolean;

begin
  lText:=Trim(ReadRestOfLine);
  lOpen:=pos('(',lText);
  lClose:=pos(')',lText);
  lPack:=(copy(lText,1,4)='pack') and (lOpen>0) and (Trim(copy(lText,5,lOpen-5))='') and (lClose>lOpen);
  if lPack then
    lPack:=HandlePragmaPack(copy(lText,lOpen+1,lClose-lOpen-1));
  if not lPack and not stripinfo then
    writeln(outfile,'(** unsupported pragma#pragma ',lText,'*)');
  flush(outfile);
  block_type:=bt_no;
end;

Procedure HandleContinuation;

begin
   if in_define then
   begin
     cont_line:=true;
   end
   else
   begin
     writeln('Unexpected wrap of line ',yylineno);
     writeln('"',yyline,'"');
     return(256);
   end;
end;

Procedure HandleEOL;
begin
  if not in_define then
    exit;
  in_space_define:=0;
  if cont_line then
  begin
    cont_line:=false;
  end
  else
  begin
    in_define:=false;
    if NotInCPlusBlock then
      return(NEW_LINE)
    else
      skip_until_eol
  end;
end;

const
  // The characters of a C identifier.
  IdentChars = ['A'..'Z','a'..'z','0'..'9','_'];

// Returns the define text aText with its continued lines joined.
function JoinContinuations(const aText : AnsiString) : AnsiString;

begin
  Result:=StringReplace(StringReplace(aText,'\'#13#10,' ',[rfReplaceAll]),'\'#10,' ',[rfReplaceAll]);
end;


// Returns the macro name at the start of the define text aText, after blanks; aPos is the position after the name.
function ReadDefineName(const aText : AnsiString; out aPos : integer) : AnsiString;

begin
  Result:='';
  aPos:=1;
  while (aPos<=length(aText)) and (aText[aPos] in [' ',#9]) do
    inc(aPos);
  while (aPos<=length(aText)) and (aText[aPos] in IdentChars) do
    begin
    Result:=Result+aText[aPos];
    inc(aPos);
    end;
end;


// Returns true when the define aText (the text after #define) has statements, assignments, ++, -- or a comma operator in its body;
// aName is the name of the macro.
function IsStatementDefine(const aText : AnsiString; out aName : AnsiString) : boolean;

const
  MaxDepth = 64;

var
  i, lDepth : integer;
  lCall : array[1..MaxDepth] of boolean;
  lPrev, lQuote : char;
  lLine : AnsiString;

begin
  Result:=false;
  aName:='';
  lLine:=JoinContinuations(aText);
  aName:=ReadDefineName(lLine,i);
  if (i<=length(lLine)) and (lLine[i]='(') then
    begin
    while (i<=length(lLine)) and (lLine[i]<>')') do
      inc(i);
    inc(i);
    end;
  lDepth:=0;
  lPrev:=' ';
  while i<=length(lLine) do
    begin
    case lLine[i] of
      #10, #13 :
        exit;
      '"', '''' :
        begin
        lQuote:=lLine[i];
        inc(i);
        while (i<=length(lLine)) and (lLine[i]<>lQuote) do
          begin
          if lLine[i]='\' then
            inc(i);
          inc(i);
          end;
        lPrev:='a';
        end;
      '/' :
        if (i<length(lLine)) and (lLine[i+1]='/') then
          exit
        else if (i<length(lLine)) and (lLine[i+1]='*') then
          begin
          inc(i,2);
          while (i<length(lLine)) and not ((lLine[i]='*') and (lLine[i+1]='/')) do
            inc(i);
          inc(i);
          end
        else
          lPrev:='/';
      '{', '}', ';' :
        exit(true);
      '=' :
        if (i<length(lLine)) and (lLine[i+1]='=') then
          begin
          inc(i);
          lPrev:='=';
          end
        else if lPrev in ['!','<','>'] then
          lPrev:='='
        else
          exit(true);
      '+', '-' :
        if (i<length(lLine)) and (lLine[i+1]=lLine[i]) then
          exit(true)
        else
          lPrev:=lLine[i];
      '(' :
        begin
        if lDepth<MaxDepth then
          begin
          inc(lDepth);
          lCall[lDepth]:=(lPrev in IdentChars) or (lPrev in [')',']']);
          end;
        lPrev:='(';
        end;
      ')' :
        begin
        if lDepth>0 then
          dec(lDepth);
        lPrev:=')';
        end;
      ',' :
        if (lDepth=0) or not lCall[lDepth] then
          exit(true)
        else
          lPrev:=',';
      ' ', #9, '\' : ;
    else
      lPrev:=lLine[i];
    end;
    inc(i);
    end;
end;


// Returns true when the body of the define aText (the text after #define) consists of declaration keywords only:
// storage classes, qualifiers, calling conventions and attributes; aName is the name of the macro.
function IsKeywordDefine(const aText : AnsiString; out aName : AnsiString) : boolean;

const
  MaxKeywords = 17;
  Keywords : array[1..MaxKeywords] of string = (
    'extern','static','inline','__inline','__inline__','const','volatile','register','__extension__',
    '__cdecl','__stdcall','__fastcall','__declspec','__attribute__','__attribute','__asm__','__asm');

var
  i, j, lDepth : integer;
  lWord : AnsiString;
  lFound, lKnown : boolean;

begin
  Result:=false;
  aName:='';
  aName:=ReadDefineName(aText,i);
  if (i<=length(aText)) and (aText[i]='(') then
    exit;
  lFound:=false;
  while i<=length(aText) do
    begin
    case aText[i] of
      ' ', #9, #10, #13, '\' :
        inc(i);
      '/' :
        exit(lFound and (i<length(aText)) and (aText[i+1] in ['/','*']));
      '(' :
        begin
        (* the argument of __declspec or __attribute__ *)
        if not lFound then
          exit;
        lDepth:=0;
        repeat
          if aText[i]='(' then
            inc(lDepth)
          else if aText[i]=')' then
            dec(lDepth);
          inc(i);
        until (lDepth=0) or (i>length(aText));
        end;
    else
      if not (aText[i] in IdentChars) then
        exit;
      lWord:='';
      while (i<=length(aText)) and (aText[i] in IdentChars) do
        begin
        lWord:=lWord+aText[i];
        inc(i);
        end;
      lKnown:=false;
      for j:=1 to MaxKeywords do
        if lWord=Keywords[j] then
          lKnown:=true;
      if not lKnown then
        exit;
      lFound:=true;
    end;
    end;
  Result:=lFound;
end;


// Splits the define aText (the text after #define) into its name aName, its parameters aParams
// (comma separated, '' without parameter list) and its body aBody, without continuations and trailing comment.
procedure SplitDefine(const aText : AnsiString; out aName, aParams, aBody : AnsiString);

var
  i, lEnd : integer;
  lText : AnsiString;

begin
  aName:='';
  aParams:='';
  lText:=JoinContinuations(aText);
  aName:=ReadDefineName(lText,i);
  if (i<=length(lText)) and (lText[i]='(') then
    begin
    lEnd:=i;
    while (lEnd<=length(lText)) and (lText[lEnd]<>')') do
      inc(lEnd);
    aParams:=StringReplace(copy(lText,i+1,lEnd-i-1),' ','',[rfReplaceAll]);
    i:=lEnd+1;
    end;
  aBody:=copy(lText,i,length(lText));
  lEnd:=pos('//',aBody);
  if lEnd>0 then
    aBody:=copy(aBody,1,lEnd-1);
  lEnd:=pos('/*',aBody);
  if lEnd>0 then
    aBody:=copy(aBody,1,lEnd-1);
  aBody:=Trim(aBody);
end;


// Returns true when the function macro aText has the body (), as #define OF(args) (); aName is the name of the macro.
function IsEmptyBodyDefine(const aText : AnsiString; out aName : AnsiString) : boolean;

var
  lParams, lBody : AnsiString;

begin
  SplitDefine(aText,aName,lParams,lBody);
  Result:=(lParams<>'') and (StringReplace(lBody,' ','',[rfReplaceAll])='()');
end;


// Returns true when the define aText defines a name that h2pas reads as a keyword, such as FAR or CDECL;
// aName is the name of the macro.
function IsKeywordNameDefine(const aText : AnsiString; out aName : AnsiString) : boolean;

const
  MaxNames = 22;
  Names : array[1..MaxNames] of string = (
    'STDCALL','CDECL','PASCAL','PACKED','WINAPI','SYS_TRAP','WINGDIAPI','CALLBACK','EXPENTRY','VOID','CONST',
    'FAR','far','NEAR','near','HUGE','huge','int8','int16','int32','int64','__stdcall');

var
  lParams, lBody : AnsiString;
  i : integer;

begin
  Result:=false;
  SplitDefine(aText,aName,lParams,lBody);
  for i:=1 to MaxNames do
    if aName=Names[i] then
      exit(true);
end;


// Returns the rest of the define with its continuation lines, without reading it.
function PeekDefine : AnsiString;

const
  MaxDefine = 2000;

var
  lChar, lPrev : char;
  lIndex : integer;
  lLine, lText : AnsiString;

begin
  lLine:=PeekLine;
  lText:=TrimRight(lLine);
  if (lText='') or (lText[length(lText)]<>'\') then
    exit(lLine);
  lText:='';
  lPrev:=' ';
  repeat
    lChar:=get_char;
    if lChar=#0 then
      break;
    lText:=lText+lChar;
    if ((lChar=newline) and (lPrev<>'\')) or (length(lText)>=MaxDefine) then
      break;
    if lChar<>#13 then
      lPrev:=lChar;
  until false;
  for lIndex:=length(lText) downto 1 do
    unget_char(lText[lIndex]);
  Result:=lText;
end;


// Skips the rest of the define, with its continuation lines.
procedure SkipDefine;

var
  lChar, lPrev : char;

begin
  lPrev:=' ';
  repeat
    lChar:=get_char;
    if (lChar=#0) or ((lChar=newline) and (lPrev<>'\')) then
      break;
    if lChar<>#13 then
      lPrev:=lChar;
  until false;
end;


Procedure HandleBrace(aOpen : Boolean);

begin
  if in_define then
    exit;
  if aOpen then
    inc(BraceDepth)
  else if BraceDepth>0 then
    dec(BraceDepth);
end;


Procedure HandleSemicolon;

var
  i : integer;

begin
  if in_define or (BraceDepth>0) or (PendingDefines='') then
    exit;
  unget_char(newline);
  for i:=length(PendingDefines) downto 1 do
    unget_char(PendingDefines[i]);
  unget_char(newline);
  PendingDefines:='';
end;


Procedure HandlePreProcDefine;

var
  lText, lName, lReason : AnsiString;

begin
  if SkipInCPlusBlock then
    exit;
  lText:=PeekDefine;
  if BraceDepth>0 then
    begin
    (* a define inside a struct, union, enum or function body follows the declaration *)
    SkipDefine;
    PendingDefines:=PendingDefines+'#define'+lText;
    if (lText='') or (lText[length(lText)]<>newline) then
      PendingDefines:=PendingDefines+newline;
    exit;
    end;
  if IsStatementDefine(lText,lName) then
    lReason:='macro '+lName+' with statements or side effects'
  else if IsKeywordNameDefine(lText,lName) then
    lReason:='define of the keyword '+lName
  else if IsEmptyBodyDefine(lText,lName) then
    lReason:='macro '+lName+' with an empty body'
  else if IsKeywordDefine(lText,lName) then
    lReason:='macro '+lName+' with declaration keywords'
  else
    lReason:='';
  if lReason<>'' then
    begin
    if not stripinfo then
      writeln(outfile,aktspace,'(* ',lReason,' ignored *)');
    SkipDefine;
    end
  else
    begin
    commentstr:='';
    in_define:=true;
    in_space_define:=1;
    return(DEFINE);
    end;
end;

Procedure HandlePreProcError;

begin
  write(outfile,'{$error');
  copy_until_eol;
  writeln(outfile,'}');
  flush(outfile);
end;

Procedure EnterCplusPlus;
begin
  Inc(cplusblocklevel);
end;

Procedure HandlePreProcStripConditional(isEnd : Boolean);

begin
  if not stripinfo then
    if isEnd then
      writeln(outfile,'{ C++ end of extern C conditional removed }')
    else
      writeln(outfile,'{ C++ extern C conditional removed }');
end;

Procedure HandleIdentifier;

begin
  if SkipInCPlusBlock then
    exit;
  if in_space_define=1 then
    in_space_define:=2;
  return(ID);
end;

Procedure HandleWhiteSpace;

begin
  if SkipInCPlusBlock then
    exit;
  if (arglevel=0) and (in_space_define=2) then
   begin
     in_space_define:=0;
     return(SPACE_DEFINE);
   end;
end;

Procedure HandleCallingConvention(aCC :integer);

begin
  if SkipInCPlusBlock then
    exit;
  if Win32headers then
    return(aCC)
  else
    return(ID);
end;

Procedure HandlePalmPilotCallingConvention;

begin
  if SkipInCPlusBlock then
    exit;
  if not palmpilot then
    return(ID)
  else
    return(SYS_TRAP);
end;

Procedure HandleSkipParenthesized;

var
  lDepth : integer;

begin
  if SkipInCPlusBlock then
    exit;
  repeat
    c:=get_char;
  until not ((c in [' ',#9]) or ((c=newline) and not in_define));
  if c<>'(' then
    begin
    unget_char(c);
    exit;
    end;
  lDepth:=1;
  repeat
    c:=get_char;
    case c of
      '(' : Inc(lDepth);
      ')' : Dec(lDepth);
      #0 : lDepth:=0;
    end;
  until lDepth=0;
end;


Procedure HandleIllegalCharacter;
begin
   writeln('Illegal character in line ',yylineno);
   writeln('"',yyline,'"');
   return(256);
end;

end.
