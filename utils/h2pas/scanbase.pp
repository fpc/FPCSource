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

procedure writetree(p: presobject);

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
   SysUtils,h2poptions,h2pconst;

var
  CondText : string;
  CondPos : integer;
  CondToken : string;
  CondOk : boolean;

procedure openInputfile;

begin
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

procedure writeentry(p: presobject; var currentlevel: integer);
begin
                 if assigned(p^.p1) then
                    begin
                      WriteLn(' Entry p1[',ttypstr[p^.p1^.typ],']',p^.p1^.str);
                    end;
                 if assigned(p^.p2) then
                    begin
                      WriteLn(' Entry p2[',ttypstr[p^.p2^.typ],']',p^.p2^.str);
                    end;
                 if assigned(p^.p3) then
                    begin
                      WriteLn(' Entry p3[',ttypstr[p^.p3^.typ],']',p^.p3^.str);
                    end;
end;

procedure writetree(p: presobject);
var
 localp: presobject;
 localp1: presobject;
 currentlevel : integer;
begin
  localp:=p;
  currentlevel:=0;
  while assigned(localp) do
     begin
      WriteLn('Entry[',ttypstr[localp^.typ],']',localp^.str);
      case localp^.typ of
      { Some arguments sharing the same type }
      t_arglist:
        begin
           localp1:=localp;
           while assigned(localp1) do
              begin
                 writeentry(localp1,currentlevel);
                 localp1:=localp1^.p1;
              end;
        end;
      end;

      localp:=localp^.next;
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



procedure HandleMultiLineComment;

begin
  if not NotInCPlusBlock then
    begin
    Skip_until_eol;
    exit;
    end;
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
          commenteof;
        else
          if not stripcomment then
           write(outfile,c);
    end;
  until false;
  flush(outfile);
end;

procedure HandleSingleLineComment;

begin
  if not NotInCPlusBlock then
    begin
    skip_until_eol;
    exit;
    end;

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
      newline :
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
      #0 :
        commenteof;
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
  flush(outfile);
end;

Procedure CheckLongString;

begin
  if NotInCPlusBlock then
    begin
      if win32headers then
        return(CSTRING)
      else
        return(256);
    end
    else skip_until_eol;
end;

Procedure HandleLongInteger;

begin
  if NotInCPlusBlock then
  begin
     if (length(yytext)>1) and (yytext[1]='0') and (yytext[2] in ['0'..'7']) then
       begin
          delete(yytext,1,1);
          yytext:='&'+yytext;
       end;
     while yytext[length(yytext)] in ['L','U','l','u'] do
       Delete(yytext,length(yytext),1);
     return(NUMBER);
  end
   else skip_until_eol;
end;

Procedure HandleHexLongInteger;

begin
  if NotInCPlusBlock then
  begin
     (* handle pre- and postfixes *)
     if copy(yytext,1,2)='0x' then
       begin
          delete(yytext,1,2);
          yytext:='$'+yytext;
       end;
     while yytext[length(yytext)] in ['L','U','l','u'] do
       Delete(yytext,length(yytext),1);
     return(NUMBER);
  end
  else
   skip_until_eol;
end;

procedure HandleNumber;

var
  lPos : integer;

begin
  if NotInCPlusBlock then
  begin
    if yytext[length(yytext)] in ['F','f','L','l'] then
      Delete(yytext,length(yytext),1);
    if yytext[1]='.' then
      yytext:='0'+yytext;
    lPos:=pos('.',yytext);
    if (lPos>0) and ((lPos=length(yytext)) or not (yytext[lPos+1] in ['0'..'9'])) then
      Insert('0',yytext,lPos+1);
    return(NUMBER);
  end
  else
    skip_until_eol;
end;

Procedure HandleDeref;

begin
  if NotInCPlusBlock then
  begin
    if in_define then
      return(DEREF)
    else
      return(256);
  end
  else
    skip_until_eol;
end;

// Returns the rest of the directive line, with lines continued by a backslash joined.
function ReadDirectiveLine : string;

var
  lLine : string;

begin
  lLine:='';
  repeat
    c:=get_char;
    while (c<>newline) and (c<>#0) do
      begin
      lLine:=lLine+c;
      c:=get_char;
      end;
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
  while (Length(Result)>1) and (Result[Length(Result)] in ['u','U','l','L']) do
    SetLength(Result,Length(Result)-1);
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


Procedure HandlePreProcIfDef;

begin
  if cplusblocklevel > 0 then
    Inc(cplusblocklevel)
  else
  begin
    if cplusblocklevel < 0 then
      Dec(cplusblocklevel);
    writeln(outfile,'{$ifdef ',Trim(StripComments(ReadDirectiveLine)),'}');
    flush(outfile);
  end;
end;

Procedure HandlePreProcElse;

begin
  if cplusblocklevel < -1 then
  begin
    writeln(outfile,'{$else}');
    block_type:=bt_no;
    flush(outfile);
  end
  else
    case cplusblocklevel of
    0 :
        begin
          writeln(outfile,'{$else}');
          block_type:=bt_no;
          flush(outfile);
        end;
    1 : cplusblocklevel := -1;
    -1 : cplusblocklevel := 1;
    end;
end;

Procedure HandlePreProcEndif;

begin
   if cplusblocklevel > 0 then
   begin
     Dec(cplusblocklevel);
   end
   else
   begin
     case cplusblocklevel of
       0 : begin
             writeln(outfile,'{$endif}');
             block_type:=bt_no;
             flush(outfile);
           end;
       -1 : begin
             cplusblocklevel :=0;
            end
      else
        inc(cplusblocklevel);
      end;
   end;
end;

Procedure HandlePreProcElif;

  // Writes the #elif condition as an $elseif directive.
  procedure WriteElseIf;

  begin
    writeln(outfile,'{$elseif ',TranslateCondition(ReadDirectiveLine),'}');
    block_type:=bt_no;
    flush(outfile);
  end;

begin
  if cplusblocklevel < -1 then
    WriteElseIf
  else
    case cplusblocklevel of
    0 : WriteElseIf;
    1 : cplusblocklevel := -1;
    -1 : cplusblocklevel := 1;
    end;
end;

Procedure HandlePreProcUndef;

begin
  write(outfile,'{$undef');
  copy_until_eol;
  writeln(outfile,'}');
  flush(outfile);
end;

Procedure HandlePreProcInclude;

begin
  if NotInCPlusBlock then
    begin
      write(outfile,'{$include');
      copy_until_eol;
      writeln(outfile,'}');
      flush(outfile);
      block_type:=bt_no;
    end
  else
   skip_until_eol;
end;

Procedure HandlePreProcIf;

var
  lText : string;

begin
  if cplusblocklevel > 0 then
    Inc(cplusblocklevel)
  else
  begin
    if cplusblocklevel < 0 then
      Dec(cplusblocklevel);
    lText:=ReadDirectiveLine;
    (* #ifndef has no rule of its own and comes here *)
    if Copy(lText,1,4)='ndef' then
      writeln(outfile,'{$ifndef ',Trim(StripComments(Copy(lText,5,Length(lText)-4))),'}')
    else
      writeln(outfile,'{$if ',TranslateCondition(lText),'}');
    flush(outfile);
    block_type:=bt_no;
  end;
end;

Procedure HandlePreProcLineInfo;

begin
  if NotInCPlusBlock then
    (* preprocessor line info *)
    repeat
      c:=get_char;
      case c of
        newline :
          begin
            unget_char(c);
            exit;
          end;
        #0 :
          commenteof;
      end;
    until false
  else
    skip_until_eol;
end;

procedure HandlePreProcPragma;

begin
  if not stripinfo then
   begin
     write(outfile,'(** unsupported pragma');
     write(outfile,'#pragma');
     copy_until_eol;
     writeln(outfile,'*)');
     flush(outfile);
   end
  else
   skip_until_eol;
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

Procedure HandlePreProcDefine;

begin
  if NotInCPlusBlock then
   begin
     commentstr:='';
     in_define:=true;
     in_space_define:=1;
     return(DEFINE);
   end
  else
    skip_until_eol;
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
  if NotInCPlusBlock then
  begin
    if in_space_define=1 then
      in_space_define:=2;
    return(ID);
  end
  else
    skip_until_eol;
end;

Procedure HandleWhiteSpace;

begin
  if NotInCPlusBlock then
  begin
     if (arglevel=0) and (in_space_define=2) then
      begin
        in_space_define:=0;
        return(SPACE_DEFINE);
      end;
  end
  else
    skip_until_eol;
end;

Procedure HandleCallingConvention(aCC :integer);

begin
  if NotInCPlusBlock then
  begin
    if Win32headers then
      return(aCC)
    else
      return(ID);
  end
  else
  begin
    skip_until_eol;
  end;
end;

Procedure HandlePalmPilotCallingConvention;

begin
  if NotInCPlusBlock then
  begin
    if not palmpilot then
      return(ID)
    else
      return(SYS_TRAP);
  end
  else
  begin
    skip_until_eol;
  end;
end;

Procedure HandleSkipParenthesized;

var
  lDepth : integer;

begin
  if not NotInCPlusBlock then
    begin
    skip_until_eol;
    exit;
    end;
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
