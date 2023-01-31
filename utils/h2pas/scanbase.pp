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

procedure openInputfile;

const
   newline = #10;

implementation

uses
   h2poptions,h2pconst;

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
     if yytext[1]='0' then
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

begin
  if NotInCPlusBlock then
  begin
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
 
Procedure HandlePreProcIfDef;

begin
  if cplusblocklevel > 0 then
    Inc(cplusblocklevel)
  else
  begin
    if cplusblocklevel < 0 then
      Dec(cplusblocklevel);
    write(outfile,'{$ifdef ');
    copy_until_eol;
    writeln(outfile,'}');
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

begin
  if cplusblocklevel < -1 then
  begin
    if not stripinfo then
      write(outfile,'(*** was #elif ****)');
    write(outfile,'{$else');
    copy_until_eol;
    writeln(outfile,'}');
    block_type:=bt_no;
    flush(outfile);
  end
  else
    case cplusblocklevel of
    0 :
        begin
          if not stripinfo then
            write(outfile,'(*** was #elif ****)');
          write(outfile,'{$else');
          copy_until_eol;
          writeln(outfile,'}');
          block_type:=bt_no;
          flush(outfile);
        end;
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

begin
  if cplusblocklevel > 0 then
    Inc(cplusblocklevel)
  else
  begin
    if cplusblocklevel < 0 then
      Dec(cplusblocklevel);
    write(outfile,'{$if');
    copy_until_eol;
    writeln(outfile,'}');
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
      writeln(outfile,'{ C++ end of extern C conditionnal removed }')
    else
      writeln(outfile,'{ C++ extern C conditionnal removed }');
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

Procedure HandleIllegalCharacter;
begin
   writeln('Illegal character in line ',yylineno);
   writeln('"',yyline,'"');
   return(256);
end;

end.
