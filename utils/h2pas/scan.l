%{
{
    $Id$
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


unit scan;

  interface

  uses
   strings,
   lexlib,yacclib;

    type
       Char=system.char;
       ttyp = (
          t_id,
          { p contains the string }
          t_arraydef,
          { }
          t_pointerdef,
          { p1 contains the definition
            if in type overrider
            or nothing for args
          }
          t_addrdef,

          t_void,
          { no field }
          t_dec,
          { }
          t_declist,
          { p1 is t_dec
            next if exists }
          t_memberdec,
          { p1 is type specifier
            p2 is declarator_list }
          t_structdef,
          { }
          t_memberdeclist,
          { p1 is memberdec
            next is next if it exist }
          t_procdef,
          { }
          t_uniondef,
          { }
          t_enumdef,
          { }
          t_enumlist,
          { }
          t_preop,
          { p contains the operator string
            p1 contains the right expr }
          t_bop,
          { p contains the operator string
            p1 contains the left expr
            p2 contains the right expr }
          t_arg,
          {
            p1 contain the typedef
            p2 the declarator (t_dec)
          }
          t_arglist,
          { }
          t_funexprlist,
          { }
          t_exprlist,
          { p1 contains the expr
            next contains the next if it exists }
          t_ifexpr,
          { p1 contains the condition expr
            p2 contains the if branch
            p3 contains the else branch }
          t_funcname,
          { p1 contains the function dname
            p2 contains the funexprlist
            p3 possibly contains the return type }
          t_typespec,
          { p1 is the type itself
            p2 the typecast expr }
          t_size_specifier,
          { p1 expr for size }
          t_default_value
          { p1 expr for value }
          );

       {tdtyp = (dt_id,dt_one,dt_two,dt_three,dt_no,dt_uop,dt_bop);
        obsolete removed }

       presobject = ^tresobject;

       tresobject = object
          typ : ttyp;
          p : pchar;
          next : presobject;
          p1,p2,p3 : presobject;
          { dtyp : tdtyp; }
          constructor init_no(t : ttyp);
          constructor init_one(t : ttyp;_p1 : presobject);
          constructor init_two(t : ttyp;_p1,_p2 : presobject);
          constructor init_three(t : ttyp;_p1,_p2,_p3 : presobject);
          constructor init_id(const s : string);
          constructor init_bop(const s : string;_p1,_p2 : presobject);
          constructor init_preop(const s : string;_p1 : presobject);
          function str : string;
          function strlength : byte;
          function get_copy : presobject;
          { can this ve considered as a constant ? }
          function is_const : boolean;
          destructor done;
       end;

     tblocktype = (bt_type,bt_const,bt_var,bt_func,bt_no);


    var
       infile : string;
       textinfile,outfile : text;
       c : char;
       aktspace : string;
       block_type : tblocktype;

    const
       in_define : boolean = false;
       { 1 after define; 2 after the ID to print the first
       separating space }
       in_space_define : byte = 0;
       arglevel : longint = 0;
       prev_line : string = '';
       last_source_line : string = 'Line number 0';

    function yylex : integer;
    function act_token : string;
    procedure internalerror(i : integer);

    procedure next_line;

    function strpnew(const s : string) : pchar;

  implementation
    uses options,converu;

    procedure internalerror(i : integer);
      begin
         writeln('Internal error ',i,' in line ',line_no);
         halt(1);
      end;

    { keep the last source line }
    procedure next_line;

      begin
         inc(line_no);
         prev_line:=last_source_line;
         readln(textinfile,last_source_line);
      end;

    procedure commenteof;
      begin
         writeln('unexpected EOF inside comment at line ',line_no);
      end;

    var         p : pchar;
    function strpnew(const s : string) : pchar;
      begin
         getmem(p,length(s)+1);
         strpcopy(p,s);
         strpnew:=p;
      end;

    const
       newline = #10;

    constructor tresobject.init_preop(const s : string;_p1 : presobject);
      begin
         typ:=t_preop;
         p:=strpnew(s);
         p1:=_p1;
         p2:=nil;
         p3:=nil;
         next:=nil;
      end;

    constructor tresobject.init_bop(const s : string;_p1,_p2 : presobject);
      begin
         typ:=t_bop;
         p:=strpnew(s);
         p1:=_p1;
         p2:=_p2;
         p3:=nil;
         next:=nil;
      end;

    constructor tresobject.init_id(const s : string);
      begin
         typ:=t_id;
         p:=strpnew(s);
         p1:=nil;
         p2:=nil;
         p3:=nil;
         next:=nil;
      end;

    constructor tresobject.init_two(t : ttyp;_p1,_p2 : presobject);
      begin
         typ:=t;
         p1:=_p1;
         p2:=_p2;
         p3:=nil;
         p:=nil;
         next:=nil;
      end;

    constructor tresobject.init_three(t : ttyp;_p1,_p2,_p3 : presobject);
      begin
         typ:=t;
         p1:=_p1;
         p2:=_p2;
         p3:=_p3;
         p:=nil;
         next:=nil;
      end;

    constructor tresobject.init_one(t : ttyp;_p1 : presobject);
      begin
         typ:=t;
         p1:=_p1;
         p2:=nil;
         p3:=nil;
         next:=nil;
         p:=nil;
      end;

    constructor tresobject.init_no(t : ttyp);
      begin
         typ:=t;
         p:=nil;
         p1:=nil;
         p2:=nil;
         p3:=nil;
         next:=nil;
      end;

    function tresobject.str : string;

      begin
         str:=strpas(p);
      end;

    function tresobject.strlength : byte;

      begin
         if assigned(p) then
           strlength:=strlen(p)
         else
           strlength:=0;
      end;

          { can this ve considered as a constant ? }
    function tresobject.is_const : boolean;

      begin
         case typ of
           t_id,t_void :
             is_const:=true;
           t_preop  :
             is_const:= ((str='-') or (str=' not ')) and p1^.is_const;
           t_bop  :
             is_const:= p2^.is_const and p1^.is_const;
         else
           is_const:=false;
         end;
      end;

    function tresobject.get_copy : presobject;
      var
         newres : presobject;
      begin
         newres:=new(presobject,init_no(typ));
         if assigned(p) then
           newres^.p:=strnew(p);
         if assigned(p1) then
           newres^.p1:=p1^.get_copy;
         if assigned(p2) then
           newres^.p2:=p2^.get_copy;
         if assigned(p3) then
           newres^.p3:=p3^.get_copy;
         if assigned(next) then
           newres^.next:=next^.get_copy;
         get_copy:=newres;
      end;

    destructor tresobject.done;
      begin
         (* writeln('disposing ',byte(typ)); *)
         if assigned(p)then strdispose(p);
         if assigned(p1) then
           dispose(p1,done);
         if assigned(p2) then
           dispose(p2,done);
         if assigned(p3) then
           dispose(p3,done);
         if assigned(next) then
           dispose(next,done);
      end;
%}

D [0-9]
%%

"/*"                   begin
                          if not stripcomment then
                            write(outfile,aktspace,'{');
                          repeat
                            c:=get_char;
                            case c of
                               '*' : begin
                                         c:=get_char;
                                         if c='/' then
                                           begin
                                              if not stripcomment then
                                                writeln(outfile,' }');
                                              flush(outfile);
                                              exit;
                                           end
                                         else
                                           begin
                                              if not stripcomment then
                                                write(outfile,' ');
                                              unget_char(c)
                                           end;
                                      end;
                               newline : begin
                                            next_line;
                                            if not stripcomment then
                                               begin
                                               writeln(outfile);
                                               write(outfile,aktspace);
                                               end;
                                         end;
                               #0 : commenteof;
                               else if not stripcomment then
                                    write(outfile,c);
                            end;
                          until false;
                          flush(outfile);
                        end;

"//"                   begin
                          If not stripcomment then
                             write(outfile,aktspace,'{');
                          repeat
                            c:=get_char;
                            case c of
                              newline : begin
                                        unget_char(c);
                                        if not stripcomment then
                                          writeln(outfile,' }');
                                        flush(outfile);
                                        exit;
                                        end;
                               #0 : commenteof;
                               else if not stripcomment then
                                    write(outfile,c);
                            flush(outfile);
                            end;
                          until false;
                          flush(outfile);
                        end;
\"[^\"]*\"              return(CSTRING);
\'[^\']*\'              return(CSTRING);
"L"\"[^\"]*\"           if win32headers then
                          return(CSTRING)
                        else
                          return(256);
"L"\'[^\']*\'           if win32headers then
                          return(CSTRING)
                        else
                          return(256);
{D}*[U]?[L]?              begin
                           if yytext[length(yytext)]='L' then
                             dec(byte(yytext[0]));
                           if yytext[length(yytext)]='U' then
                             dec(byte(yytext[0]));
                           return(NUMBER);
                        end;
"0x"[0-9A-Fa-f]*[U]?[L]?    begin
                           (* handle pre- and postfixes *)
                           if copy(yytext,1,2)='0x' then
                             begin
                                delete(yytext,1,2);
                                yytext:='$'+yytext;
                             end;
                           if yytext[length(yytext)]='L' then
                             dec(byte(yytext[0]));
                           if yytext[length(yytext)]='U' then
                             dec(byte(yytext[0]));
                           return(NUMBER);
                        end;

{D}+(\.{D}+)?([Ee][+-]?{D}+)?
                       begin
                       return(NUMBER);
                       end;

"->"                    if in_define then
                          return(DEREF)
                        else
                          return(256);
"-"                     return(MINUS);
"=="                    return(EQUAL);
"!="                    return(UNEQUAL);
">="                    return(GTE);
"<="                    return(LTE);
">>"                    return(_SHR);
"##"                    return(STICK);
"<<"                    return(_SHL);
">"                     return(GT);
"<"                     return(LT);
"|"                     return(_OR);
"&"                     return(_AND);
"!"                     return(_NOT);
"/"                     return(_SLASH);
"+"                     return(_PLUS);
"?"                     return(QUESTIONMARK);
":"                     return(COLON);
","                     return(COMMA);
"["                     return(LECKKLAMMER);
"]"                     return(RECKKLAMMER);
"("                     begin
                           inc(arglevel);
                           return(LKLAMMER);
                        end;
")"                     begin
                           dec(arglevel);
                           return(RKLAMMER);
                        end;
"*"                     return(STAR);
"..."                   return(ELLIPSIS);
"."                     if in_define then
                          return(POINT)
                        else
                          return(256);
"="                     return(_ASSIGN);
"extern"                return(EXTERN);
"STDCALL"               if Win32headers then
                          return(STDCALL)
                        else
                          return(ID);
"CDECL"                 if not Win32headers then
                          return(ID)
                        else
                          return(CDECL);
"PASCAL"                            if not Win32headers then
                          return(ID)
                        else
                          return(PASCAL);
"PACKED"                            if not Win32headers then
                          return(ID)
                        else
                          return(_PACKED);
"WINAPI"                if not Win32headers then
                          return(ID)
                        else
                          return(WINAPI);
"SYS_TRAP"              if not palmpilot then
                          return(ID)
                        else
                          return(SYS_TRAP);
"WINGDIAPI"             if not Win32headers then
                          return(ID)
                        else
                          return(WINGDIAPI);
"CALLBACK"                       if not Win32headers then
                          return(ID)
                        else
                          return(CALLBACK);
"EXPENTRY"                       if not Win32headers then
                          return(ID)
                        else
                          return(CALLBACK);

"void"                  return(VOID);
"VOID"                  return(VOID);
"#ifdef __cplusplus"[ \t]*\n"extern \"C\" {"\n"#endif"
                        writeln(outfile,'{ C++ extern C conditionnal removed }');
"#ifdef __cplusplus"[ \t]*\n"}"\n"#endif"
                        writeln(outfile,'{ C++ end of extern C conditionnal removed }');

"#else"                 begin
                           writeln(outfile,'{$else}');
                           block_type:=bt_no;
                           flush(outfile);
                        end;
"#endif"                begin
                           writeln(outfile,'{$endif}');
                           block_type:=bt_no;
                           flush(outfile);
                        end;
"#elif"                begin
                           write(outfile,'(*** was #elif ****)');
                           write(outfile,'{$else');
                                          c:=get_char;
                           while c<>newline do
                             begin write(outfile,c);c:=get_char;end;
                           writeln(outfile,'}');
                           block_type:=bt_no;
                           flush(outfile);
                           next_line;
                        end;
"#undef"                begin
                           write(outfile,'{$undef');
                                          c:=get_char;
                           while c<>newline do
                             begin write(outfile,c);c:=get_char;end;
                           writeln(outfile,'}');
                           flush(outfile);
                           next_line;
                        end;
"#error"                begin
                           write(outfile,'{$error');
                           c:=get_char;
                           while c<>newline do
                             begin
                                write(outfile,c);
                                c:=get_char;
                             end;
                           writeln(outfile,'}');
                           flush(outfile);
                           next_line;
                        end;

"#include"              begin
                           write(outfile,'{$include');
                                          c:=get_char;
                           while c<>newline do
                             begin write(outfile,c);c:=get_char;end;
                           writeln(outfile,'}');
                           flush(outfile);
                           block_type:=bt_no;
                           next_line;
                        end;
"#if"                   begin
                           write(outfile,'{$if');
                                          c:=get_char;
                           while c<>newline do
                             begin write(outfile,c);c:=get_char;end;
                           writeln(outfile,'}');
                           flush(outfile);
                           block_type:=bt_no;
                           next_line;
                        end;
"#pragma"               begin
                           write(outfile,'(** unsupported pragma');
                           write(outfile,'#pragma');
                                          c:=get_char;
                           while c<>newline do
                             begin write(outfile,c);c:=get_char;end;
                           writeln(outfile,'*)');
                           flush(outfile);
                           block_type:=bt_no;
                           next_line;
                        end;
"#define"               begin
                           in_define:=true;
                           in_space_define:=1;
                           return(DEFINE);
                        end;
"char"                  return(_CHAR);
"union"                 return(UNION);
"enum"                  return(ENUM);
"struct"                return(STRUCT);
"{"                     return(LGKLAMMER);
"}"                     return(RGKLAMMER);
"typedef"               return(TYPEDEF);
"int"                   return(INT);
"short"                 return(SHORT);
"long"                  return(LONG);
"unsigned"              return(UNSIGNED);
"float"                 return(REAL);
"const"                 return(_CONST);
"CONST"                 return(_CONST);
"FAR"                   return(_FAR);
"far"                   return(_FAR);
"NEAR"                   return(_NEAR);
"near"                   return(_NEAR);
"HUGE"                   return(_HUGE);
"huge"                   return(_HUGE);
[A-Za-z_][A-Za-z0-9_]*  begin
                           if in_space_define=1 then
                             in_space_define:=2;
                           return(ID);
                        end;
";"                     return(SEMICOLON);
[ \f\t]                 if arglevel=0 then
                          if in_space_define=2 then
                            begin
                               in_space_define:=0;
                               return(SPACE_DEFINE);
                            end;
\\\n                    begin
                           next_line;
                           if arglevel=0 then
                             if in_space_define=2 then
                               begin
                                  in_space_define:=0;
                                  return(SPACE_DEFINE);
                               end;
                        end;
\n                      begin
                           next_line;
                           if in_define then
                             begin
                                 in_define:=false;
                                 in_space_define:=0;
                                 return(NEW_LINE);
                             end;
                        end;
.                       begin
                           writeln('Illegal character in line ',line_no);
                           writeln(last_source_line);
                           return(256 { error });
                        end;
%%

    function act_token : string;
      begin
         act_token:=yytext;
      end;

Function ForceExtension(Const HStr,ext:String):String;
{
  Return a filename which certainly has the extension ext
  (no dot in ext !!)
}
var
  j : longint;
begin
  j:=length(Hstr);
  while (j>0) and (Hstr[j]<>'.') do
   dec(j);
  if j=0 then
   j:=255;
  ForceExtension:=Copy(Hstr,1,j-1)+'.'+Ext;
end;

  begin
     ProcessOptions;
     line_no := 1;
     assign(yyinput, inputfilename);
     reset(yyinput);
     assign(textinfile, inputfilename);
     reset(textinfile);
     readln(textinfile,last_source_line);
     assign(outfile, outputfilename);
     rewrite(outfile);
     if not(includefile) then
       begin
          writeln(outfile,'unit ',unitname,';');
          writeln(outfile);
          writeln(outfile,'{  Automatically converted by H2PAS.EXE from '+inputfilename);
          writeln(outfile,'   Utility made by Florian Klaempfl 25th-28th september 96');
          writeln(outfile,'   Improvements made by Mark A. Malakanov 22nd-25th may 97 ');
          writeln(outfile,'   Further improvements by Michael Van Canneyt, April 1998 ');
          writeln(outfile,'   define handling and error recovery by Pierre Muller, June 1998 }');
          writeln(outfile);
          writeln(outfile);
          writeln(outfile,'  interface');
          writeln(outfile);
          writeln(outfile,'  { C default packing is dword }');
          writeln(outfile);
          writeln(outfile,'{$PACKRECORDS 4}');
       end;
     if UsePPointers then
       begin
       { Define some pointers to basic pascal types }
       writeln(outfile);
       Writeln(outfile,' { Pointers to basic pascal types, inserted by h2pas conversion program.}');
       Writeln(outfile,'  Type');
       Writeln(outfile,'     PLongint  = ^Longint;');
       Writeln(outfile,'     PByte     = ^Byte;');
       Writeln(outfile,'     PWord     = ^Word;');
       Writeln(outfile,'     PInteger  = ^Integer;');
       Writeln(outfile,'     PCardinal = ^Cardinal;');
       Writeln(outfile,'     PReal     = ^Real;');
       Writeln(outfile,'     PDouble   = ^Double;');
       Writeln(outfile);
       end;
  end.

