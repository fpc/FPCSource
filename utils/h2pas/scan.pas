
(* lexical analyzer template (TP Lex V3.0), V1.0 3-2-91 AG *)

(* global definitions: *)
{
    $Id: scan.l,v 1.7 2004/09/08 22:21:41 carl Exp $
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

    const
       version = '1.0.0';

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
          t_arrayop,
          {
            p1 contains the array expr
            p2 contains the index expressions }
          t_callop,
          {
            p1 contains the proc expr
            p2 contains the index expressions }
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
          t_default_value,
          { p1 expr for value }
          t_statement_list,
          { p1 is the statement
            next is next if it exist }
          t_whilenode,
          t_fornode,
          t_dowhilenode,
          t_switchnode,
          t_gotonode,
          t_continuenode,
          t_breaknode
          );

const
   ttypstr: array[ttyp] of string =
   (
          't_id',
          't_arraydef',
          't_pointerdef',
          't_addrdef',
          't_void',
          't_dec',
          't_declist',
          't_memberdec',
          't_structdef',
          't_memberdeclist',
          't_procdef',
          't_uniondef',
          't_enumdef',
          't_enumlist',
          't_preop',
          't_bop',
          't_arrayop',
          't_callop',
          't_arg',
          't_arglist',
          't_funexprlist',
          't_exprlist',
          't_ifexpr',
          't_funcname',
          't_typespec',
          't_size_specifier',
          't_default_value',
          't_statement_list',
          't_whilenode',
          't_fornode',
          't_dowhilenode',
          't_switchnode',
          't_gotonode',
          't_continuenode',
          't_breaknode'
   );

type

       presobject = ^tresobject;
       tresobject = object
          typ : ttyp;
          p : pchar;
          next : presobject;
          p1,p2,p3 : presobject;
          { name of int/real, then no T prefix is required }
          intname : boolean;
          constructor init_no(t : ttyp);
          constructor init_one(t : ttyp;_p1 : presobject);
          constructor init_two(t : ttyp;_p1,_p2 : presobject);
          constructor init_three(t : ttyp;_p1,_p2,_p3 : presobject);
          constructor init_id(const s : string);
          constructor init_intid(const s : string);
          constructor init_bop(const s : string;_p1,_p2 : presobject);
          constructor init_preop(const s : string;_p1 : presobject);
          procedure setstr(const s:string);
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

    function yylex : integer;
    function act_token : string;
    procedure internalerror(i : integer);

    function strpnew(const s : string) : pchar;

    procedure writetree(p: presobject);


  implementation

    uses
       options,converu;

    const
       newline = #10;


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
     i : integer;
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


    function strpnew(const s : string) : pchar;
      var
        p : pchar;
      begin
         getmem(p,length(s)+1);
         strpcopy(p,s);
         strpnew:=p;
      end;


    constructor tresobject.init_preop(const s : string;_p1 : presobject);
      begin
         typ:=t_preop;
         p:=strpnew(s);
         p1:=_p1;
         p2:=nil;
         p3:=nil;
         next:=nil;
         intname:=false;
      end;

    constructor tresobject.init_bop(const s : string;_p1,_p2 : presobject);
      begin
         typ:=t_bop;
         p:=strpnew(s);
         p1:=_p1;
         p2:=_p2;
         p3:=nil;
         next:=nil;
         intname:=false;
      end;

    constructor tresobject.init_id(const s : string);
      begin
         typ:=t_id;
         p:=strpnew(s);
         p1:=nil;
         p2:=nil;
         p3:=nil;
         next:=nil;
         intname:=false;
      end;

    constructor tresobject.init_intid(const s : string);
      begin
         typ:=t_id;
         p:=strpnew(s);
         p1:=nil;
         p2:=nil;
         p3:=nil;
         next:=nil;
         intname:=true;
      end;

    constructor tresobject.init_two(t : ttyp;_p1,_p2 : presobject);
      begin
         typ:=t;
         p1:=_p1;
         p2:=_p2;
         p3:=nil;
         p:=nil;
         next:=nil;
         intname:=false;
      end;

    constructor tresobject.init_three(t : ttyp;_p1,_p2,_p3 : presobject);
      begin
         typ:=t;
         p1:=_p1;
         p2:=_p2;
         p3:=_p3;
         p:=nil;
         next:=nil;
         intname:=false;
      end;

    constructor tresobject.init_one(t : ttyp;_p1 : presobject);
      begin
         typ:=t;
         p1:=_p1;
         p2:=nil;
         p3:=nil;
         next:=nil;
         p:=nil;
         intname:=false;
      end;

    constructor tresobject.init_no(t : ttyp);
      begin
         typ:=t;
         p:=nil;
         p1:=nil;
         p2:=nil;
         p3:=nil;
         next:=nil;
         intname:=false;
      end;

    procedure tresobject.setstr(const s : string);
      begin
         if assigned(p) then
          strdispose(p);
         p:=strpnew(s);
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
         newres^.intname:=intname;
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


function yylex : Integer;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)

begin
  (* actions: *)
  case yyruleno of
  1:
                        begin
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
                                      if (c=newline) then
                                      begin
                                        writeln(outfile);
                                        unget_char(c);
                                      end;
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
  2:
                        begin
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
  3:
                        return(CSTRING);
  4:
                        return(CSTRING);
  5:
                        if win32headers then
                          return(CSTRING)
                        else
                          return(256);
  6:
                        if win32headers then
                          return(CSTRING)
                        else
                          return(256);
  7:
                        begin
                           while yytext[length(yytext)] in ['L','U','l','u'] do
                             Delete(yytext,length(yytext),1);
                           return(NUMBER);
                        end;
  8:
                          
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
                        end;
  9:
                             
                        begin
                          return(NUMBER);
                        end;
  10:
                        if in_define then
                          return(DEREF)
                        else
                          return(256);
  11:
                        return(MINUS);
  12:
                        return(EQUAL);
  13:
                        return(UNEQUAL);
  14:
                        return(GTE);
  15:
                        return(LTE);
  16:
                        return(_SHR);
  17:
                        return(STICK);
  18:
                        return(_SHL);
  19:
                        return(GT);
  20:
                        return(LT);
  21:
                        return(_OR);
  22:
                        return(_AND);
  23:
                        return(_NOT); (* inverse, but handled as not operation *)
  24:
                        return(_NOT);
  25:
                        return(_SLASH);
  26:
                        return(_PLUS);
  27:
                        return(QUESTIONMARK);
  28:
                        return(COLON);
  29:
                        return(COMMA);
  30:
                        return(LECKKLAMMER);
  31:
                        return(RECKKLAMMER);
  32:
                        begin
                           inc(arglevel);
                           return(LKLAMMER);
                        end;
  33:
                        begin
                           dec(arglevel);
                           return(RKLAMMER);
                        end;
  34:
                        return(STAR);
  35:
                        return(ELLIPSIS);
  36:
                        if in_define then
                          return(POINT)
                        else
                          return(256);
  37:
                        return(_ASSIGN);
  38:
                        return(EXTERN);
  39:
                        if Win32headers then
                          return(STDCALL)
                        else
                          return(ID);
  40:
                        if not Win32headers then
                          return(ID)
                        else
                          return(CDECL);
  41:
                        if not Win32headers then
                          return(ID)
                        else
                          return(PASCAL);
  42:
                        if not Win32headers then
                          return(ID)
                        else
                          return(_PACKED);
  43:
                        if not Win32headers then
                          return(ID)
                        else
                          return(WINAPI);
  44:
                        if not palmpilot then
                          return(ID)
                        else
                          return(SYS_TRAP);
  45:
                        if not Win32headers then
                          return(ID)
                        else
                          return(WINGDIAPI);
  46:
                        if not Win32headers then
                          return(ID)
                        else
                          return(CALLBACK);
  47:
                        if not Win32headers then
                          return(ID)
                        else
                          return(CALLBACK);
  48:
                        return(VOID);
  49:
                        return(VOID);
  50:
                                                      
                        begin
                          if not stripinfo then
                            writeln(outfile,'{ C++ extern C conditionnal removed }');
                        end;
  51:
                                         
                        begin
                          if not stripinfo then
                            writeln(outfile,'{ C++ end of extern C conditionnal removed }');
                        end;
  52:
                        begin
                           writeln(outfile,'{$else}');
                           block_type:=bt_no;
                           flush(outfile);
                        end;
  53:
                        begin
                           writeln(outfile,'{$endif}');
                           block_type:=bt_no;
                           flush(outfile);
                        end;
  54:
                        begin
                           if not stripinfo then
                             write(outfile,'(*** was #elif ****)');
                           write(outfile,'{$else');
                           copy_until_eol;
                           writeln(outfile,'}');
                           block_type:=bt_no;
                           flush(outfile);
                        end;
  55:
                        begin
                           write(outfile,'{$undef');
                           copy_until_eol;
                           writeln(outfile,'}');
                           flush(outfile);
                        end;
  56:
                        begin
                           write(outfile,'{$error');
                           copy_until_eol;
                           writeln(outfile,'}');
                           flush(outfile);
                        end;
  57:
                        begin
                           write(outfile,'{$include');
                           copy_until_eol;
                           writeln(outfile,'}');
                           flush(outfile);
                           block_type:=bt_no;
                        end;
  58:
                        begin
                           write(outfile,'{$if');
                           copy_until_eol;
                           writeln(outfile,'}');
                           flush(outfile);
                           block_type:=bt_no;
                        end;
  59:
                        begin
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
                          until false;
                        end;
  60:
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
  61:
                        begin
                           commentstr:='';
                           in_define:=true;
                           in_space_define:=1;
                           return(DEFINE);
                        end;
  62:
                        return(_CHAR);
  63:
                        return(UNION);
  64:
                        return(ENUM);
  65:
                        return(STRUCT);
  66:
                        return(LGKLAMMER);
  67:
                        return(RGKLAMMER);
  68:
                        return(TYPEDEF);
  69:
                        return(INT);
  70:
                        return(SHORT);
  71:
                        return(LONG);
  72:
                        return(SIGNED);
  73:
                        return(UNSIGNED);
  74:
                        return(REAL);
  75:
                        return(_CONST);
  76:
                        return(_CONST);
  77:
                        return(_FAR);
  78:
                        return(_FAR);
  79:
                        return(_NEAR);
  80:
                        return(_NEAR);
  81:
                        return(_HUGE);
  82:
                        return(_HUGE);
  83:
                        return(_WHILE);
  84:
                        begin
                           if in_space_define=1 then
                             in_space_define:=2;
                           return(ID);
                        end;
  85:
                        return(SEMICOLON);
  86:
                        begin
                           if (arglevel=0) and (in_space_define=2) then
                            begin
                              in_space_define:=0;
                              return(SPACE_DEFINE);
                            end;
                        end;
  87:
                        begin
                           if in_define then
                            begin
                              in_space_define:=0;
                              if cont_line then
                              begin
                                cont_line:=false;
                              end
                              else
                              begin
                                in_define:=false;
                                return(NEW_LINE);
                              end;
                            end;
                       end;
  88:
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
  89:
                       begin
                           writeln('Illegal character in line ',yylineno);
                           writeln('"',yyline,'"');
                           return(256);
                        end;
  end;
end(*yyaction*);

(* DFA table: *)

type YYTRec = record
                cc : set of Char;
                s  : Integer;
              end;

const

yynmarks   = 310;
yynmatches = 310;
yyntrans   = 559;
yynstates  = 312;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  { 1: }
  { 2: }
  25,
  89,
  { 3: }
  89,
  { 4: }
  89,
  { 5: }
  84,
  89,
  { 6: }
  7,
  9,
  89,
  { 7: }
  7,
  9,
  89,
  { 8: }
  11,
  89,
  { 9: }
  37,
  89,
  { 10: }
  24,
  89,
  { 11: }
  19,
  89,
  { 12: }
  20,
  89,
  { 13: }
  89,
  { 14: }
  21,
  89,
  { 15: }
  22,
  89,
  { 16: }
  23,
  89,
  { 17: }
  26,
  89,
  { 18: }
  27,
  89,
  { 19: }
  28,
  89,
  { 20: }
  29,
  89,
  { 21: }
  30,
  89,
  { 22: }
  31,
  89,
  { 23: }
  32,
  89,
  { 24: }
  33,
  89,
  { 25: }
  34,
  89,
  { 26: }
  36,
  89,
  { 27: }
  84,
  89,
  { 28: }
  84,
  89,
  { 29: }
  84,
  89,
  { 30: }
  84,
  89,
  { 31: }
  84,
  89,
  { 32: }
  84,
  89,
  { 33: }
  84,
  89,
  { 34: }
  84,
  89,
  { 35: }
  84,
  89,
  { 36: }
  84,
  89,
  { 37: }
  84,
  89,
  { 38: }
  66,
  89,
  { 39: }
  67,
  89,
  { 40: }
  84,
  89,
  { 41: }
  84,
  89,
  { 42: }
  84,
  89,
  { 43: }
  84,
  89,
  { 44: }
  84,
  89,
  { 45: }
  84,
  89,
  { 46: }
  84,
  89,
  { 47: }
  84,
  89,
  { 48: }
  84,
  89,
  { 49: }
  84,
  89,
  { 50: }
  84,
  89,
  { 51: }
  85,
  89,
  { 52: }
  86,
  89,
  { 53: }
  87,
  { 54: }
  88,
  89,
  { 55: }
  89,
  { 56: }
  1,
  { 57: }
  2,
  { 58: }
  { 59: }
  3,
  { 60: }
  { 61: }
  4,
  { 62: }
  { 63: }
  { 64: }
  84,
  { 65: }
  7,
  9,
  { 66: }
  7,
  { 67: }
  7,
  { 68: }
  { 69: }
  { 70: }
  8,
  { 71: }
  10,
  { 72: }
  12,
  { 73: }
  13,
  { 74: }
  14,
  { 75: }
  16,
  { 76: }
  15,
  { 77: }
  18,
  { 78: }
  17,
  { 79: }
  { 80: }
  { 81: }
  { 82: }
  { 83: }
  { 84: }
  { 85: }
  { 86: }
  { 87: }
  84,
  { 88: }
  84,
  { 89: }
  84,
  { 90: }
  84,
  { 91: }
  84,
  { 92: }
  84,
  { 93: }
  84,
  { 94: }
  84,
  { 95: }
  84,
  { 96: }
  84,
  { 97: }
  84,
  { 98: }
  84,
  { 99: }
  84,
  { 100: }
  84,
  { 101: }
  84,
  { 102: }
  84,
  { 103: }
  84,
  { 104: }
  84,
  { 105: }
  84,
  { 106: }
  84,
  { 107: }
  84,
  { 108: }
  84,
  { 109: }
  84,
  { 110: }
  84,
  { 111: }
  84,
  { 112: }
  84,
  { 113: }
  84,
  { 114: }
  84,
  { 115: }
  84,
  { 116: }
  { 117: }
  5,
  { 118: }
  6,
  { 119: }
  9,
  { 120: }
  { 121: }
  9,
  { 122: }
  8,
  { 123: }
  8,
  { 124: }
  58,
  { 125: }
  { 126: }
  { 127: }
  { 128: }
  { 129: }
  { 130: }
  { 131: }
  { 132: }
  { 133: }
  { 134: }
  35,
  { 135: }
  84,
  { 136: }
  84,
  { 137: }
  84,
  { 138: }
  84,
  { 139: }
  84,
  { 140: }
  84,
  { 141: }
  84,
  { 142: }
  84,
  { 143: }
  84,
  { 144: }
  84,
  { 145: }
  84,
  { 146: }
  84,
  { 147: }
  84,
  { 148: }
  84,
  { 149: }
  84,
  { 150: }
  84,
  { 151: }
  84,
  { 152: }
  84,
  { 153: }
  84,
  { 154: }
  84,
  { 155: }
  84,
  { 156: }
  69,
  84,
  { 157: }
  84,
  { 158: }
  84,
  { 159: }
  78,
  84,
  { 160: }
  77,
  84,
  { 161: }
  84,
  { 162: }
  84,
  { 163: }
  84,
  { 164: }
  84,
  { 165: }
  84,
  { 166: }
  { 167: }
  { 168: }
  58,
  { 169: }
  { 170: }
  { 171: }
  { 172: }
  { 173: }
  { 174: }
  59,
  { 175: }
  { 176: }
  { 177: }
  84,
  { 178: }
  64,
  84,
  { 179: }
  84,
  { 180: }
  84,
  { 181: }
  84,
  { 182: }
  84,
  { 183: }
  84,
  { 184: }
  84,
  { 185: }
  84,
  { 186: }
  84,
  { 187: }
  84,
  { 188: }
  84,
  { 189: }
  48,
  84,
  { 190: }
  49,
  84,
  { 191: }
  62,
  84,
  { 192: }
  84,
  { 193: }
  84,
  { 194: }
  84,
  { 195: }
  84,
  { 196: }
  84,
  { 197: }
  84,
  { 198: }
  84,
  { 199: }
  71,
  84,
  { 200: }
  84,
  { 201: }
  79,
  84,
  { 202: }
  80,
  84,
  { 203: }
  81,
  84,
  { 204: }
  82,
  84,
  { 205: }
  84,
  { 206: }
  { 207: }
  { 208: }
  52,
  { 209: }
  54,
  { 210: }
  { 211: }
  { 212: }
  { 213: }
  { 214: }
  { 215: }
  84,
  { 216: }
  84,
  { 217: }
  84,
  { 218: }
  40,
  84,
  { 219: }
  84,
  { 220: }
  76,
  84,
  { 221: }
  84,
  { 222: }
  84,
  { 223: }
  84,
  { 224: }
  84,
  { 225: }
  84,
  { 226: }
  75,
  84,
  { 227: }
  63,
  84,
  { 228: }
  84,
  { 229: }
  84,
  { 230: }
  70,
  84,
  { 231: }
  84,
  { 232: }
  84,
  { 233: }
  74,
  84,
  { 234: }
  83,
  84,
  { 235: }
  { 236: }
  { 237: }
  53,
  { 238: }
  56,
  { 239: }
  55,
  { 240: }
  { 241: }
  { 242: }
  38,
  84,
  { 243: }
  84,
  { 244: }
  84,
  { 245: }
  84,
  { 246: }
  41,
  84,
  { 247: }
  42,
  84,
  { 248: }
  43,
  84,
  { 249: }
  84,
  { 250: }
  84,
  { 251: }
  84,
  { 252: }
  65,
  84,
  { 253: }
  72,
  84,
  { 254: }
  84,
  { 255: }
  { 256: }
  { 257: }
  60,
  { 258: }
  61,
  { 259: }
  39,
  84,
  { 260: }
  84,
  { 261: }
  84,
  { 262: }
  84,
  { 263: }
  84,
  { 264: }
  84,
  { 265: }
  68,
  84,
  { 266: }
  { 267: }
  57,
  { 268: }
  44,
  84,
  { 269: }
  46,
  84,
  { 270: }
  84,
  { 271: }
  47,
  84,
  { 272: }
  73,
  84,
  { 273: }
  { 274: }
  45,
  84,
  { 275: }
  { 276: }
  { 277: }
  { 278: }
  { 279: }
  { 280: }
  { 281: }
  { 282: }
  { 283: }
  { 284: }
  { 285: }
  { 286: }
  { 287: }
  { 288: }
  { 289: }
  { 290: }
  { 291: }
  { 292: }
  { 293: }
  { 294: }
  { 295: }
  { 296: }
  { 297: }
  { 298: }
  { 299: }
  { 300: }
  51,
  { 301: }
  { 302: }
  { 303: }
  { 304: }
  { 305: }
  { 306: }
  { 307: }
  { 308: }
  { 309: }
  { 310: }
  { 311: }
  50
);

yym : array [1..yynmatches] of Integer = (
{ 0: }
{ 1: }
{ 2: }
  25,
  89,
{ 3: }
  89,
{ 4: }
  89,
{ 5: }
  84,
  89,
{ 6: }
  7,
  9,
  89,
{ 7: }
  7,
  9,
  89,
{ 8: }
  11,
  89,
{ 9: }
  37,
  89,
{ 10: }
  24,
  89,
{ 11: }
  19,
  89,
{ 12: }
  20,
  89,
{ 13: }
  89,
{ 14: }
  21,
  89,
{ 15: }
  22,
  89,
{ 16: }
  23,
  89,
{ 17: }
  26,
  89,
{ 18: }
  27,
  89,
{ 19: }
  28,
  89,
{ 20: }
  29,
  89,
{ 21: }
  30,
  89,
{ 22: }
  31,
  89,
{ 23: }
  32,
  89,
{ 24: }
  33,
  89,
{ 25: }
  34,
  89,
{ 26: }
  36,
  89,
{ 27: }
  84,
  89,
{ 28: }
  84,
  89,
{ 29: }
  84,
  89,
{ 30: }
  84,
  89,
{ 31: }
  84,
  89,
{ 32: }
  84,
  89,
{ 33: }
  84,
  89,
{ 34: }
  84,
  89,
{ 35: }
  84,
  89,
{ 36: }
  84,
  89,
{ 37: }
  84,
  89,
{ 38: }
  66,
  89,
{ 39: }
  67,
  89,
{ 40: }
  84,
  89,
{ 41: }
  84,
  89,
{ 42: }
  84,
  89,
{ 43: }
  84,
  89,
{ 44: }
  84,
  89,
{ 45: }
  84,
  89,
{ 46: }
  84,
  89,
{ 47: }
  84,
  89,
{ 48: }
  84,
  89,
{ 49: }
  84,
  89,
{ 50: }
  84,
  89,
{ 51: }
  85,
  89,
{ 52: }
  86,
  89,
{ 53: }
  87,
{ 54: }
  89,
{ 55: }
  89,
{ 56: }
  1,
{ 57: }
  2,
{ 58: }
{ 59: }
  3,
{ 60: }
{ 61: }
  4,
{ 62: }
{ 63: }
{ 64: }
  84,
{ 65: }
  7,
  9,
{ 66: }
  7,
{ 67: }
  7,
{ 68: }
{ 69: }
{ 70: }
  8,
{ 71: }
  10,
{ 72: }
  12,
{ 73: }
  13,
{ 74: }
  14,
{ 75: }
  16,
{ 76: }
  15,
{ 77: }
  18,
{ 78: }
  17,
{ 79: }
{ 80: }
{ 81: }
{ 82: }
{ 83: }
{ 84: }
{ 85: }
{ 86: }
{ 87: }
  84,
{ 88: }
  84,
{ 89: }
  84,
{ 90: }
  84,
{ 91: }
  84,
{ 92: }
  84,
{ 93: }
  84,
{ 94: }
  84,
{ 95: }
  84,
{ 96: }
  84,
{ 97: }
  84,
{ 98: }
  84,
{ 99: }
  84,
{ 100: }
  84,
{ 101: }
  84,
{ 102: }
  84,
{ 103: }
  84,
{ 104: }
  84,
{ 105: }
  84,
{ 106: }
  84,
{ 107: }
  84,
{ 108: }
  84,
{ 109: }
  84,
{ 110: }
  84,
{ 111: }
  84,
{ 112: }
  84,
{ 113: }
  84,
{ 114: }
  84,
{ 115: }
  84,
{ 116: }
  88,
{ 117: }
  5,
{ 118: }
  6,
{ 119: }
  9,
{ 120: }
{ 121: }
  9,
{ 122: }
  8,
{ 123: }
  8,
{ 124: }
  58,
{ 125: }
{ 126: }
{ 127: }
{ 128: }
{ 129: }
{ 130: }
{ 131: }
{ 132: }
{ 133: }
{ 134: }
  35,
{ 135: }
  84,
{ 136: }
  84,
{ 137: }
  84,
{ 138: }
  84,
{ 139: }
  84,
{ 140: }
  84,
{ 141: }
  84,
{ 142: }
  84,
{ 143: }
  84,
{ 144: }
  84,
{ 145: }
  84,
{ 146: }
  84,
{ 147: }
  84,
{ 148: }
  84,
{ 149: }
  84,
{ 150: }
  84,
{ 151: }
  84,
{ 152: }
  84,
{ 153: }
  84,
{ 154: }
  84,
{ 155: }
  84,
{ 156: }
  69,
  84,
{ 157: }
  84,
{ 158: }
  84,
{ 159: }
  78,
  84,
{ 160: }
  77,
  84,
{ 161: }
  84,
{ 162: }
  84,
{ 163: }
  84,
{ 164: }
  84,
{ 165: }
  84,
{ 166: }
{ 167: }
{ 168: }
  58,
{ 169: }
{ 170: }
{ 171: }
{ 172: }
{ 173: }
{ 174: }
  59,
{ 175: }
{ 176: }
{ 177: }
  84,
{ 178: }
  64,
  84,
{ 179: }
  84,
{ 180: }
  84,
{ 181: }
  84,
{ 182: }
  84,
{ 183: }
  84,
{ 184: }
  84,
{ 185: }
  84,
{ 186: }
  84,
{ 187: }
  84,
{ 188: }
  84,
{ 189: }
  48,
  84,
{ 190: }
  49,
  84,
{ 191: }
  62,
  84,
{ 192: }
  84,
{ 193: }
  84,
{ 194: }
  84,
{ 195: }
  84,
{ 196: }
  84,
{ 197: }
  84,
{ 198: }
  84,
{ 199: }
  71,
  84,
{ 200: }
  84,
{ 201: }
  79,
  84,
{ 202: }
  80,
  84,
{ 203: }
  81,
  84,
{ 204: }
  82,
  84,
{ 205: }
  84,
{ 206: }
{ 207: }
{ 208: }
  52,
{ 209: }
  54,
{ 210: }
{ 211: }
{ 212: }
{ 213: }
{ 214: }
{ 215: }
  84,
{ 216: }
  84,
{ 217: }
  84,
{ 218: }
  40,
  84,
{ 219: }
  84,
{ 220: }
  76,
  84,
{ 221: }
  84,
{ 222: }
  84,
{ 223: }
  84,
{ 224: }
  84,
{ 225: }
  84,
{ 226: }
  75,
  84,
{ 227: }
  63,
  84,
{ 228: }
  84,
{ 229: }
  84,
{ 230: }
  70,
  84,
{ 231: }
  84,
{ 232: }
  84,
{ 233: }
  74,
  84,
{ 234: }
  83,
  84,
{ 235: }
{ 236: }
{ 237: }
  53,
{ 238: }
  56,
{ 239: }
  55,
{ 240: }
{ 241: }
{ 242: }
  38,
  84,
{ 243: }
  84,
{ 244: }
  84,
{ 245: }
  84,
{ 246: }
  41,
  84,
{ 247: }
  42,
  84,
{ 248: }
  43,
  84,
{ 249: }
  84,
{ 250: }
  84,
{ 251: }
  84,
{ 252: }
  65,
  84,
{ 253: }
  72,
  84,
{ 254: }
  84,
{ 255: }
{ 256: }
{ 257: }
  60,
{ 258: }
  61,
{ 259: }
  39,
  84,
{ 260: }
  84,
{ 261: }
  84,
{ 262: }
  84,
{ 263: }
  84,
{ 264: }
  84,
{ 265: }
  68,
  84,
{ 266: }
{ 267: }
  57,
{ 268: }
  44,
  84,
{ 269: }
  46,
  84,
{ 270: }
  84,
{ 271: }
  47,
  84,
{ 272: }
  73,
  84,
{ 273: }
{ 274: }
  45,
  84,
{ 275: }
{ 276: }
{ 277: }
{ 278: }
{ 279: }
{ 280: }
{ 281: }
{ 282: }
{ 283: }
{ 284: }
{ 285: }
{ 286: }
{ 287: }
{ 288: }
{ 289: }
{ 290: }
{ 291: }
{ 292: }
{ 293: }
{ 294: }
{ 295: }
{ 296: }
{ 297: }
{ 298: }
{ 299: }
{ 300: }
  51,
{ 301: }
{ 302: }
{ 303: }
{ 304: }
{ 305: }
{ 306: }
{ 307: }
{ 308: }
{ 309: }
{ 310: }
{ 311: }
  50
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
  ( cc: [ #1..#8,#11,#13..#31,'$','%','@','^','`',#127..#255 ]; s: 55),
  ( cc: [ #9,#12,' ' ]; s: 52),
  ( cc: [ #10 ]; s: 53),
  ( cc: [ '!' ]; s: 10),
  ( cc: [ '"' ]; s: 3),
  ( cc: [ '#' ]; s: 13),
  ( cc: [ '&' ]; s: 15),
  ( cc: [ '''' ]; s: 4),
  ( cc: [ '(' ]; s: 23),
  ( cc: [ ')' ]; s: 24),
  ( cc: [ '*' ]; s: 25),
  ( cc: [ '+' ]; s: 17),
  ( cc: [ ',' ]; s: 20),
  ( cc: [ '-' ]; s: 8),
  ( cc: [ '.' ]; s: 26),
  ( cc: [ '/' ]; s: 2),
  ( cc: [ '0' ]; s: 7),
  ( cc: [ '1'..'9' ]; s: 6),
  ( cc: [ ':' ]; s: 19),
  ( cc: [ ';' ]; s: 51),
  ( cc: [ '<' ]; s: 12),
  ( cc: [ '=' ]; s: 9),
  ( cc: [ '>' ]; s: 11),
  ( cc: [ '?' ]; s: 18),
  ( cc: [ 'A','B','D','G','I'..'K','M','O','Q','R',
            'T','U','X'..'Z','_','a','b','d','g','j','k',
            'm','o'..'r','x'..'z' ]; s: 50),
  ( cc: [ 'C' ]; s: 29),
  ( cc: [ 'E' ]; s: 32),
  ( cc: [ 'F' ]; s: 44),
  ( cc: [ 'H' ]; s: 47),
  ( cc: [ 'L' ]; s: 5),
  ( cc: [ 'N' ]; s: 45),
  ( cc: [ 'P' ]; s: 30),
  ( cc: [ 'S' ]; s: 28),
  ( cc: [ 'V' ]; s: 34),
  ( cc: [ 'W' ]; s: 31),
  ( cc: [ '[' ]; s: 21),
  ( cc: [ '\' ]; s: 54),
  ( cc: [ ']' ]; s: 22),
  ( cc: [ 'c' ]; s: 35),
  ( cc: [ 'e' ]; s: 27),
  ( cc: [ 'f' ]; s: 43),
  ( cc: [ 'h' ]; s: 48),
  ( cc: [ 'i' ]; s: 41),
  ( cc: [ 'l' ]; s: 42),
  ( cc: [ 'n' ]; s: 46),
  ( cc: [ 's' ]; s: 37),
  ( cc: [ 't' ]; s: 40),
  ( cc: [ 'u' ]; s: 36),
  ( cc: [ 'v' ]; s: 33),
  ( cc: [ 'w' ]; s: 49),
  ( cc: [ '{' ]; s: 38),
  ( cc: [ '|' ]; s: 14),
  ( cc: [ '}' ]; s: 39),
  ( cc: [ '~' ]; s: 16),
{ 1: }
  ( cc: [ #1..#8,#11,#13..#31,'$','%','@','^','`',#127..#255 ]; s: 55),
  ( cc: [ #9,#12,' ' ]; s: 52),
  ( cc: [ #10 ]; s: 53),
  ( cc: [ '!' ]; s: 10),
  ( cc: [ '"' ]; s: 3),
  ( cc: [ '#' ]; s: 13),
  ( cc: [ '&' ]; s: 15),
  ( cc: [ '''' ]; s: 4),
  ( cc: [ '(' ]; s: 23),
  ( cc: [ ')' ]; s: 24),
  ( cc: [ '*' ]; s: 25),
  ( cc: [ '+' ]; s: 17),
  ( cc: [ ',' ]; s: 20),
  ( cc: [ '-' ]; s: 8),
  ( cc: [ '.' ]; s: 26),
  ( cc: [ '/' ]; s: 2),
  ( cc: [ '0' ]; s: 7),
  ( cc: [ '1'..'9' ]; s: 6),
  ( cc: [ ':' ]; s: 19),
  ( cc: [ ';' ]; s: 51),
  ( cc: [ '<' ]; s: 12),
  ( cc: [ '=' ]; s: 9),
  ( cc: [ '>' ]; s: 11),
  ( cc: [ '?' ]; s: 18),
  ( cc: [ 'A','B','D','G','I'..'K','M','O','Q','R',
            'T','U','X'..'Z','_','a','b','d','g','j','k',
            'm','o'..'r','x'..'z' ]; s: 50),
  ( cc: [ 'C' ]; s: 29),
  ( cc: [ 'E' ]; s: 32),
  ( cc: [ 'F' ]; s: 44),
  ( cc: [ 'H' ]; s: 47),
  ( cc: [ 'L' ]; s: 5),
  ( cc: [ 'N' ]; s: 45),
  ( cc: [ 'P' ]; s: 30),
  ( cc: [ 'S' ]; s: 28),
  ( cc: [ 'V' ]; s: 34),
  ( cc: [ 'W' ]; s: 31),
  ( cc: [ '[' ]; s: 21),
  ( cc: [ '\' ]; s: 54),
  ( cc: [ ']' ]; s: 22),
  ( cc: [ 'c' ]; s: 35),
  ( cc: [ 'e' ]; s: 27),
  ( cc: [ 'f' ]; s: 43),
  ( cc: [ 'h' ]; s: 48),
  ( cc: [ 'i' ]; s: 41),
  ( cc: [ 'l' ]; s: 42),
  ( cc: [ 'n' ]; s: 46),
  ( cc: [ 's' ]; s: 37),
  ( cc: [ 't' ]; s: 40),
  ( cc: [ 'u' ]; s: 36),
  ( cc: [ 'v' ]; s: 33),
  ( cc: [ 'w' ]; s: 49),
  ( cc: [ '{' ]; s: 38),
  ( cc: [ '|' ]; s: 14),
  ( cc: [ '}' ]; s: 39),
  ( cc: [ '~' ]; s: 16),
{ 2: }
  ( cc: [ '*' ]; s: 56),
  ( cc: [ '/' ]; s: 57),
{ 3: }
  ( cc: [ #1..'!','#'..#255 ]; s: 58),
  ( cc: [ '"' ]; s: 59),
{ 4: }
  ( cc: [ #1..'&','('..#255 ]; s: 60),
  ( cc: [ '''' ]; s: 61),
{ 5: }
  ( cc: [ '"' ]; s: 62),
  ( cc: [ '''' ]; s: 63),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 6: }
  ( cc: [ '.' ]; s: 68),
  ( cc: [ '0'..'9' ]; s: 65),
  ( cc: [ 'E','e' ]; s: 69),
  ( cc: [ 'L','l' ]; s: 67),
  ( cc: [ 'U','u' ]; s: 66),
{ 7: }
  ( cc: [ '.' ]; s: 68),
  ( cc: [ '0'..'9' ]; s: 65),
  ( cc: [ 'E','e' ]; s: 69),
  ( cc: [ 'L','l' ]; s: 67),
  ( cc: [ 'U','u' ]; s: 66),
  ( cc: [ 'x' ]; s: 70),
{ 8: }
  ( cc: [ '>' ]; s: 71),
{ 9: }
  ( cc: [ '=' ]; s: 72),
{ 10: }
  ( cc: [ '=' ]; s: 73),
{ 11: }
  ( cc: [ '=' ]; s: 74),
  ( cc: [ '>' ]; s: 75),
{ 12: }
  ( cc: [ '<' ]; s: 77),
  ( cc: [ '=' ]; s: 76),
{ 13: }
  ( cc: [ #9 ]; s: 80),
  ( cc: [ ' ' ]; s: 83),
  ( cc: [ '#' ]; s: 78),
  ( cc: [ 'd' ]; s: 85),
  ( cc: [ 'e' ]; s: 81),
  ( cc: [ 'i' ]; s: 79),
  ( cc: [ 'p' ]; s: 84),
  ( cc: [ 'u' ]; s: 82),
{ 14: }
{ 15: }
{ 16: }
{ 17: }
{ 18: }
{ 19: }
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
{ 25: }
{ 26: }
  ( cc: [ '.' ]; s: 86),
{ 27: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'w','y','z' ]; s: 64),
  ( cc: [ 'n' ]; s: 88),
  ( cc: [ 'x' ]; s: 87),
{ 28: }
  ( cc: [ '0'..'9','A'..'S','U'..'X','Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'T' ]; s: 89),
  ( cc: [ 'Y' ]; s: 90),
{ 29: }
  ( cc: [ '0'..'9','B','C','E'..'N','P'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'A' ]; s: 92),
  ( cc: [ 'D' ]; s: 91),
  ( cc: [ 'O' ]; s: 93),
{ 30: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'A' ]; s: 94),
{ 31: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'I' ]; s: 95),
{ 32: }
  ( cc: [ '0'..'9','A'..'W','Y','Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'X' ]; s: 96),
{ 33: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 64),
  ( cc: [ 'o' ]; s: 97),
{ 34: }
  ( cc: [ '0'..'9','A'..'N','P'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'O' ]; s: 98),
{ 35: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','i'..'n','p'..'z' ]; s: 64),
  ( cc: [ 'h' ]; s: 99),
  ( cc: [ 'o' ]; s: 100),
{ 36: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 64),
  ( cc: [ 'n' ]; s: 101),
{ 37: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','j'..'s','u'..'z' ]; s: 64),
  ( cc: [ 'h' ]; s: 103),
  ( cc: [ 'i' ]; s: 104),
  ( cc: [ 't' ]; s: 102),
{ 38: }
{ 39: }
{ 40: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'x','z' ]; s: 64),
  ( cc: [ 'y' ]; s: 105),
{ 41: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 64),
  ( cc: [ 'n' ]; s: 106),
{ 42: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 64),
  ( cc: [ 'o' ]; s: 107),
{ 43: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'k','m'..'z' ]; s: 64),
  ( cc: [ 'a' ]; s: 109),
  ( cc: [ 'l' ]; s: 108),
{ 44: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'A' ]; s: 110),
{ 45: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'E' ]; s: 111),
{ 46: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 64),
  ( cc: [ 'e' ]; s: 112),
{ 47: }
  ( cc: [ '0'..'9','A'..'T','V'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'U' ]; s: 113),
{ 48: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 64),
  ( cc: [ 'u' ]; s: 114),
{ 49: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','i'..'z' ]; s: 64),
  ( cc: [ 'h' ]; s: 115),
{ 50: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 51: }
{ 52: }
{ 53: }
{ 54: }
  ( cc: [ #10 ]; s: 116),
{ 55: }
{ 56: }
{ 57: }
{ 58: }
  ( cc: [ #1..'!','#'..#255 ]; s: 58),
  ( cc: [ '"' ]; s: 59),
{ 59: }
{ 60: }
  ( cc: [ #1..'&','('..#255 ]; s: 60),
  ( cc: [ '''' ]; s: 61),
{ 61: }
{ 62: }
  ( cc: [ #1..'!','#'..#255 ]; s: 62),
  ( cc: [ '"' ]; s: 117),
{ 63: }
  ( cc: [ #1..'&','('..#255 ]; s: 63),
  ( cc: [ '''' ]; s: 118),
{ 64: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 65: }
  ( cc: [ '.' ]; s: 68),
  ( cc: [ '0'..'9' ]; s: 65),
  ( cc: [ 'E','e' ]; s: 69),
  ( cc: [ 'L','l' ]; s: 67),
  ( cc: [ 'U','u' ]; s: 66),
{ 66: }
  ( cc: [ 'L','l' ]; s: 67),
{ 67: }
{ 68: }
  ( cc: [ '0'..'9' ]; s: 119),
{ 69: }
  ( cc: [ '+','-' ]; s: 120),
  ( cc: [ '0'..'9' ]; s: 121),
{ 70: }
  ( cc: [ '0'..'9','A'..'F','a'..'f' ]; s: 70),
  ( cc: [ 'L','l' ]; s: 123),
  ( cc: [ 'U','u' ]; s: 122),
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
{ 77: }
{ 78: }
{ 79: }
  ( cc: [ 'f' ]; s: 124),
  ( cc: [ 'n' ]; s: 125),
{ 80: }
  ( cc: [ #9,' ' ]; s: 80),
  ( cc: [ 'd' ]; s: 85),
  ( cc: [ 'e' ]; s: 81),
  ( cc: [ 'i' ]; s: 126),
  ( cc: [ 'p' ]; s: 84),
  ( cc: [ 'u' ]; s: 82),
{ 81: }
  ( cc: [ 'l' ]; s: 127),
  ( cc: [ 'n' ]; s: 128),
  ( cc: [ 'r' ]; s: 129),
{ 82: }
  ( cc: [ 'n' ]; s: 130),
{ 83: }
  ( cc: [ #9,' ' ]; s: 80),
  ( cc: [ '0'..'9' ]; s: 131),
  ( cc: [ 'd' ]; s: 85),
  ( cc: [ 'e' ]; s: 81),
  ( cc: [ 'i' ]; s: 126),
  ( cc: [ 'p' ]; s: 84),
  ( cc: [ 'u' ]; s: 82),
{ 84: }
  ( cc: [ 'r' ]; s: 132),
{ 85: }
  ( cc: [ 'e' ]; s: 133),
{ 86: }
  ( cc: [ '.' ]; s: 134),
{ 87: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 64),
  ( cc: [ 't' ]; s: 135),
{ 88: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 64),
  ( cc: [ 'u' ]; s: 136),
{ 89: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'D' ]; s: 137),
{ 90: }
  ( cc: [ '0'..'9','A'..'R','T'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'S' ]; s: 138),
{ 91: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'E' ]; s: 139),
{ 92: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'L' ]; s: 140),
{ 93: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'N' ]; s: 141),
{ 94: }
  ( cc: [ '0'..'9','A','B','D'..'R','T'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'C' ]; s: 143),
  ( cc: [ 'S' ]; s: 142),
{ 95: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'N' ]; s: 144),
{ 96: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'P' ]; s: 145),
{ 97: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'z' ]; s: 64),
  ( cc: [ 'i' ]; s: 146),
{ 98: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'I' ]; s: 147),
{ 99: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 64),
  ( cc: [ 'a' ]; s: 148),
{ 100: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 64),
  ( cc: [ 'n' ]; s: 149),
{ 101: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'r','t'..'z' ]; s: 64),
  ( cc: [ 'i' ]; s: 150),
  ( cc: [ 's' ]; s: 151),
{ 102: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 64),
  ( cc: [ 'r' ]; s: 152),
{ 103: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 64),
  ( cc: [ 'o' ]; s: 153),
{ 104: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 64),
  ( cc: [ 'g' ]; s: 154),
{ 105: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'o','q'..'z' ]; s: 64),
  ( cc: [ 'p' ]; s: 155),
{ 106: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 64),
  ( cc: [ 't' ]; s: 156),
{ 107: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 64),
  ( cc: [ 'n' ]; s: 157),
{ 108: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 64),
  ( cc: [ 'o' ]; s: 158),
{ 109: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 64),
  ( cc: [ 'r' ]; s: 159),
{ 110: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'R' ]; s: 160),
{ 111: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'A' ]; s: 161),
{ 112: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 64),
  ( cc: [ 'a' ]; s: 162),
{ 113: }
  ( cc: [ '0'..'9','A'..'F','H'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'G' ]; s: 163),
{ 114: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 64),
  ( cc: [ 'g' ]; s: 164),
{ 115: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'z' ]; s: 64),
  ( cc: [ 'i' ]; s: 165),
{ 116: }
{ 117: }
{ 118: }
{ 119: }
  ( cc: [ '0'..'9' ]; s: 119),
  ( cc: [ 'E','e' ]; s: 69),
{ 120: }
  ( cc: [ '0'..'9' ]; s: 121),
{ 121: }
  ( cc: [ '0'..'9' ]; s: 121),
{ 122: }
  ( cc: [ 'L','l' ]; s: 123),
{ 123: }
{ 124: }
  ( cc: [ 'd' ]; s: 166),
{ 125: }
  ( cc: [ 'c' ]; s: 167),
{ 126: }
  ( cc: [ 'f' ]; s: 168),
  ( cc: [ 'n' ]; s: 125),
{ 127: }
  ( cc: [ 'i' ]; s: 170),
  ( cc: [ 's' ]; s: 169),
{ 128: }
  ( cc: [ 'd' ]; s: 171),
{ 129: }
  ( cc: [ 'r' ]; s: 172),
{ 130: }
  ( cc: [ 'd' ]; s: 173),
{ 131: }
  ( cc: [ ' ' ]; s: 174),
  ( cc: [ '0'..'9' ]; s: 131),
{ 132: }
  ( cc: [ 'a' ]; s: 175),
{ 133: }
  ( cc: [ 'f' ]; s: 176),
{ 134: }
{ 135: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 64),
  ( cc: [ 'e' ]; s: 177),
{ 136: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'l','n'..'z' ]; s: 64),
  ( cc: [ 'm' ]; s: 178),
{ 137: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'C' ]; s: 179),
{ 138: }
  ( cc: [ '0'..'9','A'..'Z','a'..'z' ]; s: 64),
  ( cc: [ '_' ]; s: 180),
{ 139: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'C' ]; s: 181),
{ 140: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'L' ]; s: 182),
{ 141: }
  ( cc: [ '0'..'9','A'..'R','T'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'S' ]; s: 183),
{ 142: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'C' ]; s: 184),
{ 143: }
  ( cc: [ '0'..'9','A'..'J','L'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'K' ]; s: 185),
{ 144: }
  ( cc: [ '0'..'9','B'..'F','H'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'A' ]; s: 186),
  ( cc: [ 'G' ]; s: 187),
{ 145: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'E' ]; s: 188),
{ 146: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 64),
  ( cc: [ 'd' ]; s: 189),
{ 147: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'D' ]; s: 190),
{ 148: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 64),
  ( cc: [ 'r' ]; s: 191),
{ 149: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'r','t'..'z' ]; s: 64),
  ( cc: [ 's' ]; s: 192),
{ 150: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 64),
  ( cc: [ 'o' ]; s: 193),
{ 151: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'z' ]; s: 64),
  ( cc: [ 'i' ]; s: 194),
{ 152: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 64),
  ( cc: [ 'u' ]; s: 195),
{ 153: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 64),
  ( cc: [ 'r' ]; s: 196),
{ 154: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 64),
  ( cc: [ 'n' ]; s: 197),
{ 155: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 64),
  ( cc: [ 'e' ]; s: 198),
{ 156: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 157: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 64),
  ( cc: [ 'g' ]; s: 199),
{ 158: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 64),
  ( cc: [ 'a' ]; s: 200),
{ 159: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 160: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 161: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'R' ]; s: 201),
{ 162: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 64),
  ( cc: [ 'r' ]; s: 202),
{ 163: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'E' ]; s: 203),
{ 164: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 64),
  ( cc: [ 'e' ]; s: 204),
{ 165: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'k','m'..'z' ]; s: 64),
  ( cc: [ 'l' ]; s: 205),
{ 166: }
  ( cc: [ 'e' ]; s: 206),
{ 167: }
  ( cc: [ 'l' ]; s: 207),
{ 168: }
{ 169: }
  ( cc: [ 'e' ]; s: 208),
{ 170: }
  ( cc: [ 'f' ]; s: 209),
{ 171: }
  ( cc: [ 'i' ]; s: 210),
{ 172: }
  ( cc: [ 'o' ]; s: 211),
{ 173: }
  ( cc: [ 'e' ]; s: 212),
{ 174: }
{ 175: }
  ( cc: [ 'g' ]; s: 213),
{ 176: }
  ( cc: [ 'i' ]; s: 214),
{ 177: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 64),
  ( cc: [ 'r' ]; s: 215),
{ 178: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 179: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'A' ]; s: 216),
{ 180: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'T' ]; s: 217),
{ 181: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'L' ]; s: 218),
{ 182: }
  ( cc: [ '0'..'9','A','C'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'B' ]; s: 219),
{ 183: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'T' ]; s: 220),
{ 184: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'A' ]; s: 221),
{ 185: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'E' ]; s: 222),
{ 186: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'P' ]; s: 223),
{ 187: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'D' ]; s: 224),
{ 188: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'N' ]; s: 225),
{ 189: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 190: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 191: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 192: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 64),
  ( cc: [ 't' ]; s: 226),
{ 193: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 64),
  ( cc: [ 'n' ]; s: 227),
{ 194: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 64),
  ( cc: [ 'g' ]; s: 228),
{ 195: }
  ( cc: [ '0'..'9','A'..'Z','_','a','b','d'..'z' ]; s: 64),
  ( cc: [ 'c' ]; s: 229),
{ 196: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 64),
  ( cc: [ 't' ]; s: 230),
{ 197: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 64),
  ( cc: [ 'e' ]; s: 231),
{ 198: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 64),
  ( cc: [ 'd' ]; s: 232),
{ 199: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 200: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 64),
  ( cc: [ 't' ]; s: 233),
{ 201: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 202: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 203: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 204: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 205: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 64),
  ( cc: [ 'e' ]; s: 234),
{ 206: }
  ( cc: [ 'f' ]; s: 235),
{ 207: }
  ( cc: [ 'u' ]; s: 236),
{ 208: }
{ 209: }
{ 210: }
  ( cc: [ 'f' ]; s: 237),
{ 211: }
  ( cc: [ 'r' ]; s: 238),
{ 212: }
  ( cc: [ 'f' ]; s: 239),
{ 213: }
  ( cc: [ 'm' ]; s: 240),
{ 214: }
  ( cc: [ 'n' ]; s: 241),
{ 215: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 64),
  ( cc: [ 'n' ]; s: 242),
{ 216: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'L' ]; s: 243),
{ 217: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'R' ]; s: 244),
{ 218: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 219: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'A' ]; s: 245),
{ 220: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 221: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'L' ]; s: 246),
{ 222: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'D' ]; s: 247),
{ 223: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'I' ]; s: 248),
{ 224: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'I' ]; s: 249),
{ 225: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'T' ]; s: 250),
{ 226: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 227: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 228: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 64),
  ( cc: [ 'n' ]; s: 251),
{ 229: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 64),
  ( cc: [ 't' ]; s: 252),
{ 230: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 231: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 64),
  ( cc: [ 'd' ]; s: 253),
{ 232: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 64),
  ( cc: [ 'e' ]; s: 254),
{ 233: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 234: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 235: }
  ( cc: [ ' ' ]; s: 255),
{ 236: }
  ( cc: [ 'd' ]; s: 256),
{ 237: }
{ 238: }
{ 239: }
{ 240: }
  ( cc: [ 'a' ]; s: 257),
{ 241: }
  ( cc: [ 'e' ]; s: 258),
{ 242: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 243: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'L' ]; s: 259),
{ 244: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'A' ]; s: 260),
{ 245: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'C' ]; s: 261),
{ 246: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 247: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 248: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 249: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'A' ]; s: 262),
{ 250: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'R' ]; s: 263),
{ 251: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 64),
  ( cc: [ 'e' ]; s: 264),
{ 252: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 253: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 254: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'e','g'..'z' ]; s: 64),
  ( cc: [ 'f' ]; s: 265),
{ 255: }
  ( cc: [ '_' ]; s: 266),
{ 256: }
  ( cc: [ 'e' ]; s: 267),
{ 257: }
{ 258: }
{ 259: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 260: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'P' ]; s: 268),
{ 261: }
  ( cc: [ '0'..'9','A'..'J','L'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'K' ]; s: 269),
{ 262: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'P' ]; s: 270),
{ 263: }
  ( cc: [ '0'..'9','A'..'X','Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'Y' ]; s: 271),
{ 264: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 64),
  ( cc: [ 'd' ]; s: 272),
{ 265: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 266: }
  ( cc: [ '_' ]; s: 273),
{ 267: }
{ 268: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 269: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 270: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 64),
  ( cc: [ 'I' ]; s: 274),
{ 271: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 272: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 273: }
  ( cc: [ 'c' ]; s: 275),
{ 274: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 64),
{ 275: }
  ( cc: [ 'p' ]; s: 276),
{ 276: }
  ( cc: [ 'l' ]; s: 277),
{ 277: }
  ( cc: [ 'u' ]; s: 278),
{ 278: }
  ( cc: [ 's' ]; s: 279),
{ 279: }
  ( cc: [ 'p' ]; s: 280),
{ 280: }
  ( cc: [ 'l' ]; s: 281),
{ 281: }
  ( cc: [ 'u' ]; s: 282),
{ 282: }
  ( cc: [ 's' ]; s: 283),
{ 283: }
  ( cc: [ #9,' ' ]; s: 283),
  ( cc: [ #10 ]; s: 284),
{ 284: }
  ( cc: [ 'e' ]; s: 285),
  ( cc: [ '}' ]; s: 286),
{ 285: }
  ( cc: [ 'x' ]; s: 287),
{ 286: }
  ( cc: [ #10 ]; s: 288),
{ 287: }
  ( cc: [ 't' ]; s: 289),
{ 288: }
  ( cc: [ '#' ]; s: 290),
{ 289: }
  ( cc: [ 'e' ]; s: 291),
{ 290: }
  ( cc: [ 'e' ]; s: 292),
{ 291: }
  ( cc: [ 'r' ]; s: 293),
{ 292: }
  ( cc: [ 'n' ]; s: 294),
{ 293: }
  ( cc: [ 'n' ]; s: 295),
{ 294: }
  ( cc: [ 'd' ]; s: 296),
{ 295: }
  ( cc: [ ' ' ]; s: 297),
{ 296: }
  ( cc: [ 'i' ]; s: 298),
{ 297: }
  ( cc: [ '"' ]; s: 299),
{ 298: }
  ( cc: [ 'f' ]; s: 300),
{ 299: }
  ( cc: [ 'C' ]; s: 301),
{ 300: }
{ 301: }
  ( cc: [ '"' ]; s: 302),
{ 302: }
  ( cc: [ ' ' ]; s: 303),
{ 303: }
  ( cc: [ '{' ]; s: 304),
{ 304: }
  ( cc: [ #10 ]; s: 305),
{ 305: }
  ( cc: [ '#' ]; s: 306),
{ 306: }
  ( cc: [ 'e' ]; s: 307),
{ 307: }
  ( cc: [ 'n' ]; s: 308),
{ 308: }
  ( cc: [ 'd' ]; s: 309),
{ 309: }
  ( cc: [ 'i' ]; s: 310),
{ 310: }
  ( cc: [ 'f' ]; s: 311)
{ 311: }
);

yykl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 3,
{ 4: } 4,
{ 5: } 5,
{ 6: } 7,
{ 7: } 10,
{ 8: } 13,
{ 9: } 15,
{ 10: } 17,
{ 11: } 19,
{ 12: } 21,
{ 13: } 23,
{ 14: } 24,
{ 15: } 26,
{ 16: } 28,
{ 17: } 30,
{ 18: } 32,
{ 19: } 34,
{ 20: } 36,
{ 21: } 38,
{ 22: } 40,
{ 23: } 42,
{ 24: } 44,
{ 25: } 46,
{ 26: } 48,
{ 27: } 50,
{ 28: } 52,
{ 29: } 54,
{ 30: } 56,
{ 31: } 58,
{ 32: } 60,
{ 33: } 62,
{ 34: } 64,
{ 35: } 66,
{ 36: } 68,
{ 37: } 70,
{ 38: } 72,
{ 39: } 74,
{ 40: } 76,
{ 41: } 78,
{ 42: } 80,
{ 43: } 82,
{ 44: } 84,
{ 45: } 86,
{ 46: } 88,
{ 47: } 90,
{ 48: } 92,
{ 49: } 94,
{ 50: } 96,
{ 51: } 98,
{ 52: } 100,
{ 53: } 102,
{ 54: } 103,
{ 55: } 105,
{ 56: } 106,
{ 57: } 107,
{ 58: } 108,
{ 59: } 108,
{ 60: } 109,
{ 61: } 109,
{ 62: } 110,
{ 63: } 110,
{ 64: } 110,
{ 65: } 111,
{ 66: } 113,
{ 67: } 114,
{ 68: } 115,
{ 69: } 115,
{ 70: } 115,
{ 71: } 116,
{ 72: } 117,
{ 73: } 118,
{ 74: } 119,
{ 75: } 120,
{ 76: } 121,
{ 77: } 122,
{ 78: } 123,
{ 79: } 124,
{ 80: } 124,
{ 81: } 124,
{ 82: } 124,
{ 83: } 124,
{ 84: } 124,
{ 85: } 124,
{ 86: } 124,
{ 87: } 124,
{ 88: } 125,
{ 89: } 126,
{ 90: } 127,
{ 91: } 128,
{ 92: } 129,
{ 93: } 130,
{ 94: } 131,
{ 95: } 132,
{ 96: } 133,
{ 97: } 134,
{ 98: } 135,
{ 99: } 136,
{ 100: } 137,
{ 101: } 138,
{ 102: } 139,
{ 103: } 140,
{ 104: } 141,
{ 105: } 142,
{ 106: } 143,
{ 107: } 144,
{ 108: } 145,
{ 109: } 146,
{ 110: } 147,
{ 111: } 148,
{ 112: } 149,
{ 113: } 150,
{ 114: } 151,
{ 115: } 152,
{ 116: } 153,
{ 117: } 153,
{ 118: } 154,
{ 119: } 155,
{ 120: } 156,
{ 121: } 156,
{ 122: } 157,
{ 123: } 158,
{ 124: } 159,
{ 125: } 160,
{ 126: } 160,
{ 127: } 160,
{ 128: } 160,
{ 129: } 160,
{ 130: } 160,
{ 131: } 160,
{ 132: } 160,
{ 133: } 160,
{ 134: } 160,
{ 135: } 161,
{ 136: } 162,
{ 137: } 163,
{ 138: } 164,
{ 139: } 165,
{ 140: } 166,
{ 141: } 167,
{ 142: } 168,
{ 143: } 169,
{ 144: } 170,
{ 145: } 171,
{ 146: } 172,
{ 147: } 173,
{ 148: } 174,
{ 149: } 175,
{ 150: } 176,
{ 151: } 177,
{ 152: } 178,
{ 153: } 179,
{ 154: } 180,
{ 155: } 181,
{ 156: } 182,
{ 157: } 184,
{ 158: } 185,
{ 159: } 186,
{ 160: } 188,
{ 161: } 190,
{ 162: } 191,
{ 163: } 192,
{ 164: } 193,
{ 165: } 194,
{ 166: } 195,
{ 167: } 195,
{ 168: } 195,
{ 169: } 196,
{ 170: } 196,
{ 171: } 196,
{ 172: } 196,
{ 173: } 196,
{ 174: } 196,
{ 175: } 197,
{ 176: } 197,
{ 177: } 197,
{ 178: } 198,
{ 179: } 200,
{ 180: } 201,
{ 181: } 202,
{ 182: } 203,
{ 183: } 204,
{ 184: } 205,
{ 185: } 206,
{ 186: } 207,
{ 187: } 208,
{ 188: } 209,
{ 189: } 210,
{ 190: } 212,
{ 191: } 214,
{ 192: } 216,
{ 193: } 217,
{ 194: } 218,
{ 195: } 219,
{ 196: } 220,
{ 197: } 221,
{ 198: } 222,
{ 199: } 223,
{ 200: } 225,
{ 201: } 226,
{ 202: } 228,
{ 203: } 230,
{ 204: } 232,
{ 205: } 234,
{ 206: } 235,
{ 207: } 235,
{ 208: } 235,
{ 209: } 236,
{ 210: } 237,
{ 211: } 237,
{ 212: } 237,
{ 213: } 237,
{ 214: } 237,
{ 215: } 237,
{ 216: } 238,
{ 217: } 239,
{ 218: } 240,
{ 219: } 242,
{ 220: } 243,
{ 221: } 245,
{ 222: } 246,
{ 223: } 247,
{ 224: } 248,
{ 225: } 249,
{ 226: } 250,
{ 227: } 252,
{ 228: } 254,
{ 229: } 255,
{ 230: } 256,
{ 231: } 258,
{ 232: } 259,
{ 233: } 260,
{ 234: } 262,
{ 235: } 264,
{ 236: } 264,
{ 237: } 264,
{ 238: } 265,
{ 239: } 266,
{ 240: } 267,
{ 241: } 267,
{ 242: } 267,
{ 243: } 269,
{ 244: } 270,
{ 245: } 271,
{ 246: } 272,
{ 247: } 274,
{ 248: } 276,
{ 249: } 278,
{ 250: } 279,
{ 251: } 280,
{ 252: } 281,
{ 253: } 283,
{ 254: } 285,
{ 255: } 286,
{ 256: } 286,
{ 257: } 286,
{ 258: } 287,
{ 259: } 288,
{ 260: } 290,
{ 261: } 291,
{ 262: } 292,
{ 263: } 293,
{ 264: } 294,
{ 265: } 295,
{ 266: } 297,
{ 267: } 297,
{ 268: } 298,
{ 269: } 300,
{ 270: } 302,
{ 271: } 303,
{ 272: } 305,
{ 273: } 307,
{ 274: } 307,
{ 275: } 309,
{ 276: } 309,
{ 277: } 309,
{ 278: } 309,
{ 279: } 309,
{ 280: } 309,
{ 281: } 309,
{ 282: } 309,
{ 283: } 309,
{ 284: } 309,
{ 285: } 309,
{ 286: } 309,
{ 287: } 309,
{ 288: } 309,
{ 289: } 309,
{ 290: } 309,
{ 291: } 309,
{ 292: } 309,
{ 293: } 309,
{ 294: } 309,
{ 295: } 309,
{ 296: } 309,
{ 297: } 309,
{ 298: } 309,
{ 299: } 309,
{ 300: } 309,
{ 301: } 310,
{ 302: } 310,
{ 303: } 310,
{ 304: } 310,
{ 305: } 310,
{ 306: } 310,
{ 307: } 310,
{ 308: } 310,
{ 309: } 310,
{ 310: } 310,
{ 311: } 310
);

yykh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 2,
{ 3: } 3,
{ 4: } 4,
{ 5: } 6,
{ 6: } 9,
{ 7: } 12,
{ 8: } 14,
{ 9: } 16,
{ 10: } 18,
{ 11: } 20,
{ 12: } 22,
{ 13: } 23,
{ 14: } 25,
{ 15: } 27,
{ 16: } 29,
{ 17: } 31,
{ 18: } 33,
{ 19: } 35,
{ 20: } 37,
{ 21: } 39,
{ 22: } 41,
{ 23: } 43,
{ 24: } 45,
{ 25: } 47,
{ 26: } 49,
{ 27: } 51,
{ 28: } 53,
{ 29: } 55,
{ 30: } 57,
{ 31: } 59,
{ 32: } 61,
{ 33: } 63,
{ 34: } 65,
{ 35: } 67,
{ 36: } 69,
{ 37: } 71,
{ 38: } 73,
{ 39: } 75,
{ 40: } 77,
{ 41: } 79,
{ 42: } 81,
{ 43: } 83,
{ 44: } 85,
{ 45: } 87,
{ 46: } 89,
{ 47: } 91,
{ 48: } 93,
{ 49: } 95,
{ 50: } 97,
{ 51: } 99,
{ 52: } 101,
{ 53: } 102,
{ 54: } 104,
{ 55: } 105,
{ 56: } 106,
{ 57: } 107,
{ 58: } 107,
{ 59: } 108,
{ 60: } 108,
{ 61: } 109,
{ 62: } 109,
{ 63: } 109,
{ 64: } 110,
{ 65: } 112,
{ 66: } 113,
{ 67: } 114,
{ 68: } 114,
{ 69: } 114,
{ 70: } 115,
{ 71: } 116,
{ 72: } 117,
{ 73: } 118,
{ 74: } 119,
{ 75: } 120,
{ 76: } 121,
{ 77: } 122,
{ 78: } 123,
{ 79: } 123,
{ 80: } 123,
{ 81: } 123,
{ 82: } 123,
{ 83: } 123,
{ 84: } 123,
{ 85: } 123,
{ 86: } 123,
{ 87: } 124,
{ 88: } 125,
{ 89: } 126,
{ 90: } 127,
{ 91: } 128,
{ 92: } 129,
{ 93: } 130,
{ 94: } 131,
{ 95: } 132,
{ 96: } 133,
{ 97: } 134,
{ 98: } 135,
{ 99: } 136,
{ 100: } 137,
{ 101: } 138,
{ 102: } 139,
{ 103: } 140,
{ 104: } 141,
{ 105: } 142,
{ 106: } 143,
{ 107: } 144,
{ 108: } 145,
{ 109: } 146,
{ 110: } 147,
{ 111: } 148,
{ 112: } 149,
{ 113: } 150,
{ 114: } 151,
{ 115: } 152,
{ 116: } 152,
{ 117: } 153,
{ 118: } 154,
{ 119: } 155,
{ 120: } 155,
{ 121: } 156,
{ 122: } 157,
{ 123: } 158,
{ 124: } 159,
{ 125: } 159,
{ 126: } 159,
{ 127: } 159,
{ 128: } 159,
{ 129: } 159,
{ 130: } 159,
{ 131: } 159,
{ 132: } 159,
{ 133: } 159,
{ 134: } 160,
{ 135: } 161,
{ 136: } 162,
{ 137: } 163,
{ 138: } 164,
{ 139: } 165,
{ 140: } 166,
{ 141: } 167,
{ 142: } 168,
{ 143: } 169,
{ 144: } 170,
{ 145: } 171,
{ 146: } 172,
{ 147: } 173,
{ 148: } 174,
{ 149: } 175,
{ 150: } 176,
{ 151: } 177,
{ 152: } 178,
{ 153: } 179,
{ 154: } 180,
{ 155: } 181,
{ 156: } 183,
{ 157: } 184,
{ 158: } 185,
{ 159: } 187,
{ 160: } 189,
{ 161: } 190,
{ 162: } 191,
{ 163: } 192,
{ 164: } 193,
{ 165: } 194,
{ 166: } 194,
{ 167: } 194,
{ 168: } 195,
{ 169: } 195,
{ 170: } 195,
{ 171: } 195,
{ 172: } 195,
{ 173: } 195,
{ 174: } 196,
{ 175: } 196,
{ 176: } 196,
{ 177: } 197,
{ 178: } 199,
{ 179: } 200,
{ 180: } 201,
{ 181: } 202,
{ 182: } 203,
{ 183: } 204,
{ 184: } 205,
{ 185: } 206,
{ 186: } 207,
{ 187: } 208,
{ 188: } 209,
{ 189: } 211,
{ 190: } 213,
{ 191: } 215,
{ 192: } 216,
{ 193: } 217,
{ 194: } 218,
{ 195: } 219,
{ 196: } 220,
{ 197: } 221,
{ 198: } 222,
{ 199: } 224,
{ 200: } 225,
{ 201: } 227,
{ 202: } 229,
{ 203: } 231,
{ 204: } 233,
{ 205: } 234,
{ 206: } 234,
{ 207: } 234,
{ 208: } 235,
{ 209: } 236,
{ 210: } 236,
{ 211: } 236,
{ 212: } 236,
{ 213: } 236,
{ 214: } 236,
{ 215: } 237,
{ 216: } 238,
{ 217: } 239,
{ 218: } 241,
{ 219: } 242,
{ 220: } 244,
{ 221: } 245,
{ 222: } 246,
{ 223: } 247,
{ 224: } 248,
{ 225: } 249,
{ 226: } 251,
{ 227: } 253,
{ 228: } 254,
{ 229: } 255,
{ 230: } 257,
{ 231: } 258,
{ 232: } 259,
{ 233: } 261,
{ 234: } 263,
{ 235: } 263,
{ 236: } 263,
{ 237: } 264,
{ 238: } 265,
{ 239: } 266,
{ 240: } 266,
{ 241: } 266,
{ 242: } 268,
{ 243: } 269,
{ 244: } 270,
{ 245: } 271,
{ 246: } 273,
{ 247: } 275,
{ 248: } 277,
{ 249: } 278,
{ 250: } 279,
{ 251: } 280,
{ 252: } 282,
{ 253: } 284,
{ 254: } 285,
{ 255: } 285,
{ 256: } 285,
{ 257: } 286,
{ 258: } 287,
{ 259: } 289,
{ 260: } 290,
{ 261: } 291,
{ 262: } 292,
{ 263: } 293,
{ 264: } 294,
{ 265: } 296,
{ 266: } 296,
{ 267: } 297,
{ 268: } 299,
{ 269: } 301,
{ 270: } 302,
{ 271: } 304,
{ 272: } 306,
{ 273: } 306,
{ 274: } 308,
{ 275: } 308,
{ 276: } 308,
{ 277: } 308,
{ 278: } 308,
{ 279: } 308,
{ 280: } 308,
{ 281: } 308,
{ 282: } 308,
{ 283: } 308,
{ 284: } 308,
{ 285: } 308,
{ 286: } 308,
{ 287: } 308,
{ 288: } 308,
{ 289: } 308,
{ 290: } 308,
{ 291: } 308,
{ 292: } 308,
{ 293: } 308,
{ 294: } 308,
{ 295: } 308,
{ 296: } 308,
{ 297: } 308,
{ 298: } 308,
{ 299: } 308,
{ 300: } 309,
{ 301: } 309,
{ 302: } 309,
{ 303: } 309,
{ 304: } 309,
{ 305: } 309,
{ 306: } 309,
{ 307: } 309,
{ 308: } 309,
{ 309: } 309,
{ 310: } 309,
{ 311: } 310
);

yyml : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 3,
{ 4: } 4,
{ 5: } 5,
{ 6: } 7,
{ 7: } 10,
{ 8: } 13,
{ 9: } 15,
{ 10: } 17,
{ 11: } 19,
{ 12: } 21,
{ 13: } 23,
{ 14: } 24,
{ 15: } 26,
{ 16: } 28,
{ 17: } 30,
{ 18: } 32,
{ 19: } 34,
{ 20: } 36,
{ 21: } 38,
{ 22: } 40,
{ 23: } 42,
{ 24: } 44,
{ 25: } 46,
{ 26: } 48,
{ 27: } 50,
{ 28: } 52,
{ 29: } 54,
{ 30: } 56,
{ 31: } 58,
{ 32: } 60,
{ 33: } 62,
{ 34: } 64,
{ 35: } 66,
{ 36: } 68,
{ 37: } 70,
{ 38: } 72,
{ 39: } 74,
{ 40: } 76,
{ 41: } 78,
{ 42: } 80,
{ 43: } 82,
{ 44: } 84,
{ 45: } 86,
{ 46: } 88,
{ 47: } 90,
{ 48: } 92,
{ 49: } 94,
{ 50: } 96,
{ 51: } 98,
{ 52: } 100,
{ 53: } 102,
{ 54: } 103,
{ 55: } 104,
{ 56: } 105,
{ 57: } 106,
{ 58: } 107,
{ 59: } 107,
{ 60: } 108,
{ 61: } 108,
{ 62: } 109,
{ 63: } 109,
{ 64: } 109,
{ 65: } 110,
{ 66: } 112,
{ 67: } 113,
{ 68: } 114,
{ 69: } 114,
{ 70: } 114,
{ 71: } 115,
{ 72: } 116,
{ 73: } 117,
{ 74: } 118,
{ 75: } 119,
{ 76: } 120,
{ 77: } 121,
{ 78: } 122,
{ 79: } 123,
{ 80: } 123,
{ 81: } 123,
{ 82: } 123,
{ 83: } 123,
{ 84: } 123,
{ 85: } 123,
{ 86: } 123,
{ 87: } 123,
{ 88: } 124,
{ 89: } 125,
{ 90: } 126,
{ 91: } 127,
{ 92: } 128,
{ 93: } 129,
{ 94: } 130,
{ 95: } 131,
{ 96: } 132,
{ 97: } 133,
{ 98: } 134,
{ 99: } 135,
{ 100: } 136,
{ 101: } 137,
{ 102: } 138,
{ 103: } 139,
{ 104: } 140,
{ 105: } 141,
{ 106: } 142,
{ 107: } 143,
{ 108: } 144,
{ 109: } 145,
{ 110: } 146,
{ 111: } 147,
{ 112: } 148,
{ 113: } 149,
{ 114: } 150,
{ 115: } 151,
{ 116: } 152,
{ 117: } 153,
{ 118: } 154,
{ 119: } 155,
{ 120: } 156,
{ 121: } 156,
{ 122: } 157,
{ 123: } 158,
{ 124: } 159,
{ 125: } 160,
{ 126: } 160,
{ 127: } 160,
{ 128: } 160,
{ 129: } 160,
{ 130: } 160,
{ 131: } 160,
{ 132: } 160,
{ 133: } 160,
{ 134: } 160,
{ 135: } 161,
{ 136: } 162,
{ 137: } 163,
{ 138: } 164,
{ 139: } 165,
{ 140: } 166,
{ 141: } 167,
{ 142: } 168,
{ 143: } 169,
{ 144: } 170,
{ 145: } 171,
{ 146: } 172,
{ 147: } 173,
{ 148: } 174,
{ 149: } 175,
{ 150: } 176,
{ 151: } 177,
{ 152: } 178,
{ 153: } 179,
{ 154: } 180,
{ 155: } 181,
{ 156: } 182,
{ 157: } 184,
{ 158: } 185,
{ 159: } 186,
{ 160: } 188,
{ 161: } 190,
{ 162: } 191,
{ 163: } 192,
{ 164: } 193,
{ 165: } 194,
{ 166: } 195,
{ 167: } 195,
{ 168: } 195,
{ 169: } 196,
{ 170: } 196,
{ 171: } 196,
{ 172: } 196,
{ 173: } 196,
{ 174: } 196,
{ 175: } 197,
{ 176: } 197,
{ 177: } 197,
{ 178: } 198,
{ 179: } 200,
{ 180: } 201,
{ 181: } 202,
{ 182: } 203,
{ 183: } 204,
{ 184: } 205,
{ 185: } 206,
{ 186: } 207,
{ 187: } 208,
{ 188: } 209,
{ 189: } 210,
{ 190: } 212,
{ 191: } 214,
{ 192: } 216,
{ 193: } 217,
{ 194: } 218,
{ 195: } 219,
{ 196: } 220,
{ 197: } 221,
{ 198: } 222,
{ 199: } 223,
{ 200: } 225,
{ 201: } 226,
{ 202: } 228,
{ 203: } 230,
{ 204: } 232,
{ 205: } 234,
{ 206: } 235,
{ 207: } 235,
{ 208: } 235,
{ 209: } 236,
{ 210: } 237,
{ 211: } 237,
{ 212: } 237,
{ 213: } 237,
{ 214: } 237,
{ 215: } 237,
{ 216: } 238,
{ 217: } 239,
{ 218: } 240,
{ 219: } 242,
{ 220: } 243,
{ 221: } 245,
{ 222: } 246,
{ 223: } 247,
{ 224: } 248,
{ 225: } 249,
{ 226: } 250,
{ 227: } 252,
{ 228: } 254,
{ 229: } 255,
{ 230: } 256,
{ 231: } 258,
{ 232: } 259,
{ 233: } 260,
{ 234: } 262,
{ 235: } 264,
{ 236: } 264,
{ 237: } 264,
{ 238: } 265,
{ 239: } 266,
{ 240: } 267,
{ 241: } 267,
{ 242: } 267,
{ 243: } 269,
{ 244: } 270,
{ 245: } 271,
{ 246: } 272,
{ 247: } 274,
{ 248: } 276,
{ 249: } 278,
{ 250: } 279,
{ 251: } 280,
{ 252: } 281,
{ 253: } 283,
{ 254: } 285,
{ 255: } 286,
{ 256: } 286,
{ 257: } 286,
{ 258: } 287,
{ 259: } 288,
{ 260: } 290,
{ 261: } 291,
{ 262: } 292,
{ 263: } 293,
{ 264: } 294,
{ 265: } 295,
{ 266: } 297,
{ 267: } 297,
{ 268: } 298,
{ 269: } 300,
{ 270: } 302,
{ 271: } 303,
{ 272: } 305,
{ 273: } 307,
{ 274: } 307,
{ 275: } 309,
{ 276: } 309,
{ 277: } 309,
{ 278: } 309,
{ 279: } 309,
{ 280: } 309,
{ 281: } 309,
{ 282: } 309,
{ 283: } 309,
{ 284: } 309,
{ 285: } 309,
{ 286: } 309,
{ 287: } 309,
{ 288: } 309,
{ 289: } 309,
{ 290: } 309,
{ 291: } 309,
{ 292: } 309,
{ 293: } 309,
{ 294: } 309,
{ 295: } 309,
{ 296: } 309,
{ 297: } 309,
{ 298: } 309,
{ 299: } 309,
{ 300: } 309,
{ 301: } 310,
{ 302: } 310,
{ 303: } 310,
{ 304: } 310,
{ 305: } 310,
{ 306: } 310,
{ 307: } 310,
{ 308: } 310,
{ 309: } 310,
{ 310: } 310,
{ 311: } 310
);

yymh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 2,
{ 3: } 3,
{ 4: } 4,
{ 5: } 6,
{ 6: } 9,
{ 7: } 12,
{ 8: } 14,
{ 9: } 16,
{ 10: } 18,
{ 11: } 20,
{ 12: } 22,
{ 13: } 23,
{ 14: } 25,
{ 15: } 27,
{ 16: } 29,
{ 17: } 31,
{ 18: } 33,
{ 19: } 35,
{ 20: } 37,
{ 21: } 39,
{ 22: } 41,
{ 23: } 43,
{ 24: } 45,
{ 25: } 47,
{ 26: } 49,
{ 27: } 51,
{ 28: } 53,
{ 29: } 55,
{ 30: } 57,
{ 31: } 59,
{ 32: } 61,
{ 33: } 63,
{ 34: } 65,
{ 35: } 67,
{ 36: } 69,
{ 37: } 71,
{ 38: } 73,
{ 39: } 75,
{ 40: } 77,
{ 41: } 79,
{ 42: } 81,
{ 43: } 83,
{ 44: } 85,
{ 45: } 87,
{ 46: } 89,
{ 47: } 91,
{ 48: } 93,
{ 49: } 95,
{ 50: } 97,
{ 51: } 99,
{ 52: } 101,
{ 53: } 102,
{ 54: } 103,
{ 55: } 104,
{ 56: } 105,
{ 57: } 106,
{ 58: } 106,
{ 59: } 107,
{ 60: } 107,
{ 61: } 108,
{ 62: } 108,
{ 63: } 108,
{ 64: } 109,
{ 65: } 111,
{ 66: } 112,
{ 67: } 113,
{ 68: } 113,
{ 69: } 113,
{ 70: } 114,
{ 71: } 115,
{ 72: } 116,
{ 73: } 117,
{ 74: } 118,
{ 75: } 119,
{ 76: } 120,
{ 77: } 121,
{ 78: } 122,
{ 79: } 122,
{ 80: } 122,
{ 81: } 122,
{ 82: } 122,
{ 83: } 122,
{ 84: } 122,
{ 85: } 122,
{ 86: } 122,
{ 87: } 123,
{ 88: } 124,
{ 89: } 125,
{ 90: } 126,
{ 91: } 127,
{ 92: } 128,
{ 93: } 129,
{ 94: } 130,
{ 95: } 131,
{ 96: } 132,
{ 97: } 133,
{ 98: } 134,
{ 99: } 135,
{ 100: } 136,
{ 101: } 137,
{ 102: } 138,
{ 103: } 139,
{ 104: } 140,
{ 105: } 141,
{ 106: } 142,
{ 107: } 143,
{ 108: } 144,
{ 109: } 145,
{ 110: } 146,
{ 111: } 147,
{ 112: } 148,
{ 113: } 149,
{ 114: } 150,
{ 115: } 151,
{ 116: } 152,
{ 117: } 153,
{ 118: } 154,
{ 119: } 155,
{ 120: } 155,
{ 121: } 156,
{ 122: } 157,
{ 123: } 158,
{ 124: } 159,
{ 125: } 159,
{ 126: } 159,
{ 127: } 159,
{ 128: } 159,
{ 129: } 159,
{ 130: } 159,
{ 131: } 159,
{ 132: } 159,
{ 133: } 159,
{ 134: } 160,
{ 135: } 161,
{ 136: } 162,
{ 137: } 163,
{ 138: } 164,
{ 139: } 165,
{ 140: } 166,
{ 141: } 167,
{ 142: } 168,
{ 143: } 169,
{ 144: } 170,
{ 145: } 171,
{ 146: } 172,
{ 147: } 173,
{ 148: } 174,
{ 149: } 175,
{ 150: } 176,
{ 151: } 177,
{ 152: } 178,
{ 153: } 179,
{ 154: } 180,
{ 155: } 181,
{ 156: } 183,
{ 157: } 184,
{ 158: } 185,
{ 159: } 187,
{ 160: } 189,
{ 161: } 190,
{ 162: } 191,
{ 163: } 192,
{ 164: } 193,
{ 165: } 194,
{ 166: } 194,
{ 167: } 194,
{ 168: } 195,
{ 169: } 195,
{ 170: } 195,
{ 171: } 195,
{ 172: } 195,
{ 173: } 195,
{ 174: } 196,
{ 175: } 196,
{ 176: } 196,
{ 177: } 197,
{ 178: } 199,
{ 179: } 200,
{ 180: } 201,
{ 181: } 202,
{ 182: } 203,
{ 183: } 204,
{ 184: } 205,
{ 185: } 206,
{ 186: } 207,
{ 187: } 208,
{ 188: } 209,
{ 189: } 211,
{ 190: } 213,
{ 191: } 215,
{ 192: } 216,
{ 193: } 217,
{ 194: } 218,
{ 195: } 219,
{ 196: } 220,
{ 197: } 221,
{ 198: } 222,
{ 199: } 224,
{ 200: } 225,
{ 201: } 227,
{ 202: } 229,
{ 203: } 231,
{ 204: } 233,
{ 205: } 234,
{ 206: } 234,
{ 207: } 234,
{ 208: } 235,
{ 209: } 236,
{ 210: } 236,
{ 211: } 236,
{ 212: } 236,
{ 213: } 236,
{ 214: } 236,
{ 215: } 237,
{ 216: } 238,
{ 217: } 239,
{ 218: } 241,
{ 219: } 242,
{ 220: } 244,
{ 221: } 245,
{ 222: } 246,
{ 223: } 247,
{ 224: } 248,
{ 225: } 249,
{ 226: } 251,
{ 227: } 253,
{ 228: } 254,
{ 229: } 255,
{ 230: } 257,
{ 231: } 258,
{ 232: } 259,
{ 233: } 261,
{ 234: } 263,
{ 235: } 263,
{ 236: } 263,
{ 237: } 264,
{ 238: } 265,
{ 239: } 266,
{ 240: } 266,
{ 241: } 266,
{ 242: } 268,
{ 243: } 269,
{ 244: } 270,
{ 245: } 271,
{ 246: } 273,
{ 247: } 275,
{ 248: } 277,
{ 249: } 278,
{ 250: } 279,
{ 251: } 280,
{ 252: } 282,
{ 253: } 284,
{ 254: } 285,
{ 255: } 285,
{ 256: } 285,
{ 257: } 286,
{ 258: } 287,
{ 259: } 289,
{ 260: } 290,
{ 261: } 291,
{ 262: } 292,
{ 263: } 293,
{ 264: } 294,
{ 265: } 296,
{ 266: } 296,
{ 267: } 297,
{ 268: } 299,
{ 269: } 301,
{ 270: } 302,
{ 271: } 304,
{ 272: } 306,
{ 273: } 306,
{ 274: } 308,
{ 275: } 308,
{ 276: } 308,
{ 277: } 308,
{ 278: } 308,
{ 279: } 308,
{ 280: } 308,
{ 281: } 308,
{ 282: } 308,
{ 283: } 308,
{ 284: } 308,
{ 285: } 308,
{ 286: } 308,
{ 287: } 308,
{ 288: } 308,
{ 289: } 308,
{ 290: } 308,
{ 291: } 308,
{ 292: } 308,
{ 293: } 308,
{ 294: } 308,
{ 295: } 308,
{ 296: } 308,
{ 297: } 308,
{ 298: } 308,
{ 299: } 308,
{ 300: } 309,
{ 301: } 309,
{ 302: } 309,
{ 303: } 309,
{ 304: } 309,
{ 305: } 309,
{ 306: } 309,
{ 307: } 309,
{ 308: } 309,
{ 309: } 309,
{ 310: } 309,
{ 311: } 310
);

yytl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 55,
{ 2: } 109,
{ 3: } 111,
{ 4: } 113,
{ 5: } 115,
{ 6: } 118,
{ 7: } 123,
{ 8: } 129,
{ 9: } 130,
{ 10: } 131,
{ 11: } 132,
{ 12: } 134,
{ 13: } 136,
{ 14: } 144,
{ 15: } 144,
{ 16: } 144,
{ 17: } 144,
{ 18: } 144,
{ 19: } 144,
{ 20: } 144,
{ 21: } 144,
{ 22: } 144,
{ 23: } 144,
{ 24: } 144,
{ 25: } 144,
{ 26: } 144,
{ 27: } 145,
{ 28: } 148,
{ 29: } 151,
{ 30: } 155,
{ 31: } 157,
{ 32: } 159,
{ 33: } 161,
{ 34: } 163,
{ 35: } 165,
{ 36: } 168,
{ 37: } 170,
{ 38: } 174,
{ 39: } 174,
{ 40: } 174,
{ 41: } 176,
{ 42: } 178,
{ 43: } 180,
{ 44: } 183,
{ 45: } 185,
{ 46: } 187,
{ 47: } 189,
{ 48: } 191,
{ 49: } 193,
{ 50: } 195,
{ 51: } 196,
{ 52: } 196,
{ 53: } 196,
{ 54: } 196,
{ 55: } 197,
{ 56: } 197,
{ 57: } 197,
{ 58: } 197,
{ 59: } 199,
{ 60: } 199,
{ 61: } 201,
{ 62: } 201,
{ 63: } 203,
{ 64: } 205,
{ 65: } 206,
{ 66: } 211,
{ 67: } 212,
{ 68: } 212,
{ 69: } 213,
{ 70: } 215,
{ 71: } 218,
{ 72: } 218,
{ 73: } 218,
{ 74: } 218,
{ 75: } 218,
{ 76: } 218,
{ 77: } 218,
{ 78: } 218,
{ 79: } 218,
{ 80: } 220,
{ 81: } 226,
{ 82: } 229,
{ 83: } 230,
{ 84: } 237,
{ 85: } 238,
{ 86: } 239,
{ 87: } 240,
{ 88: } 242,
{ 89: } 244,
{ 90: } 246,
{ 91: } 248,
{ 92: } 250,
{ 93: } 252,
{ 94: } 254,
{ 95: } 257,
{ 96: } 259,
{ 97: } 261,
{ 98: } 263,
{ 99: } 265,
{ 100: } 267,
{ 101: } 269,
{ 102: } 272,
{ 103: } 274,
{ 104: } 276,
{ 105: } 278,
{ 106: } 280,
{ 107: } 282,
{ 108: } 284,
{ 109: } 286,
{ 110: } 288,
{ 111: } 290,
{ 112: } 292,
{ 113: } 294,
{ 114: } 296,
{ 115: } 298,
{ 116: } 300,
{ 117: } 300,
{ 118: } 300,
{ 119: } 300,
{ 120: } 302,
{ 121: } 303,
{ 122: } 304,
{ 123: } 305,
{ 124: } 305,
{ 125: } 306,
{ 126: } 307,
{ 127: } 309,
{ 128: } 311,
{ 129: } 312,
{ 130: } 313,
{ 131: } 314,
{ 132: } 316,
{ 133: } 317,
{ 134: } 318,
{ 135: } 318,
{ 136: } 320,
{ 137: } 322,
{ 138: } 324,
{ 139: } 326,
{ 140: } 328,
{ 141: } 330,
{ 142: } 332,
{ 143: } 334,
{ 144: } 336,
{ 145: } 339,
{ 146: } 341,
{ 147: } 343,
{ 148: } 345,
{ 149: } 347,
{ 150: } 349,
{ 151: } 351,
{ 152: } 353,
{ 153: } 355,
{ 154: } 357,
{ 155: } 359,
{ 156: } 361,
{ 157: } 362,
{ 158: } 364,
{ 159: } 366,
{ 160: } 367,
{ 161: } 368,
{ 162: } 370,
{ 163: } 372,
{ 164: } 374,
{ 165: } 376,
{ 166: } 378,
{ 167: } 379,
{ 168: } 380,
{ 169: } 380,
{ 170: } 381,
{ 171: } 382,
{ 172: } 383,
{ 173: } 384,
{ 174: } 385,
{ 175: } 385,
{ 176: } 386,
{ 177: } 387,
{ 178: } 389,
{ 179: } 390,
{ 180: } 392,
{ 181: } 394,
{ 182: } 396,
{ 183: } 398,
{ 184: } 400,
{ 185: } 402,
{ 186: } 404,
{ 187: } 406,
{ 188: } 408,
{ 189: } 410,
{ 190: } 411,
{ 191: } 412,
{ 192: } 413,
{ 193: } 415,
{ 194: } 417,
{ 195: } 419,
{ 196: } 421,
{ 197: } 423,
{ 198: } 425,
{ 199: } 427,
{ 200: } 428,
{ 201: } 430,
{ 202: } 431,
{ 203: } 432,
{ 204: } 433,
{ 205: } 434,
{ 206: } 436,
{ 207: } 437,
{ 208: } 438,
{ 209: } 438,
{ 210: } 438,
{ 211: } 439,
{ 212: } 440,
{ 213: } 441,
{ 214: } 442,
{ 215: } 443,
{ 216: } 445,
{ 217: } 447,
{ 218: } 449,
{ 219: } 450,
{ 220: } 452,
{ 221: } 453,
{ 222: } 455,
{ 223: } 457,
{ 224: } 459,
{ 225: } 461,
{ 226: } 463,
{ 227: } 464,
{ 228: } 465,
{ 229: } 467,
{ 230: } 469,
{ 231: } 470,
{ 232: } 472,
{ 233: } 474,
{ 234: } 475,
{ 235: } 476,
{ 236: } 477,
{ 237: } 478,
{ 238: } 478,
{ 239: } 478,
{ 240: } 478,
{ 241: } 479,
{ 242: } 480,
{ 243: } 481,
{ 244: } 483,
{ 245: } 485,
{ 246: } 487,
{ 247: } 488,
{ 248: } 489,
{ 249: } 490,
{ 250: } 492,
{ 251: } 494,
{ 252: } 496,
{ 253: } 497,
{ 254: } 498,
{ 255: } 500,
{ 256: } 501,
{ 257: } 502,
{ 258: } 502,
{ 259: } 502,
{ 260: } 503,
{ 261: } 505,
{ 262: } 507,
{ 263: } 509,
{ 264: } 511,
{ 265: } 513,
{ 266: } 514,
{ 267: } 515,
{ 268: } 515,
{ 269: } 516,
{ 270: } 517,
{ 271: } 519,
{ 272: } 520,
{ 273: } 521,
{ 274: } 522,
{ 275: } 523,
{ 276: } 524,
{ 277: } 525,
{ 278: } 526,
{ 279: } 527,
{ 280: } 528,
{ 281: } 529,
{ 282: } 530,
{ 283: } 531,
{ 284: } 533,
{ 285: } 535,
{ 286: } 536,
{ 287: } 537,
{ 288: } 538,
{ 289: } 539,
{ 290: } 540,
{ 291: } 541,
{ 292: } 542,
{ 293: } 543,
{ 294: } 544,
{ 295: } 545,
{ 296: } 546,
{ 297: } 547,
{ 298: } 548,
{ 299: } 549,
{ 300: } 550,
{ 301: } 550,
{ 302: } 551,
{ 303: } 552,
{ 304: } 553,
{ 305: } 554,
{ 306: } 555,
{ 307: } 556,
{ 308: } 557,
{ 309: } 558,
{ 310: } 559,
{ 311: } 560
);

yyth : array [0..yynstates-1] of Integer = (
{ 0: } 54,
{ 1: } 108,
{ 2: } 110,
{ 3: } 112,
{ 4: } 114,
{ 5: } 117,
{ 6: } 122,
{ 7: } 128,
{ 8: } 129,
{ 9: } 130,
{ 10: } 131,
{ 11: } 133,
{ 12: } 135,
{ 13: } 143,
{ 14: } 143,
{ 15: } 143,
{ 16: } 143,
{ 17: } 143,
{ 18: } 143,
{ 19: } 143,
{ 20: } 143,
{ 21: } 143,
{ 22: } 143,
{ 23: } 143,
{ 24: } 143,
{ 25: } 143,
{ 26: } 144,
{ 27: } 147,
{ 28: } 150,
{ 29: } 154,
{ 30: } 156,
{ 31: } 158,
{ 32: } 160,
{ 33: } 162,
{ 34: } 164,
{ 35: } 167,
{ 36: } 169,
{ 37: } 173,
{ 38: } 173,
{ 39: } 173,
{ 40: } 175,
{ 41: } 177,
{ 42: } 179,
{ 43: } 182,
{ 44: } 184,
{ 45: } 186,
{ 46: } 188,
{ 47: } 190,
{ 48: } 192,
{ 49: } 194,
{ 50: } 195,
{ 51: } 195,
{ 52: } 195,
{ 53: } 195,
{ 54: } 196,
{ 55: } 196,
{ 56: } 196,
{ 57: } 196,
{ 58: } 198,
{ 59: } 198,
{ 60: } 200,
{ 61: } 200,
{ 62: } 202,
{ 63: } 204,
{ 64: } 205,
{ 65: } 210,
{ 66: } 211,
{ 67: } 211,
{ 68: } 212,
{ 69: } 214,
{ 70: } 217,
{ 71: } 217,
{ 72: } 217,
{ 73: } 217,
{ 74: } 217,
{ 75: } 217,
{ 76: } 217,
{ 77: } 217,
{ 78: } 217,
{ 79: } 219,
{ 80: } 225,
{ 81: } 228,
{ 82: } 229,
{ 83: } 236,
{ 84: } 237,
{ 85: } 238,
{ 86: } 239,
{ 87: } 241,
{ 88: } 243,
{ 89: } 245,
{ 90: } 247,
{ 91: } 249,
{ 92: } 251,
{ 93: } 253,
{ 94: } 256,
{ 95: } 258,
{ 96: } 260,
{ 97: } 262,
{ 98: } 264,
{ 99: } 266,
{ 100: } 268,
{ 101: } 271,
{ 102: } 273,
{ 103: } 275,
{ 104: } 277,
{ 105: } 279,
{ 106: } 281,
{ 107: } 283,
{ 108: } 285,
{ 109: } 287,
{ 110: } 289,
{ 111: } 291,
{ 112: } 293,
{ 113: } 295,
{ 114: } 297,
{ 115: } 299,
{ 116: } 299,
{ 117: } 299,
{ 118: } 299,
{ 119: } 301,
{ 120: } 302,
{ 121: } 303,
{ 122: } 304,
{ 123: } 304,
{ 124: } 305,
{ 125: } 306,
{ 126: } 308,
{ 127: } 310,
{ 128: } 311,
{ 129: } 312,
{ 130: } 313,
{ 131: } 315,
{ 132: } 316,
{ 133: } 317,
{ 134: } 317,
{ 135: } 319,
{ 136: } 321,
{ 137: } 323,
{ 138: } 325,
{ 139: } 327,
{ 140: } 329,
{ 141: } 331,
{ 142: } 333,
{ 143: } 335,
{ 144: } 338,
{ 145: } 340,
{ 146: } 342,
{ 147: } 344,
{ 148: } 346,
{ 149: } 348,
{ 150: } 350,
{ 151: } 352,
{ 152: } 354,
{ 153: } 356,
{ 154: } 358,
{ 155: } 360,
{ 156: } 361,
{ 157: } 363,
{ 158: } 365,
{ 159: } 366,
{ 160: } 367,
{ 161: } 369,
{ 162: } 371,
{ 163: } 373,
{ 164: } 375,
{ 165: } 377,
{ 166: } 378,
{ 167: } 379,
{ 168: } 379,
{ 169: } 380,
{ 170: } 381,
{ 171: } 382,
{ 172: } 383,
{ 173: } 384,
{ 174: } 384,
{ 175: } 385,
{ 176: } 386,
{ 177: } 388,
{ 178: } 389,
{ 179: } 391,
{ 180: } 393,
{ 181: } 395,
{ 182: } 397,
{ 183: } 399,
{ 184: } 401,
{ 185: } 403,
{ 186: } 405,
{ 187: } 407,
{ 188: } 409,
{ 189: } 410,
{ 190: } 411,
{ 191: } 412,
{ 192: } 414,
{ 193: } 416,
{ 194: } 418,
{ 195: } 420,
{ 196: } 422,
{ 197: } 424,
{ 198: } 426,
{ 199: } 427,
{ 200: } 429,
{ 201: } 430,
{ 202: } 431,
{ 203: } 432,
{ 204: } 433,
{ 205: } 435,
{ 206: } 436,
{ 207: } 437,
{ 208: } 437,
{ 209: } 437,
{ 210: } 438,
{ 211: } 439,
{ 212: } 440,
{ 213: } 441,
{ 214: } 442,
{ 215: } 444,
{ 216: } 446,
{ 217: } 448,
{ 218: } 449,
{ 219: } 451,
{ 220: } 452,
{ 221: } 454,
{ 222: } 456,
{ 223: } 458,
{ 224: } 460,
{ 225: } 462,
{ 226: } 463,
{ 227: } 464,
{ 228: } 466,
{ 229: } 468,
{ 230: } 469,
{ 231: } 471,
{ 232: } 473,
{ 233: } 474,
{ 234: } 475,
{ 235: } 476,
{ 236: } 477,
{ 237: } 477,
{ 238: } 477,
{ 239: } 477,
{ 240: } 478,
{ 241: } 479,
{ 242: } 480,
{ 243: } 482,
{ 244: } 484,
{ 245: } 486,
{ 246: } 487,
{ 247: } 488,
{ 248: } 489,
{ 249: } 491,
{ 250: } 493,
{ 251: } 495,
{ 252: } 496,
{ 253: } 497,
{ 254: } 499,
{ 255: } 500,
{ 256: } 501,
{ 257: } 501,
{ 258: } 501,
{ 259: } 502,
{ 260: } 504,
{ 261: } 506,
{ 262: } 508,
{ 263: } 510,
{ 264: } 512,
{ 265: } 513,
{ 266: } 514,
{ 267: } 514,
{ 268: } 515,
{ 269: } 516,
{ 270: } 518,
{ 271: } 519,
{ 272: } 520,
{ 273: } 521,
{ 274: } 522,
{ 275: } 523,
{ 276: } 524,
{ 277: } 525,
{ 278: } 526,
{ 279: } 527,
{ 280: } 528,
{ 281: } 529,
{ 282: } 530,
{ 283: } 532,
{ 284: } 534,
{ 285: } 535,
{ 286: } 536,
{ 287: } 537,
{ 288: } 538,
{ 289: } 539,
{ 290: } 540,
{ 291: } 541,
{ 292: } 542,
{ 293: } 543,
{ 294: } 544,
{ 295: } 545,
{ 296: } 546,
{ 297: } 547,
{ 298: } 548,
{ 299: } 549,
{ 300: } 549,
{ 301: } 550,
{ 302: } 551,
{ 303: } 552,
{ 304: } 553,
{ 305: } 554,
{ 306: } 555,
{ 307: } 556,
{ 308: } 557,
{ 309: } 558,
{ 310: } 559,
{ 311: } 559
);


var yyn : Integer;

label start, scan, action;

begin

start:

  (* initialize: *)

  yynew;

scan:

  (* mark positions and matches: *)

  for yyn := yykl[yystate] to     yykh[yystate] do yymark(yyk[yyn]);
  for yyn := yymh[yystate] downto yyml[yystate] do yymatch(yym[yyn]);

  if yytl[yystate]>yyth[yystate] then goto action; (* dead state *)

  (* get next character: *)

  yyscan;

  (* determine action: *)

  yyn := yytl[yystate];
  while (yyn<=yyth[yystate]) and not (yyactchar in yyt[yyn].cc) do inc(yyn);
  if yyn>yyth[yystate] then goto action;
    (* no transition on yyactchar in this state *)

  (* switch to new state: *)

  yystate := yyt[yyn].s;

  goto scan;

action:

  (* execute action: *)

  if yyfind(yyrule) then
    begin
      yyaction(yyrule);
      if yyreject then goto action;
    end
  else if not yydefault and yywrap then
    begin
      yyclear;
      return(0);
    end;

  if not yydone then goto start;

  yylex := yyretval;

end(*yylex*);




function act_token : string;
begin
  act_token:=yytext;
end;

end.

