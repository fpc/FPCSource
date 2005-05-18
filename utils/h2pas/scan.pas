
(* lexical analyzer template (TP Lex V3.0), V1.0 3-2-91 AG *)

(* global definitions: *)
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


unit scan;

  interface

  uses
   strings,
   lexlib,yacclib;

    const
       version = '0.99.16';

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
          t_default_value
          { p1 expr for value }
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
          't_default_value'
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
                        begin
                           if in_space_define=1 then
                             in_space_define:=2;
                           return(ID);
                        end;
  84:
                        return(SEMICOLON);
  85:
                        begin
                           if (arglevel=0) and (in_space_define=2) then
                            begin
                              in_space_define:=0;
                              return(SPACE_DEFINE);
                            end;
                        end;
  86:
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
  87:
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
  88:
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

yynmarks   = 303;
yynmatches = 303;
yyntrans   = 548;
yynstates  = 307;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  { 1: }
  { 2: }
  25,
  88,
  { 3: }
  88,
  { 4: }
  88,
  { 5: }
  83,
  88,
  { 6: }
  7,
  9,
  88,
  { 7: }
  7,
  9,
  88,
  { 8: }
  11,
  88,
  { 9: }
  37,
  88,
  { 10: }
  24,
  88,
  { 11: }
  19,
  88,
  { 12: }
  20,
  88,
  { 13: }
  88,
  { 14: }
  21,
  88,
  { 15: }
  22,
  88,
  { 16: }
  23,
  88,
  { 17: }
  26,
  88,
  { 18: }
  27,
  88,
  { 19: }
  28,
  88,
  { 20: }
  29,
  88,
  { 21: }
  30,
  88,
  { 22: }
  31,
  88,
  { 23: }
  32,
  88,
  { 24: }
  33,
  88,
  { 25: }
  34,
  88,
  { 26: }
  36,
  88,
  { 27: }
  83,
  88,
  { 28: }
  83,
  88,
  { 29: }
  83,
  88,
  { 30: }
  83,
  88,
  { 31: }
  83,
  88,
  { 32: }
  83,
  88,
  { 33: }
  83,
  88,
  { 34: }
  83,
  88,
  { 35: }
  83,
  88,
  { 36: }
  83,
  88,
  { 37: }
  83,
  88,
  { 38: }
  66,
  88,
  { 39: }
  67,
  88,
  { 40: }
  83,
  88,
  { 41: }
  83,
  88,
  { 42: }
  83,
  88,
  { 43: }
  83,
  88,
  { 44: }
  83,
  88,
  { 45: }
  83,
  88,
  { 46: }
  83,
  88,
  { 47: }
  83,
  88,
  { 48: }
  83,
  88,
  { 49: }
  83,
  88,
  { 50: }
  84,
  88,
  { 51: }
  85,
  88,
  { 52: }
  86,
  { 53: }
  87,
  88,
  { 54: }
  88,
  { 55: }
  1,
  { 56: }
  2,
  { 57: }
  { 58: }
  3,
  { 59: }
  { 60: }
  4,
  { 61: }
  { 62: }
  { 63: }
  83,
  { 64: }
  7,
  9,
  { 65: }
  7,
  { 66: }
  7,
  { 67: }
  { 68: }
  { 69: }
  8,
  { 70: }
  10,
  { 71: }
  12,
  { 72: }
  13,
  { 73: }
  14,
  { 74: }
  16,
  { 75: }
  15,
  { 76: }
  18,
  { 77: }
  17,
  { 78: }
  { 79: }
  { 80: }
  { 81: }
  { 82: }
  { 83: }
  { 84: }
  { 85: }
  { 86: }
  83,
  { 87: }
  83,
  { 88: }
  83,
  { 89: }
  83,
  { 90: }
  83,
  { 91: }
  83,
  { 92: }
  83,
  { 93: }
  83,
  { 94: }
  83,
  { 95: }
  83,
  { 96: }
  83,
  { 97: }
  83,
  { 98: }
  83,
  { 99: }
  83,
  { 100: }
  83,
  { 101: }
  83,
  { 102: }
  83,
  { 103: }
  83,
  { 104: }
  83,
  { 105: }
  83,
  { 106: }
  83,
  { 107: }
  83,
  { 108: }
  83,
  { 109: }
  83,
  { 110: }
  83,
  { 111: }
  83,
  { 112: }
  83,
  { 113: }
  83,
  { 114: }
  { 115: }
  5,
  { 116: }
  6,
  { 117: }
  9,
  { 118: }
  { 119: }
  9,
  { 120: }
  8,
  { 121: }
  8,
  { 122: }
  58,
  { 123: }
  { 124: }
  { 125: }
  { 126: }
  { 127: }
  { 128: }
  { 129: }
  { 130: }
  { 131: }
  { 132: }
  35,
  { 133: }
  83,
  { 134: }
  83,
  { 135: }
  83,
  { 136: }
  83,
  { 137: }
  83,
  { 138: }
  83,
  { 139: }
  83,
  { 140: }
  83,
  { 141: }
  83,
  { 142: }
  83,
  { 143: }
  83,
  { 144: }
  83,
  { 145: }
  83,
  { 146: }
  83,
  { 147: }
  83,
  { 148: }
  83,
  { 149: }
  83,
  { 150: }
  83,
  { 151: }
  83,
  { 152: }
  83,
  { 153: }
  83,
  { 154: }
  69,
  83,
  { 155: }
  83,
  { 156: }
  83,
  { 157: }
  78,
  83,
  { 158: }
  77,
  83,
  { 159: }
  83,
  { 160: }
  83,
  { 161: }
  83,
  { 162: }
  83,
  { 163: }
  { 164: }
  { 165: }
  58,
  { 166: }
  { 167: }
  { 168: }
  { 169: }
  { 170: }
  { 171: }
  59,
  { 172: }
  { 173: }
  { 174: }
  83,
  { 175: }
  64,
  83,
  { 176: }
  83,
  { 177: }
  83,
  { 178: }
  83,
  { 179: }
  83,
  { 180: }
  83,
  { 181: }
  83,
  { 182: }
  83,
  { 183: }
  83,
  { 184: }
  83,
  { 185: }
  83,
  { 186: }
  48,
  83,
  { 187: }
  49,
  83,
  { 188: }
  62,
  83,
  { 189: }
  83,
  { 190: }
  83,
  { 191: }
  83,
  { 192: }
  83,
  { 193: }
  83,
  { 194: }
  83,
  { 195: }
  83,
  { 196: }
  71,
  83,
  { 197: }
  83,
  { 198: }
  79,
  83,
  { 199: }
  80,
  83,
  { 200: }
  81,
  83,
  { 201: }
  82,
  83,
  { 202: }
  { 203: }
  { 204: }
  52,
  { 205: }
  54,
  { 206: }
  { 207: }
  { 208: }
  { 209: }
  { 210: }
  { 211: }
  83,
  { 212: }
  83,
  { 213: }
  83,
  { 214: }
  40,
  83,
  { 215: }
  83,
  { 216: }
  76,
  83,
  { 217: }
  83,
  { 218: }
  83,
  { 219: }
  83,
  { 220: }
  83,
  { 221: }
  83,
  { 222: }
  75,
  83,
  { 223: }
  63,
  83,
  { 224: }
  83,
  { 225: }
  83,
  { 226: }
  70,
  83,
  { 227: }
  83,
  { 228: }
  83,
  { 229: }
  74,
  83,
  { 230: }
  { 231: }
  { 232: }
  53,
  { 233: }
  56,
  { 234: }
  55,
  { 235: }
  { 236: }
  { 237: }
  38,
  83,
  { 238: }
  83,
  { 239: }
  83,
  { 240: }
  83,
  { 241: }
  41,
  83,
  { 242: }
  42,
  83,
  { 243: }
  43,
  83,
  { 244: }
  83,
  { 245: }
  83,
  { 246: }
  83,
  { 247: }
  65,
  83,
  { 248: }
  72,
  83,
  { 249: }
  83,
  { 250: }
  { 251: }
  { 252: }
  60,
  { 253: }
  61,
  { 254: }
  39,
  83,
  { 255: }
  83,
  { 256: }
  83,
  { 257: }
  83,
  { 258: }
  83,
  { 259: }
  83,
  { 260: }
  68,
  83,
  { 261: }
  { 262: }
  57,
  { 263: }
  44,
  83,
  { 264: }
  46,
  83,
  { 265: }
  83,
  { 266: }
  47,
  83,
  { 267: }
  73,
  83,
  { 268: }
  { 269: }
  45,
  83,
  { 270: }
  { 271: }
  { 272: }
  { 273: }
  { 274: }
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
  51,
  { 296: }
  { 297: }
  { 298: }
  { 299: }
  { 300: }
  { 301: }
  { 302: }
  { 303: }
  { 304: }
  { 305: }
  { 306: }
  50
);

yym : array [1..yynmatches] of Integer = (
{ 0: }
{ 1: }
{ 2: }
  25,
  88,
{ 3: }
  88,
{ 4: }
  88,
{ 5: }
  83,
  88,
{ 6: }
  7,
  9,
  88,
{ 7: }
  7,
  9,
  88,
{ 8: }
  11,
  88,
{ 9: }
  37,
  88,
{ 10: }
  24,
  88,
{ 11: }
  19,
  88,
{ 12: }
  20,
  88,
{ 13: }
  88,
{ 14: }
  21,
  88,
{ 15: }
  22,
  88,
{ 16: }
  23,
  88,
{ 17: }
  26,
  88,
{ 18: }
  27,
  88,
{ 19: }
  28,
  88,
{ 20: }
  29,
  88,
{ 21: }
  30,
  88,
{ 22: }
  31,
  88,
{ 23: }
  32,
  88,
{ 24: }
  33,
  88,
{ 25: }
  34,
  88,
{ 26: }
  36,
  88,
{ 27: }
  83,
  88,
{ 28: }
  83,
  88,
{ 29: }
  83,
  88,
{ 30: }
  83,
  88,
{ 31: }
  83,
  88,
{ 32: }
  83,
  88,
{ 33: }
  83,
  88,
{ 34: }
  83,
  88,
{ 35: }
  83,
  88,
{ 36: }
  83,
  88,
{ 37: }
  83,
  88,
{ 38: }
  66,
  88,
{ 39: }
  67,
  88,
{ 40: }
  83,
  88,
{ 41: }
  83,
  88,
{ 42: }
  83,
  88,
{ 43: }
  83,
  88,
{ 44: }
  83,
  88,
{ 45: }
  83,
  88,
{ 46: }
  83,
  88,
{ 47: }
  83,
  88,
{ 48: }
  83,
  88,
{ 49: }
  83,
  88,
{ 50: }
  84,
  88,
{ 51: }
  85,
  88,
{ 52: }
  86,
{ 53: }
  88,
{ 54: }
  88,
{ 55: }
  1,
{ 56: }
  2,
{ 57: }
{ 58: }
  3,
{ 59: }
{ 60: }
  4,
{ 61: }
{ 62: }
{ 63: }
  83,
{ 64: }
  7,
  9,
{ 65: }
  7,
{ 66: }
  7,
{ 67: }
{ 68: }
{ 69: }
  8,
{ 70: }
  10,
{ 71: }
  12,
{ 72: }
  13,
{ 73: }
  14,
{ 74: }
  16,
{ 75: }
  15,
{ 76: }
  18,
{ 77: }
  17,
{ 78: }
{ 79: }
{ 80: }
{ 81: }
{ 82: }
{ 83: }
{ 84: }
{ 85: }
{ 86: }
  83,
{ 87: }
  83,
{ 88: }
  83,
{ 89: }
  83,
{ 90: }
  83,
{ 91: }
  83,
{ 92: }
  83,
{ 93: }
  83,
{ 94: }
  83,
{ 95: }
  83,
{ 96: }
  83,
{ 97: }
  83,
{ 98: }
  83,
{ 99: }
  83,
{ 100: }
  83,
{ 101: }
  83,
{ 102: }
  83,
{ 103: }
  83,
{ 104: }
  83,
{ 105: }
  83,
{ 106: }
  83,
{ 107: }
  83,
{ 108: }
  83,
{ 109: }
  83,
{ 110: }
  83,
{ 111: }
  83,
{ 112: }
  83,
{ 113: }
  83,
{ 114: }
  87,
{ 115: }
  5,
{ 116: }
  6,
{ 117: }
  9,
{ 118: }
{ 119: }
  9,
{ 120: }
  8,
{ 121: }
  8,
{ 122: }
  58,
{ 123: }
{ 124: }
{ 125: }
{ 126: }
{ 127: }
{ 128: }
{ 129: }
{ 130: }
{ 131: }
{ 132: }
  35,
{ 133: }
  83,
{ 134: }
  83,
{ 135: }
  83,
{ 136: }
  83,
{ 137: }
  83,
{ 138: }
  83,
{ 139: }
  83,
{ 140: }
  83,
{ 141: }
  83,
{ 142: }
  83,
{ 143: }
  83,
{ 144: }
  83,
{ 145: }
  83,
{ 146: }
  83,
{ 147: }
  83,
{ 148: }
  83,
{ 149: }
  83,
{ 150: }
  83,
{ 151: }
  83,
{ 152: }
  83,
{ 153: }
  83,
{ 154: }
  69,
  83,
{ 155: }
  83,
{ 156: }
  83,
{ 157: }
  78,
  83,
{ 158: }
  77,
  83,
{ 159: }
  83,
{ 160: }
  83,
{ 161: }
  83,
{ 162: }
  83,
{ 163: }
{ 164: }
{ 165: }
  58,
{ 166: }
{ 167: }
{ 168: }
{ 169: }
{ 170: }
{ 171: }
  59,
{ 172: }
{ 173: }
{ 174: }
  83,
{ 175: }
  64,
  83,
{ 176: }
  83,
{ 177: }
  83,
{ 178: }
  83,
{ 179: }
  83,
{ 180: }
  83,
{ 181: }
  83,
{ 182: }
  83,
{ 183: }
  83,
{ 184: }
  83,
{ 185: }
  83,
{ 186: }
  48,
  83,
{ 187: }
  49,
  83,
{ 188: }
  62,
  83,
{ 189: }
  83,
{ 190: }
  83,
{ 191: }
  83,
{ 192: }
  83,
{ 193: }
  83,
{ 194: }
  83,
{ 195: }
  83,
{ 196: }
  71,
  83,
{ 197: }
  83,
{ 198: }
  79,
  83,
{ 199: }
  80,
  83,
{ 200: }
  81,
  83,
{ 201: }
  82,
  83,
{ 202: }
{ 203: }
{ 204: }
  52,
{ 205: }
  54,
{ 206: }
{ 207: }
{ 208: }
{ 209: }
{ 210: }
{ 211: }
  83,
{ 212: }
  83,
{ 213: }
  83,
{ 214: }
  40,
  83,
{ 215: }
  83,
{ 216: }
  76,
  83,
{ 217: }
  83,
{ 218: }
  83,
{ 219: }
  83,
{ 220: }
  83,
{ 221: }
  83,
{ 222: }
  75,
  83,
{ 223: }
  63,
  83,
{ 224: }
  83,
{ 225: }
  83,
{ 226: }
  70,
  83,
{ 227: }
  83,
{ 228: }
  83,
{ 229: }
  74,
  83,
{ 230: }
{ 231: }
{ 232: }
  53,
{ 233: }
  56,
{ 234: }
  55,
{ 235: }
{ 236: }
{ 237: }
  38,
  83,
{ 238: }
  83,
{ 239: }
  83,
{ 240: }
  83,
{ 241: }
  41,
  83,
{ 242: }
  42,
  83,
{ 243: }
  43,
  83,
{ 244: }
  83,
{ 245: }
  83,
{ 246: }
  83,
{ 247: }
  65,
  83,
{ 248: }
  72,
  83,
{ 249: }
  83,
{ 250: }
{ 251: }
{ 252: }
  60,
{ 253: }
  61,
{ 254: }
  39,
  83,
{ 255: }
  83,
{ 256: }
  83,
{ 257: }
  83,
{ 258: }
  83,
{ 259: }
  83,
{ 260: }
  68,
  83,
{ 261: }
{ 262: }
  57,
{ 263: }
  44,
  83,
{ 264: }
  46,
  83,
{ 265: }
  83,
{ 266: }
  47,
  83,
{ 267: }
  73,
  83,
{ 268: }
{ 269: }
  45,
  83,
{ 270: }
{ 271: }
{ 272: }
{ 273: }
{ 274: }
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
  51,
{ 296: }
{ 297: }
{ 298: }
{ 299: }
{ 300: }
{ 301: }
{ 302: }
{ 303: }
{ 304: }
{ 305: }
{ 306: }
  50
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
  ( cc: [ #1..#8,#11,#13..#31,'$','%','@','^','`',#127..#255 ]; s: 54),
  ( cc: [ #9,#12,' ' ]; s: 51),
  ( cc: [ #10 ]; s: 52),
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
  ( cc: [ ';' ]; s: 50),
  ( cc: [ '<' ]; s: 12),
  ( cc: [ '=' ]; s: 9),
  ( cc: [ '>' ]; s: 11),
  ( cc: [ '?' ]; s: 18),
  ( cc: [ 'A','B','D','G','I'..'K','M','O','Q','R',
            'T','U','X'..'Z','_','a','b','d','g','j','k',
            'm','o'..'r','w'..'z' ]; s: 49),
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
  ( cc: [ '\' ]; s: 53),
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
  ( cc: [ '{' ]; s: 38),
  ( cc: [ '|' ]; s: 14),
  ( cc: [ '}' ]; s: 39),
  ( cc: [ '~' ]; s: 16),
{ 1: }
  ( cc: [ #1..#8,#11,#13..#31,'$','%','@','^','`',#127..#255 ]; s: 54),
  ( cc: [ #9,#12,' ' ]; s: 51),
  ( cc: [ #10 ]; s: 52),
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
  ( cc: [ ';' ]; s: 50),
  ( cc: [ '<' ]; s: 12),
  ( cc: [ '=' ]; s: 9),
  ( cc: [ '>' ]; s: 11),
  ( cc: [ '?' ]; s: 18),
  ( cc: [ 'A','B','D','G','I'..'K','M','O','Q','R',
            'T','U','X'..'Z','_','a','b','d','g','j','k',
            'm','o'..'r','w'..'z' ]; s: 49),
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
  ( cc: [ '\' ]; s: 53),
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
  ( cc: [ '{' ]; s: 38),
  ( cc: [ '|' ]; s: 14),
  ( cc: [ '}' ]; s: 39),
  ( cc: [ '~' ]; s: 16),
{ 2: }
  ( cc: [ '*' ]; s: 55),
  ( cc: [ '/' ]; s: 56),
{ 3: }
  ( cc: [ #1..'!','#'..#255 ]; s: 57),
  ( cc: [ '"' ]; s: 58),
{ 4: }
  ( cc: [ #1..'&','('..#255 ]; s: 59),
  ( cc: [ '''' ]; s: 60),
{ 5: }
  ( cc: [ '"' ]; s: 61),
  ( cc: [ '''' ]; s: 62),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 6: }
  ( cc: [ '.' ]; s: 67),
  ( cc: [ '0'..'9' ]; s: 64),
  ( cc: [ 'E','e' ]; s: 68),
  ( cc: [ 'L','l' ]; s: 66),
  ( cc: [ 'U','u' ]; s: 65),
{ 7: }
  ( cc: [ '.' ]; s: 67),
  ( cc: [ '0'..'9' ]; s: 64),
  ( cc: [ 'E','e' ]; s: 68),
  ( cc: [ 'L','l' ]; s: 66),
  ( cc: [ 'U','u' ]; s: 65),
  ( cc: [ 'x' ]; s: 69),
{ 8: }
  ( cc: [ '>' ]; s: 70),
{ 9: }
  ( cc: [ '=' ]; s: 71),
{ 10: }
  ( cc: [ '=' ]; s: 72),
{ 11: }
  ( cc: [ '=' ]; s: 73),
  ( cc: [ '>' ]; s: 74),
{ 12: }
  ( cc: [ '<' ]; s: 76),
  ( cc: [ '=' ]; s: 75),
{ 13: }
  ( cc: [ #9 ]; s: 79),
  ( cc: [ ' ' ]; s: 82),
  ( cc: [ '#' ]; s: 77),
  ( cc: [ 'd' ]; s: 84),
  ( cc: [ 'e' ]; s: 80),
  ( cc: [ 'i' ]; s: 78),
  ( cc: [ 'p' ]; s: 83),
  ( cc: [ 'u' ]; s: 81),
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
  ( cc: [ '.' ]; s: 85),
{ 27: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'w','y','z' ]; s: 63),
  ( cc: [ 'n' ]; s: 87),
  ( cc: [ 'x' ]; s: 86),
{ 28: }
  ( cc: [ '0'..'9','A'..'S','U'..'X','Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'T' ]; s: 88),
  ( cc: [ 'Y' ]; s: 89),
{ 29: }
  ( cc: [ '0'..'9','B','C','E'..'N','P'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 91),
  ( cc: [ 'D' ]; s: 90),
  ( cc: [ 'O' ]; s: 92),
{ 30: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 93),
{ 31: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'I' ]; s: 94),
{ 32: }
  ( cc: [ '0'..'9','A'..'W','Y','Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'X' ]; s: 95),
{ 33: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 96),
{ 34: }
  ( cc: [ '0'..'9','A'..'N','P'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'O' ]; s: 97),
{ 35: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','i'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'h' ]; s: 98),
  ( cc: [ 'o' ]; s: 99),
{ 36: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 100),
{ 37: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','j'..'s','u'..'z' ]; s: 63),
  ( cc: [ 'h' ]; s: 102),
  ( cc: [ 'i' ]; s: 103),
  ( cc: [ 't' ]; s: 101),
{ 38: }
{ 39: }
{ 40: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'x','z' ]; s: 63),
  ( cc: [ 'y' ]; s: 104),
{ 41: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 105),
{ 42: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 106),
{ 43: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'k','m'..'z' ]; s: 63),
  ( cc: [ 'a' ]; s: 108),
  ( cc: [ 'l' ]; s: 107),
{ 44: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 109),
{ 45: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 110),
{ 46: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 111),
{ 47: }
  ( cc: [ '0'..'9','A'..'T','V'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'U' ]; s: 112),
{ 48: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 63),
  ( cc: [ 'u' ]; s: 113),
{ 49: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 50: }
{ 51: }
{ 52: }
{ 53: }
  ( cc: [ #10 ]; s: 114),
{ 54: }
{ 55: }
{ 56: }
{ 57: }
  ( cc: [ #1..'!','#'..#255 ]; s: 57),
  ( cc: [ '"' ]; s: 58),
{ 58: }
{ 59: }
  ( cc: [ #1..'&','('..#255 ]; s: 59),
  ( cc: [ '''' ]; s: 60),
{ 60: }
{ 61: }
  ( cc: [ #1..'!','#'..#255 ]; s: 61),
  ( cc: [ '"' ]; s: 115),
{ 62: }
  ( cc: [ #1..'&','('..#255 ]; s: 62),
  ( cc: [ '''' ]; s: 116),
{ 63: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 64: }
  ( cc: [ '.' ]; s: 67),
  ( cc: [ '0'..'9' ]; s: 64),
  ( cc: [ 'E','e' ]; s: 68),
  ( cc: [ 'L','l' ]; s: 66),
  ( cc: [ 'U','u' ]; s: 65),
{ 65: }
  ( cc: [ 'L','l' ]; s: 66),
{ 66: }
{ 67: }
  ( cc: [ '0'..'9' ]; s: 117),
{ 68: }
  ( cc: [ '+','-' ]; s: 118),
  ( cc: [ '0'..'9' ]; s: 119),
{ 69: }
  ( cc: [ '0'..'9','A'..'F','a'..'f' ]; s: 69),
  ( cc: [ 'L','l' ]; s: 121),
  ( cc: [ 'U','u' ]; s: 120),
{ 70: }
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
{ 77: }
{ 78: }
  ( cc: [ 'f' ]; s: 122),
  ( cc: [ 'n' ]; s: 123),
{ 79: }
  ( cc: [ #9,' ' ]; s: 79),
  ( cc: [ 'd' ]; s: 84),
  ( cc: [ 'e' ]; s: 80),
  ( cc: [ 'i' ]; s: 124),
  ( cc: [ 'p' ]; s: 83),
  ( cc: [ 'u' ]; s: 81),
{ 80: }
  ( cc: [ 'l' ]; s: 125),
  ( cc: [ 'n' ]; s: 126),
  ( cc: [ 'r' ]; s: 127),
{ 81: }
  ( cc: [ 'n' ]; s: 128),
{ 82: }
  ( cc: [ #9,' ' ]; s: 79),
  ( cc: [ '0'..'9' ]; s: 129),
  ( cc: [ 'd' ]; s: 84),
  ( cc: [ 'e' ]; s: 80),
  ( cc: [ 'i' ]; s: 124),
  ( cc: [ 'p' ]; s: 83),
  ( cc: [ 'u' ]; s: 81),
{ 83: }
  ( cc: [ 'r' ]; s: 130),
{ 84: }
  ( cc: [ 'e' ]; s: 131),
{ 85: }
  ( cc: [ '.' ]; s: 132),
{ 86: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 133),
{ 87: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 63),
  ( cc: [ 'u' ]; s: 134),
{ 88: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'D' ]; s: 135),
{ 89: }
  ( cc: [ '0'..'9','A'..'R','T'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'S' ]; s: 136),
{ 90: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 137),
{ 91: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 138),
{ 92: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'N' ]; s: 139),
{ 93: }
  ( cc: [ '0'..'9','A','B','D'..'R','T'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 141),
  ( cc: [ 'S' ]; s: 140),
{ 94: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'N' ]; s: 142),
{ 95: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'P' ]; s: 143),
{ 96: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'z' ]; s: 63),
  ( cc: [ 'i' ]; s: 144),
{ 97: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'I' ]; s: 145),
{ 98: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 63),
  ( cc: [ 'a' ]; s: 146),
{ 99: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 147),
{ 100: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'r','t'..'z' ]; s: 63),
  ( cc: [ 'i' ]; s: 148),
  ( cc: [ 's' ]; s: 149),
{ 101: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 150),
{ 102: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 151),
{ 103: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 63),
  ( cc: [ 'g' ]; s: 152),
{ 104: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'o','q'..'z' ]; s: 63),
  ( cc: [ 'p' ]; s: 153),
{ 105: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 154),
{ 106: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 155),
{ 107: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 156),
{ 108: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 157),
{ 109: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'R' ]; s: 158),
{ 110: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 159),
{ 111: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 63),
  ( cc: [ 'a' ]; s: 160),
{ 112: }
  ( cc: [ '0'..'9','A'..'F','H'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'G' ]; s: 161),
{ 113: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 63),
  ( cc: [ 'g' ]; s: 162),
{ 114: }
{ 115: }
{ 116: }
{ 117: }
  ( cc: [ '0'..'9' ]; s: 117),
  ( cc: [ 'E','e' ]; s: 68),
{ 118: }
  ( cc: [ '0'..'9' ]; s: 119),
{ 119: }
  ( cc: [ '0'..'9' ]; s: 119),
{ 120: }
  ( cc: [ 'L','l' ]; s: 121),
{ 121: }
{ 122: }
  ( cc: [ 'd' ]; s: 163),
{ 123: }
  ( cc: [ 'c' ]; s: 164),
{ 124: }
  ( cc: [ 'f' ]; s: 165),
  ( cc: [ 'n' ]; s: 123),
{ 125: }
  ( cc: [ 'i' ]; s: 167),
  ( cc: [ 's' ]; s: 166),
{ 126: }
  ( cc: [ 'd' ]; s: 168),
{ 127: }
  ( cc: [ 'r' ]; s: 169),
{ 128: }
  ( cc: [ 'd' ]; s: 170),
{ 129: }
  ( cc: [ ' ' ]; s: 171),
  ( cc: [ '0'..'9' ]; s: 129),
{ 130: }
  ( cc: [ 'a' ]; s: 172),
{ 131: }
  ( cc: [ 'f' ]; s: 173),
{ 132: }
{ 133: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 174),
{ 134: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'l','n'..'z' ]; s: 63),
  ( cc: [ 'm' ]; s: 175),
{ 135: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 176),
{ 136: }
  ( cc: [ '0'..'9','A'..'Z','a'..'z' ]; s: 63),
  ( cc: [ '_' ]; s: 177),
{ 137: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 178),
{ 138: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 179),
{ 139: }
  ( cc: [ '0'..'9','A'..'R','T'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'S' ]; s: 180),
{ 140: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 181),
{ 141: }
  ( cc: [ '0'..'9','A'..'J','L'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'K' ]; s: 182),
{ 142: }
  ( cc: [ '0'..'9','B'..'F','H'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 183),
  ( cc: [ 'G' ]; s: 184),
{ 143: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 185),
{ 144: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 63),
  ( cc: [ 'd' ]; s: 186),
{ 145: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'D' ]; s: 187),
{ 146: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 188),
{ 147: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'r','t'..'z' ]; s: 63),
  ( cc: [ 's' ]; s: 189),
{ 148: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 190),
{ 149: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'z' ]; s: 63),
  ( cc: [ 'i' ]; s: 191),
{ 150: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 63),
  ( cc: [ 'u' ]; s: 192),
{ 151: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 193),
{ 152: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 194),
{ 153: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 195),
{ 154: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 155: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 63),
  ( cc: [ 'g' ]; s: 196),
{ 156: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 63),
  ( cc: [ 'a' ]; s: 197),
{ 157: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 158: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 159: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'R' ]; s: 198),
{ 160: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 199),
{ 161: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 200),
{ 162: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 201),
{ 163: }
  ( cc: [ 'e' ]; s: 202),
{ 164: }
  ( cc: [ 'l' ]; s: 203),
{ 165: }
{ 166: }
  ( cc: [ 'e' ]; s: 204),
{ 167: }
  ( cc: [ 'f' ]; s: 205),
{ 168: }
  ( cc: [ 'i' ]; s: 206),
{ 169: }
  ( cc: [ 'o' ]; s: 207),
{ 170: }
  ( cc: [ 'e' ]; s: 208),
{ 171: }
{ 172: }
  ( cc: [ 'g' ]; s: 209),
{ 173: }
  ( cc: [ 'i' ]; s: 210),
{ 174: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 211),
{ 175: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 176: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 212),
{ 177: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'T' ]; s: 213),
{ 178: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 214),
{ 179: }
  ( cc: [ '0'..'9','A','C'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'B' ]; s: 215),
{ 180: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'T' ]; s: 216),
{ 181: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 217),
{ 182: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 218),
{ 183: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'P' ]; s: 219),
{ 184: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'D' ]; s: 220),
{ 185: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'N' ]; s: 221),
{ 186: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 187: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 188: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 189: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 222),
{ 190: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 223),
{ 191: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 63),
  ( cc: [ 'g' ]; s: 224),
{ 192: }
  ( cc: [ '0'..'9','A'..'Z','_','a','b','d'..'z' ]; s: 63),
  ( cc: [ 'c' ]; s: 225),
{ 193: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 226),
{ 194: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 227),
{ 195: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 63),
  ( cc: [ 'd' ]; s: 228),
{ 196: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 197: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 229),
{ 198: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 199: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 200: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 201: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 202: }
  ( cc: [ 'f' ]; s: 230),
{ 203: }
  ( cc: [ 'u' ]; s: 231),
{ 204: }
{ 205: }
{ 206: }
  ( cc: [ 'f' ]; s: 232),
{ 207: }
  ( cc: [ 'r' ]; s: 233),
{ 208: }
  ( cc: [ 'f' ]; s: 234),
{ 209: }
  ( cc: [ 'm' ]; s: 235),
{ 210: }
  ( cc: [ 'n' ]; s: 236),
{ 211: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 237),
{ 212: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 238),
{ 213: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'R' ]; s: 239),
{ 214: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 215: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 240),
{ 216: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 217: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 241),
{ 218: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'D' ]; s: 242),
{ 219: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'I' ]; s: 243),
{ 220: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'I' ]; s: 244),
{ 221: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'T' ]; s: 245),
{ 222: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 223: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 224: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 246),
{ 225: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 247),
{ 226: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 227: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 63),
  ( cc: [ 'd' ]; s: 248),
{ 228: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 249),
{ 229: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 230: }
  ( cc: [ ' ' ]; s: 250),
{ 231: }
  ( cc: [ 'd' ]; s: 251),
{ 232: }
{ 233: }
{ 234: }
{ 235: }
  ( cc: [ 'a' ]; s: 252),
{ 236: }
  ( cc: [ 'e' ]; s: 253),
{ 237: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 238: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 254),
{ 239: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 255),
{ 240: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 256),
{ 241: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 242: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 243: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 244: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 257),
{ 245: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'R' ]; s: 258),
{ 246: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 259),
{ 247: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 248: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 249: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'e','g'..'z' ]; s: 63),
  ( cc: [ 'f' ]; s: 260),
{ 250: }
  ( cc: [ '_' ]; s: 261),
{ 251: }
  ( cc: [ 'e' ]; s: 262),
{ 252: }
{ 253: }
{ 254: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 255: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'P' ]; s: 263),
{ 256: }
  ( cc: [ '0'..'9','A'..'J','L'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'K' ]; s: 264),
{ 257: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'P' ]; s: 265),
{ 258: }
  ( cc: [ '0'..'9','A'..'X','Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'Y' ]; s: 266),
{ 259: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 63),
  ( cc: [ 'd' ]; s: 267),
{ 260: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 261: }
  ( cc: [ '_' ]; s: 268),
{ 262: }
{ 263: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 264: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 265: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'I' ]; s: 269),
{ 266: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 267: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 268: }
  ( cc: [ 'c' ]; s: 270),
{ 269: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 270: }
  ( cc: [ 'p' ]; s: 271),
{ 271: }
  ( cc: [ 'l' ]; s: 272),
{ 272: }
  ( cc: [ 'u' ]; s: 273),
{ 273: }
  ( cc: [ 's' ]; s: 274),
{ 274: }
  ( cc: [ 'p' ]; s: 275),
{ 275: }
  ( cc: [ 'l' ]; s: 276),
{ 276: }
  ( cc: [ 'u' ]; s: 277),
{ 277: }
  ( cc: [ 's' ]; s: 278),
{ 278: }
  ( cc: [ #9,' ' ]; s: 278),
  ( cc: [ #10 ]; s: 279),
{ 279: }
  ( cc: [ 'e' ]; s: 280),
  ( cc: [ '}' ]; s: 281),
{ 280: }
  ( cc: [ 'x' ]; s: 282),
{ 281: }
  ( cc: [ #10 ]; s: 283),
{ 282: }
  ( cc: [ 't' ]; s: 284),
{ 283: }
  ( cc: [ '#' ]; s: 285),
{ 284: }
  ( cc: [ 'e' ]; s: 286),
{ 285: }
  ( cc: [ 'e' ]; s: 287),
{ 286: }
  ( cc: [ 'r' ]; s: 288),
{ 287: }
  ( cc: [ 'n' ]; s: 289),
{ 288: }
  ( cc: [ 'n' ]; s: 290),
{ 289: }
  ( cc: [ 'd' ]; s: 291),
{ 290: }
  ( cc: [ ' ' ]; s: 292),
{ 291: }
  ( cc: [ 'i' ]; s: 293),
{ 292: }
  ( cc: [ '"' ]; s: 294),
{ 293: }
  ( cc: [ 'f' ]; s: 295),
{ 294: }
  ( cc: [ 'C' ]; s: 296),
{ 295: }
{ 296: }
  ( cc: [ '"' ]; s: 297),
{ 297: }
  ( cc: [ ' ' ]; s: 298),
{ 298: }
  ( cc: [ '{' ]; s: 299),
{ 299: }
  ( cc: [ #10 ]; s: 300),
{ 300: }
  ( cc: [ '#' ]; s: 301),
{ 301: }
  ( cc: [ 'e' ]; s: 302),
{ 302: }
  ( cc: [ 'n' ]; s: 303),
{ 303: }
  ( cc: [ 'd' ]; s: 304),
{ 304: }
  ( cc: [ 'i' ]; s: 305),
{ 305: }
  ( cc: [ 'f' ]; s: 306)
{ 306: }
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
{ 53: } 101,
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
{ 115: } 150,
{ 116: } 151,
{ 117: } 152,
{ 118: } 153,
{ 119: } 153,
{ 120: } 154,
{ 121: } 155,
{ 122: } 156,
{ 123: } 157,
{ 124: } 157,
{ 125: } 157,
{ 126: } 157,
{ 127: } 157,
{ 128: } 157,
{ 129: } 157,
{ 130: } 157,
{ 131: } 157,
{ 132: } 157,
{ 133: } 158,
{ 134: } 159,
{ 135: } 160,
{ 136: } 161,
{ 137: } 162,
{ 138: } 163,
{ 139: } 164,
{ 140: } 165,
{ 141: } 166,
{ 142: } 167,
{ 143: } 168,
{ 144: } 169,
{ 145: } 170,
{ 146: } 171,
{ 147: } 172,
{ 148: } 173,
{ 149: } 174,
{ 150: } 175,
{ 151: } 176,
{ 152: } 177,
{ 153: } 178,
{ 154: } 179,
{ 155: } 181,
{ 156: } 182,
{ 157: } 183,
{ 158: } 185,
{ 159: } 187,
{ 160: } 188,
{ 161: } 189,
{ 162: } 190,
{ 163: } 191,
{ 164: } 191,
{ 165: } 191,
{ 166: } 192,
{ 167: } 192,
{ 168: } 192,
{ 169: } 192,
{ 170: } 192,
{ 171: } 192,
{ 172: } 193,
{ 173: } 193,
{ 174: } 193,
{ 175: } 194,
{ 176: } 196,
{ 177: } 197,
{ 178: } 198,
{ 179: } 199,
{ 180: } 200,
{ 181: } 201,
{ 182: } 202,
{ 183: } 203,
{ 184: } 204,
{ 185: } 205,
{ 186: } 206,
{ 187: } 208,
{ 188: } 210,
{ 189: } 212,
{ 190: } 213,
{ 191: } 214,
{ 192: } 215,
{ 193: } 216,
{ 194: } 217,
{ 195: } 218,
{ 196: } 219,
{ 197: } 221,
{ 198: } 222,
{ 199: } 224,
{ 200: } 226,
{ 201: } 228,
{ 202: } 230,
{ 203: } 230,
{ 204: } 230,
{ 205: } 231,
{ 206: } 232,
{ 207: } 232,
{ 208: } 232,
{ 209: } 232,
{ 210: } 232,
{ 211: } 232,
{ 212: } 233,
{ 213: } 234,
{ 214: } 235,
{ 215: } 237,
{ 216: } 238,
{ 217: } 240,
{ 218: } 241,
{ 219: } 242,
{ 220: } 243,
{ 221: } 244,
{ 222: } 245,
{ 223: } 247,
{ 224: } 249,
{ 225: } 250,
{ 226: } 251,
{ 227: } 253,
{ 228: } 254,
{ 229: } 255,
{ 230: } 257,
{ 231: } 257,
{ 232: } 257,
{ 233: } 258,
{ 234: } 259,
{ 235: } 260,
{ 236: } 260,
{ 237: } 260,
{ 238: } 262,
{ 239: } 263,
{ 240: } 264,
{ 241: } 265,
{ 242: } 267,
{ 243: } 269,
{ 244: } 271,
{ 245: } 272,
{ 246: } 273,
{ 247: } 274,
{ 248: } 276,
{ 249: } 278,
{ 250: } 279,
{ 251: } 279,
{ 252: } 279,
{ 253: } 280,
{ 254: } 281,
{ 255: } 283,
{ 256: } 284,
{ 257: } 285,
{ 258: } 286,
{ 259: } 287,
{ 260: } 288,
{ 261: } 290,
{ 262: } 290,
{ 263: } 291,
{ 264: } 293,
{ 265: } 295,
{ 266: } 296,
{ 267: } 298,
{ 268: } 300,
{ 269: } 300,
{ 270: } 302,
{ 271: } 302,
{ 272: } 302,
{ 273: } 302,
{ 274: } 302,
{ 275: } 302,
{ 276: } 302,
{ 277: } 302,
{ 278: } 302,
{ 279: } 302,
{ 280: } 302,
{ 281: } 302,
{ 282: } 302,
{ 283: } 302,
{ 284: } 302,
{ 285: } 302,
{ 286: } 302,
{ 287: } 302,
{ 288: } 302,
{ 289: } 302,
{ 290: } 302,
{ 291: } 302,
{ 292: } 302,
{ 293: } 302,
{ 294: } 302,
{ 295: } 302,
{ 296: } 303,
{ 297: } 303,
{ 298: } 303,
{ 299: } 303,
{ 300: } 303,
{ 301: } 303,
{ 302: } 303,
{ 303: } 303,
{ 304: } 303,
{ 305: } 303,
{ 306: } 303
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
{ 52: } 100,
{ 53: } 102,
{ 54: } 103,
{ 55: } 104,
{ 56: } 105,
{ 57: } 105,
{ 58: } 106,
{ 59: } 106,
{ 60: } 107,
{ 61: } 107,
{ 62: } 107,
{ 63: } 108,
{ 64: } 110,
{ 65: } 111,
{ 66: } 112,
{ 67: } 112,
{ 68: } 112,
{ 69: } 113,
{ 70: } 114,
{ 71: } 115,
{ 72: } 116,
{ 73: } 117,
{ 74: } 118,
{ 75: } 119,
{ 76: } 120,
{ 77: } 121,
{ 78: } 121,
{ 79: } 121,
{ 80: } 121,
{ 81: } 121,
{ 82: } 121,
{ 83: } 121,
{ 84: } 121,
{ 85: } 121,
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
{ 114: } 149,
{ 115: } 150,
{ 116: } 151,
{ 117: } 152,
{ 118: } 152,
{ 119: } 153,
{ 120: } 154,
{ 121: } 155,
{ 122: } 156,
{ 123: } 156,
{ 124: } 156,
{ 125: } 156,
{ 126: } 156,
{ 127: } 156,
{ 128: } 156,
{ 129: } 156,
{ 130: } 156,
{ 131: } 156,
{ 132: } 157,
{ 133: } 158,
{ 134: } 159,
{ 135: } 160,
{ 136: } 161,
{ 137: } 162,
{ 138: } 163,
{ 139: } 164,
{ 140: } 165,
{ 141: } 166,
{ 142: } 167,
{ 143: } 168,
{ 144: } 169,
{ 145: } 170,
{ 146: } 171,
{ 147: } 172,
{ 148: } 173,
{ 149: } 174,
{ 150: } 175,
{ 151: } 176,
{ 152: } 177,
{ 153: } 178,
{ 154: } 180,
{ 155: } 181,
{ 156: } 182,
{ 157: } 184,
{ 158: } 186,
{ 159: } 187,
{ 160: } 188,
{ 161: } 189,
{ 162: } 190,
{ 163: } 190,
{ 164: } 190,
{ 165: } 191,
{ 166: } 191,
{ 167: } 191,
{ 168: } 191,
{ 169: } 191,
{ 170: } 191,
{ 171: } 192,
{ 172: } 192,
{ 173: } 192,
{ 174: } 193,
{ 175: } 195,
{ 176: } 196,
{ 177: } 197,
{ 178: } 198,
{ 179: } 199,
{ 180: } 200,
{ 181: } 201,
{ 182: } 202,
{ 183: } 203,
{ 184: } 204,
{ 185: } 205,
{ 186: } 207,
{ 187: } 209,
{ 188: } 211,
{ 189: } 212,
{ 190: } 213,
{ 191: } 214,
{ 192: } 215,
{ 193: } 216,
{ 194: } 217,
{ 195: } 218,
{ 196: } 220,
{ 197: } 221,
{ 198: } 223,
{ 199: } 225,
{ 200: } 227,
{ 201: } 229,
{ 202: } 229,
{ 203: } 229,
{ 204: } 230,
{ 205: } 231,
{ 206: } 231,
{ 207: } 231,
{ 208: } 231,
{ 209: } 231,
{ 210: } 231,
{ 211: } 232,
{ 212: } 233,
{ 213: } 234,
{ 214: } 236,
{ 215: } 237,
{ 216: } 239,
{ 217: } 240,
{ 218: } 241,
{ 219: } 242,
{ 220: } 243,
{ 221: } 244,
{ 222: } 246,
{ 223: } 248,
{ 224: } 249,
{ 225: } 250,
{ 226: } 252,
{ 227: } 253,
{ 228: } 254,
{ 229: } 256,
{ 230: } 256,
{ 231: } 256,
{ 232: } 257,
{ 233: } 258,
{ 234: } 259,
{ 235: } 259,
{ 236: } 259,
{ 237: } 261,
{ 238: } 262,
{ 239: } 263,
{ 240: } 264,
{ 241: } 266,
{ 242: } 268,
{ 243: } 270,
{ 244: } 271,
{ 245: } 272,
{ 246: } 273,
{ 247: } 275,
{ 248: } 277,
{ 249: } 278,
{ 250: } 278,
{ 251: } 278,
{ 252: } 279,
{ 253: } 280,
{ 254: } 282,
{ 255: } 283,
{ 256: } 284,
{ 257: } 285,
{ 258: } 286,
{ 259: } 287,
{ 260: } 289,
{ 261: } 289,
{ 262: } 290,
{ 263: } 292,
{ 264: } 294,
{ 265: } 295,
{ 266: } 297,
{ 267: } 299,
{ 268: } 299,
{ 269: } 301,
{ 270: } 301,
{ 271: } 301,
{ 272: } 301,
{ 273: } 301,
{ 274: } 301,
{ 275: } 301,
{ 276: } 301,
{ 277: } 301,
{ 278: } 301,
{ 279: } 301,
{ 280: } 301,
{ 281: } 301,
{ 282: } 301,
{ 283: } 301,
{ 284: } 301,
{ 285: } 301,
{ 286: } 301,
{ 287: } 301,
{ 288: } 301,
{ 289: } 301,
{ 290: } 301,
{ 291: } 301,
{ 292: } 301,
{ 293: } 301,
{ 294: } 301,
{ 295: } 302,
{ 296: } 302,
{ 297: } 302,
{ 298: } 302,
{ 299: } 302,
{ 300: } 302,
{ 301: } 302,
{ 302: } 302,
{ 303: } 302,
{ 304: } 302,
{ 305: } 302,
{ 306: } 303
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
{ 53: } 101,
{ 54: } 102,
{ 55: } 103,
{ 56: } 104,
{ 57: } 105,
{ 58: } 105,
{ 59: } 106,
{ 60: } 106,
{ 61: } 107,
{ 62: } 107,
{ 63: } 107,
{ 64: } 108,
{ 65: } 110,
{ 66: } 111,
{ 67: } 112,
{ 68: } 112,
{ 69: } 112,
{ 70: } 113,
{ 71: } 114,
{ 72: } 115,
{ 73: } 116,
{ 74: } 117,
{ 75: } 118,
{ 76: } 119,
{ 77: } 120,
{ 78: } 121,
{ 79: } 121,
{ 80: } 121,
{ 81: } 121,
{ 82: } 121,
{ 83: } 121,
{ 84: } 121,
{ 85: } 121,
{ 86: } 121,
{ 87: } 122,
{ 88: } 123,
{ 89: } 124,
{ 90: } 125,
{ 91: } 126,
{ 92: } 127,
{ 93: } 128,
{ 94: } 129,
{ 95: } 130,
{ 96: } 131,
{ 97: } 132,
{ 98: } 133,
{ 99: } 134,
{ 100: } 135,
{ 101: } 136,
{ 102: } 137,
{ 103: } 138,
{ 104: } 139,
{ 105: } 140,
{ 106: } 141,
{ 107: } 142,
{ 108: } 143,
{ 109: } 144,
{ 110: } 145,
{ 111: } 146,
{ 112: } 147,
{ 113: } 148,
{ 114: } 149,
{ 115: } 150,
{ 116: } 151,
{ 117: } 152,
{ 118: } 153,
{ 119: } 153,
{ 120: } 154,
{ 121: } 155,
{ 122: } 156,
{ 123: } 157,
{ 124: } 157,
{ 125: } 157,
{ 126: } 157,
{ 127: } 157,
{ 128: } 157,
{ 129: } 157,
{ 130: } 157,
{ 131: } 157,
{ 132: } 157,
{ 133: } 158,
{ 134: } 159,
{ 135: } 160,
{ 136: } 161,
{ 137: } 162,
{ 138: } 163,
{ 139: } 164,
{ 140: } 165,
{ 141: } 166,
{ 142: } 167,
{ 143: } 168,
{ 144: } 169,
{ 145: } 170,
{ 146: } 171,
{ 147: } 172,
{ 148: } 173,
{ 149: } 174,
{ 150: } 175,
{ 151: } 176,
{ 152: } 177,
{ 153: } 178,
{ 154: } 179,
{ 155: } 181,
{ 156: } 182,
{ 157: } 183,
{ 158: } 185,
{ 159: } 187,
{ 160: } 188,
{ 161: } 189,
{ 162: } 190,
{ 163: } 191,
{ 164: } 191,
{ 165: } 191,
{ 166: } 192,
{ 167: } 192,
{ 168: } 192,
{ 169: } 192,
{ 170: } 192,
{ 171: } 192,
{ 172: } 193,
{ 173: } 193,
{ 174: } 193,
{ 175: } 194,
{ 176: } 196,
{ 177: } 197,
{ 178: } 198,
{ 179: } 199,
{ 180: } 200,
{ 181: } 201,
{ 182: } 202,
{ 183: } 203,
{ 184: } 204,
{ 185: } 205,
{ 186: } 206,
{ 187: } 208,
{ 188: } 210,
{ 189: } 212,
{ 190: } 213,
{ 191: } 214,
{ 192: } 215,
{ 193: } 216,
{ 194: } 217,
{ 195: } 218,
{ 196: } 219,
{ 197: } 221,
{ 198: } 222,
{ 199: } 224,
{ 200: } 226,
{ 201: } 228,
{ 202: } 230,
{ 203: } 230,
{ 204: } 230,
{ 205: } 231,
{ 206: } 232,
{ 207: } 232,
{ 208: } 232,
{ 209: } 232,
{ 210: } 232,
{ 211: } 232,
{ 212: } 233,
{ 213: } 234,
{ 214: } 235,
{ 215: } 237,
{ 216: } 238,
{ 217: } 240,
{ 218: } 241,
{ 219: } 242,
{ 220: } 243,
{ 221: } 244,
{ 222: } 245,
{ 223: } 247,
{ 224: } 249,
{ 225: } 250,
{ 226: } 251,
{ 227: } 253,
{ 228: } 254,
{ 229: } 255,
{ 230: } 257,
{ 231: } 257,
{ 232: } 257,
{ 233: } 258,
{ 234: } 259,
{ 235: } 260,
{ 236: } 260,
{ 237: } 260,
{ 238: } 262,
{ 239: } 263,
{ 240: } 264,
{ 241: } 265,
{ 242: } 267,
{ 243: } 269,
{ 244: } 271,
{ 245: } 272,
{ 246: } 273,
{ 247: } 274,
{ 248: } 276,
{ 249: } 278,
{ 250: } 279,
{ 251: } 279,
{ 252: } 279,
{ 253: } 280,
{ 254: } 281,
{ 255: } 283,
{ 256: } 284,
{ 257: } 285,
{ 258: } 286,
{ 259: } 287,
{ 260: } 288,
{ 261: } 290,
{ 262: } 290,
{ 263: } 291,
{ 264: } 293,
{ 265: } 295,
{ 266: } 296,
{ 267: } 298,
{ 268: } 300,
{ 269: } 300,
{ 270: } 302,
{ 271: } 302,
{ 272: } 302,
{ 273: } 302,
{ 274: } 302,
{ 275: } 302,
{ 276: } 302,
{ 277: } 302,
{ 278: } 302,
{ 279: } 302,
{ 280: } 302,
{ 281: } 302,
{ 282: } 302,
{ 283: } 302,
{ 284: } 302,
{ 285: } 302,
{ 286: } 302,
{ 287: } 302,
{ 288: } 302,
{ 289: } 302,
{ 290: } 302,
{ 291: } 302,
{ 292: } 302,
{ 293: } 302,
{ 294: } 302,
{ 295: } 302,
{ 296: } 303,
{ 297: } 303,
{ 298: } 303,
{ 299: } 303,
{ 300: } 303,
{ 301: } 303,
{ 302: } 303,
{ 303: } 303,
{ 304: } 303,
{ 305: } 303,
{ 306: } 303
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
{ 52: } 100,
{ 53: } 101,
{ 54: } 102,
{ 55: } 103,
{ 56: } 104,
{ 57: } 104,
{ 58: } 105,
{ 59: } 105,
{ 60: } 106,
{ 61: } 106,
{ 62: } 106,
{ 63: } 107,
{ 64: } 109,
{ 65: } 110,
{ 66: } 111,
{ 67: } 111,
{ 68: } 111,
{ 69: } 112,
{ 70: } 113,
{ 71: } 114,
{ 72: } 115,
{ 73: } 116,
{ 74: } 117,
{ 75: } 118,
{ 76: } 119,
{ 77: } 120,
{ 78: } 120,
{ 79: } 120,
{ 80: } 120,
{ 81: } 120,
{ 82: } 120,
{ 83: } 120,
{ 84: } 120,
{ 85: } 120,
{ 86: } 121,
{ 87: } 122,
{ 88: } 123,
{ 89: } 124,
{ 90: } 125,
{ 91: } 126,
{ 92: } 127,
{ 93: } 128,
{ 94: } 129,
{ 95: } 130,
{ 96: } 131,
{ 97: } 132,
{ 98: } 133,
{ 99: } 134,
{ 100: } 135,
{ 101: } 136,
{ 102: } 137,
{ 103: } 138,
{ 104: } 139,
{ 105: } 140,
{ 106: } 141,
{ 107: } 142,
{ 108: } 143,
{ 109: } 144,
{ 110: } 145,
{ 111: } 146,
{ 112: } 147,
{ 113: } 148,
{ 114: } 149,
{ 115: } 150,
{ 116: } 151,
{ 117: } 152,
{ 118: } 152,
{ 119: } 153,
{ 120: } 154,
{ 121: } 155,
{ 122: } 156,
{ 123: } 156,
{ 124: } 156,
{ 125: } 156,
{ 126: } 156,
{ 127: } 156,
{ 128: } 156,
{ 129: } 156,
{ 130: } 156,
{ 131: } 156,
{ 132: } 157,
{ 133: } 158,
{ 134: } 159,
{ 135: } 160,
{ 136: } 161,
{ 137: } 162,
{ 138: } 163,
{ 139: } 164,
{ 140: } 165,
{ 141: } 166,
{ 142: } 167,
{ 143: } 168,
{ 144: } 169,
{ 145: } 170,
{ 146: } 171,
{ 147: } 172,
{ 148: } 173,
{ 149: } 174,
{ 150: } 175,
{ 151: } 176,
{ 152: } 177,
{ 153: } 178,
{ 154: } 180,
{ 155: } 181,
{ 156: } 182,
{ 157: } 184,
{ 158: } 186,
{ 159: } 187,
{ 160: } 188,
{ 161: } 189,
{ 162: } 190,
{ 163: } 190,
{ 164: } 190,
{ 165: } 191,
{ 166: } 191,
{ 167: } 191,
{ 168: } 191,
{ 169: } 191,
{ 170: } 191,
{ 171: } 192,
{ 172: } 192,
{ 173: } 192,
{ 174: } 193,
{ 175: } 195,
{ 176: } 196,
{ 177: } 197,
{ 178: } 198,
{ 179: } 199,
{ 180: } 200,
{ 181: } 201,
{ 182: } 202,
{ 183: } 203,
{ 184: } 204,
{ 185: } 205,
{ 186: } 207,
{ 187: } 209,
{ 188: } 211,
{ 189: } 212,
{ 190: } 213,
{ 191: } 214,
{ 192: } 215,
{ 193: } 216,
{ 194: } 217,
{ 195: } 218,
{ 196: } 220,
{ 197: } 221,
{ 198: } 223,
{ 199: } 225,
{ 200: } 227,
{ 201: } 229,
{ 202: } 229,
{ 203: } 229,
{ 204: } 230,
{ 205: } 231,
{ 206: } 231,
{ 207: } 231,
{ 208: } 231,
{ 209: } 231,
{ 210: } 231,
{ 211: } 232,
{ 212: } 233,
{ 213: } 234,
{ 214: } 236,
{ 215: } 237,
{ 216: } 239,
{ 217: } 240,
{ 218: } 241,
{ 219: } 242,
{ 220: } 243,
{ 221: } 244,
{ 222: } 246,
{ 223: } 248,
{ 224: } 249,
{ 225: } 250,
{ 226: } 252,
{ 227: } 253,
{ 228: } 254,
{ 229: } 256,
{ 230: } 256,
{ 231: } 256,
{ 232: } 257,
{ 233: } 258,
{ 234: } 259,
{ 235: } 259,
{ 236: } 259,
{ 237: } 261,
{ 238: } 262,
{ 239: } 263,
{ 240: } 264,
{ 241: } 266,
{ 242: } 268,
{ 243: } 270,
{ 244: } 271,
{ 245: } 272,
{ 246: } 273,
{ 247: } 275,
{ 248: } 277,
{ 249: } 278,
{ 250: } 278,
{ 251: } 278,
{ 252: } 279,
{ 253: } 280,
{ 254: } 282,
{ 255: } 283,
{ 256: } 284,
{ 257: } 285,
{ 258: } 286,
{ 259: } 287,
{ 260: } 289,
{ 261: } 289,
{ 262: } 290,
{ 263: } 292,
{ 264: } 294,
{ 265: } 295,
{ 266: } 297,
{ 267: } 299,
{ 268: } 299,
{ 269: } 301,
{ 270: } 301,
{ 271: } 301,
{ 272: } 301,
{ 273: } 301,
{ 274: } 301,
{ 275: } 301,
{ 276: } 301,
{ 277: } 301,
{ 278: } 301,
{ 279: } 301,
{ 280: } 301,
{ 281: } 301,
{ 282: } 301,
{ 283: } 301,
{ 284: } 301,
{ 285: } 301,
{ 286: } 301,
{ 287: } 301,
{ 288: } 301,
{ 289: } 301,
{ 290: } 301,
{ 291: } 301,
{ 292: } 301,
{ 293: } 301,
{ 294: } 301,
{ 295: } 302,
{ 296: } 302,
{ 297: } 302,
{ 298: } 302,
{ 299: } 302,
{ 300: } 302,
{ 301: } 302,
{ 302: } 302,
{ 303: } 302,
{ 304: } 302,
{ 305: } 302,
{ 306: } 303
);

yytl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 54,
{ 2: } 107,
{ 3: } 109,
{ 4: } 111,
{ 5: } 113,
{ 6: } 116,
{ 7: } 121,
{ 8: } 127,
{ 9: } 128,
{ 10: } 129,
{ 11: } 130,
{ 12: } 132,
{ 13: } 134,
{ 14: } 142,
{ 15: } 142,
{ 16: } 142,
{ 17: } 142,
{ 18: } 142,
{ 19: } 142,
{ 20: } 142,
{ 21: } 142,
{ 22: } 142,
{ 23: } 142,
{ 24: } 142,
{ 25: } 142,
{ 26: } 142,
{ 27: } 143,
{ 28: } 146,
{ 29: } 149,
{ 30: } 153,
{ 31: } 155,
{ 32: } 157,
{ 33: } 159,
{ 34: } 161,
{ 35: } 163,
{ 36: } 166,
{ 37: } 168,
{ 38: } 172,
{ 39: } 172,
{ 40: } 172,
{ 41: } 174,
{ 42: } 176,
{ 43: } 178,
{ 44: } 181,
{ 45: } 183,
{ 46: } 185,
{ 47: } 187,
{ 48: } 189,
{ 49: } 191,
{ 50: } 192,
{ 51: } 192,
{ 52: } 192,
{ 53: } 192,
{ 54: } 193,
{ 55: } 193,
{ 56: } 193,
{ 57: } 193,
{ 58: } 195,
{ 59: } 195,
{ 60: } 197,
{ 61: } 197,
{ 62: } 199,
{ 63: } 201,
{ 64: } 202,
{ 65: } 207,
{ 66: } 208,
{ 67: } 208,
{ 68: } 209,
{ 69: } 211,
{ 70: } 214,
{ 71: } 214,
{ 72: } 214,
{ 73: } 214,
{ 74: } 214,
{ 75: } 214,
{ 76: } 214,
{ 77: } 214,
{ 78: } 214,
{ 79: } 216,
{ 80: } 222,
{ 81: } 225,
{ 82: } 226,
{ 83: } 233,
{ 84: } 234,
{ 85: } 235,
{ 86: } 236,
{ 87: } 238,
{ 88: } 240,
{ 89: } 242,
{ 90: } 244,
{ 91: } 246,
{ 92: } 248,
{ 93: } 250,
{ 94: } 253,
{ 95: } 255,
{ 96: } 257,
{ 97: } 259,
{ 98: } 261,
{ 99: } 263,
{ 100: } 265,
{ 101: } 268,
{ 102: } 270,
{ 103: } 272,
{ 104: } 274,
{ 105: } 276,
{ 106: } 278,
{ 107: } 280,
{ 108: } 282,
{ 109: } 284,
{ 110: } 286,
{ 111: } 288,
{ 112: } 290,
{ 113: } 292,
{ 114: } 294,
{ 115: } 294,
{ 116: } 294,
{ 117: } 294,
{ 118: } 296,
{ 119: } 297,
{ 120: } 298,
{ 121: } 299,
{ 122: } 299,
{ 123: } 300,
{ 124: } 301,
{ 125: } 303,
{ 126: } 305,
{ 127: } 306,
{ 128: } 307,
{ 129: } 308,
{ 130: } 310,
{ 131: } 311,
{ 132: } 312,
{ 133: } 312,
{ 134: } 314,
{ 135: } 316,
{ 136: } 318,
{ 137: } 320,
{ 138: } 322,
{ 139: } 324,
{ 140: } 326,
{ 141: } 328,
{ 142: } 330,
{ 143: } 333,
{ 144: } 335,
{ 145: } 337,
{ 146: } 339,
{ 147: } 341,
{ 148: } 343,
{ 149: } 345,
{ 150: } 347,
{ 151: } 349,
{ 152: } 351,
{ 153: } 353,
{ 154: } 355,
{ 155: } 356,
{ 156: } 358,
{ 157: } 360,
{ 158: } 361,
{ 159: } 362,
{ 160: } 364,
{ 161: } 366,
{ 162: } 368,
{ 163: } 370,
{ 164: } 371,
{ 165: } 372,
{ 166: } 372,
{ 167: } 373,
{ 168: } 374,
{ 169: } 375,
{ 170: } 376,
{ 171: } 377,
{ 172: } 377,
{ 173: } 378,
{ 174: } 379,
{ 175: } 381,
{ 176: } 382,
{ 177: } 384,
{ 178: } 386,
{ 179: } 388,
{ 180: } 390,
{ 181: } 392,
{ 182: } 394,
{ 183: } 396,
{ 184: } 398,
{ 185: } 400,
{ 186: } 402,
{ 187: } 403,
{ 188: } 404,
{ 189: } 405,
{ 190: } 407,
{ 191: } 409,
{ 192: } 411,
{ 193: } 413,
{ 194: } 415,
{ 195: } 417,
{ 196: } 419,
{ 197: } 420,
{ 198: } 422,
{ 199: } 423,
{ 200: } 424,
{ 201: } 425,
{ 202: } 426,
{ 203: } 427,
{ 204: } 428,
{ 205: } 428,
{ 206: } 428,
{ 207: } 429,
{ 208: } 430,
{ 209: } 431,
{ 210: } 432,
{ 211: } 433,
{ 212: } 435,
{ 213: } 437,
{ 214: } 439,
{ 215: } 440,
{ 216: } 442,
{ 217: } 443,
{ 218: } 445,
{ 219: } 447,
{ 220: } 449,
{ 221: } 451,
{ 222: } 453,
{ 223: } 454,
{ 224: } 455,
{ 225: } 457,
{ 226: } 459,
{ 227: } 460,
{ 228: } 462,
{ 229: } 464,
{ 230: } 465,
{ 231: } 466,
{ 232: } 467,
{ 233: } 467,
{ 234: } 467,
{ 235: } 467,
{ 236: } 468,
{ 237: } 469,
{ 238: } 470,
{ 239: } 472,
{ 240: } 474,
{ 241: } 476,
{ 242: } 477,
{ 243: } 478,
{ 244: } 479,
{ 245: } 481,
{ 246: } 483,
{ 247: } 485,
{ 248: } 486,
{ 249: } 487,
{ 250: } 489,
{ 251: } 490,
{ 252: } 491,
{ 253: } 491,
{ 254: } 491,
{ 255: } 492,
{ 256: } 494,
{ 257: } 496,
{ 258: } 498,
{ 259: } 500,
{ 260: } 502,
{ 261: } 503,
{ 262: } 504,
{ 263: } 504,
{ 264: } 505,
{ 265: } 506,
{ 266: } 508,
{ 267: } 509,
{ 268: } 510,
{ 269: } 511,
{ 270: } 512,
{ 271: } 513,
{ 272: } 514,
{ 273: } 515,
{ 274: } 516,
{ 275: } 517,
{ 276: } 518,
{ 277: } 519,
{ 278: } 520,
{ 279: } 522,
{ 280: } 524,
{ 281: } 525,
{ 282: } 526,
{ 283: } 527,
{ 284: } 528,
{ 285: } 529,
{ 286: } 530,
{ 287: } 531,
{ 288: } 532,
{ 289: } 533,
{ 290: } 534,
{ 291: } 535,
{ 292: } 536,
{ 293: } 537,
{ 294: } 538,
{ 295: } 539,
{ 296: } 539,
{ 297: } 540,
{ 298: } 541,
{ 299: } 542,
{ 300: } 543,
{ 301: } 544,
{ 302: } 545,
{ 303: } 546,
{ 304: } 547,
{ 305: } 548,
{ 306: } 549
);

yyth : array [0..yynstates-1] of Integer = (
{ 0: } 53,
{ 1: } 106,
{ 2: } 108,
{ 3: } 110,
{ 4: } 112,
{ 5: } 115,
{ 6: } 120,
{ 7: } 126,
{ 8: } 127,
{ 9: } 128,
{ 10: } 129,
{ 11: } 131,
{ 12: } 133,
{ 13: } 141,
{ 14: } 141,
{ 15: } 141,
{ 16: } 141,
{ 17: } 141,
{ 18: } 141,
{ 19: } 141,
{ 20: } 141,
{ 21: } 141,
{ 22: } 141,
{ 23: } 141,
{ 24: } 141,
{ 25: } 141,
{ 26: } 142,
{ 27: } 145,
{ 28: } 148,
{ 29: } 152,
{ 30: } 154,
{ 31: } 156,
{ 32: } 158,
{ 33: } 160,
{ 34: } 162,
{ 35: } 165,
{ 36: } 167,
{ 37: } 171,
{ 38: } 171,
{ 39: } 171,
{ 40: } 173,
{ 41: } 175,
{ 42: } 177,
{ 43: } 180,
{ 44: } 182,
{ 45: } 184,
{ 46: } 186,
{ 47: } 188,
{ 48: } 190,
{ 49: } 191,
{ 50: } 191,
{ 51: } 191,
{ 52: } 191,
{ 53: } 192,
{ 54: } 192,
{ 55: } 192,
{ 56: } 192,
{ 57: } 194,
{ 58: } 194,
{ 59: } 196,
{ 60: } 196,
{ 61: } 198,
{ 62: } 200,
{ 63: } 201,
{ 64: } 206,
{ 65: } 207,
{ 66: } 207,
{ 67: } 208,
{ 68: } 210,
{ 69: } 213,
{ 70: } 213,
{ 71: } 213,
{ 72: } 213,
{ 73: } 213,
{ 74: } 213,
{ 75: } 213,
{ 76: } 213,
{ 77: } 213,
{ 78: } 215,
{ 79: } 221,
{ 80: } 224,
{ 81: } 225,
{ 82: } 232,
{ 83: } 233,
{ 84: } 234,
{ 85: } 235,
{ 86: } 237,
{ 87: } 239,
{ 88: } 241,
{ 89: } 243,
{ 90: } 245,
{ 91: } 247,
{ 92: } 249,
{ 93: } 252,
{ 94: } 254,
{ 95: } 256,
{ 96: } 258,
{ 97: } 260,
{ 98: } 262,
{ 99: } 264,
{ 100: } 267,
{ 101: } 269,
{ 102: } 271,
{ 103: } 273,
{ 104: } 275,
{ 105: } 277,
{ 106: } 279,
{ 107: } 281,
{ 108: } 283,
{ 109: } 285,
{ 110: } 287,
{ 111: } 289,
{ 112: } 291,
{ 113: } 293,
{ 114: } 293,
{ 115: } 293,
{ 116: } 293,
{ 117: } 295,
{ 118: } 296,
{ 119: } 297,
{ 120: } 298,
{ 121: } 298,
{ 122: } 299,
{ 123: } 300,
{ 124: } 302,
{ 125: } 304,
{ 126: } 305,
{ 127: } 306,
{ 128: } 307,
{ 129: } 309,
{ 130: } 310,
{ 131: } 311,
{ 132: } 311,
{ 133: } 313,
{ 134: } 315,
{ 135: } 317,
{ 136: } 319,
{ 137: } 321,
{ 138: } 323,
{ 139: } 325,
{ 140: } 327,
{ 141: } 329,
{ 142: } 332,
{ 143: } 334,
{ 144: } 336,
{ 145: } 338,
{ 146: } 340,
{ 147: } 342,
{ 148: } 344,
{ 149: } 346,
{ 150: } 348,
{ 151: } 350,
{ 152: } 352,
{ 153: } 354,
{ 154: } 355,
{ 155: } 357,
{ 156: } 359,
{ 157: } 360,
{ 158: } 361,
{ 159: } 363,
{ 160: } 365,
{ 161: } 367,
{ 162: } 369,
{ 163: } 370,
{ 164: } 371,
{ 165: } 371,
{ 166: } 372,
{ 167: } 373,
{ 168: } 374,
{ 169: } 375,
{ 170: } 376,
{ 171: } 376,
{ 172: } 377,
{ 173: } 378,
{ 174: } 380,
{ 175: } 381,
{ 176: } 383,
{ 177: } 385,
{ 178: } 387,
{ 179: } 389,
{ 180: } 391,
{ 181: } 393,
{ 182: } 395,
{ 183: } 397,
{ 184: } 399,
{ 185: } 401,
{ 186: } 402,
{ 187: } 403,
{ 188: } 404,
{ 189: } 406,
{ 190: } 408,
{ 191: } 410,
{ 192: } 412,
{ 193: } 414,
{ 194: } 416,
{ 195: } 418,
{ 196: } 419,
{ 197: } 421,
{ 198: } 422,
{ 199: } 423,
{ 200: } 424,
{ 201: } 425,
{ 202: } 426,
{ 203: } 427,
{ 204: } 427,
{ 205: } 427,
{ 206: } 428,
{ 207: } 429,
{ 208: } 430,
{ 209: } 431,
{ 210: } 432,
{ 211: } 434,
{ 212: } 436,
{ 213: } 438,
{ 214: } 439,
{ 215: } 441,
{ 216: } 442,
{ 217: } 444,
{ 218: } 446,
{ 219: } 448,
{ 220: } 450,
{ 221: } 452,
{ 222: } 453,
{ 223: } 454,
{ 224: } 456,
{ 225: } 458,
{ 226: } 459,
{ 227: } 461,
{ 228: } 463,
{ 229: } 464,
{ 230: } 465,
{ 231: } 466,
{ 232: } 466,
{ 233: } 466,
{ 234: } 466,
{ 235: } 467,
{ 236: } 468,
{ 237: } 469,
{ 238: } 471,
{ 239: } 473,
{ 240: } 475,
{ 241: } 476,
{ 242: } 477,
{ 243: } 478,
{ 244: } 480,
{ 245: } 482,
{ 246: } 484,
{ 247: } 485,
{ 248: } 486,
{ 249: } 488,
{ 250: } 489,
{ 251: } 490,
{ 252: } 490,
{ 253: } 490,
{ 254: } 491,
{ 255: } 493,
{ 256: } 495,
{ 257: } 497,
{ 258: } 499,
{ 259: } 501,
{ 260: } 502,
{ 261: } 503,
{ 262: } 503,
{ 263: } 504,
{ 264: } 505,
{ 265: } 507,
{ 266: } 508,
{ 267: } 509,
{ 268: } 510,
{ 269: } 511,
{ 270: } 512,
{ 271: } 513,
{ 272: } 514,
{ 273: } 515,
{ 274: } 516,
{ 275: } 517,
{ 276: } 518,
{ 277: } 519,
{ 278: } 521,
{ 279: } 523,
{ 280: } 524,
{ 281: } 525,
{ 282: } 526,
{ 283: } 527,
{ 284: } 528,
{ 285: } 529,
{ 286: } 530,
{ 287: } 531,
{ 288: } 532,
{ 289: } 533,
{ 290: } 534,
{ 291: } 535,
{ 292: } 536,
{ 293: } 537,
{ 294: } 538,
{ 295: } 538,
{ 296: } 539,
{ 297: } 540,
{ 298: } 541,
{ 299: } 542,
{ 300: } 543,
{ 301: } 544,
{ 302: } 545,
{ 303: } 546,
{ 304: } 547,
{ 305: } 548,
{ 306: } 548
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
