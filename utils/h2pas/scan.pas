
(* lexical analyzer template (TP Lex V3.0), V1.0 3-2-91 AG *)

(* global definitions: *)
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

    const
       version = '0.99.15';

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

    const
       in_define : boolean = false;
       { 1 after define; 2 after the ID to print the first separating space }
       in_space_define : byte = 0;
       arglevel : longint = 0;

    function yylex : integer;
    function act_token : string;
    procedure internalerror(i : integer);

    function strpnew(const s : string) : pchar;

  implementation

    uses
       options,converu;

    const
       newline = #10;


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
                                newline :
                                  begin
                                    if not stripcomment then
                                     begin
                                       writeln(outfile);
                                       write(outfile,aktspace);
                                     end;
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
                          If not stripcomment then
                            write(outfile,aktspace,'{');
                          repeat
                            c:=get_char;
                            case c of
                              newline :
                                begin
                                  unget_char(c);
                                  if not stripcomment then
                                   writeln(outfile,' }');
                                  flush(outfile);
                                  exit;
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
                              in_define:=false;
                              in_space_define:=0;
                              return(NEW_LINE);
                            end;
                        end;
  87:
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

yynmarks   = 311;
yynmatches = 311;
yyntrans   = 551;
yynstates  = 307;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  7,
  { 1: }
  7,
  { 2: }
  25,
  87,
  { 3: }
  87,
  { 4: }
  87,
  { 5: }
  7,
  83,
  87,
  { 6: }
  7,
  9,
  87,
  { 7: }
  7,
  83,
  87,
  { 8: }
  7,
  83,
  87,
  { 9: }
  7,
  9,
  87,
  { 10: }
  11,
  87,
  { 11: }
  37,
  87,
  { 12: }
  24,
  87,
  { 13: }
  19,
  87,
  { 14: }
  20,
  87,
  { 15: }
  87,
  { 16: }
  21,
  87,
  { 17: }
  22,
  87,
  { 18: }
  23,
  87,
  { 19: }
  26,
  87,
  { 20: }
  27,
  87,
  { 21: }
  28,
  87,
  { 22: }
  29,
  87,
  { 23: }
  30,
  87,
  { 24: }
  31,
  87,
  { 25: }
  32,
  87,
  { 26: }
  33,
  87,
  { 27: }
  34,
  87,
  { 28: }
  36,
  87,
  { 29: }
  83,
  87,
  { 30: }
  83,
  87,
  { 31: }
  83,
  87,
  { 32: }
  83,
  87,
  { 33: }
  83,
  87,
  { 34: }
  83,
  87,
  { 35: }
  83,
  87,
  { 36: }
  83,
  87,
  { 37: }
  83,
  87,
  { 38: }
  7,
  83,
  87,
  { 39: }
  83,
  87,
  { 40: }
  66,
  87,
  { 41: }
  67,
  87,
  { 42: }
  83,
  87,
  { 43: }
  83,
  87,
  { 44: }
  83,
  87,
  { 45: }
  83,
  87,
  { 46: }
  83,
  87,
  { 47: }
  83,
  87,
  { 48: }
  83,
  87,
  { 49: }
  83,
  87,
  { 50: }
  83,
  87,
  { 51: }
  84,
  87,
  { 52: }
  85,
  87,
  { 53: }
  86,
  { 54: }
  87,
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
  7,
  83,
  { 70: }
  83,
  { 71: }
  8,
  { 72: }
  10,
  { 73: }
  12,
  { 74: }
  13,
  { 75: }
  14,
  { 76: }
  16,
  { 77: }
  15,
  { 78: }
  18,
  { 79: }
  17,
  { 80: }
  { 81: }
  { 82: }
  { 83: }
  { 84: }
  { 85: }
  { 86: }
  { 87: }
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
  83,
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
  83,
  { 121: }
  8,
  { 122: }
  8,
  { 123: }
  58,
  { 124: }
  { 125: }
  { 126: }
  { 127: }
  { 128: }
  { 129: }
  { 130: }
  { 131: }
  { 132: }
  { 133: }
  35,
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
  83,
  { 155: }
  69,
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
  71,
  83,
  { 164: }
  { 165: }
  { 166: }
  58,
  { 167: }
  { 168: }
  { 169: }
  { 170: }
  { 171: }
  { 172: }
  59,
  { 173: }
  { 174: }
  { 175: }
  83,
  { 176: }
  64,
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
  83,
  { 187: }
  48,
  83,
  { 188: }
  49,
  83,
  { 189: }
  62,
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
  7,
{ 1: }
  7,
{ 2: }
  25,
  87,
{ 3: }
  87,
{ 4: }
  87,
{ 5: }
  7,
  83,
  87,
{ 6: }
  7,
  9,
  87,
{ 7: }
  7,
  83,
  87,
{ 8: }
  7,
  83,
  87,
{ 9: }
  7,
  9,
  87,
{ 10: }
  11,
  87,
{ 11: }
  37,
  87,
{ 12: }
  24,
  87,
{ 13: }
  19,
  87,
{ 14: }
  20,
  87,
{ 15: }
  87,
{ 16: }
  21,
  87,
{ 17: }
  22,
  87,
{ 18: }
  23,
  87,
{ 19: }
  26,
  87,
{ 20: }
  27,
  87,
{ 21: }
  28,
  87,
{ 22: }
  29,
  87,
{ 23: }
  30,
  87,
{ 24: }
  31,
  87,
{ 25: }
  32,
  87,
{ 26: }
  33,
  87,
{ 27: }
  34,
  87,
{ 28: }
  36,
  87,
{ 29: }
  83,
  87,
{ 30: }
  83,
  87,
{ 31: }
  83,
  87,
{ 32: }
  83,
  87,
{ 33: }
  83,
  87,
{ 34: }
  83,
  87,
{ 35: }
  83,
  87,
{ 36: }
  83,
  87,
{ 37: }
  83,
  87,
{ 38: }
  7,
  83,
  87,
{ 39: }
  83,
  87,
{ 40: }
  66,
  87,
{ 41: }
  67,
  87,
{ 42: }
  83,
  87,
{ 43: }
  83,
  87,
{ 44: }
  83,
  87,
{ 45: }
  83,
  87,
{ 46: }
  83,
  87,
{ 47: }
  83,
  87,
{ 48: }
  83,
  87,
{ 49: }
  83,
  87,
{ 50: }
  83,
  87,
{ 51: }
  84,
  87,
{ 52: }
  85,
  87,
{ 53: }
  86,
{ 54: }
  87,
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
  7,
  83,
{ 70: }
  83,
{ 71: }
  8,
{ 72: }
  10,
{ 73: }
  12,
{ 74: }
  13,
{ 75: }
  14,
{ 76: }
  16,
{ 77: }
  15,
{ 78: }
  18,
{ 79: }
  17,
{ 80: }
{ 81: }
{ 82: }
{ 83: }
{ 84: }
{ 85: }
{ 86: }
{ 87: }
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
  83,
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
  83,
{ 121: }
  8,
{ 122: }
  8,
{ 123: }
  58,
{ 124: }
{ 125: }
{ 126: }
{ 127: }
{ 128: }
{ 129: }
{ 130: }
{ 131: }
{ 132: }
{ 133: }
  35,
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
  83,
{ 155: }
  69,
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
  71,
  83,
{ 164: }
{ 165: }
{ 166: }
  58,
{ 167: }
{ 168: }
{ 169: }
{ 170: }
{ 171: }
{ 172: }
  59,
{ 173: }
{ 174: }
{ 175: }
  83,
{ 176: }
  64,
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
  83,
{ 187: }
  48,
  83,
{ 188: }
  49,
  83,
{ 189: }
  62,
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
  ( cc: [ #1..#8,#11,#13..#31,'$','%','@','\','^','`',
            #127..#255 ]; s: 54),
  ( cc: [ #9,#12,' ' ]; s: 52),
  ( cc: [ #10 ]; s: 53),
  ( cc: [ '!' ]; s: 12),
  ( cc: [ '"' ]; s: 3),
  ( cc: [ '#' ]; s: 15),
  ( cc: [ '&' ]; s: 17),
  ( cc: [ '''' ]; s: 4),
  ( cc: [ '(' ]; s: 25),
  ( cc: [ ')' ]; s: 26),
  ( cc: [ '*' ]; s: 27),
  ( cc: [ '+' ]; s: 19),
  ( cc: [ ',' ]; s: 22),
  ( cc: [ '-' ]; s: 10),
  ( cc: [ '.' ]; s: 28),
  ( cc: [ '/' ]; s: 2),
  ( cc: [ '0' ]; s: 9),
  ( cc: [ '1'..'9' ]; s: 6),
  ( cc: [ ':' ]; s: 21),
  ( cc: [ ';' ]; s: 51),
  ( cc: [ '<' ]; s: 14),
  ( cc: [ '=' ]; s: 11),
  ( cc: [ '>' ]; s: 13),
  ( cc: [ '?' ]; s: 20),
  ( cc: [ 'A','B','D','G','I'..'K','M','O','Q','R',
            'T','X'..'Z','_','a','b','d','g','j','k',
            'm','o'..'r','w'..'z' ]; s: 50),
  ( cc: [ 'C' ]; s: 31),
  ( cc: [ 'E' ]; s: 34),
  ( cc: [ 'F' ]; s: 45),
  ( cc: [ 'H' ]; s: 48),
  ( cc: [ 'L' ]; s: 5),
  ( cc: [ 'N' ]; s: 46),
  ( cc: [ 'P' ]; s: 32),
  ( cc: [ 'S' ]; s: 30),
  ( cc: [ 'U' ]; s: 7),
  ( cc: [ 'V' ]; s: 36),
  ( cc: [ 'W' ]; s: 33),
  ( cc: [ '[' ]; s: 23),
  ( cc: [ ']' ]; s: 24),
  ( cc: [ 'c' ]; s: 37),
  ( cc: [ 'e' ]; s: 29),
  ( cc: [ 'f' ]; s: 44),
  ( cc: [ 'h' ]; s: 49),
  ( cc: [ 'i' ]; s: 43),
  ( cc: [ 'l' ]; s: 8),
  ( cc: [ 'n' ]; s: 47),
  ( cc: [ 's' ]; s: 39),
  ( cc: [ 't' ]; s: 42),
  ( cc: [ 'u' ]; s: 38),
  ( cc: [ 'v' ]; s: 35),
  ( cc: [ '{' ]; s: 40),
  ( cc: [ '|' ]; s: 16),
  ( cc: [ '}' ]; s: 41),
  ( cc: [ '~' ]; s: 18),
{ 1: }
  ( cc: [ #1..#8,#11,#13..#31,'$','%','@','\','^','`',
            #127..#255 ]; s: 54),
  ( cc: [ #9,#12,' ' ]; s: 52),
  ( cc: [ #10 ]; s: 53),
  ( cc: [ '!' ]; s: 12),
  ( cc: [ '"' ]; s: 3),
  ( cc: [ '#' ]; s: 15),
  ( cc: [ '&' ]; s: 17),
  ( cc: [ '''' ]; s: 4),
  ( cc: [ '(' ]; s: 25),
  ( cc: [ ')' ]; s: 26),
  ( cc: [ '*' ]; s: 27),
  ( cc: [ '+' ]; s: 19),
  ( cc: [ ',' ]; s: 22),
  ( cc: [ '-' ]; s: 10),
  ( cc: [ '.' ]; s: 28),
  ( cc: [ '/' ]; s: 2),
  ( cc: [ '0' ]; s: 9),
  ( cc: [ '1'..'9' ]; s: 6),
  ( cc: [ ':' ]; s: 21),
  ( cc: [ ';' ]; s: 51),
  ( cc: [ '<' ]; s: 14),
  ( cc: [ '=' ]; s: 11),
  ( cc: [ '>' ]; s: 13),
  ( cc: [ '?' ]; s: 20),
  ( cc: [ 'A','B','D','G','I'..'K','M','O','Q','R',
            'T','X'..'Z','_','a','b','d','g','j','k',
            'm','o'..'r','w'..'z' ]; s: 50),
  ( cc: [ 'C' ]; s: 31),
  ( cc: [ 'E' ]; s: 34),
  ( cc: [ 'F' ]; s: 45),
  ( cc: [ 'H' ]; s: 48),
  ( cc: [ 'L' ]; s: 5),
  ( cc: [ 'N' ]; s: 46),
  ( cc: [ 'P' ]; s: 32),
  ( cc: [ 'S' ]; s: 30),
  ( cc: [ 'U' ]; s: 7),
  ( cc: [ 'V' ]; s: 36),
  ( cc: [ 'W' ]; s: 33),
  ( cc: [ '[' ]; s: 23),
  ( cc: [ ']' ]; s: 24),
  ( cc: [ 'c' ]; s: 37),
  ( cc: [ 'e' ]; s: 29),
  ( cc: [ 'f' ]; s: 44),
  ( cc: [ 'h' ]; s: 49),
  ( cc: [ 'i' ]; s: 43),
  ( cc: [ 'l' ]; s: 8),
  ( cc: [ 'n' ]; s: 47),
  ( cc: [ 's' ]; s: 39),
  ( cc: [ 't' ]; s: 42),
  ( cc: [ 'u' ]; s: 38),
  ( cc: [ 'v' ]; s: 35),
  ( cc: [ '{' ]; s: 40),
  ( cc: [ '|' ]; s: 16),
  ( cc: [ '}' ]; s: 41),
  ( cc: [ '~' ]; s: 18),
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
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'k','m'..'z' ]; s: 63),
  ( cc: [ 'L','l' ]; s: 69),
{ 8: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 70),
{ 9: }
  ( cc: [ '.' ]; s: 67),
  ( cc: [ '0'..'9' ]; s: 64),
  ( cc: [ 'E','e' ]; s: 68),
  ( cc: [ 'L','l' ]; s: 66),
  ( cc: [ 'U','u' ]; s: 65),
  ( cc: [ 'x' ]; s: 71),
{ 10: }
  ( cc: [ '>' ]; s: 72),
{ 11: }
  ( cc: [ '=' ]; s: 73),
{ 12: }
  ( cc: [ '=' ]; s: 74),
{ 13: }
  ( cc: [ '=' ]; s: 75),
  ( cc: [ '>' ]; s: 76),
{ 14: }
  ( cc: [ '<' ]; s: 78),
  ( cc: [ '=' ]; s: 77),
{ 15: }
  ( cc: [ #9 ]; s: 81),
  ( cc: [ ' ' ]; s: 84),
  ( cc: [ '#' ]; s: 79),
  ( cc: [ 'd' ]; s: 86),
  ( cc: [ 'e' ]; s: 82),
  ( cc: [ 'i' ]; s: 80),
  ( cc: [ 'p' ]; s: 85),
  ( cc: [ 'u' ]; s: 83),
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
{ 27: }
{ 28: }
  ( cc: [ '.' ]; s: 87),
{ 29: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'w','y','z' ]; s: 63),
  ( cc: [ 'n' ]; s: 89),
  ( cc: [ 'x' ]; s: 88),
{ 30: }
  ( cc: [ '0'..'9','A'..'S','U'..'X','Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'T' ]; s: 90),
  ( cc: [ 'Y' ]; s: 91),
{ 31: }
  ( cc: [ '0'..'9','B','C','E'..'N','P'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 93),
  ( cc: [ 'D' ]; s: 92),
  ( cc: [ 'O' ]; s: 94),
{ 32: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 95),
{ 33: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'I' ]; s: 96),
{ 34: }
  ( cc: [ '0'..'9','A'..'W','Y','Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'X' ]; s: 97),
{ 35: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 98),
{ 36: }
  ( cc: [ '0'..'9','A'..'N','P'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'O' ]; s: 99),
{ 37: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','i'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'h' ]; s: 100),
  ( cc: [ 'o' ]; s: 101),
{ 38: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'k','m',
            'o'..'z' ]; s: 63),
  ( cc: [ 'L','l' ]; s: 69),
  ( cc: [ 'n' ]; s: 102),
{ 39: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','j'..'s','u'..'z' ]; s: 63),
  ( cc: [ 'h' ]; s: 104),
  ( cc: [ 'i' ]; s: 105),
  ( cc: [ 't' ]; s: 103),
{ 40: }
{ 41: }
{ 42: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'x','z' ]; s: 63),
  ( cc: [ 'y' ]; s: 106),
{ 43: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 107),
{ 44: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'k','m'..'z' ]; s: 63),
  ( cc: [ 'a' ]; s: 109),
  ( cc: [ 'l' ]; s: 108),
{ 45: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 110),
{ 46: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 111),
{ 47: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 112),
{ 48: }
  ( cc: [ '0'..'9','A'..'T','V'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'U' ]; s: 113),
{ 49: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 63),
  ( cc: [ 'u' ]; s: 114),
{ 50: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 51: }
{ 52: }
{ 53: }
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
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 70: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 120),
{ 71: }
  ( cc: [ '0'..'9','A'..'F','a'..'f' ]; s: 71),
  ( cc: [ 'L','l' ]; s: 122),
  ( cc: [ 'U','u' ]; s: 121),
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
{ 77: }
{ 78: }
{ 79: }
{ 80: }
  ( cc: [ 'f' ]; s: 123),
  ( cc: [ 'n' ]; s: 124),
{ 81: }
  ( cc: [ #9,' ' ]; s: 81),
  ( cc: [ 'd' ]; s: 86),
  ( cc: [ 'e' ]; s: 82),
  ( cc: [ 'i' ]; s: 125),
  ( cc: [ 'p' ]; s: 85),
  ( cc: [ 'u' ]; s: 83),
{ 82: }
  ( cc: [ 'l' ]; s: 126),
  ( cc: [ 'n' ]; s: 127),
  ( cc: [ 'r' ]; s: 128),
{ 83: }
  ( cc: [ 'n' ]; s: 129),
{ 84: }
  ( cc: [ #9,' ' ]; s: 81),
  ( cc: [ '0'..'9' ]; s: 130),
  ( cc: [ 'd' ]; s: 86),
  ( cc: [ 'e' ]; s: 82),
  ( cc: [ 'i' ]; s: 125),
  ( cc: [ 'p' ]; s: 85),
  ( cc: [ 'u' ]; s: 83),
{ 85: }
  ( cc: [ 'r' ]; s: 131),
{ 86: }
  ( cc: [ 'e' ]; s: 132),
{ 87: }
  ( cc: [ '.' ]; s: 133),
{ 88: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 134),
{ 89: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 63),
  ( cc: [ 'u' ]; s: 135),
{ 90: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'D' ]; s: 136),
{ 91: }
  ( cc: [ '0'..'9','A'..'R','T'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'S' ]; s: 137),
{ 92: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 138),
{ 93: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 139),
{ 94: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'N' ]; s: 140),
{ 95: }
  ( cc: [ '0'..'9','A','B','D'..'R','T'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 142),
  ( cc: [ 'S' ]; s: 141),
{ 96: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'N' ]; s: 143),
{ 97: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'P' ]; s: 144),
{ 98: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'z' ]; s: 63),
  ( cc: [ 'i' ]; s: 145),
{ 99: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'I' ]; s: 146),
{ 100: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 63),
  ( cc: [ 'a' ]; s: 147),
{ 101: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 148),
{ 102: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'r','t'..'z' ]; s: 63),
  ( cc: [ 'i' ]; s: 149),
  ( cc: [ 's' ]; s: 150),
{ 103: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 151),
{ 104: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 152),
{ 105: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 63),
  ( cc: [ 'g' ]; s: 153),
{ 106: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'o','q'..'z' ]; s: 63),
  ( cc: [ 'p' ]; s: 154),
{ 107: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 155),
{ 108: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 156),
{ 109: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 157),
{ 110: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'R' ]; s: 158),
{ 111: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 159),
{ 112: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 63),
  ( cc: [ 'a' ]; s: 160),
{ 113: }
  ( cc: [ '0'..'9','A'..'F','H'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'G' ]; s: 161),
{ 114: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 63),
  ( cc: [ 'g' ]; s: 162),
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
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 63),
  ( cc: [ 'g' ]; s: 163),
{ 121: }
  ( cc: [ 'L','l' ]; s: 122),
{ 122: }
{ 123: }
  ( cc: [ 'd' ]; s: 164),
{ 124: }
  ( cc: [ 'c' ]; s: 165),
{ 125: }
  ( cc: [ 'f' ]; s: 166),
  ( cc: [ 'n' ]; s: 124),
{ 126: }
  ( cc: [ 'i' ]; s: 168),
  ( cc: [ 's' ]; s: 167),
{ 127: }
  ( cc: [ 'd' ]; s: 169),
{ 128: }
  ( cc: [ 'r' ]; s: 170),
{ 129: }
  ( cc: [ 'd' ]; s: 171),
{ 130: }
  ( cc: [ ' ' ]; s: 172),
  ( cc: [ '0'..'9' ]; s: 130),
{ 131: }
  ( cc: [ 'a' ]; s: 173),
{ 132: }
  ( cc: [ 'f' ]; s: 174),
{ 133: }
{ 134: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 175),
{ 135: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'l','n'..'z' ]; s: 63),
  ( cc: [ 'm' ]; s: 176),
{ 136: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 177),
{ 137: }
  ( cc: [ '0'..'9','A'..'Z','a'..'z' ]; s: 63),
  ( cc: [ '_' ]; s: 178),
{ 138: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 179),
{ 139: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 180),
{ 140: }
  ( cc: [ '0'..'9','A'..'R','T'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'S' ]; s: 181),
{ 141: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 182),
{ 142: }
  ( cc: [ '0'..'9','A'..'J','L'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'K' ]; s: 183),
{ 143: }
  ( cc: [ '0'..'9','B'..'F','H'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 184),
  ( cc: [ 'G' ]; s: 185),
{ 144: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 186),
{ 145: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 63),
  ( cc: [ 'd' ]; s: 187),
{ 146: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'D' ]; s: 188),
{ 147: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 189),
{ 148: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'r','t'..'z' ]; s: 63),
  ( cc: [ 's' ]; s: 190),
{ 149: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 191),
{ 150: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'z' ]; s: 63),
  ( cc: [ 'i' ]; s: 192),
{ 151: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 63),
  ( cc: [ 'u' ]; s: 193),
{ 152: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 194),
{ 153: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 195),
{ 154: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 196),
{ 155: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
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
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 164: }
  ( cc: [ 'e' ]; s: 202),
{ 165: }
  ( cc: [ 'l' ]; s: 203),
{ 166: }
{ 167: }
  ( cc: [ 'e' ]; s: 204),
{ 168: }
  ( cc: [ 'f' ]; s: 205),
{ 169: }
  ( cc: [ 'i' ]; s: 206),
{ 170: }
  ( cc: [ 'o' ]; s: 207),
{ 171: }
  ( cc: [ 'e' ]; s: 208),
{ 172: }
{ 173: }
  ( cc: [ 'g' ]; s: 209),
{ 174: }
  ( cc: [ 'i' ]; s: 210),
{ 175: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 211),
{ 176: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 177: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 212),
{ 178: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'T' ]; s: 213),
{ 179: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 214),
{ 180: }
  ( cc: [ '0'..'9','A','C'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'B' ]; s: 215),
{ 181: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'T' ]; s: 216),
{ 182: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 217),
{ 183: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 218),
{ 184: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'P' ]; s: 219),
{ 185: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'D' ]; s: 220),
{ 186: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'N' ]; s: 221),
{ 187: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 188: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 189: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 190: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 222),
{ 191: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 223),
{ 192: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 63),
  ( cc: [ 'g' ]; s: 224),
{ 193: }
  ( cc: [ '0'..'9','A'..'Z','_','a','b','d'..'z' ]; s: 63),
  ( cc: [ 'c' ]; s: 225),
{ 194: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 226),
{ 195: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 227),
{ 196: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 63),
  ( cc: [ 'd' ]; s: 228),
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
{ 1: } 2,
{ 2: } 3,
{ 3: } 5,
{ 4: } 6,
{ 5: } 7,
{ 6: } 10,
{ 7: } 13,
{ 8: } 16,
{ 9: } 19,
{ 10: } 22,
{ 11: } 24,
{ 12: } 26,
{ 13: } 28,
{ 14: } 30,
{ 15: } 32,
{ 16: } 33,
{ 17: } 35,
{ 18: } 37,
{ 19: } 39,
{ 20: } 41,
{ 21: } 43,
{ 22: } 45,
{ 23: } 47,
{ 24: } 49,
{ 25: } 51,
{ 26: } 53,
{ 27: } 55,
{ 28: } 57,
{ 29: } 59,
{ 30: } 61,
{ 31: } 63,
{ 32: } 65,
{ 33: } 67,
{ 34: } 69,
{ 35: } 71,
{ 36: } 73,
{ 37: } 75,
{ 38: } 77,
{ 39: } 80,
{ 40: } 82,
{ 41: } 84,
{ 42: } 86,
{ 43: } 88,
{ 44: } 90,
{ 45: } 92,
{ 46: } 94,
{ 47: } 96,
{ 48: } 98,
{ 49: } 100,
{ 50: } 102,
{ 51: } 104,
{ 52: } 106,
{ 53: } 108,
{ 54: } 109,
{ 55: } 110,
{ 56: } 111,
{ 57: } 112,
{ 58: } 112,
{ 59: } 113,
{ 60: } 113,
{ 61: } 114,
{ 62: } 114,
{ 63: } 114,
{ 64: } 115,
{ 65: } 117,
{ 66: } 118,
{ 67: } 119,
{ 68: } 119,
{ 69: } 119,
{ 70: } 121,
{ 71: } 122,
{ 72: } 123,
{ 73: } 124,
{ 74: } 125,
{ 75: } 126,
{ 76: } 127,
{ 77: } 128,
{ 78: } 129,
{ 79: } 130,
{ 80: } 131,
{ 81: } 131,
{ 82: } 131,
{ 83: } 131,
{ 84: } 131,
{ 85: } 131,
{ 86: } 131,
{ 87: } 131,
{ 88: } 131,
{ 89: } 132,
{ 90: } 133,
{ 91: } 134,
{ 92: } 135,
{ 93: } 136,
{ 94: } 137,
{ 95: } 138,
{ 96: } 139,
{ 97: } 140,
{ 98: } 141,
{ 99: } 142,
{ 100: } 143,
{ 101: } 144,
{ 102: } 145,
{ 103: } 146,
{ 104: } 147,
{ 105: } 148,
{ 106: } 149,
{ 107: } 150,
{ 108: } 151,
{ 109: } 152,
{ 110: } 153,
{ 111: } 154,
{ 112: } 155,
{ 113: } 156,
{ 114: } 157,
{ 115: } 158,
{ 116: } 159,
{ 117: } 160,
{ 118: } 161,
{ 119: } 161,
{ 120: } 162,
{ 121: } 163,
{ 122: } 164,
{ 123: } 165,
{ 124: } 166,
{ 125: } 166,
{ 126: } 166,
{ 127: } 166,
{ 128: } 166,
{ 129: } 166,
{ 130: } 166,
{ 131: } 166,
{ 132: } 166,
{ 133: } 166,
{ 134: } 167,
{ 135: } 168,
{ 136: } 169,
{ 137: } 170,
{ 138: } 171,
{ 139: } 172,
{ 140: } 173,
{ 141: } 174,
{ 142: } 175,
{ 143: } 176,
{ 144: } 177,
{ 145: } 178,
{ 146: } 179,
{ 147: } 180,
{ 148: } 181,
{ 149: } 182,
{ 150: } 183,
{ 151: } 184,
{ 152: } 185,
{ 153: } 186,
{ 154: } 187,
{ 155: } 188,
{ 156: } 190,
{ 157: } 191,
{ 158: } 193,
{ 159: } 195,
{ 160: } 196,
{ 161: } 197,
{ 162: } 198,
{ 163: } 199,
{ 164: } 201,
{ 165: } 201,
{ 166: } 201,
{ 167: } 202,
{ 168: } 202,
{ 169: } 202,
{ 170: } 202,
{ 171: } 202,
{ 172: } 202,
{ 173: } 203,
{ 174: } 203,
{ 175: } 203,
{ 176: } 204,
{ 177: } 206,
{ 178: } 207,
{ 179: } 208,
{ 180: } 209,
{ 181: } 210,
{ 182: } 211,
{ 183: } 212,
{ 184: } 213,
{ 185: } 214,
{ 186: } 215,
{ 187: } 216,
{ 188: } 218,
{ 189: } 220,
{ 190: } 222,
{ 191: } 223,
{ 192: } 224,
{ 193: } 225,
{ 194: } 226,
{ 195: } 227,
{ 196: } 228,
{ 197: } 229,
{ 198: } 230,
{ 199: } 232,
{ 200: } 234,
{ 201: } 236,
{ 202: } 238,
{ 203: } 238,
{ 204: } 238,
{ 205: } 239,
{ 206: } 240,
{ 207: } 240,
{ 208: } 240,
{ 209: } 240,
{ 210: } 240,
{ 211: } 240,
{ 212: } 241,
{ 213: } 242,
{ 214: } 243,
{ 215: } 245,
{ 216: } 246,
{ 217: } 248,
{ 218: } 249,
{ 219: } 250,
{ 220: } 251,
{ 221: } 252,
{ 222: } 253,
{ 223: } 255,
{ 224: } 257,
{ 225: } 258,
{ 226: } 259,
{ 227: } 261,
{ 228: } 262,
{ 229: } 263,
{ 230: } 265,
{ 231: } 265,
{ 232: } 265,
{ 233: } 266,
{ 234: } 267,
{ 235: } 268,
{ 236: } 268,
{ 237: } 268,
{ 238: } 270,
{ 239: } 271,
{ 240: } 272,
{ 241: } 273,
{ 242: } 275,
{ 243: } 277,
{ 244: } 279,
{ 245: } 280,
{ 246: } 281,
{ 247: } 282,
{ 248: } 284,
{ 249: } 286,
{ 250: } 287,
{ 251: } 287,
{ 252: } 287,
{ 253: } 288,
{ 254: } 289,
{ 255: } 291,
{ 256: } 292,
{ 257: } 293,
{ 258: } 294,
{ 259: } 295,
{ 260: } 296,
{ 261: } 298,
{ 262: } 298,
{ 263: } 299,
{ 264: } 301,
{ 265: } 303,
{ 266: } 304,
{ 267: } 306,
{ 268: } 308,
{ 269: } 308,
{ 270: } 310,
{ 271: } 310,
{ 272: } 310,
{ 273: } 310,
{ 274: } 310,
{ 275: } 310,
{ 276: } 310,
{ 277: } 310,
{ 278: } 310,
{ 279: } 310,
{ 280: } 310,
{ 281: } 310,
{ 282: } 310,
{ 283: } 310,
{ 284: } 310,
{ 285: } 310,
{ 286: } 310,
{ 287: } 310,
{ 288: } 310,
{ 289: } 310,
{ 290: } 310,
{ 291: } 310,
{ 292: } 310,
{ 293: } 310,
{ 294: } 310,
{ 295: } 310,
{ 296: } 311,
{ 297: } 311,
{ 298: } 311,
{ 299: } 311,
{ 300: } 311,
{ 301: } 311,
{ 302: } 311,
{ 303: } 311,
{ 304: } 311,
{ 305: } 311,
{ 306: } 311
);

yykh : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 4,
{ 3: } 5,
{ 4: } 6,
{ 5: } 9,
{ 6: } 12,
{ 7: } 15,
{ 8: } 18,
{ 9: } 21,
{ 10: } 23,
{ 11: } 25,
{ 12: } 27,
{ 13: } 29,
{ 14: } 31,
{ 15: } 32,
{ 16: } 34,
{ 17: } 36,
{ 18: } 38,
{ 19: } 40,
{ 20: } 42,
{ 21: } 44,
{ 22: } 46,
{ 23: } 48,
{ 24: } 50,
{ 25: } 52,
{ 26: } 54,
{ 27: } 56,
{ 28: } 58,
{ 29: } 60,
{ 30: } 62,
{ 31: } 64,
{ 32: } 66,
{ 33: } 68,
{ 34: } 70,
{ 35: } 72,
{ 36: } 74,
{ 37: } 76,
{ 38: } 79,
{ 39: } 81,
{ 40: } 83,
{ 41: } 85,
{ 42: } 87,
{ 43: } 89,
{ 44: } 91,
{ 45: } 93,
{ 46: } 95,
{ 47: } 97,
{ 48: } 99,
{ 49: } 101,
{ 50: } 103,
{ 51: } 105,
{ 52: } 107,
{ 53: } 108,
{ 54: } 109,
{ 55: } 110,
{ 56: } 111,
{ 57: } 111,
{ 58: } 112,
{ 59: } 112,
{ 60: } 113,
{ 61: } 113,
{ 62: } 113,
{ 63: } 114,
{ 64: } 116,
{ 65: } 117,
{ 66: } 118,
{ 67: } 118,
{ 68: } 118,
{ 69: } 120,
{ 70: } 121,
{ 71: } 122,
{ 72: } 123,
{ 73: } 124,
{ 74: } 125,
{ 75: } 126,
{ 76: } 127,
{ 77: } 128,
{ 78: } 129,
{ 79: } 130,
{ 80: } 130,
{ 81: } 130,
{ 82: } 130,
{ 83: } 130,
{ 84: } 130,
{ 85: } 130,
{ 86: } 130,
{ 87: } 130,
{ 88: } 131,
{ 89: } 132,
{ 90: } 133,
{ 91: } 134,
{ 92: } 135,
{ 93: } 136,
{ 94: } 137,
{ 95: } 138,
{ 96: } 139,
{ 97: } 140,
{ 98: } 141,
{ 99: } 142,
{ 100: } 143,
{ 101: } 144,
{ 102: } 145,
{ 103: } 146,
{ 104: } 147,
{ 105: } 148,
{ 106: } 149,
{ 107: } 150,
{ 108: } 151,
{ 109: } 152,
{ 110: } 153,
{ 111: } 154,
{ 112: } 155,
{ 113: } 156,
{ 114: } 157,
{ 115: } 158,
{ 116: } 159,
{ 117: } 160,
{ 118: } 160,
{ 119: } 161,
{ 120: } 162,
{ 121: } 163,
{ 122: } 164,
{ 123: } 165,
{ 124: } 165,
{ 125: } 165,
{ 126: } 165,
{ 127: } 165,
{ 128: } 165,
{ 129: } 165,
{ 130: } 165,
{ 131: } 165,
{ 132: } 165,
{ 133: } 166,
{ 134: } 167,
{ 135: } 168,
{ 136: } 169,
{ 137: } 170,
{ 138: } 171,
{ 139: } 172,
{ 140: } 173,
{ 141: } 174,
{ 142: } 175,
{ 143: } 176,
{ 144: } 177,
{ 145: } 178,
{ 146: } 179,
{ 147: } 180,
{ 148: } 181,
{ 149: } 182,
{ 150: } 183,
{ 151: } 184,
{ 152: } 185,
{ 153: } 186,
{ 154: } 187,
{ 155: } 189,
{ 156: } 190,
{ 157: } 192,
{ 158: } 194,
{ 159: } 195,
{ 160: } 196,
{ 161: } 197,
{ 162: } 198,
{ 163: } 200,
{ 164: } 200,
{ 165: } 200,
{ 166: } 201,
{ 167: } 201,
{ 168: } 201,
{ 169: } 201,
{ 170: } 201,
{ 171: } 201,
{ 172: } 202,
{ 173: } 202,
{ 174: } 202,
{ 175: } 203,
{ 176: } 205,
{ 177: } 206,
{ 178: } 207,
{ 179: } 208,
{ 180: } 209,
{ 181: } 210,
{ 182: } 211,
{ 183: } 212,
{ 184: } 213,
{ 185: } 214,
{ 186: } 215,
{ 187: } 217,
{ 188: } 219,
{ 189: } 221,
{ 190: } 222,
{ 191: } 223,
{ 192: } 224,
{ 193: } 225,
{ 194: } 226,
{ 195: } 227,
{ 196: } 228,
{ 197: } 229,
{ 198: } 231,
{ 199: } 233,
{ 200: } 235,
{ 201: } 237,
{ 202: } 237,
{ 203: } 237,
{ 204: } 238,
{ 205: } 239,
{ 206: } 239,
{ 207: } 239,
{ 208: } 239,
{ 209: } 239,
{ 210: } 239,
{ 211: } 240,
{ 212: } 241,
{ 213: } 242,
{ 214: } 244,
{ 215: } 245,
{ 216: } 247,
{ 217: } 248,
{ 218: } 249,
{ 219: } 250,
{ 220: } 251,
{ 221: } 252,
{ 222: } 254,
{ 223: } 256,
{ 224: } 257,
{ 225: } 258,
{ 226: } 260,
{ 227: } 261,
{ 228: } 262,
{ 229: } 264,
{ 230: } 264,
{ 231: } 264,
{ 232: } 265,
{ 233: } 266,
{ 234: } 267,
{ 235: } 267,
{ 236: } 267,
{ 237: } 269,
{ 238: } 270,
{ 239: } 271,
{ 240: } 272,
{ 241: } 274,
{ 242: } 276,
{ 243: } 278,
{ 244: } 279,
{ 245: } 280,
{ 246: } 281,
{ 247: } 283,
{ 248: } 285,
{ 249: } 286,
{ 250: } 286,
{ 251: } 286,
{ 252: } 287,
{ 253: } 288,
{ 254: } 290,
{ 255: } 291,
{ 256: } 292,
{ 257: } 293,
{ 258: } 294,
{ 259: } 295,
{ 260: } 297,
{ 261: } 297,
{ 262: } 298,
{ 263: } 300,
{ 264: } 302,
{ 265: } 303,
{ 266: } 305,
{ 267: } 307,
{ 268: } 307,
{ 269: } 309,
{ 270: } 309,
{ 271: } 309,
{ 272: } 309,
{ 273: } 309,
{ 274: } 309,
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
{ 295: } 310,
{ 296: } 310,
{ 297: } 310,
{ 298: } 310,
{ 299: } 310,
{ 300: } 310,
{ 301: } 310,
{ 302: } 310,
{ 303: } 310,
{ 304: } 310,
{ 305: } 310,
{ 306: } 311
);

yyml : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 3,
{ 3: } 5,
{ 4: } 6,
{ 5: } 7,
{ 6: } 10,
{ 7: } 13,
{ 8: } 16,
{ 9: } 19,
{ 10: } 22,
{ 11: } 24,
{ 12: } 26,
{ 13: } 28,
{ 14: } 30,
{ 15: } 32,
{ 16: } 33,
{ 17: } 35,
{ 18: } 37,
{ 19: } 39,
{ 20: } 41,
{ 21: } 43,
{ 22: } 45,
{ 23: } 47,
{ 24: } 49,
{ 25: } 51,
{ 26: } 53,
{ 27: } 55,
{ 28: } 57,
{ 29: } 59,
{ 30: } 61,
{ 31: } 63,
{ 32: } 65,
{ 33: } 67,
{ 34: } 69,
{ 35: } 71,
{ 36: } 73,
{ 37: } 75,
{ 38: } 77,
{ 39: } 80,
{ 40: } 82,
{ 41: } 84,
{ 42: } 86,
{ 43: } 88,
{ 44: } 90,
{ 45: } 92,
{ 46: } 94,
{ 47: } 96,
{ 48: } 98,
{ 49: } 100,
{ 50: } 102,
{ 51: } 104,
{ 52: } 106,
{ 53: } 108,
{ 54: } 109,
{ 55: } 110,
{ 56: } 111,
{ 57: } 112,
{ 58: } 112,
{ 59: } 113,
{ 60: } 113,
{ 61: } 114,
{ 62: } 114,
{ 63: } 114,
{ 64: } 115,
{ 65: } 117,
{ 66: } 118,
{ 67: } 119,
{ 68: } 119,
{ 69: } 119,
{ 70: } 121,
{ 71: } 122,
{ 72: } 123,
{ 73: } 124,
{ 74: } 125,
{ 75: } 126,
{ 76: } 127,
{ 77: } 128,
{ 78: } 129,
{ 79: } 130,
{ 80: } 131,
{ 81: } 131,
{ 82: } 131,
{ 83: } 131,
{ 84: } 131,
{ 85: } 131,
{ 86: } 131,
{ 87: } 131,
{ 88: } 131,
{ 89: } 132,
{ 90: } 133,
{ 91: } 134,
{ 92: } 135,
{ 93: } 136,
{ 94: } 137,
{ 95: } 138,
{ 96: } 139,
{ 97: } 140,
{ 98: } 141,
{ 99: } 142,
{ 100: } 143,
{ 101: } 144,
{ 102: } 145,
{ 103: } 146,
{ 104: } 147,
{ 105: } 148,
{ 106: } 149,
{ 107: } 150,
{ 108: } 151,
{ 109: } 152,
{ 110: } 153,
{ 111: } 154,
{ 112: } 155,
{ 113: } 156,
{ 114: } 157,
{ 115: } 158,
{ 116: } 159,
{ 117: } 160,
{ 118: } 161,
{ 119: } 161,
{ 120: } 162,
{ 121: } 163,
{ 122: } 164,
{ 123: } 165,
{ 124: } 166,
{ 125: } 166,
{ 126: } 166,
{ 127: } 166,
{ 128: } 166,
{ 129: } 166,
{ 130: } 166,
{ 131: } 166,
{ 132: } 166,
{ 133: } 166,
{ 134: } 167,
{ 135: } 168,
{ 136: } 169,
{ 137: } 170,
{ 138: } 171,
{ 139: } 172,
{ 140: } 173,
{ 141: } 174,
{ 142: } 175,
{ 143: } 176,
{ 144: } 177,
{ 145: } 178,
{ 146: } 179,
{ 147: } 180,
{ 148: } 181,
{ 149: } 182,
{ 150: } 183,
{ 151: } 184,
{ 152: } 185,
{ 153: } 186,
{ 154: } 187,
{ 155: } 188,
{ 156: } 190,
{ 157: } 191,
{ 158: } 193,
{ 159: } 195,
{ 160: } 196,
{ 161: } 197,
{ 162: } 198,
{ 163: } 199,
{ 164: } 201,
{ 165: } 201,
{ 166: } 201,
{ 167: } 202,
{ 168: } 202,
{ 169: } 202,
{ 170: } 202,
{ 171: } 202,
{ 172: } 202,
{ 173: } 203,
{ 174: } 203,
{ 175: } 203,
{ 176: } 204,
{ 177: } 206,
{ 178: } 207,
{ 179: } 208,
{ 180: } 209,
{ 181: } 210,
{ 182: } 211,
{ 183: } 212,
{ 184: } 213,
{ 185: } 214,
{ 186: } 215,
{ 187: } 216,
{ 188: } 218,
{ 189: } 220,
{ 190: } 222,
{ 191: } 223,
{ 192: } 224,
{ 193: } 225,
{ 194: } 226,
{ 195: } 227,
{ 196: } 228,
{ 197: } 229,
{ 198: } 230,
{ 199: } 232,
{ 200: } 234,
{ 201: } 236,
{ 202: } 238,
{ 203: } 238,
{ 204: } 238,
{ 205: } 239,
{ 206: } 240,
{ 207: } 240,
{ 208: } 240,
{ 209: } 240,
{ 210: } 240,
{ 211: } 240,
{ 212: } 241,
{ 213: } 242,
{ 214: } 243,
{ 215: } 245,
{ 216: } 246,
{ 217: } 248,
{ 218: } 249,
{ 219: } 250,
{ 220: } 251,
{ 221: } 252,
{ 222: } 253,
{ 223: } 255,
{ 224: } 257,
{ 225: } 258,
{ 226: } 259,
{ 227: } 261,
{ 228: } 262,
{ 229: } 263,
{ 230: } 265,
{ 231: } 265,
{ 232: } 265,
{ 233: } 266,
{ 234: } 267,
{ 235: } 268,
{ 236: } 268,
{ 237: } 268,
{ 238: } 270,
{ 239: } 271,
{ 240: } 272,
{ 241: } 273,
{ 242: } 275,
{ 243: } 277,
{ 244: } 279,
{ 245: } 280,
{ 246: } 281,
{ 247: } 282,
{ 248: } 284,
{ 249: } 286,
{ 250: } 287,
{ 251: } 287,
{ 252: } 287,
{ 253: } 288,
{ 254: } 289,
{ 255: } 291,
{ 256: } 292,
{ 257: } 293,
{ 258: } 294,
{ 259: } 295,
{ 260: } 296,
{ 261: } 298,
{ 262: } 298,
{ 263: } 299,
{ 264: } 301,
{ 265: } 303,
{ 266: } 304,
{ 267: } 306,
{ 268: } 308,
{ 269: } 308,
{ 270: } 310,
{ 271: } 310,
{ 272: } 310,
{ 273: } 310,
{ 274: } 310,
{ 275: } 310,
{ 276: } 310,
{ 277: } 310,
{ 278: } 310,
{ 279: } 310,
{ 280: } 310,
{ 281: } 310,
{ 282: } 310,
{ 283: } 310,
{ 284: } 310,
{ 285: } 310,
{ 286: } 310,
{ 287: } 310,
{ 288: } 310,
{ 289: } 310,
{ 290: } 310,
{ 291: } 310,
{ 292: } 310,
{ 293: } 310,
{ 294: } 310,
{ 295: } 310,
{ 296: } 311,
{ 297: } 311,
{ 298: } 311,
{ 299: } 311,
{ 300: } 311,
{ 301: } 311,
{ 302: } 311,
{ 303: } 311,
{ 304: } 311,
{ 305: } 311,
{ 306: } 311
);

yymh : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 4,
{ 3: } 5,
{ 4: } 6,
{ 5: } 9,
{ 6: } 12,
{ 7: } 15,
{ 8: } 18,
{ 9: } 21,
{ 10: } 23,
{ 11: } 25,
{ 12: } 27,
{ 13: } 29,
{ 14: } 31,
{ 15: } 32,
{ 16: } 34,
{ 17: } 36,
{ 18: } 38,
{ 19: } 40,
{ 20: } 42,
{ 21: } 44,
{ 22: } 46,
{ 23: } 48,
{ 24: } 50,
{ 25: } 52,
{ 26: } 54,
{ 27: } 56,
{ 28: } 58,
{ 29: } 60,
{ 30: } 62,
{ 31: } 64,
{ 32: } 66,
{ 33: } 68,
{ 34: } 70,
{ 35: } 72,
{ 36: } 74,
{ 37: } 76,
{ 38: } 79,
{ 39: } 81,
{ 40: } 83,
{ 41: } 85,
{ 42: } 87,
{ 43: } 89,
{ 44: } 91,
{ 45: } 93,
{ 46: } 95,
{ 47: } 97,
{ 48: } 99,
{ 49: } 101,
{ 50: } 103,
{ 51: } 105,
{ 52: } 107,
{ 53: } 108,
{ 54: } 109,
{ 55: } 110,
{ 56: } 111,
{ 57: } 111,
{ 58: } 112,
{ 59: } 112,
{ 60: } 113,
{ 61: } 113,
{ 62: } 113,
{ 63: } 114,
{ 64: } 116,
{ 65: } 117,
{ 66: } 118,
{ 67: } 118,
{ 68: } 118,
{ 69: } 120,
{ 70: } 121,
{ 71: } 122,
{ 72: } 123,
{ 73: } 124,
{ 74: } 125,
{ 75: } 126,
{ 76: } 127,
{ 77: } 128,
{ 78: } 129,
{ 79: } 130,
{ 80: } 130,
{ 81: } 130,
{ 82: } 130,
{ 83: } 130,
{ 84: } 130,
{ 85: } 130,
{ 86: } 130,
{ 87: } 130,
{ 88: } 131,
{ 89: } 132,
{ 90: } 133,
{ 91: } 134,
{ 92: } 135,
{ 93: } 136,
{ 94: } 137,
{ 95: } 138,
{ 96: } 139,
{ 97: } 140,
{ 98: } 141,
{ 99: } 142,
{ 100: } 143,
{ 101: } 144,
{ 102: } 145,
{ 103: } 146,
{ 104: } 147,
{ 105: } 148,
{ 106: } 149,
{ 107: } 150,
{ 108: } 151,
{ 109: } 152,
{ 110: } 153,
{ 111: } 154,
{ 112: } 155,
{ 113: } 156,
{ 114: } 157,
{ 115: } 158,
{ 116: } 159,
{ 117: } 160,
{ 118: } 160,
{ 119: } 161,
{ 120: } 162,
{ 121: } 163,
{ 122: } 164,
{ 123: } 165,
{ 124: } 165,
{ 125: } 165,
{ 126: } 165,
{ 127: } 165,
{ 128: } 165,
{ 129: } 165,
{ 130: } 165,
{ 131: } 165,
{ 132: } 165,
{ 133: } 166,
{ 134: } 167,
{ 135: } 168,
{ 136: } 169,
{ 137: } 170,
{ 138: } 171,
{ 139: } 172,
{ 140: } 173,
{ 141: } 174,
{ 142: } 175,
{ 143: } 176,
{ 144: } 177,
{ 145: } 178,
{ 146: } 179,
{ 147: } 180,
{ 148: } 181,
{ 149: } 182,
{ 150: } 183,
{ 151: } 184,
{ 152: } 185,
{ 153: } 186,
{ 154: } 187,
{ 155: } 189,
{ 156: } 190,
{ 157: } 192,
{ 158: } 194,
{ 159: } 195,
{ 160: } 196,
{ 161: } 197,
{ 162: } 198,
{ 163: } 200,
{ 164: } 200,
{ 165: } 200,
{ 166: } 201,
{ 167: } 201,
{ 168: } 201,
{ 169: } 201,
{ 170: } 201,
{ 171: } 201,
{ 172: } 202,
{ 173: } 202,
{ 174: } 202,
{ 175: } 203,
{ 176: } 205,
{ 177: } 206,
{ 178: } 207,
{ 179: } 208,
{ 180: } 209,
{ 181: } 210,
{ 182: } 211,
{ 183: } 212,
{ 184: } 213,
{ 185: } 214,
{ 186: } 215,
{ 187: } 217,
{ 188: } 219,
{ 189: } 221,
{ 190: } 222,
{ 191: } 223,
{ 192: } 224,
{ 193: } 225,
{ 194: } 226,
{ 195: } 227,
{ 196: } 228,
{ 197: } 229,
{ 198: } 231,
{ 199: } 233,
{ 200: } 235,
{ 201: } 237,
{ 202: } 237,
{ 203: } 237,
{ 204: } 238,
{ 205: } 239,
{ 206: } 239,
{ 207: } 239,
{ 208: } 239,
{ 209: } 239,
{ 210: } 239,
{ 211: } 240,
{ 212: } 241,
{ 213: } 242,
{ 214: } 244,
{ 215: } 245,
{ 216: } 247,
{ 217: } 248,
{ 218: } 249,
{ 219: } 250,
{ 220: } 251,
{ 221: } 252,
{ 222: } 254,
{ 223: } 256,
{ 224: } 257,
{ 225: } 258,
{ 226: } 260,
{ 227: } 261,
{ 228: } 262,
{ 229: } 264,
{ 230: } 264,
{ 231: } 264,
{ 232: } 265,
{ 233: } 266,
{ 234: } 267,
{ 235: } 267,
{ 236: } 267,
{ 237: } 269,
{ 238: } 270,
{ 239: } 271,
{ 240: } 272,
{ 241: } 274,
{ 242: } 276,
{ 243: } 278,
{ 244: } 279,
{ 245: } 280,
{ 246: } 281,
{ 247: } 283,
{ 248: } 285,
{ 249: } 286,
{ 250: } 286,
{ 251: } 286,
{ 252: } 287,
{ 253: } 288,
{ 254: } 290,
{ 255: } 291,
{ 256: } 292,
{ 257: } 293,
{ 258: } 294,
{ 259: } 295,
{ 260: } 297,
{ 261: } 297,
{ 262: } 298,
{ 263: } 300,
{ 264: } 302,
{ 265: } 303,
{ 266: } 305,
{ 267: } 307,
{ 268: } 307,
{ 269: } 309,
{ 270: } 309,
{ 271: } 309,
{ 272: } 309,
{ 273: } 309,
{ 274: } 309,
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
{ 295: } 310,
{ 296: } 310,
{ 297: } 310,
{ 298: } 310,
{ 299: } 310,
{ 300: } 310,
{ 301: } 310,
{ 302: } 310,
{ 303: } 310,
{ 304: } 310,
{ 305: } 310,
{ 306: } 311
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
{ 8: } 123,
{ 9: } 125,
{ 10: } 131,
{ 11: } 132,
{ 12: } 133,
{ 13: } 134,
{ 14: } 136,
{ 15: } 138,
{ 16: } 146,
{ 17: } 146,
{ 18: } 146,
{ 19: } 146,
{ 20: } 146,
{ 21: } 146,
{ 22: } 146,
{ 23: } 146,
{ 24: } 146,
{ 25: } 146,
{ 26: } 146,
{ 27: } 146,
{ 28: } 146,
{ 29: } 147,
{ 30: } 150,
{ 31: } 153,
{ 32: } 157,
{ 33: } 159,
{ 34: } 161,
{ 35: } 163,
{ 36: } 165,
{ 37: } 167,
{ 38: } 170,
{ 39: } 173,
{ 40: } 177,
{ 41: } 177,
{ 42: } 177,
{ 43: } 179,
{ 44: } 181,
{ 45: } 184,
{ 46: } 186,
{ 47: } 188,
{ 48: } 190,
{ 49: } 192,
{ 50: } 194,
{ 51: } 195,
{ 52: } 195,
{ 53: } 195,
{ 54: } 195,
{ 55: } 195,
{ 56: } 195,
{ 57: } 195,
{ 58: } 197,
{ 59: } 197,
{ 60: } 199,
{ 61: } 199,
{ 62: } 201,
{ 63: } 203,
{ 64: } 204,
{ 65: } 209,
{ 66: } 210,
{ 67: } 210,
{ 68: } 211,
{ 69: } 213,
{ 70: } 214,
{ 71: } 216,
{ 72: } 219,
{ 73: } 219,
{ 74: } 219,
{ 75: } 219,
{ 76: } 219,
{ 77: } 219,
{ 78: } 219,
{ 79: } 219,
{ 80: } 219,
{ 81: } 221,
{ 82: } 227,
{ 83: } 230,
{ 84: } 231,
{ 85: } 238,
{ 86: } 239,
{ 87: } 240,
{ 88: } 241,
{ 89: } 243,
{ 90: } 245,
{ 91: } 247,
{ 92: } 249,
{ 93: } 251,
{ 94: } 253,
{ 95: } 255,
{ 96: } 258,
{ 97: } 260,
{ 98: } 262,
{ 99: } 264,
{ 100: } 266,
{ 101: } 268,
{ 102: } 270,
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
{ 114: } 295,
{ 115: } 297,
{ 116: } 297,
{ 117: } 297,
{ 118: } 299,
{ 119: } 300,
{ 120: } 301,
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
{ 158: } 364,
{ 159: } 365,
{ 160: } 367,
{ 161: } 369,
{ 162: } 371,
{ 163: } 373,
{ 164: } 374,
{ 165: } 375,
{ 166: } 376,
{ 167: } 376,
{ 168: } 377,
{ 169: } 378,
{ 170: } 379,
{ 171: } 380,
{ 172: } 381,
{ 173: } 381,
{ 174: } 382,
{ 175: } 383,
{ 176: } 385,
{ 177: } 386,
{ 178: } 388,
{ 179: } 390,
{ 180: } 392,
{ 181: } 394,
{ 182: } 396,
{ 183: } 398,
{ 184: } 400,
{ 185: } 402,
{ 186: } 404,
{ 187: } 406,
{ 188: } 407,
{ 189: } 408,
{ 190: } 409,
{ 191: } 411,
{ 192: } 413,
{ 193: } 415,
{ 194: } 417,
{ 195: } 419,
{ 196: } 421,
{ 197: } 423,
{ 198: } 425,
{ 199: } 426,
{ 200: } 427,
{ 201: } 428,
{ 202: } 429,
{ 203: } 430,
{ 204: } 431,
{ 205: } 431,
{ 206: } 431,
{ 207: } 432,
{ 208: } 433,
{ 209: } 434,
{ 210: } 435,
{ 211: } 436,
{ 212: } 438,
{ 213: } 440,
{ 214: } 442,
{ 215: } 443,
{ 216: } 445,
{ 217: } 446,
{ 218: } 448,
{ 219: } 450,
{ 220: } 452,
{ 221: } 454,
{ 222: } 456,
{ 223: } 457,
{ 224: } 458,
{ 225: } 460,
{ 226: } 462,
{ 227: } 463,
{ 228: } 465,
{ 229: } 467,
{ 230: } 468,
{ 231: } 469,
{ 232: } 470,
{ 233: } 470,
{ 234: } 470,
{ 235: } 470,
{ 236: } 471,
{ 237: } 472,
{ 238: } 473,
{ 239: } 475,
{ 240: } 477,
{ 241: } 479,
{ 242: } 480,
{ 243: } 481,
{ 244: } 482,
{ 245: } 484,
{ 246: } 486,
{ 247: } 488,
{ 248: } 489,
{ 249: } 490,
{ 250: } 492,
{ 251: } 493,
{ 252: } 494,
{ 253: } 494,
{ 254: } 494,
{ 255: } 495,
{ 256: } 497,
{ 257: } 499,
{ 258: } 501,
{ 259: } 503,
{ 260: } 505,
{ 261: } 506,
{ 262: } 507,
{ 263: } 507,
{ 264: } 508,
{ 265: } 509,
{ 266: } 511,
{ 267: } 512,
{ 268: } 513,
{ 269: } 514,
{ 270: } 515,
{ 271: } 516,
{ 272: } 517,
{ 273: } 518,
{ 274: } 519,
{ 275: } 520,
{ 276: } 521,
{ 277: } 522,
{ 278: } 523,
{ 279: } 525,
{ 280: } 527,
{ 281: } 528,
{ 282: } 529,
{ 283: } 530,
{ 284: } 531,
{ 285: } 532,
{ 286: } 533,
{ 287: } 534,
{ 288: } 535,
{ 289: } 536,
{ 290: } 537,
{ 291: } 538,
{ 292: } 539,
{ 293: } 540,
{ 294: } 541,
{ 295: } 542,
{ 296: } 542,
{ 297: } 543,
{ 298: } 544,
{ 299: } 545,
{ 300: } 546,
{ 301: } 547,
{ 302: } 548,
{ 303: } 549,
{ 304: } 550,
{ 305: } 551,
{ 306: } 552
);

yyth : array [0..yynstates-1] of Integer = (
{ 0: } 53,
{ 1: } 106,
{ 2: } 108,
{ 3: } 110,
{ 4: } 112,
{ 5: } 115,
{ 6: } 120,
{ 7: } 122,
{ 8: } 124,
{ 9: } 130,
{ 10: } 131,
{ 11: } 132,
{ 12: } 133,
{ 13: } 135,
{ 14: } 137,
{ 15: } 145,
{ 16: } 145,
{ 17: } 145,
{ 18: } 145,
{ 19: } 145,
{ 20: } 145,
{ 21: } 145,
{ 22: } 145,
{ 23: } 145,
{ 24: } 145,
{ 25: } 145,
{ 26: } 145,
{ 27: } 145,
{ 28: } 146,
{ 29: } 149,
{ 30: } 152,
{ 31: } 156,
{ 32: } 158,
{ 33: } 160,
{ 34: } 162,
{ 35: } 164,
{ 36: } 166,
{ 37: } 169,
{ 38: } 172,
{ 39: } 176,
{ 40: } 176,
{ 41: } 176,
{ 42: } 178,
{ 43: } 180,
{ 44: } 183,
{ 45: } 185,
{ 46: } 187,
{ 47: } 189,
{ 48: } 191,
{ 49: } 193,
{ 50: } 194,
{ 51: } 194,
{ 52: } 194,
{ 53: } 194,
{ 54: } 194,
{ 55: } 194,
{ 56: } 194,
{ 57: } 196,
{ 58: } 196,
{ 59: } 198,
{ 60: } 198,
{ 61: } 200,
{ 62: } 202,
{ 63: } 203,
{ 64: } 208,
{ 65: } 209,
{ 66: } 209,
{ 67: } 210,
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
{ 115: } 296,
{ 116: } 296,
{ 117: } 298,
{ 118: } 299,
{ 119: } 300,
{ 120: } 302,
{ 121: } 303,
{ 122: } 303,
{ 123: } 304,
{ 124: } 305,
{ 125: } 307,
{ 126: } 309,
{ 127: } 310,
{ 128: } 311,
{ 129: } 312,
{ 130: } 314,
{ 131: } 315,
{ 132: } 316,
{ 133: } 316,
{ 134: } 318,
{ 135: } 320,
{ 136: } 322,
{ 137: } 324,
{ 138: } 326,
{ 139: } 328,
{ 140: } 330,
{ 141: } 332,
{ 142: } 334,
{ 143: } 337,
{ 144: } 339,
{ 145: } 341,
{ 146: } 343,
{ 147: } 345,
{ 148: } 347,
{ 149: } 349,
{ 150: } 351,
{ 151: } 353,
{ 152: } 355,
{ 153: } 357,
{ 154: } 359,
{ 155: } 360,
{ 156: } 362,
{ 157: } 363,
{ 158: } 364,
{ 159: } 366,
{ 160: } 368,
{ 161: } 370,
{ 162: } 372,
{ 163: } 373,
{ 164: } 374,
{ 165: } 375,
{ 166: } 375,
{ 167: } 376,
{ 168: } 377,
{ 169: } 378,
{ 170: } 379,
{ 171: } 380,
{ 172: } 380,
{ 173: } 381,
{ 174: } 382,
{ 175: } 384,
{ 176: } 385,
{ 177: } 387,
{ 178: } 389,
{ 179: } 391,
{ 180: } 393,
{ 181: } 395,
{ 182: } 397,
{ 183: } 399,
{ 184: } 401,
{ 185: } 403,
{ 186: } 405,
{ 187: } 406,
{ 188: } 407,
{ 189: } 408,
{ 190: } 410,
{ 191: } 412,
{ 192: } 414,
{ 193: } 416,
{ 194: } 418,
{ 195: } 420,
{ 196: } 422,
{ 197: } 424,
{ 198: } 425,
{ 199: } 426,
{ 200: } 427,
{ 201: } 428,
{ 202: } 429,
{ 203: } 430,
{ 204: } 430,
{ 205: } 430,
{ 206: } 431,
{ 207: } 432,
{ 208: } 433,
{ 209: } 434,
{ 210: } 435,
{ 211: } 437,
{ 212: } 439,
{ 213: } 441,
{ 214: } 442,
{ 215: } 444,
{ 216: } 445,
{ 217: } 447,
{ 218: } 449,
{ 219: } 451,
{ 220: } 453,
{ 221: } 455,
{ 222: } 456,
{ 223: } 457,
{ 224: } 459,
{ 225: } 461,
{ 226: } 462,
{ 227: } 464,
{ 228: } 466,
{ 229: } 467,
{ 230: } 468,
{ 231: } 469,
{ 232: } 469,
{ 233: } 469,
{ 234: } 469,
{ 235: } 470,
{ 236: } 471,
{ 237: } 472,
{ 238: } 474,
{ 239: } 476,
{ 240: } 478,
{ 241: } 479,
{ 242: } 480,
{ 243: } 481,
{ 244: } 483,
{ 245: } 485,
{ 246: } 487,
{ 247: } 488,
{ 248: } 489,
{ 249: } 491,
{ 250: } 492,
{ 251: } 493,
{ 252: } 493,
{ 253: } 493,
{ 254: } 494,
{ 255: } 496,
{ 256: } 498,
{ 257: } 500,
{ 258: } 502,
{ 259: } 504,
{ 260: } 505,
{ 261: } 506,
{ 262: } 506,
{ 263: } 507,
{ 264: } 508,
{ 265: } 510,
{ 266: } 511,
{ 267: } 512,
{ 268: } 513,
{ 269: } 514,
{ 270: } 515,
{ 271: } 516,
{ 272: } 517,
{ 273: } 518,
{ 274: } 519,
{ 275: } 520,
{ 276: } 521,
{ 277: } 522,
{ 278: } 524,
{ 279: } 526,
{ 280: } 527,
{ 281: } 528,
{ 282: } 529,
{ 283: } 530,
{ 284: } 531,
{ 285: } 532,
{ 286: } 533,
{ 287: } 534,
{ 288: } 535,
{ 289: } 536,
{ 290: } 537,
{ 291: } 538,
{ 292: } 539,
{ 293: } 540,
{ 294: } 541,
{ 295: } 541,
{ 296: } 542,
{ 297: } 543,
{ 298: } 544,
{ 299: } 545,
{ 300: } 546,
{ 301: } 547,
{ 302: } 548,
{ 303: } 549,
{ 304: } 550,
{ 305: } 551,
{ 306: } 551
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

