
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
                           if yytext[length(yytext)] in ['L','U'] then
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
                           if yytext[length(yytext)] in ['L','U'] then
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

yynmarks   = 309;
yynmatches = 309;
yyntrans   = 550;
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
  9,
  87,
  { 9: }
  11,
  87,
  { 10: }
  37,
  87,
  { 11: }
  24,
  87,
  { 12: }
  19,
  87,
  { 13: }
  20,
  87,
  { 14: }
  87,
  { 15: }
  21,
  87,
  { 16: }
  22,
  87,
  { 17: }
  23,
  87,
  { 18: }
  26,
  87,
  { 19: }
  27,
  87,
  { 20: }
  28,
  87,
  { 21: }
  29,
  87,
  { 22: }
  30,
  87,
  { 23: }
  31,
  87,
  { 24: }
  32,
  87,
  { 25: }
  33,
  87,
  { 26: }
  34,
  87,
  { 27: }
  36,
  87,
  { 28: }
  83,
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
  83,
  87,
  { 39: }
  66,
  87,
  { 40: }
  67,
  87,
  { 41: }
  83,
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
  9,
  87,
{ 9: }
  11,
  87,
{ 10: }
  37,
  87,
{ 11: }
  24,
  87,
{ 12: }
  19,
  87,
{ 13: }
  20,
  87,
{ 14: }
  87,
{ 15: }
  21,
  87,
{ 16: }
  22,
  87,
{ 17: }
  23,
  87,
{ 18: }
  26,
  87,
{ 19: }
  27,
  87,
{ 20: }
  28,
  87,
{ 21: }
  29,
  87,
{ 22: }
  30,
  87,
{ 23: }
  31,
  87,
{ 24: }
  32,
  87,
{ 25: }
  33,
  87,
{ 26: }
  34,
  87,
{ 27: }
  36,
  87,
{ 28: }
  83,
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
  83,
  87,
{ 39: }
  66,
  87,
{ 40: }
  67,
  87,
{ 41: }
  83,
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
  ( cc: [ #1..#8,#11,#13..#31,'$','%','@','\','^','`',
            #127..#255 ]; s: 54),
  ( cc: [ #9,#12,' ' ]; s: 52),
  ( cc: [ #10 ]; s: 53),
  ( cc: [ '!' ]; s: 11),
  ( cc: [ '"' ]; s: 3),
  ( cc: [ '#' ]; s: 14),
  ( cc: [ '&' ]; s: 16),
  ( cc: [ '''' ]; s: 4),
  ( cc: [ '(' ]; s: 24),
  ( cc: [ ')' ]; s: 25),
  ( cc: [ '*' ]; s: 26),
  ( cc: [ '+' ]; s: 18),
  ( cc: [ ',' ]; s: 21),
  ( cc: [ '-' ]; s: 9),
  ( cc: [ '.' ]; s: 27),
  ( cc: [ '/' ]; s: 2),
  ( cc: [ '0' ]; s: 8),
  ( cc: [ '1'..'9' ]; s: 6),
  ( cc: [ ':' ]; s: 20),
  ( cc: [ ';' ]; s: 51),
  ( cc: [ '<' ]; s: 13),
  ( cc: [ '=' ]; s: 10),
  ( cc: [ '>' ]; s: 12),
  ( cc: [ '?' ]; s: 19),
  ( cc: [ 'A','B','D','G','I'..'K','M','O','Q','R',
            'T','X'..'Z','_','a','b','d','g','j','k',
            'm','o'..'r','w'..'z' ]; s: 50),
  ( cc: [ 'C' ]; s: 30),
  ( cc: [ 'E' ]; s: 33),
  ( cc: [ 'F' ]; s: 45),
  ( cc: [ 'H' ]; s: 48),
  ( cc: [ 'L' ]; s: 5),
  ( cc: [ 'N' ]; s: 46),
  ( cc: [ 'P' ]; s: 31),
  ( cc: [ 'S' ]; s: 29),
  ( cc: [ 'U' ]; s: 7),
  ( cc: [ 'V' ]; s: 35),
  ( cc: [ 'W' ]; s: 32),
  ( cc: [ '[' ]; s: 22),
  ( cc: [ ']' ]; s: 23),
  ( cc: [ 'c' ]; s: 36),
  ( cc: [ 'e' ]; s: 28),
  ( cc: [ 'f' ]; s: 44),
  ( cc: [ 'h' ]; s: 49),
  ( cc: [ 'i' ]; s: 42),
  ( cc: [ 'l' ]; s: 43),
  ( cc: [ 'n' ]; s: 47),
  ( cc: [ 's' ]; s: 38),
  ( cc: [ 't' ]; s: 41),
  ( cc: [ 'u' ]; s: 37),
  ( cc: [ 'v' ]; s: 34),
  ( cc: [ '{' ]; s: 39),
  ( cc: [ '|' ]; s: 15),
  ( cc: [ '}' ]; s: 40),
  ( cc: [ '~' ]; s: 17),
{ 1: }
  ( cc: [ #1..#8,#11,#13..#31,'$','%','@','\','^','`',
            #127..#255 ]; s: 54),
  ( cc: [ #9,#12,' ' ]; s: 52),
  ( cc: [ #10 ]; s: 53),
  ( cc: [ '!' ]; s: 11),
  ( cc: [ '"' ]; s: 3),
  ( cc: [ '#' ]; s: 14),
  ( cc: [ '&' ]; s: 16),
  ( cc: [ '''' ]; s: 4),
  ( cc: [ '(' ]; s: 24),
  ( cc: [ ')' ]; s: 25),
  ( cc: [ '*' ]; s: 26),
  ( cc: [ '+' ]; s: 18),
  ( cc: [ ',' ]; s: 21),
  ( cc: [ '-' ]; s: 9),
  ( cc: [ '.' ]; s: 27),
  ( cc: [ '/' ]; s: 2),
  ( cc: [ '0' ]; s: 8),
  ( cc: [ '1'..'9' ]; s: 6),
  ( cc: [ ':' ]; s: 20),
  ( cc: [ ';' ]; s: 51),
  ( cc: [ '<' ]; s: 13),
  ( cc: [ '=' ]; s: 10),
  ( cc: [ '>' ]; s: 12),
  ( cc: [ '?' ]; s: 19),
  ( cc: [ 'A','B','D','G','I'..'K','M','O','Q','R',
            'T','X'..'Z','_','a','b','d','g','j','k',
            'm','o'..'r','w'..'z' ]; s: 50),
  ( cc: [ 'C' ]; s: 30),
  ( cc: [ 'E' ]; s: 33),
  ( cc: [ 'F' ]; s: 45),
  ( cc: [ 'H' ]; s: 48),
  ( cc: [ 'L' ]; s: 5),
  ( cc: [ 'N' ]; s: 46),
  ( cc: [ 'P' ]; s: 31),
  ( cc: [ 'S' ]; s: 29),
  ( cc: [ 'U' ]; s: 7),
  ( cc: [ 'V' ]; s: 35),
  ( cc: [ 'W' ]; s: 32),
  ( cc: [ '[' ]; s: 22),
  ( cc: [ ']' ]; s: 23),
  ( cc: [ 'c' ]; s: 36),
  ( cc: [ 'e' ]; s: 28),
  ( cc: [ 'f' ]; s: 44),
  ( cc: [ 'h' ]; s: 49),
  ( cc: [ 'i' ]; s: 42),
  ( cc: [ 'l' ]; s: 43),
  ( cc: [ 'n' ]; s: 47),
  ( cc: [ 's' ]; s: 38),
  ( cc: [ 't' ]; s: 41),
  ( cc: [ 'u' ]; s: 37),
  ( cc: [ 'v' ]; s: 34),
  ( cc: [ '{' ]; s: 39),
  ( cc: [ '|' ]; s: 15),
  ( cc: [ '}' ]; s: 40),
  ( cc: [ '~' ]; s: 17),
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
  ( cc: [ 'L' ]; s: 66),
  ( cc: [ 'U' ]; s: 65),
{ 7: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 69),
{ 8: }
  ( cc: [ '.' ]; s: 67),
  ( cc: [ '0'..'9' ]; s: 64),
  ( cc: [ 'E','e' ]; s: 68),
  ( cc: [ 'L' ]; s: 66),
  ( cc: [ 'U' ]; s: 65),
  ( cc: [ 'x' ]; s: 70),
{ 9: }
  ( cc: [ '>' ]; s: 71),
{ 10: }
  ( cc: [ '=' ]; s: 72),
{ 11: }
  ( cc: [ '=' ]; s: 73),
{ 12: }
  ( cc: [ '=' ]; s: 74),
  ( cc: [ '>' ]; s: 75),
{ 13: }
  ( cc: [ '<' ]; s: 77),
  ( cc: [ '=' ]; s: 76),
{ 14: }
  ( cc: [ #9 ]; s: 80),
  ( cc: [ ' ' ]; s: 83),
  ( cc: [ '#' ]; s: 78),
  ( cc: [ 'd' ]; s: 85),
  ( cc: [ 'e' ]; s: 81),
  ( cc: [ 'i' ]; s: 79),
  ( cc: [ 'p' ]; s: 84),
  ( cc: [ 'u' ]; s: 82),
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
{ 27: }
  ( cc: [ '.' ]; s: 86),
{ 28: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'w','y','z' ]; s: 63),
  ( cc: [ 'n' ]; s: 88),
  ( cc: [ 'x' ]; s: 87),
{ 29: }
  ( cc: [ '0'..'9','A'..'S','U'..'X','Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'T' ]; s: 89),
  ( cc: [ 'Y' ]; s: 90),
{ 30: }
  ( cc: [ '0'..'9','B','C','E'..'N','P'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 92),
  ( cc: [ 'D' ]; s: 91),
  ( cc: [ 'O' ]; s: 93),
{ 31: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 94),
{ 32: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'I' ]; s: 95),
{ 33: }
  ( cc: [ '0'..'9','A'..'W','Y','Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'X' ]; s: 96),
{ 34: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 97),
{ 35: }
  ( cc: [ '0'..'9','A'..'N','P'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'O' ]; s: 98),
{ 36: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','i'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'h' ]; s: 99),
  ( cc: [ 'o' ]; s: 100),
{ 37: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 101),
{ 38: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','j'..'s','u'..'z' ]; s: 63),
  ( cc: [ 'h' ]; s: 103),
  ( cc: [ 'i' ]; s: 104),
  ( cc: [ 't' ]; s: 102),
{ 39: }
{ 40: }
{ 41: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'x','z' ]; s: 63),
  ( cc: [ 'y' ]; s: 105),
{ 42: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 106),
{ 43: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 107),
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
  ( cc: [ 'L' ]; s: 66),
  ( cc: [ 'U' ]; s: 65),
{ 65: }
  ( cc: [ 'L' ]; s: 66),
{ 66: }
{ 67: }
  ( cc: [ '0'..'9' ]; s: 117),
{ 68: }
  ( cc: [ '+','-' ]; s: 118),
  ( cc: [ '0'..'9' ]; s: 119),
{ 69: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 70: }
  ( cc: [ '0'..'9','A'..'F','a'..'f' ]; s: 70),
  ( cc: [ 'L' ]; s: 121),
  ( cc: [ 'U' ]; s: 120),
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
{ 77: }
{ 78: }
{ 79: }
  ( cc: [ 'f' ]; s: 122),
  ( cc: [ 'n' ]; s: 123),
{ 80: }
  ( cc: [ #9,' ' ]; s: 80),
  ( cc: [ 'd' ]; s: 85),
  ( cc: [ 'e' ]; s: 81),
  ( cc: [ 'i' ]; s: 124),
  ( cc: [ 'p' ]; s: 84),
  ( cc: [ 'u' ]; s: 82),
{ 81: }
  ( cc: [ 'l' ]; s: 125),
  ( cc: [ 'n' ]; s: 126),
  ( cc: [ 'r' ]; s: 127),
{ 82: }
  ( cc: [ 'n' ]; s: 128),
{ 83: }
  ( cc: [ #9,' ' ]; s: 80),
  ( cc: [ '0'..'9' ]; s: 129),
  ( cc: [ 'd' ]; s: 85),
  ( cc: [ 'e' ]; s: 81),
  ( cc: [ 'i' ]; s: 124),
  ( cc: [ 'p' ]; s: 84),
  ( cc: [ 'u' ]; s: 82),
{ 84: }
  ( cc: [ 'r' ]; s: 130),
{ 85: }
  ( cc: [ 'e' ]; s: 131),
{ 86: }
  ( cc: [ '.' ]; s: 132),
{ 87: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 133),
{ 88: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 63),
  ( cc: [ 'u' ]; s: 134),
{ 89: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'D' ]; s: 135),
{ 90: }
  ( cc: [ '0'..'9','A'..'R','T'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'S' ]; s: 136),
{ 91: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 137),
{ 92: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 138),
{ 93: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'N' ]; s: 139),
{ 94: }
  ( cc: [ '0'..'9','A','B','D'..'R','T'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 141),
  ( cc: [ 'S' ]; s: 140),
{ 95: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'N' ]; s: 142),
{ 96: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'P' ]; s: 143),
{ 97: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'z' ]; s: 63),
  ( cc: [ 'i' ]; s: 144),
{ 98: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'I' ]; s: 145),
{ 99: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 63),
  ( cc: [ 'a' ]; s: 146),
{ 100: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 147),
{ 101: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'r','t'..'z' ]; s: 63),
  ( cc: [ 'i' ]; s: 148),
  ( cc: [ 's' ]; s: 149),
{ 102: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 150),
{ 103: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 151),
{ 104: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 63),
  ( cc: [ 'g' ]; s: 152),
{ 105: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'o','q'..'z' ]; s: 63),
  ( cc: [ 'p' ]; s: 153),
{ 106: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 154),
{ 107: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 155),
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
  ( cc: [ 'L' ]; s: 121),
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
{ 1: } 2,
{ 2: } 3,
{ 3: } 5,
{ 4: } 6,
{ 5: } 7,
{ 6: } 10,
{ 7: } 13,
{ 8: } 16,
{ 9: } 19,
{ 10: } 21,
{ 11: } 23,
{ 12: } 25,
{ 13: } 27,
{ 14: } 29,
{ 15: } 30,
{ 16: } 32,
{ 17: } 34,
{ 18: } 36,
{ 19: } 38,
{ 20: } 40,
{ 21: } 42,
{ 22: } 44,
{ 23: } 46,
{ 24: } 48,
{ 25: } 50,
{ 26: } 52,
{ 27: } 54,
{ 28: } 56,
{ 29: } 58,
{ 30: } 60,
{ 31: } 62,
{ 32: } 64,
{ 33: } 66,
{ 34: } 68,
{ 35: } 70,
{ 36: } 72,
{ 37: } 74,
{ 38: } 76,
{ 39: } 78,
{ 40: } 80,
{ 41: } 82,
{ 42: } 84,
{ 43: } 86,
{ 44: } 88,
{ 45: } 90,
{ 46: } 92,
{ 47: } 94,
{ 48: } 96,
{ 49: } 98,
{ 50: } 100,
{ 51: } 102,
{ 52: } 104,
{ 53: } 106,
{ 54: } 107,
{ 55: } 108,
{ 56: } 109,
{ 57: } 110,
{ 58: } 110,
{ 59: } 111,
{ 60: } 111,
{ 61: } 112,
{ 62: } 112,
{ 63: } 112,
{ 64: } 113,
{ 65: } 115,
{ 66: } 116,
{ 67: } 117,
{ 68: } 117,
{ 69: } 117,
{ 70: } 119,
{ 71: } 120,
{ 72: } 121,
{ 73: } 122,
{ 74: } 123,
{ 75: } 124,
{ 76: } 125,
{ 77: } 126,
{ 78: } 127,
{ 79: } 128,
{ 80: } 128,
{ 81: } 128,
{ 82: } 128,
{ 83: } 128,
{ 84: } 128,
{ 85: } 128,
{ 86: } 128,
{ 87: } 128,
{ 88: } 129,
{ 89: } 130,
{ 90: } 131,
{ 91: } 132,
{ 92: } 133,
{ 93: } 134,
{ 94: } 135,
{ 95: } 136,
{ 96: } 137,
{ 97: } 138,
{ 98: } 139,
{ 99: } 140,
{ 100: } 141,
{ 101: } 142,
{ 102: } 143,
{ 103: } 144,
{ 104: } 145,
{ 105: } 146,
{ 106: } 147,
{ 107: } 148,
{ 108: } 149,
{ 109: } 150,
{ 110: } 151,
{ 111: } 152,
{ 112: } 153,
{ 113: } 154,
{ 114: } 155,
{ 115: } 156,
{ 116: } 157,
{ 117: } 158,
{ 118: } 159,
{ 119: } 159,
{ 120: } 160,
{ 121: } 161,
{ 122: } 162,
{ 123: } 163,
{ 124: } 163,
{ 125: } 163,
{ 126: } 163,
{ 127: } 163,
{ 128: } 163,
{ 129: } 163,
{ 130: } 163,
{ 131: } 163,
{ 132: } 163,
{ 133: } 164,
{ 134: } 165,
{ 135: } 166,
{ 136: } 167,
{ 137: } 168,
{ 138: } 169,
{ 139: } 170,
{ 140: } 171,
{ 141: } 172,
{ 142: } 173,
{ 143: } 174,
{ 144: } 175,
{ 145: } 176,
{ 146: } 177,
{ 147: } 178,
{ 148: } 179,
{ 149: } 180,
{ 150: } 181,
{ 151: } 182,
{ 152: } 183,
{ 153: } 184,
{ 154: } 185,
{ 155: } 187,
{ 156: } 188,
{ 157: } 189,
{ 158: } 191,
{ 159: } 193,
{ 160: } 194,
{ 161: } 195,
{ 162: } 196,
{ 163: } 197,
{ 164: } 197,
{ 165: } 197,
{ 166: } 198,
{ 167: } 198,
{ 168: } 198,
{ 169: } 198,
{ 170: } 198,
{ 171: } 198,
{ 172: } 199,
{ 173: } 199,
{ 174: } 199,
{ 175: } 200,
{ 176: } 202,
{ 177: } 203,
{ 178: } 204,
{ 179: } 205,
{ 180: } 206,
{ 181: } 207,
{ 182: } 208,
{ 183: } 209,
{ 184: } 210,
{ 185: } 211,
{ 186: } 212,
{ 187: } 214,
{ 188: } 216,
{ 189: } 218,
{ 190: } 219,
{ 191: } 220,
{ 192: } 221,
{ 193: } 222,
{ 194: } 223,
{ 195: } 224,
{ 196: } 225,
{ 197: } 227,
{ 198: } 228,
{ 199: } 230,
{ 200: } 232,
{ 201: } 234,
{ 202: } 236,
{ 203: } 236,
{ 204: } 236,
{ 205: } 237,
{ 206: } 238,
{ 207: } 238,
{ 208: } 238,
{ 209: } 238,
{ 210: } 238,
{ 211: } 238,
{ 212: } 239,
{ 213: } 240,
{ 214: } 241,
{ 215: } 243,
{ 216: } 244,
{ 217: } 246,
{ 218: } 247,
{ 219: } 248,
{ 220: } 249,
{ 221: } 250,
{ 222: } 251,
{ 223: } 253,
{ 224: } 255,
{ 225: } 256,
{ 226: } 257,
{ 227: } 259,
{ 228: } 260,
{ 229: } 261,
{ 230: } 263,
{ 231: } 263,
{ 232: } 263,
{ 233: } 264,
{ 234: } 265,
{ 235: } 266,
{ 236: } 266,
{ 237: } 266,
{ 238: } 268,
{ 239: } 269,
{ 240: } 270,
{ 241: } 271,
{ 242: } 273,
{ 243: } 275,
{ 244: } 277,
{ 245: } 278,
{ 246: } 279,
{ 247: } 280,
{ 248: } 282,
{ 249: } 284,
{ 250: } 285,
{ 251: } 285,
{ 252: } 285,
{ 253: } 286,
{ 254: } 287,
{ 255: } 289,
{ 256: } 290,
{ 257: } 291,
{ 258: } 292,
{ 259: } 293,
{ 260: } 294,
{ 261: } 296,
{ 262: } 296,
{ 263: } 297,
{ 264: } 299,
{ 265: } 301,
{ 266: } 302,
{ 267: } 304,
{ 268: } 306,
{ 269: } 306,
{ 270: } 308,
{ 271: } 308,
{ 272: } 308,
{ 273: } 308,
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
{ 296: } 309,
{ 297: } 309,
{ 298: } 309,
{ 299: } 309,
{ 300: } 309,
{ 301: } 309,
{ 302: } 309,
{ 303: } 309,
{ 304: } 309,
{ 305: } 309,
{ 306: } 309
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
{ 9: } 20,
{ 10: } 22,
{ 11: } 24,
{ 12: } 26,
{ 13: } 28,
{ 14: } 29,
{ 15: } 31,
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
{ 39: } 79,
{ 40: } 81,
{ 41: } 83,
{ 42: } 85,
{ 43: } 87,
{ 44: } 89,
{ 45: } 91,
{ 46: } 93,
{ 47: } 95,
{ 48: } 97,
{ 49: } 99,
{ 50: } 101,
{ 51: } 103,
{ 52: } 105,
{ 53: } 106,
{ 54: } 107,
{ 55: } 108,
{ 56: } 109,
{ 57: } 109,
{ 58: } 110,
{ 59: } 110,
{ 60: } 111,
{ 61: } 111,
{ 62: } 111,
{ 63: } 112,
{ 64: } 114,
{ 65: } 115,
{ 66: } 116,
{ 67: } 116,
{ 68: } 116,
{ 69: } 118,
{ 70: } 119,
{ 71: } 120,
{ 72: } 121,
{ 73: } 122,
{ 74: } 123,
{ 75: } 124,
{ 76: } 125,
{ 77: } 126,
{ 78: } 127,
{ 79: } 127,
{ 80: } 127,
{ 81: } 127,
{ 82: } 127,
{ 83: } 127,
{ 84: } 127,
{ 85: } 127,
{ 86: } 127,
{ 87: } 128,
{ 88: } 129,
{ 89: } 130,
{ 90: } 131,
{ 91: } 132,
{ 92: } 133,
{ 93: } 134,
{ 94: } 135,
{ 95: } 136,
{ 96: } 137,
{ 97: } 138,
{ 98: } 139,
{ 99: } 140,
{ 100: } 141,
{ 101: } 142,
{ 102: } 143,
{ 103: } 144,
{ 104: } 145,
{ 105: } 146,
{ 106: } 147,
{ 107: } 148,
{ 108: } 149,
{ 109: } 150,
{ 110: } 151,
{ 111: } 152,
{ 112: } 153,
{ 113: } 154,
{ 114: } 155,
{ 115: } 156,
{ 116: } 157,
{ 117: } 158,
{ 118: } 158,
{ 119: } 159,
{ 120: } 160,
{ 121: } 161,
{ 122: } 162,
{ 123: } 162,
{ 124: } 162,
{ 125: } 162,
{ 126: } 162,
{ 127: } 162,
{ 128: } 162,
{ 129: } 162,
{ 130: } 162,
{ 131: } 162,
{ 132: } 163,
{ 133: } 164,
{ 134: } 165,
{ 135: } 166,
{ 136: } 167,
{ 137: } 168,
{ 138: } 169,
{ 139: } 170,
{ 140: } 171,
{ 141: } 172,
{ 142: } 173,
{ 143: } 174,
{ 144: } 175,
{ 145: } 176,
{ 146: } 177,
{ 147: } 178,
{ 148: } 179,
{ 149: } 180,
{ 150: } 181,
{ 151: } 182,
{ 152: } 183,
{ 153: } 184,
{ 154: } 186,
{ 155: } 187,
{ 156: } 188,
{ 157: } 190,
{ 158: } 192,
{ 159: } 193,
{ 160: } 194,
{ 161: } 195,
{ 162: } 196,
{ 163: } 196,
{ 164: } 196,
{ 165: } 197,
{ 166: } 197,
{ 167: } 197,
{ 168: } 197,
{ 169: } 197,
{ 170: } 197,
{ 171: } 198,
{ 172: } 198,
{ 173: } 198,
{ 174: } 199,
{ 175: } 201,
{ 176: } 202,
{ 177: } 203,
{ 178: } 204,
{ 179: } 205,
{ 180: } 206,
{ 181: } 207,
{ 182: } 208,
{ 183: } 209,
{ 184: } 210,
{ 185: } 211,
{ 186: } 213,
{ 187: } 215,
{ 188: } 217,
{ 189: } 218,
{ 190: } 219,
{ 191: } 220,
{ 192: } 221,
{ 193: } 222,
{ 194: } 223,
{ 195: } 224,
{ 196: } 226,
{ 197: } 227,
{ 198: } 229,
{ 199: } 231,
{ 200: } 233,
{ 201: } 235,
{ 202: } 235,
{ 203: } 235,
{ 204: } 236,
{ 205: } 237,
{ 206: } 237,
{ 207: } 237,
{ 208: } 237,
{ 209: } 237,
{ 210: } 237,
{ 211: } 238,
{ 212: } 239,
{ 213: } 240,
{ 214: } 242,
{ 215: } 243,
{ 216: } 245,
{ 217: } 246,
{ 218: } 247,
{ 219: } 248,
{ 220: } 249,
{ 221: } 250,
{ 222: } 252,
{ 223: } 254,
{ 224: } 255,
{ 225: } 256,
{ 226: } 258,
{ 227: } 259,
{ 228: } 260,
{ 229: } 262,
{ 230: } 262,
{ 231: } 262,
{ 232: } 263,
{ 233: } 264,
{ 234: } 265,
{ 235: } 265,
{ 236: } 265,
{ 237: } 267,
{ 238: } 268,
{ 239: } 269,
{ 240: } 270,
{ 241: } 272,
{ 242: } 274,
{ 243: } 276,
{ 244: } 277,
{ 245: } 278,
{ 246: } 279,
{ 247: } 281,
{ 248: } 283,
{ 249: } 284,
{ 250: } 284,
{ 251: } 284,
{ 252: } 285,
{ 253: } 286,
{ 254: } 288,
{ 255: } 289,
{ 256: } 290,
{ 257: } 291,
{ 258: } 292,
{ 259: } 293,
{ 260: } 295,
{ 261: } 295,
{ 262: } 296,
{ 263: } 298,
{ 264: } 300,
{ 265: } 301,
{ 266: } 303,
{ 267: } 305,
{ 268: } 305,
{ 269: } 307,
{ 270: } 307,
{ 271: } 307,
{ 272: } 307,
{ 273: } 307,
{ 274: } 307,
{ 275: } 307,
{ 276: } 307,
{ 277: } 307,
{ 278: } 307,
{ 279: } 307,
{ 280: } 307,
{ 281: } 307,
{ 282: } 307,
{ 283: } 307,
{ 284: } 307,
{ 285: } 307,
{ 286: } 307,
{ 287: } 307,
{ 288: } 307,
{ 289: } 307,
{ 290: } 307,
{ 291: } 307,
{ 292: } 307,
{ 293: } 307,
{ 294: } 307,
{ 295: } 308,
{ 296: } 308,
{ 297: } 308,
{ 298: } 308,
{ 299: } 308,
{ 300: } 308,
{ 301: } 308,
{ 302: } 308,
{ 303: } 308,
{ 304: } 308,
{ 305: } 308,
{ 306: } 309
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
{ 10: } 21,
{ 11: } 23,
{ 12: } 25,
{ 13: } 27,
{ 14: } 29,
{ 15: } 30,
{ 16: } 32,
{ 17: } 34,
{ 18: } 36,
{ 19: } 38,
{ 20: } 40,
{ 21: } 42,
{ 22: } 44,
{ 23: } 46,
{ 24: } 48,
{ 25: } 50,
{ 26: } 52,
{ 27: } 54,
{ 28: } 56,
{ 29: } 58,
{ 30: } 60,
{ 31: } 62,
{ 32: } 64,
{ 33: } 66,
{ 34: } 68,
{ 35: } 70,
{ 36: } 72,
{ 37: } 74,
{ 38: } 76,
{ 39: } 78,
{ 40: } 80,
{ 41: } 82,
{ 42: } 84,
{ 43: } 86,
{ 44: } 88,
{ 45: } 90,
{ 46: } 92,
{ 47: } 94,
{ 48: } 96,
{ 49: } 98,
{ 50: } 100,
{ 51: } 102,
{ 52: } 104,
{ 53: } 106,
{ 54: } 107,
{ 55: } 108,
{ 56: } 109,
{ 57: } 110,
{ 58: } 110,
{ 59: } 111,
{ 60: } 111,
{ 61: } 112,
{ 62: } 112,
{ 63: } 112,
{ 64: } 113,
{ 65: } 115,
{ 66: } 116,
{ 67: } 117,
{ 68: } 117,
{ 69: } 117,
{ 70: } 119,
{ 71: } 120,
{ 72: } 121,
{ 73: } 122,
{ 74: } 123,
{ 75: } 124,
{ 76: } 125,
{ 77: } 126,
{ 78: } 127,
{ 79: } 128,
{ 80: } 128,
{ 81: } 128,
{ 82: } 128,
{ 83: } 128,
{ 84: } 128,
{ 85: } 128,
{ 86: } 128,
{ 87: } 128,
{ 88: } 129,
{ 89: } 130,
{ 90: } 131,
{ 91: } 132,
{ 92: } 133,
{ 93: } 134,
{ 94: } 135,
{ 95: } 136,
{ 96: } 137,
{ 97: } 138,
{ 98: } 139,
{ 99: } 140,
{ 100: } 141,
{ 101: } 142,
{ 102: } 143,
{ 103: } 144,
{ 104: } 145,
{ 105: } 146,
{ 106: } 147,
{ 107: } 148,
{ 108: } 149,
{ 109: } 150,
{ 110: } 151,
{ 111: } 152,
{ 112: } 153,
{ 113: } 154,
{ 114: } 155,
{ 115: } 156,
{ 116: } 157,
{ 117: } 158,
{ 118: } 159,
{ 119: } 159,
{ 120: } 160,
{ 121: } 161,
{ 122: } 162,
{ 123: } 163,
{ 124: } 163,
{ 125: } 163,
{ 126: } 163,
{ 127: } 163,
{ 128: } 163,
{ 129: } 163,
{ 130: } 163,
{ 131: } 163,
{ 132: } 163,
{ 133: } 164,
{ 134: } 165,
{ 135: } 166,
{ 136: } 167,
{ 137: } 168,
{ 138: } 169,
{ 139: } 170,
{ 140: } 171,
{ 141: } 172,
{ 142: } 173,
{ 143: } 174,
{ 144: } 175,
{ 145: } 176,
{ 146: } 177,
{ 147: } 178,
{ 148: } 179,
{ 149: } 180,
{ 150: } 181,
{ 151: } 182,
{ 152: } 183,
{ 153: } 184,
{ 154: } 185,
{ 155: } 187,
{ 156: } 188,
{ 157: } 189,
{ 158: } 191,
{ 159: } 193,
{ 160: } 194,
{ 161: } 195,
{ 162: } 196,
{ 163: } 197,
{ 164: } 197,
{ 165: } 197,
{ 166: } 198,
{ 167: } 198,
{ 168: } 198,
{ 169: } 198,
{ 170: } 198,
{ 171: } 198,
{ 172: } 199,
{ 173: } 199,
{ 174: } 199,
{ 175: } 200,
{ 176: } 202,
{ 177: } 203,
{ 178: } 204,
{ 179: } 205,
{ 180: } 206,
{ 181: } 207,
{ 182: } 208,
{ 183: } 209,
{ 184: } 210,
{ 185: } 211,
{ 186: } 212,
{ 187: } 214,
{ 188: } 216,
{ 189: } 218,
{ 190: } 219,
{ 191: } 220,
{ 192: } 221,
{ 193: } 222,
{ 194: } 223,
{ 195: } 224,
{ 196: } 225,
{ 197: } 227,
{ 198: } 228,
{ 199: } 230,
{ 200: } 232,
{ 201: } 234,
{ 202: } 236,
{ 203: } 236,
{ 204: } 236,
{ 205: } 237,
{ 206: } 238,
{ 207: } 238,
{ 208: } 238,
{ 209: } 238,
{ 210: } 238,
{ 211: } 238,
{ 212: } 239,
{ 213: } 240,
{ 214: } 241,
{ 215: } 243,
{ 216: } 244,
{ 217: } 246,
{ 218: } 247,
{ 219: } 248,
{ 220: } 249,
{ 221: } 250,
{ 222: } 251,
{ 223: } 253,
{ 224: } 255,
{ 225: } 256,
{ 226: } 257,
{ 227: } 259,
{ 228: } 260,
{ 229: } 261,
{ 230: } 263,
{ 231: } 263,
{ 232: } 263,
{ 233: } 264,
{ 234: } 265,
{ 235: } 266,
{ 236: } 266,
{ 237: } 266,
{ 238: } 268,
{ 239: } 269,
{ 240: } 270,
{ 241: } 271,
{ 242: } 273,
{ 243: } 275,
{ 244: } 277,
{ 245: } 278,
{ 246: } 279,
{ 247: } 280,
{ 248: } 282,
{ 249: } 284,
{ 250: } 285,
{ 251: } 285,
{ 252: } 285,
{ 253: } 286,
{ 254: } 287,
{ 255: } 289,
{ 256: } 290,
{ 257: } 291,
{ 258: } 292,
{ 259: } 293,
{ 260: } 294,
{ 261: } 296,
{ 262: } 296,
{ 263: } 297,
{ 264: } 299,
{ 265: } 301,
{ 266: } 302,
{ 267: } 304,
{ 268: } 306,
{ 269: } 306,
{ 270: } 308,
{ 271: } 308,
{ 272: } 308,
{ 273: } 308,
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
{ 296: } 309,
{ 297: } 309,
{ 298: } 309,
{ 299: } 309,
{ 300: } 309,
{ 301: } 309,
{ 302: } 309,
{ 303: } 309,
{ 304: } 309,
{ 305: } 309,
{ 306: } 309
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
{ 9: } 20,
{ 10: } 22,
{ 11: } 24,
{ 12: } 26,
{ 13: } 28,
{ 14: } 29,
{ 15: } 31,
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
{ 39: } 79,
{ 40: } 81,
{ 41: } 83,
{ 42: } 85,
{ 43: } 87,
{ 44: } 89,
{ 45: } 91,
{ 46: } 93,
{ 47: } 95,
{ 48: } 97,
{ 49: } 99,
{ 50: } 101,
{ 51: } 103,
{ 52: } 105,
{ 53: } 106,
{ 54: } 107,
{ 55: } 108,
{ 56: } 109,
{ 57: } 109,
{ 58: } 110,
{ 59: } 110,
{ 60: } 111,
{ 61: } 111,
{ 62: } 111,
{ 63: } 112,
{ 64: } 114,
{ 65: } 115,
{ 66: } 116,
{ 67: } 116,
{ 68: } 116,
{ 69: } 118,
{ 70: } 119,
{ 71: } 120,
{ 72: } 121,
{ 73: } 122,
{ 74: } 123,
{ 75: } 124,
{ 76: } 125,
{ 77: } 126,
{ 78: } 127,
{ 79: } 127,
{ 80: } 127,
{ 81: } 127,
{ 82: } 127,
{ 83: } 127,
{ 84: } 127,
{ 85: } 127,
{ 86: } 127,
{ 87: } 128,
{ 88: } 129,
{ 89: } 130,
{ 90: } 131,
{ 91: } 132,
{ 92: } 133,
{ 93: } 134,
{ 94: } 135,
{ 95: } 136,
{ 96: } 137,
{ 97: } 138,
{ 98: } 139,
{ 99: } 140,
{ 100: } 141,
{ 101: } 142,
{ 102: } 143,
{ 103: } 144,
{ 104: } 145,
{ 105: } 146,
{ 106: } 147,
{ 107: } 148,
{ 108: } 149,
{ 109: } 150,
{ 110: } 151,
{ 111: } 152,
{ 112: } 153,
{ 113: } 154,
{ 114: } 155,
{ 115: } 156,
{ 116: } 157,
{ 117: } 158,
{ 118: } 158,
{ 119: } 159,
{ 120: } 160,
{ 121: } 161,
{ 122: } 162,
{ 123: } 162,
{ 124: } 162,
{ 125: } 162,
{ 126: } 162,
{ 127: } 162,
{ 128: } 162,
{ 129: } 162,
{ 130: } 162,
{ 131: } 162,
{ 132: } 163,
{ 133: } 164,
{ 134: } 165,
{ 135: } 166,
{ 136: } 167,
{ 137: } 168,
{ 138: } 169,
{ 139: } 170,
{ 140: } 171,
{ 141: } 172,
{ 142: } 173,
{ 143: } 174,
{ 144: } 175,
{ 145: } 176,
{ 146: } 177,
{ 147: } 178,
{ 148: } 179,
{ 149: } 180,
{ 150: } 181,
{ 151: } 182,
{ 152: } 183,
{ 153: } 184,
{ 154: } 186,
{ 155: } 187,
{ 156: } 188,
{ 157: } 190,
{ 158: } 192,
{ 159: } 193,
{ 160: } 194,
{ 161: } 195,
{ 162: } 196,
{ 163: } 196,
{ 164: } 196,
{ 165: } 197,
{ 166: } 197,
{ 167: } 197,
{ 168: } 197,
{ 169: } 197,
{ 170: } 197,
{ 171: } 198,
{ 172: } 198,
{ 173: } 198,
{ 174: } 199,
{ 175: } 201,
{ 176: } 202,
{ 177: } 203,
{ 178: } 204,
{ 179: } 205,
{ 180: } 206,
{ 181: } 207,
{ 182: } 208,
{ 183: } 209,
{ 184: } 210,
{ 185: } 211,
{ 186: } 213,
{ 187: } 215,
{ 188: } 217,
{ 189: } 218,
{ 190: } 219,
{ 191: } 220,
{ 192: } 221,
{ 193: } 222,
{ 194: } 223,
{ 195: } 224,
{ 196: } 226,
{ 197: } 227,
{ 198: } 229,
{ 199: } 231,
{ 200: } 233,
{ 201: } 235,
{ 202: } 235,
{ 203: } 235,
{ 204: } 236,
{ 205: } 237,
{ 206: } 237,
{ 207: } 237,
{ 208: } 237,
{ 209: } 237,
{ 210: } 237,
{ 211: } 238,
{ 212: } 239,
{ 213: } 240,
{ 214: } 242,
{ 215: } 243,
{ 216: } 245,
{ 217: } 246,
{ 218: } 247,
{ 219: } 248,
{ 220: } 249,
{ 221: } 250,
{ 222: } 252,
{ 223: } 254,
{ 224: } 255,
{ 225: } 256,
{ 226: } 258,
{ 227: } 259,
{ 228: } 260,
{ 229: } 262,
{ 230: } 262,
{ 231: } 262,
{ 232: } 263,
{ 233: } 264,
{ 234: } 265,
{ 235: } 265,
{ 236: } 265,
{ 237: } 267,
{ 238: } 268,
{ 239: } 269,
{ 240: } 270,
{ 241: } 272,
{ 242: } 274,
{ 243: } 276,
{ 244: } 277,
{ 245: } 278,
{ 246: } 279,
{ 247: } 281,
{ 248: } 283,
{ 249: } 284,
{ 250: } 284,
{ 251: } 284,
{ 252: } 285,
{ 253: } 286,
{ 254: } 288,
{ 255: } 289,
{ 256: } 290,
{ 257: } 291,
{ 258: } 292,
{ 259: } 293,
{ 260: } 295,
{ 261: } 295,
{ 262: } 296,
{ 263: } 298,
{ 264: } 300,
{ 265: } 301,
{ 266: } 303,
{ 267: } 305,
{ 268: } 305,
{ 269: } 307,
{ 270: } 307,
{ 271: } 307,
{ 272: } 307,
{ 273: } 307,
{ 274: } 307,
{ 275: } 307,
{ 276: } 307,
{ 277: } 307,
{ 278: } 307,
{ 279: } 307,
{ 280: } 307,
{ 281: } 307,
{ 282: } 307,
{ 283: } 307,
{ 284: } 307,
{ 285: } 307,
{ 286: } 307,
{ 287: } 307,
{ 288: } 307,
{ 289: } 307,
{ 290: } 307,
{ 291: } 307,
{ 292: } 307,
{ 293: } 307,
{ 294: } 307,
{ 295: } 308,
{ 296: } 308,
{ 297: } 308,
{ 298: } 308,
{ 299: } 308,
{ 300: } 308,
{ 301: } 308,
{ 302: } 308,
{ 303: } 308,
{ 304: } 308,
{ 305: } 308,
{ 306: } 309
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
{ 9: } 129,
{ 10: } 130,
{ 11: } 131,
{ 12: } 132,
{ 13: } 134,
{ 14: } 136,
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
{ 27: } 144,
{ 28: } 145,
{ 29: } 148,
{ 30: } 151,
{ 31: } 155,
{ 32: } 157,
{ 33: } 159,
{ 34: } 161,
{ 35: } 163,
{ 36: } 165,
{ 37: } 168,
{ 38: } 170,
{ 39: } 174,
{ 40: } 174,
{ 41: } 174,
{ 42: } 176,
{ 43: } 178,
{ 44: } 180,
{ 45: } 183,
{ 46: } 185,
{ 47: } 187,
{ 48: } 189,
{ 49: } 191,
{ 50: } 193,
{ 51: } 194,
{ 52: } 194,
{ 53: } 194,
{ 54: } 194,
{ 55: } 194,
{ 56: } 194,
{ 57: } 194,
{ 58: } 196,
{ 59: } 196,
{ 60: } 198,
{ 61: } 198,
{ 62: } 200,
{ 63: } 202,
{ 64: } 203,
{ 65: } 208,
{ 66: } 209,
{ 67: } 209,
{ 68: } 210,
{ 69: } 212,
{ 70: } 213,
{ 71: } 216,
{ 72: } 216,
{ 73: } 216,
{ 74: } 216,
{ 75: } 216,
{ 76: } 216,
{ 77: } 216,
{ 78: } 216,
{ 79: } 216,
{ 80: } 218,
{ 81: } 224,
{ 82: } 227,
{ 83: } 228,
{ 84: } 235,
{ 85: } 236,
{ 86: } 237,
{ 87: } 238,
{ 88: } 240,
{ 89: } 242,
{ 90: } 244,
{ 91: } 246,
{ 92: } 248,
{ 93: } 250,
{ 94: } 252,
{ 95: } 255,
{ 96: } 257,
{ 97: } 259,
{ 98: } 261,
{ 99: } 263,
{ 100: } 265,
{ 101: } 267,
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
{ 115: } 296,
{ 116: } 296,
{ 117: } 296,
{ 118: } 298,
{ 119: } 299,
{ 120: } 300,
{ 121: } 301,
{ 122: } 301,
{ 123: } 302,
{ 124: } 303,
{ 125: } 305,
{ 126: } 307,
{ 127: } 308,
{ 128: } 309,
{ 129: } 310,
{ 130: } 312,
{ 131: } 313,
{ 132: } 314,
{ 133: } 314,
{ 134: } 316,
{ 135: } 318,
{ 136: } 320,
{ 137: } 322,
{ 138: } 324,
{ 139: } 326,
{ 140: } 328,
{ 141: } 330,
{ 142: } 332,
{ 143: } 335,
{ 144: } 337,
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
{ 155: } 358,
{ 156: } 360,
{ 157: } 362,
{ 158: } 363,
{ 159: } 364,
{ 160: } 366,
{ 161: } 368,
{ 162: } 370,
{ 163: } 372,
{ 164: } 373,
{ 165: } 374,
{ 166: } 374,
{ 167: } 375,
{ 168: } 376,
{ 169: } 377,
{ 170: } 378,
{ 171: } 379,
{ 172: } 379,
{ 173: } 380,
{ 174: } 381,
{ 175: } 383,
{ 176: } 384,
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
{ 187: } 405,
{ 188: } 406,
{ 189: } 407,
{ 190: } 409,
{ 191: } 411,
{ 192: } 413,
{ 193: } 415,
{ 194: } 417,
{ 195: } 419,
{ 196: } 421,
{ 197: } 422,
{ 198: } 424,
{ 199: } 425,
{ 200: } 426,
{ 201: } 427,
{ 202: } 428,
{ 203: } 429,
{ 204: } 430,
{ 205: } 430,
{ 206: } 430,
{ 207: } 431,
{ 208: } 432,
{ 209: } 433,
{ 210: } 434,
{ 211: } 435,
{ 212: } 437,
{ 213: } 439,
{ 214: } 441,
{ 215: } 442,
{ 216: } 444,
{ 217: } 445,
{ 218: } 447,
{ 219: } 449,
{ 220: } 451,
{ 221: } 453,
{ 222: } 455,
{ 223: } 456,
{ 224: } 457,
{ 225: } 459,
{ 226: } 461,
{ 227: } 462,
{ 228: } 464,
{ 229: } 466,
{ 230: } 467,
{ 231: } 468,
{ 232: } 469,
{ 233: } 469,
{ 234: } 469,
{ 235: } 469,
{ 236: } 470,
{ 237: } 471,
{ 238: } 472,
{ 239: } 474,
{ 240: } 476,
{ 241: } 478,
{ 242: } 479,
{ 243: } 480,
{ 244: } 481,
{ 245: } 483,
{ 246: } 485,
{ 247: } 487,
{ 248: } 488,
{ 249: } 489,
{ 250: } 491,
{ 251: } 492,
{ 252: } 493,
{ 253: } 493,
{ 254: } 493,
{ 255: } 494,
{ 256: } 496,
{ 257: } 498,
{ 258: } 500,
{ 259: } 502,
{ 260: } 504,
{ 261: } 505,
{ 262: } 506,
{ 263: } 506,
{ 264: } 507,
{ 265: } 508,
{ 266: } 510,
{ 267: } 511,
{ 268: } 512,
{ 269: } 513,
{ 270: } 514,
{ 271: } 515,
{ 272: } 516,
{ 273: } 517,
{ 274: } 518,
{ 275: } 519,
{ 276: } 520,
{ 277: } 521,
{ 278: } 522,
{ 279: } 524,
{ 280: } 526,
{ 281: } 527,
{ 282: } 528,
{ 283: } 529,
{ 284: } 530,
{ 285: } 531,
{ 286: } 532,
{ 287: } 533,
{ 288: } 534,
{ 289: } 535,
{ 290: } 536,
{ 291: } 537,
{ 292: } 538,
{ 293: } 539,
{ 294: } 540,
{ 295: } 541,
{ 296: } 541,
{ 297: } 542,
{ 298: } 543,
{ 299: } 544,
{ 300: } 545,
{ 301: } 546,
{ 302: } 547,
{ 303: } 548,
{ 304: } 549,
{ 305: } 550,
{ 306: } 551
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
{ 8: } 128,
{ 9: } 129,
{ 10: } 130,
{ 11: } 131,
{ 12: } 133,
{ 13: } 135,
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
{ 26: } 143,
{ 27: } 144,
{ 28: } 147,
{ 29: } 150,
{ 30: } 154,
{ 31: } 156,
{ 32: } 158,
{ 33: } 160,
{ 34: } 162,
{ 35: } 164,
{ 36: } 167,
{ 37: } 169,
{ 38: } 173,
{ 39: } 173,
{ 40: } 173,
{ 41: } 175,
{ 42: } 177,
{ 43: } 179,
{ 44: } 182,
{ 45: } 184,
{ 46: } 186,
{ 47: } 188,
{ 48: } 190,
{ 49: } 192,
{ 50: } 193,
{ 51: } 193,
{ 52: } 193,
{ 53: } 193,
{ 54: } 193,
{ 55: } 193,
{ 56: } 193,
{ 57: } 195,
{ 58: } 195,
{ 59: } 197,
{ 60: } 197,
{ 61: } 199,
{ 62: } 201,
{ 63: } 202,
{ 64: } 207,
{ 65: } 208,
{ 66: } 208,
{ 67: } 209,
{ 68: } 211,
{ 69: } 212,
{ 70: } 215,
{ 71: } 215,
{ 72: } 215,
{ 73: } 215,
{ 74: } 215,
{ 75: } 215,
{ 76: } 215,
{ 77: } 215,
{ 78: } 215,
{ 79: } 217,
{ 80: } 223,
{ 81: } 226,
{ 82: } 227,
{ 83: } 234,
{ 84: } 235,
{ 85: } 236,
{ 86: } 237,
{ 87: } 239,
{ 88: } 241,
{ 89: } 243,
{ 90: } 245,
{ 91: } 247,
{ 92: } 249,
{ 93: } 251,
{ 94: } 254,
{ 95: } 256,
{ 96: } 258,
{ 97: } 260,
{ 98: } 262,
{ 99: } 264,
{ 100: } 266,
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
{ 114: } 295,
{ 115: } 295,
{ 116: } 295,
{ 117: } 297,
{ 118: } 298,
{ 119: } 299,
{ 120: } 300,
{ 121: } 300,
{ 122: } 301,
{ 123: } 302,
{ 124: } 304,
{ 125: } 306,
{ 126: } 307,
{ 127: } 308,
{ 128: } 309,
{ 129: } 311,
{ 130: } 312,
{ 131: } 313,
{ 132: } 313,
{ 133: } 315,
{ 134: } 317,
{ 135: } 319,
{ 136: } 321,
{ 137: } 323,
{ 138: } 325,
{ 139: } 327,
{ 140: } 329,
{ 141: } 331,
{ 142: } 334,
{ 143: } 336,
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
{ 154: } 357,
{ 155: } 359,
{ 156: } 361,
{ 157: } 362,
{ 158: } 363,
{ 159: } 365,
{ 160: } 367,
{ 161: } 369,
{ 162: } 371,
{ 163: } 372,
{ 164: } 373,
{ 165: } 373,
{ 166: } 374,
{ 167: } 375,
{ 168: } 376,
{ 169: } 377,
{ 170: } 378,
{ 171: } 378,
{ 172: } 379,
{ 173: } 380,
{ 174: } 382,
{ 175: } 383,
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
{ 186: } 404,
{ 187: } 405,
{ 188: } 406,
{ 189: } 408,
{ 190: } 410,
{ 191: } 412,
{ 192: } 414,
{ 193: } 416,
{ 194: } 418,
{ 195: } 420,
{ 196: } 421,
{ 197: } 423,
{ 198: } 424,
{ 199: } 425,
{ 200: } 426,
{ 201: } 427,
{ 202: } 428,
{ 203: } 429,
{ 204: } 429,
{ 205: } 429,
{ 206: } 430,
{ 207: } 431,
{ 208: } 432,
{ 209: } 433,
{ 210: } 434,
{ 211: } 436,
{ 212: } 438,
{ 213: } 440,
{ 214: } 441,
{ 215: } 443,
{ 216: } 444,
{ 217: } 446,
{ 218: } 448,
{ 219: } 450,
{ 220: } 452,
{ 221: } 454,
{ 222: } 455,
{ 223: } 456,
{ 224: } 458,
{ 225: } 460,
{ 226: } 461,
{ 227: } 463,
{ 228: } 465,
{ 229: } 466,
{ 230: } 467,
{ 231: } 468,
{ 232: } 468,
{ 233: } 468,
{ 234: } 468,
{ 235: } 469,
{ 236: } 470,
{ 237: } 471,
{ 238: } 473,
{ 239: } 475,
{ 240: } 477,
{ 241: } 478,
{ 242: } 479,
{ 243: } 480,
{ 244: } 482,
{ 245: } 484,
{ 246: } 486,
{ 247: } 487,
{ 248: } 488,
{ 249: } 490,
{ 250: } 491,
{ 251: } 492,
{ 252: } 492,
{ 253: } 492,
{ 254: } 493,
{ 255: } 495,
{ 256: } 497,
{ 257: } 499,
{ 258: } 501,
{ 259: } 503,
{ 260: } 504,
{ 261: } 505,
{ 262: } 505,
{ 263: } 506,
{ 264: } 507,
{ 265: } 509,
{ 266: } 510,
{ 267: } 511,
{ 268: } 512,
{ 269: } 513,
{ 270: } 514,
{ 271: } 515,
{ 272: } 516,
{ 273: } 517,
{ 274: } 518,
{ 275: } 519,
{ 276: } 520,
{ 277: } 521,
{ 278: } 523,
{ 279: } 525,
{ 280: } 526,
{ 281: } 527,
{ 282: } 528,
{ 283: } 529,
{ 284: } 530,
{ 285: } 531,
{ 286: } 532,
{ 287: } 533,
{ 288: } 534,
{ 289: } 535,
{ 290: } 536,
{ 291: } 537,
{ 292: } 538,
{ 293: } 539,
{ 294: } 540,
{ 295: } 540,
{ 296: } 541,
{ 297: } 542,
{ 298: } 543,
{ 299: } 544,
{ 300: } 545,
{ 301: } 546,
{ 302: } 547,
{ 303: } 548,
{ 304: } 549,
{ 305: } 550,
{ 306: } 550
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

