
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
          { dtyp : tdtyp; }
          constructor init_no(t : ttyp);
          constructor init_one(t : ttyp;_p1 : presobject);
          constructor init_two(t : ttyp;_p1,_p2 : presobject);
          constructor init_three(t : ttyp;_p1,_p2,_p3 : presobject);
          constructor init_id(const s : string);
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
                           if yytext[length(yytext)]='L' then
                             dec(byte(yytext[0]));
                           if yytext[length(yytext)]='U' then
                             dec(byte(yytext[0]));
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
                           if yytext[length(yytext)]='L' then
                             dec(byte(yytext[0]));
                           if yytext[length(yytext)]='U' then
                             dec(byte(yytext[0]));
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
                        return(_NOT);
  24:
                        return(_SLASH);
  25:
                        return(_PLUS);
  26:
                        return(QUESTIONMARK);
  27:
                        return(COLON);
  28:
                        return(COMMA);
  29:
                        return(LECKKLAMMER);
  30:
                        return(RECKKLAMMER);
  31:
                        begin
                           inc(arglevel);
                           return(LKLAMMER);
                        end;
  32:
                        begin
                           dec(arglevel);
                           return(RKLAMMER);
                        end;
  33:
                        return(STAR);
  34:
                        return(ELLIPSIS);
  35:
                        if in_define then
                          return(POINT)
                        else
                          return(256);
  36:
                        return(_ASSIGN);
  37:
                        return(EXTERN);
  38:
                        if Win32headers then
                          return(STDCALL)
                        else
                          return(ID);
  39:
                        if not Win32headers then
                          return(ID)
                        else
                          return(CDECL);
  40:
                        if not Win32headers then
                          return(ID)
                        else
                          return(PASCAL);
  41:
                        if not Win32headers then
                          return(ID)
                        else
                          return(_PACKED);
  42:
                        if not Win32headers then
                          return(ID)
                        else
                          return(WINAPI);
  43:
                        if not palmpilot then
                          return(ID)
                        else
                          return(SYS_TRAP);
  44:
                        if not Win32headers then
                          return(ID)
                        else
                          return(WINGDIAPI);
  45:
                        if not Win32headers then
                          return(ID)
                        else
                          return(CALLBACK);
  46:
                        if not Win32headers then
                          return(ID)
                        else
                          return(CALLBACK);
  47:
                        return(VOID);
  48:
                        return(VOID);
  49:

                        begin
                          if not stripinfo then
                            writeln(outfile,'{ C++ extern C conditionnal removed }');
                        end;
  50:

                        begin
                          if not stripinfo then
                            writeln(outfile,'{ C++ end of extern C conditionnal removed }');
                        end;
  51:
                        begin
                           writeln(outfile,'{$else}');
                           block_type:=bt_no;
                           flush(outfile);
                        end;
  52:
                        begin
                           writeln(outfile,'{$endif}');
                           block_type:=bt_no;
                           flush(outfile);
                        end;
  53:
                        begin
                           if not stripinfo then
                             write(outfile,'(*** was #elif ****)');
                           write(outfile,'{$else');
                           copy_until_eol;
                           writeln(outfile,'}');
                           block_type:=bt_no;
                           flush(outfile);
                        end;
  54:
                        begin
                           write(outfile,'{$undef');
                           copy_until_eol;
                           writeln(outfile,'}');
                           flush(outfile);
                        end;
  55:
                        begin
                           write(outfile,'{$error');
                           copy_until_eol;
                           writeln(outfile,'}');
                           flush(outfile);
                        end;
  56:
                        begin
                           write(outfile,'{$include');
                           copy_until_eol;
                           writeln(outfile,'}');
                           flush(outfile);
                           block_type:=bt_no;
                        end;
  57:
                        begin
                           write(outfile,'{$if');
                           copy_until_eol;
                           writeln(outfile,'}');
                           flush(outfile);
                           block_type:=bt_no;
                        end;
  58:
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
  59:
                        begin
                           in_define:=true;
                           in_space_define:=1;
                           return(DEFINE);
                        end;
  60:
                        return(_CHAR);
  61:
                        return(UNION);
  62:
                        return(ENUM);
  63:
                        return(STRUCT);
  64:
                        return(LGKLAMMER);
  65:
                        return(RGKLAMMER);
  66:
                        return(TYPEDEF);
  67:
                        return(INT);
  68:
                        return(SHORT);
  69:
                        return(LONG);
  70:
                        return(UNSIGNED);
  71:
                        return(REAL);
  72:
                        return(_CONST);
  73:
                        return(_CONST);
  74:
                        return(_FAR);
  75:
                        return(_FAR);
  76:
                        return(_NEAR);
  77:
                        return(_NEAR);
  78:
                        return(_HUGE);
  79:
                        return(_HUGE);
  80:
                        begin
                           if in_space_define=1 then
                             in_space_define:=2;
                           return(ID);
                        end;
  81:
                        return(SEMICOLON);
  82:
                        begin
                           if (arglevel=0) and (in_space_define=2) then
                            begin
                              in_space_define:=0;
                              return(SPACE_DEFINE);
                            end;
                        end;
  83:
                        begin
                           if in_define then
                            begin
                              in_define:=false;
                              in_space_define:=0;
                              return(NEW_LINE);
                            end;
                        end;
  84:
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

yynmarks   = 300;
yynmatches = 300;
yyntrans   = 528;
yynstates  = 298;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  7,
  { 1: }
  7,
  { 2: }
  24,
  84,
  { 3: }
  84,
  { 4: }
  84,
  { 5: }
  7,
  80,
  84,
  { 6: }
  7,
  9,
  84,
  { 7: }
  7,
  80,
  84,
  { 8: }
  7,
  9,
  84,
  { 9: }
  11,
  84,
  { 10: }
  36,
  84,
  { 11: }
  23,
  84,
  { 12: }
  19,
  84,
  { 13: }
  20,
  84,
  { 14: }
  84,
  { 15: }
  21,
  84,
  { 16: }
  22,
  84,
  { 17: }
  25,
  84,
  { 18: }
  26,
  84,
  { 19: }
  27,
  84,
  { 20: }
  28,
  84,
  { 21: }
  29,
  84,
  { 22: }
  30,
  84,
  { 23: }
  31,
  84,
  { 24: }
  32,
  84,
  { 25: }
  33,
  84,
  { 26: }
  35,
  84,
  { 27: }
  80,
  84,
  { 28: }
  80,
  84,
  { 29: }
  80,
  84,
  { 30: }
  80,
  84,
  { 31: }
  80,
  84,
  { 32: }
  80,
  84,
  { 33: }
  80,
  84,
  { 34: }
  80,
  84,
  { 35: }
  80,
  84,
  { 36: }
  80,
  84,
  { 37: }
  80,
  84,
  { 38: }
  64,
  84,
  { 39: }
  65,
  84,
  { 40: }
  80,
  84,
  { 41: }
  80,
  84,
  { 42: }
  80,
  84,
  { 43: }
  80,
  84,
  { 44: }
  80,
  84,
  { 45: }
  80,
  84,
  { 46: }
  80,
  84,
  { 47: }
  80,
  84,
  { 48: }
  80,
  84,
  { 49: }
  80,
  84,
  { 50: }
  81,
  84,
  { 51: }
  82,
  84,
  { 52: }
  83,
  { 53: }
  84,
  { 54: }
  1,
  { 55: }
  2,
  { 56: }
  { 57: }
  3,
  { 58: }
  { 59: }
  4,
  { 60: }
  { 61: }
  { 62: }
  80,
  { 63: }
  7,
  9,
  { 64: }
  7,
  { 65: }
  7,
  { 66: }
  { 67: }
  { 68: }
  7,
  80,
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
  80,
  { 86: }
  80,
  { 87: }
  80,
  { 88: }
  80,
  { 89: }
  80,
  { 90: }
  80,
  { 91: }
  80,
  { 92: }
  80,
  { 93: }
  80,
  { 94: }
  80,
  { 95: }
  80,
  { 96: }
  80,
  { 97: }
  80,
  { 98: }
  80,
  { 99: }
  80,
  { 100: }
  80,
  { 101: }
  80,
  { 102: }
  80,
  { 103: }
  80,
  { 104: }
  80,
  { 105: }
  80,
  { 106: }
  80,
  { 107: }
  80,
  { 108: }
  80,
  { 109: }
  80,
  { 110: }
  80,
  { 111: }
  80,
  { 112: }
  5,
  { 113: }
  6,
  { 114: }
  9,
  { 115: }
  { 116: }
  9,
  { 117: }
  8,
  { 118: }
  8,
  { 119: }
  57,
  { 120: }
  { 121: }
  { 122: }
  { 123: }
  { 124: }
  { 125: }
  { 126: }
  { 127: }
  { 128: }
  34,
  { 129: }
  80,
  { 130: }
  80,
  { 131: }
  80,
  { 132: }
  80,
  { 133: }
  80,
  { 134: }
  80,
  { 135: }
  80,
  { 136: }
  80,
  { 137: }
  80,
  { 138: }
  80,
  { 139: }
  80,
  { 140: }
  80,
  { 141: }
  80,
  { 142: }
  80,
  { 143: }
  80,
  { 144: }
  80,
  { 145: }
  80,
  { 146: }
  80,
  { 147: }
  80,
  { 148: }
  80,
  { 149: }
  67,
  80,
  { 150: }
  80,
  { 151: }
  80,
  { 152: }
  75,
  80,
  { 153: }
  74,
  80,
  { 154: }
  80,
  { 155: }
  80,
  { 156: }
  80,
  { 157: }
  80,
  { 158: }
  { 159: }
  { 160: }
  57,
  { 161: }
  { 162: }
  { 163: }
  { 164: }
  { 165: }
  { 166: }
  { 167: }
  { 168: }
  80,
  { 169: }
  62,
  80,
  { 170: }
  80,
  { 171: }
  80,
  { 172: }
  80,
  { 173: }
  80,
  { 174: }
  80,
  { 175: }
  80,
  { 176: }
  80,
  { 177: }
  80,
  { 178: }
  80,
  { 179: }
  80,
  { 180: }
  47,
  80,
  { 181: }
  48,
  80,
  { 182: }
  60,
  80,
  { 183: }
  80,
  { 184: }
  80,
  { 185: }
  80,
  { 186: }
  80,
  { 187: }
  80,
  { 188: }
  80,
  { 189: }
  69,
  80,
  { 190: }
  80,
  { 191: }
  76,
  80,
  { 192: }
  77,
  80,
  { 193: }
  78,
  80,
  { 194: }
  79,
  80,
  { 195: }
  { 196: }
  { 197: }
  51,
  { 198: }
  53,
  { 199: }
  { 200: }
  { 201: }
  { 202: }
  { 203: }
  { 204: }
  80,
  { 205: }
  80,
  { 206: }
  80,
  { 207: }
  39,
  80,
  { 208: }
  80,
  { 209: }
  73,
  80,
  { 210: }
  80,
  { 211: }
  80,
  { 212: }
  80,
  { 213: }
  80,
  { 214: }
  80,
  { 215: }
  72,
  80,
  { 216: }
  61,
  80,
  { 217: }
  80,
  { 218: }
  80,
  { 219: }
  68,
  80,
  { 220: }
  80,
  { 221: }
  71,
  80,
  { 222: }
  { 223: }
  { 224: }
  52,
  { 225: }
  55,
  { 226: }
  54,
  { 227: }
  { 228: }
  { 229: }
  37,
  80,
  { 230: }
  80,
  { 231: }
  80,
  { 232: }
  80,
  { 233: }
  40,
  80,
  { 234: }
  41,
  80,
  { 235: }
  42,
  80,
  { 236: }
  80,
  { 237: }
  80,
  { 238: }
  80,
  { 239: }
  63,
  80,
  { 240: }
  80,
  { 241: }
  { 242: }
  { 243: }
  58,
  { 244: }
  59,
  { 245: }
  38,
  80,
  { 246: }
  80,
  { 247: }
  80,
  { 248: }
  80,
  { 249: }
  80,
  { 250: }
  80,
  { 251: }
  66,
  80,
  { 252: }
  { 253: }
  56,
  { 254: }
  43,
  80,
  { 255: }
  45,
  80,
  { 256: }
  80,
  { 257: }
  46,
  80,
  { 258: }
  70,
  80,
  { 259: }
  { 260: }
  44,
  80,
  { 261: }
  { 262: }
  { 263: }
  { 264: }
  { 265: }
  { 266: }
  { 267: }
  { 268: }
  { 269: }
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
  50,
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
  49
);

yym : array [1..yynmatches] of Integer = (
{ 0: }
  7,
{ 1: }
  7,
{ 2: }
  24,
  84,
{ 3: }
  84,
{ 4: }
  84,
{ 5: }
  7,
  80,
  84,
{ 6: }
  7,
  9,
  84,
{ 7: }
  7,
  80,
  84,
{ 8: }
  7,
  9,
  84,
{ 9: }
  11,
  84,
{ 10: }
  36,
  84,
{ 11: }
  23,
  84,
{ 12: }
  19,
  84,
{ 13: }
  20,
  84,
{ 14: }
  84,
{ 15: }
  21,
  84,
{ 16: }
  22,
  84,
{ 17: }
  25,
  84,
{ 18: }
  26,
  84,
{ 19: }
  27,
  84,
{ 20: }
  28,
  84,
{ 21: }
  29,
  84,
{ 22: }
  30,
  84,
{ 23: }
  31,
  84,
{ 24: }
  32,
  84,
{ 25: }
  33,
  84,
{ 26: }
  35,
  84,
{ 27: }
  80,
  84,
{ 28: }
  80,
  84,
{ 29: }
  80,
  84,
{ 30: }
  80,
  84,
{ 31: }
  80,
  84,
{ 32: }
  80,
  84,
{ 33: }
  80,
  84,
{ 34: }
  80,
  84,
{ 35: }
  80,
  84,
{ 36: }
  80,
  84,
{ 37: }
  80,
  84,
{ 38: }
  64,
  84,
{ 39: }
  65,
  84,
{ 40: }
  80,
  84,
{ 41: }
  80,
  84,
{ 42: }
  80,
  84,
{ 43: }
  80,
  84,
{ 44: }
  80,
  84,
{ 45: }
  80,
  84,
{ 46: }
  80,
  84,
{ 47: }
  80,
  84,
{ 48: }
  80,
  84,
{ 49: }
  80,
  84,
{ 50: }
  81,
  84,
{ 51: }
  82,
  84,
{ 52: }
  83,
{ 53: }
  84,
{ 54: }
  1,
{ 55: }
  2,
{ 56: }
{ 57: }
  3,
{ 58: }
{ 59: }
  4,
{ 60: }
{ 61: }
{ 62: }
  80,
{ 63: }
  7,
  9,
{ 64: }
  7,
{ 65: }
  7,
{ 66: }
{ 67: }
{ 68: }
  7,
  80,
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
  80,
{ 86: }
  80,
{ 87: }
  80,
{ 88: }
  80,
{ 89: }
  80,
{ 90: }
  80,
{ 91: }
  80,
{ 92: }
  80,
{ 93: }
  80,
{ 94: }
  80,
{ 95: }
  80,
{ 96: }
  80,
{ 97: }
  80,
{ 98: }
  80,
{ 99: }
  80,
{ 100: }
  80,
{ 101: }
  80,
{ 102: }
  80,
{ 103: }
  80,
{ 104: }
  80,
{ 105: }
  80,
{ 106: }
  80,
{ 107: }
  80,
{ 108: }
  80,
{ 109: }
  80,
{ 110: }
  80,
{ 111: }
  80,
{ 112: }
  5,
{ 113: }
  6,
{ 114: }
  9,
{ 115: }
{ 116: }
  9,
{ 117: }
  8,
{ 118: }
  8,
{ 119: }
  57,
{ 120: }
{ 121: }
{ 122: }
{ 123: }
{ 124: }
{ 125: }
{ 126: }
{ 127: }
{ 128: }
  34,
{ 129: }
  80,
{ 130: }
  80,
{ 131: }
  80,
{ 132: }
  80,
{ 133: }
  80,
{ 134: }
  80,
{ 135: }
  80,
{ 136: }
  80,
{ 137: }
  80,
{ 138: }
  80,
{ 139: }
  80,
{ 140: }
  80,
{ 141: }
  80,
{ 142: }
  80,
{ 143: }
  80,
{ 144: }
  80,
{ 145: }
  80,
{ 146: }
  80,
{ 147: }
  80,
{ 148: }
  80,
{ 149: }
  67,
  80,
{ 150: }
  80,
{ 151: }
  80,
{ 152: }
  75,
  80,
{ 153: }
  74,
  80,
{ 154: }
  80,
{ 155: }
  80,
{ 156: }
  80,
{ 157: }
  80,
{ 158: }
{ 159: }
{ 160: }
  57,
{ 161: }
{ 162: }
{ 163: }
{ 164: }
{ 165: }
{ 166: }
{ 167: }
{ 168: }
  80,
{ 169: }
  62,
  80,
{ 170: }
  80,
{ 171: }
  80,
{ 172: }
  80,
{ 173: }
  80,
{ 174: }
  80,
{ 175: }
  80,
{ 176: }
  80,
{ 177: }
  80,
{ 178: }
  80,
{ 179: }
  80,
{ 180: }
  47,
  80,
{ 181: }
  48,
  80,
{ 182: }
  60,
  80,
{ 183: }
  80,
{ 184: }
  80,
{ 185: }
  80,
{ 186: }
  80,
{ 187: }
  80,
{ 188: }
  80,
{ 189: }
  69,
  80,
{ 190: }
  80,
{ 191: }
  76,
  80,
{ 192: }
  77,
  80,
{ 193: }
  78,
  80,
{ 194: }
  79,
  80,
{ 195: }
{ 196: }
{ 197: }
  51,
{ 198: }
  53,
{ 199: }
{ 200: }
{ 201: }
{ 202: }
{ 203: }
{ 204: }
  80,
{ 205: }
  80,
{ 206: }
  80,
{ 207: }
  39,
  80,
{ 208: }
  80,
{ 209: }
  73,
  80,
{ 210: }
  80,
{ 211: }
  80,
{ 212: }
  80,
{ 213: }
  80,
{ 214: }
  80,
{ 215: }
  72,
  80,
{ 216: }
  61,
  80,
{ 217: }
  80,
{ 218: }
  80,
{ 219: }
  68,
  80,
{ 220: }
  80,
{ 221: }
  71,
  80,
{ 222: }
{ 223: }
{ 224: }
  52,
{ 225: }
  55,
{ 226: }
  54,
{ 227: }
{ 228: }
{ 229: }
  37,
  80,
{ 230: }
  80,
{ 231: }
  80,
{ 232: }
  80,
{ 233: }
  40,
  80,
{ 234: }
  41,
  80,
{ 235: }
  42,
  80,
{ 236: }
  80,
{ 237: }
  80,
{ 238: }
  80,
{ 239: }
  63,
  80,
{ 240: }
  80,
{ 241: }
{ 242: }
{ 243: }
  58,
{ 244: }
  59,
{ 245: }
  38,
  80,
{ 246: }
  80,
{ 247: }
  80,
{ 248: }
  80,
{ 249: }
  80,
{ 250: }
  80,
{ 251: }
  66,
  80,
{ 252: }
{ 253: }
  56,
{ 254: }
  43,
  80,
{ 255: }
  45,
  80,
{ 256: }
  80,
{ 257: }
  46,
  80,
{ 258: }
  70,
  80,
{ 259: }
{ 260: }
  44,
  80,
{ 261: }
{ 262: }
{ 263: }
{ 264: }
{ 265: }
{ 266: }
{ 267: }
{ 268: }
{ 269: }
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
  50,
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
  49
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
  ( cc: [ #1..#8,#11,#13..#31,'$','%','@','\','^','`',
            '~'..#255 ]; s: 53),
  ( cc: [ #9,#12,' ' ]; s: 51),
  ( cc: [ #10 ]; s: 52),
  ( cc: [ '!' ]; s: 11),
  ( cc: [ '"' ]; s: 3),
  ( cc: [ '#' ]; s: 14),
  ( cc: [ '&' ]; s: 16),
  ( cc: [ '''' ]; s: 4),
  ( cc: [ '(' ]; s: 23),
  ( cc: [ ')' ]; s: 24),
  ( cc: [ '*' ]; s: 25),
  ( cc: [ '+' ]; s: 17),
  ( cc: [ ',' ]; s: 20),
  ( cc: [ '-' ]; s: 9),
  ( cc: [ '.' ]; s: 26),
  ( cc: [ '/' ]; s: 2),
  ( cc: [ '0' ]; s: 8),
  ( cc: [ '1'..'9' ]; s: 6),
  ( cc: [ ':' ]; s: 19),
  ( cc: [ ';' ]; s: 50),
  ( cc: [ '<' ]; s: 13),
  ( cc: [ '=' ]; s: 10),
  ( cc: [ '>' ]; s: 12),
  ( cc: [ '?' ]; s: 18),
  ( cc: [ 'A','B','D','G','I'..'K','M','O','Q','R',
            'T','X'..'Z','_','a','b','d','g','j','k',
            'm','o'..'r','w'..'z' ]; s: 49),
  ( cc: [ 'C' ]; s: 29),
  ( cc: [ 'E' ]; s: 32),
  ( cc: [ 'F' ]; s: 44),
  ( cc: [ 'H' ]; s: 47),
  ( cc: [ 'L' ]; s: 5),
  ( cc: [ 'N' ]; s: 45),
  ( cc: [ 'P' ]; s: 30),
  ( cc: [ 'S' ]; s: 28),
  ( cc: [ 'U' ]; s: 7),
  ( cc: [ 'V' ]; s: 34),
  ( cc: [ 'W' ]; s: 31),
  ( cc: [ '[' ]; s: 21),
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
  ( cc: [ '|' ]; s: 15),
  ( cc: [ '}' ]; s: 39),
{ 1: }
  ( cc: [ #1..#8,#11,#13..#31,'$','%','@','\','^','`',
            '~'..#255 ]; s: 53),
  ( cc: [ #9,#12,' ' ]; s: 51),
  ( cc: [ #10 ]; s: 52),
  ( cc: [ '!' ]; s: 11),
  ( cc: [ '"' ]; s: 3),
  ( cc: [ '#' ]; s: 14),
  ( cc: [ '&' ]; s: 16),
  ( cc: [ '''' ]; s: 4),
  ( cc: [ '(' ]; s: 23),
  ( cc: [ ')' ]; s: 24),
  ( cc: [ '*' ]; s: 25),
  ( cc: [ '+' ]; s: 17),
  ( cc: [ ',' ]; s: 20),
  ( cc: [ '-' ]; s: 9),
  ( cc: [ '.' ]; s: 26),
  ( cc: [ '/' ]; s: 2),
  ( cc: [ '0' ]; s: 8),
  ( cc: [ '1'..'9' ]; s: 6),
  ( cc: [ ':' ]; s: 19),
  ( cc: [ ';' ]; s: 50),
  ( cc: [ '<' ]; s: 13),
  ( cc: [ '=' ]; s: 10),
  ( cc: [ '>' ]; s: 12),
  ( cc: [ '?' ]; s: 18),
  ( cc: [ 'A','B','D','G','I'..'K','M','O','Q','R',
            'T','X'..'Z','_','a','b','d','g','j','k',
            'm','o'..'r','w'..'z' ]; s: 49),
  ( cc: [ 'C' ]; s: 29),
  ( cc: [ 'E' ]; s: 32),
  ( cc: [ 'F' ]; s: 44),
  ( cc: [ 'H' ]; s: 47),
  ( cc: [ 'L' ]; s: 5),
  ( cc: [ 'N' ]; s: 45),
  ( cc: [ 'P' ]; s: 30),
  ( cc: [ 'S' ]; s: 28),
  ( cc: [ 'U' ]; s: 7),
  ( cc: [ 'V' ]; s: 34),
  ( cc: [ 'W' ]; s: 31),
  ( cc: [ '[' ]; s: 21),
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
  ( cc: [ '|' ]; s: 15),
  ( cc: [ '}' ]; s: 39),
{ 2: }
  ( cc: [ '*' ]; s: 54),
  ( cc: [ '/' ]; s: 55),
{ 3: }
  ( cc: [ #1..'!','#'..#255 ]; s: 56),
  ( cc: [ '"' ]; s: 57),
{ 4: }
  ( cc: [ #1..'&','('..#255 ]; s: 58),
  ( cc: [ '''' ]; s: 59),
{ 5: }
  ( cc: [ '"' ]; s: 60),
  ( cc: [ '''' ]; s: 61),
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 6: }
  ( cc: [ '.' ]; s: 66),
  ( cc: [ '0'..'9' ]; s: 63),
  ( cc: [ 'E','e' ]; s: 67),
  ( cc: [ 'L' ]; s: 65),
  ( cc: [ 'U' ]; s: 64),
{ 7: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'L' ]; s: 68),
{ 8: }
  ( cc: [ '.' ]; s: 66),
  ( cc: [ '0'..'9' ]; s: 63),
  ( cc: [ 'E','e' ]; s: 67),
  ( cc: [ 'L' ]; s: 65),
  ( cc: [ 'U' ]; s: 64),
  ( cc: [ 'x' ]; s: 69),
{ 9: }
  ( cc: [ '>' ]; s: 70),
{ 10: }
  ( cc: [ '=' ]; s: 71),
{ 11: }
  ( cc: [ '=' ]; s: 72),
{ 12: }
  ( cc: [ '=' ]; s: 73),
  ( cc: [ '>' ]; s: 74),
{ 13: }
  ( cc: [ '<' ]; s: 76),
  ( cc: [ '=' ]; s: 75),
{ 14: }
  ( cc: [ #9,' ' ]; s: 79),
  ( cc: [ '#' ]; s: 77),
  ( cc: [ 'd' ]; s: 83),
  ( cc: [ 'e' ]; s: 80),
  ( cc: [ 'i' ]; s: 78),
  ( cc: [ 'p' ]; s: 82),
  ( cc: [ 'u' ]; s: 81),
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
  ( cc: [ '.' ]; s: 84),
{ 27: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'w','y','z' ]; s: 62),
  ( cc: [ 'n' ]; s: 86),
  ( cc: [ 'x' ]; s: 85),
{ 28: }
  ( cc: [ '0'..'9','A'..'S','U'..'X','Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'T' ]; s: 87),
  ( cc: [ 'Y' ]; s: 88),
{ 29: }
  ( cc: [ '0'..'9','B','C','E'..'N','P'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 90),
  ( cc: [ 'D' ]; s: 89),
  ( cc: [ 'O' ]; s: 91),
{ 30: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 92),
{ 31: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'I' ]; s: 93),
{ 32: }
  ( cc: [ '0'..'9','A'..'W','Y','Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'X' ]; s: 94),
{ 33: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 62),
  ( cc: [ 'o' ]; s: 95),
{ 34: }
  ( cc: [ '0'..'9','A'..'N','P'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'O' ]; s: 96),
{ 35: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','i'..'n','p'..'z' ]; s: 62),
  ( cc: [ 'h' ]; s: 97),
  ( cc: [ 'o' ]; s: 98),
{ 36: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 62),
  ( cc: [ 'n' ]; s: 99),
{ 37: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','i'..'s','u'..'z' ]; s: 62),
  ( cc: [ 'h' ]; s: 101),
  ( cc: [ 't' ]; s: 100),
{ 38: }
{ 39: }
{ 40: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'x','z' ]; s: 62),
  ( cc: [ 'y' ]; s: 102),
{ 41: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 62),
  ( cc: [ 'n' ]; s: 103),
{ 42: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 62),
  ( cc: [ 'o' ]; s: 104),
{ 43: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'k','m'..'z' ]; s: 62),
  ( cc: [ 'a' ]; s: 106),
  ( cc: [ 'l' ]; s: 105),
{ 44: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 107),
{ 45: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'E' ]; s: 108),
{ 46: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 62),
  ( cc: [ 'e' ]; s: 109),
{ 47: }
  ( cc: [ '0'..'9','A'..'T','V'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'U' ]; s: 110),
{ 48: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 62),
  ( cc: [ 'u' ]; s: 111),
{ 49: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 50: }
{ 51: }
{ 52: }
{ 53: }
{ 54: }
{ 55: }
{ 56: }
  ( cc: [ #1..'!','#'..#255 ]; s: 56),
  ( cc: [ '"' ]; s: 57),
{ 57: }
{ 58: }
  ( cc: [ #1..'&','('..#255 ]; s: 58),
  ( cc: [ '''' ]; s: 59),
{ 59: }
{ 60: }
  ( cc: [ #1..'!','#'..#255 ]; s: 60),
  ( cc: [ '"' ]; s: 112),
{ 61: }
  ( cc: [ #1..'&','('..#255 ]; s: 61),
  ( cc: [ '''' ]; s: 113),
{ 62: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 63: }
  ( cc: [ '.' ]; s: 66),
  ( cc: [ '0'..'9' ]; s: 63),
  ( cc: [ 'E','e' ]; s: 67),
  ( cc: [ 'L' ]; s: 65),
  ( cc: [ 'U' ]; s: 64),
{ 64: }
  ( cc: [ 'L' ]; s: 65),
{ 65: }
{ 66: }
  ( cc: [ '0'..'9' ]; s: 114),
{ 67: }
  ( cc: [ '+','-' ]; s: 115),
  ( cc: [ '0'..'9' ]; s: 116),
{ 68: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 69: }
  ( cc: [ '0'..'9','A'..'F','a'..'f' ]; s: 69),
  ( cc: [ 'L' ]; s: 118),
  ( cc: [ 'U' ]; s: 117),
{ 70: }
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
{ 77: }
{ 78: }
  ( cc: [ 'f' ]; s: 119),
  ( cc: [ 'n' ]; s: 120),
{ 79: }
  ( cc: [ #9,' ' ]; s: 79),
  ( cc: [ 'd' ]; s: 83),
  ( cc: [ 'e' ]; s: 80),
  ( cc: [ 'i' ]; s: 121),
  ( cc: [ 'p' ]; s: 82),
  ( cc: [ 'u' ]; s: 81),
{ 80: }
  ( cc: [ 'l' ]; s: 122),
  ( cc: [ 'n' ]; s: 123),
  ( cc: [ 'r' ]; s: 124),
{ 81: }
  ( cc: [ 'n' ]; s: 125),
{ 82: }
  ( cc: [ 'r' ]; s: 126),
{ 83: }
  ( cc: [ 'e' ]; s: 127),
{ 84: }
  ( cc: [ '.' ]; s: 128),
{ 85: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 62),
  ( cc: [ 't' ]; s: 129),
{ 86: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 62),
  ( cc: [ 'u' ]; s: 130),
{ 87: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'D' ]; s: 131),
{ 88: }
  ( cc: [ '0'..'9','A'..'R','T'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'S' ]; s: 132),
{ 89: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'E' ]; s: 133),
{ 90: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'L' ]; s: 134),
{ 91: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'N' ]; s: 135),
{ 92: }
  ( cc: [ '0'..'9','A','B','D'..'R','T'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'C' ]; s: 137),
  ( cc: [ 'S' ]; s: 136),
{ 93: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'N' ]; s: 138),
{ 94: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'P' ]; s: 139),
{ 95: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'z' ]; s: 62),
  ( cc: [ 'i' ]; s: 140),
{ 96: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'I' ]; s: 141),
{ 97: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 62),
  ( cc: [ 'a' ]; s: 142),
{ 98: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 62),
  ( cc: [ 'n' ]; s: 143),
{ 99: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'r','t'..'z' ]; s: 62),
  ( cc: [ 'i' ]; s: 144),
  ( cc: [ 's' ]; s: 145),
{ 100: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 62),
  ( cc: [ 'r' ]; s: 146),
{ 101: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 62),
  ( cc: [ 'o' ]; s: 147),
{ 102: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'o','q'..'z' ]; s: 62),
  ( cc: [ 'p' ]; s: 148),
{ 103: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 62),
  ( cc: [ 't' ]; s: 149),
{ 104: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 62),
  ( cc: [ 'n' ]; s: 150),
{ 105: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 62),
  ( cc: [ 'o' ]; s: 151),
{ 106: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 62),
  ( cc: [ 'r' ]; s: 152),
{ 107: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'R' ]; s: 153),
{ 108: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 154),
{ 109: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 62),
  ( cc: [ 'a' ]; s: 155),
{ 110: }
  ( cc: [ '0'..'9','A'..'F','H'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'G' ]; s: 156),
{ 111: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 62),
  ( cc: [ 'g' ]; s: 157),
{ 112: }
{ 113: }
{ 114: }
  ( cc: [ '0'..'9' ]; s: 114),
  ( cc: [ 'E','e' ]; s: 67),
{ 115: }
  ( cc: [ '0'..'9' ]; s: 116),
{ 116: }
  ( cc: [ '0'..'9' ]; s: 116),
{ 117: }
  ( cc: [ 'L' ]; s: 118),
{ 118: }
{ 119: }
  ( cc: [ 'd' ]; s: 158),
{ 120: }
  ( cc: [ 'c' ]; s: 159),
{ 121: }
  ( cc: [ 'f' ]; s: 160),
  ( cc: [ 'n' ]; s: 120),
{ 122: }
  ( cc: [ 'i' ]; s: 162),
  ( cc: [ 's' ]; s: 161),
{ 123: }
  ( cc: [ 'd' ]; s: 163),
{ 124: }
  ( cc: [ 'r' ]; s: 164),
{ 125: }
  ( cc: [ 'd' ]; s: 165),
{ 126: }
  ( cc: [ 'a' ]; s: 166),
{ 127: }
  ( cc: [ 'f' ]; s: 167),
{ 128: }
{ 129: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 62),
  ( cc: [ 'e' ]; s: 168),
{ 130: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'l','n'..'z' ]; s: 62),
  ( cc: [ 'm' ]; s: 169),
{ 131: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'C' ]; s: 170),
{ 132: }
  ( cc: [ '0'..'9','A'..'Z','a'..'z' ]; s: 62),
  ( cc: [ '_' ]; s: 171),
{ 133: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'C' ]; s: 172),
{ 134: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'L' ]; s: 173),
{ 135: }
  ( cc: [ '0'..'9','A'..'R','T'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'S' ]; s: 174),
{ 136: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'C' ]; s: 175),
{ 137: }
  ( cc: [ '0'..'9','A'..'J','L'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'K' ]; s: 176),
{ 138: }
  ( cc: [ '0'..'9','B'..'F','H'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 177),
  ( cc: [ 'G' ]; s: 178),
{ 139: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'E' ]; s: 179),
{ 140: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 62),
  ( cc: [ 'd' ]; s: 180),
{ 141: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'D' ]; s: 181),
{ 142: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 62),
  ( cc: [ 'r' ]; s: 182),
{ 143: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'r','t'..'z' ]; s: 62),
  ( cc: [ 's' ]; s: 183),
{ 144: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 62),
  ( cc: [ 'o' ]; s: 184),
{ 145: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'z' ]; s: 62),
  ( cc: [ 'i' ]; s: 185),
{ 146: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 62),
  ( cc: [ 'u' ]; s: 186),
{ 147: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 62),
  ( cc: [ 'r' ]; s: 187),
{ 148: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 62),
  ( cc: [ 'e' ]; s: 188),
{ 149: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 150: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 62),
  ( cc: [ 'g' ]; s: 189),
{ 151: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 62),
  ( cc: [ 'a' ]; s: 190),
{ 152: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 153: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 154: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'R' ]; s: 191),
{ 155: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 62),
  ( cc: [ 'r' ]; s: 192),
{ 156: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'E' ]; s: 193),
{ 157: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 62),
  ( cc: [ 'e' ]; s: 194),
{ 158: }
  ( cc: [ 'e' ]; s: 195),
{ 159: }
  ( cc: [ 'l' ]; s: 196),
{ 160: }
{ 161: }
  ( cc: [ 'e' ]; s: 197),
{ 162: }
  ( cc: [ 'f' ]; s: 198),
{ 163: }
  ( cc: [ 'i' ]; s: 199),
{ 164: }
  ( cc: [ 'o' ]; s: 200),
{ 165: }
  ( cc: [ 'e' ]; s: 201),
{ 166: }
  ( cc: [ 'g' ]; s: 202),
{ 167: }
  ( cc: [ 'i' ]; s: 203),
{ 168: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 62),
  ( cc: [ 'r' ]; s: 204),
{ 169: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 170: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 205),
{ 171: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'T' ]; s: 206),
{ 172: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'L' ]; s: 207),
{ 173: }
  ( cc: [ '0'..'9','A','C'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'B' ]; s: 208),
{ 174: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'T' ]; s: 209),
{ 175: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 210),
{ 176: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'E' ]; s: 211),
{ 177: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'P' ]; s: 212),
{ 178: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'D' ]; s: 213),
{ 179: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'N' ]; s: 214),
{ 180: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 181: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 182: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 183: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 62),
  ( cc: [ 't' ]; s: 215),
{ 184: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 62),
  ( cc: [ 'n' ]; s: 216),
{ 185: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 62),
  ( cc: [ 'g' ]; s: 217),
{ 186: }
  ( cc: [ '0'..'9','A'..'Z','_','a','b','d'..'z' ]; s: 62),
  ( cc: [ 'c' ]; s: 218),
{ 187: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 62),
  ( cc: [ 't' ]; s: 219),
{ 188: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 62),
  ( cc: [ 'd' ]; s: 220),
{ 189: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 190: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 62),
  ( cc: [ 't' ]; s: 221),
{ 191: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 192: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 193: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 194: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 195: }
  ( cc: [ 'f' ]; s: 222),
{ 196: }
  ( cc: [ 'u' ]; s: 223),
{ 197: }
{ 198: }
{ 199: }
  ( cc: [ 'f' ]; s: 224),
{ 200: }
  ( cc: [ 'r' ]; s: 225),
{ 201: }
  ( cc: [ 'f' ]; s: 226),
{ 202: }
  ( cc: [ 'm' ]; s: 227),
{ 203: }
  ( cc: [ 'n' ]; s: 228),
{ 204: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 62),
  ( cc: [ 'n' ]; s: 229),
{ 205: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'L' ]; s: 230),
{ 206: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'R' ]; s: 231),
{ 207: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 208: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 232),
{ 209: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 210: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'L' ]; s: 233),
{ 211: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'D' ]; s: 234),
{ 212: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'I' ]; s: 235),
{ 213: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'I' ]; s: 236),
{ 214: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'T' ]; s: 237),
{ 215: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 216: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 217: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 62),
  ( cc: [ 'n' ]; s: 238),
{ 218: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 62),
  ( cc: [ 't' ]; s: 239),
{ 219: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 220: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 62),
  ( cc: [ 'e' ]; s: 240),
{ 221: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 222: }
  ( cc: [ ' ' ]; s: 241),
{ 223: }
  ( cc: [ 'd' ]; s: 242),
{ 224: }
{ 225: }
{ 226: }
{ 227: }
  ( cc: [ 'a' ]; s: 243),
{ 228: }
  ( cc: [ 'e' ]; s: 244),
{ 229: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 230: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'L' ]; s: 245),
{ 231: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 246),
{ 232: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'C' ]; s: 247),
{ 233: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 234: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 235: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 236: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 248),
{ 237: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'R' ]; s: 249),
{ 238: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 62),
  ( cc: [ 'e' ]; s: 250),
{ 239: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 240: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'e','g'..'z' ]; s: 62),
  ( cc: [ 'f' ]; s: 251),
{ 241: }
  ( cc: [ '_' ]; s: 252),
{ 242: }
  ( cc: [ 'e' ]; s: 253),
{ 243: }
{ 244: }
{ 245: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 246: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'P' ]; s: 254),
{ 247: }
  ( cc: [ '0'..'9','A'..'J','L'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'K' ]; s: 255),
{ 248: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'P' ]; s: 256),
{ 249: }
  ( cc: [ '0'..'9','A'..'X','Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'Y' ]; s: 257),
{ 250: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 62),
  ( cc: [ 'd' ]; s: 258),
{ 251: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 252: }
  ( cc: [ '_' ]; s: 259),
{ 253: }
{ 254: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 255: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 256: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'I' ]; s: 260),
{ 257: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 258: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 259: }
  ( cc: [ 'c' ]; s: 261),
{ 260: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 261: }
  ( cc: [ 'p' ]; s: 262),
{ 262: }
  ( cc: [ 'l' ]; s: 263),
{ 263: }
  ( cc: [ 'u' ]; s: 264),
{ 264: }
  ( cc: [ 's' ]; s: 265),
{ 265: }
  ( cc: [ 'p' ]; s: 266),
{ 266: }
  ( cc: [ 'l' ]; s: 267),
{ 267: }
  ( cc: [ 'u' ]; s: 268),
{ 268: }
  ( cc: [ 's' ]; s: 269),
{ 269: }
  ( cc: [ #9,' ' ]; s: 269),
  ( cc: [ #10 ]; s: 270),
{ 270: }
  ( cc: [ 'e' ]; s: 271),
  ( cc: [ '}' ]; s: 272),
{ 271: }
  ( cc: [ 'x' ]; s: 273),
{ 272: }
  ( cc: [ #10 ]; s: 274),
{ 273: }
  ( cc: [ 't' ]; s: 275),
{ 274: }
  ( cc: [ '#' ]; s: 276),
{ 275: }
  ( cc: [ 'e' ]; s: 277),
{ 276: }
  ( cc: [ 'e' ]; s: 278),
{ 277: }
  ( cc: [ 'r' ]; s: 279),
{ 278: }
  ( cc: [ 'n' ]; s: 280),
{ 279: }
  ( cc: [ 'n' ]; s: 281),
{ 280: }
  ( cc: [ 'd' ]; s: 282),
{ 281: }
  ( cc: [ ' ' ]; s: 283),
{ 282: }
  ( cc: [ 'i' ]; s: 284),
{ 283: }
  ( cc: [ '"' ]; s: 285),
{ 284: }
  ( cc: [ 'f' ]; s: 286),
{ 285: }
  ( cc: [ 'C' ]; s: 287),
{ 286: }
{ 287: }
  ( cc: [ '"' ]; s: 288),
{ 288: }
  ( cc: [ ' ' ]; s: 289),
{ 289: }
  ( cc: [ '{' ]; s: 290),
{ 290: }
  ( cc: [ #10 ]; s: 291),
{ 291: }
  ( cc: [ '#' ]; s: 292),
{ 292: }
  ( cc: [ 'e' ]; s: 293),
{ 293: }
  ( cc: [ 'n' ]; s: 294),
{ 294: }
  ( cc: [ 'd' ]; s: 295),
{ 295: }
  ( cc: [ 'i' ]; s: 296),
{ 296: }
  ( cc: [ 'f' ]; s: 297)
{ 297: }
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
{ 53: } 105,
{ 54: } 106,
{ 55: } 107,
{ 56: } 108,
{ 57: } 108,
{ 58: } 109,
{ 59: } 109,
{ 60: } 110,
{ 61: } 110,
{ 62: } 110,
{ 63: } 111,
{ 64: } 113,
{ 65: } 114,
{ 66: } 115,
{ 67: } 115,
{ 68: } 115,
{ 69: } 117,
{ 70: } 118,
{ 71: } 119,
{ 72: } 120,
{ 73: } 121,
{ 74: } 122,
{ 75: } 123,
{ 76: } 124,
{ 77: } 125,
{ 78: } 126,
{ 79: } 126,
{ 80: } 126,
{ 81: } 126,
{ 82: } 126,
{ 83: } 126,
{ 84: } 126,
{ 85: } 126,
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
{ 116: } 156,
{ 117: } 157,
{ 118: } 158,
{ 119: } 159,
{ 120: } 160,
{ 121: } 160,
{ 122: } 160,
{ 123: } 160,
{ 124: } 160,
{ 125: } 160,
{ 126: } 160,
{ 127: } 160,
{ 128: } 160,
{ 129: } 161,
{ 130: } 162,
{ 131: } 163,
{ 132: } 164,
{ 133: } 165,
{ 134: } 166,
{ 135: } 167,
{ 136: } 168,
{ 137: } 169,
{ 138: } 170,
{ 139: } 171,
{ 140: } 172,
{ 141: } 173,
{ 142: } 174,
{ 143: } 175,
{ 144: } 176,
{ 145: } 177,
{ 146: } 178,
{ 147: } 179,
{ 148: } 180,
{ 149: } 181,
{ 150: } 183,
{ 151: } 184,
{ 152: } 185,
{ 153: } 187,
{ 154: } 189,
{ 155: } 190,
{ 156: } 191,
{ 157: } 192,
{ 158: } 193,
{ 159: } 193,
{ 160: } 193,
{ 161: } 194,
{ 162: } 194,
{ 163: } 194,
{ 164: } 194,
{ 165: } 194,
{ 166: } 194,
{ 167: } 194,
{ 168: } 194,
{ 169: } 195,
{ 170: } 197,
{ 171: } 198,
{ 172: } 199,
{ 173: } 200,
{ 174: } 201,
{ 175: } 202,
{ 176: } 203,
{ 177: } 204,
{ 178: } 205,
{ 179: } 206,
{ 180: } 207,
{ 181: } 209,
{ 182: } 211,
{ 183: } 213,
{ 184: } 214,
{ 185: } 215,
{ 186: } 216,
{ 187: } 217,
{ 188: } 218,
{ 189: } 219,
{ 190: } 221,
{ 191: } 222,
{ 192: } 224,
{ 193: } 226,
{ 194: } 228,
{ 195: } 230,
{ 196: } 230,
{ 197: } 230,
{ 198: } 231,
{ 199: } 232,
{ 200: } 232,
{ 201: } 232,
{ 202: } 232,
{ 203: } 232,
{ 204: } 232,
{ 205: } 233,
{ 206: } 234,
{ 207: } 235,
{ 208: } 237,
{ 209: } 238,
{ 210: } 240,
{ 211: } 241,
{ 212: } 242,
{ 213: } 243,
{ 214: } 244,
{ 215: } 245,
{ 216: } 247,
{ 217: } 249,
{ 218: } 250,
{ 219: } 251,
{ 220: } 253,
{ 221: } 254,
{ 222: } 256,
{ 223: } 256,
{ 224: } 256,
{ 225: } 257,
{ 226: } 258,
{ 227: } 259,
{ 228: } 259,
{ 229: } 259,
{ 230: } 261,
{ 231: } 262,
{ 232: } 263,
{ 233: } 264,
{ 234: } 266,
{ 235: } 268,
{ 236: } 270,
{ 237: } 271,
{ 238: } 272,
{ 239: } 273,
{ 240: } 275,
{ 241: } 276,
{ 242: } 276,
{ 243: } 276,
{ 244: } 277,
{ 245: } 278,
{ 246: } 280,
{ 247: } 281,
{ 248: } 282,
{ 249: } 283,
{ 250: } 284,
{ 251: } 285,
{ 252: } 287,
{ 253: } 287,
{ 254: } 288,
{ 255: } 290,
{ 256: } 292,
{ 257: } 293,
{ 258: } 295,
{ 259: } 297,
{ 260: } 297,
{ 261: } 299,
{ 262: } 299,
{ 263: } 299,
{ 264: } 299,
{ 265: } 299,
{ 266: } 299,
{ 267: } 299,
{ 268: } 299,
{ 269: } 299,
{ 270: } 299,
{ 271: } 299,
{ 272: } 299,
{ 273: } 299,
{ 274: } 299,
{ 275: } 299,
{ 276: } 299,
{ 277: } 299,
{ 278: } 299,
{ 279: } 299,
{ 280: } 299,
{ 281: } 299,
{ 282: } 299,
{ 283: } 299,
{ 284: } 299,
{ 285: } 299,
{ 286: } 299,
{ 287: } 300,
{ 288: } 300,
{ 289: } 300,
{ 290: } 300,
{ 291: } 300,
{ 292: } 300,
{ 293: } 300,
{ 294: } 300,
{ 295: } 300,
{ 296: } 300,
{ 297: } 300
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
{ 52: } 104,
{ 53: } 105,
{ 54: } 106,
{ 55: } 107,
{ 56: } 107,
{ 57: } 108,
{ 58: } 108,
{ 59: } 109,
{ 60: } 109,
{ 61: } 109,
{ 62: } 110,
{ 63: } 112,
{ 64: } 113,
{ 65: } 114,
{ 66: } 114,
{ 67: } 114,
{ 68: } 116,
{ 69: } 117,
{ 70: } 118,
{ 71: } 119,
{ 72: } 120,
{ 73: } 121,
{ 74: } 122,
{ 75: } 123,
{ 76: } 124,
{ 77: } 125,
{ 78: } 125,
{ 79: } 125,
{ 80: } 125,
{ 81: } 125,
{ 82: } 125,
{ 83: } 125,
{ 84: } 125,
{ 85: } 126,
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
{ 115: } 155,
{ 116: } 156,
{ 117: } 157,
{ 118: } 158,
{ 119: } 159,
{ 120: } 159,
{ 121: } 159,
{ 122: } 159,
{ 123: } 159,
{ 124: } 159,
{ 125: } 159,
{ 126: } 159,
{ 127: } 159,
{ 128: } 160,
{ 129: } 161,
{ 130: } 162,
{ 131: } 163,
{ 132: } 164,
{ 133: } 165,
{ 134: } 166,
{ 135: } 167,
{ 136: } 168,
{ 137: } 169,
{ 138: } 170,
{ 139: } 171,
{ 140: } 172,
{ 141: } 173,
{ 142: } 174,
{ 143: } 175,
{ 144: } 176,
{ 145: } 177,
{ 146: } 178,
{ 147: } 179,
{ 148: } 180,
{ 149: } 182,
{ 150: } 183,
{ 151: } 184,
{ 152: } 186,
{ 153: } 188,
{ 154: } 189,
{ 155: } 190,
{ 156: } 191,
{ 157: } 192,
{ 158: } 192,
{ 159: } 192,
{ 160: } 193,
{ 161: } 193,
{ 162: } 193,
{ 163: } 193,
{ 164: } 193,
{ 165: } 193,
{ 166: } 193,
{ 167: } 193,
{ 168: } 194,
{ 169: } 196,
{ 170: } 197,
{ 171: } 198,
{ 172: } 199,
{ 173: } 200,
{ 174: } 201,
{ 175: } 202,
{ 176: } 203,
{ 177: } 204,
{ 178: } 205,
{ 179: } 206,
{ 180: } 208,
{ 181: } 210,
{ 182: } 212,
{ 183: } 213,
{ 184: } 214,
{ 185: } 215,
{ 186: } 216,
{ 187: } 217,
{ 188: } 218,
{ 189: } 220,
{ 190: } 221,
{ 191: } 223,
{ 192: } 225,
{ 193: } 227,
{ 194: } 229,
{ 195: } 229,
{ 196: } 229,
{ 197: } 230,
{ 198: } 231,
{ 199: } 231,
{ 200: } 231,
{ 201: } 231,
{ 202: } 231,
{ 203: } 231,
{ 204: } 232,
{ 205: } 233,
{ 206: } 234,
{ 207: } 236,
{ 208: } 237,
{ 209: } 239,
{ 210: } 240,
{ 211: } 241,
{ 212: } 242,
{ 213: } 243,
{ 214: } 244,
{ 215: } 246,
{ 216: } 248,
{ 217: } 249,
{ 218: } 250,
{ 219: } 252,
{ 220: } 253,
{ 221: } 255,
{ 222: } 255,
{ 223: } 255,
{ 224: } 256,
{ 225: } 257,
{ 226: } 258,
{ 227: } 258,
{ 228: } 258,
{ 229: } 260,
{ 230: } 261,
{ 231: } 262,
{ 232: } 263,
{ 233: } 265,
{ 234: } 267,
{ 235: } 269,
{ 236: } 270,
{ 237: } 271,
{ 238: } 272,
{ 239: } 274,
{ 240: } 275,
{ 241: } 275,
{ 242: } 275,
{ 243: } 276,
{ 244: } 277,
{ 245: } 279,
{ 246: } 280,
{ 247: } 281,
{ 248: } 282,
{ 249: } 283,
{ 250: } 284,
{ 251: } 286,
{ 252: } 286,
{ 253: } 287,
{ 254: } 289,
{ 255: } 291,
{ 256: } 292,
{ 257: } 294,
{ 258: } 296,
{ 259: } 296,
{ 260: } 298,
{ 261: } 298,
{ 262: } 298,
{ 263: } 298,
{ 264: } 298,
{ 265: } 298,
{ 266: } 298,
{ 267: } 298,
{ 268: } 298,
{ 269: } 298,
{ 270: } 298,
{ 271: } 298,
{ 272: } 298,
{ 273: } 298,
{ 274: } 298,
{ 275: } 298,
{ 276: } 298,
{ 277: } 298,
{ 278: } 298,
{ 279: } 298,
{ 280: } 298,
{ 281: } 298,
{ 282: } 298,
{ 283: } 298,
{ 284: } 298,
{ 285: } 298,
{ 286: } 299,
{ 287: } 299,
{ 288: } 299,
{ 289: } 299,
{ 290: } 299,
{ 291: } 299,
{ 292: } 299,
{ 293: } 299,
{ 294: } 299,
{ 295: } 299,
{ 296: } 299,
{ 297: } 300
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
{ 53: } 105,
{ 54: } 106,
{ 55: } 107,
{ 56: } 108,
{ 57: } 108,
{ 58: } 109,
{ 59: } 109,
{ 60: } 110,
{ 61: } 110,
{ 62: } 110,
{ 63: } 111,
{ 64: } 113,
{ 65: } 114,
{ 66: } 115,
{ 67: } 115,
{ 68: } 115,
{ 69: } 117,
{ 70: } 118,
{ 71: } 119,
{ 72: } 120,
{ 73: } 121,
{ 74: } 122,
{ 75: } 123,
{ 76: } 124,
{ 77: } 125,
{ 78: } 126,
{ 79: } 126,
{ 80: } 126,
{ 81: } 126,
{ 82: } 126,
{ 83: } 126,
{ 84: } 126,
{ 85: } 126,
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
{ 116: } 156,
{ 117: } 157,
{ 118: } 158,
{ 119: } 159,
{ 120: } 160,
{ 121: } 160,
{ 122: } 160,
{ 123: } 160,
{ 124: } 160,
{ 125: } 160,
{ 126: } 160,
{ 127: } 160,
{ 128: } 160,
{ 129: } 161,
{ 130: } 162,
{ 131: } 163,
{ 132: } 164,
{ 133: } 165,
{ 134: } 166,
{ 135: } 167,
{ 136: } 168,
{ 137: } 169,
{ 138: } 170,
{ 139: } 171,
{ 140: } 172,
{ 141: } 173,
{ 142: } 174,
{ 143: } 175,
{ 144: } 176,
{ 145: } 177,
{ 146: } 178,
{ 147: } 179,
{ 148: } 180,
{ 149: } 181,
{ 150: } 183,
{ 151: } 184,
{ 152: } 185,
{ 153: } 187,
{ 154: } 189,
{ 155: } 190,
{ 156: } 191,
{ 157: } 192,
{ 158: } 193,
{ 159: } 193,
{ 160: } 193,
{ 161: } 194,
{ 162: } 194,
{ 163: } 194,
{ 164: } 194,
{ 165: } 194,
{ 166: } 194,
{ 167: } 194,
{ 168: } 194,
{ 169: } 195,
{ 170: } 197,
{ 171: } 198,
{ 172: } 199,
{ 173: } 200,
{ 174: } 201,
{ 175: } 202,
{ 176: } 203,
{ 177: } 204,
{ 178: } 205,
{ 179: } 206,
{ 180: } 207,
{ 181: } 209,
{ 182: } 211,
{ 183: } 213,
{ 184: } 214,
{ 185: } 215,
{ 186: } 216,
{ 187: } 217,
{ 188: } 218,
{ 189: } 219,
{ 190: } 221,
{ 191: } 222,
{ 192: } 224,
{ 193: } 226,
{ 194: } 228,
{ 195: } 230,
{ 196: } 230,
{ 197: } 230,
{ 198: } 231,
{ 199: } 232,
{ 200: } 232,
{ 201: } 232,
{ 202: } 232,
{ 203: } 232,
{ 204: } 232,
{ 205: } 233,
{ 206: } 234,
{ 207: } 235,
{ 208: } 237,
{ 209: } 238,
{ 210: } 240,
{ 211: } 241,
{ 212: } 242,
{ 213: } 243,
{ 214: } 244,
{ 215: } 245,
{ 216: } 247,
{ 217: } 249,
{ 218: } 250,
{ 219: } 251,
{ 220: } 253,
{ 221: } 254,
{ 222: } 256,
{ 223: } 256,
{ 224: } 256,
{ 225: } 257,
{ 226: } 258,
{ 227: } 259,
{ 228: } 259,
{ 229: } 259,
{ 230: } 261,
{ 231: } 262,
{ 232: } 263,
{ 233: } 264,
{ 234: } 266,
{ 235: } 268,
{ 236: } 270,
{ 237: } 271,
{ 238: } 272,
{ 239: } 273,
{ 240: } 275,
{ 241: } 276,
{ 242: } 276,
{ 243: } 276,
{ 244: } 277,
{ 245: } 278,
{ 246: } 280,
{ 247: } 281,
{ 248: } 282,
{ 249: } 283,
{ 250: } 284,
{ 251: } 285,
{ 252: } 287,
{ 253: } 287,
{ 254: } 288,
{ 255: } 290,
{ 256: } 292,
{ 257: } 293,
{ 258: } 295,
{ 259: } 297,
{ 260: } 297,
{ 261: } 299,
{ 262: } 299,
{ 263: } 299,
{ 264: } 299,
{ 265: } 299,
{ 266: } 299,
{ 267: } 299,
{ 268: } 299,
{ 269: } 299,
{ 270: } 299,
{ 271: } 299,
{ 272: } 299,
{ 273: } 299,
{ 274: } 299,
{ 275: } 299,
{ 276: } 299,
{ 277: } 299,
{ 278: } 299,
{ 279: } 299,
{ 280: } 299,
{ 281: } 299,
{ 282: } 299,
{ 283: } 299,
{ 284: } 299,
{ 285: } 299,
{ 286: } 299,
{ 287: } 300,
{ 288: } 300,
{ 289: } 300,
{ 290: } 300,
{ 291: } 300,
{ 292: } 300,
{ 293: } 300,
{ 294: } 300,
{ 295: } 300,
{ 296: } 300,
{ 297: } 300
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
{ 52: } 104,
{ 53: } 105,
{ 54: } 106,
{ 55: } 107,
{ 56: } 107,
{ 57: } 108,
{ 58: } 108,
{ 59: } 109,
{ 60: } 109,
{ 61: } 109,
{ 62: } 110,
{ 63: } 112,
{ 64: } 113,
{ 65: } 114,
{ 66: } 114,
{ 67: } 114,
{ 68: } 116,
{ 69: } 117,
{ 70: } 118,
{ 71: } 119,
{ 72: } 120,
{ 73: } 121,
{ 74: } 122,
{ 75: } 123,
{ 76: } 124,
{ 77: } 125,
{ 78: } 125,
{ 79: } 125,
{ 80: } 125,
{ 81: } 125,
{ 82: } 125,
{ 83: } 125,
{ 84: } 125,
{ 85: } 126,
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
{ 115: } 155,
{ 116: } 156,
{ 117: } 157,
{ 118: } 158,
{ 119: } 159,
{ 120: } 159,
{ 121: } 159,
{ 122: } 159,
{ 123: } 159,
{ 124: } 159,
{ 125: } 159,
{ 126: } 159,
{ 127: } 159,
{ 128: } 160,
{ 129: } 161,
{ 130: } 162,
{ 131: } 163,
{ 132: } 164,
{ 133: } 165,
{ 134: } 166,
{ 135: } 167,
{ 136: } 168,
{ 137: } 169,
{ 138: } 170,
{ 139: } 171,
{ 140: } 172,
{ 141: } 173,
{ 142: } 174,
{ 143: } 175,
{ 144: } 176,
{ 145: } 177,
{ 146: } 178,
{ 147: } 179,
{ 148: } 180,
{ 149: } 182,
{ 150: } 183,
{ 151: } 184,
{ 152: } 186,
{ 153: } 188,
{ 154: } 189,
{ 155: } 190,
{ 156: } 191,
{ 157: } 192,
{ 158: } 192,
{ 159: } 192,
{ 160: } 193,
{ 161: } 193,
{ 162: } 193,
{ 163: } 193,
{ 164: } 193,
{ 165: } 193,
{ 166: } 193,
{ 167: } 193,
{ 168: } 194,
{ 169: } 196,
{ 170: } 197,
{ 171: } 198,
{ 172: } 199,
{ 173: } 200,
{ 174: } 201,
{ 175: } 202,
{ 176: } 203,
{ 177: } 204,
{ 178: } 205,
{ 179: } 206,
{ 180: } 208,
{ 181: } 210,
{ 182: } 212,
{ 183: } 213,
{ 184: } 214,
{ 185: } 215,
{ 186: } 216,
{ 187: } 217,
{ 188: } 218,
{ 189: } 220,
{ 190: } 221,
{ 191: } 223,
{ 192: } 225,
{ 193: } 227,
{ 194: } 229,
{ 195: } 229,
{ 196: } 229,
{ 197: } 230,
{ 198: } 231,
{ 199: } 231,
{ 200: } 231,
{ 201: } 231,
{ 202: } 231,
{ 203: } 231,
{ 204: } 232,
{ 205: } 233,
{ 206: } 234,
{ 207: } 236,
{ 208: } 237,
{ 209: } 239,
{ 210: } 240,
{ 211: } 241,
{ 212: } 242,
{ 213: } 243,
{ 214: } 244,
{ 215: } 246,
{ 216: } 248,
{ 217: } 249,
{ 218: } 250,
{ 219: } 252,
{ 220: } 253,
{ 221: } 255,
{ 222: } 255,
{ 223: } 255,
{ 224: } 256,
{ 225: } 257,
{ 226: } 258,
{ 227: } 258,
{ 228: } 258,
{ 229: } 260,
{ 230: } 261,
{ 231: } 262,
{ 232: } 263,
{ 233: } 265,
{ 234: } 267,
{ 235: } 269,
{ 236: } 270,
{ 237: } 271,
{ 238: } 272,
{ 239: } 274,
{ 240: } 275,
{ 241: } 275,
{ 242: } 275,
{ 243: } 276,
{ 244: } 277,
{ 245: } 279,
{ 246: } 280,
{ 247: } 281,
{ 248: } 282,
{ 249: } 283,
{ 250: } 284,
{ 251: } 286,
{ 252: } 286,
{ 253: } 287,
{ 254: } 289,
{ 255: } 291,
{ 256: } 292,
{ 257: } 294,
{ 258: } 296,
{ 259: } 296,
{ 260: } 298,
{ 261: } 298,
{ 262: } 298,
{ 263: } 298,
{ 264: } 298,
{ 265: } 298,
{ 266: } 298,
{ 267: } 298,
{ 268: } 298,
{ 269: } 298,
{ 270: } 298,
{ 271: } 298,
{ 272: } 298,
{ 273: } 298,
{ 274: } 298,
{ 275: } 298,
{ 276: } 298,
{ 277: } 298,
{ 278: } 298,
{ 279: } 298,
{ 280: } 298,
{ 281: } 298,
{ 282: } 298,
{ 283: } 298,
{ 284: } 298,
{ 285: } 298,
{ 286: } 299,
{ 287: } 299,
{ 288: } 299,
{ 289: } 299,
{ 290: } 299,
{ 291: } 299,
{ 292: } 299,
{ 293: } 299,
{ 294: } 299,
{ 295: } 299,
{ 296: } 299,
{ 297: } 300
);

yytl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 53,
{ 2: } 105,
{ 3: } 107,
{ 4: } 109,
{ 5: } 111,
{ 6: } 114,
{ 7: } 119,
{ 8: } 121,
{ 9: } 127,
{ 10: } 128,
{ 11: } 129,
{ 12: } 130,
{ 13: } 132,
{ 14: } 134,
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
{ 26: } 141,
{ 27: } 142,
{ 28: } 145,
{ 29: } 148,
{ 30: } 152,
{ 31: } 154,
{ 32: } 156,
{ 33: } 158,
{ 34: } 160,
{ 35: } 162,
{ 36: } 165,
{ 37: } 167,
{ 38: } 170,
{ 39: } 170,
{ 40: } 170,
{ 41: } 172,
{ 42: } 174,
{ 43: } 176,
{ 44: } 179,
{ 45: } 181,
{ 46: } 183,
{ 47: } 185,
{ 48: } 187,
{ 49: } 189,
{ 50: } 190,
{ 51: } 190,
{ 52: } 190,
{ 53: } 190,
{ 54: } 190,
{ 55: } 190,
{ 56: } 190,
{ 57: } 192,
{ 58: } 192,
{ 59: } 194,
{ 60: } 194,
{ 61: } 196,
{ 62: } 198,
{ 63: } 199,
{ 64: } 204,
{ 65: } 205,
{ 66: } 205,
{ 67: } 206,
{ 68: } 208,
{ 69: } 209,
{ 70: } 212,
{ 71: } 212,
{ 72: } 212,
{ 73: } 212,
{ 74: } 212,
{ 75: } 212,
{ 76: } 212,
{ 77: } 212,
{ 78: } 212,
{ 79: } 214,
{ 80: } 220,
{ 81: } 223,
{ 82: } 224,
{ 83: } 225,
{ 84: } 226,
{ 85: } 227,
{ 86: } 229,
{ 87: } 231,
{ 88: } 233,
{ 89: } 235,
{ 90: } 237,
{ 91: } 239,
{ 92: } 241,
{ 93: } 244,
{ 94: } 246,
{ 95: } 248,
{ 96: } 250,
{ 97: } 252,
{ 98: } 254,
{ 99: } 256,
{ 100: } 259,
{ 101: } 261,
{ 102: } 263,
{ 103: } 265,
{ 104: } 267,
{ 105: } 269,
{ 106: } 271,
{ 107: } 273,
{ 108: } 275,
{ 109: } 277,
{ 110: } 279,
{ 111: } 281,
{ 112: } 283,
{ 113: } 283,
{ 114: } 283,
{ 115: } 285,
{ 116: } 286,
{ 117: } 287,
{ 118: } 288,
{ 119: } 288,
{ 120: } 289,
{ 121: } 290,
{ 122: } 292,
{ 123: } 294,
{ 124: } 295,
{ 125: } 296,
{ 126: } 297,
{ 127: } 298,
{ 128: } 299,
{ 129: } 299,
{ 130: } 301,
{ 131: } 303,
{ 132: } 305,
{ 133: } 307,
{ 134: } 309,
{ 135: } 311,
{ 136: } 313,
{ 137: } 315,
{ 138: } 317,
{ 139: } 320,
{ 140: } 322,
{ 141: } 324,
{ 142: } 326,
{ 143: } 328,
{ 144: } 330,
{ 145: } 332,
{ 146: } 334,
{ 147: } 336,
{ 148: } 338,
{ 149: } 340,
{ 150: } 341,
{ 151: } 343,
{ 152: } 345,
{ 153: } 346,
{ 154: } 347,
{ 155: } 349,
{ 156: } 351,
{ 157: } 353,
{ 158: } 355,
{ 159: } 356,
{ 160: } 357,
{ 161: } 357,
{ 162: } 358,
{ 163: } 359,
{ 164: } 360,
{ 165: } 361,
{ 166: } 362,
{ 167: } 363,
{ 168: } 364,
{ 169: } 366,
{ 170: } 367,
{ 171: } 369,
{ 172: } 371,
{ 173: } 373,
{ 174: } 375,
{ 175: } 377,
{ 176: } 379,
{ 177: } 381,
{ 178: } 383,
{ 179: } 385,
{ 180: } 387,
{ 181: } 388,
{ 182: } 389,
{ 183: } 390,
{ 184: } 392,
{ 185: } 394,
{ 186: } 396,
{ 187: } 398,
{ 188: } 400,
{ 189: } 402,
{ 190: } 403,
{ 191: } 405,
{ 192: } 406,
{ 193: } 407,
{ 194: } 408,
{ 195: } 409,
{ 196: } 410,
{ 197: } 411,
{ 198: } 411,
{ 199: } 411,
{ 200: } 412,
{ 201: } 413,
{ 202: } 414,
{ 203: } 415,
{ 204: } 416,
{ 205: } 418,
{ 206: } 420,
{ 207: } 422,
{ 208: } 423,
{ 209: } 425,
{ 210: } 426,
{ 211: } 428,
{ 212: } 430,
{ 213: } 432,
{ 214: } 434,
{ 215: } 436,
{ 216: } 437,
{ 217: } 438,
{ 218: } 440,
{ 219: } 442,
{ 220: } 443,
{ 221: } 445,
{ 222: } 446,
{ 223: } 447,
{ 224: } 448,
{ 225: } 448,
{ 226: } 448,
{ 227: } 448,
{ 228: } 449,
{ 229: } 450,
{ 230: } 451,
{ 231: } 453,
{ 232: } 455,
{ 233: } 457,
{ 234: } 458,
{ 235: } 459,
{ 236: } 460,
{ 237: } 462,
{ 238: } 464,
{ 239: } 466,
{ 240: } 467,
{ 241: } 469,
{ 242: } 470,
{ 243: } 471,
{ 244: } 471,
{ 245: } 471,
{ 246: } 472,
{ 247: } 474,
{ 248: } 476,
{ 249: } 478,
{ 250: } 480,
{ 251: } 482,
{ 252: } 483,
{ 253: } 484,
{ 254: } 484,
{ 255: } 485,
{ 256: } 486,
{ 257: } 488,
{ 258: } 489,
{ 259: } 490,
{ 260: } 491,
{ 261: } 492,
{ 262: } 493,
{ 263: } 494,
{ 264: } 495,
{ 265: } 496,
{ 266: } 497,
{ 267: } 498,
{ 268: } 499,
{ 269: } 500,
{ 270: } 502,
{ 271: } 504,
{ 272: } 505,
{ 273: } 506,
{ 274: } 507,
{ 275: } 508,
{ 276: } 509,
{ 277: } 510,
{ 278: } 511,
{ 279: } 512,
{ 280: } 513,
{ 281: } 514,
{ 282: } 515,
{ 283: } 516,
{ 284: } 517,
{ 285: } 518,
{ 286: } 519,
{ 287: } 519,
{ 288: } 520,
{ 289: } 521,
{ 290: } 522,
{ 291: } 523,
{ 292: } 524,
{ 293: } 525,
{ 294: } 526,
{ 295: } 527,
{ 296: } 528,
{ 297: } 529
);

yyth : array [0..yynstates-1] of Integer = (
{ 0: } 52,
{ 1: } 104,
{ 2: } 106,
{ 3: } 108,
{ 4: } 110,
{ 5: } 113,
{ 6: } 118,
{ 7: } 120,
{ 8: } 126,
{ 9: } 127,
{ 10: } 128,
{ 11: } 129,
{ 12: } 131,
{ 13: } 133,
{ 14: } 140,
{ 15: } 140,
{ 16: } 140,
{ 17: } 140,
{ 18: } 140,
{ 19: } 140,
{ 20: } 140,
{ 21: } 140,
{ 22: } 140,
{ 23: } 140,
{ 24: } 140,
{ 25: } 140,
{ 26: } 141,
{ 27: } 144,
{ 28: } 147,
{ 29: } 151,
{ 30: } 153,
{ 31: } 155,
{ 32: } 157,
{ 33: } 159,
{ 34: } 161,
{ 35: } 164,
{ 36: } 166,
{ 37: } 169,
{ 38: } 169,
{ 39: } 169,
{ 40: } 171,
{ 41: } 173,
{ 42: } 175,
{ 43: } 178,
{ 44: } 180,
{ 45: } 182,
{ 46: } 184,
{ 47: } 186,
{ 48: } 188,
{ 49: } 189,
{ 50: } 189,
{ 51: } 189,
{ 52: } 189,
{ 53: } 189,
{ 54: } 189,
{ 55: } 189,
{ 56: } 191,
{ 57: } 191,
{ 58: } 193,
{ 59: } 193,
{ 60: } 195,
{ 61: } 197,
{ 62: } 198,
{ 63: } 203,
{ 64: } 204,
{ 65: } 204,
{ 66: } 205,
{ 67: } 207,
{ 68: } 208,
{ 69: } 211,
{ 70: } 211,
{ 71: } 211,
{ 72: } 211,
{ 73: } 211,
{ 74: } 211,
{ 75: } 211,
{ 76: } 211,
{ 77: } 211,
{ 78: } 213,
{ 79: } 219,
{ 80: } 222,
{ 81: } 223,
{ 82: } 224,
{ 83: } 225,
{ 84: } 226,
{ 85: } 228,
{ 86: } 230,
{ 87: } 232,
{ 88: } 234,
{ 89: } 236,
{ 90: } 238,
{ 91: } 240,
{ 92: } 243,
{ 93: } 245,
{ 94: } 247,
{ 95: } 249,
{ 96: } 251,
{ 97: } 253,
{ 98: } 255,
{ 99: } 258,
{ 100: } 260,
{ 101: } 262,
{ 102: } 264,
{ 103: } 266,
{ 104: } 268,
{ 105: } 270,
{ 106: } 272,
{ 107: } 274,
{ 108: } 276,
{ 109: } 278,
{ 110: } 280,
{ 111: } 282,
{ 112: } 282,
{ 113: } 282,
{ 114: } 284,
{ 115: } 285,
{ 116: } 286,
{ 117: } 287,
{ 118: } 287,
{ 119: } 288,
{ 120: } 289,
{ 121: } 291,
{ 122: } 293,
{ 123: } 294,
{ 124: } 295,
{ 125: } 296,
{ 126: } 297,
{ 127: } 298,
{ 128: } 298,
{ 129: } 300,
{ 130: } 302,
{ 131: } 304,
{ 132: } 306,
{ 133: } 308,
{ 134: } 310,
{ 135: } 312,
{ 136: } 314,
{ 137: } 316,
{ 138: } 319,
{ 139: } 321,
{ 140: } 323,
{ 141: } 325,
{ 142: } 327,
{ 143: } 329,
{ 144: } 331,
{ 145: } 333,
{ 146: } 335,
{ 147: } 337,
{ 148: } 339,
{ 149: } 340,
{ 150: } 342,
{ 151: } 344,
{ 152: } 345,
{ 153: } 346,
{ 154: } 348,
{ 155: } 350,
{ 156: } 352,
{ 157: } 354,
{ 158: } 355,
{ 159: } 356,
{ 160: } 356,
{ 161: } 357,
{ 162: } 358,
{ 163: } 359,
{ 164: } 360,
{ 165: } 361,
{ 166: } 362,
{ 167: } 363,
{ 168: } 365,
{ 169: } 366,
{ 170: } 368,
{ 171: } 370,
{ 172: } 372,
{ 173: } 374,
{ 174: } 376,
{ 175: } 378,
{ 176: } 380,
{ 177: } 382,
{ 178: } 384,
{ 179: } 386,
{ 180: } 387,
{ 181: } 388,
{ 182: } 389,
{ 183: } 391,
{ 184: } 393,
{ 185: } 395,
{ 186: } 397,
{ 187: } 399,
{ 188: } 401,
{ 189: } 402,
{ 190: } 404,
{ 191: } 405,
{ 192: } 406,
{ 193: } 407,
{ 194: } 408,
{ 195: } 409,
{ 196: } 410,
{ 197: } 410,
{ 198: } 410,
{ 199: } 411,
{ 200: } 412,
{ 201: } 413,
{ 202: } 414,
{ 203: } 415,
{ 204: } 417,
{ 205: } 419,
{ 206: } 421,
{ 207: } 422,
{ 208: } 424,
{ 209: } 425,
{ 210: } 427,
{ 211: } 429,
{ 212: } 431,
{ 213: } 433,
{ 214: } 435,
{ 215: } 436,
{ 216: } 437,
{ 217: } 439,
{ 218: } 441,
{ 219: } 442,
{ 220: } 444,
{ 221: } 445,
{ 222: } 446,
{ 223: } 447,
{ 224: } 447,
{ 225: } 447,
{ 226: } 447,
{ 227: } 448,
{ 228: } 449,
{ 229: } 450,
{ 230: } 452,
{ 231: } 454,
{ 232: } 456,
{ 233: } 457,
{ 234: } 458,
{ 235: } 459,
{ 236: } 461,
{ 237: } 463,
{ 238: } 465,
{ 239: } 466,
{ 240: } 468,
{ 241: } 469,
{ 242: } 470,
{ 243: } 470,
{ 244: } 470,
{ 245: } 471,
{ 246: } 473,
{ 247: } 475,
{ 248: } 477,
{ 249: } 479,
{ 250: } 481,
{ 251: } 482,
{ 252: } 483,
{ 253: } 483,
{ 254: } 484,
{ 255: } 485,
{ 256: } 487,
{ 257: } 488,
{ 258: } 489,
{ 259: } 490,
{ 260: } 491,
{ 261: } 492,
{ 262: } 493,
{ 263: } 494,
{ 264: } 495,
{ 265: } 496,
{ 266: } 497,
{ 267: } 498,
{ 268: } 499,
{ 269: } 501,
{ 270: } 503,
{ 271: } 504,
{ 272: } 505,
{ 273: } 506,
{ 274: } 507,
{ 275: } 508,
{ 276: } 509,
{ 277: } 510,
{ 278: } 511,
{ 279: } 512,
{ 280: } 513,
{ 281: } 514,
{ 282: } 515,
{ 283: } 516,
{ 284: } 517,
{ 285: } 518,
{ 286: } 518,
{ 287: } 519,
{ 288: } 520,
{ 289: } 521,
{ 290: } 522,
{ 291: } 523,
{ 292: } 524,
{ 293: } 525,
{ 294: } 526,
{ 295: } 527,
{ 296: } 528,
{ 297: } 528
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

