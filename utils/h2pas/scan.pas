
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

yynmarks   = 299;
yynmatches = 299;
yyntrans   = 519;
yynstates  = 295;

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
  80,
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
  5,
  { 112: }
  6,
  { 113: }
  9,
  { 114: }
  { 115: }
  9,
  { 116: }
  8,
  { 117: }
  8,
  { 118: }
  57,
  { 119: }
  { 120: }
  { 121: }
  { 122: }
  { 123: }
  { 124: }
  { 125: }
  { 126: }
  34,
  { 127: }
  80,
  { 128: }
  80,
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
  67,
  80,
  { 148: }
  80,
  { 149: }
  80,
  { 150: }
  75,
  80,
  { 151: }
  74,
  80,
  { 152: }
  80,
  { 153: }
  80,
  { 154: }
  80,
  { 155: }
  80,
  { 156: }
  { 157: }
  { 158: }
  { 159: }
  { 160: }
  { 161: }
  { 162: }
  { 163: }
  { 164: }
  { 165: }
  80,
  { 166: }
  62,
  80,
  { 167: }
  80,
  { 168: }
  80,
  { 169: }
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
  47,
  80,
  { 178: }
  48,
  80,
  { 179: }
  60,
  80,
  { 180: }
  80,
  { 181: }
  80,
  { 182: }
  80,
  { 183: }
  80,
  { 184: }
  80,
  { 185: }
  80,
  { 186: }
  69,
  80,
  { 187: }
  80,
  { 188: }
  76,
  80,
  { 189: }
  77,
  80,
  { 190: }
  78,
  80,
  { 191: }
  79,
  80,
  { 192: }
  { 193: }
  { 194: }
  51,
  { 195: }
  53,
  { 196: }
  { 197: }
  { 198: }
  { 199: }
  { 200: }
  { 201: }
  80,
  { 202: }
  80,
  { 203: }
  80,
  { 204: }
  39,
  80,
  { 205: }
  80,
  { 206: }
  73,
  80,
  { 207: }
  80,
  { 208: }
  80,
  { 209: }
  80,
  { 210: }
  80,
  { 211: }
  80,
  { 212: }
  72,
  80,
  { 213: }
  61,
  80,
  { 214: }
  80,
  { 215: }
  80,
  { 216: }
  68,
  80,
  { 217: }
  80,
  { 218: }
  71,
  80,
  { 219: }
  { 220: }
  { 221: }
  52,
  { 222: }
  55,
  { 223: }
  54,
  { 224: }
  { 225: }
  { 226: }
  37,
  80,
  { 227: }
  80,
  { 228: }
  80,
  { 229: }
  80,
  { 230: }
  40,
  80,
  { 231: }
  41,
  80,
  { 232: }
  42,
  80,
  { 233: }
  80,
  { 234: }
  80,
  { 235: }
  80,
  { 236: }
  63,
  80,
  { 237: }
  80,
  { 238: }
  { 239: }
  { 240: }
  58,
  { 241: }
  59,
  { 242: }
  38,
  80,
  { 243: }
  80,
  { 244: }
  80,
  { 245: }
  80,
  { 246: }
  80,
  { 247: }
  80,
  { 248: }
  66,
  80,
  { 249: }
  { 250: }
  56,
  { 251: }
  43,
  80,
  { 252: }
  45,
  80,
  { 253: }
  80,
  { 254: }
  46,
  80,
  { 255: }
  70,
  80,
  { 256: }
  { 257: }
  44,
  80,
  { 258: }
  { 259: }
  { 260: }
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
  50,
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
  80,
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
  5,
{ 112: }
  6,
{ 113: }
  9,
{ 114: }
{ 115: }
  9,
{ 116: }
  8,
{ 117: }
  8,
{ 118: }
  57,
{ 119: }
{ 120: }
{ 121: }
{ 122: }
{ 123: }
{ 124: }
{ 125: }
{ 126: }
  34,
{ 127: }
  80,
{ 128: }
  80,
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
  67,
  80,
{ 148: }
  80,
{ 149: }
  80,
{ 150: }
  75,
  80,
{ 151: }
  74,
  80,
{ 152: }
  80,
{ 153: }
  80,
{ 154: }
  80,
{ 155: }
  80,
{ 156: }
{ 157: }
{ 158: }
{ 159: }
{ 160: }
{ 161: }
{ 162: }
{ 163: }
{ 164: }
{ 165: }
  80,
{ 166: }
  62,
  80,
{ 167: }
  80,
{ 168: }
  80,
{ 169: }
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
  47,
  80,
{ 178: }
  48,
  80,
{ 179: }
  60,
  80,
{ 180: }
  80,
{ 181: }
  80,
{ 182: }
  80,
{ 183: }
  80,
{ 184: }
  80,
{ 185: }
  80,
{ 186: }
  69,
  80,
{ 187: }
  80,
{ 188: }
  76,
  80,
{ 189: }
  77,
  80,
{ 190: }
  78,
  80,
{ 191: }
  79,
  80,
{ 192: }
{ 193: }
{ 194: }
  51,
{ 195: }
  53,
{ 196: }
{ 197: }
{ 198: }
{ 199: }
{ 200: }
{ 201: }
  80,
{ 202: }
  80,
{ 203: }
  80,
{ 204: }
  39,
  80,
{ 205: }
  80,
{ 206: }
  73,
  80,
{ 207: }
  80,
{ 208: }
  80,
{ 209: }
  80,
{ 210: }
  80,
{ 211: }
  80,
{ 212: }
  72,
  80,
{ 213: }
  61,
  80,
{ 214: }
  80,
{ 215: }
  80,
{ 216: }
  68,
  80,
{ 217: }
  80,
{ 218: }
  71,
  80,
{ 219: }
{ 220: }
{ 221: }
  52,
{ 222: }
  55,
{ 223: }
  54,
{ 224: }
{ 225: }
{ 226: }
  37,
  80,
{ 227: }
  80,
{ 228: }
  80,
{ 229: }
  80,
{ 230: }
  40,
  80,
{ 231: }
  41,
  80,
{ 232: }
  42,
  80,
{ 233: }
  80,
{ 234: }
  80,
{ 235: }
  80,
{ 236: }
  63,
  80,
{ 237: }
  80,
{ 238: }
{ 239: }
{ 240: }
  58,
{ 241: }
  59,
{ 242: }
  38,
  80,
{ 243: }
  80,
{ 244: }
  80,
{ 245: }
  80,
{ 246: }
  80,
{ 247: }
  80,
{ 248: }
  66,
  80,
{ 249: }
{ 250: }
  56,
{ 251: }
  43,
  80,
{ 252: }
  45,
  80,
{ 253: }
  80,
{ 254: }
  46,
  80,
{ 255: }
  70,
  80,
{ 256: }
{ 257: }
  44,
  80,
{ 258: }
{ 259: }
{ 260: }
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
  50,
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
  ( cc: [ '#' ]; s: 77),
  ( cc: [ 'd' ]; s: 82),
  ( cc: [ 'e' ]; s: 79),
  ( cc: [ 'i' ]; s: 78),
  ( cc: [ 'p' ]; s: 81),
  ( cc: [ 'u' ]; s: 80),
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
  ( cc: [ '.' ]; s: 83),
{ 27: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'w','y','z' ]; s: 62),
  ( cc: [ 'n' ]; s: 85),
  ( cc: [ 'x' ]; s: 84),
{ 28: }
  ( cc: [ '0'..'9','A'..'S','U'..'X','Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'T' ]; s: 86),
  ( cc: [ 'Y' ]; s: 87),
{ 29: }
  ( cc: [ '0'..'9','B','C','E'..'N','P'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 89),
  ( cc: [ 'D' ]; s: 88),
  ( cc: [ 'O' ]; s: 90),
{ 30: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 91),
{ 31: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'I' ]; s: 92),
{ 32: }
  ( cc: [ '0'..'9','A'..'W','Y','Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'X' ]; s: 93),
{ 33: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 62),
  ( cc: [ 'o' ]; s: 94),
{ 34: }
  ( cc: [ '0'..'9','A'..'N','P'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'O' ]; s: 95),
{ 35: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','i'..'n','p'..'z' ]; s: 62),
  ( cc: [ 'h' ]; s: 96),
  ( cc: [ 'o' ]; s: 97),
{ 36: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 62),
  ( cc: [ 'n' ]; s: 98),
{ 37: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','i'..'s','u'..'z' ]; s: 62),
  ( cc: [ 'h' ]; s: 100),
  ( cc: [ 't' ]; s: 99),
{ 38: }
{ 39: }
{ 40: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'x','z' ]; s: 62),
  ( cc: [ 'y' ]; s: 101),
{ 41: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 62),
  ( cc: [ 'n' ]; s: 102),
{ 42: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 62),
  ( cc: [ 'o' ]; s: 103),
{ 43: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'k','m'..'z' ]; s: 62),
  ( cc: [ 'a' ]; s: 105),
  ( cc: [ 'l' ]; s: 104),
{ 44: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 106),
{ 45: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'E' ]; s: 107),
{ 46: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 62),
  ( cc: [ 'e' ]; s: 108),
{ 47: }
  ( cc: [ '0'..'9','A'..'T','V'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'U' ]; s: 109),
{ 48: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 62),
  ( cc: [ 'u' ]; s: 110),
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
  ( cc: [ '"' ]; s: 111),
{ 61: }
  ( cc: [ #1..'&','('..#255 ]; s: 61),
  ( cc: [ '''' ]; s: 112),
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
  ( cc: [ '0'..'9' ]; s: 113),
{ 67: }
  ( cc: [ '+','-' ]; s: 114),
  ( cc: [ '0'..'9' ]; s: 115),
{ 68: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 69: }
  ( cc: [ '0'..'9','A'..'F','a'..'f' ]; s: 69),
  ( cc: [ 'L' ]; s: 117),
  ( cc: [ 'U' ]; s: 116),
{ 70: }
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
{ 77: }
{ 78: }
  ( cc: [ 'f' ]; s: 118),
  ( cc: [ 'n' ]; s: 119),
{ 79: }
  ( cc: [ 'l' ]; s: 120),
  ( cc: [ 'n' ]; s: 121),
  ( cc: [ 'r' ]; s: 122),
{ 80: }
  ( cc: [ 'n' ]; s: 123),
{ 81: }
  ( cc: [ 'r' ]; s: 124),
{ 82: }
  ( cc: [ 'e' ]; s: 125),
{ 83: }
  ( cc: [ '.' ]; s: 126),
{ 84: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 62),
  ( cc: [ 't' ]; s: 127),
{ 85: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 62),
  ( cc: [ 'u' ]; s: 128),
{ 86: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'D' ]; s: 129),
{ 87: }
  ( cc: [ '0'..'9','A'..'R','T'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'S' ]; s: 130),
{ 88: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'E' ]; s: 131),
{ 89: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'L' ]; s: 132),
{ 90: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'N' ]; s: 133),
{ 91: }
  ( cc: [ '0'..'9','A','B','D'..'R','T'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'C' ]; s: 135),
  ( cc: [ 'S' ]; s: 134),
{ 92: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'N' ]; s: 136),
{ 93: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'P' ]; s: 137),
{ 94: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'z' ]; s: 62),
  ( cc: [ 'i' ]; s: 138),
{ 95: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'I' ]; s: 139),
{ 96: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 62),
  ( cc: [ 'a' ]; s: 140),
{ 97: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 62),
  ( cc: [ 'n' ]; s: 141),
{ 98: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'r','t'..'z' ]; s: 62),
  ( cc: [ 'i' ]; s: 142),
  ( cc: [ 's' ]; s: 143),
{ 99: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 62),
  ( cc: [ 'r' ]; s: 144),
{ 100: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 62),
  ( cc: [ 'o' ]; s: 145),
{ 101: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'o','q'..'z' ]; s: 62),
  ( cc: [ 'p' ]; s: 146),
{ 102: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 62),
  ( cc: [ 't' ]; s: 147),
{ 103: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 62),
  ( cc: [ 'n' ]; s: 148),
{ 104: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 62),
  ( cc: [ 'o' ]; s: 149),
{ 105: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 62),
  ( cc: [ 'r' ]; s: 150),
{ 106: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'R' ]; s: 151),
{ 107: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 152),
{ 108: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 62),
  ( cc: [ 'a' ]; s: 153),
{ 109: }
  ( cc: [ '0'..'9','A'..'F','H'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'G' ]; s: 154),
{ 110: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 62),
  ( cc: [ 'g' ]; s: 155),
{ 111: }
{ 112: }
{ 113: }
  ( cc: [ '0'..'9' ]; s: 113),
  ( cc: [ 'E','e' ]; s: 67),
{ 114: }
  ( cc: [ '0'..'9' ]; s: 115),
{ 115: }
  ( cc: [ '0'..'9' ]; s: 115),
{ 116: }
  ( cc: [ 'L' ]; s: 117),
{ 117: }
{ 118: }
  ( cc: [ 'd' ]; s: 156),
{ 119: }
  ( cc: [ 'c' ]; s: 157),
{ 120: }
  ( cc: [ 'i' ]; s: 159),
  ( cc: [ 's' ]; s: 158),
{ 121: }
  ( cc: [ 'd' ]; s: 160),
{ 122: }
  ( cc: [ 'r' ]; s: 161),
{ 123: }
  ( cc: [ 'd' ]; s: 162),
{ 124: }
  ( cc: [ 'a' ]; s: 163),
{ 125: }
  ( cc: [ 'f' ]; s: 164),
{ 126: }
{ 127: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 62),
  ( cc: [ 'e' ]; s: 165),
{ 128: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'l','n'..'z' ]; s: 62),
  ( cc: [ 'm' ]; s: 166),
{ 129: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'C' ]; s: 167),
{ 130: }
  ( cc: [ '0'..'9','A'..'Z','a'..'z' ]; s: 62),
  ( cc: [ '_' ]; s: 168),
{ 131: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'C' ]; s: 169),
{ 132: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'L' ]; s: 170),
{ 133: }
  ( cc: [ '0'..'9','A'..'R','T'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'S' ]; s: 171),
{ 134: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'C' ]; s: 172),
{ 135: }
  ( cc: [ '0'..'9','A'..'J','L'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'K' ]; s: 173),
{ 136: }
  ( cc: [ '0'..'9','B'..'F','H'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 174),
  ( cc: [ 'G' ]; s: 175),
{ 137: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'E' ]; s: 176),
{ 138: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 62),
  ( cc: [ 'd' ]; s: 177),
{ 139: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'D' ]; s: 178),
{ 140: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 62),
  ( cc: [ 'r' ]; s: 179),
{ 141: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'r','t'..'z' ]; s: 62),
  ( cc: [ 's' ]; s: 180),
{ 142: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 62),
  ( cc: [ 'o' ]; s: 181),
{ 143: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'z' ]; s: 62),
  ( cc: [ 'i' ]; s: 182),
{ 144: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 62),
  ( cc: [ 'u' ]; s: 183),
{ 145: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 62),
  ( cc: [ 'r' ]; s: 184),
{ 146: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 62),
  ( cc: [ 'e' ]; s: 185),
{ 147: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 148: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 62),
  ( cc: [ 'g' ]; s: 186),
{ 149: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 62),
  ( cc: [ 'a' ]; s: 187),
{ 150: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 151: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 152: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'R' ]; s: 188),
{ 153: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 62),
  ( cc: [ 'r' ]; s: 189),
{ 154: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'E' ]; s: 190),
{ 155: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 62),
  ( cc: [ 'e' ]; s: 191),
{ 156: }
  ( cc: [ 'e' ]; s: 192),
{ 157: }
  ( cc: [ 'l' ]; s: 193),
{ 158: }
  ( cc: [ 'e' ]; s: 194),
{ 159: }
  ( cc: [ 'f' ]; s: 195),
{ 160: }
  ( cc: [ 'i' ]; s: 196),
{ 161: }
  ( cc: [ 'o' ]; s: 197),
{ 162: }
  ( cc: [ 'e' ]; s: 198),
{ 163: }
  ( cc: [ 'g' ]; s: 199),
{ 164: }
  ( cc: [ 'i' ]; s: 200),
{ 165: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 62),
  ( cc: [ 'r' ]; s: 201),
{ 166: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 167: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 202),
{ 168: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'T' ]; s: 203),
{ 169: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'L' ]; s: 204),
{ 170: }
  ( cc: [ '0'..'9','A','C'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'B' ]; s: 205),
{ 171: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'T' ]; s: 206),
{ 172: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 207),
{ 173: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'E' ]; s: 208),
{ 174: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'P' ]; s: 209),
{ 175: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'D' ]; s: 210),
{ 176: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'N' ]; s: 211),
{ 177: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 178: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 179: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 180: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 62),
  ( cc: [ 't' ]; s: 212),
{ 181: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 62),
  ( cc: [ 'n' ]; s: 213),
{ 182: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 62),
  ( cc: [ 'g' ]; s: 214),
{ 183: }
  ( cc: [ '0'..'9','A'..'Z','_','a','b','d'..'z' ]; s: 62),
  ( cc: [ 'c' ]; s: 215),
{ 184: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 62),
  ( cc: [ 't' ]; s: 216),
{ 185: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 62),
  ( cc: [ 'd' ]; s: 217),
{ 186: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 187: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 62),
  ( cc: [ 't' ]; s: 218),
{ 188: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 189: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 190: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 191: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 192: }
  ( cc: [ 'f' ]; s: 219),
{ 193: }
  ( cc: [ 'u' ]; s: 220),
{ 194: }
{ 195: }
{ 196: }
  ( cc: [ 'f' ]; s: 221),
{ 197: }
  ( cc: [ 'r' ]; s: 222),
{ 198: }
  ( cc: [ 'f' ]; s: 223),
{ 199: }
  ( cc: [ 'm' ]; s: 224),
{ 200: }
  ( cc: [ 'n' ]; s: 225),
{ 201: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 62),
  ( cc: [ 'n' ]; s: 226),
{ 202: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'L' ]; s: 227),
{ 203: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'R' ]; s: 228),
{ 204: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 205: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 229),
{ 206: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 207: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'L' ]; s: 230),
{ 208: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'D' ]; s: 231),
{ 209: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'I' ]; s: 232),
{ 210: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'I' ]; s: 233),
{ 211: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'T' ]; s: 234),
{ 212: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 213: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 214: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 62),
  ( cc: [ 'n' ]; s: 235),
{ 215: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 62),
  ( cc: [ 't' ]; s: 236),
{ 216: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 217: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 62),
  ( cc: [ 'e' ]; s: 237),
{ 218: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 219: }
  ( cc: [ ' ' ]; s: 238),
{ 220: }
  ( cc: [ 'd' ]; s: 239),
{ 221: }
{ 222: }
{ 223: }
{ 224: }
  ( cc: [ 'a' ]; s: 240),
{ 225: }
  ( cc: [ 'e' ]; s: 241),
{ 226: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 227: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'L' ]; s: 242),
{ 228: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 243),
{ 229: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'C' ]; s: 244),
{ 230: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 231: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 232: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 233: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'A' ]; s: 245),
{ 234: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'R' ]; s: 246),
{ 235: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 62),
  ( cc: [ 'e' ]; s: 247),
{ 236: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 237: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'e','g'..'z' ]; s: 62),
  ( cc: [ 'f' ]; s: 248),
{ 238: }
  ( cc: [ '_' ]; s: 249),
{ 239: }
  ( cc: [ 'e' ]; s: 250),
{ 240: }
{ 241: }
{ 242: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 243: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'P' ]; s: 251),
{ 244: }
  ( cc: [ '0'..'9','A'..'J','L'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'K' ]; s: 252),
{ 245: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'P' ]; s: 253),
{ 246: }
  ( cc: [ '0'..'9','A'..'X','Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'Y' ]; s: 254),
{ 247: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 62),
  ( cc: [ 'd' ]; s: 255),
{ 248: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 249: }
  ( cc: [ '_' ]; s: 256),
{ 250: }
{ 251: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 252: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 253: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 62),
  ( cc: [ 'I' ]; s: 257),
{ 254: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 255: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 256: }
  ( cc: [ 'c' ]; s: 258),
{ 257: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 62),
{ 258: }
  ( cc: [ 'p' ]; s: 259),
{ 259: }
  ( cc: [ 'l' ]; s: 260),
{ 260: }
  ( cc: [ 'u' ]; s: 261),
{ 261: }
  ( cc: [ 's' ]; s: 262),
{ 262: }
  ( cc: [ 'p' ]; s: 263),
{ 263: }
  ( cc: [ 'l' ]; s: 264),
{ 264: }
  ( cc: [ 'u' ]; s: 265),
{ 265: }
  ( cc: [ 's' ]; s: 266),
{ 266: }
  ( cc: [ #9,' ' ]; s: 266),
  ( cc: [ #10 ]; s: 267),
{ 267: }
  ( cc: [ 'e' ]; s: 268),
  ( cc: [ '}' ]; s: 269),
{ 268: }
  ( cc: [ 'x' ]; s: 270),
{ 269: }
  ( cc: [ #10 ]; s: 271),
{ 270: }
  ( cc: [ 't' ]; s: 272),
{ 271: }
  ( cc: [ '#' ]; s: 273),
{ 272: }
  ( cc: [ 'e' ]; s: 274),
{ 273: }
  ( cc: [ 'e' ]; s: 275),
{ 274: }
  ( cc: [ 'r' ]; s: 276),
{ 275: }
  ( cc: [ 'n' ]; s: 277),
{ 276: }
  ( cc: [ 'n' ]; s: 278),
{ 277: }
  ( cc: [ 'd' ]; s: 279),
{ 278: }
  ( cc: [ ' ' ]; s: 280),
{ 279: }
  ( cc: [ 'i' ]; s: 281),
{ 280: }
  ( cc: [ '"' ]; s: 282),
{ 281: }
  ( cc: [ 'f' ]; s: 283),
{ 282: }
  ( cc: [ 'C' ]; s: 284),
{ 283: }
{ 284: }
  ( cc: [ '"' ]; s: 285),
{ 285: }
  ( cc: [ ' ' ]; s: 286),
{ 286: }
  ( cc: [ '{' ]; s: 287),
{ 287: }
  ( cc: [ #10 ]; s: 288),
{ 288: }
  ( cc: [ '#' ]; s: 289),
{ 289: }
  ( cc: [ 'e' ]; s: 290),
{ 290: }
  ( cc: [ 'n' ]; s: 291),
{ 291: }
  ( cc: [ 'd' ]; s: 292),
{ 292: }
  ( cc: [ 'i' ]; s: 293),
{ 293: }
  ( cc: [ 'f' ]; s: 294)
{ 294: }
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
{ 85: } 127,
{ 86: } 128,
{ 87: } 129,
{ 88: } 130,
{ 89: } 131,
{ 90: } 132,
{ 91: } 133,
{ 92: } 134,
{ 93: } 135,
{ 94: } 136,
{ 95: } 137,
{ 96: } 138,
{ 97: } 139,
{ 98: } 140,
{ 99: } 141,
{ 100: } 142,
{ 101: } 143,
{ 102: } 144,
{ 103: } 145,
{ 104: } 146,
{ 105: } 147,
{ 106: } 148,
{ 107: } 149,
{ 108: } 150,
{ 109: } 151,
{ 110: } 152,
{ 111: } 153,
{ 112: } 154,
{ 113: } 155,
{ 114: } 156,
{ 115: } 156,
{ 116: } 157,
{ 117: } 158,
{ 118: } 159,
{ 119: } 160,
{ 120: } 160,
{ 121: } 160,
{ 122: } 160,
{ 123: } 160,
{ 124: } 160,
{ 125: } 160,
{ 126: } 160,
{ 127: } 161,
{ 128: } 162,
{ 129: } 163,
{ 130: } 164,
{ 131: } 165,
{ 132: } 166,
{ 133: } 167,
{ 134: } 168,
{ 135: } 169,
{ 136: } 170,
{ 137: } 171,
{ 138: } 172,
{ 139: } 173,
{ 140: } 174,
{ 141: } 175,
{ 142: } 176,
{ 143: } 177,
{ 144: } 178,
{ 145: } 179,
{ 146: } 180,
{ 147: } 181,
{ 148: } 183,
{ 149: } 184,
{ 150: } 185,
{ 151: } 187,
{ 152: } 189,
{ 153: } 190,
{ 154: } 191,
{ 155: } 192,
{ 156: } 193,
{ 157: } 193,
{ 158: } 193,
{ 159: } 193,
{ 160: } 193,
{ 161: } 193,
{ 162: } 193,
{ 163: } 193,
{ 164: } 193,
{ 165: } 193,
{ 166: } 194,
{ 167: } 196,
{ 168: } 197,
{ 169: } 198,
{ 170: } 199,
{ 171: } 200,
{ 172: } 201,
{ 173: } 202,
{ 174: } 203,
{ 175: } 204,
{ 176: } 205,
{ 177: } 206,
{ 178: } 208,
{ 179: } 210,
{ 180: } 212,
{ 181: } 213,
{ 182: } 214,
{ 183: } 215,
{ 184: } 216,
{ 185: } 217,
{ 186: } 218,
{ 187: } 220,
{ 188: } 221,
{ 189: } 223,
{ 190: } 225,
{ 191: } 227,
{ 192: } 229,
{ 193: } 229,
{ 194: } 229,
{ 195: } 230,
{ 196: } 231,
{ 197: } 231,
{ 198: } 231,
{ 199: } 231,
{ 200: } 231,
{ 201: } 231,
{ 202: } 232,
{ 203: } 233,
{ 204: } 234,
{ 205: } 236,
{ 206: } 237,
{ 207: } 239,
{ 208: } 240,
{ 209: } 241,
{ 210: } 242,
{ 211: } 243,
{ 212: } 244,
{ 213: } 246,
{ 214: } 248,
{ 215: } 249,
{ 216: } 250,
{ 217: } 252,
{ 218: } 253,
{ 219: } 255,
{ 220: } 255,
{ 221: } 255,
{ 222: } 256,
{ 223: } 257,
{ 224: } 258,
{ 225: } 258,
{ 226: } 258,
{ 227: } 260,
{ 228: } 261,
{ 229: } 262,
{ 230: } 263,
{ 231: } 265,
{ 232: } 267,
{ 233: } 269,
{ 234: } 270,
{ 235: } 271,
{ 236: } 272,
{ 237: } 274,
{ 238: } 275,
{ 239: } 275,
{ 240: } 275,
{ 241: } 276,
{ 242: } 277,
{ 243: } 279,
{ 244: } 280,
{ 245: } 281,
{ 246: } 282,
{ 247: } 283,
{ 248: } 284,
{ 249: } 286,
{ 250: } 286,
{ 251: } 287,
{ 252: } 289,
{ 253: } 291,
{ 254: } 292,
{ 255: } 294,
{ 256: } 296,
{ 257: } 296,
{ 258: } 298,
{ 259: } 298,
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
{ 284: } 299,
{ 285: } 299,
{ 286: } 299,
{ 287: } 299,
{ 288: } 299,
{ 289: } 299,
{ 290: } 299,
{ 291: } 299,
{ 292: } 299,
{ 293: } 299,
{ 294: } 299
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
{ 84: } 126,
{ 85: } 127,
{ 86: } 128,
{ 87: } 129,
{ 88: } 130,
{ 89: } 131,
{ 90: } 132,
{ 91: } 133,
{ 92: } 134,
{ 93: } 135,
{ 94: } 136,
{ 95: } 137,
{ 96: } 138,
{ 97: } 139,
{ 98: } 140,
{ 99: } 141,
{ 100: } 142,
{ 101: } 143,
{ 102: } 144,
{ 103: } 145,
{ 104: } 146,
{ 105: } 147,
{ 106: } 148,
{ 107: } 149,
{ 108: } 150,
{ 109: } 151,
{ 110: } 152,
{ 111: } 153,
{ 112: } 154,
{ 113: } 155,
{ 114: } 155,
{ 115: } 156,
{ 116: } 157,
{ 117: } 158,
{ 118: } 159,
{ 119: } 159,
{ 120: } 159,
{ 121: } 159,
{ 122: } 159,
{ 123: } 159,
{ 124: } 159,
{ 125: } 159,
{ 126: } 160,
{ 127: } 161,
{ 128: } 162,
{ 129: } 163,
{ 130: } 164,
{ 131: } 165,
{ 132: } 166,
{ 133: } 167,
{ 134: } 168,
{ 135: } 169,
{ 136: } 170,
{ 137: } 171,
{ 138: } 172,
{ 139: } 173,
{ 140: } 174,
{ 141: } 175,
{ 142: } 176,
{ 143: } 177,
{ 144: } 178,
{ 145: } 179,
{ 146: } 180,
{ 147: } 182,
{ 148: } 183,
{ 149: } 184,
{ 150: } 186,
{ 151: } 188,
{ 152: } 189,
{ 153: } 190,
{ 154: } 191,
{ 155: } 192,
{ 156: } 192,
{ 157: } 192,
{ 158: } 192,
{ 159: } 192,
{ 160: } 192,
{ 161: } 192,
{ 162: } 192,
{ 163: } 192,
{ 164: } 192,
{ 165: } 193,
{ 166: } 195,
{ 167: } 196,
{ 168: } 197,
{ 169: } 198,
{ 170: } 199,
{ 171: } 200,
{ 172: } 201,
{ 173: } 202,
{ 174: } 203,
{ 175: } 204,
{ 176: } 205,
{ 177: } 207,
{ 178: } 209,
{ 179: } 211,
{ 180: } 212,
{ 181: } 213,
{ 182: } 214,
{ 183: } 215,
{ 184: } 216,
{ 185: } 217,
{ 186: } 219,
{ 187: } 220,
{ 188: } 222,
{ 189: } 224,
{ 190: } 226,
{ 191: } 228,
{ 192: } 228,
{ 193: } 228,
{ 194: } 229,
{ 195: } 230,
{ 196: } 230,
{ 197: } 230,
{ 198: } 230,
{ 199: } 230,
{ 200: } 230,
{ 201: } 231,
{ 202: } 232,
{ 203: } 233,
{ 204: } 235,
{ 205: } 236,
{ 206: } 238,
{ 207: } 239,
{ 208: } 240,
{ 209: } 241,
{ 210: } 242,
{ 211: } 243,
{ 212: } 245,
{ 213: } 247,
{ 214: } 248,
{ 215: } 249,
{ 216: } 251,
{ 217: } 252,
{ 218: } 254,
{ 219: } 254,
{ 220: } 254,
{ 221: } 255,
{ 222: } 256,
{ 223: } 257,
{ 224: } 257,
{ 225: } 257,
{ 226: } 259,
{ 227: } 260,
{ 228: } 261,
{ 229: } 262,
{ 230: } 264,
{ 231: } 266,
{ 232: } 268,
{ 233: } 269,
{ 234: } 270,
{ 235: } 271,
{ 236: } 273,
{ 237: } 274,
{ 238: } 274,
{ 239: } 274,
{ 240: } 275,
{ 241: } 276,
{ 242: } 278,
{ 243: } 279,
{ 244: } 280,
{ 245: } 281,
{ 246: } 282,
{ 247: } 283,
{ 248: } 285,
{ 249: } 285,
{ 250: } 286,
{ 251: } 288,
{ 252: } 290,
{ 253: } 291,
{ 254: } 293,
{ 255: } 295,
{ 256: } 295,
{ 257: } 297,
{ 258: } 297,
{ 259: } 297,
{ 260: } 297,
{ 261: } 297,
{ 262: } 297,
{ 263: } 297,
{ 264: } 297,
{ 265: } 297,
{ 266: } 297,
{ 267: } 297,
{ 268: } 297,
{ 269: } 297,
{ 270: } 297,
{ 271: } 297,
{ 272: } 297,
{ 273: } 297,
{ 274: } 297,
{ 275: } 297,
{ 276: } 297,
{ 277: } 297,
{ 278: } 297,
{ 279: } 297,
{ 280: } 297,
{ 281: } 297,
{ 282: } 297,
{ 283: } 298,
{ 284: } 298,
{ 285: } 298,
{ 286: } 298,
{ 287: } 298,
{ 288: } 298,
{ 289: } 298,
{ 290: } 298,
{ 291: } 298,
{ 292: } 298,
{ 293: } 298,
{ 294: } 299
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
{ 85: } 127,
{ 86: } 128,
{ 87: } 129,
{ 88: } 130,
{ 89: } 131,
{ 90: } 132,
{ 91: } 133,
{ 92: } 134,
{ 93: } 135,
{ 94: } 136,
{ 95: } 137,
{ 96: } 138,
{ 97: } 139,
{ 98: } 140,
{ 99: } 141,
{ 100: } 142,
{ 101: } 143,
{ 102: } 144,
{ 103: } 145,
{ 104: } 146,
{ 105: } 147,
{ 106: } 148,
{ 107: } 149,
{ 108: } 150,
{ 109: } 151,
{ 110: } 152,
{ 111: } 153,
{ 112: } 154,
{ 113: } 155,
{ 114: } 156,
{ 115: } 156,
{ 116: } 157,
{ 117: } 158,
{ 118: } 159,
{ 119: } 160,
{ 120: } 160,
{ 121: } 160,
{ 122: } 160,
{ 123: } 160,
{ 124: } 160,
{ 125: } 160,
{ 126: } 160,
{ 127: } 161,
{ 128: } 162,
{ 129: } 163,
{ 130: } 164,
{ 131: } 165,
{ 132: } 166,
{ 133: } 167,
{ 134: } 168,
{ 135: } 169,
{ 136: } 170,
{ 137: } 171,
{ 138: } 172,
{ 139: } 173,
{ 140: } 174,
{ 141: } 175,
{ 142: } 176,
{ 143: } 177,
{ 144: } 178,
{ 145: } 179,
{ 146: } 180,
{ 147: } 181,
{ 148: } 183,
{ 149: } 184,
{ 150: } 185,
{ 151: } 187,
{ 152: } 189,
{ 153: } 190,
{ 154: } 191,
{ 155: } 192,
{ 156: } 193,
{ 157: } 193,
{ 158: } 193,
{ 159: } 193,
{ 160: } 193,
{ 161: } 193,
{ 162: } 193,
{ 163: } 193,
{ 164: } 193,
{ 165: } 193,
{ 166: } 194,
{ 167: } 196,
{ 168: } 197,
{ 169: } 198,
{ 170: } 199,
{ 171: } 200,
{ 172: } 201,
{ 173: } 202,
{ 174: } 203,
{ 175: } 204,
{ 176: } 205,
{ 177: } 206,
{ 178: } 208,
{ 179: } 210,
{ 180: } 212,
{ 181: } 213,
{ 182: } 214,
{ 183: } 215,
{ 184: } 216,
{ 185: } 217,
{ 186: } 218,
{ 187: } 220,
{ 188: } 221,
{ 189: } 223,
{ 190: } 225,
{ 191: } 227,
{ 192: } 229,
{ 193: } 229,
{ 194: } 229,
{ 195: } 230,
{ 196: } 231,
{ 197: } 231,
{ 198: } 231,
{ 199: } 231,
{ 200: } 231,
{ 201: } 231,
{ 202: } 232,
{ 203: } 233,
{ 204: } 234,
{ 205: } 236,
{ 206: } 237,
{ 207: } 239,
{ 208: } 240,
{ 209: } 241,
{ 210: } 242,
{ 211: } 243,
{ 212: } 244,
{ 213: } 246,
{ 214: } 248,
{ 215: } 249,
{ 216: } 250,
{ 217: } 252,
{ 218: } 253,
{ 219: } 255,
{ 220: } 255,
{ 221: } 255,
{ 222: } 256,
{ 223: } 257,
{ 224: } 258,
{ 225: } 258,
{ 226: } 258,
{ 227: } 260,
{ 228: } 261,
{ 229: } 262,
{ 230: } 263,
{ 231: } 265,
{ 232: } 267,
{ 233: } 269,
{ 234: } 270,
{ 235: } 271,
{ 236: } 272,
{ 237: } 274,
{ 238: } 275,
{ 239: } 275,
{ 240: } 275,
{ 241: } 276,
{ 242: } 277,
{ 243: } 279,
{ 244: } 280,
{ 245: } 281,
{ 246: } 282,
{ 247: } 283,
{ 248: } 284,
{ 249: } 286,
{ 250: } 286,
{ 251: } 287,
{ 252: } 289,
{ 253: } 291,
{ 254: } 292,
{ 255: } 294,
{ 256: } 296,
{ 257: } 296,
{ 258: } 298,
{ 259: } 298,
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
{ 284: } 299,
{ 285: } 299,
{ 286: } 299,
{ 287: } 299,
{ 288: } 299,
{ 289: } 299,
{ 290: } 299,
{ 291: } 299,
{ 292: } 299,
{ 293: } 299,
{ 294: } 299
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
{ 84: } 126,
{ 85: } 127,
{ 86: } 128,
{ 87: } 129,
{ 88: } 130,
{ 89: } 131,
{ 90: } 132,
{ 91: } 133,
{ 92: } 134,
{ 93: } 135,
{ 94: } 136,
{ 95: } 137,
{ 96: } 138,
{ 97: } 139,
{ 98: } 140,
{ 99: } 141,
{ 100: } 142,
{ 101: } 143,
{ 102: } 144,
{ 103: } 145,
{ 104: } 146,
{ 105: } 147,
{ 106: } 148,
{ 107: } 149,
{ 108: } 150,
{ 109: } 151,
{ 110: } 152,
{ 111: } 153,
{ 112: } 154,
{ 113: } 155,
{ 114: } 155,
{ 115: } 156,
{ 116: } 157,
{ 117: } 158,
{ 118: } 159,
{ 119: } 159,
{ 120: } 159,
{ 121: } 159,
{ 122: } 159,
{ 123: } 159,
{ 124: } 159,
{ 125: } 159,
{ 126: } 160,
{ 127: } 161,
{ 128: } 162,
{ 129: } 163,
{ 130: } 164,
{ 131: } 165,
{ 132: } 166,
{ 133: } 167,
{ 134: } 168,
{ 135: } 169,
{ 136: } 170,
{ 137: } 171,
{ 138: } 172,
{ 139: } 173,
{ 140: } 174,
{ 141: } 175,
{ 142: } 176,
{ 143: } 177,
{ 144: } 178,
{ 145: } 179,
{ 146: } 180,
{ 147: } 182,
{ 148: } 183,
{ 149: } 184,
{ 150: } 186,
{ 151: } 188,
{ 152: } 189,
{ 153: } 190,
{ 154: } 191,
{ 155: } 192,
{ 156: } 192,
{ 157: } 192,
{ 158: } 192,
{ 159: } 192,
{ 160: } 192,
{ 161: } 192,
{ 162: } 192,
{ 163: } 192,
{ 164: } 192,
{ 165: } 193,
{ 166: } 195,
{ 167: } 196,
{ 168: } 197,
{ 169: } 198,
{ 170: } 199,
{ 171: } 200,
{ 172: } 201,
{ 173: } 202,
{ 174: } 203,
{ 175: } 204,
{ 176: } 205,
{ 177: } 207,
{ 178: } 209,
{ 179: } 211,
{ 180: } 212,
{ 181: } 213,
{ 182: } 214,
{ 183: } 215,
{ 184: } 216,
{ 185: } 217,
{ 186: } 219,
{ 187: } 220,
{ 188: } 222,
{ 189: } 224,
{ 190: } 226,
{ 191: } 228,
{ 192: } 228,
{ 193: } 228,
{ 194: } 229,
{ 195: } 230,
{ 196: } 230,
{ 197: } 230,
{ 198: } 230,
{ 199: } 230,
{ 200: } 230,
{ 201: } 231,
{ 202: } 232,
{ 203: } 233,
{ 204: } 235,
{ 205: } 236,
{ 206: } 238,
{ 207: } 239,
{ 208: } 240,
{ 209: } 241,
{ 210: } 242,
{ 211: } 243,
{ 212: } 245,
{ 213: } 247,
{ 214: } 248,
{ 215: } 249,
{ 216: } 251,
{ 217: } 252,
{ 218: } 254,
{ 219: } 254,
{ 220: } 254,
{ 221: } 255,
{ 222: } 256,
{ 223: } 257,
{ 224: } 257,
{ 225: } 257,
{ 226: } 259,
{ 227: } 260,
{ 228: } 261,
{ 229: } 262,
{ 230: } 264,
{ 231: } 266,
{ 232: } 268,
{ 233: } 269,
{ 234: } 270,
{ 235: } 271,
{ 236: } 273,
{ 237: } 274,
{ 238: } 274,
{ 239: } 274,
{ 240: } 275,
{ 241: } 276,
{ 242: } 278,
{ 243: } 279,
{ 244: } 280,
{ 245: } 281,
{ 246: } 282,
{ 247: } 283,
{ 248: } 285,
{ 249: } 285,
{ 250: } 286,
{ 251: } 288,
{ 252: } 290,
{ 253: } 291,
{ 254: } 293,
{ 255: } 295,
{ 256: } 295,
{ 257: } 297,
{ 258: } 297,
{ 259: } 297,
{ 260: } 297,
{ 261: } 297,
{ 262: } 297,
{ 263: } 297,
{ 264: } 297,
{ 265: } 297,
{ 266: } 297,
{ 267: } 297,
{ 268: } 297,
{ 269: } 297,
{ 270: } 297,
{ 271: } 297,
{ 272: } 297,
{ 273: } 297,
{ 274: } 297,
{ 275: } 297,
{ 276: } 297,
{ 277: } 297,
{ 278: } 297,
{ 279: } 297,
{ 280: } 297,
{ 281: } 297,
{ 282: } 297,
{ 283: } 298,
{ 284: } 298,
{ 285: } 298,
{ 286: } 298,
{ 287: } 298,
{ 288: } 298,
{ 289: } 298,
{ 290: } 298,
{ 291: } 298,
{ 292: } 298,
{ 293: } 298,
{ 294: } 299
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
{ 26: } 140,
{ 27: } 141,
{ 28: } 144,
{ 29: } 147,
{ 30: } 151,
{ 31: } 153,
{ 32: } 155,
{ 33: } 157,
{ 34: } 159,
{ 35: } 161,
{ 36: } 164,
{ 37: } 166,
{ 38: } 169,
{ 39: } 169,
{ 40: } 169,
{ 41: } 171,
{ 42: } 173,
{ 43: } 175,
{ 44: } 178,
{ 45: } 180,
{ 46: } 182,
{ 47: } 184,
{ 48: } 186,
{ 49: } 188,
{ 50: } 189,
{ 51: } 189,
{ 52: } 189,
{ 53: } 189,
{ 54: } 189,
{ 55: } 189,
{ 56: } 189,
{ 57: } 191,
{ 58: } 191,
{ 59: } 193,
{ 60: } 193,
{ 61: } 195,
{ 62: } 197,
{ 63: } 198,
{ 64: } 203,
{ 65: } 204,
{ 66: } 204,
{ 67: } 205,
{ 68: } 207,
{ 69: } 208,
{ 70: } 211,
{ 71: } 211,
{ 72: } 211,
{ 73: } 211,
{ 74: } 211,
{ 75: } 211,
{ 76: } 211,
{ 77: } 211,
{ 78: } 211,
{ 79: } 213,
{ 80: } 216,
{ 81: } 217,
{ 82: } 218,
{ 83: } 219,
{ 84: } 220,
{ 85: } 222,
{ 86: } 224,
{ 87: } 226,
{ 88: } 228,
{ 89: } 230,
{ 90: } 232,
{ 91: } 234,
{ 92: } 237,
{ 93: } 239,
{ 94: } 241,
{ 95: } 243,
{ 96: } 245,
{ 97: } 247,
{ 98: } 249,
{ 99: } 252,
{ 100: } 254,
{ 101: } 256,
{ 102: } 258,
{ 103: } 260,
{ 104: } 262,
{ 105: } 264,
{ 106: } 266,
{ 107: } 268,
{ 108: } 270,
{ 109: } 272,
{ 110: } 274,
{ 111: } 276,
{ 112: } 276,
{ 113: } 276,
{ 114: } 278,
{ 115: } 279,
{ 116: } 280,
{ 117: } 281,
{ 118: } 281,
{ 119: } 282,
{ 120: } 283,
{ 121: } 285,
{ 122: } 286,
{ 123: } 287,
{ 124: } 288,
{ 125: } 289,
{ 126: } 290,
{ 127: } 290,
{ 128: } 292,
{ 129: } 294,
{ 130: } 296,
{ 131: } 298,
{ 132: } 300,
{ 133: } 302,
{ 134: } 304,
{ 135: } 306,
{ 136: } 308,
{ 137: } 311,
{ 138: } 313,
{ 139: } 315,
{ 140: } 317,
{ 141: } 319,
{ 142: } 321,
{ 143: } 323,
{ 144: } 325,
{ 145: } 327,
{ 146: } 329,
{ 147: } 331,
{ 148: } 332,
{ 149: } 334,
{ 150: } 336,
{ 151: } 337,
{ 152: } 338,
{ 153: } 340,
{ 154: } 342,
{ 155: } 344,
{ 156: } 346,
{ 157: } 347,
{ 158: } 348,
{ 159: } 349,
{ 160: } 350,
{ 161: } 351,
{ 162: } 352,
{ 163: } 353,
{ 164: } 354,
{ 165: } 355,
{ 166: } 357,
{ 167: } 358,
{ 168: } 360,
{ 169: } 362,
{ 170: } 364,
{ 171: } 366,
{ 172: } 368,
{ 173: } 370,
{ 174: } 372,
{ 175: } 374,
{ 176: } 376,
{ 177: } 378,
{ 178: } 379,
{ 179: } 380,
{ 180: } 381,
{ 181: } 383,
{ 182: } 385,
{ 183: } 387,
{ 184: } 389,
{ 185: } 391,
{ 186: } 393,
{ 187: } 394,
{ 188: } 396,
{ 189: } 397,
{ 190: } 398,
{ 191: } 399,
{ 192: } 400,
{ 193: } 401,
{ 194: } 402,
{ 195: } 402,
{ 196: } 402,
{ 197: } 403,
{ 198: } 404,
{ 199: } 405,
{ 200: } 406,
{ 201: } 407,
{ 202: } 409,
{ 203: } 411,
{ 204: } 413,
{ 205: } 414,
{ 206: } 416,
{ 207: } 417,
{ 208: } 419,
{ 209: } 421,
{ 210: } 423,
{ 211: } 425,
{ 212: } 427,
{ 213: } 428,
{ 214: } 429,
{ 215: } 431,
{ 216: } 433,
{ 217: } 434,
{ 218: } 436,
{ 219: } 437,
{ 220: } 438,
{ 221: } 439,
{ 222: } 439,
{ 223: } 439,
{ 224: } 439,
{ 225: } 440,
{ 226: } 441,
{ 227: } 442,
{ 228: } 444,
{ 229: } 446,
{ 230: } 448,
{ 231: } 449,
{ 232: } 450,
{ 233: } 451,
{ 234: } 453,
{ 235: } 455,
{ 236: } 457,
{ 237: } 458,
{ 238: } 460,
{ 239: } 461,
{ 240: } 462,
{ 241: } 462,
{ 242: } 462,
{ 243: } 463,
{ 244: } 465,
{ 245: } 467,
{ 246: } 469,
{ 247: } 471,
{ 248: } 473,
{ 249: } 474,
{ 250: } 475,
{ 251: } 475,
{ 252: } 476,
{ 253: } 477,
{ 254: } 479,
{ 255: } 480,
{ 256: } 481,
{ 257: } 482,
{ 258: } 483,
{ 259: } 484,
{ 260: } 485,
{ 261: } 486,
{ 262: } 487,
{ 263: } 488,
{ 264: } 489,
{ 265: } 490,
{ 266: } 491,
{ 267: } 493,
{ 268: } 495,
{ 269: } 496,
{ 270: } 497,
{ 271: } 498,
{ 272: } 499,
{ 273: } 500,
{ 274: } 501,
{ 275: } 502,
{ 276: } 503,
{ 277: } 504,
{ 278: } 505,
{ 279: } 506,
{ 280: } 507,
{ 281: } 508,
{ 282: } 509,
{ 283: } 510,
{ 284: } 510,
{ 285: } 511,
{ 286: } 512,
{ 287: } 513,
{ 288: } 514,
{ 289: } 515,
{ 290: } 516,
{ 291: } 517,
{ 292: } 518,
{ 293: } 519,
{ 294: } 520
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
{ 14: } 139,
{ 15: } 139,
{ 16: } 139,
{ 17: } 139,
{ 18: } 139,
{ 19: } 139,
{ 20: } 139,
{ 21: } 139,
{ 22: } 139,
{ 23: } 139,
{ 24: } 139,
{ 25: } 139,
{ 26: } 140,
{ 27: } 143,
{ 28: } 146,
{ 29: } 150,
{ 30: } 152,
{ 31: } 154,
{ 32: } 156,
{ 33: } 158,
{ 34: } 160,
{ 35: } 163,
{ 36: } 165,
{ 37: } 168,
{ 38: } 168,
{ 39: } 168,
{ 40: } 170,
{ 41: } 172,
{ 42: } 174,
{ 43: } 177,
{ 44: } 179,
{ 45: } 181,
{ 46: } 183,
{ 47: } 185,
{ 48: } 187,
{ 49: } 188,
{ 50: } 188,
{ 51: } 188,
{ 52: } 188,
{ 53: } 188,
{ 54: } 188,
{ 55: } 188,
{ 56: } 190,
{ 57: } 190,
{ 58: } 192,
{ 59: } 192,
{ 60: } 194,
{ 61: } 196,
{ 62: } 197,
{ 63: } 202,
{ 64: } 203,
{ 65: } 203,
{ 66: } 204,
{ 67: } 206,
{ 68: } 207,
{ 69: } 210,
{ 70: } 210,
{ 71: } 210,
{ 72: } 210,
{ 73: } 210,
{ 74: } 210,
{ 75: } 210,
{ 76: } 210,
{ 77: } 210,
{ 78: } 212,
{ 79: } 215,
{ 80: } 216,
{ 81: } 217,
{ 82: } 218,
{ 83: } 219,
{ 84: } 221,
{ 85: } 223,
{ 86: } 225,
{ 87: } 227,
{ 88: } 229,
{ 89: } 231,
{ 90: } 233,
{ 91: } 236,
{ 92: } 238,
{ 93: } 240,
{ 94: } 242,
{ 95: } 244,
{ 96: } 246,
{ 97: } 248,
{ 98: } 251,
{ 99: } 253,
{ 100: } 255,
{ 101: } 257,
{ 102: } 259,
{ 103: } 261,
{ 104: } 263,
{ 105: } 265,
{ 106: } 267,
{ 107: } 269,
{ 108: } 271,
{ 109: } 273,
{ 110: } 275,
{ 111: } 275,
{ 112: } 275,
{ 113: } 277,
{ 114: } 278,
{ 115: } 279,
{ 116: } 280,
{ 117: } 280,
{ 118: } 281,
{ 119: } 282,
{ 120: } 284,
{ 121: } 285,
{ 122: } 286,
{ 123: } 287,
{ 124: } 288,
{ 125: } 289,
{ 126: } 289,
{ 127: } 291,
{ 128: } 293,
{ 129: } 295,
{ 130: } 297,
{ 131: } 299,
{ 132: } 301,
{ 133: } 303,
{ 134: } 305,
{ 135: } 307,
{ 136: } 310,
{ 137: } 312,
{ 138: } 314,
{ 139: } 316,
{ 140: } 318,
{ 141: } 320,
{ 142: } 322,
{ 143: } 324,
{ 144: } 326,
{ 145: } 328,
{ 146: } 330,
{ 147: } 331,
{ 148: } 333,
{ 149: } 335,
{ 150: } 336,
{ 151: } 337,
{ 152: } 339,
{ 153: } 341,
{ 154: } 343,
{ 155: } 345,
{ 156: } 346,
{ 157: } 347,
{ 158: } 348,
{ 159: } 349,
{ 160: } 350,
{ 161: } 351,
{ 162: } 352,
{ 163: } 353,
{ 164: } 354,
{ 165: } 356,
{ 166: } 357,
{ 167: } 359,
{ 168: } 361,
{ 169: } 363,
{ 170: } 365,
{ 171: } 367,
{ 172: } 369,
{ 173: } 371,
{ 174: } 373,
{ 175: } 375,
{ 176: } 377,
{ 177: } 378,
{ 178: } 379,
{ 179: } 380,
{ 180: } 382,
{ 181: } 384,
{ 182: } 386,
{ 183: } 388,
{ 184: } 390,
{ 185: } 392,
{ 186: } 393,
{ 187: } 395,
{ 188: } 396,
{ 189: } 397,
{ 190: } 398,
{ 191: } 399,
{ 192: } 400,
{ 193: } 401,
{ 194: } 401,
{ 195: } 401,
{ 196: } 402,
{ 197: } 403,
{ 198: } 404,
{ 199: } 405,
{ 200: } 406,
{ 201: } 408,
{ 202: } 410,
{ 203: } 412,
{ 204: } 413,
{ 205: } 415,
{ 206: } 416,
{ 207: } 418,
{ 208: } 420,
{ 209: } 422,
{ 210: } 424,
{ 211: } 426,
{ 212: } 427,
{ 213: } 428,
{ 214: } 430,
{ 215: } 432,
{ 216: } 433,
{ 217: } 435,
{ 218: } 436,
{ 219: } 437,
{ 220: } 438,
{ 221: } 438,
{ 222: } 438,
{ 223: } 438,
{ 224: } 439,
{ 225: } 440,
{ 226: } 441,
{ 227: } 443,
{ 228: } 445,
{ 229: } 447,
{ 230: } 448,
{ 231: } 449,
{ 232: } 450,
{ 233: } 452,
{ 234: } 454,
{ 235: } 456,
{ 236: } 457,
{ 237: } 459,
{ 238: } 460,
{ 239: } 461,
{ 240: } 461,
{ 241: } 461,
{ 242: } 462,
{ 243: } 464,
{ 244: } 466,
{ 245: } 468,
{ 246: } 470,
{ 247: } 472,
{ 248: } 473,
{ 249: } 474,
{ 250: } 474,
{ 251: } 475,
{ 252: } 476,
{ 253: } 478,
{ 254: } 479,
{ 255: } 480,
{ 256: } 481,
{ 257: } 482,
{ 258: } 483,
{ 259: } 484,
{ 260: } 485,
{ 261: } 486,
{ 262: } 487,
{ 263: } 488,
{ 264: } 489,
{ 265: } 490,
{ 266: } 492,
{ 267: } 494,
{ 268: } 495,
{ 269: } 496,
{ 270: } 497,
{ 271: } 498,
{ 272: } 499,
{ 273: } 500,
{ 274: } 501,
{ 275: } 502,
{ 276: } 503,
{ 277: } 504,
{ 278: } 505,
{ 279: } 506,
{ 280: } 507,
{ 281: } 508,
{ 282: } 509,
{ 283: } 509,
{ 284: } 510,
{ 285: } 511,
{ 286: } 512,
{ 287: } 513,
{ 288: } 514,
{ 289: } 515,
{ 290: } 516,
{ 291: } 517,
{ 292: } 518,
{ 293: } 519,
{ 294: } 519
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

