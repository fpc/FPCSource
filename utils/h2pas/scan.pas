
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

  2:
                       begin
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
                                                      
                        writeln(outfile,'{ C++ extern C conditionnal removed }');
  50:
                                         
                        writeln(outfile,'{ C++ end of extern C conditionnal removed }');
                        
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
  54:
                        begin
                           write(outfile,'{$undef');
                                          c:=get_char;
                           while c<>newline do
                             begin write(outfile,c);c:=get_char;end;
                           writeln(outfile,'}');
                           flush(outfile);
                           next_line;
                        end;
  55:
                        begin
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
                        
  56:
                        begin
                           write(outfile,'{$include');
                                          c:=get_char;
                           while c<>newline do
                             begin write(outfile,c);c:=get_char;end;
                           writeln(outfile,'}');
                           flush(outfile);
                           block_type:=bt_no;
                           next_line;
                        end;
  57:
                        begin
                           write(outfile,'{$if');
                                          c:=get_char;
                           while c<>newline do
                             begin write(outfile,c);c:=get_char;end;
                           writeln(outfile,'}');
                           flush(outfile);
                           block_type:=bt_no;
                           next_line;
                        end;
  58:
                        begin
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
                        if arglevel=0 then
                          if in_space_define=2 then
                            begin
                               in_space_define:=0;
                               return(SPACE_DEFINE);
                            end;
  83:
                        begin
                           next_line;
                           if arglevel=0 then
                             if in_space_define=2 then
                               begin
                                  in_space_define:=0;
                                  return(SPACE_DEFINE);
                               end;
                        end;
  84:
                        begin
                           next_line;
                           if in_define then
                             begin
                                 in_define:=false;
                                 in_space_define:=0;
                                 return(NEW_LINE);
                             end;
                        end;
  85:
                        begin
                           writeln('Illegal character in line ',line_no);
                           writeln(last_source_line);
                           return(256 { error });
                        end;
  end;
end(*yyaction*);

(* DFA table: *)

type YYTRec = record
                cc : set of Char;
                s  : Integer;
              end;

const

yynmarks   = 301;
yynmatches = 301;
yyntrans   = 522;
yynstates  = 297;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  7,
  { 1: }
  7,
  { 2: }
  24,
  85,
  { 3: }
  85,
  { 4: }
  85,
  { 5: }
  7,
  80,
  85,
  { 6: }
  7,
  9,
  85,
  { 7: }
  7,
  80,
  85,
  { 8: }
  7,
  9,
  85,
  { 9: }
  11,
  85,
  { 10: }
  36,
  85,
  { 11: }
  23,
  85,
  { 12: }
  19,
  85,
  { 13: }
  20,
  85,
  { 14: }
  85,
  { 15: }
  21,
  85,
  { 16: }
  22,
  85,
  { 17: }
  25,
  85,
  { 18: }
  26,
  85,
  { 19: }
  27,
  85,
  { 20: }
  28,
  85,
  { 21: }
  29,
  85,
  { 22: }
  30,
  85,
  { 23: }
  31,
  85,
  { 24: }
  32,
  85,
  { 25: }
  33,
  85,
  { 26: }
  35,
  85,
  { 27: }
  80,
  85,
  { 28: }
  80,
  85,
  { 29: }
  80,
  85,
  { 30: }
  80,
  85,
  { 31: }
  80,
  85,
  { 32: }
  80,
  85,
  { 33: }
  80,
  85,
  { 34: }
  80,
  85,
  { 35: }
  80,
  85,
  { 36: }
  80,
  85,
  { 37: }
  80,
  85,
  { 38: }
  64,
  85,
  { 39: }
  65,
  85,
  { 40: }
  80,
  85,
  { 41: }
  80,
  85,
  { 42: }
  80,
  85,
  { 43: }
  80,
  85,
  { 44: }
  80,
  85,
  { 45: }
  80,
  85,
  { 46: }
  80,
  85,
  { 47: }
  80,
  85,
  { 48: }
  80,
  85,
  { 49: }
  80,
  85,
  { 50: }
  81,
  85,
  { 51: }
  82,
  85,
  { 52: }
  85,
  { 53: }
  84,
  { 54: }
  85,
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
  80,
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
  80,
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
  83,
  { 113: }
  5,
  { 114: }
  6,
  { 115: }
  9,
  { 116: }
  { 117: }
  9,
  { 118: }
  8,
  { 119: }
  8,
  { 120: }
  57,
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
  { 161: }
  { 162: }
  { 163: }
  { 164: }
  { 165: }
  { 166: }
  { 167: }
  80,
  { 168: }
  62,
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
  80,
  { 178: }
  80,
  { 179: }
  47,
  80,
  { 180: }
  48,
  80,
  { 181: }
  60,
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
  80,
  { 187: }
  80,
  { 188: }
  69,
  80,
  { 189: }
  80,
  { 190: }
  76,
  80,
  { 191: }
  77,
  80,
  { 192: }
  78,
  80,
  { 193: }
  79,
  80,
  { 194: }
  { 195: }
  { 196: }
  51,
  { 197: }
  53,
  { 198: }
  { 199: }
  { 200: }
  { 201: }
  { 202: }
  { 203: }
  80,
  { 204: }
  80,
  { 205: }
  80,
  { 206: }
  39,
  80,
  { 207: }
  80,
  { 208: }
  73,
  80,
  { 209: }
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
  72,
  80,
  { 215: }
  61,
  80,
  { 216: }
  80,
  { 217: }
  80,
  { 218: }
  68,
  80,
  { 219: }
  80,
  { 220: }
  71,
  80,
  { 221: }
  { 222: }
  { 223: }
  52,
  { 224: }
  55,
  { 225: }
  54,
  { 226: }
  { 227: }
  { 228: }
  37,
  80,
  { 229: }
  80,
  { 230: }
  80,
  { 231: }
  80,
  { 232: }
  40,
  80,
  { 233: }
  41,
  80,
  { 234: }
  42,
  80,
  { 235: }
  80,
  { 236: }
  80,
  { 237: }
  80,
  { 238: }
  63,
  80,
  { 239: }
  80,
  { 240: }
  { 241: }
  { 242: }
  58,
  { 243: }
  59,
  { 244: }
  38,
  80,
  { 245: }
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
  66,
  80,
  { 251: }
  { 252: }
  56,
  { 253: }
  43,
  80,
  { 254: }
  45,
  80,
  { 255: }
  80,
  { 256: }
  46,
  80,
  { 257: }
  70,
  80,
  { 258: }
  { 259: }
  44,
  80,
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
  { 284: }
  { 285: }
  50,
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
  49
);

yym : array [1..yynmatches] of Integer = (
{ 0: }
  7,
{ 1: }
  7,
{ 2: }
  24,
  85,
{ 3: }
  85,
{ 4: }
  85,
{ 5: }
  7,
  80,
  85,
{ 6: }
  7,
  9,
  85,
{ 7: }
  7,
  80,
  85,
{ 8: }
  7,
  9,
  85,
{ 9: }
  11,
  85,
{ 10: }
  36,
  85,
{ 11: }
  23,
  85,
{ 12: }
  19,
  85,
{ 13: }
  20,
  85,
{ 14: }
  85,
{ 15: }
  21,
  85,
{ 16: }
  22,
  85,
{ 17: }
  25,
  85,
{ 18: }
  26,
  85,
{ 19: }
  27,
  85,
{ 20: }
  28,
  85,
{ 21: }
  29,
  85,
{ 22: }
  30,
  85,
{ 23: }
  31,
  85,
{ 24: }
  32,
  85,
{ 25: }
  33,
  85,
{ 26: }
  35,
  85,
{ 27: }
  80,
  85,
{ 28: }
  80,
  85,
{ 29: }
  80,
  85,
{ 30: }
  80,
  85,
{ 31: }
  80,
  85,
{ 32: }
  80,
  85,
{ 33: }
  80,
  85,
{ 34: }
  80,
  85,
{ 35: }
  80,
  85,
{ 36: }
  80,
  85,
{ 37: }
  80,
  85,
{ 38: }
  64,
  85,
{ 39: }
  65,
  85,
{ 40: }
  80,
  85,
{ 41: }
  80,
  85,
{ 42: }
  80,
  85,
{ 43: }
  80,
  85,
{ 44: }
  80,
  85,
{ 45: }
  80,
  85,
{ 46: }
  80,
  85,
{ 47: }
  80,
  85,
{ 48: }
  80,
  85,
{ 49: }
  80,
  85,
{ 50: }
  81,
  85,
{ 51: }
  82,
  85,
{ 52: }
  85,
{ 53: }
  84,
{ 54: }
  85,
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
  80,
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
  80,
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
  83,
{ 113: }
  5,
{ 114: }
  6,
{ 115: }
  9,
{ 116: }
{ 117: }
  9,
{ 118: }
  8,
{ 119: }
  8,
{ 120: }
  57,
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
{ 161: }
{ 162: }
{ 163: }
{ 164: }
{ 165: }
{ 166: }
{ 167: }
  80,
{ 168: }
  62,
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
  80,
{ 178: }
  80,
{ 179: }
  47,
  80,
{ 180: }
  48,
  80,
{ 181: }
  60,
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
  80,
{ 187: }
  80,
{ 188: }
  69,
  80,
{ 189: }
  80,
{ 190: }
  76,
  80,
{ 191: }
  77,
  80,
{ 192: }
  78,
  80,
{ 193: }
  79,
  80,
{ 194: }
{ 195: }
{ 196: }
  51,
{ 197: }
  53,
{ 198: }
{ 199: }
{ 200: }
{ 201: }
{ 202: }
{ 203: }
  80,
{ 204: }
  80,
{ 205: }
  80,
{ 206: }
  39,
  80,
{ 207: }
  80,
{ 208: }
  73,
  80,
{ 209: }
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
  72,
  80,
{ 215: }
  61,
  80,
{ 216: }
  80,
{ 217: }
  80,
{ 218: }
  68,
  80,
{ 219: }
  80,
{ 220: }
  71,
  80,
{ 221: }
{ 222: }
{ 223: }
  52,
{ 224: }
  55,
{ 225: }
  54,
{ 226: }
{ 227: }
{ 228: }
  37,
  80,
{ 229: }
  80,
{ 230: }
  80,
{ 231: }
  80,
{ 232: }
  40,
  80,
{ 233: }
  41,
  80,
{ 234: }
  42,
  80,
{ 235: }
  80,
{ 236: }
  80,
{ 237: }
  80,
{ 238: }
  63,
  80,
{ 239: }
  80,
{ 240: }
{ 241: }
{ 242: }
  58,
{ 243: }
  59,
{ 244: }
  38,
  80,
{ 245: }
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
  66,
  80,
{ 251: }
{ 252: }
  56,
{ 253: }
  43,
  80,
{ 254: }
  45,
  80,
{ 255: }
  80,
{ 256: }
  46,
  80,
{ 257: }
  70,
  80,
{ 258: }
{ 259: }
  44,
  80,
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
{ 284: }
{ 285: }
  50,
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
  49
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
  ( cc: [ #1..#8,#11,#13..#31,'$','%','@','^','`','~'..#255 ]; s: 54),
  ( cc: [ #9,#12,' ' ]; s: 51),
  ( cc: [ #10 ]; s: 53),
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
  ( cc: [ '\' ]; s: 52),
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
  ( cc: [ #1..#8,#11,#13..#31,'$','%','@','^','`','~'..#255 ]; s: 54),
  ( cc: [ #9,#12,' ' ]; s: 51),
  ( cc: [ #10 ]; s: 53),
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
  ( cc: [ '\' ]; s: 52),
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
  ( cc: [ '#' ]; s: 78),
  ( cc: [ 'd' ]; s: 83),
  ( cc: [ 'e' ]; s: 80),
  ( cc: [ 'i' ]; s: 79),
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
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'w','y','z' ]; s: 63),
  ( cc: [ 'n' ]; s: 86),
  ( cc: [ 'x' ]; s: 85),
{ 28: }
  ( cc: [ '0'..'9','A'..'S','U'..'X','Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'T' ]; s: 87),
  ( cc: [ 'Y' ]; s: 88),
{ 29: }
  ( cc: [ '0'..'9','B','C','E'..'N','P'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 90),
  ( cc: [ 'D' ]; s: 89),
  ( cc: [ 'O' ]; s: 91),
{ 30: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 92),
{ 31: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'I' ]; s: 93),
{ 32: }
  ( cc: [ '0'..'9','A'..'W','Y','Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'X' ]; s: 94),
{ 33: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 95),
{ 34: }
  ( cc: [ '0'..'9','A'..'N','P'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'O' ]; s: 96),
{ 35: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','i'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'h' ]; s: 97),
  ( cc: [ 'o' ]; s: 98),
{ 36: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 99),
{ 37: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'g','i'..'s','u'..'z' ]; s: 63),
  ( cc: [ 'h' ]; s: 101),
  ( cc: [ 't' ]; s: 100),
{ 38: }
{ 39: }
{ 40: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'x','z' ]; s: 63),
  ( cc: [ 'y' ]; s: 102),
{ 41: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 103),
{ 42: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 104),
{ 43: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'k','m'..'z' ]; s: 63),
  ( cc: [ 'a' ]; s: 106),
  ( cc: [ 'l' ]; s: 105),
{ 44: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 107),
{ 45: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 108),
{ 46: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 109),
{ 47: }
  ( cc: [ '0'..'9','A'..'T','V'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'U' ]; s: 110),
{ 48: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 63),
  ( cc: [ 'u' ]; s: 111),
{ 49: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 50: }
{ 51: }
{ 52: }
  ( cc: [ #10 ]; s: 112),
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
  ( cc: [ '"' ]; s: 113),
{ 62: }
  ( cc: [ #1..'&','('..#255 ]; s: 62),
  ( cc: [ '''' ]; s: 114),
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
  ( cc: [ '0'..'9' ]; s: 115),
{ 68: }
  ( cc: [ '+','-' ]; s: 116),
  ( cc: [ '0'..'9' ]; s: 117),
{ 69: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 70: }
  ( cc: [ '0'..'9','A'..'F','a'..'f' ]; s: 70),
  ( cc: [ 'L' ]; s: 119),
  ( cc: [ 'U' ]; s: 118),
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
{ 77: }
{ 78: }
{ 79: }
  ( cc: [ 'f' ]; s: 120),
  ( cc: [ 'n' ]; s: 121),
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
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 129),
{ 86: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 63),
  ( cc: [ 'u' ]; s: 130),
{ 87: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'D' ]; s: 131),
{ 88: }
  ( cc: [ '0'..'9','A'..'R','T'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'S' ]; s: 132),
{ 89: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 133),
{ 90: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 134),
{ 91: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'N' ]; s: 135),
{ 92: }
  ( cc: [ '0'..'9','A','B','D'..'R','T'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 137),
  ( cc: [ 'S' ]; s: 136),
{ 93: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'N' ]; s: 138),
{ 94: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'P' ]; s: 139),
{ 95: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'z' ]; s: 63),
  ( cc: [ 'i' ]; s: 140),
{ 96: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'I' ]; s: 141),
{ 97: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 63),
  ( cc: [ 'a' ]; s: 142),
{ 98: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 143),
{ 99: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'r','t'..'z' ]; s: 63),
  ( cc: [ 'i' ]; s: 144),
  ( cc: [ 's' ]; s: 145),
{ 100: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 146),
{ 101: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 147),
{ 102: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'o','q'..'z' ]; s: 63),
  ( cc: [ 'p' ]; s: 148),
{ 103: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 149),
{ 104: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 150),
{ 105: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 151),
{ 106: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 152),
{ 107: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'R' ]; s: 153),
{ 108: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 154),
{ 109: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 63),
  ( cc: [ 'a' ]; s: 155),
{ 110: }
  ( cc: [ '0'..'9','A'..'F','H'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'G' ]; s: 156),
{ 111: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 63),
  ( cc: [ 'g' ]; s: 157),
{ 112: }
{ 113: }
{ 114: }
{ 115: }
  ( cc: [ '0'..'9' ]; s: 115),
  ( cc: [ 'E','e' ]; s: 68),
{ 116: }
  ( cc: [ '0'..'9' ]; s: 117),
{ 117: }
  ( cc: [ '0'..'9' ]; s: 117),
{ 118: }
  ( cc: [ 'L' ]; s: 119),
{ 119: }
{ 120: }
  ( cc: [ 'd' ]; s: 158),
{ 121: }
  ( cc: [ 'c' ]; s: 159),
{ 122: }
  ( cc: [ 'i' ]; s: 161),
  ( cc: [ 's' ]; s: 160),
{ 123: }
  ( cc: [ 'd' ]; s: 162),
{ 124: }
  ( cc: [ 'r' ]; s: 163),
{ 125: }
  ( cc: [ 'd' ]; s: 164),
{ 126: }
  ( cc: [ 'a' ]; s: 165),
{ 127: }
  ( cc: [ 'f' ]; s: 166),
{ 128: }
{ 129: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 167),
{ 130: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'l','n'..'z' ]; s: 63),
  ( cc: [ 'm' ]; s: 168),
{ 131: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 169),
{ 132: }
  ( cc: [ '0'..'9','A'..'Z','a'..'z' ]; s: 63),
  ( cc: [ '_' ]; s: 170),
{ 133: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 171),
{ 134: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 172),
{ 135: }
  ( cc: [ '0'..'9','A'..'R','T'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'S' ]; s: 173),
{ 136: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 174),
{ 137: }
  ( cc: [ '0'..'9','A'..'J','L'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'K' ]; s: 175),
{ 138: }
  ( cc: [ '0'..'9','B'..'F','H'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 176),
  ( cc: [ 'G' ]; s: 177),
{ 139: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 178),
{ 140: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 63),
  ( cc: [ 'd' ]; s: 179),
{ 141: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'D' ]; s: 180),
{ 142: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 181),
{ 143: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'r','t'..'z' ]; s: 63),
  ( cc: [ 's' ]; s: 182),
{ 144: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'n','p'..'z' ]; s: 63),
  ( cc: [ 'o' ]; s: 183),
{ 145: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'h','j'..'z' ]; s: 63),
  ( cc: [ 'i' ]; s: 184),
{ 146: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'t','v'..'z' ]; s: 63),
  ( cc: [ 'u' ]; s: 185),
{ 147: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 186),
{ 148: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 187),
{ 149: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 150: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 63),
  ( cc: [ 'g' ]; s: 188),
{ 151: }
  ( cc: [ '0'..'9','A'..'Z','_','b'..'z' ]; s: 63),
  ( cc: [ 'a' ]; s: 189),
{ 152: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 153: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 154: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'R' ]; s: 190),
{ 155: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 191),
{ 156: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 192),
{ 157: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 193),
{ 158: }
  ( cc: [ 'e' ]; s: 194),
{ 159: }
  ( cc: [ 'l' ]; s: 195),
{ 160: }
  ( cc: [ 'e' ]; s: 196),
{ 161: }
  ( cc: [ 'f' ]; s: 197),
{ 162: }
  ( cc: [ 'i' ]; s: 198),
{ 163: }
  ( cc: [ 'o' ]; s: 199),
{ 164: }
  ( cc: [ 'e' ]; s: 200),
{ 165: }
  ( cc: [ 'g' ]; s: 201),
{ 166: }
  ( cc: [ 'i' ]; s: 202),
{ 167: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'q','s'..'z' ]; s: 63),
  ( cc: [ 'r' ]; s: 203),
{ 168: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 169: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 204),
{ 170: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'T' ]; s: 205),
{ 171: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 206),
{ 172: }
  ( cc: [ '0'..'9','A','C'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'B' ]; s: 207),
{ 173: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'T' ]; s: 208),
{ 174: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 209),
{ 175: }
  ( cc: [ '0'..'9','A'..'D','F'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'E' ]; s: 210),
{ 176: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'P' ]; s: 211),
{ 177: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'D' ]; s: 212),
{ 178: }
  ( cc: [ '0'..'9','A'..'M','O'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'N' ]; s: 213),
{ 179: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 180: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 181: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 182: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 214),
{ 183: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 215),
{ 184: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'f','h'..'z' ]; s: 63),
  ( cc: [ 'g' ]; s: 216),
{ 185: }
  ( cc: [ '0'..'9','A'..'Z','_','a','b','d'..'z' ]; s: 63),
  ( cc: [ 'c' ]; s: 217),
{ 186: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 218),
{ 187: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 63),
  ( cc: [ 'd' ]; s: 219),
{ 188: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 189: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 220),
{ 190: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 191: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 192: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 193: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 194: }
  ( cc: [ 'f' ]; s: 221),
{ 195: }
  ( cc: [ 'u' ]; s: 222),
{ 196: }
{ 197: }
{ 198: }
  ( cc: [ 'f' ]; s: 223),
{ 199: }
  ( cc: [ 'r' ]; s: 224),
{ 200: }
  ( cc: [ 'f' ]; s: 225),
{ 201: }
  ( cc: [ 'm' ]; s: 226),
{ 202: }
  ( cc: [ 'n' ]; s: 227),
{ 203: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 228),
{ 204: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 229),
{ 205: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'R' ]; s: 230),
{ 206: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 207: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 231),
{ 208: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 209: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 232),
{ 210: }
  ( cc: [ '0'..'9','A'..'C','E'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'D' ]; s: 233),
{ 211: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'I' ]; s: 234),
{ 212: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'I' ]; s: 235),
{ 213: }
  ( cc: [ '0'..'9','A'..'S','U'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'T' ]; s: 236),
{ 214: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 215: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 216: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'m','o'..'z' ]; s: 63),
  ( cc: [ 'n' ]; s: 237),
{ 217: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'s','u'..'z' ]; s: 63),
  ( cc: [ 't' ]; s: 238),
{ 218: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 219: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 239),
{ 220: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 221: }
  ( cc: [ ' ' ]; s: 240),
{ 222: }
  ( cc: [ 'd' ]; s: 241),
{ 223: }
{ 224: }
{ 225: }
{ 226: }
  ( cc: [ 'a' ]; s: 242),
{ 227: }
  ( cc: [ 'e' ]; s: 243),
{ 228: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 229: }
  ( cc: [ '0'..'9','A'..'K','M'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'L' ]; s: 244),
{ 230: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 245),
{ 231: }
  ( cc: [ '0'..'9','A','B','D'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'C' ]; s: 246),
{ 232: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 233: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 234: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 235: }
  ( cc: [ '0'..'9','B'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'A' ]; s: 247),
{ 236: }
  ( cc: [ '0'..'9','A'..'Q','S'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'R' ]; s: 248),
{ 237: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'d','f'..'z' ]; s: 63),
  ( cc: [ 'e' ]; s: 249),
{ 238: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 239: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'e','g'..'z' ]; s: 63),
  ( cc: [ 'f' ]; s: 250),
{ 240: }
  ( cc: [ '_' ]; s: 251),
{ 241: }
  ( cc: [ 'e' ]; s: 252),
{ 242: }
{ 243: }
{ 244: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 245: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'P' ]; s: 253),
{ 246: }
  ( cc: [ '0'..'9','A'..'J','L'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'K' ]; s: 254),
{ 247: }
  ( cc: [ '0'..'9','A'..'O','Q'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'P' ]; s: 255),
{ 248: }
  ( cc: [ '0'..'9','A'..'X','Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'Y' ]; s: 256),
{ 249: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'c','e'..'z' ]; s: 63),
  ( cc: [ 'd' ]; s: 257),
{ 250: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 251: }
  ( cc: [ '_' ]; s: 258),
{ 252: }
{ 253: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 254: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 255: }
  ( cc: [ '0'..'9','A'..'H','J'..'Z','_','a'..'z' ]; s: 63),
  ( cc: [ 'I' ]; s: 259),
{ 256: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 257: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 258: }
  ( cc: [ 'c' ]; s: 260),
{ 259: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 63),
{ 260: }
  ( cc: [ 'p' ]; s: 261),
{ 261: }
  ( cc: [ 'l' ]; s: 262),
{ 262: }
  ( cc: [ 'u' ]; s: 263),
{ 263: }
  ( cc: [ 's' ]; s: 264),
{ 264: }
  ( cc: [ 'p' ]; s: 265),
{ 265: }
  ( cc: [ 'l' ]; s: 266),
{ 266: }
  ( cc: [ 'u' ]; s: 267),
{ 267: }
  ( cc: [ 's' ]; s: 268),
{ 268: }
  ( cc: [ #9,' ' ]; s: 268),
  ( cc: [ #10 ]; s: 269),
{ 269: }
  ( cc: [ 'e' ]; s: 270),
  ( cc: [ '}' ]; s: 271),
{ 270: }
  ( cc: [ 'x' ]; s: 272),
{ 271: }
  ( cc: [ #10 ]; s: 273),
{ 272: }
  ( cc: [ 't' ]; s: 274),
{ 273: }
  ( cc: [ '#' ]; s: 275),
{ 274: }
  ( cc: [ 'e' ]; s: 276),
{ 275: }
  ( cc: [ 'e' ]; s: 277),
{ 276: }
  ( cc: [ 'r' ]; s: 278),
{ 277: }
  ( cc: [ 'n' ]; s: 279),
{ 278: }
  ( cc: [ 'n' ]; s: 280),
{ 279: }
  ( cc: [ 'd' ]; s: 281),
{ 280: }
  ( cc: [ ' ' ]; s: 282),
{ 281: }
  ( cc: [ 'i' ]; s: 283),
{ 282: }
  ( cc: [ '"' ]; s: 284),
{ 283: }
  ( cc: [ 'f' ]; s: 285),
{ 284: }
  ( cc: [ 'C' ]; s: 286),
{ 285: }
{ 286: }
  ( cc: [ '"' ]; s: 287),
{ 287: }
  ( cc: [ ' ' ]; s: 288),
{ 288: }
  ( cc: [ '{' ]; s: 289),
{ 289: }
  ( cc: [ #10 ]; s: 290),
{ 290: }
  ( cc: [ '#' ]; s: 291),
{ 291: }
  ( cc: [ 'e' ]; s: 292),
{ 292: }
  ( cc: [ 'n' ]; s: 293),
{ 293: }
  ( cc: [ 'd' ]; s: 294),
{ 294: }
  ( cc: [ 'i' ]; s: 295),
{ 295: }
  ( cc: [ 'f' ]; s: 296)
{ 296: }
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
{ 57: } 109,
{ 58: } 109,
{ 59: } 110,
{ 60: } 110,
{ 61: } 111,
{ 62: } 111,
{ 63: } 111,
{ 64: } 112,
{ 65: } 114,
{ 66: } 115,
{ 67: } 116,
{ 68: } 116,
{ 69: } 116,
{ 70: } 118,
{ 71: } 119,
{ 72: } 120,
{ 73: } 121,
{ 74: } 122,
{ 75: } 123,
{ 76: } 124,
{ 77: } 125,
{ 78: } 126,
{ 79: } 127,
{ 80: } 127,
{ 81: } 127,
{ 82: } 127,
{ 83: } 127,
{ 84: } 127,
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
{ 115: } 157,
{ 116: } 158,
{ 117: } 158,
{ 118: } 159,
{ 119: } 160,
{ 120: } 161,
{ 121: } 162,
{ 122: } 162,
{ 123: } 162,
{ 124: } 162,
{ 125: } 162,
{ 126: } 162,
{ 127: } 162,
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
{ 148: } 182,
{ 149: } 183,
{ 150: } 185,
{ 151: } 186,
{ 152: } 187,
{ 153: } 189,
{ 154: } 191,
{ 155: } 192,
{ 156: } 193,
{ 157: } 194,
{ 158: } 195,
{ 159: } 195,
{ 160: } 195,
{ 161: } 195,
{ 162: } 195,
{ 163: } 195,
{ 164: } 195,
{ 165: } 195,
{ 166: } 195,
{ 167: } 195,
{ 168: } 196,
{ 169: } 198,
{ 170: } 199,
{ 171: } 200,
{ 172: } 201,
{ 173: } 202,
{ 174: } 203,
{ 175: } 204,
{ 176: } 205,
{ 177: } 206,
{ 178: } 207,
{ 179: } 208,
{ 180: } 210,
{ 181: } 212,
{ 182: } 214,
{ 183: } 215,
{ 184: } 216,
{ 185: } 217,
{ 186: } 218,
{ 187: } 219,
{ 188: } 220,
{ 189: } 222,
{ 190: } 223,
{ 191: } 225,
{ 192: } 227,
{ 193: } 229,
{ 194: } 231,
{ 195: } 231,
{ 196: } 231,
{ 197: } 232,
{ 198: } 233,
{ 199: } 233,
{ 200: } 233,
{ 201: } 233,
{ 202: } 233,
{ 203: } 233,
{ 204: } 234,
{ 205: } 235,
{ 206: } 236,
{ 207: } 238,
{ 208: } 239,
{ 209: } 241,
{ 210: } 242,
{ 211: } 243,
{ 212: } 244,
{ 213: } 245,
{ 214: } 246,
{ 215: } 248,
{ 216: } 250,
{ 217: } 251,
{ 218: } 252,
{ 219: } 254,
{ 220: } 255,
{ 221: } 257,
{ 222: } 257,
{ 223: } 257,
{ 224: } 258,
{ 225: } 259,
{ 226: } 260,
{ 227: } 260,
{ 228: } 260,
{ 229: } 262,
{ 230: } 263,
{ 231: } 264,
{ 232: } 265,
{ 233: } 267,
{ 234: } 269,
{ 235: } 271,
{ 236: } 272,
{ 237: } 273,
{ 238: } 274,
{ 239: } 276,
{ 240: } 277,
{ 241: } 277,
{ 242: } 277,
{ 243: } 278,
{ 244: } 279,
{ 245: } 281,
{ 246: } 282,
{ 247: } 283,
{ 248: } 284,
{ 249: } 285,
{ 250: } 286,
{ 251: } 288,
{ 252: } 288,
{ 253: } 289,
{ 254: } 291,
{ 255: } 293,
{ 256: } 294,
{ 257: } 296,
{ 258: } 298,
{ 259: } 298,
{ 260: } 300,
{ 261: } 300,
{ 262: } 300,
{ 263: } 300,
{ 264: } 300,
{ 265: } 300,
{ 266: } 300,
{ 267: } 300,
{ 268: } 300,
{ 269: } 300,
{ 270: } 300,
{ 271: } 300,
{ 272: } 300,
{ 273: } 300,
{ 274: } 300,
{ 275: } 300,
{ 276: } 300,
{ 277: } 300,
{ 278: } 300,
{ 279: } 300,
{ 280: } 300,
{ 281: } 300,
{ 282: } 300,
{ 283: } 300,
{ 284: } 300,
{ 285: } 300,
{ 286: } 301,
{ 287: } 301,
{ 288: } 301,
{ 289: } 301,
{ 290: } 301,
{ 291: } 301,
{ 292: } 301,
{ 293: } 301,
{ 294: } 301,
{ 295: } 301,
{ 296: } 301
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
{ 115: } 157,
{ 116: } 157,
{ 117: } 158,
{ 118: } 159,
{ 119: } 160,
{ 120: } 161,
{ 121: } 161,
{ 122: } 161,
{ 123: } 161,
{ 124: } 161,
{ 125: } 161,
{ 126: } 161,
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
{ 148: } 182,
{ 149: } 184,
{ 150: } 185,
{ 151: } 186,
{ 152: } 188,
{ 153: } 190,
{ 154: } 191,
{ 155: } 192,
{ 156: } 193,
{ 157: } 194,
{ 158: } 194,
{ 159: } 194,
{ 160: } 194,
{ 161: } 194,
{ 162: } 194,
{ 163: } 194,
{ 164: } 194,
{ 165: } 194,
{ 166: } 194,
{ 167: } 195,
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
{ 178: } 207,
{ 179: } 209,
{ 180: } 211,
{ 181: } 213,
{ 182: } 214,
{ 183: } 215,
{ 184: } 216,
{ 185: } 217,
{ 186: } 218,
{ 187: } 219,
{ 188: } 221,
{ 189: } 222,
{ 190: } 224,
{ 191: } 226,
{ 192: } 228,
{ 193: } 230,
{ 194: } 230,
{ 195: } 230,
{ 196: } 231,
{ 197: } 232,
{ 198: } 232,
{ 199: } 232,
{ 200: } 232,
{ 201: } 232,
{ 202: } 232,
{ 203: } 233,
{ 204: } 234,
{ 205: } 235,
{ 206: } 237,
{ 207: } 238,
{ 208: } 240,
{ 209: } 241,
{ 210: } 242,
{ 211: } 243,
{ 212: } 244,
{ 213: } 245,
{ 214: } 247,
{ 215: } 249,
{ 216: } 250,
{ 217: } 251,
{ 218: } 253,
{ 219: } 254,
{ 220: } 256,
{ 221: } 256,
{ 222: } 256,
{ 223: } 257,
{ 224: } 258,
{ 225: } 259,
{ 226: } 259,
{ 227: } 259,
{ 228: } 261,
{ 229: } 262,
{ 230: } 263,
{ 231: } 264,
{ 232: } 266,
{ 233: } 268,
{ 234: } 270,
{ 235: } 271,
{ 236: } 272,
{ 237: } 273,
{ 238: } 275,
{ 239: } 276,
{ 240: } 276,
{ 241: } 276,
{ 242: } 277,
{ 243: } 278,
{ 244: } 280,
{ 245: } 281,
{ 246: } 282,
{ 247: } 283,
{ 248: } 284,
{ 249: } 285,
{ 250: } 287,
{ 251: } 287,
{ 252: } 288,
{ 253: } 290,
{ 254: } 292,
{ 255: } 293,
{ 256: } 295,
{ 257: } 297,
{ 258: } 297,
{ 259: } 299,
{ 260: } 299,
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
{ 285: } 300,
{ 286: } 300,
{ 287: } 300,
{ 288: } 300,
{ 289: } 300,
{ 290: } 300,
{ 291: } 300,
{ 292: } 300,
{ 293: } 300,
{ 294: } 300,
{ 295: } 300,
{ 296: } 301
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
{ 57: } 109,
{ 58: } 109,
{ 59: } 110,
{ 60: } 110,
{ 61: } 111,
{ 62: } 111,
{ 63: } 111,
{ 64: } 112,
{ 65: } 114,
{ 66: } 115,
{ 67: } 116,
{ 68: } 116,
{ 69: } 116,
{ 70: } 118,
{ 71: } 119,
{ 72: } 120,
{ 73: } 121,
{ 74: } 122,
{ 75: } 123,
{ 76: } 124,
{ 77: } 125,
{ 78: } 126,
{ 79: } 127,
{ 80: } 127,
{ 81: } 127,
{ 82: } 127,
{ 83: } 127,
{ 84: } 127,
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
{ 115: } 157,
{ 116: } 158,
{ 117: } 158,
{ 118: } 159,
{ 119: } 160,
{ 120: } 161,
{ 121: } 162,
{ 122: } 162,
{ 123: } 162,
{ 124: } 162,
{ 125: } 162,
{ 126: } 162,
{ 127: } 162,
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
{ 148: } 182,
{ 149: } 183,
{ 150: } 185,
{ 151: } 186,
{ 152: } 187,
{ 153: } 189,
{ 154: } 191,
{ 155: } 192,
{ 156: } 193,
{ 157: } 194,
{ 158: } 195,
{ 159: } 195,
{ 160: } 195,
{ 161: } 195,
{ 162: } 195,
{ 163: } 195,
{ 164: } 195,
{ 165: } 195,
{ 166: } 195,
{ 167: } 195,
{ 168: } 196,
{ 169: } 198,
{ 170: } 199,
{ 171: } 200,
{ 172: } 201,
{ 173: } 202,
{ 174: } 203,
{ 175: } 204,
{ 176: } 205,
{ 177: } 206,
{ 178: } 207,
{ 179: } 208,
{ 180: } 210,
{ 181: } 212,
{ 182: } 214,
{ 183: } 215,
{ 184: } 216,
{ 185: } 217,
{ 186: } 218,
{ 187: } 219,
{ 188: } 220,
{ 189: } 222,
{ 190: } 223,
{ 191: } 225,
{ 192: } 227,
{ 193: } 229,
{ 194: } 231,
{ 195: } 231,
{ 196: } 231,
{ 197: } 232,
{ 198: } 233,
{ 199: } 233,
{ 200: } 233,
{ 201: } 233,
{ 202: } 233,
{ 203: } 233,
{ 204: } 234,
{ 205: } 235,
{ 206: } 236,
{ 207: } 238,
{ 208: } 239,
{ 209: } 241,
{ 210: } 242,
{ 211: } 243,
{ 212: } 244,
{ 213: } 245,
{ 214: } 246,
{ 215: } 248,
{ 216: } 250,
{ 217: } 251,
{ 218: } 252,
{ 219: } 254,
{ 220: } 255,
{ 221: } 257,
{ 222: } 257,
{ 223: } 257,
{ 224: } 258,
{ 225: } 259,
{ 226: } 260,
{ 227: } 260,
{ 228: } 260,
{ 229: } 262,
{ 230: } 263,
{ 231: } 264,
{ 232: } 265,
{ 233: } 267,
{ 234: } 269,
{ 235: } 271,
{ 236: } 272,
{ 237: } 273,
{ 238: } 274,
{ 239: } 276,
{ 240: } 277,
{ 241: } 277,
{ 242: } 277,
{ 243: } 278,
{ 244: } 279,
{ 245: } 281,
{ 246: } 282,
{ 247: } 283,
{ 248: } 284,
{ 249: } 285,
{ 250: } 286,
{ 251: } 288,
{ 252: } 288,
{ 253: } 289,
{ 254: } 291,
{ 255: } 293,
{ 256: } 294,
{ 257: } 296,
{ 258: } 298,
{ 259: } 298,
{ 260: } 300,
{ 261: } 300,
{ 262: } 300,
{ 263: } 300,
{ 264: } 300,
{ 265: } 300,
{ 266: } 300,
{ 267: } 300,
{ 268: } 300,
{ 269: } 300,
{ 270: } 300,
{ 271: } 300,
{ 272: } 300,
{ 273: } 300,
{ 274: } 300,
{ 275: } 300,
{ 276: } 300,
{ 277: } 300,
{ 278: } 300,
{ 279: } 300,
{ 280: } 300,
{ 281: } 300,
{ 282: } 300,
{ 283: } 300,
{ 284: } 300,
{ 285: } 300,
{ 286: } 301,
{ 287: } 301,
{ 288: } 301,
{ 289: } 301,
{ 290: } 301,
{ 291: } 301,
{ 292: } 301,
{ 293: } 301,
{ 294: } 301,
{ 295: } 301,
{ 296: } 301
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
{ 115: } 157,
{ 116: } 157,
{ 117: } 158,
{ 118: } 159,
{ 119: } 160,
{ 120: } 161,
{ 121: } 161,
{ 122: } 161,
{ 123: } 161,
{ 124: } 161,
{ 125: } 161,
{ 126: } 161,
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
{ 148: } 182,
{ 149: } 184,
{ 150: } 185,
{ 151: } 186,
{ 152: } 188,
{ 153: } 190,
{ 154: } 191,
{ 155: } 192,
{ 156: } 193,
{ 157: } 194,
{ 158: } 194,
{ 159: } 194,
{ 160: } 194,
{ 161: } 194,
{ 162: } 194,
{ 163: } 194,
{ 164: } 194,
{ 165: } 194,
{ 166: } 194,
{ 167: } 195,
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
{ 178: } 207,
{ 179: } 209,
{ 180: } 211,
{ 181: } 213,
{ 182: } 214,
{ 183: } 215,
{ 184: } 216,
{ 185: } 217,
{ 186: } 218,
{ 187: } 219,
{ 188: } 221,
{ 189: } 222,
{ 190: } 224,
{ 191: } 226,
{ 192: } 228,
{ 193: } 230,
{ 194: } 230,
{ 195: } 230,
{ 196: } 231,
{ 197: } 232,
{ 198: } 232,
{ 199: } 232,
{ 200: } 232,
{ 201: } 232,
{ 202: } 232,
{ 203: } 233,
{ 204: } 234,
{ 205: } 235,
{ 206: } 237,
{ 207: } 238,
{ 208: } 240,
{ 209: } 241,
{ 210: } 242,
{ 211: } 243,
{ 212: } 244,
{ 213: } 245,
{ 214: } 247,
{ 215: } 249,
{ 216: } 250,
{ 217: } 251,
{ 218: } 253,
{ 219: } 254,
{ 220: } 256,
{ 221: } 256,
{ 222: } 256,
{ 223: } 257,
{ 224: } 258,
{ 225: } 259,
{ 226: } 259,
{ 227: } 259,
{ 228: } 261,
{ 229: } 262,
{ 230: } 263,
{ 231: } 264,
{ 232: } 266,
{ 233: } 268,
{ 234: } 270,
{ 235: } 271,
{ 236: } 272,
{ 237: } 273,
{ 238: } 275,
{ 239: } 276,
{ 240: } 276,
{ 241: } 276,
{ 242: } 277,
{ 243: } 278,
{ 244: } 280,
{ 245: } 281,
{ 246: } 282,
{ 247: } 283,
{ 248: } 284,
{ 249: } 285,
{ 250: } 287,
{ 251: } 287,
{ 252: } 288,
{ 253: } 290,
{ 254: } 292,
{ 255: } 293,
{ 256: } 295,
{ 257: } 297,
{ 258: } 297,
{ 259: } 299,
{ 260: } 299,
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
{ 285: } 300,
{ 286: } 300,
{ 287: } 300,
{ 288: } 300,
{ 289: } 300,
{ 290: } 300,
{ 291: } 300,
{ 292: } 300,
{ 293: } 300,
{ 294: } 300,
{ 295: } 300,
{ 296: } 301
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
{ 38: } 171,
{ 39: } 171,
{ 40: } 171,
{ 41: } 173,
{ 42: } 175,
{ 43: } 177,
{ 44: } 180,
{ 45: } 182,
{ 46: } 184,
{ 47: } 186,
{ 48: } 188,
{ 49: } 190,
{ 50: } 191,
{ 51: } 191,
{ 52: } 191,
{ 53: } 192,
{ 54: } 192,
{ 55: } 192,
{ 56: } 192,
{ 57: } 192,
{ 58: } 194,
{ 59: } 194,
{ 60: } 196,
{ 61: } 196,
{ 62: } 198,
{ 63: } 200,
{ 64: } 201,
{ 65: } 206,
{ 66: } 207,
{ 67: } 207,
{ 68: } 208,
{ 69: } 210,
{ 70: } 211,
{ 71: } 214,
{ 72: } 214,
{ 73: } 214,
{ 74: } 214,
{ 75: } 214,
{ 76: } 214,
{ 77: } 214,
{ 78: } 214,
{ 79: } 214,
{ 80: } 216,
{ 81: } 219,
{ 82: } 220,
{ 83: } 221,
{ 84: } 222,
{ 85: } 223,
{ 86: } 225,
{ 87: } 227,
{ 88: } 229,
{ 89: } 231,
{ 90: } 233,
{ 91: } 235,
{ 92: } 237,
{ 93: } 240,
{ 94: } 242,
{ 95: } 244,
{ 96: } 246,
{ 97: } 248,
{ 98: } 250,
{ 99: } 252,
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
{ 111: } 277,
{ 112: } 279,
{ 113: } 279,
{ 114: } 279,
{ 115: } 279,
{ 116: } 281,
{ 117: } 282,
{ 118: } 283,
{ 119: } 284,
{ 120: } 284,
{ 121: } 285,
{ 122: } 286,
{ 123: } 288,
{ 124: } 289,
{ 125: } 290,
{ 126: } 291,
{ 127: } 292,
{ 128: } 293,
{ 129: } 293,
{ 130: } 295,
{ 131: } 297,
{ 132: } 299,
{ 133: } 301,
{ 134: } 303,
{ 135: } 305,
{ 136: } 307,
{ 137: } 309,
{ 138: } 311,
{ 139: } 314,
{ 140: } 316,
{ 141: } 318,
{ 142: } 320,
{ 143: } 322,
{ 144: } 324,
{ 145: } 326,
{ 146: } 328,
{ 147: } 330,
{ 148: } 332,
{ 149: } 334,
{ 150: } 335,
{ 151: } 337,
{ 152: } 339,
{ 153: } 340,
{ 154: } 341,
{ 155: } 343,
{ 156: } 345,
{ 157: } 347,
{ 158: } 349,
{ 159: } 350,
{ 160: } 351,
{ 161: } 352,
{ 162: } 353,
{ 163: } 354,
{ 164: } 355,
{ 165: } 356,
{ 166: } 357,
{ 167: } 358,
{ 168: } 360,
{ 169: } 361,
{ 170: } 363,
{ 171: } 365,
{ 172: } 367,
{ 173: } 369,
{ 174: } 371,
{ 175: } 373,
{ 176: } 375,
{ 177: } 377,
{ 178: } 379,
{ 179: } 381,
{ 180: } 382,
{ 181: } 383,
{ 182: } 384,
{ 183: } 386,
{ 184: } 388,
{ 185: } 390,
{ 186: } 392,
{ 187: } 394,
{ 188: } 396,
{ 189: } 397,
{ 190: } 399,
{ 191: } 400,
{ 192: } 401,
{ 193: } 402,
{ 194: } 403,
{ 195: } 404,
{ 196: } 405,
{ 197: } 405,
{ 198: } 405,
{ 199: } 406,
{ 200: } 407,
{ 201: } 408,
{ 202: } 409,
{ 203: } 410,
{ 204: } 412,
{ 205: } 414,
{ 206: } 416,
{ 207: } 417,
{ 208: } 419,
{ 209: } 420,
{ 210: } 422,
{ 211: } 424,
{ 212: } 426,
{ 213: } 428,
{ 214: } 430,
{ 215: } 431,
{ 216: } 432,
{ 217: } 434,
{ 218: } 436,
{ 219: } 437,
{ 220: } 439,
{ 221: } 440,
{ 222: } 441,
{ 223: } 442,
{ 224: } 442,
{ 225: } 442,
{ 226: } 442,
{ 227: } 443,
{ 228: } 444,
{ 229: } 445,
{ 230: } 447,
{ 231: } 449,
{ 232: } 451,
{ 233: } 452,
{ 234: } 453,
{ 235: } 454,
{ 236: } 456,
{ 237: } 458,
{ 238: } 460,
{ 239: } 461,
{ 240: } 463,
{ 241: } 464,
{ 242: } 465,
{ 243: } 465,
{ 244: } 465,
{ 245: } 466,
{ 246: } 468,
{ 247: } 470,
{ 248: } 472,
{ 249: } 474,
{ 250: } 476,
{ 251: } 477,
{ 252: } 478,
{ 253: } 478,
{ 254: } 479,
{ 255: } 480,
{ 256: } 482,
{ 257: } 483,
{ 258: } 484,
{ 259: } 485,
{ 260: } 486,
{ 261: } 487,
{ 262: } 488,
{ 263: } 489,
{ 264: } 490,
{ 265: } 491,
{ 266: } 492,
{ 267: } 493,
{ 268: } 494,
{ 269: } 496,
{ 270: } 498,
{ 271: } 499,
{ 272: } 500,
{ 273: } 501,
{ 274: } 502,
{ 275: } 503,
{ 276: } 504,
{ 277: } 505,
{ 278: } 506,
{ 279: } 507,
{ 280: } 508,
{ 281: } 509,
{ 282: } 510,
{ 283: } 511,
{ 284: } 512,
{ 285: } 513,
{ 286: } 513,
{ 287: } 514,
{ 288: } 515,
{ 289: } 516,
{ 290: } 517,
{ 291: } 518,
{ 292: } 519,
{ 293: } 520,
{ 294: } 521,
{ 295: } 522,
{ 296: } 523
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
{ 37: } 170,
{ 38: } 170,
{ 39: } 170,
{ 40: } 172,
{ 41: } 174,
{ 42: } 176,
{ 43: } 179,
{ 44: } 181,
{ 45: } 183,
{ 46: } 185,
{ 47: } 187,
{ 48: } 189,
{ 49: } 190,
{ 50: } 190,
{ 51: } 190,
{ 52: } 191,
{ 53: } 191,
{ 54: } 191,
{ 55: } 191,
{ 56: } 191,
{ 57: } 193,
{ 58: } 193,
{ 59: } 195,
{ 60: } 195,
{ 61: } 197,
{ 62: } 199,
{ 63: } 200,
{ 64: } 205,
{ 65: } 206,
{ 66: } 206,
{ 67: } 207,
{ 68: } 209,
{ 69: } 210,
{ 70: } 213,
{ 71: } 213,
{ 72: } 213,
{ 73: } 213,
{ 74: } 213,
{ 75: } 213,
{ 76: } 213,
{ 77: } 213,
{ 78: } 213,
{ 79: } 215,
{ 80: } 218,
{ 81: } 219,
{ 82: } 220,
{ 83: } 221,
{ 84: } 222,
{ 85: } 224,
{ 86: } 226,
{ 87: } 228,
{ 88: } 230,
{ 89: } 232,
{ 90: } 234,
{ 91: } 236,
{ 92: } 239,
{ 93: } 241,
{ 94: } 243,
{ 95: } 245,
{ 96: } 247,
{ 97: } 249,
{ 98: } 251,
{ 99: } 254,
{ 100: } 256,
{ 101: } 258,
{ 102: } 260,
{ 103: } 262,
{ 104: } 264,
{ 105: } 266,
{ 106: } 268,
{ 107: } 270,
{ 108: } 272,
{ 109: } 274,
{ 110: } 276,
{ 111: } 278,
{ 112: } 278,
{ 113: } 278,
{ 114: } 278,
{ 115: } 280,
{ 116: } 281,
{ 117: } 282,
{ 118: } 283,
{ 119: } 283,
{ 120: } 284,
{ 121: } 285,
{ 122: } 287,
{ 123: } 288,
{ 124: } 289,
{ 125: } 290,
{ 126: } 291,
{ 127: } 292,
{ 128: } 292,
{ 129: } 294,
{ 130: } 296,
{ 131: } 298,
{ 132: } 300,
{ 133: } 302,
{ 134: } 304,
{ 135: } 306,
{ 136: } 308,
{ 137: } 310,
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
{ 148: } 333,
{ 149: } 334,
{ 150: } 336,
{ 151: } 338,
{ 152: } 339,
{ 153: } 340,
{ 154: } 342,
{ 155: } 344,
{ 156: } 346,
{ 157: } 348,
{ 158: } 349,
{ 159: } 350,
{ 160: } 351,
{ 161: } 352,
{ 162: } 353,
{ 163: } 354,
{ 164: } 355,
{ 165: } 356,
{ 166: } 357,
{ 167: } 359,
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
{ 178: } 380,
{ 179: } 381,
{ 180: } 382,
{ 181: } 383,
{ 182: } 385,
{ 183: } 387,
{ 184: } 389,
{ 185: } 391,
{ 186: } 393,
{ 187: } 395,
{ 188: } 396,
{ 189: } 398,
{ 190: } 399,
{ 191: } 400,
{ 192: } 401,
{ 193: } 402,
{ 194: } 403,
{ 195: } 404,
{ 196: } 404,
{ 197: } 404,
{ 198: } 405,
{ 199: } 406,
{ 200: } 407,
{ 201: } 408,
{ 202: } 409,
{ 203: } 411,
{ 204: } 413,
{ 205: } 415,
{ 206: } 416,
{ 207: } 418,
{ 208: } 419,
{ 209: } 421,
{ 210: } 423,
{ 211: } 425,
{ 212: } 427,
{ 213: } 429,
{ 214: } 430,
{ 215: } 431,
{ 216: } 433,
{ 217: } 435,
{ 218: } 436,
{ 219: } 438,
{ 220: } 439,
{ 221: } 440,
{ 222: } 441,
{ 223: } 441,
{ 224: } 441,
{ 225: } 441,
{ 226: } 442,
{ 227: } 443,
{ 228: } 444,
{ 229: } 446,
{ 230: } 448,
{ 231: } 450,
{ 232: } 451,
{ 233: } 452,
{ 234: } 453,
{ 235: } 455,
{ 236: } 457,
{ 237: } 459,
{ 238: } 460,
{ 239: } 462,
{ 240: } 463,
{ 241: } 464,
{ 242: } 464,
{ 243: } 464,
{ 244: } 465,
{ 245: } 467,
{ 246: } 469,
{ 247: } 471,
{ 248: } 473,
{ 249: } 475,
{ 250: } 476,
{ 251: } 477,
{ 252: } 477,
{ 253: } 478,
{ 254: } 479,
{ 255: } 481,
{ 256: } 482,
{ 257: } 483,
{ 258: } 484,
{ 259: } 485,
{ 260: } 486,
{ 261: } 487,
{ 262: } 488,
{ 263: } 489,
{ 264: } 490,
{ 265: } 491,
{ 266: } 492,
{ 267: } 493,
{ 268: } 495,
{ 269: } 497,
{ 270: } 498,
{ 271: } 499,
{ 272: } 500,
{ 273: } 501,
{ 274: } 502,
{ 275: } 503,
{ 276: } 504,
{ 277: } 505,
{ 278: } 506,
{ 279: } 507,
{ 280: } 508,
{ 281: } 509,
{ 282: } 510,
{ 283: } 511,
{ 284: } 512,
{ 285: } 512,
{ 286: } 513,
{ 287: } 514,
{ 288: } 515,
{ 289: } 516,
{ 290: } 517,
{ 291: } 518,
{ 292: } 519,
{ 293: } 520,
{ 294: } 521,
{ 295: } 522,
{ 296: } 522
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

