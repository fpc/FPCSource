{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl

    Contains some helper routines for the parser

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

 ****************************************************************************
}
unit pbase;

  interface

    uses
       cobjects,globals,symtable;

    const
       { forward types should only be possible inside  }
       { a TYPE statement, this crashed the compiler   }
       { when trying to dispose local symbols          }
       typecanbeforward : boolean = false;

       { true, if we are after an assignement }
       afterassignment : boolean = false;
       { sspecial for handling procedure vars }
       getprocvar : boolean = false;
       getprocvardef : pprocvardef = nil;


    var
       { contains the current token to be processes }
       token : ttoken;

       { size of data segment, set by proc_unit or proc_program }
       datasize : longint;

       { for operators }
       optoken : ttoken;
       opsym : pvarsym;

       { symtable were unit references are stored }
       refsymtable : psymtable;

       { true, if only routine headers should be }
       { parsed                                  }
       parse_only : boolean;

       { true, if we are in a except block }
       in_except_block : boolean;

       { true, if we should ignore an equal in const x : 1..2=2 }
       ignore_equal : boolean;

    { consumes token i, if the current token is unequal i }
    { a syntax error is written                           }
    procedure consume(i : ttoken);

    function tokenstring(i : ttoken) : string;

    { consumes all tokens til atoken (for error recovering }
    procedure consume_all_until(atoken : ttoken);

    { consumes tokens while they are semicolons }
    procedure emptystats;

    { reads a list of identifiers into a string container }
    function idlist : pstringcontainer;

    { inserts the symbols of sc in st with def as definition }
    { sc is disposed                                         }
    procedure insert_syms(st : psymtable;sc : pstringcontainer;def : pdef);

    { just for an accurate position of the end of a procedure (PM) }
    var
       last_endtoken_filepos: tfileposinfo;

  implementation

    uses

       files,scanner,systems,verbose;

      { generates a syntax error message }
      procedure syntaxerror(s : string);

        begin
           Message2(scan_f_syn_expected,tostr(aktfilepos.column),s);
        end;

      { This is changed since I changed the order of token
      in cobjects.pas for operator overloading !!!! }
      { ttoken = (PLUS,MINUS,STAR,SLASH,EQUAL,GT,
                 LT,LTE,GTE,SYMDIF,STARSTAR,ASSIGNMENT,CARET,
                 LECKKLAMMER,RECKKLAMMER,
                 POINT,COMMA,LKLAMMER,RKLAMMER,COLON,SEMICOLON,
                 KLAMMERAFFE,UNEQUAL,POINTPOINT,
                 ID,REALNUMBER,_EOF,INTCONST,CSTRING,CCHAR,DOUBLEADDR,}


      const tokens : array[PLUS..DOUBLEADDR] of string[12] = (
                 '+','-','*','/','=','>','<','>=','<=','is','as','in',
                 '><','**',':=','^','<>','[',']','.',',','(',')',':',';',
                 '@','..',
                 'identifier','const real.','end of file',
                 'ord const','const string','const char','@@');

    function tokenstring(i : ttoken) : string;

      var
         j : integer;

      begin
         if i<_AND then
           tokenstring:=tokens[i]
         else
           begin
              { um die Programmgr”áe klein zu halten, }
              { wird fr ein Schlsselwort-Token der  }
              { "Text" in der Schlsselworttabelle    }
              { des Scanners nachgeschaut             }

              for j:=1 to anz_keywords do
                if keyword_token[j]=i then
                tokenstring:=keyword[j];
           end;
      end;

    { consumes token i, if the current token is unequal i }
    { a syntax error is written                           }
    procedure consume(i : ttoken);

      begin
         if token<>i then
           begin
              syntaxerror(tokenstring(i));
           end
         else
           begin
             if token=_END then
                last_endtoken_filepos:=tokenpos;
             token:=current_scanner^.yylex;
           end;
      end;

    procedure consume_all_until(atoken : ttoken);

      begin
         while (token<>atoken) and (token<>_EOF) do
           consume(token);
         { this will create an error if the token is _EOF }
         if token<>atoken then
           consume(atoken);
         { this error is fatal as we have read the whole file }
         Message(scan_f_end_of_file);
      end;

    procedure emptystats;

      begin
         while token=SEMICOLON do
           consume(SEMICOLON);
      end;

    { reads a list of identifiers into a string container }
    function idlist : pstringcontainer;

      var
        sc : pstringcontainer;

      begin
         sc:=new(pstringcontainer,init);
         repeat
           sc^.insert_with_tokeninfo(pattern,
             tokenpos);
           consume(ID);
           if token=COMMA then consume(COMMA)
             else break
         until false;
         idlist:=sc;
      end;

    { inserts the symbols of sc in st with def as definition }
    { sc is disposed                                         }
    procedure insert_syms(st : psymtable;sc : pstringcontainer;def : pdef);

      var
         s : string;
         filepos : tfileposinfo;
         ss : pvarsym;


      begin
         while not sc^.empty do
           begin
              s:=sc^.get_with_tokeninfo(filepos);
              ss:=new(pvarsym,init(s,def));
              ss^.fileinfo:=filepos;
              st^.insert(ss);
              { static data fields are inserted in the globalsymtable }
              if (st^.symtabletype=objectsymtable) and
                 ((current_object_option and sp_static)<>0) then
                begin
                   s:=lower(st^.name^)+'_'+s;
                   st^.defowner^.owner^.insert(new(pvarsym,init(s,def)));
                end;
           end;
         dispose(sc,done);
      end;

end.

{
  $Log$
  Revision 1.13  1998-07-14 14:46:52  peter
    * released NEWINPUT

  Revision 1.12  1998/07/09 23:59:59  peter
    * fixed ttypesym bug finally
    * fileinfo in the symtable and better using for unused vars

  Revision 1.11  1998/07/07 11:20:02  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.10  1998/06/05 14:37:31  pierre
    * fixes for inline for operators
    * inline procedure more correctly restricted

  Revision 1.9  1998/06/03 22:48:58  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.8  1998/05/23 01:21:18  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.7  1998/05/20 09:42:35  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.6  1998/05/12 10:47:00  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.5  1998/05/06 08:38:44  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.4  1998/04/30 15:59:41  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.3  1998/04/29 10:33:57  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.2  1998/04/07 22:45:05  florian
    * bug0092, bug0115 and bug0121 fixed
    + packed object/class/array
}
