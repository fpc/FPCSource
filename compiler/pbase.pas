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
       cobjects,globals,scanner,symtable,systems,verbose;

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

    type
       tblock_type = (bt_general,bt_type,bt_const);

    var
       { contains the current token to be processes }
       token : ttoken;
{$ifdef UseTokenInfo}
       tokeninfo : ptokeninfo;
{$endif UseTokenInfo}

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
       { type of currently parsed block }
       { isn't full implemented (FK)    }
       block_type : tblock_type;

       { true, if we should ignore an equal in const x : 1..2=2 }
       ignore_equal : boolean;

    { consumes token i, if the current token is unequal i }
    { a syntax error is written                           }
    procedure consume(i : ttoken);

    { consumes all tokens til atoken (for error recovering }
    procedure consume_all_until(atoken : ttoken);

    { consumes tokens while they are semicolons }
    procedure emptystats;

    { reads a list of identifiers into a string container }
    function idlist : pstringcontainer;

    { inserts the symbols of sc in st with def as definition }
    { sc is disposed                                         }
    procedure insert_syms(st : psymtable;sc : pstringcontainer;def : pdef);

  implementation


    { consumes token i, if the current token is unequal i }
    { a syntax error is written                           }
    procedure consume(i : ttoken);

      { generates a syntax error message }
      procedure syntaxerror(const s : string);

        begin
           Message2(scan_f_syn_expected,tostr(get_current_col),s);
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

      var
         j : integer;

      begin
{$ifndef UseTokenInfo}
         if token<>i then
           begin
              if i<_AND then
                syntaxerror(tokens[i])
              else
                begin

                   { um die Programmgr”áe klein zu halten, }
                   { wird fr ein Schlsselwort-Token der  }
                   { "Text" in der Schlsselworttabelle    }
                   { des Scanners nachgeschaut             }

                   for j:=1 to anz_keywords do
                     if keyword_token[j]=i then
                       syntaxerror(keyword[j])
                end;
           end
         else
           token:=yylex;
{$else UseTokenInfo}
         if token<>i then
           begin
              if i<_AND then
                syntaxerror(tokens[i])
              else
                begin

                   { um die Programmgr”áe klein zu halten, }
                   { wird fr ein Schlsselwort-Token der  }
                   { "Text" in der Schlsselworttabelle    }
                   { des Scanners nachgeschaut             }

                   for j:=1 to anz_keywords do
                     if keyword_token[j]=i then
                       syntaxerror(keyword[j])
                end;
           end
         else
           begin
             if assigned(tokeninfo) then
               dispose(tokeninfo);
             tokeninfo:=yylex;
             token:=tokeninfo^.token;
           end;
{$endif UseTokenInfo}
      end;

    procedure consume_all_until(atoken : ttoken);

      begin
{$ifndef UseTokenInfo}
         while (token<>atoken) and (token<>_EOF) do
           consume(token);
         { this will create an error if the token is _EOF }
         if token<>atoken then
           consume(atoken);
{$else UseTokenInfo}
         while (token<>atoken) and (token<>_EOF) do
           consume(token);
         { this will create an error if the token is _EOF }
         if token<>atoken then
           consume(atoken);
{$endif UseTokenInfo}
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
           sc^.insert(pattern);
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

      begin
         s:=sc^.get;
         while s<>'' do
           begin
              st^.insert(new(pvarsym,init(s,def)));
              { static data fields are inserted in the globalsymtable }
              if (st^.symtabletype=objectsymtable) and
                 ((current_object_option and sp_static)<>0) then
                begin
                   s:=lowercase(st^.name^)+'_'+s;
                   st^.defowner^.owner^.insert(new(pvarsym,init(s,def)));
                end;
              s:=sc^.get;
           end;
         dispose(sc,done);
      end;

end.

{
  $Log$
  Revision 1.3  1998-04-29 10:33:57  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.2  1998/04/07 22:45:05  florian
    * bug0092, bug0115 and bug0121 fixed
    + packed object/class/array

  Revision 1.1.1.1  1998/03/25 11:18:14  root
  * Restored version

  Revision 1.9  1998/03/10 01:17:23  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.8  1998/03/06 00:52:40  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.7  1998/03/02 01:48:59  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.6  1998/02/16 12:51:38  michael
  + Implemented linker object

  Revision 1.5  1998/02/13 10:35:22  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.4  1998/02/12 11:50:24  daniel
  Yes! Finally! After three retries, my patch!

  Changes:

  Complete rewrite of psub.pas.
  Added support for DLL's.
  Compiler requires less memory.
  Platform units for each platform.

  Revision 1.3  1998/01/13 17:13:08  michael
  * File time handling and file searching is now done in an OS-independent way,
    using the new file treating functions in globals.pas.

  Revision 1.2  1998/01/09 09:09:58  michael
  + Initial implementation, second try

}
