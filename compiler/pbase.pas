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

    type
       tblock_type = (bt_general,bt_type,bt_const);

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

    { just for an accurate position of the end of a procedure (PM) }
    var
       last_endtoken_filepos: tfileposinfo;

  implementation

    uses

       files,scanner,symtable,systems,verbose;

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
             if token=_END then
{$ifdef UseTokenInfo}
                last_endtoken_filepos:=tokenpos;
{$else UseTokenInfo}
                get_cur_file_pos(last_endtoken_filepos);
{$endif UseTokenInfo}
             token:=yylex;
           end;
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
{$ifndef UseTokenInfo}
           sc^.insert(pattern);
{$else UseTokenInfo}
           sc^.insert_with_tokeninfo(pattern,
             tokenpos);
{$endif UseTokenInfo}
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
{$ifdef UseTokenInfo}
         filepos : tfileposinfo;
         ss : pvarsym;
{$endif UseTokenInfo}


      begin
{$ifdef UseTokenInfo}
        s:=sc^.get_with_tokeninfo(filepos);
{$else UseTokenInfo}
        s:=sc^.get;
{$endif UseTokenInfo}
         while s<>'' do
           begin
{$ifndef UseTokenInfo}
              st^.insert(new(pvarsym,init(s,def)));
{$else UseTokenInfo}
              ss:=new(pvarsym,init(s,def));
              ss^.line_no:=filepos.line;
              st^.insert(ss);
{$endif UseTokenInfo}
              { static data fields are inserted in the globalsymtable }
              if (st^.symtabletype=objectsymtable) and
                 ((current_object_option and sp_static)<>0) then
                begin
                   s:=lowercase(st^.name^)+'_'+s;
                   st^.defowner^.owner^.insert(new(pvarsym,init(s,def)));
                end;
{$ifdef UseTokenInfo}
              s:=sc^.get_with_tokeninfo(filepos);
{$else UseTokenInfo}
              s:=sc^.get;
{$endif UseTokenInfo}
           end;
         dispose(sc,done);
      end;

end.

{
  $Log$
  Revision 1.6  1998-05-12 10:47:00  peter
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
