{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
       cobjects,tokens,globals,symtable;

    const
       { true, if we are after an assignement }
       afterassignment : boolean = false;

       { sspecial for handling procedure vars }
       getprocvar : boolean = false;
       getprocvardef : pprocvardef = nil;


    var
       { size of data segment, set by proc_unit or proc_program }
       datasize : longint;

       { for operators }
       optoken : ttoken;
       opsym : pvarsym;

       { symtable were unit references are stored }
       refsymtable : psymtable;

       { true, if only routine headers should be parsed }
       parse_only : boolean;

       { true, if we should ignore an equal in const x : 1..2=2 }
       ignore_equal : boolean;


    function tokenstring(i : ttoken):string;

    { consumes token i, if the current token is unequal i }
    { a syntax error is written                           }
    procedure consume(i : ttoken);

    {Tries to consume the token i, and returns true if it was consumed:
     if token=i.}
    function try_to_consume(i:Ttoken):boolean;

    { consumes all tokens til atoken (for error recovering }
    procedure consume_all_until(atoken : ttoken);

    { consumes tokens while they are semicolons }
    procedure emptystats;

    { reads a list of identifiers into a string container }
    function idlist : pstringcontainer;

    { just for an accurate position of the end of a procedure (PM) }
    var
       last_endtoken_filepos: tfileposinfo;


  implementation

    uses
       files,scanner,systems,verbose;

    function tokenstring(i : ttoken):string;
      begin
        tokenstring:=tokeninfo^[i].str;
      end;

    { consumes token i, write error if token is different }
    procedure consume(i : ttoken);
      begin
        if (token<>i) and (idtoken<>i) then
          if token=_id then
            Message2(scan_f_syn_expected,tokeninfo^[i].str,'identifier '+pattern)
          else
            Message2(scan_f_syn_expected,tokeninfo^[i].str,tokeninfo^[token].str)
        else
          begin
            if token=_END then
              last_endtoken_filepos:=tokenpos;
            current_scanner^.readtoken;
          end;
      end;

    function try_to_consume(i:Ttoken):boolean;


    begin
        try_to_consume:=false;
        if (token=i) or (idtoken=i) then
            begin
                try_to_consume:=true;
                if token=_END then
                    last_endtoken_filepos:=tokenpos;
                current_scanner^.readtoken;
            end;
    end;

    procedure consume_all_until(atoken : ttoken);
      begin
         while (token<>atoken) and (idtoken<>atoken) do
          begin
            Consume(token);
            if token=_EOF then
             begin
               Consume(atoken);
               Message(scan_f_end_of_file);
               exit;
             end;
          end;
      end;


    procedure emptystats;
      begin
         repeat
         until not try_to_consume(_SEMICOLON);
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
           consume(_id);
           if token=_COMMA then consume(_COMMA)
             else break
         until false;
         idlist:=sc;
      end;

end.

{
  $Log$
  Revision 1.28  2000-01-07 01:14:28  peter
    * updated copyright to 2000

  Revision 1.27  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.26  1999/10/01 08:02:46  peter
    * forward type declaration rewritten

  Revision 1.25  1999/09/02 18:47:44  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

  Revision 1.24  1999/08/04 13:02:50  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.23  1999/07/27 23:42:10  peter
    * indirect type referencing is now allowed

  Revision 1.22  1999/07/26 09:42:10  florian
    * bugs 494-496 fixed

  Revision 1.21  1999/04/28 06:02:05  florian
    * changes of Bruessel:
       + message handler can now take an explicit self
       * typinfo fixed: sometimes the type names weren't written
       * the type checking for pointer comparisations and subtraction
         and are now more strict (was also buggy)
       * small bug fix to link.pas to support compiling on another
         drive
       * probable bug in popt386 fixed: call/jmp => push/jmp
         transformation didn't count correctly the jmp references
       + threadvar support
       * warning if ln/sqrt gets an invalid constant argument

  Revision 1.20  1999/04/14 18:41:24  daniel
  * Better use of routines in pbase and symtable. 4k code removed.

  Revision 1.19  1999/04/08 20:59:42  florian
    * fixed problem with default properties which are a class
    * case bug (from the mailing list with -O2) fixed, the
      distance of the case labels can be greater than the positive
      range of a longint => it is now a dword for fpc

  Revision 1.18  1998/12/11 00:03:29  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.17  1998/09/26 17:45:31  peter
    + idtoken and only one token table

  Revision 1.16  1998/09/23 15:39:08  pierre
    * browser bugfixes
      was adding a reference when looking for the symbol
      if -bSYM_NAME was used

  Revision 1.15  1998/09/07 23:10:21  florian
    * a lot of stuff fixed regarding rtti and publishing of properties,
      basics should now work

  Revision 1.14  1998/07/14 21:46:49  peter
    * updated messages file

  Revision 1.13  1998/07/14 14:46:52  peter
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

}
