{
    $Id$
    Copyright (c) 1996-98 by Florian Klaempfl

    This unit exports some help routines for the code generation

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
unit hcodegen;

  interface

     uses
        cobjects,systems,globals,tree,symtable,types,strings,aasm
{$ifdef i386}
       ,i386
{$endif}
{$ifdef m68k}
       ,m68k
{$endif}
       ;

    const
       { set, if the procedure uses asm }
       pi_uses_asm = $1;
       { set, if the procedure is exported by an unit }
       pi_is_global = $2;
       { set, if the procedure does a call }
       { this is for the optimizer         }
       pi_do_call = $4;
       { if the procedure is an operator   }
       pi_operator = $8;
       { set, if the procedure is an external C function }
       pi_C_import = $10;

    type
       pprocinfo = ^tprocinfo;

       tprocinfo = record
          { pointer to parent in nested procedures }
          parent : pprocinfo;
          { current class, if we are in a method }
          _class : pobjectdef;
          { return type }
          retdef : pdef;
          { frame pointer offset }
          framepointer_offset : longint;
          { self pointer offset }
          ESI_offset : longint;
          { result value offset }
          retoffset : longint;

          { firsttemp position }
          firsttemp : longint;

          funcret_is_valid : boolean;

          { parameter offset }
          call_offset : longint;

          { some collected informations about the procedure }
          { see pi_xxxx above                               }
          flags : longint;

          { register used as frame pointer }
          framepointer : tregister;

{$ifdef GDB}
          { true, if the procedure is exported by an unit }
          globalsymbol : boolean;
{$endif * GDB *}

          { true, if the procedure should be exported (only OS/2) }
          exported : boolean;

          { code for the current procedure }
          aktproccode,aktentrycode,aktexitcode : paasmoutput;
       end;

    var
       { info about the current sub routine }
       procinfo : tprocinfo;

       { Die Nummer der Label die bei BREAK bzw CONTINUE }
       { angesprungen werden sollen }
       aktbreaklabel,aktcontinuelabel : plabel;

       { truelabel wird angesprungen, wenn ein Ausdruck true ist, falselabel }
       { entsprechend                                                        }
       truelabel,falselabel : plabel;

       { Nr des Labels welches zum Verlassen eines Unterprogramm }
       { angesprungen wird                                       }
       aktexitlabel : plabel;

       { also an exit label, only used we need to clear only the }
       { stack                                                   }
       aktexit2label : plabel;

       { only used in constructor for fail or if getmem fails }
       quickexitlabel : plabel;

       { this asm list contains the debug info }
       {debuginfos : paasmoutput;  debuglist is enough }

       { Boolean, wenn eine loadn kein Assembler erzeugt hat }
       simple_loadn : boolean;

       { enth„lt die gesch„tzte Durchlaufanzahl*100 fr den }
       { momentan bearbeiteten Baum                         }
       t_times : longint;

       { true, if an error while code generation occurs }
       codegenerror : boolean;

    { some support routines for the case instruction }

    { counts the labels }
    function case_count_labels(root : pcaserecord) : longint;

    { searches the highest label }
    function case_get_max(root : pcaserecord) : longint;

    { searches the lowest label }
    function case_get_min(root : pcaserecord) : longint;

    { concates the ASCII string to the const segment }
    procedure generate_ascii(hs : string);

    { inserts the ASCII string to the const segment }
    procedure generate_ascii_insert(hs : string);

    procedure generate_interrupt_stackframe_entry;
    procedure generate_interrupt_stackframe_exit;

  implementation

{$ifdef i386}
    procedure generate_interrupt_stackframe_entry;

      begin
         { save the registers of an interrupt procedure }
         procinfo.aktentrycode^.insert(new(pai386,op_reg(A_PUSH,S_L,R_EAX)));
         procinfo.aktentrycode^.insert(new(pai386,op_reg(A_PUSH,S_L,R_EBX)));
         procinfo.aktentrycode^.insert(new(pai386,op_reg(A_PUSH,S_L,R_ECX)));
         procinfo.aktentrycode^.insert(new(pai386,op_reg(A_PUSH,S_L,R_EDX)));
         procinfo.aktentrycode^.insert(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
         procinfo.aktentrycode^.insert(new(pai386,op_reg(A_PUSH,S_L,R_EDI)));

         { .... also the segment registers }
         procinfo.aktentrycode^.insert(new(pai386,op_reg(A_PUSH,S_W,R_DS)));
         procinfo.aktentrycode^.insert(new(pai386,op_reg(A_PUSH,S_W,R_ES)));
         procinfo.aktentrycode^.insert(new(pai386,op_reg(A_PUSH,S_W,R_FS)));
         procinfo.aktentrycode^.insert(new(pai386,op_reg(A_PUSH,S_W,R_GS)));
      end;

    procedure generate_interrupt_stackframe_exit;

      begin
         { restore the registers of an interrupt procedure }
         { this was all with entrycode instead of exitcode !!}
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_L,R_EAX)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_L,R_EBX)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_L,R_ECX)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_L,R_EDX)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_L,R_ESI)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)));

         { .... also the segment registers }
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_W,R_DS)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_W,R_ES)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_W,R_FS)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_W,R_GS)));

        { this restores the flags }
         procinfo.aktexitcode^.concat(new(pai386,op_none(A_IRET,S_NO)));
      end;
{$endif}
{$ifdef m68k}
    procedure generate_interrupt_stackframe_entry;
      begin
         { save the registers of an interrupt procedure }

         { .... also the segment registers }
      end;

    procedure generate_interrupt_stackframe_exit;

      begin
         { restore the registers of an interrupt procedure }
      end;
{$endif}

    procedure generate_ascii(hs : string);

      begin
         while length(hs)>32 do
           begin
              datasegment^.concat(new(pai_string,init(copy(hs,1,32))));
              delete(hs,1,32);
           end;
         datasegment^.concat(new(pai_string,init(hs)))
      end;

    procedure generate_ascii_insert(hs : string);

      begin
         while length(hs)>32 do
           begin
              datasegment^.insert(new(pai_string,init(copy(hs,length(hs)-32+1,length(hs)))));
              delete(hs,length(hs)-32+1,length(hs));
           end;
         datasegment^.insert(new(pai_string,init(hs)));
      end;

    function case_count_labels(root : pcaserecord) : longint;

      var
         _l : longint;

      procedure count(p : pcaserecord);

        begin
           inc(_l);
           if assigned(p^.less) then
             count(p^.less);
           if assigned(p^.greater) then
             count(p^.greater);
        end;

      begin
         _l:=0;
         count(root);
         case_count_labels:=_l;
      end;

    function case_get_max(root : pcaserecord) : longint;

      var
         hp : pcaserecord;

      begin
         hp:=root;
         while assigned(hp^.greater) do
           hp:=hp^.greater;
         case_get_max:=hp^._high;
      end;

    function case_get_min(root : pcaserecord) : longint;

      var
         hp : pcaserecord;

      begin
         hp:=root;
         while assigned(hp^.less) do
           hp:=hp^.less;
         case_get_min:=hp^._low;
      end;

end.

{
  $Log$
  Revision 1.1  1998-03-25 11:18:13  root
  Initial revision

  Revision 1.6  1998/03/10 16:27:38  pierre
    * better line info in stabs debug
    * symtabletype and lexlevel separated into two fields of tsymtable
    + ifdef MAKELIB for direct library output, not complete
    + ifdef CHAINPROCSYMS for overloaded seach across units, not fully
      working
    + ifdef TESTFUNCRET for setting func result in underfunction, not
      working

  Revision 1.5  1998/03/10 01:17:19  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.4  1998/03/02 01:48:37  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.3  1998/02/13 10:35:03  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.2  1998/01/16 18:03:15  florian
    * small bug fixes, some stuff of delphi styled constructores added

  Revision 1.1.1.1  1997/11/27 08:32:56  michael
  FPC Compiler CVS start

  Pre-CVS log:

  CEC   Carl-Eric Codere
  FK    Florian Klaempfl
  PM    Pierre Muller
  +     feature added
  -     removed
  *     bug fixed or changed

  History:
       5th september 1997:
         + added support for MC68000 (CEC)
      22th september 1997:
         + added tprocinfo member parent (FK)
}
