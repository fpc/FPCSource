{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    This units contains special support for the GDB

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
unit gdb;

  interface

    uses
      globtype,cpubase,
      strings,cobjects,globals,aasm;

    {stab constants }
Const
    N_GSYM = $20;
    N_STSYM = 38; {initialized const }
    N_LCSYM = 40; {non initialized variable}
    N_Function = $24; {function or const }
    N_TextLine = $44;
    N_DataLine = $46;
    N_BssLine = $48;
    N_RSYM = $40; { register variable }
    N_LSYM = $80;
    N_PSYM = 160;
    N_SourceFile = $64;
    N_IncludeFile = $84;
    N_BINCL = $82;
    N_EINCL = $A2;
    N_EXCL  = $C2;

    type
       pai_stabs = ^tai_stabs;

       tai_stabs = object(tai)
          str : pchar;
          constructor init(_str : pchar);
          destructor done; virtual;
       end;

       pai_stabn = ^tai_stabn;

       tai_stabn = object(tai)
          str : pchar;
          constructor init(_str : pchar);
          destructor done; virtual;
       end;

       { insert a cut to split into several smaller files }
       pai_force_line = ^tai_force_line;
       tai_force_line = object(tai)
          constructor init;
       end;

       pai_stab_function_name = ^tai_stab_function_name;

       tai_stab_function_name = object(tai)
          str : pchar;
          constructor init(_str : pchar);
          destructor done; virtual;
       end;

    const
       DBX_counter : plongint = nil;
       do_count_dbx : boolean = false;

{$ifdef i386}
           { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi",
             "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
           { this is the register order for GDB }
        GDB_i386index : array[tregister] of shortint =(-1,
          0,1,2,3,4,5,6,7,0,1,2,3,4,5,7,0,1,2,3,0,1,2,3,
          -1,10,12,13,14,15,11,
          -1,-1,-1,-1,-1,-1,-1,-1,-1,
          -1,-1,-1,-1,-1,-1,
          -1,-1,-1,-1,
          -1,-1,-1,-1,-1,
          { I think, GDB doesn't know MMX (FK) }
          -1,-1,-1,-1,-1,-1,-1,-1,
          -1,-1,-1,-1,-1,-1,-1,-1
        );
{$endif i386}

  implementation

  uses
    verbose;
{ to use N_EXCL we have to count the character in the stabs for
N_BINCL to N_EINCL
  Code comes from stabs.c for ld
      if (type == N_BINCL)
    (
      bfd_vma val;
      int nest;
      bfd_byte *incl_sym;
      struct stab_link_includes_entry *incl_entry;
      struct stab_link_includes_totals *t;
      struct stab_excl_list *ne;

      val = 0;
      nest = 0;
      for (incl_sym = sym + STABSIZE;
           incl_sym < symend;
           incl_sym += STABSIZE)
        (
          int incl_type;

          incl_type = incl_sym[TYPEOFF];
          if (incl_type == 0)
        break;
          else if (incl_type == N_EINCL)
        (
          if (nest == 0)
            break;
          --nest;
        )
          else if (incl_type == N_BINCL)
        ++nest;
          else if (nest == 0)
        (
          const char *str;

          str = ((char *) stabstrbuf
             + stroff
             + bfd_get_32 (abfd, incl_sym + STRDXOFF));
          for (; *str != '\0'; str++)
            (
              val += *str;
              if *str == '('
            (
               Skip the file number.
              ++str;
              while (isdigit ((unsigned char) *str))
                ++str;
              --str;
            )
            )
        )
        ) }


   procedure count_dbx(st : pchar);
     var i : longint;
         do_count : boolean;
     begin
     do_count := false;
     if assigned(dbx_counter) then
       begin
{$IfDef ExtDebug }
        Comment(V_Info,'Counting '+st);
        Comment(V_Info,'count =  '+tostr(dbx_counter^));
        Comment(V_Info,'addr = '+tostr(longint(dbx_counter)));
{$EndIf ExtDebug }
          for i:=0 to strlen(st) do
            begin
               if st[i] = '"' then
                 if do_count then exit
                 else do_count := true
               else
               if do_count then
                 begin
                   dbx_counter^ := dbx_counter^+byte(st[i]);
                   { skip file number }
                   if st[i] = '(' then
                     begin
                        inc(i);
                        while st[i] in ['0'..'9'] do inc(i);
                        dec(i);
                     end;
                 end;
            end;
       end;
     end;


    constructor tai_stabs.init(_str : pchar);

      begin
         inherited init;
         typ:=ait_stabs;
         str:=_str;
         if do_count_dbx then
           begin
              count_dbx(str);
           end;
      end;

    destructor tai_stabs.done;

      begin
         strdispose(str);
         inherited done;
      end;

    constructor tai_stabn.init(_str : pchar);

      begin
         inherited init;
         typ:=ait_stabn;
         str:=_str;
      end;

    destructor tai_stabn.done;

      begin
         strdispose(str);
         inherited done;
      end;

    constructor tai_force_line.init;

      begin
         inherited init;
         typ:=ait_force_line;
      end;

    constructor tai_stab_function_name.init(_str : pchar);

      begin
         inherited init;
         typ:=ait_stab_function_name;
         str:=_str;
      end;

    destructor tai_stab_function_name.done;

      begin
         strdispose(str);
         inherited done;
      end;
end.

{
  $Log$
  Revision 1.14  2000-01-07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.13  1999/11/09 23:51:25  pierre
   * some DBX work

  Revision 1.12  1999/08/04 00:23:01  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.11  1999/05/27 19:44:27  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.10  1999/05/12 00:19:48  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.9  1999/05/01 13:24:20  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.8  1999/03/17 10:52:38  peter
    * fixed comment in directive

  Revision 1.7  1999/03/02 02:56:12  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.6  1999/01/08 12:39:23  florian
    Changes of Alexander Stohr integrated:
      + added KNI opcodes
      + added KNI registers
      + added 3DNow! opcodes
      + added 64 bit and 128 bit register flags
      * translated a few comments into english

  Revision 1.5  1998/12/11 00:03:16  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.4  1998/11/12 11:19:45  pierre
   * fix for first line of function break

  Revision 1.3  1998/09/22 17:13:45  pierre
    + browsing updated and developed
      records and objects fields are also stored

  Revision 1.2  1998/07/10 08:31:38  pierre
    *  Just the N_FNAME to N_FUN substitution for stabs of functions
      thanks again Daniel !!

  Revision 1.1.1.1  1998/03/25 11:18:13  root
  * Restored version

  Revision 1.5  1998/03/10 01:17:18  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.4  1998/03/02 01:48:33  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.3  1998/02/13 10:35:01  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.2  1997/11/28 18:14:32  pierre
   working version with several bug fixes

  Revision 1.1.1.1  1997/11/27 08:32:56  michael
  FPC Compiler CVS start

}
