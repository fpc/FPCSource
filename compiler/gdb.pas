{
    $Id$
    Copyright (c) 1996-98 by Florian Klaempfl

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
{$ifdef i386}
       i386,
{$endif i386}
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

       pai_stab_function_name = ^tai_stab_function_name;

       tai_stab_function_name = object(tai)
          str : pchar;
          constructor init(_str : pchar);
          destructor done; virtual;
       end;

const          DBX_counter : plongint = nil;
               do_count_dbx : boolean = false;
           { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi",
            "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
           { this is the register order for GDB }

{$ifdef i386}
       {tregister = (R_NO,R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI,
                    R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI,
                    R_AL,R_CL,R_DL,R_BL,R_AH,R_CH,R_BH,R_DH,
                     for an easier assembler generation
                    R_DEFAULT_SEG,R_CS,R_DS,R_ES,R_FS,R_GS,R_SS,
                    R_ST,R_ST0,R_ST1,R_ST2,R_ST3,R_ST4,R_ST5,R_ST6,R_ST7); }
           GDB_i386index : array[tregister] of shortint =
           (-1,0,1,2,3,4,5,6,7,0,1,2,3,4,5,7,0,1,2,3,0,1,2,3,
           -1,10,12,13,14,15,11,
           -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
           { I think, GDB doesn't know MMX (FK) }
           -1,-1,-1,-1,-1,-1,-1,-1);
{$endif i386}

  implementation

{$IfDef DBX}

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
              if (*str == '(')
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
     if dbx_counter = nil then
     else
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

{$EndIf DBX}

    constructor tai_stabs.init(_str : pchar);

      begin
         inherited init;
         typ:=ait_stabs;
         str:=_str;
{$IfDef DBX}
         if do_count_dbx then
           begin
              count_dbx(str);
              do_count_dbx := false;
           end;
{$EndIf DBX}
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
  Revision 1.3  1998-09-22 17:13:45  pierre
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
