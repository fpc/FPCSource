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

{$i defines.inc}

interface

uses
{$ifdef delphi}
  sysutils,
{$else}
  strings,
{$endif}
  globtype,cpubase,
  globals,aasm;

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
    N_tsym = 160;
    N_SourceFile = $64;
    N_IncludeFile = $84;
    N_BINCL = $82;
    N_EINCL = $A2;
    N_EXCL  = $C2;

    type
       tai_stabs = class(tai)
          str : pchar;
          constructor Create(_str : pchar);
          destructor Destroy;override;
       end;

       tai_stabn = class(tai)
          str : pchar;
          constructor Create(_str : pchar);
          destructor Destroy;override;
       end;

       { insert a cut to split into several smaller files }
       tai_force_line = class(tai)
          constructor Create;
       end;

       tai_stab_function_name = class(tai)
          str : pchar;
          constructor create(_str : pchar);
          destructor destroy;override;
       end;

    const
       DBX_counter : plongint = nil;
       do_count_dbx : boolean = false;

{$ifdef i386}
           { this is the register order for GDB }
           { this is indeed the internal order of
             registers in GDB, but as we use STABS,
             the values are converted using
             i386_stab_reg_to_regnum from i386_tdep.c PM }
           { 0 "eax",   "ecx",    "edx",   "ebx",     \
             4 "esp",   "ebp",    "esi",   "edi",        \
             8 "eip",   "eflags", "cs",    "ss",        \
             12 "ds",    "es",     "fs",    "gs",        \
             16 "st0",   "st1",    "st2",   "st3",        \
             20 "st4",   "st5",    "st6",   "st7",        \
             24 "fctrl", "fstat",  "ftag",  "fiseg",        \
             28 "fioff", "foseg",  "fooff", "fop",     \
             32 "xmm0",  "xmm1",   "xmm2",  "xmm3",        \
             36 "xmm4",  "xmm5",   "xmm6",  "xmm7",        \
             40 "mxcsr"                                \
           }
  { tregister = (R_NO,
    R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI,
    R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI,
    R_AL,R_CL,R_DL,R_BL,R_AH,R_CH,R_BH,R_DH,
    R_CS,R_DS,R_ES,R_SS,R_FS,R_GS,
    R_ST,R_ST0,R_ST1,R_ST2,R_ST3,R_ST4,R_ST5,R_ST6,R_ST7,
    R_DR0,R_DR1,R_DR2,R_DR3,R_DR6,R_DR7,
    R_CR0,R_CR2,R_CR3,R_CR4,
    R_TR3,R_TR4,R_TR5,R_TR6,R_TR7,
    R_MM0,R_MM1,R_MM2,R_MM3,R_MM4,R_MM5,R_MM6,R_MM7,
    R_XMM0,R_XMM1,R_XMM2,R_XMM3,R_XMM4,R_XMM5,R_XMM6,R_XMM7
  ); }

    { So here we need to use the stabs numbers PM }
           GDB_i386index : array[tregister] of shortint =(-1,
          0,1,2,3,4,5,6,7,
          0,1,2,3,4,5,6,7,
          0,1,2,3,0,1,2,3,
          -1,-1,-1,-1,-1,-1,
          12,12,13,14,15,16,17,18,19,
          -1,-1,-1,-1,-1,-1,
          -1,-1,-1,-1,
          -1,-1,-1,-1,-1,
          { I think, GDB doesn't know MMX (FK)
            GDB does not, but stabs does PM }
          29,30,31,32,33,34,35,36,
          21,22,23,24,25,26,27,28
        );
{$endif i386}
{$ifdef m68k}
           { "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7",
             "a0", "a1", "a2", "a3", "a4", "a5", "fp", "sp",
             "ps", "pc", "fp0", "fp1", "fp2", "fp3", "fp4" ,
             "fp5", "fp6", "fp7", "fpcontrol", "fpstatus",
             "fpiaddr","fpcode","fpflags"
           }
        { this is the register order for GDB }
        GDB_m68kindex : array[tregister] of shortint =
        (-1,                 { R_NO }
          0,1,2,3,4,5,6,7,   { R_D0..R_D7 }
          8,9,10,11,12,13,14,15,  { R_A0..R_A7 }
          -1,-1,-1,                { R_SPPUSH, R_SPPULL, R_CCR }
          18,19,20,21,22,23,24,25, { R_FP0..R_FP7    }
          -1,-1,-1,-1,-1,-1,-1,-1
        );
{$endif}

  implementation

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
{$IfDef ExtDebugDbx }
        Comment(V_Info,'Counting '+st);
        Comment(V_Info,'count =  '+tostr(dbx_counter^));
        Comment(V_Info,'addr = '+tostr(longint(dbx_counter)));
{$EndIf ExtDebugDbx }
          i:=0;
          while i<=strlen(st) do
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
               inc(i);
            end;
       end;
     end;


    constructor tai_stabs.create(_str : pchar);

      begin
         inherited create;
         typ:=ait_stabs;
         str:=_str;
         if do_count_dbx then
           begin
              count_dbx(str);
           end;
      end;

    destructor tai_stabs.destroy;

      begin
         strdispose(str);
         inherited destroy;
      end;

    constructor tai_stabn.create(_str : pchar);

      begin
         inherited create;
         typ:=ait_stabn;
         str:=_str;
      end;

    destructor tai_stabn.destroy;

      begin
         strdispose(str);
         inherited destroy;
      end;

    constructor tai_force_line.create;

      begin
         inherited create;
         typ:=ait_force_line;
      end;

    constructor tai_stab_function_name.create(_str : pchar);

      begin
         inherited create;
         typ:=ait_stab_function_name;
         str:=_str;
      end;

    destructor tai_stab_function_name.destroy;

      begin
         strdispose(str);
         inherited destroy;
      end;
end.

{
  $Log$
  Revision 1.9  2001-12-02 17:20:58  peter
    * merged register fixes

  Revision 1.8  2001/10/12 16:05:36  peter
    * more registers (merged)

  Revision 1.7  2001/04/21 12:03:11  peter
    * m68k updates merged from fixes branch

  Revision 1.6  2001/04/13 01:22:07  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.5  2000/12/25 00:07:26  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.4  2000/11/29 00:30:30  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.3  2000/09/24 15:06:16  peter
    * use defines.inc

  Revision 1.2  2000/07/13 11:32:41  michael
  + removed logs

}
