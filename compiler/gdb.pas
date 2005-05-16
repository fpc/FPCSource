{
    $Id: gdb.pas,v 1.21 2005/02/14 17:13:06 peter Exp $
    Copyright (c) 1998-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

interface

uses
  strings,
  globtype,
  aasmtai;

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
       tai_force_line = class(tailineinfo)
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


  implementation

uses fmodule;

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

if current_module.modulename^='NCNV' then
  current_module:=current_module;

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
  $Log: gdb.pas,v $
  Revision 1.21  2005/02/14 17:13:06  peter
    * truncate log

}
