{
    $Id$
    Copyright (c) 1998-2000 by Pierre Muller

    Simple unit to add source line and column to each
    memory allocation made with heaptrc unit

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
unit ppheap;

{$i fpcdefs.inc}

interface

    uses heaptrc;

    { call this function before any memory allocation
      in a unit initialization code (PM) }

    procedure pp_heap_init;

implementation

    uses
       globtype,globals,fmodule;

    type
      pextra_info = ^textra_info;
      textra_info = record
        line,
        col,
        fileindex : longint;
      end;

    procedure set_extra_info(p : pointer);
      begin
        with pextra_info(p)^ do
         begin
           line:=aktfilepos.line;
           col:=aktfilepos.column;
           if assigned(current_module) then
            fileindex:=current_module.unit_index*100000+aktfilepos.fileindex
           else
            fileindex:=aktfilepos.fileindex;
         end;
      end;


    procedure show_extra_info(var t : text;p : pointer);
      begin
        with pextra_info(p)^ do
         begin
           writeln(t,'fileinfo: (',line,',',col,') ',fileindex);
         end;
      end;

  const
     pp_heap_inited : boolean = false;

  procedure pp_heap_init;
    begin
       if not pp_heap_inited then
         begin
            keepreleased:=true;
            SetHeapTraceOutput('heap.log');
            SetHeapExtraInfo(sizeof(textra_info),
                             {$ifdef FPCPROCVAR}@{$endif}set_extra_info,
                             {$ifdef FPCPROCVAR}@{$endif}show_extra_info);
         end;
       pp_heap_inited:=true;
    end;


begin
  pp_heap_init;
end.
{
  $Log$
  Revision 1.9  2002-05-16 19:46:43  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.7  2001/04/13 01:22:13  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.5  2001/03/13 18:43:17  peter
    * made memdebug and heaptrc compilable again

  Revision 1.4  2000/10/14 21:52:56  peter
    * fixed memory leaks

  Revision 1.3  2000/09/24 15:06:24  peter
    * use defines.inc

  Revision 1.2  2000/07/13 11:32:45  michael
  + removed logs

}
