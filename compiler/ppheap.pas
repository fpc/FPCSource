{
    Copyright (c) 1998-2002 by Pierre Muller

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

    procedure ppheap_register_file(name : string;index : longint);


implementation

    uses
       cutils,globtype,globals,fmodule;

{*****************************************************************************
                            Filename registration
*****************************************************************************}

    const
      MaxFiles = 1024;
      MaxNameLength = 39;

    type
      theapfileinfo = record
        name : string[MaxNameLength];
        index : longint;
      end;

      tfileinfoarray = array [1..MaxFiles] of theapfileinfo;

    var
      fileinfoarray : tfileinfoarray;
      last_index : longint;


    procedure ppheap_register_file(name : string;index : longint);
      begin
        inc(last_index);
        if last_index <= MaxFiles then
          begin
            fileinfoarray[last_index].name:=copy(name,1,MaxNameLength);
            fileinfoarray[last_index].index:=index;
          end
        else
          writeln(stderr,'file',name,' has index ',index);
      end;


    function getfilename(index : longint) : string;
      var
        i : longint;
      begin
        for i:=1 to last_index do
          begin
            if fileinfoarray[i].index=index then
              begin
                getfilename:=fileinfoarray[i].name;
                exit;
              end;
          end;
        getfilename:=tostr(index);
      end;


{*****************************************************************************
                              Heaptrc callbacks
*****************************************************************************}

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
           line:=current_filepos.line;
           col:=current_filepos.column;
           if assigned(current_module) then
            fileindex:=current_module.unit_index*100000+current_filepos.fileindex
           else
            fileindex:=current_filepos.fileindex;
         end;
      end;


    procedure show_extra_info(var t : text;p : pointer);
      begin
        with pextra_info(p)^ do
         begin
           writeln(t,'Memory allocated at '+getfilename(fileindex)+'('+tostr(line)+','+tostr(col)+') ');
         end;
      end;


  const
     pp_heap_inited : boolean = false;

  procedure pp_heap_init;
    begin
       if not pp_heap_inited then
         begin
{$ifdef extheaptrc}
            keepreleased:=true;
{$endif extheaptrc}
            SetHeapTraceOutput('heap.log');
            SetHeapExtraInfo(sizeof(textra_info),
                             @set_extra_info,
                             @show_extra_info);
         end;
       pp_heap_inited:=true;
    end;


begin
  pp_heap_init;
end.
