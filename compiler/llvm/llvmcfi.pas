{
    Copyright (c) 2019 by Jonas Maebe, member of the Free Pascal Compiler
    development team

    LLVM CFI wrapper: use native CFI instance for pure assembler routines,
    and dummy one for LLVM (the LLVM code generator will take care of CFI)

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
unit llvmcfi;

{$i fpcdefs.inc}

  interface

    uses
      aasmbase,
      aasmdata,
      cgbase;

    type
      tllvmcfi = class(TAsmCFI)
        constructor create; override;
        destructor destroy; override;
        procedure generate_code(list: TAsmList); override;
        procedure start_frame(list:TAsmList);override;
        procedure end_frame(list:TAsmList);override;
        procedure outmost_frame(list:TAsmList);override;
        procedure cfa_offset(list:TAsmList;reg:tregister;ofs:longint);override;
        procedure cfa_restore(list:TAsmList;reg:tregister);override;
        procedure cfa_def_cfa_register(list:TAsmList;reg:tregister);override;
        procedure cfa_def_cfa_offset(list:TAsmList;ofs:longint);override;
        function get_frame_start: TAsmLabel; override;
        function get_cfa_list: TAsmList; override;
       private
         fnativecfi: TAsmCFI;
      end;

  implementation

    uses
      symconst,
      procinfo;

    var
      nativecficlass: TAsmCFIClass;

    constructor tllvmcfi.create;
      begin
        inherited;
        fnativecfi:=nativecficlass.create;
      end;


    destructor tllvmcfi.destroy;
      begin
        fnativecfi.free;
        inherited destroy;
      end;


    procedure tllvmcfi.generate_code(list: TAsmList);
      begin
        fnativecfi.generate_code(list);
      end;


    procedure tllvmcfi.start_frame(list: TAsmList);
      begin
        if po_assembler in current_procinfo.procdef.procoptions then
          fnativecfi.start_frame(list);
      end;


    procedure tllvmcfi.end_frame(list: TAsmList);
      begin
        if po_assembler in current_procinfo.procdef.procoptions then
          fnativecfi.end_frame(list);
      end;


    procedure tllvmcfi.outmost_frame(list: TAsmList);
      begin
        if po_assembler in current_procinfo.procdef.procoptions then
          fnativecfi.outmost_frame(list);
      end;


    procedure tllvmcfi.cfa_offset(list: TAsmList; reg: tregister; ofs: longint);
      begin
        if po_assembler in current_procinfo.procdef.procoptions then
          fnativecfi.cfa_offset(list, reg, ofs);
      end;


    procedure tllvmcfi.cfa_restore(list: TAsmList; reg: tregister);
      begin
        if po_assembler in current_procinfo.procdef.procoptions then
          fnativecfi.cfa_restore(list, reg);
      end;


    procedure tllvmcfi.cfa_def_cfa_register(list: TAsmList; reg: tregister);
      begin
        if po_assembler in current_procinfo.procdef.procoptions then
          fnativecfi.cfa_def_cfa_register(list, reg);
      end;


    procedure tllvmcfi.cfa_def_cfa_offset(list: TAsmList; ofs: longint);
      begin
        if po_assembler in current_procinfo.procdef.procoptions then
          fnativecfi.cfa_def_cfa_offset(list, ofs);
      end;


    function tllvmcfi.get_frame_start: TAsmLabel;
      begin
        result:=fnativecfi.get_frame_start;
      end;


    function tllvmcfi.get_cfa_list: TAsmList;
      begin
        result:=fnativecfi.get_cfa_list;
      end;


begin
  nativecficlass:=CAsmCFI;
  CAsmCFI:=tllvmcfi;
end.

