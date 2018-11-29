{
    Copyright (c) 2016 by Jonas Maebe

    Information about the current procedure that is being compiled

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
unit llvmpi;

{$i fpcdefs.inc}

interface

    uses
      cclasses,
      aasmbase,
      procinfo,
      cpupi;

    type
      tllvmprocinfo = class(tcpuprocinfo)
       private
        fexceptlabelstack: tfplist;
       public
        constructor create(aparent: tprocinfo); override;
        destructor destroy; override;
        procedure set_first_temp_offset; override;
        procedure pushexceptlabel(lab: TAsmLabel);
        procedure popexceptlabel(lab: TAsmLabel);
        function CurrExceptLabel: TAsmLabel; inline;
      end;

implementation

    uses
      globtype,verbose,systems,
      symtable;


    constructor tllvmprocinfo.create(aparent: tprocinfo);
      begin
        inherited;
        fexceptlabelstack:=tfplist.create;
      end;

    destructor tllvmprocinfo.destroy;
      begin
        if fexceptlabelstack.Count<>0 then
          Internalerror(2016121301);
        fexceptlabelstack.free;
        inherited;
      end;


    procedure tllvmprocinfo.set_first_temp_offset;
      begin
        inherited;
        if not(target_info.system in systems_windows) and
           (([pi_uses_exceptions,pi_needs_implicit_finally]*flags)<>[]) then
          procdef.personality:=search_system_proc('_FPC_EXCEPTION_PERSONALITY_DO_NOTHING_V0');
      end;


    procedure tllvmprocinfo.pushexceptlabel(lab: TAsmLabel);
      begin
        fexceptlabelstack.add(lab);
      end;


    procedure tllvmprocinfo.popexceptlabel(lab: TAsmLabel);
      begin
        if CurrExceptLabel<>lab then
          internalerror(2016121302);
        fexceptlabelstack.count:=fexceptlabelstack.count-1;
      end;


    function tllvmprocinfo.CurrExceptLabel: TAsmLabel; inline;
      begin
        result:=TAsmLabel(fexceptlabelstack.last);
        if not assigned(result) then
          internalerror(2016121703);
      end;


begin
  if not assigned(cprocinfo) then
    begin
      writeln('Internalerror 2018052005');
      halt(1);
    end;
  cprocinfo:=tllvmprocinfo;
end.

