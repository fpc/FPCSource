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
      cpupi,
      aasmllvm;

    type
      tllvmprocinfo = class(tcpuprocinfo)
       private
        fexceptlabelstack: tfplist;
        flandingpadstack: tfplist;
       public
        constructor create(aparent: tprocinfo); override;
        destructor destroy; override;
        procedure pushexceptlabel(lab: TAsmLabel);
        { returns true if there no more landing pads on the stack }
        function popexceptlabel(lab: TAsmLabel): boolean;
        function CurrExceptLabel: TAsmLabel;
        procedure pushlandingpad(pad: taillvm);
        procedure poppad;
        function currlandingpad: taillvm;
      end;

implementation

    uses
      globtype,globals,verbose,systems,
      symtable;


    constructor tllvmprocinfo.create(aparent: tprocinfo);
      begin
        inherited;
        fexceptlabelstack:=tfplist.create;
        flandingpadstack:=tfplist.create;
      end;

    destructor tllvmprocinfo.destroy;
      begin
        if fexceptlabelstack.Count<>0 then
          Internalerror(2016121301);
        fexceptlabelstack.free;
        if flandingpadstack.Count<>0 then
          internalerror(2018051901);
        flandingpadstack.free;
        inherited;
      end;


    procedure tllvmprocinfo.pushexceptlabel(lab: TAsmLabel);
      begin
        fexceptlabelstack.add(lab);
      end;


    function tllvmprocinfo.popexceptlabel(lab: TAsmLabel): boolean;
      begin
        if CurrExceptLabel<>lab then
          internalerror(2016121302);
        fexceptlabelstack.count:=fexceptlabelstack.count-1;
        result:=fexceptlabelstack.count=0;
      end;


    function tllvmprocinfo.CurrExceptLabel: TAsmLabel; inline;
      begin
        result:=TAsmLabel(fexceptlabelstack.last);
        if not assigned(result) then
          internalerror(2016121703);
      end;


    procedure tllvmprocinfo.pushlandingpad(pad: taillvm);
      begin
        flandingpadstack.add(pad);
      end;

    procedure tllvmprocinfo.poppad;
      begin
        if flandingpadstack.Count=0 then
          internalerror(2018051902);
        flandingpadstack.Count:=flandingpadstack.Count-1;
      end;


    function tllvmprocinfo.currlandingpad: taillvm;
      begin
        if flandingpadstack.Count=0 then
          internalerror(2018051903);
        result:=taillvm(flandingpadstack.last);
      end;


begin
  if not assigned(cprocinfo) then
    begin
      writeln('Internalerror 2018052005');
      halt(1);
    end;
  cprocinfo:=tllvmprocinfo;
end.

