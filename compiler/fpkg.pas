{
    Copyright (c) 2013-2016 by Free Pascal Development Team

    This unit implements basic parts of the package system

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
unit fpkg;

{$i fpcdefs.inc}

interface

  uses
    sysutils,
    cclasses,
    globtype,
    compilerbase,
    finput;

  type
    tcontainedunit=record
      module:tmodulebase;
      ppufile:tpathstr;
      offset:longint;
      size:longint;
    end;
    pcontainedunit=^tcontainedunit;

    tpackage=class
    private
      FCompiler: TCompilerBase;
    protected
      property Compiler: TCompilerBase read FCompiler;
    public
      realpackagename,
      packagename : pshortstring;
      containedmodules : TFPHashList;
      requiredpackages : TFPHashObjectList;
      pcpfilename,
      ppafilename,
      pplfilename : tpathstr;
      constructor create(const pn:string;acompiler: TCompilerBase);
      destructor destroy;override;
    end;

    tpackageentry=record
      package : tpackage;
      realpkgname : string;
      usedunits : longint;
      direct : boolean;
    end;
    ppackageentry=^tpackageentry;

implementation

  uses
    cutils,globals,compiler;

  { tpackage }

  constructor tpackage.create(const pn: string;acompiler: TCompilerBase);
    begin
      FCompiler:=acompiler;
      realpackagename:=stringdup(pn);
      packagename:=stringdup(upper(pn));
      containedmodules:=TFPHashList.Create;
      requiredpackages:=TFPHashObjectList.Create(false);
    end;

  destructor tpackage.destroy;
    var
      p : pcontainedunit;
      i : longint;
    begin
      if assigned(containedmodules) then
        for i:=0 to containedmodules.count-1 do
          begin
            p:=pcontainedunit(containedmodules[i]);
            dispose(p);
          end;
      containedmodules.free;
      containedmodules := nil;
      requiredpackages.free;
      requiredpackages := nil;
      inherited destroy;
    end;


    procedure packageinit;
      var
        compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
      begin
        compiler.globals.packagelist:=TFPHashList.Create;
      end;


    procedure packagedone;
      var
        compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
      var
        i : longint;
        pkgentry : ppackageentry;
      begin
        if assigned(compiler.globals.packagelist) then
          begin
            for i:=0 to compiler.globals.packagelist.count-1 do
              begin
                pkgentry:=ppackageentry(compiler.globals.packagelist[i]);
                FreeAndNil(pkgentry^.package);
                dispose(pkgentry);
              end;
          end;
        compiler.globals.packagelist.Free;
        compiler.globals.packagelist:=nil;
      end;


initialization
  register_initdone_proc(@packageinit,@packagedone);
end.

