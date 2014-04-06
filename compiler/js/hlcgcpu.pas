{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit implements the js high level code generator

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
unit hlcgcpu;

{$i fpcdefs.inc}

interface

uses
  globtype,
  aasmbase,aasmdata,
  symbase,symconst,symtype,symdef,symsym,
  cpubase, hlcgobj, cgbase, cgutils, parabase;

  type
    thlcgjs = class(thlcgobj)
    public
      constructor create;
    end;

  procedure create_hlcodegen;

implementation

  uses
    verbose,cutils,globals,fmodule,constexp,
    defutil,
    aasmtai,aasmcpu,
    symtable,
    procinfo,cpuinfo,cgcpu,tgobj;


  constructor thlcgjs.create;
    begin
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgjs.create;
      create_codegen;
    end;

end.
