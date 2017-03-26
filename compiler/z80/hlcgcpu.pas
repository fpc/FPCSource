{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains routines to create a pass-through high-level code
    generator. This is used by most regular code generators.

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
    aasmdata,
    symdef,
    hlcg2ll;

  type
    thlcgcpu = class(thlcg2ll)
      procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
    end;

  procedure create_hlcodegen;

implementation

  uses
    hlcgobj,
    cgcpu;

  procedure thlcgcpu.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
    begin
      //internalerror(2011021324);
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgcpu.create;
      create_codegen;
    end;

begin
  chlcgobj:=thlcgcpu;
end.
