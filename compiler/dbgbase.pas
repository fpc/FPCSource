{
    Copyright (c) 2003-2006 by Peter Vreman and Florian Klaempfl

    This units contains the base class for debug info generation

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
unit dbgbase;

{$i fpcdefs.inc}

interface

    uses
      cclasses,
      systems,
      symconst,symbase,symdef,symtype,symsym,symtable,
      fmodule,
      aasmtai,aasmdata;

    type
      TDebugInfo=class
        constructor Create;virtual;

        procedure reset_unit_type_info;

        procedure inserttypeinfo;virtual;
        procedure insertmoduleinfo;virtual;
        procedure insertlineinfo(list:TAsmList);virtual;
        procedure referencesections(list:TAsmList);virtual;
        procedure insertdef(list:TAsmList;def:tdef);virtual;abstract;
        procedure write_symtable_defs(list:TAsmList;st:TSymtable);virtual;abstract;

        procedure write_used_unit_type_info(list:TAsmList;hp:tmodule);
        procedure field_write_defs(p:TObject;arg:pointer);
        procedure method_write_defs(p:TObject;arg:pointer);
      end;
      TDebugInfoClass=class of TDebugInfo;

    var
      CDebugInfo : array[tdbg] of TDebugInfoClass;
      DebugInfo  : TDebugInfo;

    procedure InitDebugInfo;
    procedure DoneDebugInfo;
    procedure RegisterDebugInfo(const r:tdbginfo;c:TDebugInfoClass);


implementation

    uses
      verbose;


    constructor tdebuginfo.Create;
      begin
      end;


    procedure tdebuginfo.insertmoduleinfo;
      begin
      end;


    procedure tdebuginfo.inserttypeinfo;
      begin
      end;


    procedure tdebuginfo.insertlineinfo(list:TAsmList);
      begin
      end;


    procedure tdebuginfo.referencesections(list:TAsmList);
      begin
      end;


    procedure tdebuginfo.reset_unit_type_info;
      var
        hp : tmodule;
      begin
        hp:=tmodule(loaded_units.first);
        while assigned(hp) do
          begin
            hp.is_dbginfo_written:=false;
            hp:=tmodule(hp.next);
          end;
      end;


    procedure TDebugInfo.field_write_defs(p:TObject;arg:pointer);
      begin
        if (Tsym(p).typ=fieldvarsym) and
           not(sp_static in Tsym(p).symoptions) then
          insertdef(TAsmList(arg),tfieldvarsym(p).vardef);
      end;


    procedure TDebugInfo.method_write_defs(p:TObject;arg:pointer);
      var
        i  : longint;
        pd : tprocdef;
      begin
        if tsym(p).typ<>procsym then
          exit;
        for i:=0 to tprocsym(p).ProcdefList.Count-1 do
          begin
            pd:=tprocdef(tprocsym(p).ProcdefList[i]);
            insertdef(TAsmList(arg),pd.returndef);
          end;
      end;


    procedure TDebugInfo.write_used_unit_type_info(list:TAsmList;hp:tmodule);
      var
        pu : tused_unit;
      begin
        pu:=tused_unit(hp.used_units.first);
        while assigned(pu) do
          begin
            if not pu.u.is_dbginfo_written then
              begin
                { prevent infinte loop for circular dependencies }
                pu.u.is_dbginfo_written:=true;
                { write type info from used units, use a depth first
                  strategy to reduce the recursion in writing all
                  dependent stabs }
                write_used_unit_type_info(list,pu.u);
                if assigned(pu.u.globalsymtable) then
                  write_symtable_defs(list,pu.u.globalsymtable);
              end;
            pu:=tused_unit(pu.next);
          end;
      end;


    procedure InitDebugInfo;
      begin
        if not assigned(CDebugInfo[target_dbg.id]) then
          begin
            Comment(V_Fatal,'cg_f_debuginfo_output_not_supported');
            exit;
          end;
        DebugInfo:=CDebugInfo[target_dbg.id].Create;
      end;


    procedure DoneDebugInfo;
      begin
        if assigned(DebugInfo) then
          begin
            DebugInfo.Free;
            DebugInfo:=nil;
          end;
      end;


    procedure RegisterDebugInfo(const r:tdbginfo;c:TDebugInfoClass);
      var
        t : tdbg;
      begin
        t:=r.id;
        if assigned(dbginfos[t]) then
          writeln('Warning: DebugInfo is already registered!')
        else
          Getmem(dbginfos[t],sizeof(tdbginfo));
        dbginfos[t]^:=r;
        CDebugInfo[t]:=c;
      end;


    const
      dbg_none_info : tdbginfo =
         (
           id     : dbg_none;
           idtxt  : 'NONE';
         );

initialization
  RegisterDebugInfo(dbg_none_info,tdebuginfo);
end.
