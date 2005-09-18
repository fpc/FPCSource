{
    Copyright (c) 2003-2004 by Peter Vreman and Florian Klaempfl

    This units contains support for debug info generation

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
      systems,
      aasmtai;

    type
      TDebugInfo=class
        constructor Create;virtual;
        procedure insertmodulestart(list:taasmoutput);virtual;
        procedure insertmoduleend(list:taasmoutput);virtual;
        procedure insertlineinfo(list:taasmoutput);virtual;
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


    procedure tdebuginfo.insertmodulestart(list:taasmoutput);
      begin
      end;


    procedure tdebuginfo.insertmoduleend(list:taasmoutput);
      begin
      end;


    procedure tdebuginfo.insertlineinfo(list:taasmoutput);
      begin
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

end.
