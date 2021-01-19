{
    Copyright (c) 2002-2010 by Florian Klaempfl and Jonas Maebe

    This unit contains the CPU specific part of tprocinfo

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
unit cpupi;

{$i fpcdefs.inc}

interface

  uses
    cutils,globtype,
    procinfo,cpuinfo, symtype,aasmbase,
    psub, cclasses;

  type

    { tcpuprocinfo }

    tcpuprocinfo=class(tcgprocinfo)
    public
      procedure setup_eh; override;
      procedure postprocess_code; override;
      procedure set_first_temp_offset;override;
    end;

implementation

    uses
      systems,globals,cpubase,tgcpu,aasmdata,aasmcpu,aasmtai,cgexcept,
      tgobj,paramgr,symconst,symcpu;

{*****************************************************************************
                     twasmexceptionstatehandler
*****************************************************************************}

    type
      twasmexceptionstatehandler = class(tcgexceptionstatehandler)
        class procedure new_exception(list:TAsmList;const t:texceptiontemps; const exceptframekind: texceptframekind; out exceptstate: texceptionstate); override;
        class procedure free_exception(list: TAsmList; const t: texceptiontemps; const s: texceptionstate; a: aint; endexceptlabel: tasmlabel; onlyfree:boolean); override;
      end;

    class procedure twasmexceptionstatehandler.new_exception(list:TAsmList;const t:texceptiontemps; const exceptframekind: texceptframekind; out exceptstate: texceptionstate);
      begin
        list.Concat(tai_comment.Create(strpnew('TODO: new_exception')));
      end;

    class procedure twasmexceptionstatehandler.free_exception(list: TAsmList; const t: texceptiontemps; const s: texceptionstate; a: aint; endexceptlabel: tasmlabel; onlyfree:boolean);
      begin
        list.Concat(tai_comment.Create(strpnew('TODO: free_exception')));
      end;

{*****************************************************************************
                           tcpuprocinfo
*****************************************************************************}

    procedure tcpuprocinfo.setup_eh;
      begin
        cexceptionstatehandler:=twasmexceptionstatehandler;
      end;

    procedure tcpuprocinfo.postprocess_code;

      function findfirst_tai_local(asmlist: TAsmList): tai_local;
        var
          hp: tai;
        begin
          result:=nil;
          if not assigned(asmlist) then
            exit;
          hp:=tai(asmlist.first);
          while assigned(hp) do
            begin
              if hp.typ=ait_local then
                begin
                  result:=tai_local(hp);
                  exit;
                end;
              hp:=tai(hp.Next);
            end;
        end;

      procedure replace_local_frame_pointer(asmlist: TAsmList);
        var
          hp: tai;
          instr: taicpu;
          l: Integer;
        begin
          if not assigned(asmlist) then
            exit;
          hp:=tai(asmlist.first);
          while assigned(hp) do
            begin
              if hp.typ=ait_instruction then
                begin
                  instr:=taicpu(hp);
                  for l:=0 to instr.ops-1 do
                    if (instr.oper[l]^.typ=top_reg) and (instr.oper[l]^.reg=NR_LOCAL_FRAME_POINTER_REG) then
                      instr.loadref(l,tcpuprocdef(current_procinfo.procdef).frame_pointer_ref);
                end;
              hp:=tai(hp.Next);
            end;
        end;

      var
       templist : TAsmList;
       l : TWasmLocal;
       first_tai_local, last_tai_local: tai_local;
      begin
        templist := TAsmList.create;
        l := ttgwasm(tg).localvars.first;
        while Assigned(l) do begin
          templist.Concat( tai_local.create(l.typ));
          l := l.nextseq;
        end;
        aktproccode.insertListBefore(findfirst_tai_local(aktproccode),templist);
        templist.Free;

        first_tai_local:=findfirst_tai_local(aktproccode);
        if assigned(first_tai_local) then
          begin
            first_tai_local.first:=true;
            { also find the last tai_local in the group }
            last_tai_local:=first_tai_local;
            while assigned(last_tai_local.Next) and (tai(last_tai_local.Next).typ=ait_local) do
              last_tai_local:=tai_local(last_tai_local.Next);
            last_tai_local.last:=true;
          end;

        replace_local_frame_pointer(aktproccode);

        inherited postprocess_code;
      end;

    procedure tcpuprocinfo.set_first_temp_offset;
      var
        sz : integer;
        i  : integer;
        sym: tsym;
      begin
        {
          Stackframe layout:
          sp:
            <incoming parameters>
            sp+first_temp_offset:
            <locals>
            <temp>
        }
        procdef.init_paraloc_info(calleeside);
        sz := procdef.calleeargareasize;
        tg.setfirsttemp(sz);
      end;


initialization
  cprocinfo:=tcpuprocinfo;
end.
