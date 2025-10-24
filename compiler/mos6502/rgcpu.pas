{
    Copyright (c) 1998-2008 by Florian Klaempfl

    This unit implements the MOS Technology 6502 specific class for the register
    allocator

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

unit rgcpu;

{$i fpcdefs.inc}

  interface

     uses
       aasmbase,aasmtai,aasmdata,aasmcpu,aasmsym,
       cgbase,cgutils,
       cpubase,
       systems,
       rgobj;

     type

       { tmos6502registermap }

       tmos6502registermap = class
         bmap: array [RS_RZB_FIRST..RS_RZB_LAST] of SmallInt;
         wmap: array [RS_RZW_FIRST..RS_RZW_LAST] of SmallInt;
         LastBReg, LastWReg: Integer;
         BRegCount, WRegCount: Integer;

         constructor Create(p0a: tmos6502page0alloc);
       end;

       trgcpu = class(trgobj)
       end;

       trgintcpu = class(trgcpu)
         procedure add_cpu_interferences(p : tai);override;
       end;

     function get_register_map_for_system(const sysinfo: tsysteminfo): tmos6502registermap;

  implementation

    uses
      globals,
      verbose, cutils,
      cgobj,
      procinfo;

    var
      register_maps: array [tsystem] of tmos6502registermap;

    { tmos6502registermap }

    constructor tmos6502registermap.Create(p0a: tmos6502page0alloc);
      var
        Page0Addr: Integer;
        R: Integer;
      begin
        { initialize with -1 }
        FillChar(bmap,SizeOf(bmap),$FF);
        FillChar(wmap,SizeOf(wmap),$FF);

        { allocate as many 16-bit registers as possible }
        LastWReg:=RS_RZW_FIRST-1;
        for Page0Addr:=0 to 254 do
          if (Page0Addr in p0a) and (Page0Addr+1 in p0a) then
            begin
              exclude(p0a,Page0Addr);
              exclude(p0a,Page0Addr+1);
              if LastWReg>=RS_RZW_LAST then
                break;
              Inc(LastWReg);
              wmap[LastWReg]:=Page0Addr;
            end;
        WRegCount:=LastWReg-RS_RZW_FIRST+1;

        { allocate remaining 8-bit registers }
        LastBReg:=RS_RZB_FIRST-1;
        for Page0Addr:=0 to 255 do
          if Page0Addr in p0a then
            begin
              if LastBReg>=RS_RZB_LAST then
                break;
              Inc(LastBReg);
              bmap[LastBReg]:=Page0Addr;
            end;
        BRegCount:=LastBReg-RS_RZB_FIRST+1;

        {for R:=RS_RZB_FIRST to LastBReg do
          Writeln('RS_RZB',R-RS_RZB_FIRST,'=*',bmap[R]);
        for R:=RS_RZW_FIRST to LastWReg do
          Writeln('RS_RZW',R-RS_RZW_FIRST,'=*',wmap[R]);}
      end;


    procedure trgintcpu.add_cpu_interferences(p : tai);
      var
        r : tsuperregister;
      begin
        inherited;
        //if p.typ=ait_instruction then
        //  begin
        //    case taicpu(p).opcode of
        //      A_CPI,
        //      A_ANDI,
        //      A_ORI,
        //      A_SUBI,
        //      A_SBCI,
        //      A_LDI:
        //        for r:=RS_R0 to RS_R15 do
        //          add_edge(r,GetSupReg(taicpu(p).oper[0]^.reg));
        //      A_MULS:
        //        begin
        //          for r:=RS_R0 to RS_R15 do
        //            add_edge(r,GetSupReg(taicpu(p).oper[0]^.reg));
        //          for r:=RS_R0 to RS_R15 do
        //            add_edge(r,GetSupReg(taicpu(p).oper[1]^.reg));
        //        end;
        //    end;
        //  end;
      end;


    function get_register_map_for_system(const sysinfo: tsysteminfo): tmos6502registermap;
      begin
        if not assigned(register_maps[sysinfo.system]) then
          register_maps[sysinfo.system]:=tmos6502registermap.create(sysinfo.mos6502page0alloc);
        result:=register_maps[sysinfo.system];
      end;


    procedure done_rgcpu;
      var
        s: tsystem;
      begin
        for s in tsystem do
          begin
            register_maps[s].Free;
            register_maps[s]:=nil;
          end;
      end;


initialization
  register_initdone_proc(nil,@done_rgcpu);
end.
