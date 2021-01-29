{
    Copyright (c) 2016 by the Free Pascal development team

    This unit is the VASM assembler writer for PowerPC

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

unit agppcvasm;

{$i fpcdefs.inc}

  interface

    uses
       aasmbase,systems,
       aasmtai,aasmdata,
       assemble,aggas,agppcgas,
       cpubase,cgutils,
       globtype;

  type
    tppcvasm = class(TPPCGNUassembler)
    protected
      function sectionattrs(atype:TAsmSectiontype):string; override;
    public
      constructor CreateWithWriter(info: pasminfo; wr: TExternalAssemblerOutputFile; freewriter, smart: boolean); override;
      function MakeCmdLine: TCmdStr; override;
      procedure WriteExtraHeader; override;
    end;

  implementation

    uses
       cutils,cfileutl,globals,verbose,
       cgbase,
       cscript,
       itcpugas,cpuinfo,
       aasmcpu;


{****************************************************************************}
{                         VASM PPC Assembler writer                          }
{****************************************************************************}


    constructor tppcvasm.CreateWithWriter(info: pasminfo; wr: TExternalAssemblerOutputFile; freewriter, smart: boolean);
      begin
        inherited;
        InstrWriter := TPPCInstrWriter.create(self);
      end;

    function tppcvasm.sectionattrs(atype:TAsmSectiontype):string;
      begin
        case atype of
          sec_code, sec_fpc, sec_init, sec_fini:
            result:='acrx';
          sec_data:
            result:='adrw';
          sec_rodata, sec_rodata_norel:
            result:='adr';
          sec_bss, sec_threadvar:
            result:='aurw';
          sec_stab, sec_stabstr:
            result:='dr';
          else
            result:='';
        end;
      end;

    function tppcvasm.MakeCmdLine: TCmdStr;
      var
        objtype: string;
      begin
        result:=asminfo^.asmcmd;

        objtype:='-Felf';
        if (target_info.system in [system_powerpc_amiga, system_powerpc_morphos]) then 
          begin
            Replace(result,'$ASM',maybequoted(ScriptFixFileName(Unix2AmigaPath(AsmFileName))));
            Replace(result,'$OBJ',maybequoted(ScriptFixFileName(Unix2AmigaPath(ObjFileName))));
          end
        else
          begin
            Replace(result,'$ASM',maybequoted(ScriptFixFileName(AsmFileName)));
            Replace(result,'$OBJ',maybequoted(ScriptFixFileName(ObjFileName)));
          end;
        Replace(result,'$OTYPE',objtype);
        Replace(result,'$EXTRAOPT',asmextraopt);
      end;

    procedure tppcvasm.WriteExtraHeader;
      begin
        { no-op, compared to the PPC GAS writer, because vasm defines
          the register symbols by default, so lets not redefine them }
      end;


{*****************************************************************************
                                  Initialize
*****************************************************************************}

  const
    as_powerpc_vasm_info : tasminfo =
       (
         id     : as_powerpc_vasm;

         idtxt  : 'VASM';
         asmbin : 'vasmppc_std';
         asmcmd:  '-quiet $OTYPE -o $OBJ $EXTRAOPT $ASM';
         supported_targets : [system_powerpc_amiga,system_powerpc_morphos,system_powerpc_linux];
         flags : [af_needar,af_smartlink_sections];
         labelprefix : '.L';
         labelmaxlen : -1;
         comment : '# ';
         dollarsign: '$';
       );

begin
  RegisterAssembler(as_powerpc_vasm_info,tppcvasm);
end.
