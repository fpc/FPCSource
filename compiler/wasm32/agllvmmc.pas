{
    Copyright (c) 1998-2020 by the Free Pascal team

    This unit implements the llvm-mc ("llvm machine code playground")
    assembler writer for WebAssembly

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

unit agllvmmc;

{$i fpcdefs.inc}

interface

  uses
    systems,
    globtype,globals,
    symbase,symdef,symtype,symconst,symcpu,
    aasmbase,aasmtai,aasmdata,
    assemble,aggas;

  type

    { TLLVMMachineCodePlaygroundAssembler }

    TLLVMMachineCodePlaygroundAssembler=class(TGNUassembler)
    protected
      procedure WriteProcDef(pd: tprocdef);
      procedure WriteSymtableProcdefs(st: TSymtable);

      function sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;override;
    public
      constructor CreateWithWriter(info: pasminfo; wr: TExternalAssemblerOutputFile; freewriter, smart: boolean); override;
      procedure WriteAsmList;override;
    end;

    { TWASM32InstrWriter }

    TWASM32InstrWriter = class(TCPUInstrWriter)
      procedure WriteInstruction(hp : tai);override;
    end;

implementation

  uses
    cutils,
    fmodule,finput,
    cpubase;

  { TLLVMMachineCodePlaygroundAssembler }


  procedure TLLVMMachineCodePlaygroundAssembler.WriteProcDef(pd: tprocdef);
    begin
      if not assigned(tcpuprocdef(pd).exprasmlist) and
         not(po_abstractmethod in pd.procoptions) and
         (pd.proctypeoption in [potype_unitinit,potype_unitfinalize]) then
        exit;

      writer.AsmWriteLn(asminfo^.comment+'WriteProcDef('+pd.mangledname+')');
      WriteTree(tcpuprocdef(pd).exprasmlist);
      writer.AsmWriteLn(asminfo^.comment+'WriteProcDef('+pd.mangledname+') done');
    end;


  procedure TLLVMMachineCodePlaygroundAssembler.WriteSymtableProcdefs(st: TSymtable);
    var
      i   : longint;
      def : tdef;
    begin
      if not assigned(st) then
        exit;
      for i:=0 to st.DefList.Count-1 do
        begin
          def:=tdef(st.DefList[i]);
          case def.typ of
            procdef :
              begin
                { methods are also in the static/globalsymtable of the unit
                  -> make sure they are only written for the objectdefs that
                  own them }
                if (not(st.symtabletype in [staticsymtable,globalsymtable]) or
                    (def.owner=st)) and
                   not(df_generic in def.defoptions) then
                  begin
                    WriteProcDef(tprocdef(def));
                    if assigned(tprocdef(def).localst) then
                      WriteSymtableProcdefs(tprocdef(def).localst);
                  end;
              end;
            else
              ;
          end;
        end;
    end;


  function TLLVMMachineCodePlaygroundAssembler.sectionname(atype: TAsmSectiontype; const aname: string; aorder: TAsmSectionOrder): string;
    begin
      Result:=inherited sectionname(atype, aname, aorder)+',"",@';
    end;


  constructor TLLVMMachineCodePlaygroundAssembler.CreateWithWriter(info: pasminfo; wr: TExternalAssemblerOutputFile; freewriter, smart: boolean);
    begin
      inherited;
      InstrWriter:=TWASM32InstrWriter.create(self);
    end;


  procedure TLLVMMachineCodePlaygroundAssembler.WriteAsmList;
    begin
      inherited;
      { print all global procedures/functions }
      WriteSymtableProcdefs(current_module.globalsymtable);
      WriteSymtableProcdefs(current_module.localsymtable);
    end;


  { TWASM32InstrWriter }


  procedure TWASM32InstrWriter.WriteInstruction(hp: tai);
    begin
      owner.writer.AsmWriteLn('# TODO: implement TWASM32InstrWriter.WriteInstruction');
    end;


  const
    as_wasm32_llvm_mc_info : tasminfo =
       (
         id     : as_wasm32_llvm_mc;
         idtxt  : 'LLVM-MC';
         asmbin : 'llvm-mc';
         asmcmd : '--assemble --arch=wasm32 --filetype=obj -o $OBJ $EXTRAOPT $ASM';
         supported_targets : [system_wasm32_wasm,system_wasm32_wasi];
         flags : [];
         labelprefix : '.L';
         labelmaxlen : -1;
         comment : '# ';
         dollarsign : '$';
       );

initialization
  RegisterAssembler(as_wasm32_llvm_mc_info,TLLVMMachineCodePlaygroundAssembler);
end.

