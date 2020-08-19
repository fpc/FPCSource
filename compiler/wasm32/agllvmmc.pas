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
    aasmdata,
    assemble;

  type

    { TLLVMMachineCodePlaygroundAssembler }

    TLLVMMachineCodePlaygroundAssembler=class(texternalassembler)
    public
      procedure WriteAsmList;override;
    end;

implementation

  { TLLVMMachineCodePlaygroundAssembler }

  procedure TLLVMMachineCodePlaygroundAssembler.WriteAsmList;
    var
      hal: TAsmListType;
    begin
      for hal:=low(TasmlistType) to high(TasmlistType) do
        begin
          writer.AsmWriteLn(asminfo^.comment+'Begin asmlist '+AsmListTypeStr[hal]);
          writetree(current_asmdata.asmlists[hal]);
          writer.AsmWriteLn(asminfo^.comment+'End asmlist '+AsmListTypeStr[hal]);
        end;
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

