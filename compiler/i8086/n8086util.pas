{
    Copyright (c) 2014 by Nikolay Nikolov

    i8086 version of some node tree helper routines

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
unit n8086util;

{$i fpcdefs.inc}

interface

  uses
    ngenutil,aasmdata,compilerbase;


  type
    ti8086nodeutils = class(tnodeutils)
      procedure InsertMemorySizes(AsmData: TAsmData); override;
      procedure InsertStackSegment(AsmData: TAsmData);
      procedure InsertHeapSegment(AsmData: TAsmData);
      procedure InsertStackPlusHeapSize(AsmData: TAsmData);
    end;


implementation

  uses
    sysutils,cutils,
    globtype,globals,cpuinfo,systemstypes,systems,
    aasmbase,aasmtai,
    symdef,
    compiler;


  procedure ti8086nodeutils.InsertMemorySizes(AsmData: TAsmData);
    begin
      inherited;
      if compiler.globals.current_settings.x86memorymodel<>mm_tiny then
        InsertStackSegment(AsmData);
      if compiler.target.info.system<>system_i8086_win16 then
        InsertHeapSegment(AsmData);
      if compiler.globals.current_settings.x86memorymodel in x86_near_data_models then
        InsertStackPlusHeapSize(AsmData);
    end;


  procedure ti8086nodeutils.InsertStackSegment(AsmData: TAsmData);
    var
      stacksizeleft,stackblock: LongInt;
      i: Integer;
    begin
      maybe_new_object_file(AsmData.asmlists[al_globals]);
      new_section(AsmData.asmlists[al_globals],sec_stack,'__stack', 16);
      AsmData.asmlists[al_globals].concat(tai_symbol.Createname_global(AsmData,'___stack', AT_DATA, compiler.globals.stacksize, carraydef.getreusable(compiler.deftypes.u8inttype,compiler.globals.stacksize,compiler)));
      { HACK: since tai_datablock's size parameter is aint, which cannot be
        larger than 32767 on i8086, but we'd like to support stack size of
        up to 64kb, we may need to use several tai_datablocks to reserve
        the stack segment }
      i:=0;
      stacksizeleft:=compiler.globals.stacksize;
      while stacksizeleft>0 do
        begin
          stackblock:=min(stacksizeleft,high(aint));
          AsmData.asmlists[al_globals].concat(tai_datablock.Create(AsmData,'___stackblock'+IntToStr(i),stackblock,carraydef.getreusable(compiler.deftypes.u8inttype,stackblock,compiler),AT_DATA));
          dec(stacksizeleft,stackblock);
          inc(i);
        end;
      AsmData.asmlists[al_globals].concat(tai_symbol.Createname_global(AsmData,'___stacktop',AT_DATA,0,compiler.deftypes.voidtype));
    end;


  procedure ti8086nodeutils.InsertHeapSegment(AsmData: TAsmData);
    var
      heapsizeleft,heapblock: LongInt;
      i: Integer;
    begin
      maybe_new_object_file(AsmData.asmlists[al_globals]);
      new_section(AsmData.asmlists[al_globals],sec_heap,'__heap', 16);
      AsmData.asmlists[al_globals].concat(tai_symbol.Createname_global(AsmData,'___heap', AT_DATA, compiler.globals.heapsize,carraydef.getreusable(compiler.deftypes.u8inttype,compiler.globals.heapsize,compiler)));
      { HACK: since tai_datablock's size parameter is aint, which cannot be
        larger than 32767 on i8086, but we'd like to support heap size of
        up to 640kb, we may need to use several tai_datablocks to reserve
        the heap segment }
      i:=0;
      heapsizeleft:=compiler.globals.heapsize;
      while heapsizeleft>0 do
        begin
          heapblock:=min(heapsizeleft,high(aint));
          AsmData.asmlists[al_globals].concat(tai_datablock.Create(AsmData,'___heapblock'+IntToStr(i),heapblock,carraydef.getreusable(compiler.deftypes.u8inttype,heapblock,compiler),AT_DATA));
          dec(heapsizeleft,heapblock);
          inc(i);
        end;
      AsmData.asmlists[al_globals].concat(tai_symbol.Createname_global(AsmData,'___heaptop',AT_DATA,0,compiler.deftypes.voidtype));
    end;


  procedure ti8086nodeutils.InsertStackPlusHeapSize(AsmData: TAsmData);
    var
      maxheapsize_para: Word;
      stacksize_para: Word;
    begin
      maxheapsize_para:=(compiler.globals.maxheapsize+15) div 16;
      stacksize_para:=(compiler.globals.stacksize+15) div 16;

      maybe_new_object_file(AsmData.asmlists[al_globals]);
      new_section(AsmData.asmlists[al_globals],sec_data,'__fpc_stackplusmaxheap_in_para',sizeof(pint));
      AsmData.asmlists[al_globals].concat(Tai_symbol.Createname_global(AsmData,'__fpc_stackplusmaxheap_in_para',AT_DATA,4,compiler.deftypes.u32inttype));
      AsmData.asmlists[al_globals].concat(Tai_const.Create_16bit(min($1000,stacksize_para+maxheapsize_para)));
    end;


begin
  cnodeutils:=ti8086nodeutils;
end.
