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
    ngenutil;


  type
    ti8086nodeutils = class(tnodeutils)
      class procedure InsertMemorySizes; override;
      class procedure InsertStackSegment;
      class procedure InsertHeapSegment;
      class procedure InsertStackPlusHeapSize;
    end;


implementation

  uses
    sysutils,cutils,
    globtype,globals,cpuinfo,
    aasmbase,aasmdata,aasmtai,
    symdef;


  class procedure ti8086nodeutils.InsertMemorySizes;
    begin
      inherited;
      if current_settings.x86memorymodel<>mm_tiny then
        InsertStackSegment;
      InsertHeapSegment;
      if current_settings.x86memorymodel in x86_near_data_models then
        InsertStackPlusHeapSize;
    end;


  class procedure ti8086nodeutils.InsertStackSegment;
    var
      stacksizeleft,stackblock: LongInt;
      i: Integer;
    begin
      maybe_new_object_file(current_asmdata.asmlists[al_globals]);
      new_section(current_asmdata.asmlists[al_globals],sec_stack,'__stack', 16);
      current_asmdata.asmlists[al_globals].concat(tai_symbol.Createname_global('___stack', AT_DATA, stacksize, carraydef.getreusable(u8inttype,stacksize)));
      { HACK: since tai_datablock's size parameter is aint, which cannot be
        larger than 32767 on i8086, but we'd like to support stack size of
        up to 64kb, we may need to use several tai_datablocks to reserve
        the stack segment }
      i:=0;
      stacksizeleft:=stacksize;
      while stacksizeleft>0 do
        begin
          stackblock:=min(stacksizeleft,high(aint));
          current_asmdata.asmlists[al_globals].concat(tai_datablock.Create('___stackblock'+IntToStr(i),stackblock,carraydef.getreusable(u8inttype,stackblock)));
          dec(stacksizeleft,stackblock);
          inc(i);
        end;
      current_asmdata.asmlists[al_globals].concat(tai_symbol.Createname_global('___stacktop',AT_DATA,0,voidtype));
    end;


  class procedure ti8086nodeutils.InsertHeapSegment;
    var
      heapsizeleft,heapblock: LongInt;
      i: Integer;
    begin
      maybe_new_object_file(current_asmdata.asmlists[al_globals]);
      new_section(current_asmdata.asmlists[al_globals],sec_heap,'__heap', 16);
      current_asmdata.asmlists[al_globals].concat(tai_symbol.Createname_global('___heap', AT_DATA, heapsize,carraydef.getreusable(u8inttype,heapsize)));
      { HACK: since tai_datablock's size parameter is aint, which cannot be
        larger than 32767 on i8086, but we'd like to support heap size of
        up to 640kb, we may need to use several tai_datablocks to reserve
        the heap segment }
      i:=0;
      heapsizeleft:=heapsize;
      while heapsizeleft>0 do
        begin
          heapblock:=min(heapsizeleft,high(aint));
          current_asmdata.asmlists[al_globals].concat(tai_datablock.Create('___heapblock'+IntToStr(i),heapblock,carraydef.getreusable(u8inttype,heapblock)));
          dec(heapsizeleft,heapblock);
          inc(i);
        end;
      current_asmdata.asmlists[al_globals].concat(tai_symbol.Createname_global('___heaptop',AT_DATA,0,voidtype));
    end;


  class procedure ti8086nodeutils.InsertStackPlusHeapSize;
    var
      maxheapsize_para: Word;
      stacksize_para: Word;
    begin
      maxheapsize_para:=(maxheapsize+15) div 16;
      stacksize_para:=(stacksize+15) div 16;

      maybe_new_object_file(current_asmdata.asmlists[al_globals]);
      new_section(current_asmdata.asmlists[al_globals],sec_data,'__fpc_stackplusmaxheap_in_para',sizeof(pint));
      current_asmdata.asmlists[al_globals].concat(Tai_symbol.Createname_global('__fpc_stackplusmaxheap_in_para',AT_DATA,4,u32inttype));
      current_asmdata.asmlists[al_globals].concat(Tai_const.Create_16bit(min($1000,stacksize_para+maxheapsize_para)));
    end;


begin
  cnodeutils:=ti8086nodeutils;
end.
