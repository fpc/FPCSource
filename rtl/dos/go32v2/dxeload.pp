{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Pierre Muller,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ Translated to FPK pascal by Pierre Muller,
without changing the fpu.s file }
{
/* Copyright (C) 1994, 1995 Charles Sandmann (sandmann@clio.rice.edu)
 * FPU setup and emulation hooks for DJGPP V2.0
 * This file maybe freely distributed, no warranty. */
this file has been translated from
  dxe.h
  dxeload.c
  npxsetup.c
  it uses fpu.as unchanged from fpu.s in DJGPP/SRC/LIBC/}

{/* Copyright (C) 1995 Charles Sandmann (sandmann@clio.rice.edu)
   This software may be freely distributed with above copyright, no warranty.
   Based on code by DJ Delorie, it's really his, enhanced, bugs fixed. */}


Unit dxeload;

  interface

    type
       dxe_header = record
                      magic : longint;
                      symbol_offset : longint;
                      element_size : longint;
                      nrelocs : longint;
                    end;

    const
       DXE_MAGIC  = $31455844;

{/* data stored after dxe_header in file; then relocs, 4 bytes each */}

    function dxe_load(filename : string) : pointer;

  implementation

    function dxe_load(filename : string) : pointer;

      type
          pointer_array = array[0..0] of pointer;
          tpa = ^pointer_array;
          plongint = ^longint;
          ppointer = ^pointer;
      var
         dh : dxe_header;
         data : pchar;
         f : file;
         relocs : tpa;
         i : longint;
	      addr : plongint;

      begin
         dxe_load:=nil;
         assign(f,filename);
         reset(f,1);
         blockread(f,@dh,sizeof(dxe_header));
         if dh.magic<>DXE_MAGIC then
           begin
              close(f);
              exit;
           end;

         { get memory for code }
         getmem(data,dh.element_size);
         if data=nil then
           exit;
         { get memory for relocations }
         getmem(relocs,dh.nrelocs*sizeof(pointer));
         if relocs=nil then
           begin
              freemem(data,dh.element_size);
              exit;
           end;
         { copy code }
         blockread(f,data^,dh.element_size);
         blockread(f,relocs^,dh.nrelocs*sizeof(pointer));

         { relocate internal references }
         for i:=0 to dh.nrelocs-1 do
           begin
              cardinal(addr):=cardinal(data)+cardinal(relocs^[i]);
              addr^:=addr^+pointer(data);
           end;
         dxe_load:=pointer( dh.symbol_offset + cardinal(data));
      end;
end.

{
  $Log$
  Revision 1.1.1.1  1998-03-25 11:18:42  root
  * Restored version

  Revision 1.3  1998/01/26 11:57:29  michael
  + Added log at the end

  Revision 1.2  1998/01/19 17:04:39  pierre
    * bug in dxe loading corrected, emu still does not work !!

  Revision 1.1  1998/01/16 16:50:49  pierre
      dxeload is a pascal version of the DJGPP C dxe loader

}


{
  $Log$
  Revision 1.1.1.1  1998-03-25 11:18:42  root
  * Restored version

  Revision 1.3  1998/01/26 11:57:29  michael
  + Added log at the end


  
  Working file: rtl/dos/go32v2/dxeload.pp
  description:
  ----------------------------
  revision 1.2
  date: 1998/01/19 17:04:39;  author: pierre;  state: Exp;  lines: +7 -3
    * bug in dxe loading corrected, emu still does not work !!
  ----------------------------
  revision 1.1
  date: 1998/01/16 16:50:49;  author: pierre;  state: Exp;
      dxeload is a pascal version of the DJGPP C dxe loader
  =============================================================================
}
