{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Pierre Muller,
    member of the Free Pascal development team.

    Unit to Load DXE files for Go32V2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}


Unit dxeload;
interface

const
   DXE_MAGIC  = $31455844;
type
  dxe_header = record
     magic,
     symbol_offset,
     element_size,
     nrelocs       : cardinal;
  end;

function dxe_load(filename : string) : pointer;

implementation

function dxe_load(filename : string) : pointer;
{
  Copyright (C) 1995 Charles Sandmann (sandmann@clio.rice.edu)
  translated to Free Pascal by Pierre Muller
}
type
  { to avoid range check problems }
  pointer_array = array[0..maxlongint div sizeof(pointer)] of pointer;
  tpa = ^pointer_array;
  plongint = ^longint;
  pcardinal = ^cardinal;
  ppointer = ^pointer;
var
  dh     : dxe_header;
  data   : pchar;
  f      : file;
  relocs : tpa;
  i      : longint;
  addr   : pcardinal;
begin
   dxe_load:=nil;
{ open the file }
   assign(f,filename);
{$I-}
   reset(f,1);
{$I+}
   { quit if no file !! }
   if ioresult<>0 then
     exit;
{ load the header }
   blockread(f,dh,sizeof(dxe_header),i);
   if (i<>sizeof(dxe_header)) or (dh.magic<>DXE_MAGIC) then
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
   close(f);
{ relocate internal references }
   for i:=0 to dh.nrelocs-1 do
     begin
        cardinal(addr):=cardinal(data)+cardinal(relocs^[i]);
        addr^:=addr^+cardinal(data);
     end;
   FreeMem(relocs,dh.nrelocs*sizeof(pointer));
   dxe_load:=pointer( dh.symbol_offset + cardinal(data));
end;

end.
{
  $Log$
  Revision 1.5  2002-04-27 07:58:23  peter
    * fixed 2gb limit

  Revision 1.4  2001/07/23 09:52:38  marco
   * Fixed an unnecessary memleak.

  Revision 1.3  2000/12/16 15:57:52  jonas
    * avoid the longint + cardinal to int64 conversion (merged Pierre's patch)

  Revision 1.2  2000/07/13 11:33:40  michael
  + removed logs

}
