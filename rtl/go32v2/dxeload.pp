{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998-2000 by Pierre Muller,
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
     nrelocs       : longint;
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
  pointer_array = array[0..maxlongint] of pointer;
  tpa = ^pointer_array;
  plongint = ^longint;
  ppointer = ^pointer;
var
  dh     : dxe_header;
  data   : pchar;
  f      : file;
  relocs : tpa;
  i      : longint;
  addr   : plongint;
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
   blockread(f,@dh,sizeof(dxe_header),i);
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
        addr^:=addr^+pointer(data);
     end;
   dxe_load:=pointer( dh.symbol_offset + cardinal(data));
end;

end.
{
  $Log$
  Revision 1.2  2000-01-07 16:32:23  daniel
    * copyright 2000 added

  Revision 1.1  1998/12/21 13:07:02  peter
    * use -FE

  Revision 1.5  1998/10/23 16:45:41  pierre
   * check if file can be opened, returns nil if not

  Revision 1.4  1998/10/21 17:05:46  pierre
   * wmemu387.dxe was not closed !!

  Revision 1.3  1998/10/21 16:51:08  pierre
   * dxeload range check problem solved

  Revision 1.2  1998/05/31 14:18:24  peter
    * force att or direct assembling
    * cleanup of some files

}
