{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Routines to read/write ppu files

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
unit ppu;

{$i fpcdefs.inc}

interface

  uses
    constexp,entfile;

{ Also write the ppu if only crc if done, this can be used with ppudump to
  see the differences between the intf and implementation }
{ define INTFPPU}

{$ifdef Test_Double_checksum}
var
  CRCFile : text;
const
  CRC_array_Size = 200000;
type
  tcrc_array = array[0..crc_array_size] of dword;
  pcrc_array = ^tcrc_array;
{$endif Test_Double_checksum}

const
  { only update this version if something change in the tppuheader:
     * the unit flags listed below
     * the format of the header itslf
    This number cannot become bigger than 255 (it's stored in a byte) }
  CurrentPPUVersion = 207;

{ unit flags }
  uf_init                = $000001; { unit has initialization section }
  uf_finalize            = $000002; { unit has finalization section   }
  uf_big_endian          = $000004;
//uf_has_browser         = $000010;
  uf_in_library          = $000020; { is the file in another file than <ppufile>.* ? }
  uf_smart_linked        = $000040; { the ppu can be smartlinked }
  uf_static_linked       = $000080; { the ppu can be linked static }
  uf_shared_linked       = $000100; { the ppu can be linked shared }
//uf_local_browser       = $000200;
  uf_checkpointer_called = $000200; { Unit uses experimental checkpointer test code }
  uf_no_link             = $000400; { unit has no .o generated, but can still have external linking! }
  uf_has_resourcestrings = $000800; { unit has resource string section }
  uf_little_endian       = $001000;
  uf_release             = $002000; { unit was compiled with -Ur option }
  uf_threadvars          = $004000; { unit has threadvars }
  uf_fpu_emulation       = $008000; { this unit was compiled with fpu emulation on }
  uf_has_stabs_debuginfo = $010000; { this unit has stabs debuginfo generated }
  uf_local_symtable      = $020000; { this unit has a local symtable stored }
  uf_uses_variants       = $040000; { this unit uses variants }
  uf_has_resourcefiles   = $080000; { this unit has external resources (using $R directive)}
  uf_has_exports         = $100000; { this module or a used unit has exports }
  uf_has_dwarf_debuginfo = $200000; { this unit has dwarf debuginfo generated }
  uf_wideinits           = $400000; { this unit has winlike widestring typed constants }
  uf_classinits          = $800000; { this unit has class constructors/destructors }
  uf_resstrinits        = $1000000; { this unit has string consts referencing resourcestrings }
  uf_i8086_far_code     = $2000000; { this unit uses an i8086 memory model with far code (i.e. medium, large or huge) }
  uf_i8086_far_data     = $4000000; { this unit uses an i8086 memory model with far data (i.e. compact or large) }
  uf_i8086_huge_data    = $8000000; { this unit uses an i8086 memory model with huge data (i.e. huge) }
  uf_i8086_cs_equals_ds = $10000000; { this unit uses an i8086 memory model with CS=DS (i.e. tiny) }
  uf_package_deny       = $20000000; { this unit must not be part of a package }
  uf_package_weak       = $40000000; { this unit may be completely contained in a package }
  uf_i8086_ss_equals_ds = $80000000; { this unit uses an i8086 memory model with SS=DS (i.e. tiny, small or medium) }

type
  { bestreal is defined based on the target architecture }
  ppureal=bestreal;

  tppuerror=(ppuentrytoobig,ppuentryerror);

  tppuheader=record
    common   : tentryheader;
    checksum : cardinal; { checksum for this ppufile }
    interface_checksum : cardinal;
    deflistsize,
    symlistsize : longint;
    indirect_checksum: cardinal;
  end;

  tppuentry=tentry;

  tunitasmlisttype=(ualt_public,ualt_extern);

  { tppufile }

  tppufile=class(tentryfile)
{$ifdef Test_Double_checksum}
  public
    crcindex,
    crc_index,
    crcindex2,
    crc_index2 : cardinal;
    crc_test,
    crc_test2  : pcrc_array;
  private
{$endif def Test_Double_checksum}
  protected
    procedure newheader;override;
    function readheader: longint;override;
    function outputallowed: boolean;override;
    //procedure doputdata(const b;len:integer);override;
    function getheadersize:longint;override;
    function getheaderaddr:pentryheader;override;
    procedure resetfile;override;
  public
    header           : tppuheader;
    { crc for the entire unit }
    crc,
    { crc for the interface definitions in this unit }
    interface_crc,
    { crc of all object/class definitions in the interface of this unit, xor'ed
      by the crc's of all object/class definitions in the interfaces of units
      used by this unit. Reason: see mantis #13840 }
    indirect_crc     : cardinal;
    do_crc,
    do_interface_crc,
    do_indirect_crc  : boolean;
    crc_only         : boolean;    { used to calculate interface_crc before implementation }
    constructor Create(const fn:string);
    destructor destroy;override;
    function  CheckPPUId:boolean;
  {read}
  { nothing special currently }
  {write}
    function  createfile:boolean;override;
    procedure writeheader;override;
    procedure putdata(const b;len:integer);override;
  end;

implementation

  uses
{$ifdef Test_Double_checksum}
    comphook,
{$endif def Test_Double_checksum}
    fpccrc;

function swapendian_ppureal(d:ppureal):ppureal;

type ppureal_bytes=array[0..sizeof(d)-1] of byte;

var i:0..sizeof(d)-1;

begin
  for i:=low(ppureal_bytes) to high(ppureal_bytes) do
    ppureal_bytes(swapendian_ppureal)[i]:=ppureal_bytes(d)[high(ppureal_bytes)-i];
end;


{*****************************************************************************
                                  TPPUFile
*****************************************************************************}

constructor tppufile.Create(const fn:string);
begin
  inherited Create(fn);
  crc_only:=false;
{$ifdef Test_Double_checksum}
  if not assigned(crc_test) then
    new(crc_test);
  if not assigned(crc_test2) then
    new(crc_test2);
{$endif Test_Double_checksum}
end;

destructor tppufile.destroy;
begin
{$ifdef Test_Double_checksum}
  if assigned(crc_test) then
    dispose(crc_test);
  crc_test:=nil;
  if assigned(crc_test2) then
    dispose(crc_test2);
  crc_test2:=nil;
{$endif Test_Double_checksum}
  inherited destroy;
end;

function tppufile.CheckPPUId:boolean;
begin
  CheckPPUId:=((Header.common.Id[1]='P') and
                (Header.common.Id[2]='P') and
                (Header.common.Id[3]='U'));
end;


procedure tppufile.newheader;
var
  s : string;
begin
  fillchar(header,sizeof(tppuheader),0);
  str(currentppuversion,s);
  while length(s)<3 do
   s:='0'+s;
  with header.common do
   begin
     Id[1]:='P';
     Id[2]:='P';
     Id[3]:='U';
     Ver[1]:=s[1];
     Ver[2]:=s[2];
     Ver[3]:=s[3];
   end;
end;


function tppufile.readheader: longint;
begin
  if fsize<sizeof(tppuheader) then
    exit(0);
  result:=f.Read(header,sizeof(tppuheader));
  { The header is always stored in little endian order }
  { therefore swap if on a big endian machine          }
{$IFDEF ENDIAN_BIG}
  header.common.compiler := swapendian(header.common.compiler);
  header.common.cpu := swapendian(header.common.cpu);
  header.common.target := swapendian(header.common.target);
  header.common.flags := swapendian(header.common.flags);
  header.common.size := swapendian(header.common.size);
  header.checksum := swapendian(header.checksum);
  header.interface_checksum := swapendian(header.interface_checksum);
  header.indirect_checksum := swapendian(header.indirect_checksum);
  header.deflistsize:=swapendian(header.deflistsize);
  header.symlistsize:=swapendian(header.symlistsize);

  { the PPU DATA is stored in native order }
  change_endian := (header.common.flags and uf_little_endian) = uf_little_endian;
{$ELSE not ENDIAN_BIG}
  change_endian := (header.common.flags and uf_big_endian) = uf_big_endian;
{$ENDIF}
end;

function tppufile.outputallowed: boolean;
begin
  result:=not crc_only;
end;


{*****************************************************************************
                                TPPUFile Reading
*****************************************************************************}

{ nothing special currently }

{*****************************************************************************
                                TPPUFile Writing
*****************************************************************************}

function tppufile.createfile:boolean;
begin
{$ifdef INTFPPU}
  if crc_only then
   begin
     fname:=fname+'.intf';
     crc_only:=false;
   end;
{$endif}
  result:=inherited createfile;
end;


procedure tppufile.writeheader;
var
  opos : integer;
begin
  if crc_only then
   exit;
  { flush buffer }
  writebuf;
  { update size (w/o header!) in the header }
  header.common.size:=bufstart-sizeof(tppuheader);
  { set the endian flag }
{$ifndef FPC_BIG_ENDIAN}
    header.common.flags := header.common.flags or uf_little_endian;
{$else not FPC_BIG_ENDIAN}
    header.common.flags := header.common.flags or uf_big_endian;
    { Now swap the header.common in the correct endian (always little endian) }
    header.common.compiler := swapendian(header.common.compiler);
    header.common.cpu := swapendian(header.common.cpu);
    header.common.target := swapendian(header.common.target);
    header.common.flags := swapendian(header.common.flags);
    header.common.size := swapendian(header.common.size);
    header.checksum := swapendian(header.checksum);
    header.interface_checksum := swapendian(header.interface_checksum);
    header.indirect_checksum := swapendian(header.indirect_checksum);
    header.deflistsize:=swapendian(header.deflistsize);
    header.symlistsize:=swapendian(header.symlistsize);
{$endif not FPC_BIG_ENDIAN}
{ write header and restore filepos after it }
  opos:=f.Position;
  f.Position:=0;
  f.Write(header,sizeof(tppuheader));
  f.Position:=opos;
end;


procedure tppufile.putdata(const b;len:integer);
begin
  if do_crc then
   begin
     crc:=UpdateCrc32(crc,b,len);
{$ifdef Test_Double_checksum}
     if crc_only then
       begin
         crc_test2^[crc_index2]:=crc;
{$ifdef Test_Double_checksum_write}
         Writeln(CRCFile,crc);
{$endif Test_Double_checksum_write}
         if crc_index2<crc_array_size then
          inc(crc_index2);
       end
     else
       begin
         if (crcindex2<crc_array_size) and (crcindex2<crc_index2) and
            (crc_test2^[crcindex2]<>crc) then
           Do_comment(V_Note,'impl CRC changed');
{$ifdef Test_Double_checksum_write}
         Writeln(CRCFile,crc);
{$endif Test_Double_checksum_write}
         inc(crcindex2);
       end;
{$endif def Test_Double_checksum}
     if do_interface_crc then
       begin
         interface_crc:=UpdateCrc32(interface_crc,b,len);
{$ifdef Test_Double_checksum}
        if crc_only then
          begin
            crc_test^[crc_index]:=interface_crc;
{$ifdef Test_Double_checksum_write}
            Writeln(CRCFile,interface_crc);
{$endif Test_Double_checksum_write}
            if crc_index<crc_array_size then
             inc(crc_index);
          end
        else
          begin
            if (crcindex<crc_array_size) and (crcindex<crc_index) and
               (crc_test^[crcindex]<>interface_crc) then
              Do_comment(V_Warning,'CRC changed');
{$ifdef Test_Double_checksum_write}
            Writeln(CRCFile,interface_crc);
{$endif Test_Double_checksum_write}
            inc(crcindex);
          end;
{$endif def Test_Double_checksum}
         { indirect crc must only be calculated for the interface; changes
           to a class in the implementation cannot require another unit to
           be recompiled }
         if do_indirect_crc then
           indirect_crc:=UpdateCrc32(indirect_crc,b,len);
       end;
    end;
  inherited putdata(b,len);
  (*if not crc_only then
    writedata(b,len);
  inc(entryidx,len);*)
end;

function tppufile.getheadersize: longint;
begin
  result:=sizeof(header);
end;

function tppufile.getheaderaddr: pentryheader;
begin
  result:=@header;
end;

procedure tppufile.resetfile;
begin
  crc:=0;
  interface_crc:=0;
  indirect_crc:=0;
  do_interface_crc:=true;
  do_indirect_crc:=false;
  do_crc:=true;
end;


end.
