{
   $Id$
   VESA-Textmode support for the DOS version of the FPC API

   Copyright (c) 1999 by Florian Klaempfl

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.


   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}
unit vesamode;

  interface

  implementation

    uses
       dos,go32,dpmiexcp,video,mouse;

    type
       twordarray = array[0..0] of word;

       pwordarray = ^twordarray;
       TVESAInfoBlock = record
	 VESASignature   : ARRAY[0..3] OF CHAR;
	 VESAVersion     : WORD;
	 OEMStringPtr    : PChar;
	 Capabilities    : LONGINT;
	 VideoModePtr    : pwordarray;
	 TotalMemory     : WORD;
	 Reserved        : ARRAY[1..242] OF BYTE;
       end;

    function ReturnSuperVGAInfo(var ib : TVESAInfoBLock) : Word;

      var
	 regs : registers;

      begin
	 regs.ah:=$4f;
	 regs.al:=0;
	 regs.es:=tb_segment;
	 regs.di:=tb_offset;
	 intr($10,regs);
	 dosmemget(tb_segment,tb_offset,ib,sizeof(ib));
	 ReturnSuperVGAInfo:=regs.ax;
      end;

    function SetSuperVGAMode(m : word) : word;

      var
	 regs : registers;

      begin
	 regs.ah:=$4f;
	 regs.al:=2;
	 regs.bx:=m;
	 intr($10,regs);
	 SetSuperVGAMode:=regs.ax;
      end;

    function SetVESAMode(const VideoMode: TVideoMode; Params: Longint): Boolean;

      var
	 w : word;

      begin
	 w:=SetSuperVGAMode(Params);
         if w<>$4f then
	   SetVESAMode:=false
	 else
           begin
              SetVESAMode:=true;
              ScreenWidth:=VideoMode.Col;
              ScreenHeight:=VideoMode.Row;
              ScreenColor:=true;
              // cheat to get a correct mouse
              {
              mem[$40:$84]:=ScreenHeight-1;
              mem[$40:$4a]:=ScreenWidth;
              memw[$40:$4c]:=ScreenHeight*((ScreenWidth shl 1)-1);
              }
              DoCustomMouse(true);
           end;
      end;

var
   infoblock : TVESAInfoBLock;
   i : longint;
   m : word;

begin
   ReturnSuperVGAInfo(infoblock);
   if not((infoblock.VESASignature[0]<>'V') or
      (infoblock.VESASignature[1]<>'E') or
      (infoblock.VESASignature[2]<>'S') or
      (infoblock.VESASignature[3]<>'A')) then
     begin
{$R-}
   i:=0;
   while true do
     begin
	dosmemget(hi(dword(infoblock.VideoModePtr)),lo(dword(infoblock.VideoModePtr))+i*2,m,2);
	case m of
           264:
             RegisterVideoMode(80,60,true,@SetVESAMode,264);
           265:
             RegisterVideoMode(132,25,true,@SetVESAMode,265);
           266:
             RegisterVideoMode(132,43,true,@SetVESAMode,266);
           267:
             RegisterVideoMode(132,50,true,@SetVESAMode,267);
           268:
             RegisterVideoMode(132,60,true,@SetVESAMode,268);
	   $ffff:
	     break;
	end;
	inc(i);
     end;
   end;
end.
{
  $Log$
  Revision 1.3  2000-02-07 22:54:44  florian
    * custommouse define removed, i.e. code is always active
    * the xor value for the mouse cursor must be $7f instead of $ff

  Revision 1.2  2000/02/06 14:29:45  florian
    * mouse support for vesa resolutions under go32v2, needs currently the define
      custommouse

  Revision 1.1  2000/01/06 01:20:30  peter
    * moved out of packages/ back to topdir

  Revision 1.2  1999/12/23 22:37:38  pierre
    * Use @SetVesaMode for normal FPC syntax
    * variable I was not initialized in unit initialization!!

  Revision 1.1  1999/11/24 23:36:38  peter
    * moved to packages dir

  Revision 1.2  1999/03/14 17:43:02  florian
    + 80x50 mode support added
    * some bugs in VESA mode support removed

  Revision 1.1  1999/03/13 17:29:39  florian
    + first implementation for VESA 1.x, only standard modes are supported

}