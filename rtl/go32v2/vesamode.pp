{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Video unit extension for VESA Modes for go32v2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
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
