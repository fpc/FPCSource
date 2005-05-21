{
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

       TVesaVideoMode = record
         {Col,Row      : word;
          Color        : boolean;}
         V            : TVideoMode;
         Mode         : word;
       end;

Const
  VesaVideoModeCount = 5;
  VesaVMD : Array[264..268] of TVesaVideoMode = (
   (V : (Col: 80; Row : 60;  Color : True); Mode : 264),
   (V : (Col: 132; Row : 25;  Color : True); Mode : 265),
   (V : (Col: 132; Row : 43;  Color : True); Mode : 266),
   (V : (Col: 132; Row : 50;  Color : True); Mode : 267),
   (V : (Col: 132; Row : 60;  Color : True); Mode : 268)
  );

var
   infoblock : TVESAInfoBLock;
   SupportedVesaVMD : Array[0..VesaVideoModeCount-1] of TVesaVideoMode;
   i : longint;
   m : word;
Var
  SysGetVideoModeCount : function : word;
  SysSetVideoMode : function (Const VideoMode : TVideoMode) : boolean;
  SysGetVideoModeData : Function (Index : Word; Var Data : TVideoMode) : boolean;


const
  VesaRegisteredModes : word = 0;

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

    function SetVESAMode(const VideoMode: TVideoMode): Boolean;

      var
         w : word;

      begin
         SetVESAMode:=false;
         for w:=VesaRegisteredModes-1 downto 0 do
           begin
             if (VideoMode.col=SupportedVesaVMD[w].v.col) and
                (VideoMode.row=SupportedVesaVMD[w].v.row) and
                (VideoMode.color=SupportedVesaVMD[w].v.color) then
               begin
                 if SetSuperVGAMode(SupportedVesaVMD[w].mode) <> $4f then
                   SetVESAMode:=false
                 else
                   begin
                      SetVESAMode:=true;
                      ScreenWidth:=VideoMode.Col;
                      ScreenHeight:=VideoMode.Row;
                      ScreenColor:=VideoMode.Color;
                      // cheat to get a correct mouse
                      {
                      mem[$40:$84]:=ScreenHeight-1;
                      mem[$40:$4a]:=ScreenWidth;
                      memw[$40:$4c]:=ScreenHeight*((ScreenWidth shl 1)-1);
                      }
                      DoCustomMouse(true);
                   end;
               end;
             if SetVESAMode then
               exit;
           end;
         SetVESAMode:=SysSetVideoMode(VideoMode);
      end;

procedure InitializeVesaModes;
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
             Begin
               {RegisterVideoMode(80,60,true,@SetVESAMode,264);}
               SupportedVesaVMD[VesaRegisteredModes]:=VesaVMD[m];
               Inc(VesaRegisteredModes);
             End;
           265:
             Begin
               {RegisterVideoMode(132,25,true,@SetVESAMode,265);}
               SupportedVesaVMD[VesaRegisteredModes]:=VesaVMD[m];
               Inc(VesaRegisteredModes);
             End;
           266:
             Begin
               {RegisterVideoMode(132,43,true,@SetVESAMode,266);}
               SupportedVesaVMD[VesaRegisteredModes]:=VesaVMD[m];
               Inc(VesaRegisteredModes);
             End;
           267:
             Begin
               {RegisterVideoMode(132,50,true,@SetVESAMode,267);}
               SupportedVesaVMD[VesaRegisteredModes]:=VesaVMD[m];
               Inc(VesaRegisteredModes);
             End;
           268:
             Begin
               {RegisterVideoMode(132,60,true,@SetVESAMode,268);}
               SupportedVesaVMD[VesaRegisteredModes]:=VesaVMD[m];
               Inc(VesaRegisteredModes);
             End;
           $ffff:
             break;
        end;
        inc(i);
     end;
   end;
end;


Function VesaGetVideoModeData (Index : Word; Var Data : TVideoMode) : boolean;
Var
  PrevCount : word;

begin
  PrevCount:=SysGetVideoModeCount();
  VesaGetVideoModeData:=(Index<=PrevCount);
  If VesaGetVideoModeData then
    begin
      SysGetVideoModeData(Index,Data);
      exit;
    end;
  VesaGetVideoModeData:=(Index-PrevCount)<=VesaRegisteredModes;
  If VesaGetVideoModeData then
    Data:=SupportedVesaVMD[Index-PrevCount-1].V;
end;

Function VesaGetVideoModeCount : Word;

begin
  VesaGetVideoModeCount:=SysGetVideoModeCount()+VesaRegisteredModes;
end;


Var
  Driver : TVideoDriver;
(*
Const
  SysVideoDriver : TVideoDriver = (
    InitDriver      : @SysInitVideo;
    DoneDriver      : @SysDoneVideo;
    UpdateScreen    : @SysUpdateScreen;
    ClearScreen     : Nil;
    SetVideoMode    : @SysSetVideoMode;
    GetVideoModeCount : @SysGetVideoModeCount;
    GetVideoModeData : @SysGetVideoModedata;
    SetCursorPos    : @SysSetCursorPos;
    GetCursorType   : @SysGetCursorType;
    SetCursorType   : @SysSetCursorType;
    GetCapabilities : @SysGetCapabilities
  );
*)
initialization

{ Get the videodriver to be used }
  GetVideoDriver (Driver);
  InitializeVesaModes;
{ Change needed functions }
  SysGetVideoModeCount:=Driver.GetVideoModeCount;
  Driver.GetVideoModeCount:=@VesaGetVideoModeCount;
  SysGetVideoModeData:=Driver.GetVideoModeData;
  Driver.GetVideoModeData:=@VesaGetVideoModeData;
  SysSetVideoMode:=Driver.SetVideoMode;
  Driver.SetVideoMode:=@SetVESAMode;

  SetVideoDriver (Driver);
end.
