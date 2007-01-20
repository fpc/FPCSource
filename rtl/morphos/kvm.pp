{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 Karoly Balogh
    member of the Free Pascal Development Team

    Keyboard/Video/Mouse helper unit for Amiga/MorphOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit kvm;

interface

uses
  exec, intuition, graphics;


function initKVM: boolean;
procedure doneKVM;



implementation


var
  kvmWindow: PWindow;

const
  DEFAULT_WINWIDTH  = 80;
  DEFAULT_WINHEIGHT = 25;

const
  CHAR_XSIZE = 8;
  CHAR_YSIZE = 16;




function initKVM: boolean;
begin
  initKVM:=false;
  kvmWindow:=OpenWindowTags(nil, [
      WA_Left,50,
      WA_Top, 50,
      WA_InnerWidth, DEFAULT_WINWIDTH *CHAR_XSIZE,
      WA_InnerHeight,DEFAULT_WINHEIGHT*CHAR_YSIZE,
      WA_IDCMP, IDCMP_VANILLAKEY or IDCMP_RAWKEY,
      WA_Title,DWord(PChar('Free Pascal Video Output')),
      WA_Flags,(WFLG_GIMMEZEROZERO or 
                WFLG_SMART_REFRESH or 
                WFLG_NOCAREREFRESH or 
                WFLG_ACTIVATE      or 
                WFLG_DRAGBAR       or 
                WFLG_DEPTHGADGET)
    ]);

  if kvmWindow<>nil then initKVM:=true;
end;


procedure doneKVM;
begin
  if kvmWindow <> nil then CloseWindow(kvmWindow);
end;


begin
  InitGraphicsLibrary;
  InitIntuitionLibrary;
end.
