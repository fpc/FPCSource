{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl

    This file implements the win32 gui support for the graph unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


  var
    { this procedure allows to hook keyboard messages }
    charmessagehandler : function(Window: hwnd; AMessage, WParam,
                                  LParam: Longint): Longint;
    { this procedure allows to hook mouse messages }
    mousemessagehandler : function(Window: hwnd; AMessage, WParam,
                                   LParam: Longint): Longint;
   mainwindow : HWnd;

  const
    { predefined window style }
    { we shouldn't set CS_DBLCLKS here }
    { because most dos applications    }
    { handle double clicks on it's own }
    graphwindowstyle : DWord = cs_hRedraw or cs_vRedraw;

    windowtitle : pchar = 'Graph window application';

CONST

  m640x200x16       = VGALo;
  m640x400x16       = VGAMed;
  m640x480x16       = VGAHi;

  { VESA Specific video modes. }
  m320x200x32k      = $10D;
  m320x200x64k      = $10E;

  m640x400x256      = $100;

  m640x480x256      = $101;
  m640x480x32k      = $110;
  m640x480x64k      = $111;

  m800x600x16       = $102;
  m800x600x256      = $103;
  m800x600x32k      = $113;
  m800x600x64k      = $114;

  m1024x768x16      = $104;
  m1024x768x256     = $105;
  m1024x768x32k     = $116;
  m1024x768x64k     = $117;

  m1280x1024x16     = $106;
  m1280x1024x256    = $107;
  m1280x1024x32k    = $119;
  m1280x1024x64k    = $11A;


{
  $Log$
  Revision 1.5  2000-03-05 13:07:04  florian
    * the title can be user defined

  Revision 1.4  2000/01/07 16:41:52  daniel
    * copyright 2000

  Revision 1.3  1999/12/08 09:09:34  pierre
   + add VESA compatible mode in 16 and 256 colors

  Revision 1.2  1999/11/29 22:03:39  florian
    * first implementation of winmouse unit

  Revision 1.1  1999/11/08 15:01:39  peter
    * fpcmake support
}
