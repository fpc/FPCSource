{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 by Florian Klaempfl

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
    graphwindowstyle : DWord = cs_hRedraw or cs_vRedraw;
 

{
  $Log$
  Revision 1.2  1999-11-29 22:03:39  florian
    * first implementation of winmouse unit

  Revision 1.1  1999/11/08 15:01:39  peter
    * fpcmake support
}
