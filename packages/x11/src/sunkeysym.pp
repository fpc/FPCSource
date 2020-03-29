(*
 * Copyright (c) 1991, Oracle and/or its affiliates. All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)
(************************************************************

Copyright 1991, 1998  The Open Group

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of The Open Group shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from The Open Group.

***********************************************************)

unit sunkeysym;

interface

{*
 * Floating Accent
 *}
const
  SunXK_FA_Grave     = $1005FF00;
  SunXK_FA_Circum    = $1005FF01;
  SunXK_FA_Tilde     = $1005FF02;
  SunXK_FA_Acute     = $1005FF03;
  SunXK_FA_Diaeresis = $1005FF04;
  SunXK_FA_Cedilla   = $1005FF05;

{*
 * Miscellaneous Functions
 *}

  SunXK_F36 = $1005FF10;   { Labeled F11 }
  SunXK_F37 = $1005FF11;   { Labeled F12 }

  SunXK_Sys_Req      = $1005FF60;
  SunXK_Print_Screen = $0000FF61;   { Same as XK_Print }

{*
 * International & Multi-Key Character Composition
 *}

  SunXK_Compose  = $0000FF20;   { Same as XK_Multi_key }
  SunXK_AltGraph = $0000FF7E;   { Same as XK_Mode_switch }

{*
 * Cursor Control
 *}

  SunXK_PageUp   = $0000FF55;   { Same as XK_Prior }
  SunXK_PageDown = $0000FF56;   { Same as XK_Next }

{*
 * Open Look Functions
 *}

  SunXK_Undo  = $0000FF65;   { Same as XK_Undo }
  SunXK_Again = $0000FF66;   { Same as XK_Redo }
  SunXK_Find  = $0000FF68;   { Same as XK_Find }
  SunXK_Stop  = $0000FF69;   { Same as XK_Cancel }
  SunXK_Props = $1005FF70;
  SunXK_Front = $1005FF71;
  SunXK_Copy  = $1005FF72;
  SunXK_Open  = $1005FF73;
  SunXK_Paste = $1005FF74;
  SunXK_Cut   = $1005FF75;

  SunXK_PowerSwitch          = $1005FF76;
  SunXK_AudioLowerVolume     = $1005FF77;
  SunXK_AudioMute            = $1005FF78;
  SunXK_AudioRaiseVolume     = $1005FF79;
  SunXK_VideoDegauss         = $1005FF7A;
  SunXK_VideoLowerBrightness = $1005FF7B;
  SunXK_VideoRaiseBrightness = $1005FF7C;
  SunXK_PowerSwitchShift     = $1005FF7D;

implementation
end.
