(*

Copyright 1987, 1998  The Open Group

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation.

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of The Open Group shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from The Open Group.

Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the names of Hewlett Packard
or Digital not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE.  Hewlett-Packard shall not be liable for errors
contained herein or direct, indirect, special, incidental or
consequential damages in connection with the furnishing,
performance, or use of this material.

*)

unit hpkeysym;

interface

const
  hpXK_ClearLine        = $1000FF6F;
  hpXK_InsertLine       = $1000FF70;
  hpXK_DeleteLine       = $1000FF71;
  hpXK_InsertChar       = $1000FF72;
  hpXK_DeleteChar       = $1000FF73;
  hpXK_BackTab          = $1000FF74;
  hpXK_KP_BackTab       = $1000FF75;
  hpXK_Modelock1        = $1000FF48;
  hpXK_Modelock2        = $1000FF49;
  hpXK_Reset            = $1000FF6C;
  hpXK_System           = $1000FF6D;
  hpXK_User             = $1000FF6E;
  hpXK_mute_acute       = $100000A8;
  hpXK_mute_grave       = $100000A9;
  hpXK_mute_asciicircum = $100000AA;
  hpXK_mute_diaeresis   = $100000AB;
  hpXK_mute_asciitilde  = $100000AC;
  hpXK_lira             = $100000AF;
  hpXK_guilder          = $100000BE;
  hpXK_Ydiaeresis       = $100000EE;
  hpXK_IO               = $100000EE;
  hpXK_longminus        = $100000F6;
  hpXK_block            = $100000FC;


//#ifndef _OSF_Keysyms
//#define _OSF_Keysyms

  osfXK_Copy         = $1004FF02;
  osfXK_Cut          = $1004FF03;
  osfXK_Paste        = $1004FF04;
  osfXK_BackTab      = $1004FF07;
  osfXK_BackSpace    = $1004FF08;
  osfXK_Clear        = $1004FF0B;
  osfXK_Escape       = $1004FF1B;
  osfXK_AddMode      = $1004FF31;
  osfXK_PrimaryPaste = $1004FF32;
  osfXK_QuickPaste   = $1004FF33;
  osfXK_PageLeft     = $1004FF40;
  osfXK_PageUp       = $1004FF41;
  osfXK_PageDown     = $1004FF42;
  osfXK_PageRight    = $1004FF43;
  osfXK_Activate     = $1004FF44;
  osfXK_MenuBar      = $1004FF45;
  osfXK_Left         = $1004FF51;
  osfXK_Up           = $1004FF52;
  osfXK_Right        = $1004FF53;
  osfXK_Down         = $1004FF54;
  osfXK_EndLine      = $1004FF57;
  osfXK_BeginLine    = $1004FF58;
  osfXK_EndData      = $1004FF59;
  osfXK_BeginData    = $1004FF5A;
  osfXK_PrevMenu     = $1004FF5B;
  osfXK_NextMenu     = $1004FF5C;
  osfXK_PrevField    = $1004FF5D;
  osfXK_NextField    = $1004FF5E;
  osfXK_Select       = $1004FF60;
  osfXK_Insert       = $1004FF63;
  osfXK_Undo         = $1004FF65;
  osfXK_Menu         = $1004FF67;
  osfXK_Cancel       = $1004FF69;
  osfXK_Help         = $1004FF6A;
  osfXK_SelectAll    = $1004FF71;
  osfXK_DeselectAll  = $1004FF72;
  osfXK_Reselect     = $1004FF73;
  osfXK_Extend       = $1004FF74;
  osfXK_Restore      = $1004FF78;
  osfXK_Delete       = $1004FFFF;

//#endif /* _OSF_Keysyms */


(**************************************************************
 * The use of the following macros is deprecated.
 * They are listed below only for backwards compatibility.
 *)
  XK_Reset            = $1000FF6C;
  XK_System           = $1000FF6D;
  XK_User             = $1000FF6E;
  XK_ClearLine        = $1000FF6F;
  XK_InsertLine       = $1000FF70;
  XK_DeleteLine       = $1000FF71;
  XK_InsertChar       = $1000FF72;
  XK_DeleteChar       = $1000FF73;
  XK_BackTab          = $1000FF74;
  XK_KP_BackTab       = $1000FF75;
  XK_Ext16bit_L       = $1000FF76;
  XK_Ext16bit_R       = $1000FF77;
  XK_mute_acute       = $100000a8;
  XK_mute_grave       = $100000a9;
  XK_mute_asciicircum = $100000aa;
  XK_mute_diaeresis   = $100000ab;
  XK_mute_asciitilde  = $100000ac;
  XK_lira             = $100000af;
  XK_guilder          = $100000be;
//#ifndef XK_Ydiaeresis
  XK_Ydiaeresis       = $100000ee;
//#endif
  XK_IO               = $100000ee;
  XK_longminus        = $100000f6;
  XK_block            = $100000fc;

implementation
end.
