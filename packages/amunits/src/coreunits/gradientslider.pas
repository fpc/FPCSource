{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit gradientslider;
{$ENDIF FPC_DOTTEDUNITS}

INTERFACE

{$IFDEF FPC_DOTTEDUNITS}
uses Amiga.Core.Utility;
{$ELSE FPC_DOTTEDUNITS}
uses utility;
{$ENDIF FPC_DOTTEDUNITS}


Const
    GRAD_Dummy       = (TAG_USER+$05000000);
    GRAD_MaxVal      = (GRAD_Dummy+1);     {  max value of slider         }
    GRAD_CurVal      = (GRAD_Dummy+2);     {  current value of slider     }
    GRAD_SkipVal     = (GRAD_Dummy+3);     {  "body click" move amount    }
    GRAD_KnobPixels  = (GRAD_Dummy+4);     {  size of knob in pixels      }
    GRAD_PenArray    = (GRAD_Dummy+5);     {  pen colors                  }


IMPLEMENTATION

end.
