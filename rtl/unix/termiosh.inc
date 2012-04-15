{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    Termios basic prototypes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ***********************************************************************}

{ SunOS provides these functions directly }
{$if not defined(SunOS) and not defined(aix)}
Function  TCGetAttr   (fd:cint;var tios:TermIOS):cint; inline;
Function  TCSetAttr   (fd:cint;OptAct:cint;const tios:TermIOS):cint;
Procedure CFSetISpeed (var tios:TermIOS;speed:Cardinal); inline;
Procedure CFSetOSpeed (var tios:TermIOS;speed:Cardinal); inline;
{$endif SunOS or aix}
Procedure CFMakeRaw   (var tios:TermIOS);
Function  TCSendBreak (fd,duration:cint):cint; inline;
Function  TCSetPGrp   (fd,id:cint)  :cint;  inline;

Function  TCGetPGrp   (fd:cint;var id:cint):cint; inline;
Function  TCFlush     (fd,qsel:cint):cint; inline;
Function  TCDrain     (fd:cint)     :cint;inline;
Function  TCFlow      (fd,act:cint) :cint;inline;
Function  IsATTY      (Handle:cint) :cint;
Function  IsATTY      (var f:text)  :cint; inline;
function  TTYname     (Handle:cint):string;
function  TTYname     (var F:Text) :string;

