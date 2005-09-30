{
    This file is part of the Free Pascal run time library.
    This unit contains the record definition for the Win32 API
    Copyright (c) 1999-2000 by Florian KLaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Changes :

  08-15-2005 : ORO06
    update for wince4.2 port
}

unit windows;

{$ifndef NO_SMART_LINK}
{$smartlink on}
{$endif}

{ stuff like array of const is used }
{$mode objfpc}
{$calling stdcall}

interface


{$define read_interface}
{$undef read_implementation}


{$ifdef UNDER_CE}
{$define UNICODE}  //ce is unicode only
//{$define _X86_}  //for testing compilation
{$calling cedcl}
{$endif UNDER_CE}

{$i base.inc}
{$i errors.inc}
{$i defines.inc}
{$i struct.inc}
{$i messages.inc}

{$ifndef UNDER_CE}
{$i ascfun.inc}
{$i unifun.inc}
{$endif UNDER_CE}

{$ifdef UNICODE}
{$i unidef.inc}
{$else not UNICODE}
{$i ascdef.inc}
{$endif UNICODE}

{$i func.inc}
{$i redef.inc}

{$ifdef UNDER_CE}
{$i aygshell.inc}
//{$i commctrl.inc}
{$endif UNDER_CE}

implementation

{$undef read_interface}
{$define read_implementation}

{$i base.inc}
{$i errors.inc}
{$i defines.inc}
{$i struct.inc}
{$i messages.inc}

{$ifndef UNDER_CE}
{$i ascfun.inc}
{$i unifun.inc}
{$endif UNDER_CE}

{$ifdef UNICODE}
{$i unidef.inc}
{$else not UNICODE}
{$i ascdef.inc}
{$endif UNICODE}

{$i func.inc}
{$i redef.inc}

{$ifdef UNDER_CE}
{$i aygshell.inc}
//{$i commctrl.inc}
{$endif UNDER_CE}

{$undef read_implementation}

end.
