{
    $Id$
    This file is part of the Free Pascal run time library.
    This unit contains the record definition for the Win32 API
    Copyright (c) 1999-2000 by Florian KLaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit windows;

{$ifndef NO_SMART_LINK}
{$smartlink on}
{$endif}

{$calling stdcall}

interface

{$define read_interface}
{$undef read_implementation}

{$i base.inc}
{$i errors.inc}
{$i defines.inc}
{$i struct.inc}
{$i messages.inc}
{$i ascfun.inc}
{$i unifun.inc}
{$ifdef UNICODE}
{$i unidef.inc}
{$else not UNICODE}
{$i ascdef.inc}
{$endif UNICODE}
{$i func.inc}
{$i redef.inc}

implementation

{$undef read_interface}
{$define read_implementation}

{$i base.inc}
{$i errors.inc}
{$i defines.inc}
{$i struct.inc}
{$i messages.inc}
{$i ascfun.inc}
{$i unifun.inc}
{$ifdef UNICODE}
{$i unidef.inc}
{$else not UNICODE}
{$i ascdef.inc}
{$endif UNICODE}
{$i func.inc}
{$i redef.inc}

end.
{
  $Log$
  Revision 1.6  2003-09-17 15:06:36  peter
    * stdcall patch

  Revision 1.5  2002/11/04 12:19:01  marco
   * Move tmsg and family to messages.inc. windows.pp needed include sequence patch

  Revision 1.4  2002/10/10 14:58:16  florian
    - removed conditionals for 0.99.14

  Revision 1.3  2002/09/07 16:01:29  peter
    * old logs removed and tabs fixed

}
