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
interface

{$define read_interface}
{$undef read_implementation}

{$i base.inc}
{$i errors.inc}
{$i defines.inc}
{$i messages.inc}
{$i struct.inc}
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
{$i messages.inc}
{$i struct.inc}
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
  Revision 1.12  2000-06-11 07:04:58  peter
    * Windows unit has now more Delphi compatibile functions
    * placed the external functions only in the interface

  Revision 1.11  2000/05/17 11:00:29  marco
   * JCL alias

  Revision 1.10  2000/04/22 19:51:08  marco
   * Forgot to remove the tpoint stuff.

  Revision 1.9  2000/04/22 17:46:05  marco
   * some redef fixes (for lazarus mwedit component)

  Revision 1.8  2000/03/19 20:30:27  marco
   * Some more delphi compatible kernelfunc headers for JCL.

  Revision 1.7  2000/02/09 16:59:35  peter
    * truncated log

  Revision 1.6  2000/01/07 16:41:53  daniel
    * copyright 2000

  Revision 1.5  2000/01/07 16:32:35  daniel
    * copyright 2000 added

  Revision 1.4  1999/09/16 13:38:21  peter
    * windows unit include moved to wininc/

}
