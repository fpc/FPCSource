{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Peter Vreman
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This file contains the termios interface.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit termio;
{$ENDIF FPC_DOTTEDUNITS}

interface
{$inline on}
{$IFDEF FPC_DOTTEDUNITS}
Uses UnixApi.Types,UnixApi.Base;          // load base UnixApi.Unix typing
{$ELSE FPC_DOTTEDUNITS}
Uses UnixType,BaseUnix;          // load base unix typing
{$ENDIF FPC_DOTTEDUNITS}

// load types + consts

{$i termios.inc}

// load default prototypes from unix dir.

{$i termiosh.inc}

implementation

// load implementation for prototypes from current dir.
{$i termiosproc.inc}

// load ttyname from unix dir.
{$i ttyname.inc}

end.
