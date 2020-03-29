{
    This file is part of the Free Pascal run time library.
    and implements some stuff for protected mode programming
    Copyright (c) 1999-2000 by the Free Pascal development team.

    These files adds support for TP styled port accesses

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit ports;

interface

{ Since this platform has port access built into the System unit, this unit just
  creates aliases, for compatibility for programs, that already use the ports
  unit.  }

type
  tport = System.tport;
  tportw = System.tportw;
  tportl = System.tportl;

var
   port  : tport absolute System.port;
   portb : tport absolute System.portb;
   portw : tportw absolute System.portw;
   portl : tportl absolute System.portl;

implementation

end.
