{  
   $Id$
   This file is part of the Free Pascal run time library.
   (c) 2000-2003 by Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.
   
   OS dependant part of the header.   

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

Const
  AF_AX25         = 3;      { Amateur Radio AX.25          }
  AF_IPX          = 4;      { Novell IPX                   }
  AF_APPLETALK    = 5;      { Appletalk DDP                }
  AF_NETROM       = 6;      { Amateur radio NetROM         }
  AF_BRIDGE       = 7;       { Multiprotocol bridge         }
  AF_AAL5         = 8;       { Reserved for Werner's ATM    }
  AF_X25          = 9;       { Reserved for X.25 project    }
  AF_INET6        = 10;      { IP version 6                 }
  AF_MAX          = 12;

  SOCK_PACKET     = 10;

  PF_AX25         = AF_AX25;
  PF_IPX          = AF_IPX;
  PF_APPLETALK    = AF_APPLETALK;
  PF_NETROM       = AF_NETROM;
  PF_BRIDGE       = AF_BRIDGE;
  PF_AAL5         = AF_AAL5;
  PF_X25          = AF_X25;
  PF_INET6        = AF_INET6;

  PF_MAX          = AF_MAX;

{
   $Log$
   Revision 1.1  2004-03-09 19:40:52  marco
    * header for sockets unit

}


