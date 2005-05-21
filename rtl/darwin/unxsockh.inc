{
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

        SOL_SOCKET = $FFFF;
        SO_DEBUG         =$0001;        { turn on debugging info recording }
        SO_ACCEPTCONN    =$0002;        { socket has had listen() }
        SO_REUSEADDR     =$0004;        { allow local address reuse }
        SO_KEEPALIVE     =$0008;        { keep connections alive }
        SO_DONTROUTE     =$0010;        { just use interface addresses }
        SO_BROADCAST     =$0020;        { permit sending of broadcast msgs }
        SO_USELOOPBACK   =$0040;        { bypass hardware when possible }
        SO_LINGER        =$0080;        { linger on close if data present }
        SO_OOBINLINE     =$0100;        { leave received OOB data in line }
        SO_REUSEPORT     =$0200;        { allow local address & port reuse }
        SO_TIMESTAMP     =$0400;        { timestamp received dgram traffic }

{
 * Additional options, not kept in so_options.
 }
        SO_SNDBUF        =$1001;        { send buffer size }
        SO_RCVBUF        =$1002;        { receive buffer size }
        SO_SNDLOWAT      =$1003;        { send low-water mark }
        SO_RCVLOWAT      =$1004;        { receive low-water mark }
        SO_SNDTIMEO      =$1005;        { send timeout }
        SO_RCVTIMEO      =$1006;        { receive timeout }
        SO_ERROR         =$1007;        { get error status and clear }
        SO_TYPE          =$1008;        { get socket type }


        SHUT_RD         =0;             { shut down the reading side }
        SHUT_WR         =1;             { shut down the writing side }
        SHUT_RDWR       =2;             { shut down both sides }


{
   $Log: unxsockh.inc,v $
   Revision 1.2  2005/02/14 17:13:22  peter
     * truncate log

}


