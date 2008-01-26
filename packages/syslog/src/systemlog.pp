{
    This file is part of the Free Pascal packages.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    This unit implements an interface to the Linux system logger.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit systemlog;

{$mode objfpc}
{$linklib c}


  interface

  { C default packing is dword }

{$PACKRECORDS 4}

  const
     _PATH_LOG = '/dev/log';

     LOG_EMERG = 0;
     LOG_ALERT = 1;
     LOG_CRIT = 2;
     LOG_ERR = 3;
     LOG_WARNING = 4;
     LOG_NOTICE = 5;
     LOG_INFO = 6;
     LOG_DEBUG = 7;
     LOG_PRIMASK = $07;

     LOG_KERN = 0 shl 3;
     LOG_USER = 1 shl 3;
     LOG_MAIL = 2 shl 3;
     LOG_DAEMON = 3 shl 3;
     LOG_AUTH = 4 shl 3;
     LOG_SYSLOG = 5 shl 3;
     LOG_LPR = 6 shl 3;
     LOG_NEWS = 7 shl 3;
     LOG_UUCP = 8 shl 3;
     LOG_CRON = 9 shl 3;
     LOG_AUTHPRIV = 10 shl 3;
     LOG_FTP = 11 shl 3;
     LOG_LOCAL0 = 16 shl 3;
     LOG_LOCAL1 = 17 shl 3;
     LOG_LOCAL2 = 18 shl 3;
     LOG_LOCAL3 = 19 shl 3;
     LOG_LOCAL4 = 20 shl 3;
     LOG_LOCAL5 = 21 shl 3;
     LOG_LOCAL6 = 22 shl 3;
     LOG_LOCAL7 = 23 shl 3;
     LOG_NFACILITIES = 24;
     LOG_FACMASK = $03f8;
     INTERNAL_NOPRI = $10;
    INTERNAL_MARK = LOG_NFACILITIES shl 3;
  type

     CODE = record
         name : string[10];
         val : longint;
       end;
const
  prioritynames : Array [1..12] of code =
  (   (name : 'alert'; val : LOG_ALERT ),
     ( name : 'crit'; val  : LOG_CRIT ),
     ( name : 'debug'; val : LOG_DEBUG ),
     ( name : 'emerg'; val : LOG_EMERG ),
     ( name : 'err';val :  LOG_ERR ),
     ( name : 'error'; val :  LOG_ERR ),
     ( name : 'info';val : LOG_INFO ),
     ( name : 'none'; val : INTERNAL_NOPRI ),
     ( name : 'notice'; val  :  LOG_NOTICE ),
     ( name : 'panic'; val : LOG_EMERG ),
     ( name : 'warn'; val : LOG_WARNING ),
     ( name : 'warning'; val : LOG_WARNING )
);

const

   facilitynames : array [1..22] of code =
    (
      ( name : 'auth'; val :  LOG_AUTH ),
      ( name : 'authpriv'; val :  LOG_AUTHPRIV ),
      ( name : 'cron'; val :  LOG_CRON ),
      ( name : 'daemon'; val :  LOG_DAEMON ),
      ( name : 'ftp'; val :  LOG_FTP ),
      ( name : 'kern'; val :  LOG_KERN ),
      ( name : 'lpr'; val :  LOG_LPR ),
      ( name : 'mail'; val :  LOG_MAIL ),
      ( name : 'mark'; val :  INTERNAL_MARK ),
      ( name : 'news'; val :  LOG_NEWS ),
      ( name : 'security'; val :  LOG_AUTH ),
      ( name : 'syslog'; val :  LOG_SYSLOG ),
      ( name : 'user'; val :  LOG_USER ),
      ( name : 'uucp'; val :  LOG_UUCP ),
      ( name : 'local0'; val :  LOG_LOCAL0 ),
      ( name : 'local1'; val :  LOG_LOCAL1 ),
      ( name : 'local2'; val :  LOG_LOCAL2 ),
      ( name : 'local3'; val :  LOG_LOCAL3 ),
      ( name : 'local4'; val :  LOG_LOCAL4 ),
      ( name : 'local5'; val :  LOG_LOCAL5 ),
      ( name : 'local6'; val :  LOG_LOCAL6 ),
      ( name : 'local7'; val :  LOG_LOCAL7 )
    );

  const
     LOG_PID = $01;
     LOG_CONS = $02;
     LOG_ODELAY = $04;
     LOG_NDELAY = $08;
     LOG_NOWAIT = $10;
     LOG_PERROR = $20;


  function LOG_PRI(p : longint) : longint;
  function LOG_MAKEPRI(fac,pri : longint) : longint;
  function LOG_FAC(p : longint) : longint;
  function LOG_MASK(pri : longint) : longint;
  function LOG_UPTO(pri : longint) : longint;

  procedure closelog;cdecl;external;
  procedure openlog(__ident:pchar; __option:longint; __facilit:longint);cdecl;external;
  function setlogmask(__mask:longint):longint;cdecl;external;
  procedure syslog(__pri:longint; __fmt:pchar; args:array of const);cdecl;external;
//   procedure vsyslog(__pri:longint; __fmt:pchar; __ap:_BSD_VA_LIST_);cdecl;external;


  implementation

  function LOG_PRI(p : longint) : longint;
    begin
       LOG_PRI:=p and LOG_PRIMASK;
    end;

  function LOG_MAKEPRI(fac,pri : longint) : longint;
    begin
       LOG_MAKEPRI:=(fac shl 3) or pri;
    end;


  function LOG_FAC(p : longint) : longint;
    begin
       LOG_FAC:=(p and (LOG_FACMASK)) shr 3;
    end;

  function LOG_MASK(pri : longint) : longint;
    begin
       LOG_MASK:=1 shl pri;
    end;

  function LOG_UPTO(pri : longint) : longint;
    begin
       LOG_UPTO:=(1 shl (pri + 1)) - 1;
    end;

end.
