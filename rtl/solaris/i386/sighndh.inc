{
    $Id: sighndh.inc,v 1.2 2005/02/14 17:13:31 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    Sigcontext and Sigaction

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$packrecords C}

type
  PSigContext = ^TSigContext;
  TSigContext = record
  end;

{
  $Log: sighndh.inc,v $
  Revision 1.2  2005/02/14 17:13:31  peter
    * truncate log

  Revision 1.1  2005/02/13 22:13:20  peter
    * get solaris back in shape

  Revision 1.1  2005/01/30 18:01:15  peter
    * signal cleanup for linux
    * sigactionhandler instead of tsigaction for bsds
    * sigcontext moved to cpu dir

}

