{
    $Id$

    fpAsync: Asynchronous event management for Free Pascal
    Copyright (C) 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Unix implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit fpAsync;

{$MODE objfpc}

interface

uses Classes, libasync;

type

  TNotifyEvent = procedure(Sender: TObject) of object;

{$INCLUDE fpasynch.inc}


implementation


{$INCLUDE fpasync.inc}


end.


{
  $Log$
  Revision 1.2  2002-09-07 15:42:57  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:55:02  peter
    * splitted to base and extra

}
