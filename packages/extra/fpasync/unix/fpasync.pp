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
  Revision 1.1  2002-01-29 17:55:02  peter
    * splitted to base and extra

  Revision 1.2  2001/12/11 17:45:28  marco
   * was only commited to fixes.

  Revision 1.1.2.2  2001/11/16 12:51:41  sg
  * Now different handlers for available data and space in write buffer can
    be set
  * LOTS of bugfixes in the implementation
  * fpAsync now has a write buffer class (a read buffer class for reading
    line by line will be included in the next release)

  Revision 1.1.2.1  2001/09/08 15:43:24  sg
  * First public version

}
