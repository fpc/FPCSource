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

uses libasync;

type

  TNotifyEvent = procedure(Sender: TObject) of object;

{$INCLUDE fpasynch.inc}


implementation


{$INCLUDE fpasync.inc}


end.


{
  $Log$
  Revision 1.1.2.1  2001-09-08 15:43:24  sg
  * First public version

}
