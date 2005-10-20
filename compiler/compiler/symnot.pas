{
    Copyright (c) 2002 by Daniel Mantione

    This unit contains support routines for the variable access
    notifier.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

unit symnot;

{$i fpcdefs.inc}

interface

uses  cclasses,symtype;

type  Tnotification_flag=(vn_onread,vn_onwrite,vn_unknown);
      Tnotification_flags=set of Tnotification_flag;

      Tnotification_callback=procedure(not_type:Tnotification_flag;
                                       symbol:Tsym) of object;

      Tnotification=class(Tlinkedlistitem)
        flags:Tnotification_flags;
        callback:Tnotification_callback;
        id:cardinal;
        constructor create(Aflags:Tnotification_flags;
                           Acallback:Tnotification_callback);
      end;

implementation

var notification_counter:cardinal;

constructor Tnotification.create(Aflags:Tnotification_flags;
                                 Acallback:Tnotification_callback);

begin
  inherited create;
  flags:=Aflags;
  callback:=Acallback;
  id:=notification_counter;
  inc(notification_counter);
end;

begin
  notification_counter:=0;
end.
