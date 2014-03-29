{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by the Free Pascal development team

    Common header for unit Printer.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
var
  Lst: text;

(* Check whether opening of Lst was successful. *)
function IsLstAvailable: boolean;

(* Allow to initialize printer with different name. *)
procedure InitPrinter (const PrinterName: string);

