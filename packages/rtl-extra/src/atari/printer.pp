{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Printer unit for BP7/PurePascal compatible RTL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit printer;
{$ENDIF FPC_DOTTEDUNITS}
interface

{$I printerh.inc}

implementation

{$I printer.inc}

begin
  (* WARNING: has to be checked; do_open('PRN') returns a valid, negative OS handle *)
  InitPrinter ('PRN:');
  SetPrinterExit;
end.
