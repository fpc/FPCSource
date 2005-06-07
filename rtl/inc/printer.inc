{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by the Free Pascal development team

    Common part of implementation for unit Printer.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$I-}

var
  Old_Exit: pointer;
  LstAvailable: boolean;

function IsLstAvailable: boolean;
begin
  IsLstAvailable := LstAvailable;
end;

procedure Printer_Exit;
begin
  if LstAvailable then
    Close (Lst);
  ExitProc := Old_Exit;
end;

procedure InitPrinter (const PrinterName: string);
var
  OldInOutRes: word;
begin
(* Avoid potential problems with previous InOutRes value... *)
  OldInOutRes := InOutRes;
  InOutRes := 0;
  Assign (Lst, PrinterName);
  Rewrite (Lst);
  LstAvailable := InOutRes = 0;
  InOutRes := OldInOutRes;
end;

procedure SetPrinterExit;
begin
  Old_Exit := ExitProc;
  ExitProc := @Printer_Exit;
end;

(* The default $I state is left for potential
   platform-specific part of implementation.  *)
{$I+}

