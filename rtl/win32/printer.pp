{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Printer unit for BP7 compatible RTL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit printer;
interface

{$I printerh.inc}

implementation

{$I printer.inc}

begin
  InitPrinter ('PRN');
  SetPrinterExit;
end.
{
  $Log$
  Revision 1.4  2004-12-05 11:21:46  hajny
    * common implementation of unit printer - fix for bug 3421

  Revision 1.3  2002/09/07 16:01:29  peter
    * old logs removed and tabs fixed

}
