{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef HASINTF}

type
   IReadWriteSync = interface
     ['{7B108C52-1D8F-4CDB-9CDF-57E071193D3F}']
     procedure BeginRead;
     procedure EndRead;
     function BeginWrite : boolean;
     procedure EndWrite;
   end;

{$endif HASINTF}

function InterLockedIncrement (var Target: longint) : longint;
function InterLockedDecrement (var Target: longint) : longint;
function InterLockedExchange (var Target: longint;Source : longint) : longint;
function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;

{
  $Log$
  Revision 1.3  2005-02-06 20:31:35  florian
    + GlobalNameSpace

  Revision 1.2  2004/03/05 12:17:50  marco
   * interlocked* changed to longints, including winapi. (which was a bug)

  Revision 1.1  2003/10/06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.3  2002/09/07 16:01:22  peter
    * old logs removed and tabs fixed

}
