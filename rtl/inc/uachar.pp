{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2011 by the Free Pascal development team

    This unit redefines the Char type from widechar into ansichar

    See the file COPYING.FPC, included in this distribution,
    For details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit uachar;

{$h-} 

interface

  type
    Char = AnsiChar;
    PChar = PAnsiChar;
    PPChar = ^PChar;
    PPPChar = ^PPChar;

implementation

end.
