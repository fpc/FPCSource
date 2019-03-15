{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Sven Barth, member of the Free Pascal development
    team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF VER3_0}
  {$I openbsd_ident.inc}
{$ENDIF VER3_0}

procedure PascalMain; external name 'PASCALMAIN';

var
  operatingsystem_result: longint; external name 'operatingsystem_result';
