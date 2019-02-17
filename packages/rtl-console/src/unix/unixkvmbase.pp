{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Miscellaneous routines used by the Keyboard, Mouse and
    Video units on Unix-like operating systems.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit UnixKvmBase;

{*****************************************************************************}
                                   interface
{*****************************************************************************}

function UTF8Enabled: Boolean;

{*****************************************************************************}
                                implementation
{*****************************************************************************}

uses
  baseunix;

function UTF8Enabled: Boolean;
var
  lang:string;
begin
  {$ifdef BEOS}
  UTF8Enabled := true;
  exit;
  {$endif}
  lang:=upcase(fpgetenv('LANG'));
  UTF8Enabled := (Pos('.UTF-8', lang) > 0) or (Pos('.UTF8', lang) > 0);
end;

end.
