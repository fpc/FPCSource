unit shlwapi;

{
    This file is part of the Free Pascal run time library.
    shlwapi calls are parked here for now.
    Copyright (c) 1999-2002 by Marco van de Voort,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ************************************************************************}

interface
{$mode delphi}

function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; stdcall; external 'shlwapi.dll';

implementation

end.