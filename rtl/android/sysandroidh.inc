{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Yury Sidorov,
    member of the Free Pascal development team.

    Android-specific part of the System unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

// Returns an Android system property
function GetSystemProperty(Name: PAnsiChar): shortstring;
// Returns an Android API level of the current system
function SystemApiLevel: shortint;
