{%MainUnit ndk.pas}
{
    Native Development Kit for Native NT

    This file is part of the Free Pascal run time library.
    This unit contains RTL functions.
    Copyright (c) 2010 by Sven Barth

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function RtlTimeFieldsToTime(
  TimeFields: PTIME_FIELDS;
  Time: PLARGE_INTEGER
): NT_BOOLEAN; external ntdll;

procedure RtlTimeToTimeFields(
  Time: PLARGE_INTEGER;
  TimeFields: PTIME_FIELDS
); external ntdll;

