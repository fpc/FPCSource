{
    $Id$
    This file is part of the Free Pascal run time library.
    This unit contains the record definition for the Win32 API
    Copyright (c) 1999-2000 by Florian KLaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit windows;

{$define windows_include_files}

  interface

{$define read_interface}
{$undef read_implementation}

{$message starting interface of windows unit }

{$i base.inc}
{$i errors.inc}
{$i defines.inc}
{$i messages.inc}
{$i struct.inc}
{$i redef.inc}
{$i ascfun.inc}
{$i unifun.inc}
{$ifdef UNICODE}
{$i unidef.inc}
{$else not UNICODE}
{$i ascdef.inc}
{$endif UNICODE}
{$i func.inc}

  implementation

{$undef read_interface}
{$define read_implementation}

{$i base.inc}
{$i errors.inc}
{$i defines.inc}
{$i messages.inc}
{$i struct.inc}
{$i ascfun.inc}
{$i unifun.inc}
{$ifdef UNICODE}
{$i unidef.inc}
{$else not UNICODE}
{$i ascdef.inc}
{$endif UNICODE}
{$i func.inc}

  procedure InitializeCriticalSection(var CriticalSection : TRTLCriticalSection); external 'kernel32' name 'InitializeCriticalSection';
  procedure EnterCriticalSection(var CriticalSection : TRTLCriticalSection); external 'kernel32' name 'EnterCriticalSection';
  procedure LeaveCriticalSection(var CriticalSection : TRTLCriticalSection); external 'kernel32' name 'LeaveCriticalSection';
  procedure DeleteCriticalSection(var CriticalSection : TRTLCriticalSection); external 'kernel32' name 'DeleteCriticalSection';

{MvdV: JCL templates}
function GetTimeZoneInformation(var TimeZoneInformation:TTimeZoneInformation):DWORD;  external 'kernel32' name 'GetTimeZoneInformation';
function FileTimeToLocalFileTime(const lpFileTime:TFILETIME; var LocalFileTime :TFILETIME):WINBOOL; external 'kernel32' name 'FileTimeToLocalFileTime';
function FileTimeToSystemTime(const lpFileTime:TFILETIME; var lpSystemTime:TSYSTEMTIME):WINBOOL; external 'kernel32' name 'FileTimeToSystemTime';
function DosDateTimeToFileTime(wFatDate:WORD; wFatTime:WORD; var lpFileTime:tFILETIME):WINBOOL; external 'kernel32' name 'DosDateTimeToFileTime';
function SystemTimeToFileTime(const lSystemTime:tSYSTEMTIME;var FileTime:tFILETIME):WINBOOL;external 'kernel32' name 'SystemTimeToFileTime';


end.
{
  $Log$
  Revision 1.8  2000-03-19 20:30:27  marco
   * Some more delphi compatible kernelfunc headers for JCL.

  Revision 1.7  2000/02/09 16:59:35  peter
    * truncated log

  Revision 1.6  2000/01/07 16:41:53  daniel
    * copyright 2000

  Revision 1.5  2000/01/07 16:32:35  daniel
    * copyright 2000 added

  Revision 1.4  1999/09/16 13:38:21  peter
    * windows unit include moved to wininc/

}