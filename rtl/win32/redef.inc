{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by the Free Pascal development team

    This file defines type names as they are used by Delphi
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
type
   TRTLCriticalSection = CRITICAL_SECTION;
   PRTLCriticalSection = PCRITICAL_SECTION;

  procedure InitializeCriticalSection(var CriticalSection : TRTLCriticalSection);
  procedure EnterCriticalSection(var CriticalSection : TRTLCriticalSection);
  procedure LeaveCriticalSection(var CriticalSection : TRTLCriticalSection);
  procedure DeleteCriticalSection(var CriticalSection : TRTLCriticalSection);

