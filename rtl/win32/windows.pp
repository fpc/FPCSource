{
    $Id$
    This file is part of the Free Pascal run time library.
    This unit contains the record definition for the Win32 API
    Copyright (c) 1993,97 by Florian KLaempfl,
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

{$i base.pp}
{$i errors.pp}
{$i defines.pp}
{$i messages.pp}
{$i struct.pp}
{$i redef.inc}
{$i ascfun.pp}
{$i unifun.pp}
{$ifdef UNICODE}
{$i unidef.pp}
{$else not UNICODE}
{$i ascdef.pp}
{$endif UNICODE}
{$i func.pp}

  implementation

{$undef read_interface}
{$define read_implementation}
  
const External_library='kernel32';

{$i base.pp}
{$i errors.pp}
{$i defines.pp}
{$i messages.pp}
{$i struct.pp}
{$i ascfun.pp}
{$i unifun.pp}
{$ifdef UNICODE}
{$i unidef.pp}
{$else not UNICODE}
{$i ascdef.pp}
{$endif UNICODE}
{$i func.pp}
  procedure InitializeCriticalSection(var CriticalSection : TRTLCriticalSection); external 'kernel32' name 'InitializeCriticalSection';
  procedure EnterCriticalSection(var CriticalSection : TRTLCriticalSection); external 'kernel32' name 'EnterCriticalSection';
  procedure LeaveCriticalSection(var CriticalSection : TRTLCriticalSection); external 'kernel32' name 'LeaveCriticalSection';
  procedure DeleteCriticalSection(var CriticalSection : TRTLCriticalSection); external 'kernel32' name 'DeleteCriticalSection';

end.
{
  $Log$
  Revision 1.3  1999-05-10 19:34:15  florian
    * moved all opengl32.dll stuff to a newly created opengl32 unit, so
      win32 programs should also run on Windows without opengl32.dll

  Revision 1.2  1999/01/09 07:29:51  florian
    * some updates to compile API units for win32

  Revision 1.1  1998/08/31 11:54:02  pierre
    * compilable windows.pp file
      still to do :
       - findout problems
       - findout the correct DLL for each call !!

}
