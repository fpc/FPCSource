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
  
{$warning starting interface of windows unit }

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

end.
{
  $Log$
  Revision 1.1  1998-08-31 11:54:02  pierre
    * compilable windows.pp file
      still to do :
       - findout problems
       - findout the correct DLL for each call !!

}
