{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 by the Free Pascal development team.

    Implemtents some stuff of OLE2, tries to be Delphi compatible

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit ole2;

  interface

    uses
       windows;

    type
       IUnknown = class
         public
           function QueryInterface(const iid: TIID; var obj): HResult; virtual; {$ifndef VER0_99_10}stdcall;{$endif} abstract;
           function AddRef: Longint; virtual; {$ifndef VER0_99_10}stdcall;{$endif} abstract;
           function Release: Longint; virtual; {$ifndef VER0_99_10}stdcall;{$endif} abstract;
       end;

  implementation

end.
{
  $Log$
  Revision 1.2  1999-03-30 17:00:25  peter
    * fixes for 0.99.10

  Revision 1.1  1999/03/22 22:12:51  florian
    + addition and changes to compile the direct draw unit
      of Erik Ungerer (with -dv2com and indirect disabled)

}

