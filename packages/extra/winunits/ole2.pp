{
    $Id: ole2.pp,v 1.1 2005/03/28 15:09:35 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Implemtents some stuff of OLE2, tries to be Delphi compatible

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit ole2;

{$Mode ObjFpc}

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
  $Log: ole2.pp,v $
  Revision 1.1  2005/03/28 15:09:35  peter
  new winunits packages

  Revision 1.4  2005/02/14 17:13:32  peter
    * truncate log

}
