{
   $Id$

   GMODULE - GLIB wrapper code for dynamic module loading
   Copyright (C) 1998 Tim Janik

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
}
unit gmodule;
interface

{$mode objfpc}

{ Always use smartlinking for win32, this solves some undefined functions
  in the development gtk versions which change often (PFV) }
{$ifdef win32}
  {$smartlink on}
{$endif}

uses
  glib;

{$ifdef win32}
  const
    gmoduledll='gmodule-1.3';
  {$define gtkwin}

  {$packrecords 4}
{$else}
  const
    gmoduledll='gmodule';

  {$packrecords C}
{$endif}

    var
       g_log_domain_gmodule : Pgchar;external gmoduledll name 'g_log_domain_gmodule';

    type
       PGModule=pointer;

       TGModuleFlags = longint;
    const
       G_MODULE_BIND_LAZY = 1 shl 0;
       G_MODULE_BIND_MASK = 1;

    type
       TGModuleCheckInit = function (module:PGModule):Pgchar;cdecl;
       TGModuleUnload = procedure (module:PGModule);cdecl;

function  g_module_supported:gboolean;cdecl;external gmoduledll name 'g_module_supported';
function  g_module_open(file_name:Pgchar; flags:TGModuleFlags):PGModule;cdecl;external gmoduledll name 'g_module_open';
function  g_module_close(module:PGModule):gboolean;cdecl;external gmoduledll name 'g_module_close';
procedure g_module_make_resident(module:PGModule);cdecl;external gmoduledll name 'g_module_make_resident';
function  g_module_error:Pgchar;cdecl;external gmoduledll name 'g_module_error';
function  g_module_symbol(module:PGModule; symbol_name:Pgchar; symbol:Pgpointer):gboolean;cdecl;external gmoduledll name 'g_module_symbol';
function  g_module_name(module:PGModule):Pgchar;cdecl;external gmoduledll name 'g_module_name';
function  g_module_build_path(directory:Pgchar; module_name:Pgchar):Pgchar;cdecl;external gmoduledll name 'g_module_build_path';


implementation

end.
{
  $Log$
  Revision 1.1.2.2  2000-09-06 21:13:35  peter
    * packrecords 4 for win32, packrecords c for linux

  Revision 1.1.2.1  2000/08/06 10:33:10  peter
    * force smartlinking

  Revision 1.1  2000/07/13 06:34:02  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:34  peter
    * moved to packages dir

  Revision 1.6  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.5  1999/07/23 16:11:48  peter
    * use packrecords C

  Revision 1.4  1999/07/03 10:29:23  peter
    * enum fixes
    * use version 1.3 instead of 1.2 for win32

  Revision 1.3  1999/05/11 00:37:58  peter
    * win32 fixes

  Revision 1.2  1999/05/10 15:18:46  peter
    * cdecl fixes

  Revision 1.1  1999/05/07 10:40:22  peter
    * first things for 1.2

}
