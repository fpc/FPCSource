{
    $Id$
    Copyright (c) 2001-2002 by Peter Vreman

    This unit implements support import,export,link routines for MacOS.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit t_macos;

{$i fpcdefs.inc}

interface

  uses
     import,symsym,symdef;

  type
    timportlibmacos=class(timportlib)
      procedure preparelib(const s:string);override;
      procedure importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);override;
      procedure importvariable(vs:tvarsym;const name,module:string);override;
      procedure generatelib;override;
    end;

implementation

    uses
       link,
       cutils,cclasses,
       globtype,globals,systems,verbose,script,fmodule,i_macos,
       symconst;

{*****************************************************************************
                               TIMPORTLIBMACOS
*****************************************************************************}

procedure timportlibmacos.preparelib(const s : string);
begin
end;


procedure timportliblinux.importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_allways);
  { do nothing with the procedure, only set the mangledname }
  if name<>'' then
   begin
     aprocdef.setmangledname(name);
   end
  else
    message(parser_e_empty_import_name);
end;


procedure timportlibmacos.importvariable(vs:tvarsym;const name,module:string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_allways);
  { reset the mangledname and turn off the dll_var option }
  vs.set_mangledname(name);
  exclude(vs.varoptions,vo_is_dll_var);
end;


procedure timportlibmacos.generatelib;
begin
end;


{*****************************************************************************
                                  Initialize
*****************************************************************************}

initialization
{$ifdef m68k}
  RegisterTarget(system_m68k_macos_info);
  RegisterImport(system_m68k_macos,timportlibmacos);
{$endif m68k}
{$ifdef powerpc}
  RegisterTarget(system_powerpc_macos_info);
  RegisterImport(system_powerpc_macos,timportlibmacos);
{$endif powerpc}
end.
{
  $Log$
  Revision 1.5  2003-04-27 08:50:45  peter
    * compile fix

  Revision 1.4  2002/11/17 16:32:04  carl
    * memory optimization (3-4%) : cleanup of tai fields,
       cleanup of tdef and tsym fields.
    * make it work for m68k

  Revision 1.3  2002/10/16 20:18:33  olle
    * header comment updated

  Revision 1.2  2002/10/02 21:50:19  florian
    * importing via external is now possible for macos

  Revision 1.1  2002/09/06 15:03:50  carl
    * moved files to systems directory

  Revision 1.13  2002/08/20 21:40:44  florian
    + target macos for ppc added
    + frame work for mpw assembler output

  Revision 1.12  2002/07/26 21:15:46  florian
    * rewrote the system handling
}
