{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl

    This unit implements some support routines for the linux target like
    import/export handling

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
unit lin_targ;
interface

  uses import;

  type
    pimportliblinux=^timportliblinux;
    timportliblinux=object(timportlib)
      procedure preparelib(const s:string);virtual;
      procedure importprocedure(const func,module:string;index:longint;const name:string);virtual;
      procedure importvariable(const varname,module:string;const name:string);virtual;
      procedure generatelib;virtual;
    end;


implementation

  uses
    verbose,strings,cobjects,systems,globtype,globals,
    files,aasm,symtable;


    procedure timportliblinux.preparelib(const s : string);
      begin
      end;


    procedure timportliblinux.importprocedure(const func,module : string;index : longint;const name : string);
      begin
        { insert sharedlibrary }
        current_module^.linkothersharedlibs.insert(SplitName(module),link_allways);
        { do nothing with the procedure, only set the mangledname }
        if name<>'' then
          aktprocsym^.definition^.setmangledname(name)
        else
          Message(parser_e_empty_import_name);
      end;


    procedure timportliblinux.importvariable(const varname,module:string;const name:string);
      begin
        { insert sharedlibrary }
        current_module^.linkothersharedlibs.insert(SplitName(module),link_allways);
        { reset the mangledname and turn off the dll_var option }
        aktvarsym^.setmangledname(name);
        aktvarsym^.var_options:=aktvarsym^.var_options and (not vo_is_dll_var);
      end;


    procedure timportliblinux.generatelib;
      begin
      end;


end.
{
  $Log$
  Revision 1.4  1999-07-03 00:29:50  peter
    * new link writing to the ppu, one .ppu is needed for all link types,
      static (.o) is now always created also when smartlinking is used

  Revision 1.3  1999/01/20 14:18:32  pierre
    * bugs related to mangledname solved
      - linux external without name
      -external procs already used
      (added count and is_used boolean fiels in tprocvar)

  Revision 1.2  1998/11/28 16:20:51  peter
    + support for dll variables

  Revision 1.1  1998/10/19 18:07:13  peter
    + external dll_name name func support for linux

}
