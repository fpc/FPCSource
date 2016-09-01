{
    Copyright (c) 2008 by the Free Pascal Compiler team

    This unit implements common support for import,export,link routines
    for unix target

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
unit expunix;

{$i fpcdefs.inc}

interface

uses
  cutils,cclasses,
  systems,
  export,
  symtype,symdef,symsym,
  aasmbase;

type
  texportlibunix=class(texportlib)
   private
     fexportedsymnames: TCmdStrList;
   public
    constructor Create; override;
    destructor destroy; override;
    procedure preparelib(const s : string);override;
    procedure exportprocedure(hp : texported_item);override;
    procedure exportvar(hp : texported_item);override;
    procedure generatelib;override;
    property exportedsymnames: TCmdStrList read fexportedsymnames;
  end;

implementation

{****************************************************************************
                              TExportLibUnix
****************************************************************************}

uses
  symconst,
  globtype,globals,
  aasmdata,aasmtai,aasmcpu,
  fmodule,
  cgbase,cgutils,cpubase,cgobj,
  cgcpu,hlcgobj,hlcgcpu,
{$ifdef llvm}
  hlcgllvm,
{$endif llvm}
  ncgutil,
  verbose;


constructor texportlibunix.create;
begin
  inherited create;
  fexportedsymnames:=tcmdstrlist.create_no_double;
end;

destructor texportlibunix.destroy;
begin
  fexportedsymnames.free;
  inherited destroy;
end;



procedure texportlibunix.preparelib(const s:string);
begin
end;


procedure texportlibunix.exportprocedure(hp : texported_item);
var
  hp2 : texported_item;
begin
  { first test the index value }
  if eo_index in hp.options then
   begin
     Message1(parser_e_no_export_with_index_for_target,target_info.shortname);
     exit;
   end;
  { now place in correct order }
  hp2:=texported_item(current_module._exports.first);
  while assigned(hp2) and
     (hp.name^>hp2.name^) do
    hp2:=texported_item(hp2.next);
  { insert hp there !! }
  if assigned(hp2) and (hp2.name^=hp.name^) then
    begin
      { this is not allowed !! }
      duplicatesymbol(hp.name^);
      exit;
    end;
  if hp2=texported_item(current_module._exports.first) then
    current_module._exports.concat(hp)
  else if assigned(hp2) then
    begin
       hp.next:=hp2;
       hp.previous:=hp2.previous;
       if assigned(hp2.previous) then
         hp2.previous.next:=hp;
       hp2.previous:=hp;
    end
  else
    current_module._exports.concat(hp);
end;


procedure texportlibunix.exportvar(hp : texported_item);
begin
  hp.is_var:=true;
  exportprocedure(hp);
end;


procedure texportlibunix.generatelib;  // straight t_linux copy for now.
var
  hp2 : texported_item;
  pd  : tprocdef;
  anyhasalias : boolean;
  i : longint;
begin
  pd:=nil;
  create_hlcodegen;
  new_section(current_asmdata.asmlists[al_procedures],sec_code,'',0);
  hp2:=texported_item(current_module._exports.first);
  while assigned(hp2) do
   begin
     if (not hp2.is_var) and
        assigned(hp2.sym) and
        (hp2.sym.typ=procsym) then
      begin
        { the manglednames can already be the same when the procedure
          is declared with cdecl }
        { note: for "exports" sections we only allow non overloaded procsyms,
                so checking all symbols only matters for packages }
        anyhasalias:=false;
        for i:=0 to tprocsym(hp2.sym).procdeflist.count-1 do
          begin
            pd:=tprocdef(tprocsym(hp2.sym).procdeflist[i]);
            anyhasalias:=has_alias_name(pd,hp2.name^);
            if anyhasalias then
              break;
          end;
        if not anyhasalias then
          hlcg.g_external_wrapper(current_asmdata.asmlists[al_procedures],pd,hp2.name^,pd.mangledname,true);
        exportedsymnames.insert(hp2.name^);
      end
     else
       begin
         if assigned(hp2.sym) and
            (hp2.name^<>hp2.sym.mangledname) then
           Message2(parser_e_cant_export_var_different_name,hp2.sym.realname,hp2.sym.mangledname)
         else
           exportedsymnames.insert(hp2.name^);
       end;
     hp2:=texported_item(hp2.next);
   end;
   destroy_hlcodegen;
end;


end.
