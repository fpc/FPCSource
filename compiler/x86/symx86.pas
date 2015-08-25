{
    Copyright (c) 2014 by Florian Klaempfl

    Symbol table overrides for x86

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
unit symx86;

{$i fpcdefs.inc}

interface

uses
  globtype, cclasses,
  symconst, symtype,symdef,symsym;

type
  tx86pointerdef = class(tpointerdef)
   protected
    procedure ppuload_platform(ppufile: tcompilerppufile); override;
    procedure ppuwrite_platform(ppufile: tcompilerppufile); override;
   public
    x86pointertyp : tx86pointertyp;
    constructor create(def: tdef); override;
    class function getreusable(def: tdef): tpointerdef; override;
    class function getreusablex86(def: tdef; x86typ: tx86pointertyp): tpointerdef;
    constructor createx86(def:tdef;x86typ:tx86pointertyp);virtual;
    function size: asizeint; override;
    function getcopy: tstoreddef; override;
    function GetTypeName: string; override;
    class function default_x86_data_pointer_type: tx86pointertyp; virtual;
  end;
  tx86pointerdefclass = class of tx86pointerdef;

implementation

  uses
    globals, verbose,
    symbase, fmodule;

{****************************************************************************
                             tx86pointerdef
****************************************************************************}

  procedure tx86pointerdef.ppuload_platform(ppufile: tcompilerppufile);
    begin
      inherited;
      x86pointertyp:=tx86pointertyp(ppufile.getbyte);
    end;


  procedure tx86pointerdef.ppuwrite_platform(ppufile: tcompilerppufile);
    begin
      inherited;
      ppufile.putbyte(byte(x86pointertyp));
    end;


  constructor tx86pointerdef.create(def: tdef);
    begin
      inherited create(def);
      x86pointertyp := default_x86_data_pointer_type;
    end;


  class function tx86pointerdef.getreusable(def: tdef): tpointerdef;
    begin
      result:=getreusablex86(def,default_x86_data_pointer_type);
    end;


  class function tx86pointerdef.getreusablex86(def: tdef; x86typ: tx86pointertyp): tpointerdef;
    type
      tx86PtrDefKey = packed record
        def: tdef;
        x86typ:tx86pointertyp;
      end;
    var
      res: PHashSetItem;
      oldsymtablestack: tsymtablestack;
      key: tx86PtrDefKey;
    begin
      if not assigned(current_module) then
        internalerror(2011071101);
      key.def:=def;
      key.x86typ:=x86typ;
      res:=current_module.ptrdefs.FindOrAdd(@key,sizeof(key));
      if not assigned(res^.Data) then
        begin
          { since these pointerdefs can be reused anywhere in the current
            unit, add them to the global/staticsymtable (or local symtable
            if they're a local def, because otherwise they'll be saved
            to the ppu referencing a local symtable entry that doesn't
            exist in the ppu) }
          oldsymtablestack:=symtablestack;
          { do not simply push/pop current_module.localsymtable, because
            that can have side-effects (e.g., it removes helpers) }
          symtablestack:=nil;
          res^.Data:=tx86pointerdefclass(cpointerdef).createx86(def,x86typ);
          def.getreusablesymtab.insertdef(tdef(res^.Data));
          symtablestack:=oldsymtablestack;
        end;
      result:=tpointerdef(res^.Data);
    end;


  constructor tx86pointerdef.createx86(def: tdef; x86typ: tx86pointertyp);
    begin
      tabstractpointerdef(self).create(pointerdef,def);
      x86pointertyp := x86typ;
      has_pointer_math:=cs_pointermath in current_settings.localswitches;
    end;


  function tx86pointerdef.size: asizeint;
    begin
      if x86pointertyp in [x86pt_far,x86pt_huge] then
        result:=sizeof(pint)+2
      else
        result:=inherited;
    end;


  function tx86pointerdef.getcopy: tstoreddef;
    begin
      result:=inherited;
      tx86pointerdef(result).x86pointertyp:=x86pointertyp;
    end;


  function tx86pointerdef.GetTypeName: string;
    begin
      result:=inherited;
      if x86pointertyp<>default_x86_data_pointer_type then
        begin
          case x86pointertyp of
            x86pt_near:
              result:=result+';near';
            x86pt_near_cs:
              result:=result+';near ''CS''';
            x86pt_near_ds:
              result:=result+';near ''DS''';
            x86pt_near_ss:
              result:=result+';near ''SS''';
            x86pt_near_es:
              result:=result+';near ''ES''';
            x86pt_near_fs:
              result:=result+';near ''FS''';
            x86pt_near_gs:
              result:=result+';near ''GS''';
            x86pt_far:
              result:=result+';far';
            x86pt_huge:
              result:=result+';huge';
            else
              internalerror(2013050301);
          end;
        end;
    end;


  class function tx86pointerdef.default_x86_data_pointer_type: tx86pointertyp;
    begin
      result:=x86pt_near;
    end;

end.

