{
    Copyright (C) 2010 by Jonas Maebe

    This unit handles the temporary variables for the JVM

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
{
  This unit handles the temporary variables for the JVM.
}
unit tgcpu;

{$i fpcdefs.inc}

  interface

    uses
       globtype,
       aasmdata,
       cgutils,
       symtype,tgobj;

    type

       { ttgjvm }

       ttgjvm = class(ttgobj)
        protected
         function getifspecialtemp(list: TAsmList; def: tdef; forcesize: aint; temptype: ttemptype; out ref: treference): boolean;
         function alloctemp(list: TAsmList; size, alignment: longint; temptype: ttemptype; def: tdef): longint; override;
        public
         procedure setfirsttemp(l : longint); override;
         procedure getlocal(list: TAsmList; size: longint; alignment: shortint; def: tdef; var ref: treference); override;
         procedure gethltemp(list: TAsmList; def: tdef; forcesize: aint; temptype: ttemptype; out ref: treference); override;
       end;

  implementation

    uses
       verbose,
       cgbase,
       symconst,defutil,
       hlcgobj,hlcgcpu,
       symdef;


    { ttgjvm }

    function ttgjvm.getifspecialtemp(list: TAsmList; def: tdef; forcesize: aint; temptype: ttemptype; out ref: treference): boolean;
      var
        eledef: tdef;
        ndim: longint;
      begin
        result:=false;
        case def.typ of
          arraydef:
            begin
              if not is_dynamic_array(def) then
                begin
                  { allocate an array of the right size }
                  gettemp(list,java_jlobject.size,java_jlobject.alignment,temptype,ref);
                  ndim:=0;
                  eledef:=def;
                  repeat
                    thlcgjvm(hlcg).a_load_const_stack(list,s32inttype,tarraydef(eledef).elecount,R_INTREGISTER);
                    eledef:=tarraydef(eledef).elementdef;
                    inc(ndim);
                  until (eledef.typ<>arraydef) or
                        is_dynamic_array(eledef);
                  eledef:=tarraydef(def).elementdef;
                  thlcgjvm(hlcg).g_newarray(list,def,ndim);
                  thlcgjvm(hlcg).a_load_stack_ref(list,java_jlobject,ref,0);
                  result:=true;
                end;
            end;
        end;
      end;


    function ttgjvm.alloctemp(list: TAsmList; size, alignment: longint; temptype: ttemptype; def: tdef): longint;
      begin
        { the JVM only supports 1 slot (= 4 bytes in FPC) and 2 slot (= 8 bytes in
          FPC) temps on the stack. double and int64 are 2 slots, the rest is one slot.
          There are no problems with reusing the same slot for a value of a different
          type. There are no alignment requirements either. }
        if size<4 then
          size:=4;
        if not(size in [4,8]) then
          internalerror(2010121401);
        { don't pass on "def", since we don't care if a slot is used again for a
          different type }
        result:=inherited alloctemp(list, size shr 2, 1, temptype, nil);
      end;


    procedure ttgjvm.setfirsttemp(l: longint);
      begin
        firsttemp:=l;
        lasttemp:=l;
      end;


    procedure ttgjvm.getlocal(list: TAsmList; size: longint; alignment: shortint; def: tdef; var ref: treference);
      begin
        if not getifspecialtemp(list,def,size,tt_persistent,ref) then
          inherited;
      end;


    procedure ttgjvm.gethltemp(list: TAsmList; def: tdef; forcesize: aint; temptype: ttemptype; out ref: treference);
      begin
        if not getifspecialtemp(list,def,forcesize,temptype,ref) then
          inherited;
      end;


begin
  tgobjclass:=ttgjvm;
end.
