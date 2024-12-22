{
    Copyright (c) 2002 by Florian Klaempfl

    Risc-V32 specific calling conventions

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
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
       globtype,
       aasmtai,aasmdata,
       cpubase,
       symconst,symtype,symdef,symsym,
       paramgr,parabase,cgbase,cgutils,
       pararv;

    type
       tcpuparamanager = class(trvparamanager)
         function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;

         function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
       end;

  implementation

    uses
       cpuinfo,globals,
       verbose,systems,
       defutil,symtable,
       procinfo,cpupi;

    function tcpuparamanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        result:=false;
        { var,out,constref always require address }
        if varspez in [vs_var,vs_out,vs_constref] then
          begin
            result:=true;
            exit;
          end;
        case def.typ of
          variantdef,
          formaldef :
            result:=true;
          { regular procvars must be passed by value, because you cannot pass
            the address of a local stack location when calling e.g.
            pthread_create with the address of a function (first of all it
            expects the address of the function to execute and not the address
            of a memory location containing that address, and secondly if you
            first store the address on the stack and then pass the address of
            this stack location, then this stack location may no longer be
            valid when the newly started thread accesses it.

            However, for "procedure of object" we must use the same calling
            convention as for "8 byte record" due to the need for
            interchangeability with the TMethod record type.
          }
          procvardef,
          recorddef:
            result := not(def.size in [0..sizeof(aint)*2]) or (varspez = vs_const);
          arraydef:
            result:=(tarraydef(def).highrange>=tarraydef(def).lowrange) or
                             is_open_array(def) or
                             is_array_of_const(def) or
                             is_array_constructor(def);
          objectdef :
            result:=is_object(def);
          setdef :
            result:=not is_smallset(def);
          stringdef :
            result:=tstringdef(def).stringtype in [st_shortstring,st_longstring];
          else
            ;
        end;
      end;


    function tcpuparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        cur_stack_offset: aword;
        curintreg, curfloatreg, curmmreg: tsuperregister;
      begin
        init_values(curintreg,curfloatreg,curmmreg,cur_stack_offset);

        result := create_paraloc_info_intern(p,side,p.paras,curintreg,curfloatreg,curmmreg,cur_stack_offset,false);

        create_funcretloc_info(p,side);
      end;

begin
   paramanager:=tcpuparamanager.create;
end.
