{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    PowerPC specific calling conventions

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
}
unit paramgr;

{$i fpcdefs.inc}

  interface

    uses
       cpubase,
       symtype,symdef;

    type
       tparamanager = class
          { Returns true if the return value can be put in accumulator }
          function ret_in_acc(def : tdef) : boolean;virtual;

          { Returns true if uses a parameter as return value (???) }
          function ret_in_param(def : tdef) : boolean;virtual;

          function push_high_param(def : tdef) : boolean;virtual;

          { Returns true if a parameter is too large to copy and only the address is pushed
          }
          function push_addr_param(def : tdef) : boolean;virtual;
          function getintparaloc(nr : longint) : tparalocation;virtual;abstract;
          procedure create_param_loc_info(p : tabstractprocdef);virtual;abstract;

          { Returns the location where the invisible parameter for structured
            function results will be passed.
          }
          function getfuncretloc(p : tabstractprocdef) : tparalocation;virtual;abstract;
          { Returns the self pointer for the give procdef
          function getfuncretloc(p : tabstractprocdef) : tparalocation;virtual;abstract;
          }
       end;

    var
       paralocdummy : tparalocation;
       paramanager : tparamanager;

  implementation

    uses
       cpuinfo,
       symconst,symbase,
       defbase;

    { true if the return value is in accumulator (EAX for i386), D0 for 68k }
    function tparamanager.ret_in_acc(def : tdef) : boolean;
      begin
         ret_in_acc:=(def.deftype in [orddef,pointerdef,enumdef,classrefdef]) or
                     ((def.deftype=stringdef) and (tstringdef(def).string_typ in [st_ansistring,st_widestring])) or
                     ((def.deftype=procvardef) and not(po_methodpointer in tprocvardef(def).procoptions)) or
                     ((def.deftype=objectdef) and not is_object(def)) or
                     ((def.deftype=setdef) and (tsetdef(def).settype=smallset));
      end;


    { true if uses a parameter as return value }
    function tparamanager.ret_in_param(def : tdef) : boolean;
      begin
         ret_in_param:=(def.deftype in [arraydef,recorddef]) or
           ((def.deftype=stringdef) and (tstringdef(def).string_typ in [st_shortstring,st_longstring])) or
           ((def.deftype=procvardef) and (po_methodpointer in tprocvardef(def).procoptions)) or
           ((def.deftype=objectdef) and is_object(def)) or
           (def.deftype=variantdef) or
           ((def.deftype=setdef) and (tsetdef(def).settype<>smallset));
      end;


    function tparamanager.push_high_param(def : tdef) : boolean;
      begin
         push_high_param:=is_open_array(def) or
                          is_open_string(def) or
                          is_array_of_const(def);
      end;


    { true if a parameter is too large to copy and only the address is pushed }
    function tparamanager.push_addr_param(def : tdef) : boolean;
      begin
        push_addr_param:=false;
        if never_copy_const_param then
         push_addr_param:=true
        else
         begin
           case def.deftype of
             variantdef,
             formaldef :
               push_addr_param:=true;
             recorddef :
               push_addr_param:=(def.size>pointer_size);
             arraydef :
               push_addr_param:=((tarraydef(def).highrange>=tarraydef(def).lowrange) and (def.size>pointer_size)) or
                                is_open_array(def) or
                                is_array_of_const(def) or
                                is_array_constructor(def);
             objectdef :
               push_addr_param:=is_object(def);
             stringdef :
               push_addr_param:=tstringdef(def).string_typ in [st_shortstring,st_longstring];
             procvardef :
               push_addr_param:=(po_methodpointer in tprocvardef(def).procoptions);
             setdef :
               push_addr_param:=(tsetdef(def).settype<>smallset);
           end;
         end;
      end;

finalization
  paramanager.free;
end.

{
   $Log$
   Revision 1.5  2002-07-26 21:15:39  florian
     * rewrote the system handling

   Revision 1.4  2002/07/20 11:57:55  florian
     * types.pas renamed to defbase.pas because D6 contains a types
       unit so this would conflicts if D6 programms are compiled
     + Willamette/SSE2 instructions to assembler added

   Revision 1.3  2002/07/13 19:38:43  florian
     * some more generic calling stuff fixed

   Revision 1.2  2002/07/13 07:17:15  jonas
     * fixed memory leak reported by  Sergey Korshunoff

   Revision 1.1  2002/07/11 14:41:28  florian
     * start of the new generic parameter handling
}
