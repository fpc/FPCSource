{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Pierre Muller

    Interface for the definition types of the symtable

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

{************************************************
                    TDef
************************************************}

       { definition contains the informations about a type }
       tdeftype = (abstractdef,arraydef,recorddef,pointerdef,orddef,
                   stringdef,enumdef,procdef,objectdef,errordef,
                   filedef,formaldef,setdef,procvardef,floatdef,
                   classrefdef);

       pdef = ^tdef;
       tdef = object
          deftype  : tdeftype;
          indexnb  : longint;
          savesize : longint;
          next     : pdef;
          owner    : psymtable;
          sym      : ptypesym;  { which type the definition was generated this def }

          has_inittable : boolean;
          { adress of init informations }
          inittable_label : plabel;

          has_rtti   : boolean;
          { address of rtti }
          rtti_label : plabel;

          nextglobal,
          previousglobal : pdef;
{$ifdef GDB}
          globalnb       : word;
          is_def_stab_written : boolean;
{$endif GDB}
          constructor init;
          constructor load;
          destructor  done;virtual;
          { registers enumdef inside objects or
            record directly in the owner symtable !! }
          procedure correct_owner_symtable;
          procedure write;virtual;
          procedure writename;
          function  size:longint;virtual;
{$ifdef GDB}
          function  NumberString:string;
          procedure set_globalnb;
          function  stabstring : pchar;virtual;
          function  allstabstring : pchar;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          procedure deref;virtual;

          { init. tables }
          function  needs_inittable : boolean;virtual;
          procedure generate_inittable;
          function  get_inittable_label : plabel;
          { the default implemenation calls write_rtti_data     }
          { if init and rtti data is different these procedures }
          { must be overloaded                                  }
          procedure write_init_data;virtual;
          { writes rtti of child to avoid mixup of rtti }
          procedure write_child_init_data;virtual;

          { rtti }
          function  get_rtti_label : plabel;
          procedure generate_rtti;virtual;
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;

          { returns true, if the definition can be published }
          function is_publishable : boolean;virtual;
       end;

       targconvtyp = (act_convertable,act_equal,act_exact);

       tvarspez = (vs_value,vs_const,vs_var);

       pdefcoll = ^tdefcoll;
       tdefcoll = record
          data    : pdef;
          next    : pdefcoll;
          paratyp : tvarspez;
          argconvtyp : targconvtyp;
       end;

       tfiletype = (ft_text,ft_typed,ft_untyped);

       pfiledef = ^tfiledef;
       tfiledef = object(tdef)
          filetype : tfiletype;
          typed_as : pdef;
          constructor init(ft : tfiletype;tas : pdef);
          constructor load;
          procedure write;virtual;
          procedure deref;virtual;
          procedure setsize;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pformaldef = ^tformaldef;
       tformaldef = object(tdef)
          constructor init;
          constructor load;
          procedure write;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       perrordef = ^terrordef;
       terrordef = object(tdef)
          constructor init;
{$ifdef GDB}
          function stabstring : pchar;virtual;
{$endif GDB}
       end;

       { tpointerdef and tclassrefdef should get a common
         base class, but I derived tclassrefdef from tpointerdef
         to avoid problems with bugs (FK)
       }

       ppointerdef = ^tpointerdef;
       tpointerdef = object(tdef)
          definition : pdef;
          defsym : ptypesym;
          constructor init(def : pdef);
          constructor load;
          procedure write;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          procedure deref;virtual;
       end;

       pobjectdef = ^tobjectdef;
       tobjectdef = object(tdef)
          childof : pobjectdef;
          name : pstring;
          { privatesyms : psymtable;
          protectedsyms : psymtable; }
          publicsyms : psymtable;
          options : longint;
          { to be able to have a variable vmt position }
          { and no vmt field for objects without virtuals }
          vmt_offset : longint;
          constructor init(const n : string;c : pobjectdef);
          destructor done;virtual;
          procedure check_forwards;
          function isrelated(d : pobjectdef) : boolean;
          function size : longint;virtual;
          constructor load;
          procedure write;virtual;
          function vmt_mangledname : string;
          function rtti_name : string;
          function isclass : boolean;
          procedure insertvmt;
          procedure set_parent(c : pobjectdef);
{$ifdef GDB}
          function stabstring : pchar;virtual;
{$endif GDB}
          procedure deref;virtual;

          function  needs_inittable : boolean;virtual;
          procedure write_init_data;virtual;
          procedure write_child_init_data;virtual;

          { rtti }
          procedure generate_rtti;virtual;
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
          function next_free_name_index : longint;
          function is_publishable : boolean;virtual;
       end;


       pclassrefdef = ^tclassrefdef;
       tclassrefdef = object(tpointerdef)
          constructor init(def : pdef);
          constructor load;
          procedure write;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       parraydef = ^tarraydef;
       tarraydef = object(tdef)
       private
          rangenr    : longint;
       public
          lowrange,
          highrange  : longint;
          definition : pdef;
          rangedef   : pdef;
          IsVariant,
          IsConstructor,
          IsArrayOfConst : boolean;
          function elesize : longint;
          constructor init(l,h : longint;rd : pdef);
          constructor load;
          procedure write;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          procedure deref;virtual;
          function size : longint;virtual;
          { generates the ranges needed by the asm instruction BOUND (i386)
            or CMP2 (Motorola) }
          procedure genrangecheck;

          { returns the label of the range check string }
          function getrangecheckstring : string;
          function needs_inittable : boolean;virtual;
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_table;virtual;
       end;

       precdef = ^trecdef;
       trecdef = object(tdef)
          symtable : psymtable;
          constructor init(p : psymtable);
          constructor load;
          destructor done;virtual;
          procedure write;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          procedure deref;virtual;
          function  needs_inittable : boolean;virtual;
          procedure write_rtti_data;virtual;
          procedure write_init_data;virtual;
          procedure write_child_rtti_data;virtual;
          procedure write_child_init_data;virtual;
       end;

       { base types }
       tbasetype = (uauto,uvoid,uchar,
                    u8bit,u16bit,u32bit,
                    s8bit,s16bit,s32bit,
                    bool8bit,bool16bit,bool32bit { uwchar,bool1bit,bitfield});

       porddef = ^torddef;
       torddef = object(tdef)
          low,high : longint;
          rangenr  : longint;
          typ      : tbasetype;
          {
          bits     : byte;
          }
          constructor init(t : tbasetype;v,b : longint);
          constructor load;
          procedure write;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
{$endif GDB}
          procedure setsize;

          { generates the ranges needed by the asm instruction BOUND }
          { or CMP2 (Motorola)                                       }
          procedure genrangecheck;
          { returns the label of the range check string }
          function getrangecheckstring : string;
          procedure write_rtti_data;virtual;
          function is_publishable : boolean;virtual;
       end;

       { sextreal is dependant on the cpu, s64bit is also }
       { dependant on the size (tp = 80bit for both)      }
       { The EXTENDED format exists on the motorola FPU   }
       { but it uses 96 bits instead of 80, with some     }
       { unused bits within the number itself! Pretty     }
       { complicated to support, so no support for the    }
       { moment.                                          }
       { s64 bit is considered as a real because all      }
       { calculations are done by the fpu.                }
       tfloattype = (f32bit,s32real,s64real,s80real,s64bit,f16bit);

       pfloatdef = ^tfloatdef;
       tfloatdef = object(tdef)
          typ : tfloattype;
          constructor init(t : tfloattype);
          constructor load;
          procedure write;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
{$endif GDB}
          procedure setsize;
          function is_publishable : boolean;virtual;
          procedure write_rtti_data;virtual;
       end;

       pabstractprocdef = ^tabstractprocdef;
       tabstractprocdef = object(tdef)
          { saves a definition to the return type }
          retdef   : pdef;
          fpu_used : byte;    { how many stack fpu must be empty }
          options  : longint; { save the procedure options }
          para1    : pdefcoll;
          constructor init;
          constructor load;
          destructor done;virtual;
          procedure concatdef(p : pdef;vsp : tvarspez);
          procedure deref;virtual;
          function para_size : longint;
          function demangled_paras : string;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          procedure test_if_fpu_result;
          procedure write;virtual;
       end;

       pprocvardef = ^tprocvardef;
       tprocvardef = object(tabstractprocdef)
          constructor init;
          constructor load;
          procedure write;virtual;
          function size : longint;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput); virtual;
{$endif GDB}
          procedure write_child_rtti_data;virtual;
          function is_publishable : boolean;virtual;
          procedure write_rtti_data;virtual;
       end;

       pprocdef = ^tprocdef;
       tprocdef = object(tabstractprocdef)
          extnumber : longint;
          nextoverloaded : pprocdef;
          { pointer to the local symbol table }
          localst : psymtable;
          { pointer to the parameter symbol table }
          parast : psymtable;
{$ifdef UseBrowser}
          lastref,
          defref,
          lastwritten : pref;
          refcount : longint;
{$endif UseBrowser}
          _class : pobjectdef;
          _mangledname : pchar;
          { it's a tree, but this not easy to handle }
          { used for inlined procs                   }
          code : pointer;
          { true, if the procedure is only declared }
          { (forward procedure) }
          forwarddef : boolean;
          { set which contains the modified registers }
{$ifdef i386}
          usedregisters : byte;
{$endif}
{$ifdef m68k}
          usedregisters : word;
{$endif}
{$ifdef alpha}
          usedregisters_int : longint;
          usedregisters_fpu : longint;
{$endif}
          constructor init;
          destructor done;virtual;
          constructor load;
          procedure write;virtual;
{$ifdef GDB}
          function cplusplusmangledname : string;
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          procedure deref;virtual;
          function mangledname : string;
          procedure setmangledname(const s : string);
{$ifdef UseBrowser}
          procedure load_references;
          function  write_references : boolean;
          procedure add_to_browserlog;
{$endif UseBrowser}
       end;

       tstringtype = (st_shortstring, st_longstring, st_ansistring, st_widestring);

       pstringdef = ^tstringdef;
       tstringdef = object(tdef)
          string_typ : tstringtype;
          len : longint;
          constructor init(l : byte);
          constructor load;
          constructor longinit(l : longint);
          constructor longload;
          constructor ansiinit(l : longint);
          constructor ansiload;
          constructor wideinit(l : longint);
          constructor wideload;
          function size : longint;virtual;
          procedure write;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          function needs_inittable : boolean;virtual;
          procedure write_rtti_data;virtual;
          function is_publishable : boolean;virtual;
       end;

       penumdef = ^tenumdef;
       tenumdef = object(tdef)
          rangenr,
          minval,
          maxval    : longint;
          has_jumps : boolean;
          first     : penumsym;
          basedef   : penumdef;
          constructor init;
          constructor init_subrange(_basedef:penumdef;_min,_max:longint);
          constructor load;
          destructor done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          procedure calcsavesize;
          procedure setmax(_max:longint);
          procedure setmin(_min:longint);
          function  min:longint;
          function  max:longint;
          function  getrangecheckstring:string;
          procedure genrangecheck;
{$ifdef GDB}
          function stabstring : pchar;virtual;
{$endif GDB}
          procedure write_child_rtti_data;virtual;
          procedure write_rtti_data;virtual;
          function is_publishable : boolean;virtual;
       end;

       tsettype = (normset,smallset,varset);

       psetdef = ^tsetdef;
       tsetdef = object(tdef)
          setof : pdef;
          settype : tsettype;
          constructor init(s : pdef;high : longint);
          constructor load;
          procedure write;virtual;
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          procedure deref;virtual;
          function is_publishable : boolean;virtual;
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
       end;

{
  $Log$
  Revision 1.6  1998-10-22 17:11:23  pierre
    + terminated the include exclude implementation for i386
    * enums inside records fixed

  Revision 1.5  1998/10/16 13:12:55  pierre
    * added vmt_offsets in destructors code also !!!
    * vmt_offset code for m68k

  Revision 1.4  1998/10/16 08:51:52  peter
    + target_os.stackalignment
    + stack can be aligned at 2 or 4 byte boundaries

  Revision 1.3  1998/10/05 21:33:30  peter
    * fixed 161,165,166,167,168

  Revision 1.2  1998/10/02 07:20:40  florian
    * range checking in units doesn't work if the units are smartlinked, fixed

  Revision 1.1  1998/09/23 12:03:57  peter
    * overloading fix for array of const

}
