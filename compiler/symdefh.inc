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
       tdef = object(tsymtableentry)
          deftype   : tdeftype;
          sym       : ptypesym;  { which type the definition was generated this def }

          has_inittable : boolean;
          { adress of init informations }
          inittable_label : pasmlabel;

          has_rtti   : boolean;
          { address of rtti }
          rtti_label : pasmlabel;

          nextglobal,
          previousglobal : pdef;
{$ifdef GDB}
          globalnb       : word;
          is_def_stab_written : boolean;
{$endif GDB}
          constructor init;
          constructor load;
          destructor  done;virtual;
          procedure deref;virtual;
          procedure symderef;virtual;
          function  typename:string;
          procedure write;virtual;
          function  size:longint;virtual;
          function  alignment:longint;virtual;
          function  gettypename:string;virtual;
          function  is_publishable : boolean;virtual;
          function  is_in_current : boolean;
          procedure correct_owner_symtable; { registers enumdef inside objects or
                                              record directly in the owner symtable !! }
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
          function  NumberString:string;
          procedure set_globalnb;
          function  allstabstring : pchar;
{$endif GDB}
          { init. tables }
          function  needs_inittable : boolean;virtual;
          procedure generate_inittable;
          function  get_inittable_label : pasmlabel;
          { the default implemenation calls write_rtti_data     }
          { if init and rtti data is different these procedures }
          { must be overloaded                                  }
          procedure write_init_data;virtual;
          procedure write_child_init_data;virtual;
          { rtti }
          procedure write_rtti_name;
          function  get_rtti_label : string;virtual;
          procedure generate_rtti;virtual;
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
          function is_intregable : boolean;
          function is_fpuregable : boolean;
       private
          savesize  : longint;
       end;

       targconvtyp = (act_convertable,act_equal,act_exact);

       tvarspez = (vs_value,vs_const,vs_var);

       pdefcoll = ^tdefcoll;
       tdefcoll = record
          data    : pdef;
          datasym : ptypesym;
          next    : pdefcoll;
          paratyp : tvarspez;
          argconvtyp : targconvtyp;
          convertlevel : byte;
          register : tregister;
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
          function  gettypename:string;virtual;
          procedure setsize;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pformaldef = ^tformaldef;
       tformaldef = object(tdef)
          constructor init;
          constructor load;
          procedure write;virtual;
          function  gettypename:string;virtual;
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       perrordef = ^terrordef;
       terrordef = object(tdef)
          constructor init;
          function  gettypename:string;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
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
          is_far : boolean;
          constructor init(def : pdef);
          constructor initfar(def : pdef);
          constructor load;
          procedure write;virtual;
          procedure deref;virtual;
          function  gettypename:string;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pobjectdef = ^tobjectdef;
       tobjectdef = object(tdef)
          childof  : pobjectdef;
          objname  : pstring;
          symtable : psymtable;
          objectoptions : tobjectoptions;
          { to be able to have a variable vmt position }
          { and no vmt field for objects without virtuals }
          vmt_offset : longint;
          constructor init(const n : string;c : pobjectdef);
          constructor load;
          destructor  done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  size : longint;virtual;
          function  alignment:longint;virtual;
          function  vmtmethodoffset(index:longint):longint;
          function  is_publishable : boolean;virtual;
          function  vmt_mangledname : string;
          function  rtti_name : string;
          procedure check_forwards;
          function  is_related(d : pobjectdef) : boolean;
          function  is_class : boolean;
          function  next_free_name_index : longint;
          procedure insertvmt;
          procedure set_parent(c : pobjectdef);
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
{$endif GDB}
          { init/final }
          function  needs_inittable : boolean;virtual;
          procedure write_init_data;virtual;
          procedure write_child_init_data;virtual;
          { rtti }
          function  get_rtti_label : string;virtual;
          procedure generate_rtti;virtual;
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
       end;

       pclassrefdef = ^tclassrefdef;
       tclassrefdef = object(tpointerdef)
          constructor init(def : pdef);
          constructor load;
          procedure write;virtual;
          function gettypename:string;virtual;
          { debug }
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
          function gettypename:string;virtual;
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
          function alignment : longint;virtual;
          { generates the ranges needed by the asm instruction BOUND (i386)
            or CMP2 (Motorola) }
          procedure genrangecheck;

          { returns the label of the range check string }
          function getrangecheckstring : string;
          function needs_inittable : boolean;virtual;
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
       end;

       precorddef = ^trecorddef;
       trecorddef = object(tdef)
          symtable : psymtable;
          constructor init(p : psymtable);
          constructor load;
          destructor done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  size:longint;virtual;
          function  alignment : longint;virtual;
          function  gettypename:string;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          { init/final }
          procedure write_init_data;virtual;
          procedure write_child_init_data;virtual;
          function  needs_inittable : boolean;virtual;
          { rtti }
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
       end;

       porddef = ^torddef;
       torddef = object(tdef)
          low,high : longint;
          rangenr  : longint;
          typ      : tbasetype;
          constructor init(t : tbasetype;v,b : longint);
          constructor load;
          procedure write;virtual;
          function  is_publishable : boolean;virtual;
          function  gettypename:string;virtual;
          procedure setsize;
          { generates the ranges needed by the asm instruction BOUND }
          { or CMP2 (Motorola)                                       }
          procedure genrangecheck;
          function  getrangecheckstring : string;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
{$endif GDB}
          { rtti }
          procedure write_rtti_data;virtual;
       end;

       pfloatdef = ^tfloatdef;
       tfloatdef = object(tdef)
          typ : tfloattype;
          constructor init(t : tfloattype);
          constructor load;
          procedure write;virtual;
          function  gettypename:string;virtual;
          function  is_publishable : boolean;virtual;
          procedure setsize;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
{$endif GDB}
          { rtti }
          procedure write_rtti_data;virtual;
       end;

       pabstractprocdef = ^tabstractprocdef;
       tabstractprocdef = object(tdef)
          { saves a definition to the return type }
          retdef          : pdef;
          proctypeoption  : tproctypeoption;
          proccalloptions : tproccalloptions;
          procoptions     : tprocoptions;
          para1           : pdefcoll;
          symtablelevel   : byte;
          fpu_used        : byte;    { how many stack fpu must be empty }
          constructor init;
          constructor load;
          destructor done;virtual;
          procedure  write;virtual;
          procedure deref;virtual;
          procedure concatdef(p : pdef;vsp : tvarspez);
          procedure concattypesym(p : ptypesym;vsp : tvarspez);
          function  para_size : longint;
          function  demangled_paras : string;
          procedure test_if_fpu_result;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pprocvardef = ^tprocvardef;
       tprocvardef = object(tabstractprocdef)
          constructor init;
          constructor load;
          procedure write;virtual;
          function  size : longint;virtual;
          function gettypename:string;virtual;
          function is_publishable : boolean;virtual;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput); virtual;
{$endif GDB}
          { rtti }
          procedure write_child_rtti_data;virtual;
          procedure write_rtti_data;virtual;
       end;

       tmessageinf = record
         case integer of
           0 : (str : pchar);
           1 : (i : longint);
       end;

       pprocdef = ^tprocdef;
       tprocdef = object(tabstractprocdef)
       private
          _mangledname : pchar;
       public
          extnumber  : longint;
          messageinf : tmessageinf;
          nextoverloaded : pprocdef;
          { where is this function defined, needed here because there
            is only one symbol for all overloaded functions }
          fileinfo : tfileposinfo;
          { pointer to the local symbol table }
          localst : psymtable;
          { pointer to the parameter symbol table }
          parast : psymtable;
          { browser info }
          lastref,
          defref,
          crossref,
          lastwritten : pref;
          refcount : longint;
          _class : pobjectdef;
          { it's a tree, but this not easy to handle }
          { used for inlined procs                   }
          code : pointer;
          { true, if the procedure is only declared }
          { (forward procedure) }
          forwarddef,
          { true if the procedure is declared in the interface }
          interfacedef : boolean;
          { check the problems of manglednames }
          count      : boolean;
          is_used    : boolean;
          { small set which contains the modified registers }
{$ifdef newcg}
          usedregisters : tregisterset;
{$else newcg}
          usedregisters : longint;
{$endif newcg}
          constructor init;
          constructor load;
          destructor  done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  haspara:boolean;
          function  mangledname : string;
          procedure setmangledname(const s : string);
          procedure load_references;
          function  write_references : boolean;
          function  procname: string;
          { debug }
{$ifdef GDB}
          function  cplusplusmangledname : string;
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          { browser }
{$ifdef BrowserLog}
          procedure add_to_browserlog;
{$endif BrowserLog}
       end;

       pstringdef = ^tstringdef;
       tstringdef = object(tdef)
          string_typ : tstringtype;
          len        : longint;
          constructor shortinit(l : byte);
          constructor shortload;
          constructor longinit(l : longint);
          constructor longload;
          constructor ansiinit(l : longint);
          constructor ansiload;
          constructor wideinit(l : longint);
          constructor wideload;
          function  stringtypname:string;
          function  size : longint;virtual;
          procedure write;virtual;
          function  gettypename:string;virtual;
          function  is_publishable : boolean;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          { init/final }
          function  needs_inittable : boolean;virtual;
          { rtti }
          procedure write_rtti_data;virtual;
       end;

       penumdef = ^tenumdef;
       tenumdef = object(tdef)
          rangenr,
          minval,
          maxval    : longint;
          has_jumps : boolean;
          firstenum : penumsym;
          basedef   : penumdef;
          constructor init;
          constructor init_subrange(_basedef:penumdef;_min,_max:longint);
          constructor load;
          destructor done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  gettypename:string;virtual;
          function  is_publishable : boolean;virtual;
          procedure calcsavesize;
          procedure setmax(_max:longint);
          procedure setmin(_min:longint);
          function  min:longint;
          function  max:longint;
          function  getrangecheckstring:string;
          procedure genrangecheck;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
{$endif GDB}
          { rtti }
          procedure write_child_rtti_data;virtual;
          procedure write_rtti_data;virtual;
       end;

       psetdef = ^tsetdef;
       tsetdef = object(tdef)
          setof   : pdef;
          settype : tsettype;
          constructor init(s : pdef;high : longint);
          constructor load;
          procedure write;virtual;
          procedure deref;virtual;
          function  gettypename:string;virtual;
          function  is_publishable : boolean;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
          { rtti }
          procedure write_rtti_data;virtual;
          procedure write_child_rtti_data;virtual;
       end;

{
  $Log$
  Revision 1.40  1999-08-09 22:19:57  peter
    * classes vmt changed to only positive addresses
    * sharedlib creation is working

  Revision 1.39  1999/08/07 14:21:02  florian
    * some small problems fixed

  Revision 1.38  1999/08/05 16:53:15  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.37  1999/08/03 22:03:16  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.36  1999/08/02 21:29:04  florian
    * the main branch psub.pas is now used for
      newcg compiler

  Revision 1.35  1999/07/27 23:42:20  peter
    * indirect type referencing is now allowed

  Revision 1.34  1999/07/23 16:05:30  peter
    * alignment is now saved in the symtable
    * C alignment added for records
    * PPU version increased to solve .12 <-> .13 probs

  Revision 1.33  1999/06/22 16:24:45  pierre
   * local browser stuff corrected

  Revision 1.32  1999/06/02 10:11:51  florian
    * make cycle fixed i.e. compilation with 0.99.10
    * some fixes for qword
    * start of register calling conventions

  Revision 1.31  1999/05/31 16:42:35  peter
    * interfacedef flag for procdef if it's defined in the interface, to
      make a difference with 'forward;' directive forwarddef. Fixes 253

  Revision 1.30  1999/05/27 19:45:04  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.29  1999/05/23 18:42:15  florian
    * better error recovering in typed constants
    * some problems with arrays of const fixed, some problems
      due my previous
       - the location type of array constructor is now LOC_MEM
       - the pushing of high fixed
       - parameter copying fixed
       - zero temp. allocation removed
    * small problem in the assembler writers fixed:
      ref to nil wasn't written correctly

  Revision 1.28  1999/05/19 16:48:28  florian
    * tdef.typename: returns a now a proper type name for the most types

  Revision 1.27  1999/05/13 21:59:42  peter
    * removed oldppu code
    * warning if objpas is loaded from uses
    * first things for new deref writing

  Revision 1.26  1999/05/12 00:19:59  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.25  1999/05/08 19:52:37  peter
    + MessagePos() which is enhanced Message() function but also gets the
      position info
    * Removed comp warnings

  Revision 1.24  1999/05/06 09:05:30  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.23  1999/04/26 18:30:02  peter
    * farpointerdef moved into pointerdef.is_far

  Revision 1.22  1999/04/26 13:31:49  peter
    * release storenumber,double_checksum

  Revision 1.21  1999/04/21 09:43:52  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.20  1999/04/14 09:15:00  peter
    * first things to store the symbol/def number in the ppu

  Revision 1.19  1999/04/08 15:57:52  peter
    + subrange checking for readln()

  Revision 1.18  1999/03/02 18:24:21  peter
    * fixed overloading of array of char

  Revision 1.17  1999/03/01 13:45:06  pierre
   + added staticppusymtable symtable type for local browsing

  Revision 1.16  1999/02/22 20:13:39  florian
    + first implementation of message keyword

  Revision 1.15  1999/01/20 14:18:40  pierre
    * bugs related to mangledname solved
      - linux external without name
      -external procs already used
      (added count and is_used boolean fiels in tprocvar)

  Revision 1.14  1999/01/12 14:25:33  peter
    + BrowserLog for browser.log generation
    + BrowserCol for browser info in TCollections
    * released all other UseBrowser

  Revision 1.13  1998/12/30 22:15:53  peter
    + farpointer type
    * absolutesym now also stores if its far

  Revision 1.12  1998/12/10 09:47:28  florian
    + basic operations with int64/qord (compiler with -dint64)
    + rtti of enumerations extended: names are now written

  Revision 1.11  1998/11/29 21:45:49  florian
    * problem with arrays with init tables fixed

  Revision 1.10  1998/11/20 15:36:00  florian
    * problems with rtti fixed, hope it works

  Revision 1.9  1998/11/10 10:09:14  peter
    * va_list -> array of const

  Revision 1.8  1998/11/09 11:44:37  peter
    + va_list for printf support

  Revision 1.7  1998/11/05 12:02:59  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.6  1998/10/22 17:11:23  pierre
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
