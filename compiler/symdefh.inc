{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

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

       tdef = object(tsymtableentry)
          deftype    : tdeftype;
          typesym    : ptypesym;  { which type the definition was generated this def }

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
          is_def_stab_written : tdefstabstatus;
{$endif GDB}
          constructor init;
          constructor load;
          destructor  done;virtual;
          procedure deref;virtual;
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
          procedure set_globalnb;virtual;
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

       tvarspez = (vs_value,vs_const,vs_var,vs_out);

       pparaitem = ^tparaitem;
       tparaitem = object(tlinkedlist_item)
          paratype     : ttype;
          paratyp      : tvarspez;
          argconvtyp   : targconvtyp;
          convertlevel : byte;
          register     : tregister;
          defaultvalue : psym; { pconstsym }
       end;

       { this is only here to override the count method,
         which can't be used }
       pparalinkedlist = ^tparalinkedlist;
       tparalinkedlist = object(tlinkedlist)
          function count:longint;
       end;

       tfiletyp = (ft_text,ft_typed,ft_untyped);

       pfiledef = ^tfiledef;
       tfiledef = object(tdef)
          filetyp : tfiletyp;
          typedfiletype : ttype;
          constructor inittext;
          constructor inituntyped;
          constructor inittyped(const tt : ttype);
          constructor inittypeddef(p : pdef);
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

       pforwarddef = ^tforwarddef;
       tforwarddef = object(tdef)
          tosymname : string;
          forwardpos : tfileposinfo;
          constructor init(const s:string;const pos : tfileposinfo);
          function  gettypename:string;virtual;
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
          pointertype : ttype;
          is_far : boolean;
          constructor init(const tt : ttype);
          constructor initfar(const tt : ttype);
          constructor initdef(p : pdef);
          constructor initfardef(p : pdef);
          constructor load;
          destructor  done;virtual;
          procedure write;virtual;
          procedure deref;virtual;
          function  gettypename:string;virtual;
          { debug }
{$ifdef GDB}
          function  stabstring : pchar;virtual;
          procedure concatstabto(asmlist : paasmoutput);virtual;
{$endif GDB}
       end;

       pprocdef = ^tprocdef;

       pobjectdef = ^tobjectdef;
       tobjectdef = object(tdef)
          childof  : pobjectdef;
          objname  : pstring;
          symtable : psymtable;
          objectoptions : tobjectoptions;
          { to be able to have a variable vmt position }
          { and no vmt field for objects without virtuals }
          vmt_offset : longint;
{$ifdef GDB}
          classglobalnb,
          classptrglobalnb : word;
          writing_stabs : boolean;
{$endif GDB}
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
          function  is_interface : boolean;
          function  is_cppclass : boolean;
          function  is_object : boolean;
          function  next_free_name_index : longint;
          procedure insertvmt;
          procedure set_parent(c : pobjectdef);
          function searchdestructor : pprocdef;
          { debug }
{$ifdef GDB}
          function stabstring : pchar;virtual;
          procedure set_globalnb;virtual;
          function  classnumberstring : string;
          function  classptrnumberstring : string;
          procedure concatstabto(asmlist : paasmoutput);virtual;
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
          function generate_field_table : pasmlabel;
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
          elementtype,
          rangetype  : ttype;
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
        private
          rangenr  : longint;
        public
          low,high : longint;
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
          rettype         : ttype;
          proctypeoption  : tproctypeoption;
          proccalloptions : tproccalloptions;
          procoptions     : tprocoptions;
          para            : pparalinkedlist;
          maxparacount,
          minparacount    : longint;
          symtablelevel   : byte;
          fpu_used        : byte;    { how many stack fpu must be empty }
          constructor init;
          constructor load;
          destructor done;virtual;
          procedure  write;virtual;
          procedure deref;virtual;
          procedure concatpara(tt:ttype;vsp : tvarspez;defval:psym);
          function  para_size(alignsize:longint) : longint;
          function  demangled_paras : string;
          function  proccalloption2str : string;
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

       tprocdef = object(tabstractprocdef)
       private
          _mangledname : pstring;
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
          { symbol owning this definition }
          procsym : pprocsym;
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
          { info about register variables (JM) }
          regvarinfo: pointer;
          { true, if the procedure is only declared }
          { (forward procedure) }
          forwarddef,
          { true if the procedure is declared in the interface }
          interfacedef : boolean;
          { true if the procedure has a forward declaration }
          hasforward : boolean;
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
{$ifdef dummy}
          function  procname: string;
{$endif dummy}
          function  cplusplusmangledname(const rn : string) : string;
          { debug }
{$ifdef GDB}
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
          elementtype : ttype;
          settype : tsettype;
          constructor init(s : pdef;high : longint);
          constructor load;
          destructor  done;virtual;
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
  Revision 1.12  2000-10-15 07:47:52  peter
    * unit names and procedure names are stored mixed case

  Revision 1.11  2000/10/14 10:14:53  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.10  2000/09/24 15:06:29  peter
    * use defines.inc

  Revision 1.9  2000/09/19 23:08:03  pierre
   * fixes for local class debuggging problem (merged)

  Revision 1.8  2000/08/21 11:27:44  pierre
   * fix the stabs problems

  Revision 1.7  2000/08/06 19:39:28  peter
    * default parameters working !

  Revision 1.6  2000/08/06 14:17:15  peter
    * overload fixes (merged)

  Revision 1.5  2000/08/03 13:17:26  jonas
    + allow regvars to be used inside inlined procs, which required  the
      following changes:
        + load regvars in genentrycode/free them in genexitcode (cgai386)
        * moved all regvar related code to new regvars unit
        + added pregvarinfo type to hcodegen
        + added regvarinfo field to tprocinfo (symdef/symdefh)
        * deallocate the regvars of the caller in secondprocinline before
          inlining the called procedure and reallocate them afterwards

  Revision 1.4  2000/08/02 19:49:59  peter
    * first things for default parameters

  Revision 1.3  2000/07/13 12:08:27  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:49  michael
  + removed logs

}