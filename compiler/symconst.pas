{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

    Symbol table constants

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
unit symconst;
interface

{$ifdef FPC}
  {$ifdef PACKENUMFIXED}
    {$PACKENUM 1}
  {$endif}
{$endif}

const
  def_alignment = 4;

  { if you change one of the following contants, }
  { you have also to change the typinfo unit}
  { and the rtl/i386,template/rttip.inc files    }
  tkUnknown  = 0;
  tkInteger  = 1;
  tkChar     = 2;
  tkEnumeration   = 3;
  tkFloat    = 4;
  tkSet      = 5;
  tkMethod   = 6;
  tkSString  = 7;
  tkString   = tkSString;
  tkLString  = 8;
  tkAString  = 9;
  tkWString  = 10;
  tkVariant  = 11;
  tkArray    = 12;
  tkRecord   = 13;
  tkInterface= 14;
  tkClass    = 15;
  tkObject   = 16;
  tkWChar    = 17;
  tkBool     = 18;
  tkInt64    = 19;
  tkQWord    = 20;

  otSByte    = 0;
  otUByte    = 1;
  otSWord    = 2;
  otUWord    = 3;
  otSLong    = 4;
  otULong    = 5;

  ftSingle   = 0;
  ftDouble   = 1;
  ftExtended = 2;
  ftComp     = 3;
  ftCurr     = 4;
  ftFixed16  = 5;
  ftFixed32  = 6;

  mkProcedure= 0;
  mkFunction = 1;
  mkConstructor   = 2;
  mkDestructor    = 3;
  mkClassProcedure= 4;
  mkClassFunction = 5;

  pfvar      = 1;
  pfConst    = 2;
  pfArray    = 4;
  pfAddress  = 8;
  pfReference= 16;
  pfOut      = 32;


type
  { symbol options }
  tsymoption=(sp_none,
    sp_public,
    sp_private,
    sp_published,
    sp_protected,
    sp_static,
    sp_primary_typesym    { this is for typesym, to know who is the primary symbol of a def }
  );
  tsymoptions=set of tsymoption;

  { flags for a definition }
  tdefoption=(df_none,
    df_need_rtti,          { the definitions needs rtti }
    df_has_rtti            { the rtti is generated      }
  );
  tdefoptions=set of tdefoption;

  { base types for orddef }
  tbasetype = (
    uauto,uvoid,uchar,
    u8bit,u16bit,u32bit,
    s8bit,s16bit,s32bit,
    bool8bit,bool16bit,bool32bit,
    u64bit,s64bit,uwidechar
  );

  { float types }
  tfloattype = (
    s32real,s64real,s80real,
    s64comp,
    f16bit,f32bit
  );

  { string types }
  tstringtype = (st_default,
    st_shortstring, st_longstring, st_ansistring, st_widestring
  );

  { set types }
  tsettype = (
    normset,smallset,varset
  );

  { calling convention for tprocdef and tprocvardef }
  tproccalloption=(pocall_none,
    pocall_clearstack,    { Use IBM flat calling convention. (Used by GCC.) }
    pocall_leftright,     { Push parameters from left to right }
    pocall_cdecl,         { procedure uses C styled calling }
    pocall_register,      { procedure uses register (fastcall) calling }
    pocall_stdcall,       { procedure uses stdcall call }
    pocall_safecall,      { safe call calling conventions }
    pocall_palmossyscall, { procedure is a PalmOS system call }
    pocall_system,
    pocall_inline,        { Procedure is an assembler macro }
    pocall_internproc,    { Procedure has compiler magic}
    pocall_internconst    { procedure has constant evaluator intern }
  );
  tproccalloptions=set of tproccalloption;

  { basic type for tprocdef and tprocvardef }
  tproctypeoption=(potype_none,
    potype_proginit,     { Program initialization }
    potype_unitinit,     { unit initialization }
    potype_unitfinalize, { unit finalization }
    potype_constructor,  { Procedure is a constructor }
    potype_destructor,   { Procedure is a destructor }
    potype_operator      { Procedure defines an operator }
  );
  tproctypeoptions=set of tproctypeoption;

  { other options for tprocdef and tprocvardef }
  tprocoption=(po_none,
    po_classmethod,       { class method }
    po_virtualmethod,     { Procedure is a virtual method }
    po_abstractmethod,    { Procedure is an abstract method }
    po_staticmethod,      { static method }
    po_overridingmethod,  { method with override directive }
    po_methodpointer,     { method pointer, only in procvardef, also used for 'with object do' }
    po_containsself,      { self is passed explicit to the compiler }
    po_interrupt,         { Procedure is an interrupt handler }
    po_iocheck,           { IO checking should be done after a call to the procedure }
    po_assembler,         { Procedure is written in assembler }
    po_msgstr,            { method for string message handling }
    po_msgint,            { method for int message handling }
    po_exports,           { Procedure has export directive (needed for OS/2) }
    po_external,          { Procedure is external (in other object or lib)}
    po_savestdregs,       { save std regs cdecl and stdcall need that ! }
    po_saveregisters,     { save all registers }
    po_overload           { procedure is declared with overload directive }
  );
  tprocoptions=set of tprocoption;

  { options for objects and classes }
  tobjectoption=(oo_none,
    oo_is_class,
    oo_is_forward,         { the class is only a forward declared yet }
    oo_has_virtual,        { the object/class has virtual methods }
    oo_has_private,
    oo_has_protected,
    oo_has_constructor,    { the object/class has a constructor }
    oo_has_destructor,     { the object/class has a destructor }
    oo_has_vmt,            { the object/class has a vmt }
    oo_has_msgstr,
    oo_has_msgint,
    oo_has_abstract,       { the object/class has an abstract method => no instances can be created }
    oo_can_have_published, { the class has rtti, i.e. you can publish properties }
    oo_is_cppclass,        { the object/class uses an C++ compatible }
                           { class layout }
    oo_is_interface        { delphi styled interface }
  );

  tobjectoptions=set of tobjectoption;

  { options for properties }
  tpropertyoption=(ppo_none,
    ppo_indexed,
    ppo_defaultproperty,
    ppo_stored,
    ppo_hasparameters,
    ppo_is_override
  );
  tpropertyoptions=set of tpropertyoption;

  { options for variables }
  tvaroption=(vo_none,
    vo_regable,
    vo_is_C_var,
    vo_is_external,
    vo_is_dll_var,
    vo_is_thread_var,
    vo_fpuregable,
    vo_is_local_copy,
    vo_is_const,  { variable is declared as const (parameter) and can't be written to }
    vo_is_exported
  );
  tvaroptions=set of tvaroption;

  { definition contains the informations about a type }
  tdeftype = (abstractdef,arraydef,recorddef,pointerdef,orddef,
              stringdef,enumdef,procdef,objectdef,errordef,
              filedef,formaldef,setdef,procvardef,floatdef,
              classrefdef,forwarddef);

  { possible types for symtable entries }
  tsymtyp = (abstractsym,varsym,typesym,procsym,unitsym,programsym,
             constsym,enumsym,typedconstsym,errorsym,syssym,
             labelsym,absolutesym,propertysym,funcretsym,
             macrosym);

  { State of the variable, if it's declared, assigned or used }
  tvarstate=(vs_none,
    vs_declared,vs_declared_and_first_found,
    vs_set_but_first_not_passed,vs_assigned,vs_used
  );

  absolutetyp = (tovar,toasm,toaddr);

  tconsttyp = (constnone,
    constord,conststring,constreal,constbool,
    constint,constchar,constset,constpointer,constnil,
    constresourcestring
  );

{$ifdef GDB}
  tdefstabstatus = (
    not_written,
    being_written,
    written);
{$endif GDB}

const
  { relevant options for assigning a proc or a procvar to a procvar }
  po_compatibility_options = [
    po_classmethod,
    po_staticmethod,
    po_methodpointer,
    po_containsself,
    po_interrupt,
    po_iocheck,
    po_exports
  ];

const
     SymTypeName : array[tsymtyp] of string[12] =
     ('abstractsym','variable','type','proc','unit','program',
      'const','enum','typed const','errorsym','system sym',
      'label','absolute','property','funcret',
      'macrosym');

implementation

end.
{
  $Log$
  Revision 1.6  2000-08-21 11:27:44  pierre
   * fix the stabs problems

  Revision 1.5  2000/08/06 19:39:28  peter
    * default parameters working !

  Revision 1.4  2000/08/05 13:25:06  peter
    * packenum 1 fixes (merged)

  Revision 1.3  2000/07/13 12:08:27  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:49  michael
  + removed logs

}
