{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl, Pierre Muller

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

{$i fpcdefs.inc}

interface

uses
  globtype;

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
{$ifdef ansistring_bits}
  tkA32String  = 9;
{$else}
  tkAString  = 9;
{$endif}
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
  tkDynArray = 21;
  tkInterfaceCorba = 22;
{$ifdef ansistring_bits}
  tkA16string = 23;
  tkA64string = 24;
{$endif}

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
  ftFloat128 = 5;

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

  unknown_level         = 0;
  main_program_level    = 1;
  normal_function_level = 2;


type
  { Deref entry options }
  tdereftype = (deref_nil,
    deref_sym,
    deref_def,
    deref_aktrecord,
    deref_aktstatic,
    deref_aktglobal,
    deref_aktlocal,
    deref_aktpara,
    deref_unit,
    deref_record,
    deref_local,
    deref_para,
    deref_parent_object
  );

  { symbol options }
  tsymoption=(sp_none,
    sp_public,
    sp_private,
    sp_published,
    sp_protected,
    sp_static,
    sp_hint_deprecated,
    sp_hint_platform,
    sp_hint_library,
    sp_hint_unimplemented,
    sp_has_overloaded,
    sp_internal  { internal symbol, not reported as unused }
  );
  tsymoptions=set of tsymoption;

  { flags for a definition }
  tdefoption=(df_none,
    { init data has been generated }
    df_has_inittable,
    { rtti data has been generated }
    df_has_rttitable,
    { type is unique, i.e. declared with type = type <tdef>; }
    df_unique
  );
  tdefoptions=set of tdefoption;

  { tsymlist entry types }
  tsltype = (sl_none,
    sl_load,
    sl_call,
    sl_subscript,
    sl_vec,
    sl_typeconv
  );

  { base types for orddef }
  tbasetype = (
    uvoid,
    u8bit,u16bit,u32bit,u64bit,
    s8bit,s16bit,s32bit,s64bit,
    bool8bit,bool16bit,bool32bit,
    uchar,uwidechar,scurrency
  );

  { float types }
  tfloattype = (
    s32real,s64real,s80real,
    s64comp,s64currency,s128real
  );

  { string types }
  tstringtype = (st_default,
    st_shortstring,
    st_longstring,
  {$ifndef ansistring_bits}
    st_ansistring,
  {$else}
    st_ansistring16,
    st_ansistring32,
    st_ansistring64,
  {$endif}
    st_widestring
  );

  { set types }
  tsettype = (
    normset,smallset,varset
  );

  tvarianttype = (
    vt_normalvariant,vt_olevariant
  );

  tcallercallee = (callerside,calleeside);

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
    po_interrupt,         { Procedure is an interrupt handler }
    po_iocheck,           { IO checking should be done after a call to the procedure }
    po_assembler,         { Procedure is written in assembler }
    po_msgstr,            { method for string message handling }
    po_msgint,            { method for int message handling }
    po_exports,           { Procedure has export directive (needed for OS/2) }
    po_external,          { Procedure is external (in other object or lib)}
    po_saveregisters,     { save all registers }
    po_overload,          { procedure is declared with overload directive }
    po_varargs,           { printf like arguments }
    po_internconst,       { procedure has constant evaluator intern }
    { flag that only the address of a method is returned and not a full methodpointer }
    po_addressonly,
    { procedure is exported }
    po_public,
    { calling convention is specified explicitly }
    po_hascallingconvention,
    { reintroduce flag }
    po_reintroduce,
    { location of parameters is given explicitly as it is necessary for some syscall
      conventions like that one of MorphOS }
    po_explicitparaloc,
    { no stackframe will be generated, used by lowlevel assembler like get_frame }
    po_nostackframe,
    { localst is valid }
    po_haslocalst
  );
  tprocoptions=set of tprocoption;

  { options for objects and classes }
  tobjectdeftype = (odt_none,
    odt_class,
    odt_object,
    odt_interfacecom,
    odt_interfacecorba,
    odt_cppclass,
    odt_dispinterface
  );

  { options for objects and classes }
  tobjectoption=(oo_none,
    oo_is_forward,         { the class is only a forward declared yet }
    oo_has_virtual,        { the object/class has virtual methods }
    oo_has_private,
    oo_has_protected,
    oo_has_constructor,    { the object/class has a constructor }
    oo_has_destructor,     { the object/class has a destructor }
    oo_has_vmt,            { the object/class has a vmt }
    oo_has_msgstr,
    oo_has_msgint,
    oo_can_have_published { the class has rtti, i.e. you can publish properties }
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
    vo_has_local_copy,
    vo_is_const,  { variable is declared as const (parameter) and can't be written to }
    vo_is_exported,
    vo_is_high_value,
    vo_is_funcret,
    vo_is_self,
    vo_is_vmt,
    vo_is_result,  { special result variable }
    vo_is_reg_para, { register parameter, no space allocation in parast, but in localst }
    vo_is_parentfp
  );
  tvaroptions=set of tvaroption;

  { types of the symtables }
  tsymtabletype = (abstractsymtable,
    globalsymtable,staticsymtable,
    objectsymtable,recordsymtable,
    localsymtable,parasymtable,
    withsymtable,stt_exceptsymtable
  );


  { definition contains the informations about a type }
  tdeftype = (abstractdef,arraydef,recorddef,pointerdef,orddef,
              stringdef,enumdef,procdef,objectdef,errordef,
              filedef,formaldef,setdef,procvardef,floatdef,
              classrefdef,forwarddef,variantdef);

  { possible types for symtable entries }
  tsymtyp = (abstractsym,varsym,typesym,procsym,unitsym,
             constsym,enumsym,typedconstsym,errorsym,syssym,
             labelsym,absolutesym,propertysym,macrosym,rttisym);

  { State of the variable, if it's declared, assigned or used }
  tvarstate=(vs_none,
    vs_declared,vs_assigned,vs_used
  );

  tvarspez = (vs_value,vs_const,vs_var,vs_out);

  absolutetyp = (tovar,toasm,toaddr);

  tconsttyp = (constnone,
    constord,conststring,constreal,
    constset,constpointer,constnil,
    constresourcestring,constwstring,constguid
  );

  { RTTI information to store }
  trttitype = (
    fullrtti,initrtti
  );

  { The order is from low priority to high priority,
    Note: the operators > and < are used on this list }
  tequaltype = (
    te_incompatible,
    te_convert_operator,
    te_convert_l3,     { compatible conversion with possible loss of data }
    te_convert_l2,     { compatible less prefered conversion }
    te_convert_l1,     { compatible conversion     }
    te_equal,          { the definitions are equal }
    te_exact
  );

{$ifdef GDB}
type
  tdefstabstatus = (
    stab_state_unused,
    stab_state_used,
    stab_state_writing,
    stab_state_written
  );

const
  tagtypes : Set of tdeftype =
    [recorddef,enumdef,
    {$IfNDef GDBKnowsStrings}
    stringdef,
    {$EndIf not GDBKnowsStrings}
    {$IfNDef GDBKnowsFiles}
    filedef,
    {$EndIf not GDBKnowsFiles}
    objectdef];
{$endif GDB}


const
   savestdregs_pocalls = [
     pocall_cdecl,pocall_cppdecl,pocall_syscall,
     pocall_stdcall,pocall_safecall,pocall_compilerproc,
     pocall_register,pocall_softfloat
   ];

   clearstack_pocalls = [
     pocall_cdecl,pocall_cppdecl,pocall_syscall
   ];

   pushleftright_pocalls : tproccalloptions = [pocall_register,pocall_pascal];

     SymTypeName : array[tsymtyp] of string[12] = (
       'abstractsym','variable','type','proc','unit',
       'const','enum','typed const','errorsym','system sym',
       'label','absolute','property','macrosym','rttisym'
     );

     DefTypeName : array[tdeftype] of string[12] = (
       'abstractdef','arraydef','recorddef','pointerdef','orddef',
       'stringdef','enumdef','procdef','objectdef','errordef',
       'filedef','formaldef','setdef','procvardef','floatdef',
       'classrefdef','forwarddef','variantdef'
     );

     EqualTypeName : array[tequaltype] of string[16] = (
       'incompatible','convert_operator','convert_l3','convert_l2',
       'convert_l1','equal','exact'
     );

implementation

initialization
  if pocall_default in [pocall_register,pocall_internproc] then
    include(pushleftright_pocalls,pocall_compilerproc);

end.
{
  $Log$
  Revision 1.85  2004-07-06 19:52:04  peter
    * fix storing of localst in ppu

  Revision 1.84  2004/06/20 08:55:30  florian
    * logs truncated

  Revision 1.83  2004/06/16 20:07:09  florian
    * dwarf branch merged

  Revision 1.82  2004/05/23 14:32:17  peter
    * tprocinfoflag moved to globtype

  Revision 1.81  2004/04/29 19:56:37  daniel
    * Prepare compiler infrastructure for multiple ansistring types

  Revision 1.80  2004/04/28 15:19:03  florian
    + syscall directive support for MorphOS added

  Revision 1.79  2004/04/18 15:22:24  florian
    + location support for arguments, currently PowerPC/MorphOS only

}
