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
  tkDynArray = 21;
  tkInterfaceCorba = 22;

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

  {# These are the different possible base values that can
     be taken from the lexlevel variable when parsing. The
     lexlevel can be bigger if parsding recursive routines.
  }
  main_program_level = 1;
  unit_init_level = 1;
  normal_function_level = 2;


type
  { Deref entry options }
  tdereftype = (derefnil,
    derefaktrecordindex,
    derefaktstaticindex,
    derefunit,
    derefrecord,
    derefindex,
    dereflocal,
    derefpara,
    derefaktlocal
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
    sp_has_overloaded
  );
  tsymoptions=set of tsymoption;

  { flags for a definition }
  tdefoption=(df_none,
    { init data has been generated }
    df_has_inittable,
    { rtti data has been generated }
    df_has_rttitable
  );
  tdefoptions=set of tdefoption;

  { tsymlist entry types }
  tsltype = (sl_none,
    sl_load,
    sl_call,
    sl_subscript,
    sl_vec
  );

  { base types for orddef }
  tbasetype = (
    uvoid,
    u8bit,u16bit,u32bit,u64bit,
    s8bit,s16bit,s32bit,s64bit,
    bool8bit,bool16bit,bool32bit,
    uchar,uwidechar
  );

  { float types }
  tfloattype = (
    s32real,s64real,s80real,
    s64comp,s64currency
  );

  { string types }
  tstringtype = (st_default,
    st_shortstring, st_longstring, st_ansistring, st_widestring
  );

  { set types }
  tsettype = (
    normset,smallset,varset
  );

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
    po_overload,          { procedure is declared with overload directive }
    po_varargs,           { printf like arguments }
    po_leftright,         { push arguments from left to right }
    po_clearstack,        { caller clears the stack }
    po_internconst        { procedure has constant evaluator intern }
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
    oo_has_abstract,       { the object/class has an abstract method => no instances can be created }
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
    vo_is_local_copy,
    vo_is_const,  { variable is declared as const (parameter) and can't be written to }
    vo_is_exported
  );
  tvaroptions=set of tvaroption;

  { types of the symtables }
  tsymtabletype = (abstractsymtable,
    globalsymtable,staticsymtable,
    objectsymtable,recordsymtable,
    localsymtable,parasymtable,
    withsymtable,stt_exceptsymtable,
    { used for inline detection }
    inlineparasymtable,inlinelocalsymtable
  );


  { definition contains the informations about a type }
  tdeftype = (abstractdef,arraydef,recorddef,pointerdef,orddef,
              stringdef,enumdef,procdef,objectdef,errordef,
              filedef,formaldef,setdef,procvardef,floatdef,
              classrefdef,forwarddef,variantdef);

  { possible types for symtable entries }
  tsymtyp = (abstractsym,varsym,typesym,procsym,unitsym,
             constsym,enumsym,typedconstsym,errorsym,syssym,
             labelsym,absolutesym,propertysym,funcretsym,
             macrosym,rttisym);

  { State of the variable, if it's declared, assigned or used }
  tvarstate=(vs_none,
    vs_declared,vs_declared_and_first_found,
    vs_set_but_first_not_passed,vs_assigned,vs_used
  );

  tvarspez = (vs_value,vs_const,vs_var,vs_out,vs_hidden);

  targconvtyp = (act_convertable,act_equal,act_exact);

  absolutetyp = (tovar,toasm,toaddr);

  tconsttyp = (constnone,
    constord,conststring,constreal,constbool,
    constint,constchar,constset,constpointer,constnil,
    constresourcestring,constwstring,constwchar,constguid
  );

  { RTTI information to store }
  trttitype = (
    fullrtti,initrtti
  );

{$ifdef GDB}
type
  tdefstabstatus = (
    not_written,
    being_written,
    written);

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
  { relevant options for assigning a proc or a procvar to a procvar }
  po_compatibility_options = [
    po_classmethod,
    po_staticmethod,
    po_methodpointer,
    po_containsself,
    po_interrupt,
    po_iocheck,
    po_varargs,
    po_exports
  ];

const
     SymTypeName : array[tsymtyp] of string[12] =
     ('abstractsym','variable','type','proc','unit',
      'const','enum','typed const','errorsym','system sym',
      'label','absolute','property','funcret',
      'macrosym','rttisym');

implementation

end.
{
  $Log$
  Revision 1.39  2002-12-15 11:26:02  peter
    * ignore vs_hidden parameters when choosing overloaded proc

  Revision 1.38  2002/12/05 14:44:38  florian
    + oo_dispinterface added

  Revision 1.37  2002/11/29 22:31:20  carl
    + unimplemented hint directive added
    * hint directive parsing implemented
    * warning on these directives

  Revision 1.36  2002/10/20 15:34:16  peter
    * removed df_unique flag. It breaks code. For a good type=type <id>
      a def copy is required

  Revision 1.35  2002/10/06 12:25:05  florian
    + proper support of type <id> = type <another id>;

  Revision 1.34  2002/08/19 19:36:44  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.33  2002/07/01 16:23:54  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.32  2002/05/18 13:34:18  peter
    * readded missing revisions

  Revision 1.31  2002/05/16 19:46:44  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.30  2002/05/14 19:34:50  peter
    * removed old logs and updated copyright year

  Revision 1.29  2002/05/12 16:53:10  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.28  2002/01/06 12:08:15  peter
    * removed uauto from orddef, use new range_to_basetype generating
      the correct ordinal type for a range

}
