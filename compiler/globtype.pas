{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

    Global types

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
unit globtype;
interface

{$ifdef FPC}
  {$ifdef PACKENUMFIXED}
    {$PACKENUM 1}
  {$endif}
{$endif}

    const
       maxidlen = 64;

    type
       { System independent float names }
{$ifdef i386}
       bestreal = extended;
       ts32real = single;
       ts64real = double;
       ts80real = extended;
       ts64comp = extended;
{$endif}
{$ifdef m68k}
       bestreal = real;
       ts32real = single;
       ts64real = double;
       ts80real = extended;
       ts64comp = comp;
{$endif}
{$ifdef alpha}
       bestreal = extended;
       ts32real = single;
       ts64real = double;
       ts80real = extended;
       ts64comp = comp;
{$endif}
{$ifdef powerpc}
       bestreal = double;
       ts32real = single;
       ts64real = double;
       ts80real = extended;
       ts64comp = comp;
{$endif powerpc}
       pbestreal=^bestreal;

       { Switches which can be changed locally }
       tlocalswitch = (cs_localnone,
         { codegen }
         cs_check_overflow,cs_check_range,cs_check_object_ext,
         cs_check_io,cs_check_stack,
         cs_omitstackframe,cs_do_assertion,cs_generate_rtti,
         cs_full_boolean_eval,
         { mmx }
         cs_mmx,cs_mmx_saturation,
         { parser }
         cs_typed_addresses,cs_strict_var_strings,cs_ansistrings
       );
       tlocalswitches = set of tlocalswitch;

       { Switches which can be changed only at the beginning of a new module }
       tmoduleswitch = (cs_modulenone,
         { parser }
         cs_fp_emulation,cs_extsyntax,cs_openstring,
         { support }
         cs_support_inline,cs_support_goto,cs_support_macro,
         cs_support_c_operators,cs_static_keyword,
         cs_typed_const_not_changeable,
         { generation }
         cs_profile,cs_debuginfo,cs_browser,cs_local_browser,cs_compilesystem,
         cs_lineinfo,
         { linking }
         cs_create_smart,cs_create_dynamic
       );
       tmoduleswitches = set of tmoduleswitch;

       { Switches which can be changed only for a whole program/compilation,
         mostly set with commandline }
       tglobalswitch = (cs_globalnone,
         { parameter switches }
         cs_check_unit_name,cs_constructor_name,
         { units }
         cs_load_objpas_unit,
         cs_load_gpc_unit,
         { optimizer }
         cs_regalloc,cs_uncertainopts,cs_littlesize,cs_optimize,
         cs_fastoptimize, cs_slowoptimize,cs_align,
         { browser }
         cs_browser_log,
         { debugger }
         cs_gdb_dbx,cs_gdb_gsym,cs_gdb_heaptrc,cs_gdb_lineinfo,cs_checkpointer,
         { assembling }
         cs_asm_leave,cs_asm_extern,cs_asm_pipe,cs_asm_source,
         cs_asm_regalloc,cs_asm_tempalloc,
         { linking }
         cs_link_extern,cs_link_static,cs_link_smart,cs_link_shared,cs_link_deffile,
         cs_link_strip,cs_link_toc,cs_link_staticflag
       );
       tglobalswitches = set of tglobalswitch;

       { Switches which can be changed by a mode (fpc,tp7,delphi) }
       tmodeswitch = (m_none,m_all, { needed for keyword }
         { generic }
         m_fpc,m_objfpc,m_delphi,m_tp,m_tp7,m_gpc,
         { more specific }
         m_class,               { delphi class model }
         m_objpas,              { load objpas unit }
         m_result,              { result in functions }
         m_string_pchar,        { pchar 2 string conversion }
         m_cvar_support,        { cvar variable directive }
         m_nested_comment,      { nested comments }
         m_tp_procvar,          { tp style procvars (no @ needed) }
         m_repeat_forward,      { repeating forward declarations is needed }
         m_pointer_2_procedure, { allows the assignement of pointers to
                                  procedure variables                     }
         m_autoderef,           { does auto dereferencing of struct. vars }
         m_initfinal,           { initialization/finalization for units }
         m_add_pointer,         { allow pointer add/sub operations }
         m_default_ansistring,  { ansistring turned on by default }
         m_out,                 { support the calling convention OUT }
         m_default_para         { support default parameters }
       );
       tmodeswitches = set of tmodeswitch;

       { win32 sub system }
       tapptype = (at_none,
         at_gui,at_cui
       );

       { currently parsed block type }
       tblock_type = (bt_none,
         bt_general,bt_type,bt_const,bt_except
       );

       { packrecords types }
       tpackrecords = (packrecord_none,
         packrecord_1,packrecord_2,packrecord_4,
         packrecord_8,packrecord_16,packrecord_32,
         packrecord_C
       );

    const
       packrecordalignment : array[tpackrecords] of byte=(0,
         1,2,4,8,16,32,1
       );

    type
       stringid = string[maxidlen];

       tnormalset = set of byte; { 256 elements set }
       pnormalset = ^tnormalset;

       pdouble    = ^double;
       pbyte      = ^byte;
       pword      = ^word;
       plongint   = ^longint;

    {$IFDEF TP}
       Tconstant=record
            case signed:boolean of
                false:
                    (valueu:longint);
                true:
                    (values:longint);
       end;
    {$ELSE}
       Tconstant=record
            case signed:boolean of
                false:
                    (valueu:cardinal);
                true:
                    (values:longint);
       end;
    {$ENDIF}

    const
       { link options }
       link_none    = $0;
       link_allways = $1;
       link_static  = $2;
       link_smart   = $4;
       link_shared  = $8;


implementation


begin
end.
{
  $Log$
  Revision 1.6  2000-09-21 11:30:49  jonas
    + support for full boolean evaluation (b+/b-), default remains short
      circuit boolean evaluation

  Revision 1.5  2000/08/05 13:25:06  peter
    * packenum 1 fixes (merged)

  Revision 1.4  2000/08/02 19:49:59  peter
    * first things for default parameters

  Revision 1.3  2000/07/13 12:08:25  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:41  michael
  + removed logs

}
