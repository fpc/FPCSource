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

{$i defines.inc}

interface

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
       ts64comp = extended;
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
{$ifdef ia64}
       bestreal = extended;
       ts32real = single;
       ts64real = double;
       ts80real = extended;
       { on the ia64 comp will be mapped to int64 }
       ts64comp = comp;
{$endif}
{$ifdef SPARC}
  bestreal = real;
  ts32real = single;
  ts64real = double;
  ts80real = extended;
  ts64comp = extended;
{$endif}

       pbestreal=^bestreal;

       { Switches which can be changed locally }
       tlocalswitch = (cs_localnone,
         { codegen }
         cs_check_overflow,cs_check_range,cs_check_object_ext,
         cs_check_io,cs_check_stack,
         cs_omitstackframe,cs_do_assertion,cs_generate_rtti,
         cs_full_boolean_eval,cs_typed_const_writable,
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
         cs_link_strip,cs_link_staticflag,cs_link_on_target
       );
       tglobalswitches = set of tglobalswitch;

       { Switches which can be changed by a mode (fpc,tp7,delphi) }
       tmodeswitch = (m_none,m_all, { needed for keyword }
         { generic }
         m_fpc,m_objfpc,m_delphi,m_tp7,m_gpc,
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
         m_default_para,        { support default parameters }
         m_hintdirective,       { support hint directives }
         m_duplicate_names      { allow locals/paras to have duplicate names of globals }
       );
       tmodeswitches = set of tmodeswitch;

       { win32 & OS/2 application types }
       tapptype = (app_none,
         app_gui,app_cui,app_fs
       );

       { interface types }
       tinterfacetypes = (
         it_interfacecom,
         it_interfacecorba
       );

       { currently parsed block type }
       tblock_type = (bt_none,
         bt_general,bt_type,bt_const,bt_except
       );

       { calling convention for tprocdef and tprocvardef }
       tproccalloption=(pocall_none,
         pocall_cdecl,         { procedure uses C styled calling }
         pocall_cppdecl,       { C++ calling conventions }
         pocall_compilerproc,  { Procedure is used for internal compiler calls }
         pocall_far16,         { Far16 for OS/2 }
         pocall_fpccall,       { FPC default calling }
         pocall_inline,        { Procedure is an assembler macro }
         pocall_internconst,   { procedure has constant evaluator intern }
         pocall_internproc,    { Procedure has compiler magic}
         pocall_palmossyscall, { procedure is a PalmOS system call }
         pocall_pascal,        { pascal standard left to right }
         pocall_register,      { procedure uses register (fastcall) calling }
         pocall_safecall,      { safe call calling conventions }
         pocall_stdcall,       { procedure uses stdcall call }
         pocall_system         { system call }
       );
       tproccalloptions = set of tproccalloption;

     const
       proccalloptionStr : array[tproccalloption] of string[14]=('',
           'CDecl',
           'CPPDecl',
           'CompilerProc',
           'Far16',
           'FPCCall',
           'Inline',
           'InternConst',
           'InternProc',
           'PalmOSSysCall',
           'Pascal',
           'Register',
           'SafeCall',
           'StdCall',
           'System'
         );

    type
       stringid = string[maxidlen];

       tnormalset = set of byte; { 256 elements set }
       pnormalset = ^tnormalset;

       pdouble    = ^double;
       pbyte      = ^byte;
       pword      = ^word;
       plongint   = ^longint;

       Tconstant=record
            case signed:boolean of
                false:
                    (valueu:cardinal);
                true:
                    (values:longint);
       end;

{$ifndef Delphi}
  {$ifndef xFPC}
    type
      pguid = ^tguid;
      tguid = packed record
        D1: LongWord;
        D2: Word;
        D3: Word;
        D4: array[0..7] of Byte;
      end;
  {$endif}
{$endif}

    const
       { link options }
       link_none    = $0;
       link_allways = $1;
       link_static  = $2;
       link_smart   = $4;
       link_shared  = $8;

implementation

end.
{
  $Log$
  Revision 1.21  2002-03-24 19:05:59  carl
  + patch for SPARC from Mazen NEIFER

  Revision 1.20  2002/01/24 18:25:48  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

  Revision 1.19  2001/10/25 21:22:32  peter
    * calling convention rewrite

  Revision 1.18  2001/10/24 11:46:06  marco
   * Opt Align fix.

  Revision 1.17  2001/10/23 21:49:42  peter
    * $calling directive and -Cc commandline patch added
      from Pavel Ozerski

  Revision 1.16  2001/10/20 20:30:21  peter
    * read only typed const support, switch $J-

  Revision 1.15  2001/09/17 21:29:11  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.14  2001/07/30 20:59:27  peter
    * m68k updates from v10 merged

  Revision 1.13  2001/07/01 20:16:15  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.12  2001/06/03 21:57:35  peter
    + hint directive parsing support

  Revision 1.11  2001/01/20 18:32:52  hajny
    + APPTYPE support under OS/2, app_fs, GetEnvPChar for OS/2

  Revision 1.10  2000/11/29 00:30:30  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.9  2000/11/12 22:20:37  peter
    * create generic toutputsection for binary writers

  Revision 1.8  2000/11/04 14:25:19  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.7  2000/09/24 15:06:16  peter
    * use defines.inc

  Revision 1.6  2000/09/21 11:30:49  jonas
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
