{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl, Pierre Muller

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

{$i fpcdefs.inc}

interface

    const
       maxidlen = 64;

    type
{TCmdStr is used to pass command line parameters to an external program to be
executed from the FPC application. In some circomstances, this can be more
than 255 characters. That's why using Ansi Strings}
{$IFDEF USE_SYSUTILS}
       TCmdStr = AnsiString;
       PathStr = String;
       DirStr = String;
       NameStr = String;
       ExtStr = String;
{$ELSE USE_SYSUTILS}
       TCmdStr = String;
{$ENDIF USE_SYSUTILS}

{$ifndef fpc}
       qword = int64;
{$endif fpc}

{$ifdef ver1_0}
       { Bootstrapping }
       PtrInt = DWord;
       SizeInt = Longint;
{$endif ver1_0}

       { Natural integer register type and size for the target machine }
{$ifdef cpu64bit}
       AWord = qword;
       AInt = Int64;
{$else cpu64bit}
       AWord = longword;
       AInt = longint;
{$endif cpu64bit}
       PAWord = ^AWord;
       PAInt = ^AInt;

       { the ordinal type used when evaluating constant integer expressions }
       TConstExprInt = int64;
       { ... the same unsigned }
       TConstExprUInt = qword;
       { This must be an ordinal type with the same size as a pointer
         Note: Must be unsigned! Otherwise, ugly code like
         pointer(-1) will result in a pointer with the value
         $fffffffffffffff on a 32bit machine if the compiler uses
         int64 constants internally (JM) }
       TConstPtrUInt = AWord;

       { Switches which can be changed locally }
       tlocalswitch = (cs_localnone,
         { codegen }
         cs_check_overflow,cs_check_range,cs_check_object,
         cs_check_io,cs_check_stack,
         cs_omitstackframe,cs_do_assertion,cs_generate_rtti,
         cs_full_boolean_eval,cs_typed_const_writable,
         { mmx }
         cs_mmx,cs_mmx_saturation,
         { parser }
         cs_typed_addresses,cs_strict_var_strings,cs_ansistrings,
         { macpas specific}
         cs_external_var, cs_externally_visible
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
         cs_lineinfo,cs_threading,cs_implicit_exceptions,
         { linking }
         cs_create_smart,cs_create_dynamic,cs_create_pic
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
         cs_regvars,cs_no_regalloc,cs_uncertainopts,cs_littlesize,
         cs_optimize,cs_fastoptimize,cs_slowoptimize,cs_align,
         { browser }
         cs_browser_log,
         { debugger }
         cs_gdb_dbx,cs_gdb_gsym,cs_gdb_heaptrc,cs_gdb_lineinfo,
         cs_checkpointer,cs_gdb_valgrind,cs_gdb_dwarf,
         { assembling }
         cs_asm_leave,cs_asm_extern,cs_asm_pipe,cs_asm_source,
         cs_asm_regalloc,cs_asm_tempalloc,cs_asm_nodes,
         { linking }
         cs_link_extern,cs_link_static,cs_link_smart,cs_link_shared,cs_link_deffile,
         cs_link_strip,cs_link_staticflag,cs_link_on_target,cs_link_internal,
         cs_link_map,cs_link_pthread
       );
       tglobalswitches = set of tglobalswitch;

       { Switches which can be changed by a mode (fpc,tp7,delphi) }
       tmodeswitch = (m_none,m_all, { needed for keyword }
         { generic }
         m_fpc,m_objfpc,m_delphi,m_tp7,m_gpc,m_mac,
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

       { Win32, OS/2 & MacOS application types }
       tapptype = (
         app_none,
         app_gui,               { graphic user-interface application}
         app_cui,       { console application}
         app_fs,        { full-screen type application (OS/2 and EMX only) }
         app_tool       { tool application, (MPW tool for MacOS, MacOS only)}
       );

       { interface types }
       tinterfacetypes = (
         it_interfacecom,
         it_interfacecorba
       );

       { currently parsed block type }
       tblock_type = (bt_none,
         bt_general,bt_type,bt_const,bt_except,bt_body
       );

       { calling convention for tprocdef and tprocvardef }
       tproccalloption=(pocall_none,
         { procedure uses C styled calling }
         pocall_cdecl,
         { C++ calling conventions }
         pocall_cppdecl,
         { Procedure is used for internal compiler calls }
         pocall_compilerproc,
         { Far16 for OS/2 }
         pocall_far16,
         { Old style FPC default calling }
         pocall_oldfpccall,
         { Procedure is an assembler macro }
         pocall_inline,
         { Procedure has compiler magic}
         pocall_internproc,
         { procedure is a system call, applies e.g. to MorphOS and PalmOS }
         pocall_syscall,
         { pascal standard left to right }
         pocall_pascal,
         { procedure uses register (fastcall) calling }
         pocall_register,
         { safe call calling conventions }
         pocall_safecall,
         { procedure uses stdcall call }
         pocall_stdcall,
         { Special calling convention for cpus without a floating point
           unit. Floating point numbers are passed in integer registers
           instead of floating point registers. Depending on the other
           available calling conventions available for the cpu
           this replaces either pocall_fastcall or pocall_stdcall.
         }
         pocall_softfloat
       );
       tproccalloptions = set of tproccalloption;

       tprocinfoflag=(
         { procedure has at least one assembler block }
         pi_has_assembler_block,
         { procedure does a call }
         pi_do_call,
         { procedure has a try statement = no register optimization }
         pi_uses_exceptions,
         { procedure is declared as @var(assembler), don't optimize}
         pi_is_assembler,
         { procedure contains data which needs to be finalized }
         pi_needs_implicit_finally,
         { procedure has the implicit try..finally generated }
         pi_has_implicit_finally,
         { procedure uses fpu}
         pi_uses_fpu,
         { procedure uses GOT for PIC code }
         pi_needs_got
       );
       tprocinfoflags=set of tprocinfoflag;

{$ifdef ansistring_bits}
       Tstringbits=(sb_16,sb_32,sb_64);
{$endif}

     const
       proccalloptionStr : array[tproccalloption] of string[14]=('',
           'CDecl',
           'CPPDecl',
           'CompilerProc',
           'Far16',
           'OldFPCCall',
           'Inline',
           'InternProc',
           'SysCall',
           'Pascal',
           'Register',
           'SafeCall',
           'StdCall',
           'SoftFloat'
         );

       { Default calling convention }
{$ifdef x86}
       pocall_default = pocall_register;
{$else}
       pocall_default = pocall_stdcall;
{$endif}

    type
       stringid = string[maxidlen];

       tnormalset = set of byte; { 256 elements set }
       pnormalset = ^tnormalset;

       pboolean   = ^boolean;
       pdouble    = ^double;
       pbyte      = ^byte;
       pword      = ^word;
       plongint   = ^longint;
       plongintarray = plongint;

       Tconstant=record
            case signed:boolean of
                false:
                    (valueu:cardinal);
                true:
                    (values:longint);
       end;

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
  Revision 1.62  2004-10-15 09:14:16  mazen
  - remove $IFDEF DELPHI and related code
  - remove $IFDEF FPCPROCVAR and related code

  Revision 1.61  2004/10/14 14:42:57  mazen
  + Needed types aliases to merge USE_SYSUTILS branch

  Revision 1.60  2004/09/26 17:45:30  peter
    * simple regvar support, not yet finished

  Revision 1.59  2004/07/14 23:19:22  olle
    + added external facilities for macpas

  Revision 1.58  2004/06/20 08:55:29  florian
    * logs truncated

  Revision 1.57  2004/06/16 20:07:07  florian
    * dwarf branch merged

  Revision 1.56  2004/05/23 15:06:20  peter
    * implicit_finally flag must be set in pass1
    * add check whether the implicit frame is generated when expected

  Revision 1.55  2004/05/23 14:32:17  peter
    * tprocinfoflag moved to globtype

  Revision 1.54  2004/05/02 11:48:46  peter
    * strlenint is replaced with sizeint

  Revision 1.53  2004/04/29 19:56:36  daniel
    * Prepare compiler infrastructure for multiple ansistring types

}
