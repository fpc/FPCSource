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
         m_default_ansistring   { ansistring turned on by default }
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
  Revision 1.1  2000-07-13 06:29:51  michael
  + Initial import

  Revision 1.32  2000/06/11 07:00:21  peter
    * fixed pchar->string conversion for delphi mode

  Revision 1.31  2000/05/31 06:57:11  florian
    * first implementation of -Oa switch

  Revision 1.30  2000/05/16 20:19:05  pierre
    + -CR option to enable check for object virtual method

  Revision 1.29  2000/02/28 17:23:57  daniel
  * Current work of symtable integration committed. The symtable can be
    activated by defining 'newst', but doesn't compile yet. Changes in type
    checking and oop are completed. What is left is to write a new
    symtablestack and adapt the parser to use it.

  Revision 1.28  2000/02/09 13:22:53  peter
    * log truncated

  Revision 1.27  2000/02/09 10:35:48  peter
    * -Xt option to link staticly against c libs

  Revision 1.26  2000/02/06 17:20:52  peter
    * -gl switch for auto lineinfo including

  Revision 1.25  2000/01/07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.24  1999/11/12 11:03:50  peter
    * searchpaths changed to stringqueue object

  Revision 1.23  1999/11/09 13:00:38  peter
    * define FPC_DELPHI,FPC_OBJFPC,FPC_TP,FPC_GPC
    * initial support for ansistring default with modes

  Revision 1.22  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.21  1999/11/04 10:55:31  peter
    * TSearchPathString for the string type of the searchpaths, which is
      ansistring under FPC/Delphi

  Revision 1.20  1999/10/22 10:39:34  peter
    * split type reading from pdecl to ptype unit
    * parameter_dec routine is now used for procedure and procvars

  Revision 1.19  1999/09/20 16:38:54  peter
    * cs_create_smart instead of cs_smartlink
    * -CX is create smartlink
    * -CD is create dynamic, but does nothing atm.

  Revision 1.18  1999/09/08 16:05:32  peter
    * pointer add/sub is now as expected and the same results as inc/dec

  Revision 1.17  1999/08/13 15:44:58  peter
    * first things to include lineinfo in the executable

  Revision 1.16  1999/08/11 17:26:33  peter
    * tlinker object is now inherited for win32 and dos
    * postprocessexecutable is now a method of tlinker

  Revision 1.15  1999/08/04 13:02:42  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.14  1999/08/01 23:04:48  michael
  + Changes for Alpha

  Revision 1.13  1999/07/23 16:05:21  peter
    * alignment is now saved in the symtable
    * C alignment added for records
    * PPU version increased to solve .12 <-> .13 probs

}