{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Pierre Muller

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

    const
       def_alignment = 4;

       { symbol options }
       sp_public     = $1;
       sp_private    = $2;
       sp_published  = $4;
       sp_protected  = $8;
       sp_forwarddef = $10;
       sp_static     = $20;

       { flags for a definition }
       df_needsrtti = $1;           { the definitions needs rtti }
       df_hasrtti   = $2;           { the rtti is generated      }

       { options for tprocdef and tprocvardef }
       poexceptions     = $1;        { unused }
       povirtualmethod  = $2;        { Procedure is a virtual method }
       poclearstack     = $4;        { Use IBM flat calling convention. (Used by GCC.) }
       poconstructor    = $8;        { Procedure is a constructor }
       podestructor     = $10;       { Procedure is a destructor }
       pointernproc     = $20;       { Procedure has compiler magic}
       poexports        = $40;       { Procedure is exported }
       poiocheck        = $80;       { IO checking should be done after a call to the procedure }
       poabstractmethod = $100;      { Procedure is an abstract method }
       pointerrupt      = $200;      { Procedure is an interrupt handler }
       poinline         = $400;      { Procedure is an assembler macro }
       poassembler      = $800;      { Procedure is written in assembler }
       pooperator       = $1000;     { Procedure defines an operator }
       poexternal       = $2000;     { Procedure is external (in other object or lib)}
       poleftright      = $4000;     { Push parameters from left to right }
       poproginit       = $8000;     { Program initialization }
       postaticmethod   = $10000;    { static method }
       pooverridingmethod=$20000;    { method with override directive }
       poclassmethod    = $40000;    { class method }
       pounitinit       = $80000;    { unit initialization }
       pomethodpointer  = $100000;   { method pointer, only in procvardef, also used for 'with object do' }
       pocdecl          = $200000;   { procedure uses C styled calling }
       popalmossyscall  = $400000;   { procedure is a PalmOS system call }
       pointernconst    = $800000;   { procedure has constant evaluator intern }
       poregister       = $1000000;  { procedure uses register (fastcall) calling }
       pounitfinalize   = $2000000;  { unit finalization }
       postdcall        = $4000000;  { procedure uses stdcall call }
       pomsgstr         = $8000000;  { method for string message handling }
       pomsgint         = $10000000; { method for int message handling }
       posavestdregs    = $20000000; { save std regs cdecl and stdcall need that ! }
       pocontainsself   = $40000000; { self is passed explicit to the compiler }
       { relevant options for assigning a proc or a procvar to a procvar }
       po_compatibility_options = $7FFFFFFF-
         (pomethodpointer+povirtualmethod+pooverridingmethod);

       { options for objects and classes }
       oo_is_abstract  = $1;
       oo_is_class     = $2;
       oo_hasvirtual   = $4;
       oo_hasprivate   = $8;
       oo_hasprotected = $10;
       oo_isforward    = $20;
       oo_can_have_published = $40;
       oo_hasconstructor = $80;
       oo_hasdestructor = $100;
       oo_hasvmt        = $200;
       oo_hasmsgstr     = $400;
       oo_hasmsgint     = $800;

       { options for properties }
       ppo_indexed = $1;
       ppo_defaultproperty = $2;
       ppo_stored = $4;

       { options for variables }
       vo_regable     = $1;
       vo_is_C_var    = $2;
       vo_is_external = $4;
       vo_is_dll_var  = $8;
       vo_is_thread_var = $10;

{
  $Log$
  Revision 1.6  1999-04-28 06:02:10  florian
    * changes of Bruessel:
       + message handler can now take an explicit self
       * typinfo fixed: sometimes the type names weren't written
       * the type checking for pointer comparisations and subtraction
         and are now more strict (was also buggy)
       * small bug fix to link.pas to support compiling on another
         drive
       * probable bug in popt386 fixed: call/jmp => push/jmp
         transformation didn't count correctly the jmp references
       + threadvar support
       * warning if ln/sqrt gets an invalid constant argument

  Revision 1.5  1999/04/26 13:31:46  peter
    * release storenumber,double_checksum

  Revision 1.4  1999/04/16 10:28:26  pierre
    + added posavestdregs used for cdecl AND stdcall functions
      (saves ESI EDI and EBX for i386)

  Revision 1.3  1999/03/05 01:14:23  pierre
    * bug0198 : call conventions for methods
      not yet implemented is the control of same calling convention
      for virtual and child's virtual
    * msgstr and msgint only created if message was found
      who implemented this by the way ?
      it leaks lots of plabels !!!! (check with heaptrc !)

  Revision 1.2  1999/02/22 20:13:37  florian
    + first implementation of message keyword

  Revision 1.1  1999/01/12 14:32:49  peter
    * splitted from symtable.pas

}

