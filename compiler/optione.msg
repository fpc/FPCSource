 [options] <inputfile> [options]
Only one source file supported
DEF file can be created only for OS/2
nested response files are not supported
No source file name in command line
Illegal parameter: $1
-? writes help pages
Unable to open file $1
Reading further options from $1
Target is already set to: $1
Shared libs not supported on DOS platform, reverting to static
too many IF(N)DEFs
too many ENDIFs
open conditional at the end of the file
;
; Logo
;
Free Pascal Compiler version 0.99.0 for $1
Copyright (c) 1993-98 by Florian Klaempfl
;
; Info
;
Free Pascal Compiler version 0.99.5
<lf>
This program comes under the GNU General Public Licence
For more information read COPYING
<lf>
Report bugs,suggestions etc to:
                klaempfl@haegar.cip.mw.tu-muenchen.de
;
; The -? help pages
;
; XXX<X>_<Text>
; ||| +- letter(s)  [ the option letter(s) ]
; ||+-- ident       [ 0123 level 0,1,2,3 ]
; |+--- OS          [ L,D,W,O,t(TP),*(all) ]
; +---- Processor   [ 3,6,*(all) ]
;
**0*_+ switch option on, - off
**1a_the compiler doesn''t delete the generated assembler file
*t1b_use EMS
**1B+_build
**1C_code generation options
**2Ca_not implemented
**2Ce_not implemented
3*2CD_Dynamic linking
**2Ch<n>_<n> bytes heap (between 1023 and 67107840)
**2Ci_IO-checking
**2Cn_omit linking stage
**2Co_check overflow of integer operations
**2Cr_range checking
*O2Cs<n>_set stack size to <n>
**2Ct_stack checking
3*2CS_static linking
**1d<x>_defines the symbol <x>
*O1D_controls the generation of DEF file (only OS/2)
*O2Dd<x>_set description to <x>
*O2Do_generate DEF file
*O2Dw_PM application
*L1e<x>_set path to executables
**1E_same as -Cn
**1g_generate debugger information
**2gp_generate also profile code for gprof
**1F_set file names and paths
**2Fe<x>_redirect error output to <x>
*L2Fg<x>_<x> search path for the GNU C lib
*L2Fr<x>_<x> search path for the error message file
**2Fi<x>_adds <x> to include path
**2Fl<x>_adds <x> to library path
**2Fu<x>_adds <x> to unit path
**1k<x>_Pass <x> to the linker
**1L_set language
**2LD_german
**2LE_english
**1l_write logo
**1i_information
**1n_don't read the default config file
**1o<x>_change the name of the executable produced to <x>
**1pg_generate profile code for gprof
*L1P_use pipes instead of creating temporary assembler files
**1S_syntax options
**2S2_switch some Delphi 2 extension on
**2Sc_supports operators like C (*=,+=,/= and -=)
**2Se_compiler stops after the first error
**2Sg_allows LABEL and GOTO
**2Si_support C++ stlyed INLINE
**2Sm_support macros like C (global)
**2So_tries to be TP/BP 7.0 compatible
**2Ss_constructor name must be init (destructor must be done)
**2St_allows static keyword in objects
**1s_don't call assembler and linker (only with -a)
**1T<x>_Target operating system
3*2TDOS_DOS extender by DJ Delorie
3*2TOS2_OS/2 2.x
3*2TLINUX_Linux
3*2TWin32_Windows 32 Bit
3*2TGO32V2_version 2 of DJ Delorie DOS extender
6*2TAMIGA_Commodore Amiga
6*2TATARI_Atari ST/STe/TT
6*2TMACOS_Macintosh m68k
6*2TLINUX_Linux-68k
**1u<x>_undefines the symbol <x>
**1U_unit options
**2Uls_make static library from unit
**2Uld_make dynamic library from unit
**2Un_don't check the unit name
**2Up<x>_same as -Fu<x>
**2Us_compile a system unit
**1v<x>_Be verbose. <x> is a combination of the following letters :
**2*_e : Show errors (default)       d : Show debug info
**2*_w : Show warnings               u : Show used files
**2*_n : Show notes                  t : Show tried files
**2*_h : Show hints                  m : Show defined macros
**2*_i : Show general info           p : Show compiled procedures
**2*_l : Show linenumbers            c : Show conditionals
**2*_a : Show everything             0 : Show nothing (except errors)
**1X_executable options
*L2Xc_link with the c library
**2Xs_strip all symbols from executable
**0*_Processor specific options:
3*1A_output format
3*2Aatt_AT&T assembler
3*2Ao_coff file using GNU AS
3*2Aobj_OMF file using NASM
3*2Anasm_coff file using NASM
3*2Amasm_assembler for the Microsoft/Borland/Watcom assembler
3*1R_assembler reading style
3*2Ratt_read AT&T style assembler
3*2Rintel_read Intel style assembler
3*2Rdirect_copy assembler text directly to assembler file
3*1O_optimizations
3*2Oa_simple optimizations
3*2Og_optimize for size
3*2OG_optimize for time
3*2Ox_optimize maximum
3*2Oz_uncertain optimizes (see docs)
3*2O2_optimize for Pentium II (tm)
3*2O3_optimize for i386
3*2O4_optimize for i486
3*2O5_optimize for Pentium (tm)
3*2O6_optimizations for PentiumPro (tm)
6*1A_output format
6*2Agas_GNU Motorola assembler
6*2Ao_UNIX o-file
6*2Am_Standard Motorola assembler
6*2Ai_MIT Syntax (old GAS)
6*1O_optimizations
6*2Oa_simple optimizations
6*2Og_optimize for size
6*2OG_optimize for time
6*2Ox_optimize maximum
6*2O2_target is a MC68020+ processor
**1*_
**1?_shows this help
**1h_shows this help without waiting
