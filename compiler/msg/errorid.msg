#   $Id: errorid.msg,v 1.124 2006/06/29 02:52:50 zaenal Exp $
#   This file is part of the Free Pascal Compiler
#   Copyright (c) 1999-2000 by the Free Pascal Development team
#
#   Indonesian Language File for Free Pascal
#   Contributed by Zaenal Mutaqin <ade999 at gmail.com>
#   Based on errore.msg of SVN revision 3982
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
#
# The constants are build in the following order:
# <part>_<type>_<txtidentifier>
#
# <part> is the part of the compiler the message is used
#   asmr_     assembler parsing
#   asmw_     assembler writing/binary writers

#   unit_     unit handling
#   scan_     scanner
#   parser_   parser
#   type_     type checking
#   general_  general info
#   exec_     calls to assembler, linker, binder
#
# <type> the type of the message it should normally used for
#   f_   fatal error
#   e_   error
#   w_   warning
#   n_   note
#   h_   hint
#   i_   info
#   l_   add linenumber
#   u_   used
#   t_   tried
#   c_   conditional
#   d_   debug message
#   x_   executable informations
#

#
# Umum
#
# 01023 adalah yang terakhir digunakan
#
general_text_bytes_code=01019_byte kode
general_text_bytes_data=01020_byte data
# BeginOfTeX
% \section{Pesan kompilator Umum}
% Bagian ini memberikan pesan kompilator yang tidak fatal, tetapi 
% menampilkan informasi berguna. Jumlah pesan tersebut bisa
% dikontrol dengan berbagai tingkat penggunaan saklar \var{-v}.
% \begin{description}
general_t_compilername=01000_T_Kompilator: $1
% Ketika saklar \var{-vt} digunakan, baris ini memberitahu kompilator apa
% yang digunakan.
general_d_sourceos=01001_D_Kompilator OS: $1
% Ketika saklar \var{-vd} digunakan, baris ini memberitahu anda sistem
% operasi apa sebagai sumbernya.
general_i_targetos=01002_I_Target OS: $1
% When the \var{-vd} switch is used, this line tells you what the target
% operating system is.
general_t_exepath=01003_T_Menggunakan path eksekutabel: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for its binaries.
general_t_unitpath=01004_T_Menggunakan path unit: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for compiled units. You can set this path with the \var{-Fu}
general_t_includepath=01005_T_Menggunakan path include: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for its include files (files used in \var{\{\$I xxx\}} statements).
% You can set this path with the \var{-I} option.
general_t_librarypath=01006_T_Menggunakan path librari: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for the libraries. You can set this path with the \var{-Fl} option.
general_t_objectpath=01007_T_Menggunakan path obyek: $1
% When the \var{-vt} switch is used, this line tells you where the compiler
% looks for object files you link in (files used in \var{\{\$L xxx\}} statements).
% You can set this path with the \var{-Fo} option.
general_i_abslines_compiled=01008_I_$1 baris dikompilasi, $2 det$3
% When the \var{-vi} switch is used, the compiler reports the number
% of lines compiled, and the time it took to compile them (real time,
% not program time).
general_f_no_memory_left=01009_F_Tidak ada memori tersisa
% The compiler doesn't have enough memory to compile your program. There are
% several remedies for this:
% \begin{itemize}
% \item If you're using the build option of the compiler, try compiling the
% different units manually.
% \item If you're compiling a huge program, split it up in units, and compile
% these separately.
% \item If the previous two don't work, recompile the compiler with a bigger
% heap (you can use the \var{-Ch} option for this, \seeo{Ch})
% \end{itemize}
general_i_writingresourcefile=01010_I_Menulis file Resource String Table: $1
% This message is shown when the compiler writes the Resource String Table
% file containing all the resource strings for a program.
general_e_errorwritingresourcefile=01011_E_Menulis file Resource String Table: $1
% This message is shown when the compiler encountered an error when writing
% the Resource String Table file
general_i_fatal=01012_I_Fatal:
% Prefix for Fatal Errors
general_i_error=01013_I_Kesalahan:
% Prefix for Errors
general_i_warning=01014_I_Perhatian:
% Prefix for Warnings
general_i_note=01015_I_Catatan:
% Prefix for Notes
general_i_hint=01016_I_Petunjuk:
% Prefix for Hints
general_e_path_does_not_exist=01017_E_Path "$1" tidak ada
% Path yang ditetapkan tidak ada.
general_f_compilation_aborted=01018_F_Kompilasi dibatalkan
% Kompilasi dibatalkan.
general_i_number_of_warnings=01021_I_$1 perhatian ditemukan
% Jumlah peringatan dimunculkan selama kompilasi.
general_i_number_of_hints=01022_I_$1 petujuk ditemukan
% Total number of hints issued during compilation.
general_i_number_of_notes=01023_I_$1 catatan ditemukan
% Total number of notes issued during compilation.
% \end{description}
#
# Scanner
#
# 02063 is the last used one
#
% \section{Pesan pemindai.}
% This section lists the messages that the scanner emits. The scanner takes
% care of the lexical structure of the pascal file, i.e. it tries to find
% reserved words, strings, etc. It also takes care of directives and
% conditional compiling handling.
% \begin{description}
scan_f_end_of_file=02000_F_Akhir file tidak diharapkan
% this typically happens in one of the following cases :
% \begin{itemize}
% \item The source file ends before the final \var{end.} statement. This
% happens mostly when the \var{begin} and \var{end} statements aren't
% balanced;
% \item An include file ends in the middle of a statement.
% \item A comment was not closed
% \end{itemize}
scan_f_string_exceeds_line=02001_F_String melampaui baris
% There is a missing closing ' in a string, so it occupies
% multiple lines.
scan_f_illegal_char=02002_F_karakter tidak benar "$1" ($2)
% An illegal character was encountered in the input file.
scan_f_syn_expected=02003_F_Sintaks salah, "$1" diharapkan tapi "$2" yang ditemukan
% This indicates that the compiler expected a different token than
% the one you typed. It can occur almost everywhere where you make a
% mistake against the pascal language.
scan_t_start_include_file=02004_TL_Mulai pembacaan file include $1
% When you provide the \var{-vt} switch, the compiler tells you
% when it starts reading an included file.
scan_w_comment_level=02005_W_Komentar tingkat $1 ditemukan
% When the \var{-vw} switch is used, then the compiler warns you if
% it finds nested comments. Nested comments are not allowed in Turbo Pascal
% and can be a possible source of errors.
scan_n_ignored_switch=02008_N_Saklar kompilator "$1" diabaikan
% With \var{-vn} on, the compiler warns if it ignores a switch
scan_w_illegal_switch=02009_W_Saklar kompilator "$1" tidak benar
% You included a compiler switch (i.e. \var{\{\$... \}}) which the compiler
% does not recognise
scan_w_switch_is_global=02010_W_Saklar kompilator global salah tempat
% The compiler switch is misplaced, and should be located at
% the start of the unit or program.
scan_e_illegal_char_const=02011_E_Konstan char tidak benar
% This happens when you specify a character with its ASCII code, as in
% \var{\#96}, but the number is either illegal, or out of range.
scan_f_cannot_open_input=02012_F_Tidak bisa membuka file "$1"
% \fpc cannot find the program or unit source file you specified on the
% command line.
scan_f_cannot_open_includefile=02013_F_Tidak bisa membuka file include "$1"
% \fpc cannot find the source file you specified in a \var{\{\$include ..\}}
% statement.
scan_e_illegal_pack_records=02015_E_Pembeda penjajaran rekaman tidak benar "$1"
% You are specifying the \var{\{\$PACKRECORDS n\} } or \var{\{\$ALIGN n\} }
% with an illegal value for \var{n}. For \$PACKRECORDS valid alignments are 1, 2, 4, 8, 16, 32, C,
% NORMAL, DEFAULT, and for \$ALIGN valid alignment are 1, 2, 4, 8, 16, 32, ON,
% OFF. Under mode MacPas \$ALIGN also supports MAC68K, POWER and RESET.
scan_e_illegal_pack_enum=02016_E_Pembeda enum besar-minimum "$1" tidak benar
% You are specifying the \var{\{\$PACKENUM n\}} with an illegal value for
% \var{n}. Only 1,2,4, NORMAL or DEFAULT are valid here.
scan_e_endif_expected=02017_E_$ENDIF diharapkan untuk $1 $2 yang ditetapkan dalam $3 baris $4
% Your conditional compilation statements are unbalanced.
scan_e_preproc_syntax_error=02018_E_Syntaks salah saat pemindaian ekspresi kompilasi kondisional
% There is an error in the expression following the \var{\{\$if ..\}}, $ifc or $setc compiler
% directives.
scan_e_error_in_preproc_expr=02019_E_Mengevaluasi ekspresi kompilasi kondisional
% There is an error in the expression following the \var{\{\$if ..\}}, $ifc or $setc compiler
% directives.
scan_w_macro_cut_after_255_chars=02020_W_Panjang isi makro dibatasi hanya 255 karakter
% The contents of macros cannot be longer than 255 characters.
scan_e_endif_without_if=02021_E_ENDIF tanpa IF(N)DEF
% Your \var{\{\$IFDEF ..\}} and {\{\$ENDIF\}} statements aren't balanced.
scan_f_user_defined=02022_F_Pemakai mendefinisikan: $1
% A user defined fatal error occurred. see also the \progref
scan_e_user_defined=02023_E_Pemakai mendefinisikan: $1
% A user defined error occurred. see also the \progref
scan_w_user_defined=02024_W_Pemakai mendefinisikan: $1
% A user defined warning occurred. see also the \progref
scan_n_user_defined=02025_N_Pemakai mendefinisikan: $1
% A user defined note was encountered. see also the \progref
scan_h_user_defined=02026_H_Pemakai mendefinisikan: $1
% A user defined hint was encountered. see also the \progref
scan_i_user_defined=02027_I_Pemakai mendefinisikan: $1
% User defined information was encountered. see also the \progref
scan_e_keyword_cant_be_a_macro=02028_E_Keyword yang didefinisikan ulang sebagai makro tidak berpengaruh
% You cannot redefine keywords with macros.
scan_f_macro_buffer_overflow=02029_F_Bufer makro kelebihan ketika pembacaan atau perluasan makro
% Your macro or its result was too long for the compiler.
scan_w_macro_too_deep=02030_W_Perluasan makro melebihi kedalaman dari 16.
% When expanding a macro, macros have been nested to a level of 16.
% The compiler will expand no further, since this may be a sign that
% recursion is used.
scan_w_wrong_styled_switch=02031_W_saklar kompilator tidak didukung dalam gaya komentar //
% Compiler switches should be in normal pascal style comments.
scan_d_handling_switch=02032_DL_Saklar penanganan "$1"
% When you set debugging info on (\var{-vd}) the compiler tells you when it
% is evaluating conditional compile statements.
scan_c_endif_found=02033_CL_ENDIF $1 ditemukan
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifdef_found=02034_CL_IFDEF $1 ditemukan, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifopt_found=02035_CL_IFOPT $1 ditemukan, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_if_found=02036_CL_IF $1 ditemukan, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_ifndef_found=02037_CL_IFNDEF $1 ditemukan, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_else_found=02038_CL_ELSE $1 ditemukan, $2
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements.
scan_c_skipping_until=02039_CL_Melewati sampai...
% When you turn on conditional messages(\var{-vc}), the compiler tells you
% where it encounters conditional statements, and whether it is skipping or
% compiling parts.
scan_i_press_enter=02040_I_Tekan <return> untuk melanjutkan
% When the \var{-vi} switch is used, the compiler stops compilation
% and waits for the \var{Enter} key to be pressed when it encounters
% a \var{\{\$STOP\}} directive.
scan_w_unsupported_switch=02041_W_Saklar "$1" tidak didukung
% When warnings are turned on (\var{-vw}) the compiler warns you about
% unsupported switches. This means that the switch is used in Delphi or
% Turbo Pascal, but not in \fpc
scan_w_illegal_directive=02042_W_Direktif kompilator "$1" tidak benar
% When warings are turned on (\var{-vw}) the compiler warns you about
% unrecognised switches. For a list of recognised switches, \progref
scan_t_back_in=02043_TL_Kembali ke $1
% When you use (\var{-vt}) the compiler tells you when it has finished
% reading an include file.
scan_w_unsupported_app_type=02044_W_Tipe aplikasi tidak didukung: "$1"
% You get this warning, if you specify an unknown application type
% with the directive \var{\{\$APPTYPE\}}
scan_w_app_type_not_support=02045_W_APPTYPE tidak didukung oleh target OS
% The \var{\{\$APPTYPE\}} directive is supported by certain operating systems only.
scan_w_description_not_support=02046_W_DESCRIPTION tidak didukung oleh target OS
% The \var{\{\$DESCRIPTION\}} directive is not supported on this target OS
scan_n_version_not_support=02047_N_VERSION tidak didukung oleh target OS
% The \var{\{\$VERSION\}} directive is not supported on this target OS
scan_n_only_exe_version=02048_N_VERSION hanya untuk exe atau DLL
% The \var{\{\$VERSION\}} directive is only used for executable or DLL sources.
scan_w_wrong_version_ignored=02049_W_Format salah untuk direktif VERSION "$1"
% The \var{\{\$VERSION\}} directive format is majorversion.minorversion
% where majorversion and minorversion are words.
scan_e_illegal_asmmode_specifier=02050_E_Gaya assembler yang ditetapkan "$1" tidak benar
% When you specify an assembler mode with the \var{\{\$ASMMODE xxx\}}
% the compiler didn't recognize the mode you specified.
scan_w_no_asm_reader_switch_inside_asm=02051_W_Saklar pembaca ASM tidak mungkin berada dalam pernyataan asm, "$1" hanya akan efektif untuk next
% It is not possible to switch from one assembler reader to another
% inside an assembler block. The new reader will be used for next
% assembler statements only.
scan_e_wrong_switch_toggle=02052_E_Saklar toggle salah, gunakan ON/OFF atau +/-
% You need to use ON or OFF or a + or - to toggle the switch
scan_e_resourcefiles_not_supported=02053_E_File Resource tidak didukung untuk target ini
% The target you are compiling for doesn't support resource files.
scan_w_include_env_not_found=02054_W_Lingkungan include "$1" tidak ditemukan dalam lingkungan
% The included environment variable can't be found in the environment, it will
% be replaced by an empty string instead.
scan_e_invalid_maxfpureg_value=02055_E_Nilai tidak benar untuk batas register FPU
% Valid values for this directive are 0..8 and NORMAL/DEFAULT
scan_w_only_one_resourcefile_supported=02056_W_Hanya satu file resource didukung untuk target ini
% The target you are compiling for supports only one resource file.
% The first resource file found is used, the others are discarded.
scan_w_macro_support_turned_off=02057_W_Dukungan makro sudah dimatikan
% A macro declaration has been found, but macro support is currently off,
% so the declaration will be ignored. To turn macro support on compile with
% -Sm on the commandline or add \{\$MACRO ON\} in the source
scan_e_invalid_interface_type=02058_E_Tipe antar muka yang ditetapkan tidak benar. Yang benar COM, CORBA atau DEFAULT.
% The interface type that was specified is not supported
scan_w_appid_not_support=02059_W_APPID hanya didukung untuk PalmOS
% The \var{\{\$APPID\}} directive is only supported for the PalmOS target.
scan_w_appname_not_support=02060_W_APPNAME hanya didukung untuk PalmOS
% The \var{\{\$APPNAME\}} directive is only supported for the PalmOS target.
scan_e_string_exceeds_255_chars=02061_E_String konstan tidak bisa lebih panjang dari 255 karakter
% A single string constant can contain at most 255 chars. Try splitting up the
% string in multiple smaller parts and concatenate them with a + operator.
scan_f_include_deep_ten=02062_F_Menyertakan file include melebihi kedalaman 16.
% When including include files the files have been nested to a level of 16.
% The compiler will expand no further, since this may be a sign that
% recursion is used.
scan_e_too_many_push=02063_F_Terlalu banyak tingkat dari PUSH
% A maximum of 20 levels is allowed. This error occurs only in mode MacPas.
scan_e_too_many_pop=02064_E_POP tanpa PUSH sebelumnya
% This error occurs only in mode MacPas.
scan_e_error_macro_lacks_value=02065_E_Makro atau variabel waktu kompilasi "$1" tidak mempunyai nilai apapun
% Thus the conditional compile time expression cannot be evaluated.
scan_e_wrong_switch_toggle_default=02066_E_Saklar toggle salah, gunakan ON/OFF/DEFAULT atau +/-/*
% You need to use ON or OFF or DEFAULT or a + or - or * to toggle the switch
scan_e_mode_switch_not_allowed=02067_E_Saklar mode "$1" tidak diijinkan disini
% A mode switch has already been encountered, or, in case of option -Mmacpas,
% a mode switch occur after UNIT.
scan_e_error_macro_undefined=02068_E_Variabel waktu kompilasi atau makro "$1" tidak ditetapkan.
% Thus the conditional compile time expression cannot be evaluated. Only in mode MacPas.
scan_e_utf8_bigger_than_65535=02069_E_Kode UTF-8 lebih besar dari 65535 ditemukan
% \fpc handles utf-8 strings internally as widestrings e.g. the char codes are limited to 65535
scan_e_utf8_malformed=02070_E_Bentuk salah string UTF-8
% The given string isn't a valid UTF-8 string
scan_c_switching_to_utf8=02071_C_Tanda UTF-8 ditemukan, gunakan pengkodean UTF-8
% The compiler found an UTF-8 encoding signature (\$ef, \$bb, \$bf) at the beginning of a file,
% so it interprets it as an UTF-8 file
scan_e_compile_time_typeerror=02072_E_Ekspresi waktu kompilasi: Diinginkan $1 tapi didapat $2 di $3
% Type check of a compile time expression failed.
scan_n_app_type_not_support=02073_N_APPTYPE tidak didukung oleh target OS
% The \var{\{\$APPTYPE\}} directive is supported by certain operating systems only.
scan_e_illegal_optimization_specifier=02074_E_Optimasi yang ditetapkan "$1" tidak benar
% When you specify an optimization with the \var{\{\$OPTIMIZATION xxx\}}
% the compiler didn't recognize the optimization you specified.
scan_w_setpeflags_not_support=02075_W_SETPEFLAGS tidak didukung oleh target OS
% The \var{\{\$SETPEFLAGS\}} directive is not supported by the target OS
scan_w_imagebase_not_support=02076_W_IMAGEBASE tidak didukung oleh target OS
% The \var{\{\$IMAGEBASE\}} directive is not supported by the target OS
scan_w_minstacksize_not_support=02077_W_MINSTACKSIZE tidak didukung oleh target OS
% The \var{\{\$MINSTACKSIZE\}} directive is not supported by the target OS
scan_w_maxstacksize_not_support=02078_W_MAXSTACKSIZE tidak didukung oleh target OS
% The \var{\{\$MAXSTACKSIZE\}} directive is not supported by the target OS
% \end{description}
#
# Parser
#
# 03192 is the last used one
#
% \section{Parser messages}
% This section lists all parser messages. The parser takes care of the
% semantics of you language, i.e. it determines if your pascal constructs
% are correct.
% \begin{description}
parser_e_syntax_error=03000_E_Parser - Kesalahan Sintaks
% An error against the Turbo Pascal language was encountered. This happens
% typically when an illegal character is found in the sources file.
parser_e_dont_nest_interrupt=03004_E_Prosedur INTERRUPT tidak bisa diulang
% An \var{INTERRUPT} procedure must be global.
parser_w_proc_directive_ignored=03005_W_Tipe prosedur "$1" diabaikan
% The specified procedure directive is ignored by FPC programs.
parser_e_no_overload_for_all_procs=03006_E_Tidak semua deklarasi dari "$1" dideklarasikan dengan OVERLOAD
% When you want to use overloading using the \var{OVERLOAD} directive, then
% all declarations need to have \var{OVERLOAD} specified.
parser_e_export_name_double=03008_E_Duplikasi nama fungsi yang diekspor "$1"
% Exported function names inside a specific DLL must all be different
parser_e_export_ordinal_double=03009_E_Duplikasi indeks fungsi yang diekspor $1
% Exported function names inside a specific DLL must all be different
parser_e_export_invalid_index=03010_E_Indeks tidak benar untuk fungsi yang diekspor
% DLL function index must be in the range \var{1..\$FFFF}
parser_w_parser_reloc_no_debug=03011_W_Info debug Relokasi DLL atau eksekutabel $tidak bekerja, dimatikan.
parser_w_parser_win32_debug_needs_WN=03012_W_Untuk membolehkan debugging untuk kode win32 anda perlu mematikan relokasi dengan opsi -WN
% Stabs info is wrong for relocatable DLL or EXES use -WN
% if you want to debug win32 executables.
parser_e_constructorname_must_be_init=03013_E_Nama konstruktor haruslah INIT
% You are declaring an object constructor with a name which is not \var{init}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_destructorname_must_be_done=03014_E_Nama destruktor harus DONE
% You are declaring an object destructor with a name which is not \var{done}, and the
% \var{-Ss} switch is in effect. See the \var{-Ss} switch (\seeo{Ss}).
parser_e_proc_inline_not_supported=03016_E_Tipe prosedur INLINE tidak didukung
% You tried to compile a program with C++ style inlining, and forgot to
% specify the \var{-Si} option (\seeo{Si}). The compiler doesn't support C++
% styled inlining by default.
parser_w_constructor_should_be_public=03018_W_Konstruktor harus publik
% Constructors must be in the 'public' part of an object (class) declaration.
parser_w_destructor_should_be_public=03019_W_Destruktor harus publik
% Destructors must be in the 'public' part of an object (class) declaration.
parser_n_only_one_destructor=03020_N_Kelas harus mempunyai hanya satu destruktor
% You can declare only one destructor for a class.
parser_e_no_local_objects=03021_E_Definisi kelas lokal tidak diperbolehkan
% Classes must be defined globally. They cannot be defined inside a
% procedure or function
parser_f_no_anonym_objects=03022_F_Definisi kelas anonim tidak diperbolehkan
% An invalid object (class) declaration was encountered, i.e. an
% object or class without methods that isn't derived from another object or
% class. For example:
% \begin{verbatim}
% Type o = object
%          a : longint;
%          end;
% \end{verbatim}
% will trigger this error.
parser_n_object_has_no_vmt=03023_N_Obyek "$1" tidak mempunyai VMT
% This is a note indicating that the declared object has no
% virtual method table.
parser_e_illegal_parameter_list=03024_E_Daftar parameter tidak benar
% You are calling a function with parameters that are of a different type than
% the declared parameters of the function.
parser_e_wrong_parameter_size=03026_E_Jumlah parameter yang ditetapkan salah
% There is an error in the parameter list of the function or procedure,
% the number of parameters is not correct.
parser_e_overloaded_no_procedure=03027_E_Pengenal "$1" yang dioverload bukan fungsi
% The compiler encountered a symbol with the same name as an overloaded
% function, but it is not a function it can overload.
parser_e_overloaded_have_same_parameters=03028_E_fungsi yang di-overload parameternya sama
% You're declaring overloaded functions, but with the same parameter list.
% Overloaded function must have at least 1 different parameter in their
% declaration.
parser_e_header_dont_match_forward=03029_E_header fungsi tidak sama dengan deklarasi berikutnys "$1"
% You declared a function with same parameters but
% different result type or function modifiers.
parser_e_header_different_var_names=03030_E_header fungsi "$1" tidak sama: nama var berubah $2 => $3
% You declared the function in the \var{interface} part, or with the
% \var{forward} directive, but define it with a different parameter list.
parser_n_duplicate_enum=03031_N_Nilai dalam tipe enumerasi harus ascending
% \fpc allows enumeration constructions as in C. Given the following
% declaration two declarations:
% \begin{verbatim}
% type a = (A_A,A_B,A_E:=6,A_UAS:=200);
% type a = (A_A,A_B,A_E:=6,A_UAS:=4);
% \end{verbatim}
% The second declaration would produce an error. \var{A\_UAS} needs to have a
% value higher than \var{A\_E}, i.e. at least 7.
parser_e_no_with_for_variable_in_other_segments=03033_E_With tidak bisa digunakan untuk variabel dalam segmen yang berbeda
% With stores a variable locally on the stack,
% but this is not possible if the variable belongs to another segment.
parser_e_too_much_lexlevel=03034_E_pengulangan fungsi > 31
% You can nest function definitions only 31 times.
parser_e_range_check_error=03035_E_kesalahan pemerisaan jangkauan saat mengevaluasi konstan
% The constants are out of their allowed range.
parser_w_range_check_error=03036_W_kesalahan pemeriksaan saat mengevalisasi konstan
% The constants are out of their allowed range.
parser_e_double_caselabel=03037_E_duplikasi label case
% You are specifying the same label 2 times in a \var{case} statement.
parser_e_case_lower_less_than_upper_bound=03038_E_Jangkauan batas atas dari case kurang dari batas bawah
% The upper bound of a \var{case} label is less than the lower bound and this
% is useless
parser_e_type_const_not_possible=03039_E_Konstan tipe dari kelas tidak diperbolehkan
% You cannot declare a constant of type class or object.
parser_e_no_overloaded_procvars=03040_E_variabel fungsi dari fungsi yang di-overload tidak diperbolehkan
% You are trying to assign an overloaded function to a procedural variable.
% This is not allowed
parser_e_invalid_string_size=03041_E_panjang string harus bernilai 1 sampai 255
% The length of a shortstring in Pascal is limited to 255 characters. You are
% trying to declare a string with length lower than 1 or greater than 255
parser_w_use_extended_syntax_for_objects=03042_W_penggunaan sintaks extended dari NEW dan DISPOSE untuk instan obyek
% If you have a pointer \var{a} to a class type, then the statement
% \var{new(a)} will not initialize the class (i.e. the constructor isn't
% called), although space will be allocated. you should issue the
% \var{new(a,init)} statement. This will allocate space, and call the
% constructor of the object
parser_w_no_new_dispose_on_void_pointers=03043_W_penggunaan NEW atau DISPOSE pointer untyped tidak berarti
parser_e_no_new_dispose_on_void_pointers=03044_E_penggunaan NEW atau DISPOSE tidak mungkin untuk pointer untyped
% You cannot use \var{new(p)} or \var{dispose(p)} if \var{p} is an untyped pointer
% because no size is associated to an untyped pointer.
% Accepted for compatibility in \var{tp} and \var{delphi} modes.
parser_e_class_id_expected=03045_E_pengenal kelas diharapkan
% This happens when the compiler scans a procedure declaration that contains
% a dot,
% i.e., a object or class method, but the type in front of the dot is not
% a known type.
parser_e_no_type_not_allowed_here=03046_E_pengenal tipe tidak dibolehkan disini
% You cannot use a type inside an expression.
parser_e_methode_id_expected=03047_E_pengenal metode diharapkan
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_e_header_dont_match_any_member=03048_E_header fungsi tidak sama dengan metode apapun dari kelas "$1"
% This identifier is not a method.
% This happens when the compiler scans a procedure declaration that contains
% a dot, i.e., a object or class method, but the procedure name is not a
% procedure of this type.
parser_d_procedure_start=03049_DL_prosedur/fungsi $1
% When using the \var{-vd} switch, the compiler tells you when it starts
% processing a procedure or function implementation.
parser_e_error_in_real=03050_E_konstan floating point tidak benar
% The compiler expects a floating point expression, and gets something else.
parser_e_fail_only_in_constructor=03051_E_FAIL bisa digunakan hanya dalam konstruktor
% You are using the \var{fail} keyword outside a constructor method.
parser_e_no_paras_for_destructor=03052_E_Destruktor tidak mempunyai parameter
% You are declaring a destructor with a parameter list. Destructor methods
% cannot have parameters.
parser_e_only_class_methods_via_class_ref=03053_E_Hanya metode kelas yang bisa dirujuk dengen perujuk kelas
% This error occurs in a situation like the following:
% \begin{verbatim}
% Type :
%    Tclass = Class of Tobject;
%
% Var C : TClass;
%
% begin
% ...
% C.free
% \end{verbatim}
% \var{Free} is not a class method and hence cannot be called with a class
% reference.
parser_e_only_class_methods=03054_E_Hanya metode kelas bisa diakses dalam metode kelas
% This is related to the previous error. You cannot call a method of an object
% from a inside a class method. The following code would produce this error:
% \begin{verbatim}
% class procedure tobject.x;
%
% begin
%   free
% \end{verbatim}
% Because free is a normal method of a class it cannot be called from a class
% method.
parser_e_case_mismatch=03055_E_Konstan dan tipe CASE tidak sama
% One of the labels is not of the same type as the case variable.
parser_e_illegal_symbol_exported=03056_E_Simbol tidak bisa diekspor dari sebuah librari
% You can only export procedures and functions when you write a library. You
% cannot export variables or constants.
parser_w_should_use_override=03057_W_An metode yang diturunkan tidak terlihat oleh "$1"
% A method that is declared \var{virtual} in a parent class, should be
% overridden in the descendent class with the \var{override} directive. If you
% don't specify the \var{override} directive, you will hide the parent method;
% you will not override it.
parser_e_nothing_to_be_overridden=03058_E_Tidak ada metode dalam kelas leluhurnya untuk di-overridde: "$1"
% You are trying to \var{override} a virtual method of a parent class that does
% not exist.
parser_e_no_procedure_to_access_property=03059_E_Tidak ada anggota disediakan untuk properti akses
% You specified no \var{read} directive for a property.
parser_w_stored_not_implemented=03060_W_Direktif properti tersimpan tidak diimplementasikan
% The \var{stored} directive is not yet implemented
parser_e_ill_property_access_sym=03061_E_Simbol untuk properti akses tidak benar
% There is an error in the \var{read} or \var{write} directives for an array
% property. When you declare an array property, you can only access it with
% procedures and functions. The following code would cause such an error.
% \begin{verbatim}
% tmyobject = class
%   i : integer;
%   property x [i : integer]: integer read I write i;
% \end{verbatim}
%
parser_e_cant_access_protected_member=03062_E_Tidak bisa mengakses field terkunci dari sebuah obyek disini
% Fields that are declared in a \var{protected} section of an object or class
% declaration cannot be accessed outside the module where the object is
% defined, or outside descendent object methods.
parser_e_cant_access_private_member=03063_E_Tidak bisa mengakses field privat dari obyek disini
% Fields that are declared in a \var{private} section of an object or class
% declaration cannot be accessed outside the module where the class is
% defined.
parser_e_overridden_methods_not_same_ret=03066_E_Metode yang di-overridde mempunyai tipe bail sama: "$2" di-override dengan "$1" dimana yang lain mempunyai tipe balik
% If you declare overridden methods in a class definition, they must
% have the same return type.
parser_e_dont_nest_export=03067_E_EXPORT mendeklarasikan fungsi yang tidak bisa diulang
% You cannot declare a function or procedure within a function or procedure
% that was declared as an export procedure.
parser_e_methods_dont_be_export=03068_E_Metode tidak bisa di-EXPORT
% You cannot declare a procedure that is a method for an object as
% \var{export}ed.
parser_e_call_by_ref_without_typeconv=03069_E_Pemanggilan oleh parameter var harus sama persis: diperoleh "$1" diharapkan "$2"
% When calling a function declared with \var{var} parameters, the variables in
% the function call must be of exactly the same type. There is no automatic
% type conversion.
parser_e_no_super_class=03070_E_Kelas bukan kelas leluhurnya dari kelas saat ini
% When calling inherited methods, you are trying to call a method of a non-related
% class. You can only call an inherited method of a parent class.
parser_e_self_not_in_method=03071_E_SELF hanya dibolehkan dalam metode
% You are trying to use the \var{self} parameter outside an object's method.
% Only methods get passed the \var{self} parameters.
parser_e_generic_methods_only_in_methods=03072_E_Metode hanya bisa dalam metode lain dipanggil langsung dengan pengenal tipe dari kelas
% A construction like \var{sometype.somemethod} is only allowed in a method.
parser_e_illegal_colon_qualifier=03073_E_Penggunaan ':' tidak benar
% You are using the format \var{:} (colon) 2 times on an expression that
% is not a real expression.
parser_e_illegal_set_expr=03074_E_pemeriksaan jangkauan salah dalam set konstruktor atau duplikasi elemen set
% The declaration of a set contains an error. Either one of the elements is
% outside the range of the set type, either two of the elements are in fact
% the same.
parser_e_pointer_to_class_expected=03075_E_Pointer ke object diharapkan
% You specified an illegal type in a \var{new} statement.
% The extended syntax of \var{new} needs an object as a parameter.
parser_e_expr_have_to_be_constructor_call=03076_E_Ekspresi harus berupa pemanggilan konstruktor
% When using the extended syntax of \var{new}, you must specify the constructor
% method of the object you are trying to create. The procedure you specified
% is not a constructor.
parser_e_expr_have_to_be_destructor_call=03077_E_Ekspresi harus berupa pamenggilan destruktor
% When using the extended syntax of \var{dispose}, you must specify the
% destructor method of the object you are trying to dispose of.
% The procedure you specified is not a destructor.
parser_e_invalid_record_const=03078_E_Urutan dari elemen record tidak benar
% When declaring a constant record, you specified the fields in the wrong
% order.
parser_e_false_with_expr=03079_E_Tipe ekspresi harus berupa kelas atau tipe record
% A \var{with} statement needs an argument that is of the type \var{record}
% or \var{class}. You are using \var{with} on an expression that is not of
% this type.
parser_e_void_function=03080_E_Prosedur tidak mengembalikan sebuah nilai
% In \fpc, you can specify a return value for a function when using
% the \var{exit} statement. This error occurs when you try to do this with a
% procedure. Procedures cannot return a value.
parser_e_constructors_always_objects=03081_E_konstruktor dan destruktor harus berupa metode
% You're declaring a procedure as destructor or constructor, when the
% procedure isn't a class method.
parser_e_operator_not_overloaded=03082_E_Operator tidak di-overload
% You're trying to use an overloaded operator when it is not overloaded for
% this type.
parser_e_no_such_assignment=03083_E_Tidak mungkin untuk meng-overload penugasan untuk tipe yang sama
% You can not overload assignment for types
% that the compiler considers as equal.
parser_e_overload_impossible=03084_E_Tidak mungkin operator di-overload
% The combination of operator, arguments and return type are
% incompatible.
parser_e_no_reraise_possible=03085_E_Re-raise tidak mungkin disini
% You are trying to raise an exception where it is not allowed. You can only
% raise exceptions in an \var{except} block.
parser_e_no_new_or_dispose_for_classes=03086_E_Sintaks extended dari new atau dispose tidak diperbolehkan untuk kelas
% You cannot generate an instance of a class with the extended syntax of
% \var{new}. The constructor must be used for that. For the same reason, you
% cannot call \var{dispose} to de-allocate an instance of a class, the
% destructor must be used for that.
parser_e_procedure_overloading_is_off=03088_E_Procedure yang di-overload dimatikan
% When using the \var{-So} switch, procedure overloading is switched off.
% Turbo Pascal does not support function overloading.
parser_e_overload_operator_failed=03089_E_Tidak mungkin untuk meng-overload operator ini (overload = lebih baik)
% You are trying to overload an operator which cannot be overloaded.
% The following operators can be overloaded :
% \begin{verbatim}
%    +, -, *, /, =, >, <, <=, >=, is, as, in, **, :=
% \end{verbatim}
parser_e_comparative_operator_return_boolean=03090_E_Operator komparatif harus mengembalikan nilai boolean
% When overloading the \var{=} operator, the function must return a boolean
% value.
parser_e_only_virtual_methods_abstract=03091_E_Hanya metode virtual yang bisa di-abstrak
% You are declaring a method as abstract, when it is not declared to be
% virtual.
parser_f_unsupported_feature=03092_F_Penggunaan fitur yang tidak didukung!
% You're trying to force the compiler into doing something it cannot do yet.
parser_e_mix_of_classes_and_objects=03093_E_Campuran dari obyek yang berbeda (kelas, obyek, antarmuka, dll) tidak dibolehkan
% You cannot derive \var{objects}, \var{classes}, \var{cppclasses} and \var{interfaces} interttwined . E.g.
% a class cannot have an object as parent and vice versa.
parser_w_unknown_proc_directive_ignored=03094_W_Direktif procedure tidak dikenal harus diabaikan: "$1"
% The procedure directive you specified is unknown.
parser_e_absolute_only_one_var=03095_E_absolute hanya bisa dikaitkan ke satu variabel
% You cannot specify more than one variable before the \var{absolute} directive.
% Thus, the following construct will provide this error:
% \begin{verbatim}
% Var Z : Longint;
%     X,Y : Longint absolute Z;
% \end{verbatim}
% \item [ absolute can only be associated a var or const ]
% The address of a \var{absolute} directive can only point to a variable or a
% typed constant. Therefore, the following code will produce this error:
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
%
parser_e_absolute_only_to_var_or_const=03096_E_absolute hanya bisa dikaitkan dengan var atau const
% The address of a \var{absolute} directive can only point to a variable or
% constant. Therefore, the following code will produce this error:
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
%
parser_e_initialized_only_one_var=03097_E_Hanya satu variabel bisa dinisialisasi
% You cannot specify more than one variable with a initial value
% in Delphi mode.
parser_e_abstract_no_definition=03098_E_Metode abstrak tidak boleh mempunyai definisi apapun (dengan badan fungsi)
% Abstract methods can only be declared, you cannot implement them. They
% should be overridden by a descendant class.
parser_e_overloaded_must_be_all_global=03099_E_Fungsi yang di-overload  ini tidak bisa lokal (harus diekspor)
% You are defining a overloaded function in the implementation part of a unit,
% but there is no corresponding declaration in the interface part of the unit.
parser_w_virtual_without_constructor=03100_W_Metode virtual digunakan tanpa konstruktor dalam "$1"
% If you declare objects or classes that contain virtual methods, you need
% to have a constructor and destructor to initialize them. The compiler
% encountered an object or class with virtual methods that doesn't have
% a constructor/destructor pair.
parser_c_macro_defined=03101_CL_Didefinisikan makro: $1
% When \var{-vc} is used, the compiler tells you when it defines macros.
parser_c_macro_undefined=03102_CL_Tidak didefinisikan makro: $1
% When \var{-vc} is used, the compiler tells you when it undefines macros.
parser_c_macro_set_to=03103_CL_Makro $1 di-set ke $2
% When \var{-vc} is used, the compiler tells you what values macros get.
parser_i_compiling=03104_I_Mengkompilasi $1
% When you turn on information messages (\var{-vi}), the compiler tells you
% what units it is recompiling.
parser_u_parsing_interface=03105_UL_Memindai antarmuka dari unit $1
% This tells you that the reading of the interface
% of the current unit starts
parser_u_parsing_implementation=03106_UL_Pemindaian implementasi dari $1
% This tells you that the code reading of the implementation
% of the current unit, library or program starts
parser_d_compiling_second_time=03107_DL_Mengkompilasi $1 untuk kedua kali
% When you request debug messages (\var{-vd}) the compiler tells you what
% units it recompiles for the second time.
parser_e_no_property_found_to_override=03109_E_Tidak ada properti ditemukan untuk override
% You want to override a property of a parent class, when there is, in fact,
% no such property in the parent class.
parser_e_only_one_default_property=03110_E_Hanya satu properti default yang dibolehkan
% You specified a property as \var{Default}, but the class already has a
% default property, and a class can have only one default property.
parser_e_property_need_paras=03111_E_Properti default harus berupa properti array
% Only array properties of classes can be made \var{default} properties.
parser_e_constructor_cannot_be_not_virtual=03112_E_Kontruktor virtual tidak didukung dalam model obyek kelas
% You cannot have virtual constructors in objects. You can only have them
% in classes.
parser_e_no_default_property_available=03113_E_Properti default tidak tersedia
% You are trying to access a default property of a class, but this class (or one of
% its ancestors) doesn't have a default property.
parser_e_cant_have_published=03114_E_Kelas tidak mempunyai seksi yang dipublikasikan, gunakan saklar {$M+}
% If you want a \var{published} section in a class definition, you must
% use the \var{\{\$M+\}} switch, whch turns on generation of type
% information.
parser_e_forward_declaration_must_be_resolved=03115_E_Deklarasi Forward dari kelas "$1" harus dipecahkan disini untuk menggunakan kelas sebagai leluhur
% To be able to use an object as an ancestor object, it must be defined
% first. This error occurs in the following situation:
% \begin{verbatim}
%  Type ParentClas = Class;
%       ChildClass = Class(ParentClass)
%         ...
%       end;
% \end{verbatim}
% Where \var{ParentClass} is declared but not defined.
parser_e_no_local_operator=03116_E_Operator lokal tidak didukung
% You cannot overload locally, i.e. inside procedures or function
% definitions.
parser_e_proc_dir_not_allowed_in_interface=03117_E_Direktif procedure "$1" tidak dibolehkan dalam seksi interface
% This procedure directive is not allowed in the \var{interface} section of
% a unit. You can only use it in the \var{implementation} section.
parser_e_proc_dir_not_allowed_in_implementation=03118_E_Direktif procedure "$1" tidak dibolehkan dalam seksi implementasi
% This procedure directive is not defined in the \var{implementation} section of
% a unit. You can only use it in the \var{interface} section.
parser_e_proc_dir_not_allowed_in_procvar=03119_E_Direktif procedure "$1" tidak dibolehkan dalam deklarasi procvar
% This procedure directive cannot be part of a procedural or function
% type declaration.
parser_e_function_already_declared_public_forward=03120_E_Function sudah dideklarasikan Public/Forward "$1"
% You will get this error if a function is defined as \var{forward} twice.
% Or it is once in the \var{interface} section, and once as a \var{forward}
% declaration in the \var{implmentation} section.
parser_e_not_external_and_export=03121_E_Tidak bisa menggunakan EXPORT dan EXTERNAL
% These two procedure directives are mutually exclusive
parser_w_not_supported_for_inline=03123_W_"$1" belum didukung di dalam procedure/function inline
% Inline procedures don't support this declaration.
parser_w_inlining_disabled=03124_W_Inlining dimatikan
% Inlining of procedures is disabled.
parser_i_writing_browser_log=03125_I_Penulisan log Browser $1
% When information messages are on, the compiler warns you when it
% writes the browser log (generated with the \var{\{\$Y+ \}} switch).
parser_h_maybe_deref_caret_missing=03126_H_mungkin perbedaan pointer yang kurang
% The compiler thinks that a pointer may need a dereference.
parser_f_assembler_reader_not_supported=03127_F_Pembaca assembler tidak didukung
% The selected assembler reader (with \var{\{\$ASMMODE xxx\}} is not
% supported. The compiler can be compiled with or without support for a
% particular assembler reader.
parser_e_proc_dir_conflict=03128_E_Direktif procedure "$1" konflik dengan direktif lain
% You specified a procedure directive that conflicts with other directives.
% for instance \var{cdecl} and \var{pascal} are mutually exclusive.
parser_e_call_convention_dont_match_forward=03129_E_Konvensi pemanggilan tidak sama dengan forward
% This error happens when you declare a function or procedure with
% e.g. \var{cdecl;} but omit this directive in the implementation, or vice
% versa. The calling convention is part of the function declaration, and
% must be repeated in the function definition.
parser_e_property_cant_have_a_default_value=03131_E_Properti tidak mempunyai nilai default
% Set properties or indexed properties cannot have a default value.
parser_e_property_default_value_must_const=03132_E_Nilai default dari properti harus konstan
% The value of a \var{default} declared property must be known at compile
% time. The value you specified is only known at run time. This happens
% .e.g. if you specify a variable name as a default value.
parser_e_cant_publish_that=03133_E_Simbol tidak bisa dipublikasikan, hanya bisa sebuah kelas
% Only class type variables can be in a \var{published} section of a class
% if they are not declared as a property.
parser_e_cant_publish_that_property=03134_E_Properti seperti itu tidak bisa di-publish
% Properties in a \var{published} section cannot be array properties.
% they must be moved to public sections. Properties in a \var{published}
% section must be an ordinal type, a real type, strings or sets.
parser_e_empty_import_name=03136_E_Nama impor dibutuhkan
% Some targets need a name for the imported procedure or a \var{cdecl} specifier
parser_e_division_by_zero=03138_E_Pembagian dengan nol
% There is a division by zero encounted
parser_e_invalid_float_operation=03139_E_Operasi floating point tidak benar
% An operation on two real type values produced an overflow or a division
% by zero.
parser_e_array_lower_less_than_upper_bound=03140_E_Jangkauan batas atas kurang dari batas bawah
% The upper bound of a an array declaration is less than the lower bound and this
% is not possible
parser_w_string_too_long=03141_W_string "$1" lebih panjang daripada "$2"
% The size of the constant string is larger than the size you specified in
% string type definition
parser_e_string_larger_array=03142_E_panjang string melebihi panjang karakter array
% The size of the constant string is larger than the size you specified in
% the array[x..y] of char definition
parser_e_ill_msg_expr=03143_E_Ekspresi tidak benar setelah direktif pesan
% \fpc supports only integer or string values as message constants
parser_e_ill_msg_param=03144_E_Pengendali pesan hanya bisa dipanggil sekali oleh parameter ref.
% A method declared with the \var{message}-directive as message handler
% can take only one parameter which must be declared as call by reference
% Parameters are declared as call by reference using the \var{var}-directive
parser_e_duplicate_message_label=03145_E_Label pesan duplikasi: "$1"
% A label for a message is used twice in one object/class
parser_e_self_in_non_message_handler=03146_E_Self hanya bisa sebagai parameter eksplisit dalam metode yakni pengendali pesan
% The self parameter can only be passed explicitly to a method which
% is declared as message handler.
parser_e_threadvars_only_sg=03147_E_Threadvars hanya bisa berupa statik atau global
% Threadvars must be static or global, you can't declare a thread
% local to a procedure. Local variables are always local to a thread,
% because every thread has its own stack and local variables
% are stored on the stack
parser_f_direct_assembler_not_allowed=03148_F_Assembler langsung tidak didukung untuk format output biner
% You can't use direct assembler when using a binary writer, choose an
% other outputformat or use an other assembler reader
parser_w_no_objpas_use_mode=03149_W_Jangan ambil unit OBJPAS secara manual, lebih baik gunakan \{\$mode objfpc\} atau \{\$mode delphi\}
% You are trying to load the ObjPas unit manually from a uses clause. This is
% not a good idea. Use the \var{\{\$mode objfpc\}} or
% \var{\{\$mode delphi\}}
% directives which load the unit automatically
parser_e_no_object_override=03150_E_OVERRIDE tidak bisa digunakan dalam objects
% Override is not supported for objects, use \var{virtual} instead to override
% a method of a parent object
parser_e_cant_use_inittable_here=03151_E_Tipe data yang membutuhkan initialization/finalization tidak bisa digunakan dalam variant records
% Some data type (e.g. \var{ansistring}) needs initialization/finalization
% code which is implicitly generated by the compiler. Such data types
% can't be used in the variant part of a record.
parser_e_resourcestring_only_sg=03152_E_Resourcestrings hanya bisa berupa statik atau global
% Resourcestring can not be declared local, only global or using the static
% directive.
parser_e_exit_with_argument_not__possible=03153_E_Exit dengan argumen tidak bisa digunakan disini
% an exit statement with an argument for the return value can't be used here, this
% can happen e.g. in \var{try..except} or \var{try..finally} blocks
parser_e_stored_property_must_be_boolean=03154_E_Tipe simbol penyimpanan harus berupa boolean
% If you specify a storage symbol in a property declaration, it must be of
% the type boolean
parser_e_ill_property_storage_sym=03155_E_Simbol ini tidak dibolehkan sebagai simbol penyimpanan
% You can't use this type of symbol as storage specifier in property
% declaration. You can use only methods with the result type boolean,
% boolean class fields or boolean constants
parser_e_only_publishable_classes_can__be_published=03156_E_Hanya kelas yang dikompilasi dalam mode $M+ bisa di-published
% In the published section of a class can be only class as fields used which
% are compiled in \var{\{\$M+\}} or which are derived from such a class. Normally
% such a class should be derived from TPersitent
parser_e_proc_directive_expected=03157_E_Direktif procedure diharapkan
% This error is triggered when you have a \var{\{\$Calling\}} directive without
% a calling convention specified.
% It also happens when declaring a procedure in a const block and you
% used a ; after a procedure declaration which must be followed by a
% procedure directive.
% Deklarasi yang benar adalah:
% \begin{verbatim}
% const
%   p : procedure;stdcall=nil;
%   p : procedure stdcall=nil;
% \end{verbatim}
parser_e_invalid_property_index_value=03158_E_Nilai untuk indeks properti harus tipe ordinal
% The value you use to index a property must be of an ordinal type, for
% example an integer or enumerated type.
parser_e_procname_to_short_for_export=03159_E_Nama procedure terlalu pendek untuk diekspor
% The length of the procedure/function name must be at least 2 characters
% long. This is because of a bug in dlltool which doesn't parse the .def
% file correct with a name of length 1.
parser_e_dlltool_unit_var_problem=03160_E_Tidak ada entri DEFFILE yang bisa dibuat untuk unit global vars
parser_e_dlltool_unit_var_problem2=03161_E_Kompilasi tanpa opsi -WD
% You need to compile this file without the -WD switch on the
% commandline
parser_f_need_objfpc_or_delphi_mode=03162_F_Anda memerlukan mode ObjFpc (-S2) atau Delphi (-Sd) untuk mengkompilasi modul ini
% You need to use \{\$mode objfpc\} or \{\$mode delphi\} to compile this file.
% Or use the equivalent commandline switches -S2 or -Sd.
parser_e_no_export_with_index_for_target=03163_E_Tidak bisa mengekspor dengan indeks dibawah $1
% Exporting of functions or procedures with a specified index is not
% supported on this target.
parser_e_no_export_of_variables_for_target=03164_E_Pengeksporan variabel tidak didukung pada $1
% Exporting of variables is not supported on this target.
parser_e_improper_guid_syntax=03165_E_Sintaks pengimpor GUID
parser_w_interface_mapping_notfound=03168_W_Procedure yang dinamai "$1" tidak ditemukan yang cocok untuk mengimplementasikan $2.$3
parser_e_interface_id_expected=03169_E_Pengenal antarmuka diharapkan
% This happens when the compiler scans a \var{class} declaration that contains
% \var{interface} function name mapping code like this:
% \begin{verbatim}
% type
%   TMyObject = class(TObject, IDispatch)
%     function IUnknown.QueryInterface=MyQueryInterface;
%     ....
% \end{verbatim}
% and the \var{interface} before the dot not listed in the inheritance list.
parser_e_type_cant_be_used_in_array_index=03170_E_Tipe "$1" tidak bisa digunakan sebagai tipe indeks array
% Types like \var{qword} or \var{int64} aren't allowed as array index type
parser_e_no_con_des_in_interfaces=03171_E_Con- dan destructors tidak dibolehkan dalam interfaces
% Constructor and destructor declarations aren't allowed in interface
% In the most cases the method \var{QueryInterface} of \var{IUnknown} can
% be used to create a new interface.
parser_e_no_access_specifier_in_interfaces=03172_E_Pengenal Access tidak bisa digunakan dalam INTERFACES
% The access specifiers \var{public}, \var{private}, \var{protected} and
% \var{pusblished} can't be used in interfaces because all methods
% of an interfaces must be public.
parser_e_no_vars_in_interfaces=03173_E_Interface tidak berisi fields
% Declarations of fields aren't allowed in interfaces. An interface
% can contain only methods
parser_e_no_local_proc_external=03174_E_Tidak bisa mendeklarasikan procedure lokal sebagai EXTERNAL
% Declaring local procedures as external is not possible. Local procedures
% get hidden parameters that will make the chance of errors very high
parser_w_skipped_fields_before=03175_W_Beberapa fields datang sebelum "$1" diinisialisasi
% Dalam mode Delphi, tidak semua field dari tipe konstan record harus diinisialisasi,
% tapi kompilator mengingatkan anda ketika mendeteksi situasi tersebut.
parser_e_skipped_fields_before=03176_E_Beberapa field datang sebelum "$1" diinisialisasi
% Dalam semua mode sintaks kecuali mode Delphi, anda tida bisa membiarkan beberapa field tidak 
% diinisialisasi ditengah konstan tipe record
parser_w_skipped_fields_after=03177_W_Beberapa field datang setelah "$1" tidak diinisialisasi
% You can leave some fields at the end of a type constant record uninitialized
% (the compiler will initialize them to zero automatically). This may be the cause
% of subtle problems.
parser_e_varargs_need_cdecl_and_external=03178_E_Direktif VarArgs tanpa CDecl dan External
% The varargs directive can only be used with procedures or functions
% that are declared with \var{cdecl} and \var{external} directives. The varargs directive
% is only meant to provide a compatible interface to C functions like printf.
parser_e_self_call_by_value=03179_E_Self harus parameter normal (dipanggil-dengan-nilai)
% You can't declare self as a const or var parameter, it must always be
% a call-by-value parameter
parser_e_interface_has_no_guid=03180_E_Interface "$1" tidak mempunyai identifikasi interface
% When you want to assign an interface to a constant, then the interface
% must have a GUID value set.
parser_e_illegal_field_or_method=03181_E_Field class tidak dikenal atau pengenal metode "$1"
% Properties must refer to a field or method in the same class.
parser_w_proc_overriding_calling=03182_W_Overriding konvensi pemanggilan "$1" dengan "$2"
% There are two directives in the procedure declaration that specify a calling
% convention. Only the last directive will be used
parser_e_no_procvarobj_const=03183_E_Tipe konstan dari tipe "procedure of object" hanya bisa diinisialisasi dengan NIL
% You can't assign the address of a method to a typed constant which has a
% 'procedure of object' type, because such a constant requires two addresses:
% that of the method (which is known at compile time) and that of the object or
% class instance it operates on (which can not be known at compile time).
parser_e_default_value_only_one_para=03184_E_Nilai default hanya bisa ditempatkan ke satu parameter
parser_e_default_value_expected_for_para=03185_E_Parameter default dibutuhkan untuk "$1"
parser_w_unsupported_feature=03186_W_Penggunaan daru fitur yang tidak didukung!
% You're trying to force the compiler into doing something it cannot do yet.
parser_h_c_arrays_are_references=03187_H_C arrays dioper dengan referensi
% Any array passed to a C functions is passed
% by a pointer (i.e. by reference).
parser_e_C_array_of_const_must_be_last=03188_E_C array of const harus merupakan argumen terakhir
% You can not add any other argument after an \var{array of const} for
% \var{cdecl} functions, as the size pushed on stack for this argument is
% not known.
parser_h_type_redef=03189_H_Redefinisi tipe "$1"
% This is an indicator that a previously declared type is
% being redefined as something else. This may, or may not
% be, a cause for errors.
parser_w_cdecl_has_no_high=03190_W_fungsi cdecl'ared tidak mempunyai parameter high
% Functions declared with cdecl modifier do not pass an extra implicit parameter.
parser_w_cdecl_no_openstring=03191_W_fungsi cdecl'ared tidak mendukung strings terbuka
% Openstring is not supported for cdecl'ared functions.
parser_e_initialized_not_for_threadvar=03192_E_Tidak bisa menginisialisasi variabel yang dideklarasikan sebagai threadvar
% Variables declared as threadvar can not be initialized with a default value.
% The variables will always be filled with zero at the start of a new thread.
parser_e_msg_only_for_classes=03193_E_Direktif Message hanya dibolehkan dalam Classes
% The message directive is only supported for Class types.
parser_e_procedure_or_function_expected=03194_E_Procedure atau Function diharapkan
% A class method can only be specified for procedures and functions.
parser_e_illegal_calling_convention=03195_W_Direktif konvensi pemanggilan diabaikan: "$1"
% Some calling conventions are supported only by certain CPUs. I.e. most non-i386 ports support
% only the standard ABI calling convention of the CPU.
parser_e_no_object_reintroduce=03196_E_REINTRODUCE tidak bisas digunakan dalam objects
% \var{reintroduce} is not supported for objects.
parser_e_paraloc_only_one_para=03197_E_Setiap argumen harus mempunyai lokasinya sendiri
% If locations for arguments are specified explicitly as it is required by
% some syscall conventions, each argument must have it's only location, things
% like \var{procedure p(i,j : longint 'r1');} aren't allowed
parser_e_paraloc_all_paras=03198_E_Setiap argumen harus mempunyai lokasi eksplisit
% If one argument has an explicit argument location, all arguments of a procedure
% must have one.
parser_e_illegal_explicit_paraloc=03199_E_Lokasi argumen tidak dikenal
% The location specified for an argument isn't recognized by the compiler
parser_e_32bitint_or_pointer_variable_expected=03200_E_32 Bit-Integer atau variabel pointer diharapkan
% The libbase for MorphOS/AmigaOS can be give only as \var{longint}, \var{dword} or any pointer variable.
parser_e_goto_outside_proc=03201_E_Pernyataan Goto tidak dibolehkan diantaran procedure yang berneda
% Tidak dibolehkan menggunakan pernyataan \var{goto} merujuk labels diluar
% procedure saat ini. Contoh berikut menampilkan persoalan:
% \begin{verbatim}
% ...
%   procedure p1;
%   label
%     l1;
%
%     procedure p2;
%     begin
%       goto l1; // goto ini TIDAK diperbolehkan
%     end;
%
%   begin
%     p2
%   l1:
%   end;
% ...
%
% \end{verbatim}
parser_f_too_complex_proc=03202_F_Procedure terlalu rumit, memerlukan terlalu banyak register
% Your procedure body is too long for the compiler. You should split the
% procedure into multiple smaller procedures.
parser_e_illegal_expression=03203_E_Ekspresi tidak benar
% This can occur under many circumstances. Mostly when trying to evaluate
% constant expressions.
parser_e_invalid_integer=03204_E_Ekspresi integer tidak benar
% You made an expression which isn't an integer, and the compiler expects the
% result to be an integer.
parser_e_invalid_qualifier=03205_E_Kualifier tidak benar
% One of the following is happening :
% \begin{itemize}
% \item You're trying to access a field of a variable that is not a record.
% \item You're indexing a variable that is not an array.
% \item You're dereferencing a variable that is not a pointer.
% \end{itemize}
parser_e_upper_lower_than_lower=03206_E_Batas jangkauan tinggi < batas jangkauan rendah
% You are declaring a subrange, and the lower limit is higher than the high
% limit of the range.
parser_e_macpas_exit_wrong_param=03207_E_Parameter Exit harus nama dari procedure yang digunakannya
% Non local exit is not allowed. This error occurs only in mode MacPas.
parser_e_illegal_assignment_to_count_var=03208_E_Penugasan untuk variabel for-loop "$1" tidak benar
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings. You can also not assign values to
% loop variables inside the loop (except in Delphi and TP modes). Use a while or
% repeat loop instead if you need to do something like that, since those
% constructs were built for that.
parser_e_no_local_var_external=03209_E_Tidak bisa mendeklarasikan variabel lokal sebagai EXTERNAL
% Declaring local variables as external is not allowed. Only global variables can reference
% to external variables.
parser_e_proc_already_external=03210_E_Procedure sudah dideklarasikan EXTERNAL
% The procedure is already declared with the EXTERNAL directive in an interface or
% forward declaration.
parser_w_implicit_uses_of_variants_unit=03211_W_Penggunaan implisit dari unit Variants
% The Variant type is used in the unit without any used unit using the Variants unit. The
% compiler has implicitly added the Variants unit to the uses list. To remove this warning
% the Variants unit needs to be added to the uses statement.
parser_e_no_static_method_in_interfaces=03212_E_Metode Class dan statik tidak bisa digunakan dalam INTERFACES
% The specifier \var{class} and directive \var{static} can't be used in interfaces
% because all methods of an interfaces must be public.
parser_e_arithmetic_operation_overflow=03213_E_Berlebihan dalam operasi aritmetik
% An operation on two integers values produced an overflow
parser_e_protected_or_private_expected=03214_E_Protected atau private diharapkan
% \var{strict} can be only used together with \var{protected} or \var{private}.
parser_e_illegal_slice=03215_E_SLICE tidak bisa digunakan diluar daftar parameter
% \var{slice} can be used only for arguments accepting an open array parameter
parser_e_dispinterface_cant_have_parent=03216_E_DISPINTERFACE tidak mempunyai parent class
% A DISPINMTERFACE is a special type of interface which can't have a parent class
parser_e_dispinterface_needs_a_guid=03217_E_DISPINTERFACE memerlukan guid
% A DISPINMTERFACE always needs an interface identification
parser_w_overridden_methods_not_same_ret=03218_W_Metode overridden harus mempunyai tipe hasil yang terkait. Kode ini bisa rusak, tergantung pada kesalahan pemindai Delphi ("$2" di-override oleh "$1" yang mempunyai tipe hasil lain)
% If you declare overridden methods in a class definition, they must
% have the same return type. Some versions of Delphi allow you to change the
% return type of interface methods, and even to change procedures into
% functions, but the resulting code may crash depending on the types used
% and the way the methods are called.
parser_e_dispid_must_be_ord_const=03219_E_Dispatch IDs harus berupa konstan ordinal
% \end{description}
#
# Type Checking
#
# 04049 is the last used one
#
% \section{Type checking errors}
% This section lists all errors that can occur when type checking is
% performed.
% \begin{description}
type_e_mismatch=04000_E_Tipe tidak sama
% This can happen in many cases:
% \begin{itemize}
% \item The variable you're assigning to is of a different type than the
% expression in the assignment.
% \item You are calling a function or procedure with parameters that are
% incompatible with the parameters in the function or procedure definition.
% \end{itemize}
type_e_incompatible_types=04001_E_Tipe tidak kompatibel: didapat "$1" diharapkan "$2"
% There is no conversion possible between the two types
% Another possiblity is that they are declared in different
% declarations:
% \begin{verbatim}
% Var
%    A1 : Array[1..10] Of Integer;
%    A2 : Array[1..10] Of Integer;
%
% Begin
%    A1:=A2; { pernyataan ini juga menghasilkan kesalahan,
%              karena pemeriksaan strict type dari pascal }
% End.
% \end{verbatim}
type_e_not_equal_types=04002_E_Tipe tidak sama antara "$1" dan "$2"
% The types are not equal
type_e_type_id_expected=04003_E_Type identifier expected
% The identifier is not a type, or you forgot to supply a type identifier.
type_e_variable_id_expected=04004_E_Pengenal variabel diharapkan
% This happens when you pass a constant to a routine (such as \var{Inc} var or \var{Dec})
% when it expects a variable. You can only pass variables as arguments to these functions.
type_e_integer_expr_expected=04005_E_ekspresi integer diharapkan, tetapi didapat "$1"
% The compiler expects an expression of type integer, but gets a different
% type.
type_e_boolean_expr_expected=04006_E_Ekspresi Boolean diharapkan, tapi didapat "$1"
% The expression must be a boolean type, it should be return true or
% false.
type_e_ordinal_expr_expected=04007_E_Ekspresi ordinal diharapkan
% The expression must be of ordinal type, i.e., maximum a \var{Longint}.
% This happens, for instance, when you specify a second argument
% to \var{Inc} or \var{Dec} that doesn't evaluate to an ordinal value.
type_e_pointer_type_expected=04008_E_tipe pointer diharapkan, tapi didapat "$1"
% The variable or expression isn't of the type \var{pointer}. This
% happens when you pass a variable that isn't a pointer to \var{New}
% or \var{Dispose}.
type_e_class_type_expected=04009_E_tipe class diharapkan, tapi didapat "$1"
% The variable of expression isn't of the type \var{class}. This happens
% typically when
% \begin{enumerate}
% \item The parent class in a class declaration isn't a class.
% \item An exception handler (\var{On}) contains a type identifier that
% isn't a class.
% \end{enumerate}
type_e_cant_eval_constant_expr=04011_E_Tidak bisa mengevaluasi ekspresi konstan
% This error can occur when the bounds of an array you declared does
% not evaluate to ordinal constants
type_e_set_element_are_not_comp=04012_E_Set element tidak kompatibel
% You are trying to make an operation on two sets, when the set element types
% are not the same. The base type of a set must be the same when taking the
% union
type_e_set_operation_unknown=04013_E_Operasi tidak diimplementasikan untuk sets
% several binary operations are not defined for sets
% like div mod ** (also >= <= for now)
type_w_convert_real_2_comp=04014_W_Konversi tipe otomatis dari tipe floating ke COMP yang merupakan tipe integer
% An implicit type conversion from a real type to a \var{comp} is
% encountered. Since \var{comp} is a 64 bit integer type, this may indicate
% an error.
type_h_use_div_for_int=04015_H_lebih baik gunakan DIV untuk mendapatkan hasil integer
% When hints are on, then an integer division with the '/' operator will
% procuce this message, because the result will then be of type real
type_e_strict_var_string_violation=04016_E_tipe string tidak sama, karena ada mode $V+
% When compiling in \var{\{\$V+\}} mode, the string you pass as a parameter
% should be of the exact same type as the declared parameter of the procedure.
type_e_succ_and_pred_enums_with_assign_not_possible=04017_E_succ atau pred pada enums dengan penugasan tidak mungkin
% When you declared an enumeration type which has assignments in it, as in C,
% like in the following:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% you cannot use the \var{Succ} or \var{Pred} functions on them.
type_e_cant_read_write_type=04018_E_Tidak bisa membaca atau menulis variabel dari tipe ini
% You are trying to \var{read} or \var{write} a variable from or to a
% file of type text, which doesn't support that. Only integer types,
% reals, pchars and strings can be read from/written to a text file.
% Booleans can only be written to text files.
type_e_no_readln_writeln_for_typed_file=04019_E_Tidak bisa menggunakan readln atau writeln pada typed file
% \var{readln} and \var{writeln} are only allowed for text files.
type_e_no_read_write_for_untyped_file=04020_E_Tidak bisa menggunakan read atau write pada untyped file.
% \var{read} and \var{write} are only allowed for text or typed files.
type_e_typeconflict_in_set=04021_E_Tipe konflik antara elemen set
% There is at least one set element which is of the wrong type, i.e. not of
% the set type.
type_w_maybe_wrong_hi_lo=04022_W_lo/hi(dword/qword) menghasilkan upper/lower word/dword
% \fpc supports an overloaded version of \var{lo/hi} for \var{longint/dword/int64/qword}
% which returns the lower/upper word/dword of the argument. TP always uses
% a 16 bit \var{lo/hi} which returns always bits 0..7 for \var{lo} and the
% bits 8..15 for \var{hi}. If you want the TP behavior you have
% to type cast the argument to \var{word/integer}
type_e_integer_or_real_expr_expected=04023_E_Ekspresi integer atau real diharapkan
% The first argument to \var{str} must a real or integer type.
type_e_wrong_type_in_array_constructor=04024_E_Tipe "$1" salah dalam constructor array
% You are trying to use a type in an array constructor which is not
% allowed.
type_e_wrong_parameter_type=04025_E_Tipe argumen no. $1: tidak kompatibel, didapat "$2", diharapkan "$3"
% You are trying to pass an invalid type for the specified parameter.
type_e_no_method_and_procedure_not_compatible=04026_E_Method (variabel) dan Procedure (variabel) tidak kompatibel
% You can't assign a method to a procedure variable or a procedure to a
% method pointer.
type_e_wrong_math_argument=04027_E_Konstan yang disalurkan ke fungsi math internal tidak benar
% The constant argument passed to a ln or sqrt function is out of
% the definition range of these functions.
type_e_no_addr_of_constant=04028_E_Tidak bisa mendapatkan alamat dari konstan
% It is not possible to get the address of a constant, because they
% aren't stored in memory, you can try making it a typed constant.
type_e_argument_cant_be_assigned=04029_E_Argumen tidak bisa diisi
% Only expressions which can be on the left side of an
% assignment can be passed as call by reference argument
% Remark: Properties can be only
% used on the left side of an assignment, but they cannot be used as arguments
type_e_cannot_local_proc_to_procvar=04030_E_Tidak bisa menempatkan procedure/function lokal ke variabel procedure
% It's not allowed to assign a local procedure/function to a
% procedure variable, because the calling of local procedure/function is
% different. You can only assign local procedure/function to a void pointer.
type_e_no_assign_to_addr=04031_E_Tidak bisa menempatkan nilai ke suatu alamat
% It is not allowed to assign a value to an address of a variable,constant,
% procedure or function. You can try compiling with -So if the identifier
% is a procedure variable.
type_e_no_assign_to_const=04032_E_Tidak bisa menempatkan nilai ke variabel konstan
% It's not allowed to assign a value to a variable which is declared
% as a const. This is normally a parameter declared as const, to allow
% changing the value make the parameter as a value parameter or a var.
type_e_array_required=04033_E_Tipe array yang dibutuhkan
% If you are accessing a variable using an index '[<x>]' then
% the type must be an array. In FPC mode also a pointer is allowed.
type_e_interface_type_expected=04034_E_tipe interface diharapkan, tapi didapat "$1"
type_w_mixed_signed_unsigned=04035_W_Pencampuran ekspresi signed dan longwords menghasilkan 64bit
% If you divide (or calculate the modulus of) a signed expression by a longword (or vice versa),
% or if you have overflow and/or range checking turned on and use an arithmetic
% expression (+, -, *, div, mod) in which both signed numbers and longwords appear,
% then everything has to be evaluated in 64bit which is slower than normal
% 32bit arithmetic. You can avoid this by typecasting one operand so it
% matches the result type of the other one.
type_w_mixed_signed_unsigned2=04036_W_Pencampuran ekspresi signed dancardinal disini menyebabkan kesalahan pemeriksaan jangkauan
% If you use a binary operator (and, or, xor) and one of
% the operands is a longword while the other one is a signed expression, then,
% if range checking is turned on, you may get a range check error because in
% such a case both operands are converted to longword before the operation is
% carried out. You can avoid this by typecasting one operand so it
% matches the result type of the other one.
type_e_typecast_wrong_size_for_assignment=04037_E_Typecast mempunyai ukuran berbeda ($1 -> $2) dalam penempatan
% Type casting to a type with a different size is not allowed when the variable is
% used for assigning.
type_e_array_index_enums_with_assign_not_possible=04038_E_enums dengan penempatan tidak bisa digunakan sebagai indeks array
% When you declared an enumeration type which has assignments in it, as in C,
% like in the following:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% you cannot use it as index of an array.
type_e_classes_not_related=04039_E_Tipe Class atau Object "$1" dan "$2" tidak berkaitan
% There is a typecast from one class or object to another while the class/object
% are not related. This will probably lead to errors
type_w_classes_not_related=04040_W_Tipe Class "$1" dan "$2" tidak berkaitan
% There is a typecast from one class or object to another while the class/object
% are not related. This will probably lead to errors
type_e_class_or_interface_type_expected=04041_E_Tipe Class atau interface diharapkan, tapi didapat "$1"
type_e_type_is_not_completly_defined=04042_E_Tipe "$1" tidak didefinisikan dengan lengkap
type_w_string_too_long=04043_W_Literal String mempunyai lebih banyak karakter daripada panjang string pendek
% The size of the constant string, which is assigned to a shortstring,
% is longer than the maximum size of the shortstring
type_w_signed_unsigned_always_false=04044_W_Perbandingan selalu false karena jangkauan nilai
% There is a comparison between an unsigned value and a signed constant which is
% less than zero. Because of type promotion, the statement will always evaluate to
% false. Exlicitly typecast the constant to the correct range to avoid this problem.
type_w_signed_unsigned_always_true=04045_W_Perbandingan selalu true karena jangkauan nilai
% There is a comparison between an unsigned value and a signed constant which is
% less than zero. Because of type promotion, the statement will always evaluate to
% true. Exlicitly typecast the constant to the correct range to avoid this problem.
type_w_instance_with_abstract=04046_W_Pembentukan class "$1" dengan metode abstrak
% An instance of a class is created which contains non-implemented abstract
% methods. This will probably lead to a runtime error 211 in the code if that
% routine is ever called. All abstract methods should be overriden.
type_h_in_range_check=04047_H_Operan kiri dari operator IN seharusnya berukuran byte
% The left operand of the \var{in} operator is not an ordinal or enumeration which fits
% within 8-bits, this may lead to range check errors. The \var{in} operator
% currently only supports a left operand which fits within a byte. In the case of
% enumerations, the size of an element of an enumeration can be controlled with
% the \var{\{\$PACKENUM\}} or \var{\{\$Zn\}} switches.
type_w_smaller_possible_range_check=04048_W_Ukuran tipe tidak sama, kemungkinan kehilangan data / kesalahan pemeriksaan jangkauan
% There is an assignment to a smaller type than the source type. This means that
% this may cause a range-check error, or may lead to possible loss of data.
type_h_smaller_possible_range_check=04049_H_Ukuran tipe tidak sama, kemungkinan kehilangan data / kesalahan pemeriksaan jangkauan
% There is an assignment to a smaller type than the source type. This means that
% this may cause a range-check error, or may lead to possible loss of data.
type_e_cant_take_address_of_abstract_method=04050_E_Alamat dari metode abstrak tidak bisa diambil
% An abstract method has no body, so the address of an abstract method can't be taken.
type_e_operator_not_allowed=04051_E_Operator tidak bisa diterapkan untuk tipe operan
% You are trying an operator that is not available for the type of the
% operands
type_e_constant_expr_expected=04052_E_Ekspresi Kontstan diharapkan
% The compiler expects an constant expression, but gets a variable expression.
type_e_operator_not_supported_for_types=04053_E_Operasi "$1" tidak didukung untuk tipe "$2" dan "$3"
% The operation is not allowed for the supplied types
type_e_illegal_type_conversion=04054_E_Konversi tipe tidak benar: "$1" ke "$2"
% When doing a type-cast, you must take care that the sizes of the variable and
% the destination type are the same.
type_h_pointer_to_longint_conv_not_portable=04055_H_Konversi antara ordinals dan pointers tidak mungkin
% If you typecast a pointer to a longint (or vice-versa), this code will not compile
% on a machine using 64-bit for pointer storage.
type_w_pointer_to_longint_conv_not_portable=04056_W_Konversi antara ordinals dan pointers bukan portabel
% If you typecast a pointer to a ordinal type of a different size (or vice-versa), this can
% cause problems. This is a warning to help finding the 32bit specific code where cardinal/longint is used
% to typecast pointers to ordinals. A solution is to use the ptrint/ptruint types instead.
type_e_cant_choose_overload_function=04057_E_Tidak bisa menentukan function yang di-overload mana untuk dipanggil 
% You're calling overloaded functions with a parameter that doesn't correspond
% to any of the declared function parameter lists. e.g. when you have declared
% a function with parameters \var{word} and \var{longint}, and then you call
% it with a parameter which is of type \var{integer}.
type_e_illegal_count_var=04058_E_Penghitung variabel tidak benar
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings.
type_w_double_c_varargs=04059_W_Pengubahan nilai konstan real ke double untuk argumen variabel C, tambahkan typecast eksplisit untuk mencegahnya.
% In C, constant real values are double by default. For this reason, if you
% pass a constant real value to a variable argument part of a C function, FPC
% by default converts this constant to double as well. If you want to prevent
% this from happening, add an explicit typecast around the constant.
type_e_class_or_cominterface_type_expected=04058_E_Antarmuka class atau type diharapkan, tetapi didapat "$1"
% Some operators like the AS operator are only appliable to classes or COM interfaces
% \end{description}
#
# Symtable
#
# 05060 is the last used one
#
% \section{Symbol handling}
% This section lists all the messages that concern the handling of symbols.
% This means all things that have to do with procedure and variable names.
% \begin{description}
sym_e_id_not_found=05000_E_Pengenal tidak ditemukan "$1"
% The compiler doesn't know this symbol. Usually happens when you misspel
% the name of a variable or procedure, or when you forgot to declare a
% variable.
sym_f_internal_error_in_symtablestack=05001_F_Kesalahan Internal dalam SymTableStack()
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide an exact description of
% the circumstances in which the error occurs.
sym_e_duplicate_id=05002_E_Duplikasi pengenal "$1"
% The identifier was already declared in the current scope.
sym_h_duplicate_id_where=05003_H_Pengenal sudah didefinisikan dalam $1 di baris $2
% The identifier was already declared in a previous scope.
sym_e_unknown_id=05004_E_Pengenal tidak dikenal "$1"
% The identifier encountered has not been declared, or is used outside the
% scope where it is defined.
sym_e_forward_not_resolved=05005_E_Deklarasi Forward tidak bisa dipecahkan "$1"
% This can happen in two cases:
% \begin{itemize}
% \item This happens when you declare a function (in the \var{interface} part, or
% with a \var{forward} directive, but do not implement it.
% \item You reference a type which isn't declared in the current \var{type}
% block.
% \end{itemize}
sym_e_error_in_type_def=05007_E_Kesalahan dalam definisi type
% There is an error in your definition of a new array type:
% \item One of the range delimiters in an array declaration is erroneous.
% For example, \var{Array [1..1.25]} will trigger this error.
sym_e_forward_type_not_resolved=05009_E_Tipe Forward tidak bisa dipecahkan "$1"
% A symbol was forward defined, but no declaration was encountered.
sym_e_only_static_in_static=05010_E_Hanya variabel statik bisa digunakan dalam metode statik atau metode outside
% A static method of an object can only access static variables.
sym_f_type_must_be_rec_or_class=05012_F_Tipe record atau class diharapkan
% The variable or expression isn't of the type \var{record} or \var{class}.
sym_e_no_instance_of_abstract_object=05013_E_Turunan dari classes atau objects dengan metode abstrak tidak dibolehkan
% You are trying to generate an instance of a class which has an abstract
% method that wasn't overridden.
sym_w_label_not_defined=05014_W_Label tidak didefinisikan "$1"
% A label was declared, but not defined.
sym_e_label_used_and_not_defined=05015_E_Label digunakan tapi tidak didefinisikan "$1"
% A label was declared and used, but not defined.
sym_e_ill_label_decl=05016_E_Deklarasi label tidak benar
% This error should never happen; it occurs if a label is defined outside a
% procedure or function.
sym_e_goto_and_label_not_supported=05017_E_GOTO dan LABEL tidak didukung (gunakan saklar -Sg)
% You must compile a program which has \var{label}s and \var{goto} statements
% with the \var{-Sg} switch. By default, \var{label} and \var{goto} aren't
% supported.
sym_e_label_not_found=05018_E_Label tidak ditemukan
% A \var{goto label} was encountered, but the label isn't declared.
sym_e_id_is_no_label_id=05019_E_pengenal bukan sebuah label
% The identifier specified after the \var{goto} isn't of type label.
sym_e_label_already_defined=05020_E_label sudah didefinisikan
% You are defining a label twice. You can define a label only once.
sym_e_ill_type_decl_set=05021_E_deklarasi tipe set elemen tidak benar
% The declaration of a set contains an invalid type definition.
sym_e_class_forward_not_resolved=05022_E_Definisi Forward class tidak dipecahkan "$1"
% You declared a class, but you did not implement it.
sym_n_unit_not_used=05023_H_Unit "$1" tidak digunakan dalam $2
% The unit referenced in the \var{uses} clause is not used.
sym_h_para_identifier_not_used=05024_H_Parameter "$1" tidak digunakan
% The identifier was declared (locally or globally) but
% was not used (locally or globally).
sym_n_local_identifier_not_used=05025_N_Variabel lokal "$1" tidak digunakan
% You have declared, but not used a variable in a procedure or function
% implementation.
sym_h_para_identifier_only_set=05026_H_Parameter nilai "$1" ditempati tapi tidak pernah digunakan
% The identifier was declared (locally or globally)
% set but not used (locally or globally).
sym_n_local_identifier_only_set=05027_N_Variabel lokal "$1" ditempati tapi tidak pernah digunakan
% The variable in a procedure or function
% implementation is declared, set but never used.
sym_h_local_symbol_not_used=05028_H_Lokal $1 "$2" tidak digunakan
% A local symbol is never used.
sym_n_private_identifier_not_used=05029_N_Field private "$1.$2" tidak pernah digunakan
sym_n_private_identifier_only_set=05030_N_Field private "$1.$2" ditempati tapi tidak pernah digunakan
sym_n_private_method_not_used=05031_N_Metode Private "$1.$2" tidak pernah digunakan
sym_e_set_expected=05032_E_Tipe Set diharapkan
% The variable or expression is not of type \var{set}. This happens in an
% \var{in} statement.
sym_w_function_result_not_set=05033_W_Hasil Function kelihatannya bukan set
% You can get this warning if the compiler thinks that a function return
% value is not set. This will not be displayed for assembler procedures,
% or procedures that contain assembler blocks.
sym_w_wrong_C_pack=05034_W_Type "$1" tidak dijajarkan dengan benar dalam record untuk C saat ini
% Arrays with sizes not multiples of 4 will be wrongly aligned
% for C structures.
sym_e_illegal_field=05035_E_Pengenal field record tidak dikenal "$1"
% The field doesn't exist in the record/object definition.
sym_w_uninitialized_local_variable=05036_W_Variabel lokal "$1" kelihatannya tidak diinisialisasi
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_w_uninitialized_variable=05037_W_Variabel "$1" kelihatanya tidak diinisialisasi
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_e_id_no_member=05038_E_pengenal idents bukan anggota "$1"
% This error is generated when an identifier of a record,
% field, or method is accessed while it is not defined.
sym_h_param_list=05039_H_Ditemukan deklarasi: $1
% You get this when you use the \var{-vh} switch. In case an overloaded
% procedure is not found, then all candidate overloaded procedures are
% listed, with their parameter lists.
sym_e_segment_too_large=05040_E_Elemen data terlalu besar
% You get this when you declare a data element whose size exceeds the
% prescribed limit (2 Gb on 80386+/68020+ processors)
sym_e_no_matching_implementation_found=05042_E_Tidak ada implementasi yang sama untuk metode interface "$1" yang ditemukan
% There was no matching method found which could implement the interface
% method. Check argument types and result type of the methods.
sym_w_deprecated_symbol=05043_W_Simbol "$1" tidak akan dipakai lagi nanti
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{deprecated} is used. Deprecated symbols may no longer
% be available in newer versions of the unit / library. Usage of this symbol
% should be avoided as much as possible.
sym_w_non_portable_symbol=05044_W_Simbol "$1" bukan portabel
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{platform} is used. This symbol's value, usage
% and availability is platform specific and should not be used
% if the source code must be portable.
sym_w_non_implemented_symbol=05055_W_Simbol "$1" tidak diimplementasikan
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{unimplemented} is used. This symbol is defined,
% but is not yet implemented on this specific platform.
sym_e_cant_create_unique_type=05056_E_Tidak bisa membuat tipe unik dari tipe ini
% Only simple types like ordinal, float and string types are supported when
% redefining a type with \var{type newtype = type oldtype;}.
sym_h_uninitialized_local_variable=05057_H_Variabel lokal "$1" tidak diinisialisasi
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_h_uninitialized_variable=05058_H_Variabel "$1" tidak diinisialisasi
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. appears in the right-hand-side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assigment)
sym_w_function_result_uninitialized=05059_W_Variabel hasil function tidak diinisialisasi
% This message is displayed if the compiler thinks that the function result
% variable will be used (i.e. appears in the right-hand-side of an expression)
% before it is initialized (i.e. appeared in the left-hand side of an
% assigment)
sym_h_function_result_uninitialized=05060_H_Variabel hasil Function tidak diinisialisasi
% This message is displayed if the compiler thinks that the function result
% variable will be used (i.e. appears in the right-hand-side of an expression)
% before it is initialized (i.e. appeared in the left-hand side of an
% assigment)
sym_w_identifier_only_read=05061_W_Variabel "$1" dibaca tapi tidak ada penempatan dimanapun
% You have read the value of a variable, but nowhere assigned a value to
% it.

% \end{description}
#
# Codegenerator
#
# 06040 is the last used one
#
% \section{Code generator messages}
% This section lists all messages that can be displayed if the code
% generator encounters an error condition.
% \begin{description}
cg_e_parasize_too_big=06009_E_Ukuran daftar parameter melebihi 65535 byte
% The I386 processor limits the parameter list to 65535 bytes (the \var{RET}
% instruction causes this)
cg_e_file_must_call_by_reference=06012_E_Tipe file harus parameter var
% You cannot specify files as value parameters, i.e. they must always be
% declared \var{var} parameters.
cg_e_cant_use_far_pointer_there=06013_E_Penggunaan far pointer tidak dibolehkan disini
% Free Pascal doesn't support far pointers, so you cannot take the address of
% an expression which has a far reference as a result. The \var{mem} construct
% has a far reference as a result, so the following code will produce this
% error:
% \begin{verbatim}
% var p : pointer;
% ...
% p:=@mem[a000:000];
% \end{verbatim}
cg_e_dont_call_exported_direct=06015_E_Fungsi EXPORT yang dideklarasikan tidak bisa dipanggil
% No longer in use.
cg_w_member_cd_call_from_method=06016_W_Kemungkinan pemanggilan tidak benar dari constructor atau destructor
% The compiler detected that a constructor or destructor is called within a
% a method. This will probably lead to problems, since constructors / destructors
% require parameters on entry.
cg_n_inefficient_code=06017_N_Kode tidak efisien
% Your statement seems dubious to the compiler.
cg_w_unreachable_code=06018_W_Kode tidak dapat dijangkau
% You specified a construct which will never be executed. Example:
% \begin{verbatim}
% while false do
%   begin
%   {.. code ...}
%   end;
% \end{verbatim}
cg_e_cant_call_abstract_method=06020_E_Metode abstrak tidak bisa dipanggil secara langsung
% You cannot call an abstract method directy, instead you must call a
% overriding child method, because an abstract method isn't implemented.
cg_d_register_weight=06027_DL_Register $1 bobot $2 $3
% Debugging message. Shown when the compiler considers a variable for
% keeping in the registers.
cg_d_stackframe_omited=06029_DL_Stack frame diabaikan
% Some procedure/functions do not need a complete stack-frame, so it is omitted.
% This message will be displayed when the {-vd} switch is used.
cg_e_unable_inline_object_methods=06031_E_Metode Object atau class tidak bisa inline.
% You cannot have inlined object methods.
cg_e_unable_inline_procvar=06032_E_Pemanggilan Procvar tidak bisas inline.
% A procedure with a procedural variable call cannot be inlined.
cg_e_no_code_for_inline_stored=06033_E_Tidak ada kode untuk inline procedure stored
% The compiler couldn't store code for the inline procedure.
cg_e_can_access_element_zero=06035_E_Elemen nol dari ansi/wide- atau longstring tidak bisa diakses, lebih baik gunakan (set)length
% You should use \var{setlength} to set the length of an ansi/wide/longstring
% and \var{length} to get the length of such a string types
cg_e_cannot_call_cons_dest_inside_with=06037_E_Constructors atau destructors tidak bisa dipanggil didalam ketentuan 'with'
% Inside a \var{with} clause you cannot call a constructor or destructor for the
% object you have in the \var{with} clause.
cg_e_cannot_call_message_direct=06038_E_Tidak bisa memanggil metode penendali pesan secara langsung
% A message method handler method cannot be called directly if it contains an
% explicit self argument
cg_e_goto_inout_of_exception_block=06039_E_Jump didalam atau diluar dari blok kekecualian
% tidak diijinkan untuk melompat kedalam atau keluar dari blok kekecualian seperti \var{try..finally..end;}:
% \begin{verbatim}
% label 1;
%
% ...
%
% try
%    if not(final) then
%      goto 1;   // baris ini akan menyebabkan kesalahan
% finally
%   ...
% end;
% 1:
% ...
% \end{verbatim}
cg_e_control_flow_outside_finally=06040_E_Pernyataan alur kontrol tidak dibolehkan dalam blok finally
% Tidak dibolehkan untuk menggunakan pernyataan alur kontrol \var{break},
% \var{continue} dan \var{exit}
% didalam sebuah pernyataan finally. Contoh berikut memperlihatkan persoalan:
% \begin{verbatim}
% ...
%   try
%      p;
%   finally
%      ...
%      exit;  // exit ini TIDAK dibolehkan
%   end;
% ...
%
% \end{verbatim}
% If the procedure \var{p} raises an exception the finally block is
% executed. If the execution reaches the exit, it's unclear what to do:
% exiting the procedure or searching for another exception handler
cg_w_parasize_too_big=06041_W_Ukuran parameters melebihi batas untuk cpu tertentu
% This indicates that you are declaring more than 64K of parameters, which
% might not be supported on other processor targets.
cg_w_localsize_too_big=06042_W_Ukuran variabel lokal melebihi batas untuk cpu tertentu
% This indicates that you are declaring more than 32K of local variables, which
% might not be supported on other processor targets.
cg_e_localsize_too_big=06043_E_Ukuran variabel lokal melebihi batas yang didukung
% This indicates that you are declaring more than 32K of local variables, which
% is not supported by this processor.
cg_e_break_not_allowed=06044_E_BREAK tidak dibolehkan
% You're trying to use \var{break} outside a loop construction.
cg_e_continue_not_allowed=06045_E_CONTINUE tidak dibolehkan
% You're trying to use \var{continue} outside a loop construction.
cg_f_unknown_compilerproc=06046_F_Compilerproc "$1" tidak dikenal. Periksa apakah anda menggunakan librari run time yang benar.
% The compiler expects that the runtime library contains some subrountines. If you see this error
% and you didn't mess with the runtime library, it's very likely that the runtime library
% you're using doesn't match the used compiler. If you changed the runtime library this error means
% that you removed a subroutine which the compiler needs for internal use.
% \end{description}
# EndOfTeX

#
# Assembler reader
#
# 07097 is the last used one
#
asmr_d_start_reading=07000_DL_Mulai $1 pemindaian gaya assembler
% Ini memberitahu anda bahwa blok assembler sedang dipindai.
asmr_d_finish_reading=07001_DL_Selesai $1 pemindaian gaya assembler
% Ini memberitahu anda bahwa blok assembler sudah selesai.
asmr_e_none_label_contain_at=07002_E_Pola Non-label berisi @
% A identifier which isn't a label can't contain a @.
asmr_e_building_record_offset=07004_E_Kesalahan pembuatan ofset record
% There has an error occured while building the offset of a record/object
% structure, this can happend when there is no field specified at all or
% an unknown field identifier is used.
asmr_e_offset_without_identifier=07005_E_OFFSET digunakan tanpa pengenal
% You can only use OFFSET with an identifier. Other syntaxes aren't
% supported
asmr_e_type_without_identifier=07006_E_TYPE digunakan tanpa pengenal
% You can only use TYPE with an identifier. Other syntaxes aren't
% supported
asmr_e_no_local_or_para_allowed=07007_E_Tidak bisa menggunakan variabel lokal atau parameter disini
% You can't use a local variable or parameter here, mostly because the
% addressing of locals and parameters is done using the frame pointer register so the
% address can't be obtained directly.
asmr_e_need_offset=07008_E_perlu menggunakan OFFSET disini
% You need to use OFFSET <id> here to get the address of the identifier.
asmr_e_need_dollar=07009_E_perlu menggunakan $ disini
% You need to use $<id> here to get the address of the identifier.
asmr_e_cant_have_multiple_relocatable_symbols=07010_E_Tidak bisa menggunakan simbol relokatabel multipel
% You can't have more than one relocatable symbol (variable/typed constant)
% in one argument.
asmr_e_only_add_relocatable_symbol=07011_E_Simbol relokatabel hanya bisa ditambahkan
% Relocatable symbols (variable/typed constant) can't be used with other
% operators. Only addition is allowed.
asmr_e_invalid_constant_expression=07012_E_Ekspresi konstan tidak benar
% There is an error in the constant expression.
asmr_e_relocatable_symbol_not_allowed=07013_E_Simbol relokatabel tidak dibolehkan
% You can't use a relocatable symbol (variable/typed constant) here.
asmr_e_invalid_reference_syntax=07014_E_Sintaks referensi tidak benar
% There is an error in the reference.
asmr_e_local_para_unreachable=07015_E_Anda tidak bisa mencapai $1 dari kode itu
% You can not read directly the value of a local variable or parameter
% of a higher level procedure in assembler code (except for
% local assembler code without parameter nor locals).
asmr_e_local_label_not_allowed_as_ref=07016_E_Simbol lokal/label tidak dibolehkan sebagai referensi
% You can't use local symbols/labels as references
asmr_e_wrong_base_index=07017_E_Penggunaan basis dan register indeks tidak benar
% There is an error with the base and index register, they are
% probably incorrect
asmr_w_possible_object_field_bug=07018_W_Kemungkinan kesalahan dalam penanganan field object
% Fields of objects or classes can be reached directly in normal or objfpc
% modes but TP and Delphi modes treat the field name as a simple offset.
asmr_e_wrong_scale_factor=07019_E_Faktor skala yang ditetapkan salah
% The scale factor given is wrong, only 1,2,4 and 8 are allowed
asmr_e_multiple_index=07020_E_Penggunaan register indeks multipel
% You are trying to use more than one index register
asmr_e_invalid_operand_type=07021_E_Tipe operan tidak benar
% The operand type doesn't match with the opcode used
asmr_e_invalid_string_as_opcode_operand=07022_E_String sebagai operan opcode tidak benar: $1
% The string specified as operand is not correct with this opcode
asmr_w_CODE_and_DATA_not_supported=07023_W_@CODE dan @DATA tidak didukung
% @CODE and @DATA are unsupported and are ignored.
asmr_e_null_label_ref_not_allowed=07024_E_Referensi label Null  tidak dibolehkan
asmr_e_expr_zero_divide=07025_E_Pembagian dengan nol dalam evaluator asm
% There is a division by zero in a constant expression
asmr_e_expr_illegal=07026_E_Ekspresi tidak benar
% There is an illegal expression in a constant expression
asmr_e_escape_seq_ignored=07027_E_escape sequence diabaikan: $1
% There is a C-styled string, but the escape sequence in the string
% is unknown, and is therefore ignored
asmr_e_invalid_symbol_ref=07028_E_Referensi simbol tidak benar
asmr_w_fwait_emu_prob=07029_W_Fwait bisa menyebabkan masalah emulasi dengan emu387
asmr_w_fadd_to_faddp=07030_W_$1 tanpa operan diterjemahkan kedalam $1P
asmr_w_enter_not_supported_by_linux=07031_W_Instruksi ENTER tidak didukung oleh Linux kernel
% ENTER instruction can generate a stack page fault that is not
% caught correctly by the i386 Linux page handler.
asmr_w_calling_overload_func=07032_W_Pemanggilan fungsi yang di-overload dalam assembler
% There is a call to an overloaded method in the assembler block,
% this might be the sign there is a problem
asmr_e_unsupported_symbol_type=07033_E_Tipe simbol untuk operan tidak didukung
asmr_e_constant_out_of_bounds=07034_E_Nilai konstan diluar batas
asmr_e_error_converting_decimal=07035_E_Kesalahan pengubahan desimal $1
% A constant decimal value does not have the correct syntax
asmr_e_error_converting_octal=07036_E_Kesalahan pengubahan oktal $1
% A constant octal value does not have the correct syntax
asmr_e_error_converting_binary=07037_E_Kesalahan pengubahan biner $1
% A constant binary value does not have the correct syntax
asmr_e_error_converting_hexadecimal=07038_E_Kesalahan pengubahan heksadesimal $1
% A constant hexadecimal value does not have the correct syntax
asmr_h_direct_global_to_mangled=07039_H_$1 diterjemahkan ke $2
asmr_w_direct_global_is_overloaded_func=07040_W_$1 dikaitkan ke fungsi yang di-overload
asmr_e_cannot_use_SELF_outside_a_method=07041_E_Tidak bisa menggunakan SELF diluar sebuah metode
% There is a reference to the \var{self} symbol while it is not
% allowed. \var{self} can only be referenced inside methods
asmr_e_cannot_use_OLDEBP_outside_nested_procedure=07042_E_Tidak bisa menggunakan OLDEBP diluar nested procedure
% There is a reference to the \var{oldebp} symbol while it is not
% allowed. \var{oldebp} can only be referenced inside nested routines
asmr_e_void_function=07043_W_Procedures tidak mengembalikan nilai apapaun dalam kode asm
% Trying to return a value while in a procedure. A procedure
% does not have any return value
asmr_e_SEG_not_supported=07044_E_SEG tidak didukung
asmr_e_size_suffix_and_dest_dont_match=07045_E_Ukuran suffix dan tujuan atau ukuran sumber tidak sama
% The register size and the opcode size suffix don't match. This is
% probably an error in the assembler statement
asmr_w_size_suffix_and_dest_dont_match=07046_W_Ukuran suffix dan tujuan atau ukuran sumber tidak sama
% The register size and the opcode size suffix don't match. This is
% probably an error in the assembler statement
asmr_e_syntax_error=07047_E_Kesalahan sintaks Assembler
% There is an assembler syntax error
asmr_e_invalid_opcode_and_operand=07048_E_Kombinasi dari opcode dan operands tidak benar
% The opcode cannot be used with this type of operand
asmr_e_syn_operand=07049_E_Sintaks Assembler salah dalam operand
asmr_e_syn_constant=07050_E_Kesalahan sintaks Assembler dalam konstan
asmr_e_invalid_string_expression=07051_E_Ekspresi String tidak benar
asmr_w_const32bit_for_address=07052_W_Konstan dengan simbol $1 untuk alamat dimana itu bukan pada pointer
% A constant expression represents an address which does not fit
% into a pointer. The address is probably incorrect
asmr_e_unknown_opcode=07053_E_Opcode tidak dikenal $1
% Opcode ini tidak diketahui
asmr_e_invalid_or_missing_opcode=07054_E_Tidak benar atau tidak ada opcode
asmr_e_invalid_prefix_and_opcode=07055_E_Kombinasi dari prefiks dan opcode tidak benar: $1
asmr_e_invalid_override_and_opcode=07056_E_Kombinasi dari override dan opcode tidak benar: $1
asmr_e_too_many_operands=07057_E_Terlalu banyak operan pada baris
% There are too many operands for this opcode. Check your
% assembler syntax
asmr_w_near_ignored=07058_W_NEAR diabaikan
asmr_w_far_ignored=07059_W_FAR diabaikan
asmr_e_dup_local_sym=07060_E_Duplikasi simbol lokal $1
asmr_e_unknown_local_sym=07061_E_Simbol lokal tidak ditetapkan $1
asmr_e_unknown_label_identifier=07062_E_Pengenal label tidak dikenal $1
asmr_e_invalid_register=07063_E_Nama register tidak benar
% There is an unknown register name used as operand.
asmr_e_invalid_fpu_register=07064_E_nama register floating point tidak benar
% There is an unknown register name used as operand.
asmr_w_modulo_not_supported=07066_W_Modulo tidak didukung
asmr_e_invalid_float_const=07067_E_Konstan floating point tidak benar $1
% The floating point constant declared in an assembler block is
% invalid.
asmr_e_invalid_float_expr=07068_E_Ekspresi floating point tidak benar
% The floating point expression declared in an assembler block is
% invalid.
asmr_e_wrong_sym_type=07069_E_Tipe simbol salah
asmr_e_cannot_index_relative_var=07070_E_Tidak bisa mengindeks lokal atau var atau parameter dengan a register
% Trying to index using a base register a symbol which is already relative
% to a register. This is not possible, and will probably lead to crashes.
asmr_e_invalid_seg_override=07071_E_Ekspresi override segmen tidak benar
asmr_w_id_supposed_external=07072_W_Pengenal $1 dianggap eksternal
% There is a reference to an undefined symbol. This will not result
% in an error, since the symbol might be external, but may cause
% problems at link time if the symbol is not defined anywhere.
asmr_e_string_not_allowed_as_const=07073_E_Strings tidak dibolehkan sebagai konstan
% Character strings are not allowed as constants.
asmr_e_no_var_type_specified=07074_Tidak ada tipe variabel yang ditetapkan
% The syntax expects a type idenfitifer after the dot, but
% none was found.
asmr_w_assembler_code_not_returned_to_text=07075_E_kode assembler tidak dikembalikan ke bagian text
% There was a directive in the assembler block to change sections,
% but there is a missing return to the text section at the end
% of the assembler block. This might cause errors during link time.
asmr_e_not_directive_or_local_symbol=07076_E_Bukan direktif atau simbol lokal $1
% This symbol is unknown.
asmr_w_using_defined_as_local=07077_E_Menggunakan nama yang didefinisiakan sebagai label lokal
asmr_e_dollar_without_identifier=07078_E_Token dolar digunakan tanpa pengenal
% A constant expression has an identifier which does not start with
% the $ symbol.
asmr_w_32bit_const_for_address=07079_W_32bit konstan dibuat untuk alamat
% A constant was used as an address. This is probably an error,
% since using absolute addresses will probably not work.
asmr_n_align_is_target_specific=07080_N_.align adalah target tertentu, gunakan .balign atau .p2align
% Using the .align directive is platform specific, and its meaning will vary
% from one platform to another.
asmr_e_cannot_access_field_directly_for_parameters=07081_E_Tidak bisa mengakses field secara langsung untuk parameter
% You should load the parameter first into a register and then access the
% fields using that register.
asmr_e_cannot_access_object_field_directly=07082_E_Tidak bisa mengakses fields dari objects/classes secara langsung
% You should load the self pointer first into a register and then access the
% fields using the register as base. By default the self pointer is available
% in the esi register on i386.
asmr_e_unable_to_determine_reference_size=07083_E_Tidak ada ukuran ditetapkan dan tidak bisa menentukan ukuran dari operan
% You should specify explicitly a size for the reference, because
% the compiler is unable to determine what size (byte,word,dword,etc.) it
% should use for the reference.
asmr_e_cannot_use_RESULT_here=07084_E_Tidak bisa menggunakan RESULT dalam fungsi ini
% Some functions which return complex types cannot use the \var{result}
% keyword.
asmr_w_adding_explicit_args_fXX=07086_W_"$1" tanpa operan diterjemahkan kedalaman "$1 %st,%st(1)"
asmr_w_adding_explicit_first_arg_fXX=07087_W_"$1 %st(n)" diterjemahkan kedalaman "$1 %st,%st(n)"
asmr_w_adding_explicit_second_arg_fXX=07088_W_"$1 %st(n)" diterjemahkan kedalaman "$1 %st(n),%st"
asmr_e_invalid_char_smaller=07089_E_Char < tidak dibolehkan disini
% The shift operator requires the << characters. Only one
% of those characters was found.
asmr_e_invalid_char_greater=07090_E_Char > tidak dibolehkan disini
% The shift operator requires the >> characters. Only one
% of those characters was found.
asmr_w_align_not_supported=07093_W_ALIGN tidak didukung
asmr_e_no_inc_and_dec_together=07094_E_Inc dan Dec tidak bisa bersama
% Trying to use an increment and a decrement within the same
% opcode on the 680x0. This is impossible.
asmr_e_invalid_reg_list_in_movem=07095_E_Reglist tidak benar untuk movem
% Trying to use the \var{movem} opcode with invalid registers
% to save or restore.
asmr_e_invalid_reg_list_for_opcode=07096_E_Reglist tidak benar untuk opcode
asmr_e_higher_cpu_mode_required=07097_E_Mode cpu lebih tinggi dibutuhkan ($1)
% Trying to use an instruction which is not supported in the current
% cpu mode. Use a higher cpu generation to be able to use this
% opcode in your assembler block
asmr_w_unable_to_determine_reference_size_using_dword=07098_W_Tidak ada ukuran ditetapkan dan tidak bisa menentukan ukuran dari operan, menggunakan DWORD sebagai default
% You should specify explicitly a size for the reference, because
% the compiler is unable to determine what size (byte,word,dword,etc.) it
% should use for the reference. This warning is only used in Delphi mode where
% it falls back to use DWORD as default.
asmr_e_illegal_shifterop_syntax=07099_E_Kesalahan sintaks ketika mencoba memindai operan shifter
% ARM only; ARM assembler supports a so called shifter operand. The used syntax isn't
% a valid shifter operand. Example for an operation with shifter operand:
% \begin{verbatim}
% asm
%   orr     r2,r2,r2,lsl #8
% end;
% \end{verbatim}
#
# Assembler/binary writers
#
# 08018 is the last used one
#
asmw_f_too_many_asm_files=08000_F_Terlalu banyak file assembler
% With smartlinking enabled, there are too many assembler
% files generated. Disable smartlinking.
asmw_f_assembler_output_not_supported=08001_F_Output Assembler yang dipilih tidak didukung
asmw_f_comp_not_supported=08002_F_Comp tidak didukung
asmw_f_direct_not_supported=08003_F_Direct tidak didukung untuk penulis biner
% Direct assembler mode is not supported for binary writers.
asmw_e_alloc_data_only_in_bss=08004_E_Alokasi data hanya dibolehkan dalam seksi bss
asmw_f_no_binary_writer_selected=08005_F_Tidak ada penulis biner yang dipililh
asmw_e_opcode_not_in_table=08006_E_Asm: Opcode $1 tidak dalam tabel
asmw_e_invalid_opcode_and_operands=08007_E_Asm: kombinasi $1 tidak benar dari opcode dan operands
asmw_e_16bit_not_supported=08008_E_Asm: rujukan 16 Bit tidak didukung
asmw_e_invalid_effective_address=08009_E_Asm: alamat efektif tidak benar
asmw_e_immediate_or_reference_expected=08010_E_Asm: Langsung atau rujukan diharapkan
asmw_e_value_exceeds_bounds=08011_E_Asm: $1 nilai melebihi batas $2
asmw_e_short_jmp_out_of_range=08012_E_Asm: Lompat pendek diluar jangkauan $1
asmw_e_undefined_label=08013_E_Asm: label tidak didefinisikan $1
asmw_e_comp_not_supported=08014_E_Asm: Tipe Comp tidak didukung untuk target ini
asmw_e_extended_not_supported=08015_E_Asm: Tipe ekstended tidak didukung untuk target ini
asmw_e_duplicate_label=08016_E_Asm: Duplikasi label $1
asmw_e_redefined_label=08017_E_Asm: label $1 didefinisikan ulang
asmw_e_first_defined_label=08018_E_Asm: Definisi pertama disini
asmw_e_invalid_register=08019_E_Asm: register $1 tidak benar
asmw_e_16bit_32bit_not_supported=08020_E_Asm: rujukan 16 atau 32 Bit tidak didukung
asmw_e_64bit_not_supported=08021_E_Asm: operan 64 Bit tidak didukung

#
# Executing linker/assembler
#
# 09034 is the last used one
#
# BeginOfTeX
%
% \section{Errors of assembling/linking stage}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
exec_w_source_os_redefined=09000_W_Sumber sistem operasi dideklarasikan ulang
exec_i_assembling_pipe=09001_I_Merakit (pipe) $1
exec_d_cant_create_asmfile=09002_E_Tidak bisa membuat file assembler: $1
% The mentioned file can't be created. Check if you have got
% access permissions to create this file
exec_e_cant_create_objectfile=09003_E_Tidak bisa membuat file obyek: $1
% The mentioned file can't be created. Check if you've
% got access permissions to create this file
exec_e_cant_create_archivefile=09004_E_Tidak bisa membuat file arsip: $1
% The mentioned file can't be created. Check if you've
% access permissions to create this file
exec_e_assembler_not_found=09005_E_Assembler $1 tidak ditemukan, beralih ke perakitan eksternal
exec_t_using_assembler=09006_T_Menggunakan assembler: $1
exec_e_error_while_assembling=09007_E_Kesalahan ketika merakit exitcode $1
% There was an error while assembling the file using an external assembler.
% Consult the documentation of the assembler tool to find out more information
% on this error.
exec_e_cant_call_assembler=09008_E_Tidak bisa memanggil assembler, kesalahan $1 beralih ke perakitan eksternal
exec_i_assembling=09009_I_Perakitan $1
exec_i_assembling_smart=09010_I_Perakitan dengan smartlinking $1
exec_w_objfile_not_found=09011_W_Obyek $1 tidak ditemukan, Penggabungan bisa gagal !
% One of the object file is missing, and linking will probably fail.
% Check your paths.
exec_w_libfile_not_found=09012_W_Librari $1 tidak ditemukan, Penggabungan bisa gagal !
% One of the library file is missing, and linking will probably fail.
% Check your paths.
exec_e_error_while_linking=09013_E_Kesalahan saat penggabungan
% Generic error while linking.
exec_e_cant_call_linker=09014_E_Tidak bisa memanggil linker, beralih ke penggabungan eksternal
exec_i_linking=09015_I_Penggabungan $1
exec_e_util_not_found=09016_E_Util $1 tidak ditemukan, beralih ke penggabungan eksternal
exec_t_using_util=09017_T_Menggunakan util $1
exec_e_exe_not_supported=09018_E_Pembuatan Eksekutable tidak didukung
exec_e_dll_not_supported=09019_E_Pembuatan librari Dinamis/Berbagi tidak didukung
exec_i_closing_script=09020_I_Penutupan skrip $1
exec_e_res_not_found=09021_E_resource kompilator tidak ditemukan, beralih ke mode eksternal
exec_i_compilingresource=09022_I_Mengkompilasi resource $1
exec_t_unit_not_static_linkable_switch_to_smart=09023_T_unit $1 todal bisa digabung secara statis, beralih ke penggabungan pintar
exec_t_unit_not_smart_linkable_switch_to_static=09024_T_unit $1 tidak bisa digabung secara pintar, beralih ke penggabungan statis
exec_t_unit_not_shared_linkable_switch_to_static=09025_T_unit $1 tidak bisa digabung berbagi, beralih ke penggabungan statik
exec_e_unit_not_smart_or_static_linkable=09026_E_unit $1 tidak bisa digabungkan secara pintar atau statik
exec_e_unit_not_shared_or_static_linkable=09027_E_unit $1 tidak bisa digabung berbagi atau statik
exec_d_resbin_params=09028_D_Pemanggilan kompilator resource "$1" dengan "$2" sebagai baris perintah
%\end{description}
# EndOfTeX

#
# Executable information
#
execinfo_f_cant_process_executable=09128_F_Tidak bisa menuliskan proses eksekutabel $1
execinfo_f_cant_open_executable=09129_F_Tidak bisa membuka eksekutabel$1
execinfo_x_codesize=09130_X_Besar Kode: $1 byte
execinfo_x_initdatasize=09131_X_Besar data yang diinisialisasi: $1 byte
execinfo_x_uninitdatasize=09132_X_Besar data yang tidak diinisialisasi: $1 byte
execinfo_x_stackreserve=09133_X_Ruang stack disediakan: $1 byte
execinfo_x_stackcommit=09134_X_Ruang stack dipakai: $1 byte

#
# Pengambilan unit
#
# 10041 is the last used one
#
# BeginOfTeX
% \section{Pesan pengambilan unit.}
% This section lists all messages that can occur when the compiler is
% loading a unit from disk into memory. Many of these messages are
% informational messages.
% \begin{description}
unit_t_unitsearch=10000_T_Pencarian unit: $1
% When you use the \var{-vt}, the compiler tells you where it tries to find
% unit files.
unit_t_ppu_loading=10001_T_Pemanggilan PPU $1
% When the \var{-vt} switch is used, the compiler tells you
% what units it loads.
unit_u_ppu_name=10002_U_Nama PPU: $1
% When you use the \var{-vu} flag, the unit name is shown.
unit_u_ppu_flags=10003_U_Tanda PPU: $1
% When you use the \var{-vu} flag, the unit flags are shown.
unit_u_ppu_crc=10004_U_Crc PPU: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_time=10005_U_Waktu PPU: $1
% When you use the \var{-vu} flag, the time the unit was compiled is shown.
unit_u_ppu_file_too_short=10006_U_File PPU terlalu pendek
% The ppufile is too short, not all declarations are present.
unit_u_ppu_invalid_header=10007_U_Header PPU Tidak benar (tidak ada PPU pada begin)
% A unit file contains as the first three bytes the ascii codes of \var{PPU}
unit_u_ppu_invalid_version=10008_U_Versi PPU Tidak benar $1
% This unit file was compiled with a different version of the compiler, and
% cannot be read.
unit_u_ppu_invalid_processor=10009_U_PPU dikompilasi untuk prosesor yang lain
% This unit file was compiled for a different processor type, and
% cannot be read
unit_u_ppu_invalid_target=10010_U_PPU dikompilasi untuk target yang lain
% This unit file was compiled for a different target, and
% cannot be read
unit_u_ppu_source=10011_U_Sumber PPU: $1
% When you use the \var{-vu} flag, the unit source file name is shown.
unit_u_ppu_write=10012_U_Penulisan $1
% When you specify the \var{-vu} switch, the compiler will tell you where it
% writes the unit file.
unit_f_ppu_cannot_write=10013_F_Tidak bisa Menulis File PPU
% An error occurred when writing the unit file.
unit_f_ppu_read_error=10014_F_Pembacaan File PPU salah
% This means that the unit file was corrupted, and contains invalid
% information. Recompilation will be necessary.
unit_f_ppu_read_unexpected_end=10015_F_Akhir File-PPU tidak diharapkan
% Unexpected end of file. This may mean that the PPU file is
% corrupted.
unit_f_ppu_invalid_entry=10016_F_Entri File-PPU tidak benar: $1
% The unit the compiler is trying to read is corrupted, or generated with a
% newer version of the compiler.
unit_f_ppu_dbx_count_problem=10017_F_penghitungan PPU Dbx bermasalah
% There is an inconsistency in the debugging information of the unit.
unit_e_illegal_unit_name=10018_E_Nama unit tidak benar: $1
% The name of the unit does not match the file name.
unit_f_too_much_units=10019_F_Terlalu banyak unit
% \fpc has a limit of 1024 units in a program. You can change this behavior
% by changing the \var{maxunits} constant in the \file{fmodule.pas} file of the
% compiler, and recompiling the compiler.
unit_f_circular_unit_reference=10020_F_Rujukan unit berputar antara $1 dan $2
% Two units are using each other in the interface part. This is only allowed
% in the \var{implementation} part. At least one unit must contain the other one
% in the \var{implementation} section.
unit_f_cant_compile_unit=10021_F_Tidak bisa mengkompilasi $1, sumber tidak tersedia
% A unit was found that needs to be recompiled, but no sources are
% available.
unit_f_cant_find_ppu=10022_F_Tidak bisa menemukan unit $1
% You tried to use a unit of which the PPU file isn't found by the
% compiler. Check your configuration file for the unit paths
unit_w_unit_name_error=10023_W_Unit $1 tidak ditemukan tapi $2 ada
unit_f_unit_name_error=10024_F_Unit $1 mencari tapi $2 ditemukan
% Dos truncation of 8 letters for unit PPU files
% may lead to problems when unit name is longer than 8 letters.
unit_w_switch_us_missed=10025_W_Kompilkasi unit sistem memerlukan saklar -Us
% When recompiling the system unit (it needs special treatment), the
% \var{-Us} must be specified.
unit_f_errors_in_unit=10026_F_Ada $1 kesalahan kompilasi modul, menghentikan
% When the compiler encounters a fatal error or too many errors in a module
% then it stops with this message.
unit_u_load_unit=10027_U_Diambil dari $1 ($2) unit $3
% When you use the \var{-vu} flag, which unit is loaded from which unit is
% shown.
unit_u_recompile_crc_change=10028_U_Kompilasi ulang $1, checksum berubah untuk $2
unit_u_recompile_source_found_alone=10029_U_Rekompilasi ulang $1, hanya sumber yang ditemukan
% When you use the \var{-vu} flag, these messages tell you why the current
% unit is recompiled.
unit_u_recompile_staticlib_is_older=10030_U_Rekompilasi ulang unit, statik lib lebih lama dari ppufile
% When you use the \var{-vu} flag, the compiler warns if the static library
% of the unit are older than the unit file itself.
unit_u_recompile_sharedlib_is_older=10031_U_Rekompilasi ulang unit, shared lib lebih lama dari ppufile
% When you use the \var{-vu} flag, the compiler warns if the shared library
% of the unit are older than the unit file itself.
unit_u_recompile_obj_and_asm_older=10032_U_Rekompilasi ulang unit, obj dan asm lebih lama dari ppufile
% When you use the \var{-vu} flag, the compiler warns if the assembler or
% object file of the unit are older than the unit file itself.
unit_u_recompile_obj_older_than_asm=10033_U_Rekompilasi ulang unit, obj lebih lama dari asm
% When you use the \var{-vu} flag, the compiler warns if the assembler
% file of the unit is older than the object file of the unit.
unit_u_parsing_interface=10034_U_Pemindaian antar muka dari $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the interface part of the unit
unit_u_parsing_implementation=10035_U_Pemindaian implementasi dari $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the implementation part of the unit
unit_u_second_load_unit=10036_U_pengambilan kedua untuk unit $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% recompiling a unit for the second time. This can happend with interdepend
% units.
unit_u_check_time=10037_U_PPU memeriksa file $1 waktu $2
% When you use the \var{-vu} flag, the compiler show the filename and
% date and time of the file which a recompile depends on
### The following two error msgs is currently disabled.
#unit_h_cond_not_set_in_last_compile=10038_H_Kondisional $1 tidak di-set saat startup dalam kompilasi terakhir dari $2
#% when recompilation of an unit is required the compiler will check that
#% the same conditionals are set for the recompiliation. The compiler has
#% found a conditional that currently is defined, but was not used the last
#% time the unit was compiled.
#unit_h_cond_set_in_last_compile=10039_H_Kondisional $1 tidak di-set saat startup dalam kompilasi terakhir dari $2
#% when recompilation of an unit is required the compiler will check that
#% the same conditionals are set for the recompiliation. The compiler has
#% found a conditional that was used the last time the unit was compiled, but
#% the conditional is currently not defined.
unit_w_cant_compile_unit_with_changed_incfile=10040_W_tidak bisa mengkompilasi ulang unit $1, tetapi ditemukan file include yang diubah
% A unit was found to have modified include files, but
% some source files were not found, so recompilation is impossible.
unit_h_source_modified=10041_H_File $1 is lebih baru dari file Rilis PPU $2
% A modified source file for a unit was found that was compiled with the
% release flag (-Ur). The unit will not implicitly be recompiled because
% this release flag is set.
unit_u_ppu_invalid_fpumode=10042_U_Menggunakan unit yang tidak dikompilasi dengan mode FPU yang benar
% Trying to compile code while using units which were not compiled with
% the same floating point format mode. Either all code should be compiled
% with FPU emulation on, or with FPU emulation off.
unit_u_loading_interface_units=10043_U_Pemanggilan antar muka unit dari $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% loading the units defined in the interface part of the unit.
unit_u_loading_implementation_units=10044_U_Pengambilan unit implementasi dari $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% loading the units defined in the implementation part of the unit.
unit_u_interface_crc_changed=10045_U_CRC antarmuka berubah untuk unit $1
% When you use the \var{-vu} flag, the compiler warns that it the
% CRC calculated for the interface has been changed after the implementation
% has been parsed.
unit_u_implementation_crc_changed=10046_U_CRC implementasi berubah untuk unit $1
% When you use the \var{-vu} flag, the compiler warns that it the
% CRC calculated has been changed after the implementation
% has been parsed.
unit_u_finished_compiling=10047_U_Selesai mengkompilasi unit $1
% When you use the \var{-vu} flag, the compiler warns that it
% has finished compiling the unit.
unit_u_add_depend_to=10048_U_Menambahkan dependensi $1 ke $2
% When you use the \var{-vu} flag, the compiler warns that it
% has added a dependency between the two units.
unit_u_no_reload_is_caller=10049_U_Tidak diambil lagi, adalah pemanggil: $1
% When you use the \var{-vu} flag, the compiler warns that it
% has will not reload the unit because it is the unit that wants
% to load this unit
unit_u_no_reload_in_second_compile=10050_U_Tidak diambil lagi, sudah dalam kompilasi kedua: $1
% When you use the \var{-vu} flag, the compiler warns that it
% has will not reload the unit because it is already in a second recompile
unit_u_flag_for_reload=10051_U_Tanda untuk mengambil lagi: $1
% When you use the \var{-vu} flag, the compiler warns that it
% has to reload the unit
unit_u_forced_reload=10052_U_Pengambilan dipaksa
% When you use the \var{-vu} flag, the compiler warns that it
% has is reloading the unit because it was required
unit_u_previous_state=10053_U_Keadaan sebelumnya dari $1: $2
% When you use the \var{-vu} flag, the compiler shows the
% previous state of the unit
unit_u_second_compile_unit=10054_U_Sudah mengkompilasi $1, menyeting kompilasi kedua
% When you use the \var{-vu} flag, the compiler warns that it starts
% recompiling a unit for the second time. This can happend with interdepend
% units.
unit_u_loading_unit=10055_U_Pengambilan unit $1
% When you use the \var{-vu} flag, the compiler warns that it starts
% loading the unit.
unit_u_finished_loading_unit=10056_U_Selesai mengambil unit $1
% When you use the \var{-vu} flag, the compiler warns that it finished
% loading the unit.
unit_u_registering_new_unit=10057_U_Mendaftarkan unit baru $1
% When you use the \var{-vu} flag, the compiler warns that it has
% found a new unit and registers it in the internal lists.
unit_u_reresolving_unit=10058_U_Re-resolving unit $1
% When you use the \var{-vu} flag, the compiler warns that it
% has to recalculate the internal data of the unit
unit_u_skipping_reresolving_unit=10059_U_Melewati re-resolving unit $1, masih mengmbil unit yang digunakan
% When you use the \var{-vu} flag, the compiler warns that it
% skips to recalculate the internal data of the unit because there
% is no data to recalculate
% \end{description}
# EndOfTeX

#
#  Options
#
# 11039 is the last used one
#
option_usage=11000_O_$1 [opsi] <fileinput> [opsi]
# BeginOfTeX
%
% \section{Command-line handling errors}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
option_only_one_source_support=11001_W_Hanya file sumber yang didukung
% You can specify only one source file on the command line. The first
% one will be compiled, others will be ignored. This may indicate that
% you forgot a \var{'-'} sign.
option_def_only_for_os2=11002_W_File DEF hanya bisa dibuat untuk OS/2
% This option can only be specified when you're compiling for OS/2
option_no_nested_response_file=11003_E_file respon berulang tidak didukung
% you cannot nest response files with the \var{@file} command-line option.
option_no_source_found=11004_F_Tidak ada nama file dalam baris perintah
% The compiler expects a source file name on the command line.
option_no_option_found=11005_N_Tidak ada opsi di dalam file konfigurasi $1
% The compiler didn't find any option in that config file.
option_illegal_para=11006_E_Parameter tidak benar: $1
% You specified an unknown option.
option_help_pages_para=11007_H_-? menulis halaman panduan
% When an unknown option is given, this message is diplayed.
option_too_many_cfg_files=11008_F_Terlalu banyak file konfigurasi berulang
% You can only nest up to 16 config files.
option_unable_open_file=11009_F_Tidak bisa membukan file $1
% The option file cannot be found.
option_reading_further_from=11010_D_Pembacaan opsi selanjutnya dari $1
% Displayed when you have notes turned on, and the compiler switches
% to another options file.
option_target_is_already_set=11011_W_Target sudah diset ke: $1
% Displayed if more than one \var{-T} option is specified.
option_no_shared_lib_under_dos=11012_W_Librari berbagi tidak didukung pada platform DOS, kembali ke statik
% If you specify \var{-CD} for the \dos platform, this message is displayed.
% The compiler supports only static libraries under \dos
option_too_many_ifdef=11013_F_terlalu banyak IF(N)DEFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_many_endif=11014_F_terlalu banyak ENDIFs
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_less_endif=11015_F_buka kondisional di akhir file
% the \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_no_debug_support=11016_W_Pembuatan informasi Debug tidak didukung oleh eksekutabel ini
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_no_debug_support_recompile_fpc=11017_H_Coba rekompilasi dengan -dGDB
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_obsolete_switch=11018_W_Anda menggunakan saklar tidak terpakai $1
% this warns you when you use a switch that is not needed/supported anymore.
% It is recommended that you remove the switch to overcome problems in the
% future, when the switch meaning may change.
option_obsolete_switch_use_new=11019_W_Anda menggunakan saklar $1 tidak terpakai, silahkan gunakan $2
% this warns you when you use a switch that is not supported anymore. You
% must now use the second switch instead.
% It is recommended that you change the switch to overcome problems in the
% future, when the switch meaning may change.
option_switch_bin_to_src_assembler=11020_N_Mengembalikan assembler ke penulisan sumber assembler default
% this notifies you that the assembler has been changed because you used the
% -a switch which can't be used with a binary assembler writer.
option_incompatible_asm=11021_W_Output assembler dipilih "$1" tidak kompatibel dengan "$2"
option_asm_forced=11022_W_"$1" assembler menggunakan paksaan
% The assembler output selected can not generate
% object files with the correct format. Therefore, the
% default assembler for this target is used instead.
option_using_file=11026_T_Opsi pembacaan dari file $1
% Options are also read from this file
option_using_env=11027_T_Opsi pembacaan dari lingkungan $1
% Options are also read from this environment string
option_handling_option=11028_D_Opsi penanganan "$1"
% Debug info that an option is found and will be handled
option_help_press_enter=11029__*** tekan enter ***
option_start_reading_configfile=11030_H_Mulai pembacaan file konfigurasi $1
% Starting of config file parsing.
option_end_reading_configfile=11031_H_Akhir dari pembacaan file konfigurasi $1
% End of config file parsing.
option_interpreting_option=11032_D_menterjemahkan opsi "$1"
option_interpreting_firstpass_option=11036_D_menterjemahkan opsi firstpass "$1"
option_interpreting_file_option=11033_D_menterjemahkan opsi file "$1"
option_read_config_file=11034_D_Pembacaan file konfigurasi "$1"
option_found_file=11035_D_ditemukan nama file sumber "$1"
% Additional infos about options, displayed
% when you have debug option turned on.
option_code_page_not_available=11039_E_Halaman kode tidak dikenal
option_config_is_dir=11040_F_File konfigurasi $1 adalah sebuah direktori
% Directories can not be used as configuration files.
%\end{description}
# EndOfTeX

#
# Logo (opsi -l)
#
option_logo=11023_[
Free Pascal Compiler versi $FPCFULLVERSION [$FPCDATE] untuk $FPCCPU
Hak Cipta (c) 1993-2006 oleh Florian Klaempfl
]

#
# Info (opsi -i)
#
option_info=11024_[
Free Pascal Compiler versi $FPCVERSION

Tanggal Kompilator   : $FPCDATE
Target CPU Kompilator: $FPCCPU

Target yang Didukung:
  $OSTARGETS

Set instruksi CPU yang Didukung:
  $INSTRUCTIONSETS

Set instruksi FPU yang Didukung:
  $FPUINSTRUCTIONSETS

Optimasi yang Didukung:
  $OPTIMIZATIONS

Program ini berdasarkan GNU General Public Licence
Untuk informasi lebih jauh baca COPYING.FPC

Laporkan kesalahan, saran dll ke:
                 bugs@freepascal.org
]

#
# Halaman panduan (opsi -? dan -h)
#
# Karakter pertama pada baris menunjukan siapa yang akan menampilkan baris ini,
# kemungkinan saat ini adalah :
#    * = setiap target
#    3 = 80x86 targets
#    6 = 680x0 targets
#    e = hanya dalam mode debug extended
#    P = PowerPC targets
#    S = Sparc targets
#    V = Virtual machine targets
# Karakter kedua juga menunjukan siapa yang akan menampilkan baris ini.
# (jika karakter diatas adalah TRUE) kemungkinan ini adalah :
#    * = setiap orang
#    g = dengan info GDB didukung oleh kompilator
#    O = OS/2
#    L = UNIX systems
#  Karakter ketiga menggambarkan tingkat lekukan.
#
option_help_pages=11025_[
**0*_sisipkan + setelah opsi saklar boolean untuk menghidupkannya, - untuk mematikan
**1a_kompilator tidak menghapus file assembler yang dihasilkan
**2al_daftarkan baris kode sumber dalam file assembler
**2an_daftarkan info node dalam file assembler
*L2ap_gunakan pipe daripada pembuatan file-file assembler temporal
**2ar_daftarkan info alokasi/pelepasan register dalam file assembler
**2at_daftarkan info alokasi/pelepasan temp dalam file assembler
**1A<x>_format output:
**2Adefault_gunakan assembler default
3*2Aas_himpun menggunakan GNU AS
3*2Anasmcoff_file coff (Go32v2) menggunakan Nasm
3*2Anasmelf_file elf32 (Linux) menggunakan Nasm
3*2Anasmwin32_file obyek Win32 menggunakan Nasm
3*2Anasmwdosx_file Win32/WDOSX menggunakan Nasm
3*2Awasm_file obj menggunakan Wasm (Watcom)
3*2Anasmobj_file obj menggunakan Nasm
3*2Amasm_file obj menggunakan Masm (Microsoft)
3*2Atasm_file obj menggunakan Tasm (Borland)
3*2Aelf_elf32 (Linux) menggunakan penulis internall
3*2Acoff_coff (Go32v2) menggunakan penulis internall
3*2Apecoff_pecoff (Win32) menggunakan penulis internall
4*2Aas_himpun menggunakan GNU AS
6*2Aas_Unix o-file menggunakan GNU AS
6*2Agas_GNU Motorola assembler
6*2Amit_Sintaks MIT (GAS lama)
6*2Amot_Standar Motorola assembler
A*2Aas_himpun menggunakan GNU AS
P*2Aas_himpun menggunakan GNU AS
S*2Aas_himpun menggunakan GNU AS
**1b_hasilkan info browser
**2bl_hasilkan info simbol lokal
**1B_bangun semua modul
**1C<x>_opsi pembuatan kode:
**2Cc<x>_set konvensi pemanggilan default ke <x>
**2CD_buat juga librari dinamis (tidak didukung)
**2Ce_Kompiilasi dengan opkode emulasi floating point
**2Cf<x>_Pilih set instruksi fpu yang digunakan, lihat fpc -i untuk nilai yang mungkin
**2Cg_Hasilkan kode PIC
**2Ch<n>_<n> byte heap (antara 1023 dan 67107840)
**2Ci_IO-pemeriksaan
**2Cn_abaikan tahap linking
**2Co_periksa overflow dari operasi integer
**2Cp<x>_pilih set instruksi, lihat fpc -i untuk nilai yang mungkin
**2Cr_pemeriksaan jangkauan
**2CR_verifikasi keabsahan pemanggilan metode object
**2Cs<n>_set besar stack ke <n>
**2Ct_pemeriksaan stack
**2CX_buat juga librari smartlinked
**1d<x>_definisikan simbol <x>
**1D_hasilkan file DEF
**2Dd<x>_set deskripsi ke <x>
**2Dv<x>_set versi DLL ke <x>
*O2Dw_Aplikasi PM
**1e<x>_set path ke eksekutabel
**1E_sama dengan -Cn
**1F<x>_set nama file dan path:
**2Fa<x>[,y]_untuk sebuag program mengambil unit pertama <x> dan [y] sebelum penggunaan dioper
**2Fc<x>_set codepage input ke <x>
**2FD<x>_set direktori dimana untuk mencari utilitas kompilator
**2Fe<x>_alihkan output kesalahan ke <x>
**2FE<x>_set path output exe/unit ke <x>
**2Fi<x>_tambahkan <x> ke path include
**2Fl<x>_tambahkan <x> ke path librari
**2FL<x>_gunakan <x> sebagai linker dinamis
**2Fo<x>_tambahkan <x> ke path object
**2Fr<x>_ambil file pesan kesalahan <x>
**2Fu<x>_tambahkan <x> ke path unit
**2FU<x>_set path output unit ke <x>, kesampingkan -FE
*g1g_hasilkan informasi debugger:
*g2gc_hasilkan pemeriksaan untuk pointer
*g2gd_gunakan dbx
*g2gg_gunakan gsym
*g2gh_gunakan unit penelusur heap (untuk debug kebocoran memori)
*g2gl_gunakan unit info baris untuk menampilkan info lebih banyak untuk penelusuran
*g2gv_hasilkan program yang bisa ditelusuri dengan valgrind
*g2gw_hasilkan info debug dwarf
**1i_informasi
**2iD_mengembalikan tanggal kompilator
**2iV_mengembalikan versi kompilator
**2iSO_mengembalikan OS kompilator
**2iSP_mengembalikan prosesor kompilator
**2iTO_mengembalikan OS target
**2iTP_mengembalikan prosesor target
**1I<x>_tambahkan <x> ke path include
**1k<x>_Operan <x> ke linker
**1l_tulis logo
**1M<x>_set mode bahasa ke <x>
**2Mfpc_dialek free pascal (default)
**2Mobjfpc_beberapa saklar ekstensi Delphi 2 hidup
**2Mdelphi_mencoba untuk kompatibel Delphi
**2Mtp_mencoba untuk kompatibel TP/BP 7.0
**2Mgpc_mencoba untuk kompatibel gpc
**2Mmacpas_mencoba untuk kompatibel dengan dialek pascal macintosh
**1n_jangan baca file konfigurasi default
**1N<x>_optimasi susunan node
**2Nu_jangan gulung perulangan
**1o<x>_ubah nama dari eksekutabel yang dihasilkan ke <x>
**1O<x>_optimasi:
**2O-_matikan optimasi
**2O1_tingkat 1 optimasi (cepat dan akrab debugger)
**2O2_tingkat 2 optimasi (-O1 + optimasi cepat)
**2O3_tingkat 3 optimasi (-O2 + optimasi lambat)
**2Oa<x>=<y>_set penjajaran
**2Oo[NO]<x>_hidupkan atau maukan optimasi, lihat fpc -i untuk nilai yang mungkin
**2Op<x>_set target cpu untuk optimasi, lihat fpc -i untuk nilai yang mungkin
**2Os_hasilkan kode lebih kecil
**1pg_hasilkan kode profil untuk gprof (definisikan FPC_PROFILE)
**1R<x>_gaya pembacaan assembler:
**2Rdefault_gunakan assembler default
3*2Ratt_baca gaya assembler AT&T
3*2Rintel_baca gaya assembler intel
6*2RMOT_baca gaya assembler motorola
**1S<x>_opsi sintaks:
**2S2_sama dengan -Mobjfpc
**2Sc_dukung operator mirip C (*=,+=,/= and -=)
**2Sa_sertakan kode asertasi.
**2Sd_sama dengan\ -Mdelphi
**2Se<x>_opsi kesalahan. <x> adalah kombinasi dari:
**3*_<n> : kompilator berhenti setelah <n> kesalahan (default adalah 1)
**3*_w : kompilator berhenti juga setelah peringatan
**3*_n : kompilator berhenti juga setelah catatan
**3*_h : kompilator berhenti juga setelah petunjuk
**2Sg_ijinkan LABEL dan GOTO
**2Sh_Gunakan ansistrings
**2Si_dukung gaya C++ INLINE
**2Sk_ambil unit fpcylix
**2SI<x>_set gaya antar muka ke <x>
**3SIcom_Antarmuka kompatibel COM (default)
**3SIcorba_Antarmuka kompatibel CORBA
**2Sm_dukung makro seperti C (global)
**2So_sama dengan -Mtp
**2Sp_sama dengan -Mgpc
**2Ss_nama constructor harus init (destructor harus done)
**2St_ijinkan kunci kata statik dalam objects
**1s_jangan panggil assembler dan linker
**2sh_Hasilkan skrip untuk link pada host
**2st_Hasilkan skrip untuk link pada target
**2sr_Lewati fase alokasi register (gunakan dengan -alr)
**1T<x>_Target sistem operasi:
3*2Temx_OS/2 via EMX (termasuk EMX/RSX extender)
3*2Tfreebsd_FreeBSD
3*2Tgo32v2_Versi 2 dari DJ Delorie DOS extender
3*2Tlinux_Linux
3*2Tnetbsd_NetBSD
3*2Tnetware_Novell Netware Module (clib)
3*2Tnetwlibc_Novell Netware Module (libc)
3*2Topenbsd_OpenBSD
3*2Tos2_OS/2 / eComStation
3*2Tsunos_SunOS/Solaris
3*2Twatcom_Kompatibel Watcom DOS extender
3*2Twdosx_WDOSX DOS extender
3*2Twin32_Windows 32 Bit
3*2Twince_Windows CE
4*2Tlinux_Linux
6*2Tamiga_Commodore Amiga
6*2Tatari_Atari ST/STe/TT
6*2Tlinux_Linux/m68k
6*2Tmacos_Macintosh m68k (tidak didukung)
6*2Tpalmos_PalmOS
A*2Tlinux_Linux
A*2Twince_Windows CE
P*2Tamiga_AmigaOS pada PowerPC
P*2Tdarwin_Darwin dan MacOS X pada PowerPC
P*2Tlinux_Linux pada PowerPC
P*2Tmacos_MacOS (Klasik) pada PowerPC
P*2Tmorphos_MorphOS
S*2Tlinux_Linux
**1u<x>_simbol tidak didefinisikan <x>
**1U_opsi unit:
**2Un_jangan periksa nama unit
**2Ur_hasilkan file unit rilis
**2Us_kompilasi unit sistem
**1v<x>_Tampilkan. <x> adalah kombinasi dari huruf berikut:
**2*_e : Tampilkan kesalahan (default) 0 : Jangan tampilkan apapun (kecuali kesalahan)
**2*_w : Tampilkan peringatan          u : Tampilkan info unit
**2*_n : Tampilkan catatan             t : Tampilkan file yg dicoba/digunakan
**2*_h : Tampilkan petunjuk            c : Tampilkan kondisional
**2*_i : Tampilkan info umum           d : Tampilkan info debug
**2*_l : Tampilkan nomor baris         r : Mode kompatibilitas Rhide/GCC
**2*_a : Tampilkan semua               x : Info executable (hanya Win32)
**2*_b : Tulis pesan nama file dengan path lengkap
**2*_v : tulis fpcdebug.txt dengan     p : Tulis tree.log dengan pemindaian susunan
**2*_    banya info debugging
3*1W<x>_Opsi target mirip-Win32
3*2WB_Buat image bisa direlokasi
3*2WB<x>_Set basis Image ke nilai Heksadesimal <x>
3*2WC_Tetapkan tipe aplikasi konsol
3*2WD_Gunakan DEFFILE untuk mengekspor fungsi dari DLL atau EXE
3*2WF_Tetapkan tipe aplikasi layar-penuh (hanya OS/2)
3*2WG_Tetapkan tipe aplikasi grafis
3*2WN_Jangan hasilkan kode relokasi (perlu untuk debugging)
3*2WR_Hasilkan kode relokasi
P*2WC_Tetapkan tipe aplikasi konsol (hanya MacOS)
P*2WG_Tetapkan tipe aplikasi grafis (hnya MacOS)
P*2WT_Tetapkan tipe aplikasi piranti (piranti MPW, hanya MacOS)
**1X_opsi eksecutabel:
**2Xc_pass --berbagi dengan linker (hanya Unix)
**2Xd_jangan gunakan path pencarian librari standar (perlu untuk kompilasi silang)
**2Xe_gunakan linker eksternal
**2XD_coba me-link units dinamis          (definisikan FPC_LINK_DYNAMIC)
**2Xi_gunakan linker internal
**2Xm_hasilkan peta link
**2XM<x>_set nama dari rutin program 'main' (standarnya 'main')
**2XP<x>_biarkan nama binutils dengan prefiks <x>
**2Xr<x>_set path pencarian librari ke <x> (diperlukan untuk kompilasi silang)
**2Xs_hapus semua simbol dari eksecutabel
**2XS_coba me-link unit statik (default) (tetapkan FPC_LINK_STATIC)
**2Xt_link dengan librari statik (-statik dilangsukan ke linker)
**2XX_coba me-link units smart            (definisikan FPC_LINK_SMART)
**1*_
**1?_tampilkan panduan ini
**1h_tampilkan panduan ini tanpa menunggu
]

#
# The End...
