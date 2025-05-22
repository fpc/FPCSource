#
#   This file is part of the Free Pascal Compiler
#   Copyright (c) 1999-2018 by the Free Pascal Development team
#
#   Simplified Chinese (UTF-8) Language File for Free Pascal
#
#   See the file COPYING.v2, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

# CodePage 65001

#
# 常量按以下顺序构建:
# <part>_<type>_<txtidentifier>
#
# <part> 表示编译器使用该消息的部分
#   asmr_     汇编解析
#   asmw_     汇编写入/二进制写入器
#   unit_     单元处理
#   option_   命令行参数解析
#   scan_     扫描器
#   parser_   解析器
#   type_     类型检查
#   general_  常规信息
#   exec_     调用汇编器、外部链接器、绑定器
#   link_     内部链接器
#   package_  包处理
#   sym_      符号处理
#
# <type> 表示消息通常用于的类型
#   f_   致命错误
#   e_   错误
#   w_   警告
#   n_   注意
#   h_   提示
#   i_   信息
#   l_   添加行号
#   u_   已使用
#   t_   已尝试
#   c_   条件
#   d_   调试消息
#   x_   可执行文件信息
#   o_   普通 (例如, "按回车键继续")
#
# <type> 开头可以包含一个减号来标记该消息默认是关闭的。
# 例如参考 type_w_explicit_string_cast

#
# General
#
# 01033 is the last used one
#
# BeginOfTeX
% \section{通用编译器消息}
% 本节提供了不是致命错误但显示有用信息的编译器消息。
% 这些消息的数量可以通过各种详细级别的 \var{-v} 开关来控制。
% \begin{description}
general_t_compilername=01000_T_编译器: $1
% 当使用 \var{-vt} 开关时，此行告诉您使用的是什么编译器。
general_d_sourceos=01001_D_编译器操作系统: $1
% 当使用 \var{-vd} 开关时，此行告诉您源操作系统是什么。
general_i_targetos=01002_I_目标操作系统: $1
% 当使用 \var{-vd} 开关时，此行告诉您目标操作系统是什么。
general_t_exepath=01003_T_使用可执行文件路径: $1
% 当使用 \var{-vt} 开关时，此行告诉您编译器在哪里查找其二进制文件。
general_t_unitpath=01004_T_使用单元路径: $1
% 当使用 \var{-vt} 开关时，此行告诉您编译器在哪里查找已编译的单元。
% 您可以使用 \var{-Fu} 选项设置此路径。
general_t_includepath=01005_T_使用包含路径: $1
% 当使用 \var{-vt} 开关时，此行告诉您编译器在哪里查找其包含文件（在 \var{\{\$I xxx\}} 语句中使用的文件）。
% 您可以使用 \var{-Fi} 选项设置此路径。
general_t_librarypath=01006_T_使用库路径: $1
% 当使用 \var{-vt} 开关时，此行告诉您编译器在哪里查找库。
% 您可以使用 \var{-Fl} 选项设置此路径。
general_t_objectpath=01007_T_使用对象路径: $1
% 当使用 \var{-vt} 开关时，此行告诉您编译器在哪里查找要链接的对象文件（在 \var{\{\$L xxx\}} 语句中使用的文件）。
% 您可以使用 \var{-Fo} 选项设置此路径。
general_i_abslines_compiled=01008_I_已编译 $1 行，用时 $2 秒$3
% 当使用 \var{-vi} 开关时，编译器报告已编译的行数，以及编译所需的时间（实际时间，而不是程序时间）。
general_f_no_memory_left=01009_F_内存不足
% 编译器没有足够的内存来编译您的程序。有几种解决方法：
% \begin{itemize}
% \item 如果您使用编译器的构建选项，请尝试手动编译不同的单元。
% \item 如果您正在编译一个巨大的程序，将其拆分为多个单元，并分别编译。
% \item 如果前两种方法不起作用，请使用更大的堆重新编译编译器。（您可以使用 \var{-Ch} 选项，\seeo{Ch}。）
% \end{itemize}
general_i_writingresourcefile=01010_I_正在写入资源字符串表文件: $1
% 当编译器写入包含程序所有资源字符串的资源字符串表文件时，会显示此消息。
general_e_errorwritingresourcefile=01011_E_写入资源字符串表文件时出错: $1
% 当编译器在写入资源字符串表文件时遇到错误时，会显示此消息。
general_i_fatal=01012_I_致命错误:
% 致命错误的前缀。
general_i_error=01013_I_错误:
% 错误的前缀。
general_i_warning=01014_I_警告:
% 警告的前缀。
general_i_note=01015_I_注意:
% 注意的前缀。
general_i_hint=01016_I_提示:
% 提示的前缀。
general_e_path_does_not_exist=01017_E_路径 "$1" 不存在
% 指定的路径不存在。
general_f_compilation_aborted=01018_F_编译已中止
% 编译已中止。
general_text_bytes_code=01019_字节代码
% 生成的可执行代码的大小，以字节为单位。
general_text_bytes_data=01020_字节数据
% 生成的程序数据的大小，以字节为单位。
general_i_number_of_warnings=01021_I_发出了 $1 个警告
% 编译期间发出的警告总数。
general_i_number_of_hints=01022_I_发出了 $1 个提示
% 编译期间发出的提示总数。
general_i_number_of_notes=01023_I_发出了 $1 个注意
% 编译期间发出的注意总数。
general_f_ioerror=01024_F_I/O 错误: $1
% 编译期间发生了不允许继续编译的 I/O 错误。
general_f_oserror=01025_F_操作系统错误: $1
% 编译期间发生了不允许继续编译的操作系统错误。
general_e_exception_raised=01026_E_编译内部引发异常
% 由于异常生成，编译已中止。
general_t_unitscope=01027_T_使用单元作用域: $1
% 当使用 \var{-vt} 开关时，此行告诉您编译器在查找单元时使用的单元作用域（命名空间）。
% 您可以使用 \var{-FN} 选项添加单元作用域。
general_i_reduced_filesearch=01028_I_减少文件搜索：不搜索大写或 8.3 格式的单元文件名。
% 当使用 \var{-Ft} 开关时，此行通知您编译器不会查找大写文件名或符合 8.3 格式的文件名。
general_f_compiler_aborted=01029_F_编译器已停止
% 编译已中止。
general_t_unitpath_local=01030_T_$1: 使用本地单元路径: $2
% 当使用 \var{-vt} 开关时，此行告诉您编译器在哪里查找已编译的单元。
% 您可以使用 \var{\{\$UNITPATH xxx\}} 指令设置此路径。
general_t_includepath_local=01031_T_$1: 使用本地包含路径: $2
% 当使用 \var{-vt} 开关时，此行告诉您编译器在哪里查找其包含文件（在 \var{\{\$I xxx\}} 语句中使用的文件）。
% 您可以使用 \var{\{\$INCLUDEPATH xxx\}} 指令设置此路径。
general_t_librarypath_local=01032_T_$1: 使用本地库路径: $2
% 当使用 \var{-vt} 开关时，此行告诉您编译器在哪里查找库。
% 您可以使用 \var{\{\$LIBRARYPATH xxx\}} 指令设置此路径。
general_t_objectpath_local=01033_T_$1: 使用本地对象路径: $2
% 当使用 \var{-vt} 开关时，此行告诉您编译器在哪里查找要链接的对象文件（在 \var{\{\$L xxx\}} 语句中使用的文件）。
% 您可以使用 \var{\{\$OBJECTPATH xxx\}} 指令设置此路径。
% \end{description}
#
# Scanner
#
# 02112 is the last used one
#
% \section{扫描器消息}
% 本节列出了扫描器发出的消息。扫描器负责
% 处理 Pascal 文件的词法结构，即它尝试查找
% 保留字、字符串等。它还处理指令和
% 条件编译处理。
% \begin{description}
scan_f_end_of_file=02000_F_意外的文件结束
% 这通常发生在以下情况之一：
% \begin{itemize}
% \item 源文件在最终的 \var{end.} 语句之前结束。这
% 主要发生在 \var{begin} 和 \var{end} 语句不
% 平衡的情况下；
% \item 包含文件在语句中间结束。
% \item 注释未关闭。
% \end{itemize}
scan_f_string_exceeds_line=02001_F_字符串超出行限制
% 字符串中缺少结束的引号 '，因此它占用了多行。
scan_f_illegal_char=02002_F_非法字符 "$1" ($2)
% 在输入文件中遇到了非法字符。
scan_f_syn_expected=02003_F_语法错误，需要 "$1" 但找到 "$2"
% 这表示编译器期望的标记与
% 您输入的不同。几乎在任何可能出错的地方
% 都可能发生这种情况。
scan_t_start_include_file=02004_TL_开始读取包含文件 $1
% 当您提供 \var{-vt} 开关时，编译器会告诉您
% 它何时开始读取包含文件。
scan_w_comment_level=02005_W_发现注释嵌套级别 $1
% 当使用 \var{-vw} 开关时，如果
% 编译器发现嵌套注释，它会警告您。Turbo Pascal
% 和 Delphi 中不允许嵌套注释，这可能是错误的来源。
scan_n_ignored_switch=02008_N_忽略编译器开关 "$1"
% 当 \var{-vn} 开启时，如果编译器忽略某个开关，它会发出警告。
scan_w_illegal_switch=02009_W_非法编译器开关 "$1"
% 您包含了一个编译器不识别的编译器开关（即 \var{\{\$... \}}）。
scan_w_switch_is_global=02010_W_全局编译器开关位置错误，已忽略
% 编译器开关位置错误。它必须位于
% 编译单元的开始处，在 uses 子句或任何声明之前。
scan_e_illegal_char_const=02011_E_非法字符常量
% 当您使用 ASCII 码指定字符时，如
% \var{\#96}，但数字非法或超出范围时，会发生这种情况。
scan_f_cannot_open_input=02012_F_无法打开文件 "$1"
% \fpc 找不到您在命令行上指定的程序或单元源文件。
scan_f_cannot_open_includefile=02013_F_无法打开包含文件 "$1"
% \fpc 找不到您在 \var{\{\$include ..\}} 语句中指定的源文件。
scan_e_illegal_pack_records=02015_E_非法记录对齐说明符 "$1"
% 您使用 \var{\{\$PACKRECORDS n\} } 或 \var{\{\$ALIGN n\} }
% 时指定了非法的 \var{n} 值。对于 \$PACKRECORDS，有效的对齐值为 1、2、4、8、16、32、C、
% NORMAL、DEFAULT，对于 \$ALIGN，有效的对齐值为 1、2、4、8、16、32、ON、
% OFF。在 MacPas 模式下，\$ALIGN 还支持 MAC68K、POWER 和 RESET。
scan_e_illegal_pack_enum=02016_E_非法枚举最小大小说明符 "$1"
% 您使用 \var{\{\$PACKENUM n\}} 时指定了非法的 \var{n} 值。
% 这里只有 1、2、4、NORMAL 或 DEFAULT 是有效的。
scan_e_endif_expected=02017_E_需要 $ENDIF 对应于在 $3 行 $4 中定义的 $1 $2
% 您的条件编译语句不平衡。
scan_e_preproc_syntax_error=02018_E_解析条件编译表达式时出现语法错误
% \var{\{\$if ..\}}、\var{\{\$ifc \}} 或 \var{\{\$setc \}} 编译器指令后的表达式有错误。
scan_e_error_in_preproc_expr=02019_E_评估条件编译表达式时出错
% \var{\{\$if ..\}}、$ifc 或 $setc 编译器指令后的表达式有错误。
scan_w_macro_cut_after_255_chars=02020_W_宏内容长度限制为 255 个字符
% 宏的内容不能超过 255 个字符。
scan_e_endif_without_if=02021_E_ENDIF 没有对应的 IF(N)DEF
% 您的 \var{\{\$IFDEF ..\}} 和 {\{\$ENDIF\}} 语句不平衡。
scan_f_user_defined=02022_F_用户定义：$1
% 发生了用户定义的致命错误。另请参阅 \progref。
scan_e_user_defined=02023_E_用户定义：$1
% 发生了用户定义的错误。另请参阅 \progref。
scan_w_user_defined=02024_W_用户定义：$1
% 发生了用户定义的警告。另请参阅 \progref。
scan_n_user_defined=02025_N_用户定义：$1
% 遇到了用户定义的注释。另请参阅 \progref。
scan_h_user_defined=02026_H_用户定义：$1
% 遇到了用户定义的提示。另请参阅 \progref。
scan_i_user_defined=02027_I_用户定义：$1
% 遇到了用户定义的信息。另请参阅 \progref。
scan_e_keyword_cant_be_a_macro=02028_E_关键字重定义为宏无效
% 您不能用宏重定义关键字。
scan_f_macro_buffer_overflow=02029_F_读取或展开宏时宏缓冲区溢出
% 您的宏或其结果对编译器来说太长了。
scan_w_macro_too_deep=02030_W_宏展开超过 16 层深度。
% 展开宏时，宏嵌套达到 16 层。
% 编译器将不再展开，因为这可能是
% 使用了递归的迹象。
scan_w_wrong_styled_switch=02031_W_编译器开关不支持在 // 样式的注释中使用
% 编译器开关应该在普通 Pascal 样式的注释中。
scan_d_handling_switch=02032_DL_处理开关 "$1"
% 当您开启调试信息（\var{-vd}）时，编译器会告诉您
% 它何时评估条件编译语句。
scan_c_endif_found=02033_CL_找到 ENDIF $1
% 当您开启条件消息（\var{-vc}）时，编译器会告诉您
% 它在哪里遇到条件语句。
scan_c_ifdef_found=02034_CL_找到 IFDEF $1，$2
% 当您开启条件消息（\var{-vc}）时，编译器会告诉您
% 它在哪里遇到条件语句。
scan_c_ifopt_found=02035_CL_找到 IFOPT $1，$2
% 当您开启条件消息（\var{-vc}）时，编译器会告诉您
% 它在哪里遇到条件语句。
scan_c_if_found=02036_CL_找到 IF $1，$2
% 当您开启条件消息（\var{-vc}）时，编译器会告诉您
% 它在哪里遇到条件语句。
scan_c_ifndef_found=02037_CL_找到 IFNDEF $1，$2
% 当您开启条件消息（\var{-vc}）时，编译器会告诉您
% 它在哪里遇到条件语句。
scan_c_else_found=02038_CL_找到 ELSE $1，$2
% 当您开启条件消息（\var{-vc}）时，编译器会告诉您
% 它在哪里遇到条件语句。
scan_c_skipping_until=02039_CL_跳过直到...
% 当您开启条件消息（\var{-vc}）时，编译器会告诉您
% 它在哪里遇到条件语句，以及它是跳过还是
% 编译部分。
scan_i_press_enter=02040_I_按 <回车键> 继续
% 当使用 \var{-vi} 开关时，编译器会停止编译
% 并在遇到 \var{\{\$STOP\}} 指令时等待按下 \var{Enter} 键。
scan_w_unsupported_switch=02041_W_不支持的开关 "$1"
% 当警告开启（\var{-vw}）时，编译器会警告您
% 不支持的开关。这意味着该开关在 Delphi 或
% Turbo Pascal 中使用，但在 \fpc 中不使用。
scan_w_illegal_directive=02042_W_非法编译器指令 "$1"
% 当警告开启（\var{-vw}）时，编译器会警告您
% 无法识别的开关。有关已识别开关的列表，请参阅 \progref。
scan_t_back_in=02043_TL_返回到 $1
% 当您使用 \var{-vt} 开关时，编译器会告诉您
% 它何时完成读取包含文件。
scan_w_unsupported_app_type=02044_W_不支持的应用程序类型："$1"
% 如果您使用 \var{\{\$APPTYPE\}} 指令指定了未知的应用程序类型，
% 您会收到此警告。
scan_w_app_type_not_support=02045_W_目标操作系统不支持 APPTYPE
% \var{\{\$APPTYPE\}} 指令仅受某些操作系统支持。
scan_w_description_not_support=02046_W_目标操作系统不支持 DESCRIPTION
% 目标操作系统不支持 \var{\{\$DESCRIPTION\}} 指令。
scan_n_version_not_support=02047_N_目标操作系统不支持 VERSION
% 目标操作系统不支持 \var{\{\$VERSION\}} 指令。
scan_n_only_exe_version=02048_N_VERSION 仅用于可执行文件或 DLL
% \var{\{\$VERSION\}} 指令仅用于可执行文件或 DLL 源代码。
scan_w_wrong_version_ignored=02049_W_VERSION 指令格式错误 "$1"
% \var{\{\$VERSION\}} 指令格式为 主版本号.次版本号，
% 其中主版本号和次版本号是 word 类型。
scan_e_illegal_asmmode_specifier=02050_E_指定了非法的汇编器样式 "$1"
% 当您使用 \var{\{\$ASMMODE xxx\}} 指令指定汇编器模式时，
% 编译器无法识别您指定的模式。
scan_w_no_asm_reader_switch_inside_asm=02051_W_在汇编语句内部无法切换 ASM 读取器，"$1" 仅对下一个有效
% 在汇编块内部无法从一个汇编读取器切换到另一个。
% 新的读取器仅用于下一个汇编语句。
scan_e_wrong_switch_toggle=02052_E_开关切换错误，请使用 ON/OFF 或 +/-
% 您需要使用 ON 或 OFF 或 + 或 - 来切换开关。
scan_e_resourcefiles_not_supported=02053_E_此目标不支持资源文件
% 您正在编译的目标不支持资源文件。
scan_w_include_env_not_found=02054_W_在环境中找不到包含环境变量 "$1"
% 在环境中找不到包含的环境变量；它将被替换为空字符串。
scan_e_invalid_maxfpureg_value=02055_E_FPU 寄存器限制的值非法
% 此指令的有效值为 0..8 和 NORMAL/DEFAULT。
scan_w_only_one_resourcefile_supported=02056_W_此目标仅支持一个资源文件
% 您正在编译的目标仅支持一个资源文件。
% 将使用找到的第一个资源文件，其他的将被丢弃。
scan_w_macro_support_turned_off=02057_W_宏支持已关闭
% 发现了宏声明，但宏支持当前已关闭，
% 因此声明将被忽略。要开启宏支持，请在命令行上使用
% -Sm 编译或在源代码中添加 \{\$MACRO ON\}。
scan_e_invalid_interface_type=02058_E_指定了非法的接口类型。有效类型为 COM、CORBA 或 DEFAULT。
% 不支持指定的接口类型。
scan_w_appid_not_support=02059_W_APPID 仅支持 PalmOS
% \var{\{\$APPID\}} 指令仅支持 PalmOS 目标。
scan_w_appname_not_support=02060_W_APPNAME 仅支持 PalmOS
% \var{\{\$APPNAME\}} 指令仅支持 PalmOS 目标。
scan_e_string_exceeds_255_chars=02061_E_常量字符串不能超过 255 个字符
% 单个字符串常量最多只能包含 255 个字符。尝试将字符串
% 拆分为多个较小的部分，并使用 + 运算符连接它们。
scan_f_include_deep_ten=02062_F_包含文件的嵌套超过 16 层深度。
% 包含文件时，文件嵌套达到 16 层。
% 编译器将不再展开，因为这可能是
% 使用了递归的迹象。
scan_e_too_many_push=02063_F_PUSH 的级别过多
% 最多允许 20 个级别。此错误仅在 MacPas 模式下发生。
scan_e_too_many_pop=02064_E_没有前面的 PUSH 就使用了 POP
% 此错误仅在 MacPas 模式下发生。
scan_e_error_macro_lacks_value=02065_E_宏或编译时变量 "$1" 没有任何值
% 因此无法评估条件编译时表达式。
scan_e_wrong_switch_toggle_default=02066_E_开关切换错误，请使用 ON/OFF/DEFAULT 或 +/-/*
% 您需要使用 ON 或 OFF 或 DEFAULT 或 + 或 - 或 * 来切换开关。
scan_e_mode_switch_not_allowed=02067_E_此处不允许模式开关 "$1"
% 已经遇到了模式开关，或者在 -Mmacpas 选项的情况下，
% 模式开关出现在 UNIT 之后。
scan_e_error_macro_undefined=02068_E_编译时变量或宏 "$1" 未定义。
% 因此无法评估条件编译时表达式。仅在 MacPas 模式下。
scan_e_utf8_bigger_than_65535=02069_E_发现大于 65535 的 UTF-8 代码
% \fpc 内部将 UTF-8 字符串处理为宽字符串，即字符代码限制为 65535。
scan_e_utf8_malformed=02070_E_格式错误的 UTF-8 字符串
% 给定的字符串不是有效的 UTF-8 字符串。
scan_c_switching_to_utf8=02071_C_发现 UTF-8 签名，使用 UTF-8 编码
% 编译器在文件开头发现了 UTF-8 编码签名（\$ef, \$bb, \$bf），
% 因此将其解释为 UTF-8 文件。
scan_e_compile_time_typeerror=02072_E_编译时表达式：在 $3 处需要 $1 但得到 $2
% 编译时表达式的类型检查失败。
scan_n_app_type_not_support=02073_N_目标操作系统不支持 APPTYPE
% \var{\{\$APPTYPE\}} 指令仅受某些操作系统支持。
scan_e_illegal_optimization_specifier=02074_E_指定了非法的优化 "$1"
% 您使用 \var{\{\$OPTIMIZATION xxx\}} 指令指定了优化，
% 但编译器无法识别您指定的优化。
scan_w_setpeflags_not_support=02075_W_目标操作系统不支持 SETPEFLAGS
% 目标操作系统不支持 \var{\{\$SETPEFLAGS\}} 指令。
scan_w_imagebase_not_support=02076_W_目标操作系统不支持 IMAGEBASE
% 目标操作系统不支持 \var{\{\$IMAGEBASE\}} 指令。
scan_w_minstacksize_not_support=02077_W_目标操作系统不支持 MINSTACKSIZE
% 目标操作系统不支持 \var{\{\$MINSTACKSIZE\}} 指令。
scan_w_maxstacksize_not_support=02078_W_目标操作系统不支持 MAXSTACKSIZE
% 目标操作系统不支持 \var{\{\$MAXSTACKSIZE\}} 指令。
scanner_e_illegal_warn_state=02079_E_$WARN 指令的状态 "$1" 非法
% 只能使用 ON 和 OFF 作为 \var{\{\$WARN\}} 编译器指令的状态。
scan_e_only_packset=02080_E_非法的集合打包值
% packset 参数只允许使用 0、1、2、4、8、DEFAULT 和 NORMAL。
scan_w_pic_ignored=02081_W_PIC 指令或开关被忽略
% 几个目标，如 \windows，不支持也不需要 PIC，
% 因此 PIC 指令和开关被忽略。
scan_w_unsupported_switch_by_target=02082_W_当前选择的目标不支持开关 "$1"
% 一些编译器开关，如 \$E，并非所有目标都支持。
scan_w_frameworks_darwin_only=02084_W_框架相关选项仅支持 Darwin/Mac OS X
% 在 Darwin/Mac OS X 以外的操作系统上，框架不是已知概念，
% 或者至少 FPC 不支持。
scan_e_illegal_minfpconstprec=02085_E_非法的最小浮点常量精度 "$1"
% 浮点常量的有效最小精度为 default、32 和 64，
% 分别表示最小（通常为 32 位）、32 位和 64 位精度。
scan_w_multiple_main_name_overrides=02086_W_多次覆盖 "main" 过程的名称，之前设置为 "$1"
% 主入口过程的名称被多次指定。只有最后一个
% 名称会被使用。
scanner_w_illegal_warn_identifier=02087_W_$WARN 指令的标识符 "$1" 非法
% \var{\{\$WARN\}} 编译器指令不知道该标识符。
scanner_e_illegal_alignment_directive=02088_E_非法的对齐指令
% 对齐指令无效。对齐类型未知或对齐
% 值不是 2 的幂。
scanner_f_illegal_utf8_bom=02089_F_无法在使用不同代码页的模块中包含以 UTF-8 BOM 开头的文件
% 作为单个编译实体（程序、库、单元）一部分的所有源代码必须使用
% 相同的代码页进行编码
scanner_w_directive_ignored_on_target=02090_W_当前目标平台忽略指令 "$1"
% 某些指令在某些目标上被忽略，例如在托管平台上更改
% packrecords 和 packenum 设置。
scan_w_unavailable_system_codepage=02091_W_编译器不可用当前系统代码页 "$1"。将默认代码页切换回 "$2"。
% 编译器不知道当前系统代码页。
% 编译器编译时内置了对几个代码页的支持。
% 操作系统的代码页不在该列表中。您需要重新编译
% 编译器，使其支持此代码页。
scan_w_setpeoptflags_not_support=02092_W_目标操作系统不支持 SETPEOPTFLAGS
% 目标操作系统不支持 \var{\{\$SETPEOPTFLAGS\}} 指令。
scan_e_illegal_peflag=02093_E_SETPEFLAGS 的参数非法
% SETPEFLAGS 的给定参数既不是正确的命名值，也不是
% 序数值
scan_e_illegal_peoptflag=02094_E_SETPEOPTFLAGS 的参数非法
% SETPEOPTFLAGS 的给定参数既不是正确的命名值，也不是
% 序数值
scan_e_unsupported_switch=02095_E_此目标不支持指令 $1
% 并非所有编译器指令都在所有目标上受支持。
scan_w_invalid_stacksize=02096_W_指定的堆栈大小不在平台的有效范围内。设置堆栈大小被忽略。
% 32 位和 64 位平台上堆栈大小的有效范围为 1024 - 67107839，
% 16 位平台上为 1024 - 65520。此外，为了兼容 Turbo Pascal 7，
% 在 16 位平台上指定 65521 的堆栈大小实际上会将堆栈大小设置为 65520。
scan_w_heapmax_lessthan_heapmin=02097_W_指定的 HeapMax 值小于 HeapMin 值。设置 HeapMax 被忽略。
% HeapMax 值（如果指定）必须大于或等于 HeapMin
% 值。否则，HeapMax 值将被忽略。
scan_e_illegal_hugepointernormalization=02098_E_HUGEPOINTERNORMALIZATION 的参数非法
% HUGEPOINTERNORMALIZATION 唯一允许的值为 BORLANDC、MICROSOFTC
% 和 WATCOMC。
scan_e_illegal_asmcpu_specifier=02099_E_指定了非法的汇编器 CPU 指令集 "$1"
% 当您使用 \var{\{\$ASMCPU xxx\}} 指令指定汇编器 CPU 时，
% 编译器无法识别您指定的 CPU。
scan_w_syscall_convention_not_useable_on_target=02100_W_指定的系统调用约定在此目标上不可用
% 使用 \var{\{\$SYSCALL xxx\}} 指令指定的系统调用约定，
% 在当前目标系统上不可用。
scan_w_syscall_convention_invalid=02101_W_指定的系统调用约定无效
% 编译器无法识别由 \var{\{\$SYSCALL xxx\}} 指令指定的系统调用约定。
scan_w_setpeuserversion_not_support=02102_W_目标操作系统不支持 SETPEUSERVERSION
% 目标操作系统不支持 \var{\{\$SETPEUSERVERSION\}} 指令。
scan_w_setpeosversion_not_support=02103_W_目标操作系统不支持 SETPEOSVERSION
% 目标操作系统不支持 \var{\{\$SETPEOSVERSION\}} 指令。
scan_w_setpesubsysversion_not_support=02104_W_目标操作系统不支持 SETPESUBSYSVERSION
% 目标操作系统不支持 \var{\{\$SETPESUBSYSVERSION\}} 指令。
scan_n_changecputype=02105_N_更改 CPU 类型以与指定的控制器保持一致
scan_e_emptymacroname=02106_E_宏/编译器变量名不能为空
scan_e_unexpected_ifend=02107_E_找到 $IFEND 指令，但没有匹配的 $IF 指令
% 当通过指令 \var{\$LEGACYIFEND} 开启旧式 ifend 时，
% \var{\$IF} 指令必须由 \var{\$IFEND} 指令关闭，
% \var{\$IFDEF} 指令必须由 \var{\$ENDIF} 指令关闭。
scan_e_unexpected_endif=02108_E_找到 $ENDIF 指令，但没有匹配的 $IF(N)DEF 指令
% 当通过指令 \var{\$LEGACYIFEND} 开启旧式 ifend 时，
% \var{\$IF} 指令必须由 \var{\$IFEND} 指令关闭，
% \var{\$IFDEF} 指令必须由 \var{\$ENDIF} 指令关闭。
scan_e_invalid_rtti_clause=02109_E_无效的 RTTI 子句（需要 Explicit 或 Inherit）
% \var{\$RTTI} 指令后面需要跟 \var{EXPLICIT} 或 \var{INHERIT}
% 以及 \var{FIELDS}、\var{PROPERTIES} 或 \var{METHODS} 的条目。
scan_e_incomplete_rtti_clause=02110_E_Explicit 子句至少需要一个选项（Methods、Properties 或 Fields）
% \var{\$RTTI EXPLICIT} 指令至少需要 \var{FIELDS}、\var{PROPERTIES}
% 或 \var{METHODS} 条目中的一个。
scan_e_invalid_rtti_option=02111_E_无效的 RTTI 选项 "$1"（需要 Methods、Properties 或 Fields）
% \var{\$RTTI EXPLICIT} 只能包含 \var{FIELDS}、\var{PROPERTIES} 和 \var{METHODS}
% 条目。
scan_e_duplicate_rtti_option=02112_E_重复的 RTTI 选项 "$1"
% \var{\$RTTI EXPLICIT} 指令中的选项只能出现一次。
scan_e_misplaced_rtti_directive=02113_E_RTTI 指令不能在此处使用
% \var{\$RTTI} 指令不能在此位置使用（例如在 \var{PROGRAM}
% 或 \var{UNIT} 头之前）。
% \end{description}
#
# Parser
#
# 03368 is the last used one
#
% \section{解析器消息}
% 本节列出所有解析器消息。解析器负责处理语言的语义，
% 即判断您的 Pascal 语法结构是否正确。
% \begin{description}
parser_e_syntax_error=03000_E_解析器 - 语法错误
% 遇到了违反 Turbo Pascal 语言规则的错误。这通常发生在源文件中
% 出现非法字符时。
parser_e_dont_nest_interrupt=03004_E_INTERRUPT 过程不能嵌套
% \var{INTERRUPT} 过程必须是全局的。
parser_w_proc_directive_ignored=03005_W_过程类型 "$1" 被忽略
% FPC 程序会忽略指定的过程指令。
parser_e_no_overload_for_all_procs=03006_E_不是所有的 "$1" 声明都使用了 OVERLOAD
% 当您想要使用 \var{OVERLOAD} 指令进行重载时，所有声明都需要
% 指定 \var{OVERLOAD}。
parser_e_export_name_double=03008_E_导出函数名 "$1" 重复
% 特定 DLL 内的导出函数名必须互不相同。
parser_e_export_ordinal_double=03009_E_导出函数索引 $1 重复
% 特定 DLL 内的导出函数索引必须互不相同。
parser_e_export_invalid_index=03010_E_导出函数的索引无效
% DLL 函数索引必须在 \var{1..\$FFFF} 范围内。
parser_w_parser_reloc_no_debug=03011_W_可重定位的 DLL 或可执行文件 $1 调试信息不可用，已禁用。
% 目前无法在可重定位 DLL 中包含调试信息。
parser_w_parser_win32_debug_needs_WN=03012_W_要允许调试 win32 代码，您需要使用 -WN 选项禁用重定位
% 可重定位 DLL 或 EXE 的 Stabs 调试信息是错误的。如果您想要
% 调试 win32 可执行文件，请使用 -WN。
parser_e_constructorname_must_be_init=03013_E_构造函数名必须是 INIT
% 您正在声明一个名称不是 \var{init} 的对象构造函数，并且
% \var{-Ss} 开关已启用。请参见开关 \seeo{Ss}。
parser_e_destructorname_must_be_done=03014_E_析构函数名必须是 DONE
% 您正在声明一个名称不是 \var{done} 的对象析构函数，并且
% \var{-Ss} 开关已启用。请参见开关 \seeo{Ss}。
parser_e_proc_inline_not_supported=03016_E_不支持 INLINE 过程类型
% 您试图编译带有 C++ 风格内联的程序，但忘记指定 \var{-Si} 选项
% (\seeo{Si})。编译器默认不支持 C++ 风格的内联。
parser_w_constructor_should_be_public=03018_W_构造函数应该是 public 的
% 构造函数必须在对象（类）声明的 'public' 部分中。
parser_w_destructor_should_be_public=03019_W_析构函数应该是 public 的
% 析构函数必须在对象（类）声明的 'public' 部分中。
parser_n_only_one_destructor=03020_N_类应该只有一个析构函数
% 一个类只能声明一个析构函数。
parser_e_no_local_objects=03021_E_不允许局部类定义
% 类必须在全局范围内定义。它们不能在过程或函数内部定义。
parser_f_no_anonym_objects=03022_F_不允许匿名类定义
% 遇到无效的对象（类）声明，即没有方法且不是从其他对象或
% 类派生的对象或类。例如：
% \begin{verbatim}
% Type o = object
%          a : longint;
%          end;
% \end{verbatim}
% 将触发此错误。
parser_n_object_has_no_vmt=03023_N_对象 "$1" 没有 VMT
% 这是一个提示，表明声明的对象没有虚方法表。
parser_e_illegal_parameter_list=03024_E_非法参数列表
% 您调用函数时使用的参数类型与函数声明的参数类型不同。
parser_e_wrong_parameter_size=03026_E_调用 "$1" 时指定的参数数量错误
% 函数或过程的参数列表中有错误 -- 参数数量不正确。
parser_e_overloaded_no_procedure=03027_E_重载标识符 "$1" 不是函数
% 编译器遇到了一个与重载函数同名的符号，但它不是可以重载的函数。
parser_e_overloaded_have_same_parameters=03028_E_重载函数具有相同的参数列表
% 您正在声明重载函数，但它们具有相同的参数列表。
% 重载函数的声明中必须至少有 1 个不同的参数。
parser_e_header_dont_match_forward=03029_E_函数头与先前的声明 "$1" 不匹配
% 您声明了一个具有相同参数但结果类型或函数修饰符不同的函数。
parser_e_header_different_var_names=03030_E_函数头 "$1" 与前向声明不匹配：变量名改变 $2 => $3
% 您在 \var{interface} 部分或使用 \var{forward} 指令声明了函数，
% 但定义时使用了不同的参数列表。
parser_n_duplicate_enum=03031_N_枚举类型中的值必须递增
% \fpc 允许像 C 那样的枚举构造。检查以下两个声明：
% \begin{verbatim}
% type a = (A_A,A_B,A_E:=6,A_UAS:=200);
% type a = (A_A,A_B,A_E:=6,A_UAS:=4);
% \end{verbatim}
% 第二个声明会产生错误。\var{A\_UAS} 需要有一个比 \var{A\_E} 更高的
% 值，即至少为 7。
parser_e_no_with_for_variable_in_other_segments=03033_E_不能对其他段中的变量使用 With
% With 在栈上本地存储变量，但如果变量属于另一个段，
% 这是不可能的。
parser_e_too_much_lexlevel=03034_E_函数嵌套 > 31
% 函数定义的嵌套深度只能达到 31 层。
parser_e_range_check_error=03035_E_计算常量时发生范围检查错误
% 常量超出其允许的范围。
parser_w_range_check_error=03036_W_计算常量时发生范围检查错误
% 常量超出其允许的范围。
parser_e_double_caselabel=03037_E_重复的 case 标签
% 您在 \var{case} 语句中两次指定了相同的标签。
parser_e_case_lower_less_than_upper_bound=03038_E_case 范围的上界小于下界
% case 标签的上界小于下界，这是无意义的。
parser_e_type_const_not_possible=03039_E_不允许类或接口的类型常量
% 您不能声明类或对象类型的常量。
parser_e_no_overloaded_procvars=03040_E_不允许重载函数的函数变量
% 您正在尝试将重载函数赋值给过程变量。
% 这是不允许的。
parser_e_invalid_string_size=03041_E_字符串长度必须是 1 到 255 之间的值
% Pascal 中短字符串的长度限制为 255 个字符。您正在尝试
% 声明长度小于 1 或大于 255 的字符串。
parser_w_use_extended_syntax_for_objects=03042_W_对于对象实例，请使用 NEW 和 DISPOSE 的扩展语法
% 如果您有一个指向对象类型的指针 \var{a}，那么语句
% \var{new(a)} 不会初始化对象（即不会调用构造函数），尽管会分配空间。
% 您应该使用 \var{new(a,init)} 语句。这将分配空间，并调用对象的
% 构造函数。
parser_w_no_new_dispose_on_void_pointers=03043_W_对无类型指针使用 NEW 或 DISPOSE 是无意义的
parser_e_no_new_dispose_on_void_pointers=03044_E_无法对无类型指针使用 NEW 或 DISPOSE
% 如果 \var{p} 是无类型指针，您不能使用 \var{new(p)} 或 \var{dispose(p)}，
% 因为无类型指针没有关联的大小。
% 在 \var{TP} 和 \var{DELPHI} 模式下为了兼容性而接受，但编译器
% 仍会在发现这种构造时发出警告。
parser_e_class_id_expected=03045_E_需要类标识符
% 当编译器扫描包含点的过程声明（即对象或类方法）时，
% 但点前面的类型不是已知类型时，会发生这种情况。
parser_e_no_type_not_allowed_here=03046_E_此处不允许类型标识符
% 您不能在表达式中使用类型。
parser_e_methode_id_expected=03047_E_需要方法标识符
% 此标识符不是方法。
% 当编译器扫描包含点的过程声明（即对象或类方法）时，
% 但过程名不是该类型的过程时，会发生这种情况。
parser_e_header_dont_match_any_member=03048_E_函数头与此类 "$1" 的任何方法都不匹配
% 此标识符不是方法。
% 当编译器扫描包含点的过程声明（即对象或类方法）时，
% 但过程名不是该类型的过程时，会发生这种情况。
parser_d_procedure_start=03049_DL_过程/函数 $1
% 当使用 \var{-vd} 开关时，编译器会告诉您它何时开始
% 处理过程或函数实现。
parser_e_error_in_real=03050_E_非法浮点常量
% 编译器期望一个浮点表达式，但得到了其他内容。
parser_e_fail_only_in_constructor=03051_E_FAIL 只能在构造函数中使用
% 您在构造函数方法之外使用了 \var{fail} 关键字。
parser_e_no_paras_for_destructor=03052_E_析构函数不能有参数
% 您正在声明带有参数列表的析构函数。析构函数方法
% 不能有参数。
parser_e_only_class_members_via_class_ref=03053_E_只有类方法、类属性和类变量可以通过类引用引用
% 在以下情况下会出现此错误：
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
% \var{Free} 不是类方法，因此不能用类引用调用。
parser_e_only_class_members=03054_E_在类方法中只能访问类方法、类属性和类变量
% 这与前一个错误相关。您不能从类方法内部调用对象的方法。
% 以下代码会产生此错误：
% \begin{verbatim}
% class procedure tobject.x;
%
% begin
%   free
% \end{verbatim}
% 因为 free 是类的普通方法，它不能从类方法中调用。
parser_e_case_mismatch=03055_E_常量和 CASE 类型不匹配
% 其中一个标签与 case 变量的类型不同。
parser_e_illegal_symbol_exported=03056_E_该符号不能从库中导出
% 当您编写库时，只能导出过程和函数。您不能导出
% 变量或常量。
parser_w_should_use_override=03057_W_继承的方法被 "$1" 隐藏
% 在父类中声明为 \var{virtual} 的方法，应该在派生类中
% 使用 \var{override} 指令重写。如果您不指定 \var{override} 指令，
% 您将隐藏父方法而不是重写它。
parser_e_nothing_to_be_overridden=03058_E_在祖先类中没有可以被重写的方法："$1"
% 您正在尝试 \var{override} 一个不存在的父类虚方法。
parser_e_no_procedure_to_access_property=03059_E_没有提供访问属性的成员
% 您没有为属性指定 \var{read} 指令。
parser_w_stored_not_implemented=03060_W_Stored 属性指令尚未实现
% 此消息不再使用，因为 \var{stored} 指令已经实现。
parser_e_ill_property_access_sym=03061_E_属性访问的符号非法
% 数组属性的 \var{read} 或 \var{write} 指令中有错误。当您声明
% 数组属性时，只能用过程和函数访问它。以下代码会导致此错误。
% \begin{verbatim}
% tmyobject = class
%   i : integer;
%   property x [i : integer]: integer read I write i;
% \end{verbatim}
%
parser_e_cant_access_protected_member=03062_E_不能在此处访问对象的 protected 字段
% 在对象或类声明的 \var{protected} 部分中声明的字段不能在定义该对象的模块外部
% 或在后代对象方法之外访问。
parser_e_cant_access_private_member=03063_E_不能在此处访问对象的 private 字段
% 在对象或类声明的 \var{private} 部分中声明的字段不能在定义该类的模块外部访问。
parser_e_overridden_methods_not_same_ret=03066_E_重写的方法必须具有相同的返回类型："$2" 被 "$1" 重写，但返回类型不同
% 如果在类定义中声明重写的方法，它们必须具有相同的返回类型。
parser_e_dont_nest_export=03067_E_声明为 EXPORT 的函数不能嵌套
% 不能在声明为 export 的函数或过程内部声明函数或过程。
parser_e_methods_dont_be_export=03068_E_方法不能被 EXPORT
% 不能将作为对象方法的过程声明为 \var{export}。
parser_e_call_by_ref_without_typeconv=03069_E_参数 $1 的引用调用必须完全匹配：得到 "$2" 但期望 "$3"
% 当调用带有 \var{var} 参数的函数时，函数调用中的变量必须是完全相同的类型。
% 不会进行自动类型转换。
parser_e_no_super_class=03070_E_该类不是当前类的父类
% 当调用继承的方法时，您试图调用一个不相关类的方法。您只能调用父类的继承方法。
parser_e_self_not_in_method=03071_E_SELF 只允许在方法中使用
% 您正在尝试在对象方法之外使用 \var{self} 参数。只有方法才会传递 \var{self} 参数。
parser_e_generic_methods_only_in_methods=03072_E_方法只能在其他方法中通过类的类型标识符直接调用
% 像 \var{sometype.somemethod} 这样的结构只允许在方法中使用。
parser_e_illegal_colon_qualifier=03073_E_非法使用 ':'
% 您在一个不是真实表达式的表达式上使用了两次 \var{:}（冒号）。
parser_e_illegal_set_expr=03074_E_集合构造器中的范围检查错误或重复的集合元素
% 集合的声明包含错误。要么其中一个元素超出了集合类型的范围，要么两个元素实际上是相同的。
parser_e_pointer_to_class_expected=03075_E_需要对象指针
% 您在 \var{new} 语句中指定了非法类型。\var{new} 的扩展语法需要一个对象作为参数。
parser_e_expr_have_to_be_constructor_call=03076_E_表达式必须是构造函数调用
% 当使用 \var{new} 的扩展语法时，您必须指定要创建的对象的构造函数方法。
% 您指定的过程不是构造函数。
parser_e_expr_have_to_be_destructor_call=03077_E_表达式必须是析构函数调用
% 当使用 \var{dispose} 的扩展语法时，您必须指定要释放的对象的析构函数方法。
% 您指定的过程不是析构函数。
parser_e_invalid_record_const=03078_E_记录元素顺序非法
% 在声明常量记录时，您指定的字段顺序错误。
parser_e_false_with_expr=03079_E_表达式类型必须是类或记录类型，得到 $1
% \var{with} 语句需要一个 \var{record} 或 \var{class} 类型的参数。
% 您在一个不是这些类型的表达式上使用了 \var{with}。
parser_e_void_function=03080_E_过程不能返回值
% 在 \fpc 中，您可以在使用 \var{exit} 语句时为函数指定返回值。
% 当您尝试对过程这样做时会出现此错误。过程不能返回值。
parser_e_only_methods_allowed=03081_E_构造函数、析构函数和类运算符必须是方法
% 您正在将一个不是类方法的过程声明为析构函数、构造函数或类运算符。
parser_e_operator_not_overloaded=03082_E_运算符未被重载
% 您正在尝试使用一个未为此类型重载的运算符。
parser_e_no_such_assignment=03083_E_无法为相同类型重载赋值
% 您不能为编译器认为相等的类型重载赋值。
parser_e_overload_impossible=03084_E_无法重载运算符
% 运算符、参数和返回类型的组合不兼容。
parser_e_no_reraise_possible=03085_E_此处不能重新抛出异常
% 您正在尝试在不允许的地方重新抛出异常。您只能在 \var{except} 块中重新抛出异常。
parser_e_no_new_or_dispose_for_classes=03086_E_类不允许使用 new 或 dispose 的扩展语法
% 您不能使用 \var{new} 的扩展语法生成类的实例。必须使用构造函数。
% 同样的原因，您不能调用 \var{dispose} 来释放类的实例，必须使用析构函数。
parser_e_procedure_overloading_is_off=03088_E_过程重载已关闭
% 当使用 \var{-So} 开关时，过程重载被关闭。Turbo Pascal 不支持函数重载。
parser_e_overload_operator_failed=03089_E_无法重载此运算符。相关可重载运算符（如果有）是：$1
% 您正在尝试重载一个不能重载的运算符。
% 以下运算符可以重载：
% \begin{verbatim}
%    +, -, *, /, =, >, <, <=, >=, is, as, in, **, :=
% \end{verbatim}
parser_e_comparative_operator_return_boolean=03090_E_比较运算符必须返回布尔值
% 当重载 \var{=} 运算符时，函数必须返回布尔值。
parser_e_only_virtual_methods_abstract=03091_E_只有虚方法可以是抽象的
% 您正在将一个未声明为虚方法的方法声明为抽象。
parser_f_unsupported_feature=03092_F_使用了不支持的功能："$1"。
% 您正在尝试强制编译器执行它尚不能执行的操作。
parser_e_mix_of_classes_and_objects=03093_E_不允许混合使用不同类型的对象（类、对象、接口等）
% 您不能交错派生 \var{objects}、\var{classes}、\var{cppclasses} 和 \var{interfaces}。
% 例如，类不能有对象作为父类，反之亦然。
parser_w_unknown_proc_directive_ignored=03094_W_未知的过程指令被忽略："$1"
% 您指定的过程指令未知。
parser_e_directive_only_one_var=03095_E_$1 只能与一个变量关联
% 您不能在 \var{absolute}、\var{export}、\var{external}、\var{weakexternal}、
% \var{public} 和 \var{cvar} 指令前指定多个变量。
% 因此，例如以下结构将提供此错误：
% \begin{verbatim}
% Var Z : Longint;
%     X,Y : Longint absolute Z;
% \end{verbatim}
parser_e_absolute_only_to_var_or_const=03096_E_absolute 只能与表示地址的变量或常量关联
% \var{absolute} 指令的地址只能指向变量或常量。因此，以下代码将产生此错误：
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
parser_e_initialized_only_one_var=03097_E_只能初始化一个变量
% 您不能指定多个带有初始值的变量。
parser_e_abstract_no_definition=03098_E_抽象方法不应有任何定义（带函数体）
% 抽象方法只能声明，不能实现。它们应该由后代类重写。
parser_e_overloaded_must_be_all_global=03099_E_此重载函数不能是局部的（必须导出）
% 您正在单元的实现部分定义一个重载函数，但在单元的接口部分没有相应的声明。
parser_w_virtual_without_constructor=03100_W_在 "$1" 中使用了虚方法但没有构造函数
% 如果您声明包含虚方法的对象或类，您需要有一个构造函数和析构函数来初始化它们。
% 编译器遇到了一个带有虚方法但没有构造函数/析构函数对的对象或类。
parser_c_macro_defined=03101_CL_宏已定义：$1
% 当使用 \var{-vc} 时，编译器会告诉您它何时定义宏。
parser_c_macro_undefined=03102_CL_宏已取消定义：$1
% 当使用 \var{-vc} 时，编译器会告诉您它何时取消定义宏。
parser_c_macro_set_to=03103_CL_宏 $1 设置为 $2
% 当使用 \var{-vc} 时，编译器会告诉您宏获得了什么值。
parser_i_compiling=03104_I_正在编译 $1
% 当您打开信息消息（\var{-vi}）时，编译器会告诉您它正在重新编译哪些单元。
parser_u_parsing_interface=03105_UL_正在解析单元 $1 的接口
% 这告诉您当前单元的接口读取已开始
parser_u_parsing_implementation=03106_UL_正在解析 $1 的实现
% 这告诉您当前单元、库或程序的代码读取开始
parser_d_compiling_second_time=03107_DL_正在第二次编译 $1
% 当您请求调试消息（\var{-vd}）时，编译器会告诉您它重新编译了哪些单元。
parser_e_no_property_found_to_override=03109_E_找不到要重写的属性
% 您想要重写父类的属性，但实际上父类中没有这样的属性。
parser_e_only_one_default_property=03110_E_只允许一个默认属性
% 您将一个属性指定为 \var{Default}，但该类已经有一个默认属性，而一个类只能有一个默认属性。
parser_e_property_need_paras=03111_E_默认属性必须是数组属性
% 只有类的数组属性可以设为 \var{default} 属性。
parser_e_constructor_cannot_be_not_virtual=03112_E_虚构造函数只在类对象模型中支持
% 您不能在对象中使用虚构造函数。您只能在类中使用它们。
parser_e_no_default_property_available=03113_E_没有可用的默认属性
% 您正在尝试访问类的默认属性，但这个类（或其祖先之一）没有默认属性。
parser_e_cant_have_published=03114_E_该类不能有 published 部分，请使用 {$M+} 开关
% 如果您想在类定义中使用 \var{published} 部分，您必须使用 \var{\{\$M+\}} 开关，
% 它会打开类型信息的生成。
parser_e_forward_declaration_must_be_resolved=03115_E_类 "$1" 的前向声明必须在此处解析才能将该类用作祖先
% 要能够使用对象作为祖先对象，必须首先定义它。在以下情况下会出现此错误：
% \begin{verbatim}
%  Type ParentClas = Class;
%       ChildClass = Class(ParentClass)
%         ...
%       end;
% \end{verbatim}
% 其中 \var{ParentClass} 已声明但未定义。
parser_e_no_local_operator=03116_E_不支持局部运算符
% 您不能在局部重载，即在过程或函数定义内部。
parser_e_proc_dir_not_allowed_in_interface=03117_E_过程指令 "$1" 不允许在接口部分使用
% 此过程指令不允许在单元的 \var{interface} 部分使用。您只能在 \var{implementation} 部分使用它。
parser_e_proc_dir_not_allowed_in_implementation=03118_E_过程指令 "$1" 不允许在实现部分使用
% 此过程指令不允许在单元的 \var{implementation} 部分使用。您只能在 \var{interface} 部分使用它。
parser_e_proc_dir_not_allowed_in_procvar=03119_E_过程指令 "$1" 不允许在过程变量声明中使用
% 此过程指令不能作为过程或函数类型声明的一部分。
parser_e_function_already_declared_public_forward=03120_E_函数已被声明为 Public/Forward "$1"
% 如果一个函数被定义为 \var{forward} 两次，您将得到此错误。
% 或者如果它出现在 \var{interface} 部分，并再次作为 \var{forward} 声明出现在
% \var{implementation} 部分。
parser_e_not_external_and_export=03121_E_不能同时使用 EXPORT 和 EXTERNAL
% 这两个过程指令是互斥的。
parser_n_not_supported_for_inline=03123_N_"$1" 在内联过程/函数内部尚不支持
% 内联过程不支持此声明。
parser_h_inlining_disabled=03124_H_内联已禁用
% 过程的内联已禁用。
parser_i_writing_browser_log=03125_I_正在写入浏览器日志 $1
% 当信息消息开启时，编译器会在写入浏览器日志（使用 \var{\{\$Y+ \}} 开关生成）时警告您。
parser_h_maybe_deref_caret_missing=03126_H_可能缺少指针解引用？
% 编译器认为可能需要指针解引用。
parser_f_assembler_reader_not_supported=03127_F_不支持所选的汇编器读取器
% 所选的汇编器读取器（使用 \var{\{\$ASMMODE xxx\}}）不受支持。
% 编译器可以编译时带有或不带有特定汇编器读取器的支持。
parser_e_proc_dir_conflict=03128_E_过程指令 "$1" 不能与 $2 一起使用
% 您指定了一个与其他指令冲突的过程指令。
% 例如 \var{cdecl} 和 \var{pascal} 是互斥的。
parser_e_call_convention_dont_match_forward=03129_E_调用约定与前向声明不匹配
% 当您声明一个函数或过程时使用了例如 \var{cdecl;}，但在实现中省略了这个指令，
% 或反之，就会发生此错误。调用约定是函数声明的一部分，
% 必须在函数定义中重复。
parser_e_property_cant_have_a_default_value=03131_E_属性不能有默认值
% 集合属性或索引属性不能有默认值。
parser_e_property_default_value_must_const=03132_E_属性的默认值必须是常量
% \var{default} 声明的属性的值必须在编译时已知。您指定的值只在运行时才知道。
% 例如，如果您指定变量名作为默认值，就会发生这种情况。
parser_e_cant_publish_that=03133_E_符号不能发布，只能是类
% 如果类变量不是声明为属性，则只有类类型变量可以在类的 \var{published} 部分中。
parser_e_cant_publish_that_property=03134_E_这种类型的属性不能发布
% \var{published} 部分中的属性不能是数组属性。它们必须移到 public 部分。
% \var{published} 部分中的属性必须是序数类型、实数类型、字符串或集合。
parser_e_empty_import_name=03136_E_需要导入名称
% 某些目标需要导入过程的名称或 \var{cdecl} 说明符。
parser_e_division_by_zero=03138_E_除以零
% 遇到了除以零的操作。
parser_e_invalid_float_operation=03139_E_无效的浮点操作
% 两个实数类型值的操作产生了溢出或除以零。
parser_e_array_lower_less_than_upper_bound=03140_E_范围的上界小于下界
% 数组声明的上界小于下界，这是不可能的。
parser_w_string_too_long=03141_W_字符串 "$1" 长于 "$2"
% 常量字符串的大小大于您在字符串类型定义中指定的大小。
parser_e_string_larger_array=03142_E_字符串长度大于字符数组长度
% 常量字符串的大小大于您在 \var{Array[x..y] of char} 定义中指定的大小。
parser_e_ill_msg_expr=03143_E_消息指令后的表达式非法
% \fpc 只支持整数或字符串值作为消息常量。
parser_e_ill_msg_param=03144_E_消息处理程序只能接受一个引用调用参数
% 使用 \var{message} 指令声明为消息处理程序的方法只能接受一个必须声明为引用调用的参数。
% 参数使用 \var{var} 指令声明为引用调用。
parser_e_duplicate_message_label=03145_E_重复的消息标签："$1"
% 一个消息的标签在一个对象/类中使用了两次。
parser_e_self_in_non_message_handler=03146_E_Self 只能在消息处理程序方法中作为显式参数
% \var{Self} 参数只能显式传递给声明为消息处理程序的方法。
parser_e_threadvars_only_sg=03147_E_线程变量只能是静态或全局的
% 线程变量必须是静态或全局的；您不能声明一个过程局部的线程。
% 局部变量总是线程局部的，因为每个线程都有自己的栈，而局部变量
% 存储在栈上。
parser_f_direct_assembler_not_allowed=03148_F_二进制输出格式不支持直接汇编
% 使用二进制写入器时不能使用直接汇编。选择其他输出格式或使用其他汇编器读取器。
parser_w_no_objpas_use_mode=03149_W_不要手动加载 OBJPAS 单元，请使用 {$mode objfpc} 或 {$mode delphi}
% 您正在尝试从 \var{uses} 子句手动加载 \file{ObjPas} 单元。
% 这不是一个好主意。使用 \var{\{\$MODE OBJFPC\}} 或
% \var{\{\$mode delphi\}} 指令，它们会自动加载单元。
parser_e_no_object_override=03150_E_OVERRIDE 不能在对象中使用
% \var{Override} 不支持对象，请使用 \var{virtual} 来重写父对象的方法。
parser_e_cant_use_inittable_here=03151_E_需要初始化/终结化的数据类型不能在变体记录中使用
% 某些数据类型（例如 \var{ansistring}）需要编译器隐式生成的初始化/终结化代码。
% 这样的数据类型不能在记录的变体部分中使用。
parser_e_resourcestring_only_sg=03152_E_资源字符串只能是静态或全局的
% 资源字符串不能声明为局部的，只能是全局的或使用 static 指令。
parser_e_exit_with_argument_not__possible=03153_E_此处不能使用带参数的 exit
% 带有返回值参数的 exit 语句不能在此处使用。这可能发生在例如
% \var{try..except} 或 \var{try..finally} 块中。
parser_e_stored_property_must_be_boolean=03154_E_存储符号的类型必须是布尔型
% 如果您在属性声明中指定存储符号，它必须是布尔类型。
parser_e_ill_property_storage_sym=03155_E_此符号不允许作为存储符号
% 您不能使用这种类型的符号作为属性声明中的存储说明符。您只能使用
% 结果类型为布尔型的方法、布尔类字段或布尔常量。
parser_e_only_publishable_classes_can_be_published=03156_E_只有在 $M+ 模式下编译的类才能发布
% 类的 published 部分中的类型字段只能是在 \var{\{\$M+\}} 下编译的类
% 或从这样的类派生的类。通常这样的类应该从 \var{TPersistent} 派生。
parser_e_proc_directive_expected=03157_E_需要过程指令
% 当您有一个 \var{\{\$Calling\}} 指令但没有指定调用约定时，会触发此错误。
% 当在 const 块中声明过程并且在必须跟随过程指令的过程声明后使用了分号时，
% 也会发生这种情况。
% 正确的声明是：
% \begin{verbatim}
% const
%   p : procedure;stdcall=nil;
%   p : procedure stdcall=nil;
% \end{verbatim}
parser_e_invalid_property_index_value=03158_E_属性索引的值必须是序数类型
% 用于索引属性的值必须是序数类型，例如整数或枚举类型。
parser_e_procname_to_short_for_export=03159_E_导出的过程名称太短
% 过程/函数名称必须至少有2个字符长。这是因为 dlltool 在处理长度为1的名称时无法正确解析 .def 文件。
parser_e_dlltool_unit_var_problem=03160_E_无法为单元全局变量生成 DEFFILE 条目
parser_e_dlltool_unit_var_problem2=03161_E_编译时不要使用 -WD 选项
% 您需要在命令行中不使用 -WD 开关来编译此文件。
parser_f_need_objfpc_or_delphi_mode=03162_F_编译此模块需要 ObjFpc (-S2) 或 Delphi (-Sd) 模式
% 您需要使用 \var{\{\$MODE OBJFPC\}} 或 \var{\{\$MODE DELPHI\}} 来编译此文件。
% 或者使用相应的命令行开关，\var{-Mobjfpc} 或 \var{-MDelphi}。
parser_e_no_export_with_index_for_target=03163_E_在 $1 下不能使用索引导出
% 在此目标平台上不支持使用指定索引导出函数或过程。
parser_e_no_export_of_variables_for_target=03164_E_在 $1 下不支持导出变量
% 在此目标平台上不支持导出变量。
parser_e_improper_guid_syntax=03165_E_GUID 语法不正确
% GUID 指示不具有正确的语法。它应该是以下形式：
% \begin{verbatim}
% {XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}
% \end{verbatim}
% 其中每个 \var{X} 代表一个十六进制数字。
parser_w_interface_mapping_notfound=03168_W_找不到名为"$1"的过程来实现 $2.$3
% 编译器找不到合适的过程来实现给定接口的方法。
% 找到了一个同名的过程，但参数不匹配。
parser_e_interface_id_expected=03169_E_需要接口标识符
% 当编译器扫描包含接口函数名称映射代码的 \var{class} 声明时会发生这种情况，如：
% \begin{verbatim}
% type
%   TMyObject = class(TObject, IDispatch)
%     function IUnknown.QueryInterface=MyQueryInterface;
%     ....
% \end{verbatim}
% 而点号前的 \var{interface} 未在继承列表中列出。
parser_e_type_cant_be_used_in_array_index=03170_E_类型"$1"不能用作数组索引类型
% 像 \var{qword} 或 \var{int64} 这样的类型不允许作为数组索引类型。
parser_e_no_con_des_in_interfaces=03171_E_接口中不允许构造函数和析构函数
% 接口中不允许构造函数和析构函数声明。
% 在大多数情况下，可以使用 \var{IUnknown} 的 \var{QueryInterface} 方法来创建新接口。
parser_e_no_access_specifier_in_interfaces=03172_E_访问说明符不能在接口、Objective-C 协议和类别中使用
% 访问说明符 \var{public}、\var{private}、\var{protected} 和 \var{published} 不能在接口、Objective-C 协议和类别中使用，
% 因为接口/协议/类别的所有方法必须是公开的。
parser_e_no_vars_in_interfaces=03173_E_接口、助手、Objective-C 协议或类别不能包含字段
% 接口、助手和 Objective-C 协议和类别中不允许声明字段。
% 接口/助手/协议/类别只能包含方法和带有方法读/写说明符的属性。
parser_e_no_local_proc_external=03174_E_不能将局部过程声明为 EXTERNAL
% 不能将局部过程声明为外部的。局部过程会获得隐藏参数，这会使错误的可能性非常高。
parser_w_skipped_fields_before=03175_W_在"$1"之前的一些字段未初始化
% 在 Delphi 模式下，不是所有类型常量记录的字段都必须初始化，但编译器会在检测到这种情况时发出警告。
parser_e_skipped_fields_before=03176_E_在"$1"之前的一些字段未初始化
% 在除 Delphi 模式之外的所有语法模式中，不能在类型常量记录中间留下未初始化的字段。
parser_w_skipped_fields_after=03177_W_在"$1"之后的一些字段未初始化
% 您可以在类型常量记录末尾留下一些未初始化的字段（编译器会自动将它们初始化为零）。这可能会导致一些微妙的问题。
parser_e_varargs_need_cdecl_and_external=03178_E_VarArgs 指令(或 MacPas 中的'...')需要 CDecl/CPPDecl/MWPascal/StdCall 和 External
% varargs 指令(或 MacPas 模式中的"..."可变参数)只能用于使用 \var{external} 声明并带有 \var{cdecl}、\var{cppdecl}、
% \var{stdcall} 和 \var{mwpascal} 之一的过程或函数。此功能仅支持为像 printf 这样的 C 函数提供兼容接口。
parser_e_self_call_by_value=03179_E_Self 必须是普通(按值调用)参数
% 您不能将 \var{Self} 声明为 const 或 var 参数，它必须始终是按值调用参数。
parser_e_interface_has_no_guid=03180_E_接口"$1"没有接口标识
% 当您想要将接口赋值给常量时，接口必须设置 GUID 值。
parser_e_illegal_field_or_method=03181_E_未知的类字段或方法标识符"$1"
% 属性必须引用同一类中的字段或方法。
parser_w_proc_overriding_calling=03182_W_调用约定"$1"被"$2"覆盖
% 过程声明中有两个指定调用约定的指令。只会使用最后一个指令。
parser_e_no_procvarobj_const=03183_E_"procedure of object"类型的类型常量需要在编译时知道 Self 指针
% 为了用方法初始化方法指针，在运行时调用该方法的 \var{Self} 指针的值必须在编译时已知。
% 因此，方法指针只能用 \var{Nil} 初始化，或者用通过类类型或类引用类型访问的类方法初始化。
parser_e_default_value_only_one_para=03184_E_默认值只能分配给一个参数
% 不可能一次为多个参数指定默认值。
% 以下是无效的：
% \begin{verbatim}
% Procedure MyProcedure (A,B : Integer = 0);
% \end{verbatim}
% 相反，应该这样声明：
% \begin{verbatim}
% Procedure MyProcedure (A : Integer = 0; B : Integer = 0);
% \end{verbatim}
parser_e_default_value_expected_for_para=03185_E_参数"$1"需要默认值
% 指定的参数需要一个默认值。
parser_w_unsupported_feature=03186_W_使用了不支持的功能！
% 您正在尝试强制编译器做一些它还不能做的事情。
parser_h_c_arrays_are_references=03187_H_C 数组按引用传递
% 传递给 C 函数的任何数组都是通过指针(即按引用)传递的。
parser_e_C_array_of_const_must_be_last=03188_E_C 的 array of const 必须是最后一个参数
% 对于 \var{cdecl} 函数，您不能在 \var{array of const} 参数之后添加任何其他参数，
% 因为此参数在栈上推送的大小是未知的。
parser_h_type_redef=03189_H_类型"$1"重定义
% 这表明先前声明的类型正在被重新定义为其他内容。这可能会也可能不会成为错误的潜在来源。
parser_w_cdecl_has_no_high=03190_W_cdecl 声明的函数没有 high 参数
% 使用 \var{cdecl} 修饰符声明的函数不传递额外的隐式参数。
parser_w_cdecl_no_openstring=03191_W_cdecl 声明的函数不支持开放字符串
% 对于具有 \var{cdecl} 修饰符的函数，不支持开放字符串。
parser_e_initialized_not_for_threadvar=03192_E_不能初始化声明为 threadvar 的变量
% 声明为 threadvar 的变量不能用默认值初始化。
% 变量在新线程开始时总是会被填充为零。
parser_e_msg_only_for_classes=03193_E_message 指令只允许在类中使用
% message 指令仅支持类类型。
parser_e_procedure_or_function_expected=03194_E_需要过程或函数
% 类方法只能为过程和函数指定。
parser_e_illegal_calling_convention=03195_W_调用约定指令被忽略："$1"
% 某些调用约定仅受某些 CPU 支持。例如，大多数非 i386 移植版本仅支持 CPU 的标准 ABI 调用约定。
parser_e_no_object_reintroduce=03196_E_REINTRODUCE 不能在对象中使用
% \var{reintroduce} 不支持对象、Objective-C 类和 Objective-C 协议。
parser_e_paraloc_only_one_para=03197_E_每个参数必须有自己的位置
% 如果按照某些系统调用约定的要求显式指定参数位置，每个参数必须有自己的位置。像
% \begin{verbatim}
% procedure p(i,j : longint 'r1');
% \end{verbatim}
% 这样的代码是不允许的。
parser_e_paraloc_all_paras=03198_E_每个参数必须有显式位置
% 如果一个参数有显式参数位置，过程的所有参数都必须有一个。
parser_e_illegal_explicit_paraloc=03199_E_指定的显式参数位置无效
% 系统调用特定：为此参数指定的显式位置字符串无法解析、无效，或编译器无法识别为参数指定的位置。
parser_e_32bitint_or_pointer_variable_expected=03200_E_需要32位整数或指针变量
% MorphOS/AmigaOS 的 libbase 只能作为 \var{longint}、\var{dword} 或任何指针变量给出。
parser_e_goto_outside_proc=03201_E_不允许在不同过程之间使用 goto 语句
% 不允许使用引用当前过程外部标签的 \var{goto} 语句。以下示例说明了这个问题：
% \begin{verbatim}
% ...
%   procedure p1;
%   label
%     l1;
%
%     procedure p2;
%     begin
%       goto l1; // 这个 goto 是不允许的
%     end;
%
%   begin
%     p2
%   l1:
%   end;
% ...
%
% \end{verbatim}
parser_f_too_complex_proc=03202_F_过程太复杂，需要太多寄存器
% 您的过程体对编译器来说太长了。您应该将过程拆分成多个较小的过程。
parser_e_illegal_expression=03203_E_非法表达式
% 这可能在许多情况下发生。通常是在尝试计算常量表达式时。
parser_e_invalid_integer=03204_E_无效的整数表达式
% 您创建了一个不是整数的表达式，而编译器期望结果是整数。
parser_e_invalid_qualifier=03205_E_非法限定符
% 发生以下情况之一：
% \begin{itemize}
% \item 您试图访问不是记录的变量的字段。
% \item 您正在索引不是数组的变量。
% \item 您正在解引用不是指针的变量。
% \end{itemize}
parser_e_upper_lower_than_lower=03206_E_上限小于下限
% 您正在声明一个子范围，而上限小于范围的下限。
parser_e_macpas_exit_wrong_param=03207_E_Exit 的参数必须是它所在的过程名称或外围过程名称
% macpas 模式下 exit 调用的参数必须是当前子程序的名称或外围子程序的名称
parser_e_illegal_assignment_to_count_var=03208_E_不能对 for 循环变量"$1"赋值
% \var{for} 循环变量的类型必须是序数类型。
% 循环变量不能是实数或字符串。您也不能在循环内部为循环变量赋值(除了在 Delphi 和 TP 模式下)。
% 如果您需要这样做，请使用 while 或 repeat 循环，因为这些结构是为此目的而设计的。
parser_e_no_local_var_external=03209_E_不能将局部变量声明为 EXTERNAL
% 不允许将局部变量声明为外部的。只有全局变量可以引用外部变量。
parser_e_proc_already_external=03210_E_过程已经声明为 EXTERNAL
% 该过程已经在接口或前向声明中使用 EXTERNAL 指令声明。
parser_w_implicit_uses_of_variants_unit=03211_W_隐式使用 Variants 单元
% 在单元中使用了 Variant 类型，但没有任何使用的单元使用 Variants 单元。编译器已经隐式地将 Variants 单元添加到 uses 列表中。
% 要删除此警告，需要将 Variants 单元添加到 uses 语句中。
parser_e_no_static_method_in_interfaces=03212_E_类和静态方法不能在接口中使用
% \var{class} 说明符和 \var{static} 指令不能在接口中使用，因为接口的所有方法必须是公开的。
parser_e_arithmetic_operation_overflow=03213_E_算术运算溢出
% 两个整数值的运算产生了溢出。
parser_e_protected_or_private_expected=03214_E_需要 protected 或 private
% \var{strict} 只能与 \var{protected} 或 \var{private} 一起使用。
parser_e_illegal_slice=03215_E_SLICE 不能在参数列表外使用
% \var{slice} 只能用于接受开放数组参数的参数。
parser_e_dispinterface_cant_have_parent=03216_E_DISPINTERFACE 不能有父类
% DISPINTERFACE 是一种特殊类型的接口，不能有父类。Dispinterface 总是从 IDispatch 类型派生。
parser_e_dispinterface_needs_a_guid=03217_E_DISPINTERFACE 需要一个 guid
% DISPINTERFACE 总是需要一个接口标识(GUID)。
parser_w_overridden_methods_not_same_ret=03218_W_重写的方法必须有相关的返回类型。此代码可能会崩溃，这取决于 Delphi 解析器错误("$2"被"$1"重写，后者有不同的返回类型)
% 如果您在类定义中声明重写的方法，它们必须具有相同的返回类型。某些版本的 Delphi 允许您更改接口方法的返回类型，
% 甚至允许将过程更改为函数，但根据使用的类型和方法调用的方式，生成的代码可能会崩溃。
parser_e_dispid_must_be_ord_const=03219_E_调度 ID 必须是序数常量
% \var{dispid} 关键字后面必须跟一个序数常量(dispid 索引)。
parser_e_array_range_out_of_bounds=03220_E_数组的范围太大
% 无论其元素占用的大小如何，数组的元素数量不能超过 high(ptrint)。此外，范围类型必须是 ptrint 的子范围。
parser_e_packed_element_no_var_addr=03221_E_不能获取位打包数组元素和记录字段的地址
% 如果您在 Mac Pascal 模式下将数组或记录声明为 \var{packed}(或在任何模式下使用 \var{\{\$bitpacking on\}} 声明为 \var{packed})，
% 它将在位级别打包。这意味着无法获取单个数组元素或记录字段的地址。此规则的唯一例外是打包大小是 8 位倍数的打包数组元素的情况。
parser_e_packed_dynamic_open_array=03222_E_动态数组不能打包
% 只有常规数组(以及可能在将来的开放数组)可以打包。
parser_e_packed_element_no_loop=03223_E_位打包数组元素和记录字段不能用作循环变量
% 如果您在 Mac Pascal 模式下将数组或记录声明为 \var{packed}(或在任何模式下使用 \var{\{\$bitpacking on\}} 声明为 \var{packed})，
% 它将在位级别打包。出于性能原因，它们不能用作循环变量。
parser_e_type_var_const_only_in_records_and_classes=03224_E_VAR、TYPE 和 CONST 只允许在记录、对象和类中使用
% 在对象内部使用 VAR、TYPE 和 CONST 声明新类型的用法只允许在记录、对象和类内部。
parser_e_cant_create_generics_of_this_type=03225_E_此类型不能作为泛型
% 只有类、对象、接口和记录允许用作泛型。
parser_w_no_lineinfo_use_switch=03226_W_不要手动加载 LINEINFO 单元，请使用 -gl 编译器开关
% 不要直接使用 \file{lineinfo} 单元，使用 \var{-gl} 开关，它会自动添加正确的单元来读取所选类型的调试信息。
% 需要使用的单元取决于编译二进制文件时使用的调试信息类型。
parser_e_no_funcret_specified=03227_E_未为函数"$1"指定函数返回类型
% 第一次声明函数时，您必须完整声明它，包括所有参数和返回类型。
parser_e_special_onlygenerics=03228_E_特化只支持泛型类型
% 非泛型类型不能被特化。
parser_e_no_generics_as_params=03229_E_特化泛型时不能使用泛型作为参数
% 特化泛型时，只能使用非泛型类型作为参数。
parser_e_type_object_constants=03230_E_不允许包含 VMT 的对象常量
% 如果对象因为包含构造函数或虚拟方法而需要 VMT，则不允许创建该对象的常量。在 TP 和 Delphi 模式下，出于兼容性原因允许这样做。
parser_e_label_outside_proc=03231_E_不允许获取在当前作用域外定义的标签的地址
% 不允许获取当前过程外部标签的地址。
parser_e_initialized_not_for_external=03233_E_不能初始化声明为 external 的变量
% 声明为 external 的变量不能用默认值初始化。
parser_e_illegal_function_result=03234_E_非法的函数返回类型
% 某些类型如文件类型不能用作函数结果。
parser_e_no_common_type=03235_E_"$1"和"$2"之间没有公共类型
% 要对整数执行操作，编译器将两个操作数转换为它们的公共类型，这似乎是一个无效类型。要确定操作数的公共类型，
% 编译器取两种类型的最小值的最小值，和最大值的最大值。然后公共类型是 minimum..maximum。
parser_e_no_generics_as_types=03236_E_未特化的泛型不能用作变量的类型
% 泛型在用作变量类型之前必须始终进行特化。
parser_w_register_list_ignored=03237_W_纯汇编例程的寄存器列表被忽略
% 使用纯汇编例程时，修改的寄存器列表被忽略。
parser_e_implements_must_be_class_or_interface=03238_E_implements 属性必须具有类或接口类型
% 实现接口的属性必须是类或接口类型。
parser_e_implements_must_have_correct_type=03239_E_implements 属性必须实现正确类型的接口，找到"$1"但期望"$2"
% 实现接口的属性实际上实现了一个不同的接口。
parser_e_implements_must_read_specifier=03240_E_implements 属性必须有读取说明符
% 实现接口的属性必须至少有一个读取说明符。
parser_e_implements_must_not_have_write_specifier=03241_E_implements 属性不能有写入说明符
% 实现接口的属性不能有写入说明符。
parser_e_implements_must_not_have_stored_specifier=03242_E_implements 属性不能有存储说明符
% 实现接口的属性不能有存储说明符。
parser_e_implements_uses_non_implemented_interface=03243_E_implements 属性用于未实现的接口："$1"
% 由属性实现的接口不是类实现的接口。
parser_e_unsupported_real=03244_E_此目标不支持浮点数
% 编译器解析了一个浮点表达式，但它不受支持。
parser_e_class_doesnt_implement_interface=03245_E_类"$1"没有实现接口"$2"
% implements 子句中给出的类没有实现委托的接口。
parser_e_class_implements_must_be_interface=03246_E_implements 使用的类型必须是接口
% \var{implements} 关键字后面必须跟一个接口类型。
parser_e_cant_export_var_different_name=03247_E_在此目标上不能用不同的名称导出变量，请使用"export"指令在声明中添加名称(变量名称：$1，声明的导出名称：$2)
% 在大多数目标平台上，不可能在库的 \var{exports} 语句中更改变量导出的名称。
% 在这种情况下，您必须在声明变量的地方使用 \var{export} 和 \var{alias} 指令指定导出名称。
parser_e_weak_external_not_supported=03248_E_当前目标不支持弱外部符号
% "弱外部"符号是一个在(静态或动态)链接时可能存在也可能不存在的符号。这个概念可能在当前 cpu/OS 目标上不可用(或尚未实现)。
parser_e_forward_mismatch=03249_E_前向类型定义不匹配
% 前向声明的类和接口在实现时必须具有相同的类型。前向接口不能更改为类。
parser_n_ignore_lower_visibility=03250_N_虚拟方法"$1"的可见性($2)低于父类 $3($4)
% 虚拟方法重写了一个声明为更高可见性的方法。这可能会产生意外结果。例如，如果新的可见性是私有的，
% 那么在新的子类中调用"inherited"将调用父类中更高可见性的方法，并忽略私有方法。
parser_e_field_not_allowed_here=03251_E_字段不能出现在方法或属性定义之后，请先开始一个新的可见性部分
% 一旦在类或对象中定义了方法或属性，就不能在不开始新的可见性部分(如 \var{public}、\var{private} 等)的情况下定义任何字段。
% 原因是否则源代码对编译器来说可能会显得模糊，因为也可以使用像 \var{default} 和 \var{register} 这样的修饰符作为字段名。
parser_e_no_local_para_def=03252_E_参数或结果类型不能包含局部类型定义。请在类型块中使用单独的类型定义。
% 在 Pascal 中，类型不会仅仅因为语义等价就被认为是相同的。
% 只有当两个变量或参数引用相同的类型定义时，它们才被认为是相同类型。
% 因此，不允许在参数列表中定义新类型，因为这样就不可能在单元的接口和实现的过程头中引用相同的类型定义
% (两个过程头会定义一个单独的类型)。请记住，像"file of byte"或"string[50]"这样的表达式也定义了一个新类型。
parser_e_abstract_and_sealed_conflict=03253_E_ABSTRACT 和 SEALED 冲突
% ABSTRACT 和 SEALED 不能在一个声明中一起使用
parser_e_sealed_descendant=03254_E_不能创建密封类"$1"的后代
% 密封意味着该类不能被另一个类派生。
parser_e_sealed_class_cannot_have_abstract_methods=03255_E_SEALED 类不能有 ABSTRACT 方法
% 密封意味着该类不能被派生。因此没有类能够在密封类中重写抽象方法。
parser_e_only_virtual_methods_final=03256_E_只有虚拟方法可以是 final
% 你正在将一个方法声明为 final，但它没有声明为虚拟方法。
parser_e_final_can_no_be_overridden=03257_E_Final 方法不能被重写："$1"
% 你正在尝试 \var{override} 一个父类中不存在的虚拟方法。
parser_e_multiple_messages=03258_E_每个方法只能使用一个消息
% 不可能将多个消息与单个方法关联。
parser_e_invalid_enumerator_identifier=03259_E_无效的枚举器标识符："$1"
% 只支持"MoveNext"和"Current"枚举器标识符。
parser_e_enumerator_identifier_required=03260_E_需要枚举器标识符
% \var{enumerator} 修饰符后必须跟"MoveNext"或"Current"标识符。
parser_e_enumerator_movenext_is_not_valid=03261_E_枚举器 MoveNext 模式方法无效。方法必须是返回布尔类型且没有必需参数的函数。
% "MoveNext"枚举器模式方法必须是返回布尔类型且没有必需参数的函数
parser_e_enumerator_current_is_not_valid=03262_E_枚举器 Current 模式属性无效。属性必须有一个 getter。
% "Current"枚举器模式属性必须有一个 getter
parser_e_only_one_enumerator_movenext=03263_E_每个类/对象只允许一个枚举器 MoveNext 方法
% 类或对象只能有一个枚举器 MoveNext 声明。
parser_e_only_one_enumerator_current=03264_E_每个类/对象只允许一个枚举器 Current 属性
% 类或对象只能有一个枚举器 Current 声明。
parser_e_for_in_loop_cannot_be_used_for_the_type=03265_E_不能对类型"$1"使用 for in 循环
% for in 循环不能用于所有类型。例如，它不能用于带有跳转的枚举。
parser_e_objc_requires_msgstr=03266_E_Objective-C 消息需要使用"message"指令指定其 Objective-C 选择器名称。
% Objective-C 消息需要使用 \var{message `someName:'} 过程指令指定其 Objective-C 名称(选择器名称)。
% 虽然其他语言的绑定会根据你使用的标识符自动生成这样的名称(通过将所有下划线替换为冒号)，但这是不安全的，
% 因为没有什么能阻止 Objective-C 方法名包含实际的冒号。
parser_e_objc_no_constructor_destructor=03267_E_Objective-C 没有正式的构造函数和析构函数。请使用 alloc、initXXX 和 dealloc 消息。
% Objective-C 语言没有任何构造函数或析构函数。虽然有一些具有类似目的的消息(如 \var{init} 和 \var{dealloc})，
% 但这些不能通过自动解析器识别，也不能保证像 Pascal 构造函数/析构函数那样的功能(例如，你必须注意只调用"指定的"
% 继承的"构造函数")。出于这些原因，我们选择遵循标准的 Objective-C 实例创建/销毁模式。
parser_e_message_string_too_long=03268_E_消息名称太长(最大 255 个字符)
% 由于编译器实现原因，消息名称目前限制为 255 个字符。
parser_e_objc_message_name_too_long=03269_E_"$1"的 Objective-C 消息符号名称太长
% 由于编译器实现原因，修饰后的消息名称(即在汇编代码中使用的符号名称)目前限制为 255 个字符。
parser_h_no_objc_parent=03270_H_定义新的 Objective-C 根类。要从另一个根类(例如 NSObject)派生，请将其指定为父类。
% 如果没有为 Object Pascal 类指定父类，那么它会自动从 TObject 派生。
% 然而，Objective-C 类不会自动从 NSObject 派生，因为在 Objective-C 中可以有多个根类。
% 例如，在 Cocoa 框架中，NSObject 和 NSProxy 都是根类。
% 因此，如果你想让你的 Objective-C 类从它派生，你必须显式定义一个父类(如 NSObject)。
parser_e_no_objc_published=03271_E_Objective-C 类不能有 published 部分。
% 在 Object Pascal 中，"published"决定是否生成 RTTI。由于 Objective-C 运行时总是需要所有内容的 RTTI，
% 这个指定对 Objective-C 类来说没有意义。
parser_f_need_objc=03272_F_此模块需要启用 Objective-C 模式开关才能编译
% 此错误表示在没有激活 Objective-C 模式开关的情况下使用了 Objective-C 语言特性。
% 通过命令行开关 -M 或 {\$modeswitch x} 指令启用它。
parser_e_must_use_override=03273_E_在 Objective-C 和 Java 中继承的方法只能被重写，请添加"override"(继承的方法定义在 $1 中)
parser_h_should_use_override=03274_H_在 Objective-C 和 Java 中继承的方法只能被重写，请添加"override"(继承的方法定义在 $1 中)。
% 在 Objective-C 或 Java 中不能像在 Object Pascal 中那样 \var{reintroduce} 方法。
% 具有相同名称的方法总是映射到相同的虚拟方法条目。为了在源代码中明确这一点，
% 当在 Pascal 中实现重写的 Objective-C 或 Java 方法时，编译器总是要求指定 \var{override} 指令。
% 如果实现是外部的，这个规则会放宽，因为 Objective-C 和 Java 没有任何 \var{override} 风格的关键字
% (因为这是这些语言中的默认和唯一行为)，这使得自动头文件转换工具很难在所有地方包含它。
% 明确提到继承方法定义的类型，因为在 Objective-C 的情况下，这可能是 objcclass 或 objccategory。
parser_e_objc_message_name_changed=03275_E_继承类中的消息名称"$1"与当前类中的消息名称"$2"不同。
% 重写的 Objective-C 方法不能与继承的方法有不同的消息名称。原因是这些消息名称
% 唯一地定义了 Objective-C 运行时的消息，这意味着给它们不同的消息名称会破坏"override"语义。
parser_e_unique_unsupported=03276_E_目前还不能创建 Objective-C 或 Java 类型的唯一副本
% 使用 \var{type x = type y;} 复制 Objective-C 或 Java 类型目前还不支持。你可以
% 使用 \var{type x = objcclass(y) end;} 或 \var{type x = class(y) end;} 来获得所需的效果。
parser_e_no_category_as_types=03277_E_Objective-C 类别和 Object Pascal 类助手不能用作类型
% 不可能将变量声明为 Objective-C 类别或 Object Pascal 类助手的实例。
% 类别/类助手向现有类的作用域添加方法，但本身不定义类型。
% 这个规则的一个例外是当从另一个类助手继承 Object Pascal 类助手时。
parser_e_no_category_override=03278_E_类别不是重写而是替换方法。请使用"reintroduce"。
parser_e_must_use_reintroduce_objc=03279_E_在 Objective-C 中替换的方法只能被重新引入，请添加"reintroduce"(替换的方法定义在 $1 中)。
parser_h_should_use_reintroduce_objc=03280_H_在 Objective-C 中替换的方法只能被重新引入，请添加"reintroduce"(替换的方法定义在 $1 中)。
% 类别替换 Objective-C 类中的现有方法，而不是重写它。
% 从类别方法调用继承的方法将调用扩展类的父类中的该方法，而不是扩展类本身中的方法。
% 原始类中被替换的方法基本上丢失了，不能再被调用或引用。这种行为与 \var{reintroduce}
% 的对应更接近，而不是 \var{override}(尽管在 Object Pascal 中使用 \var{reintroduce}
% 的情况下，隐藏的方法仍然可以通过 inherited 访问)。
% 明确提到继承方法定义的类型，因为在 Objective-C 的情况下，这可能是 objcclass 或 objccategory。
parser_e_implements_getter_not_default_cc=03281_E_实现接口的 getter 必须使用目标的默认调用约定。
% 接口 getter 通过运行时库中的辅助函数调用，因此必须使用目标的默认调用约定
% (在 i386 和 x86_64 上是 \var{register}，在其他架构上是 \var{stdcall})。
parser_e_no_refcounted_typed_file=03282_E_类型化文件不能包含引用计数类型。
% 类型化文件中的数据不能是引用计数类型(如 \var{ansistring} 或包含引用计数字段的记录)。
parser_e_operator_not_overloaded_2=03283_E_运算符未被重载：$2"$1"
% 你正在尝试使用一个重载运算符，但它没有为此类型重载。
parser_e_operator_not_overloaded_3=03284_E_运算符未被重载："$1"$2"$3"
% 你正在尝试使用一个重载运算符，但它没有为此类型重载。
parser_e_more_array_elements_expected=03285_E_期望另外 $1 个数组元素
% 在声明类型化常量数组时，你提供的元素太少，无法初始化数组
parser_e_string_const_too_long=03286_E_在禁用 ansistrings 时字符串常量太长
% 只有当代码在启用 ansistrings (\var{\{\$H+\}}) 的情况下编译时，才允许长度超过 255 个字符的字符串常量。
parser_e_invalid_univ_para=03287_E_类型不能用作 univ 参数，因为其大小在编译时未知："$1"
% \var{univ} 参数与所有相同大小的值兼容，但如果参数的大小在编译时未知，则无法检查这一点。
parser_e_only_one_class_constructor_allowed=03288_E_类中只能声明一个类构造函数："$1"
% 你正在尝试声明多个类构造函数，但只能声明一个类构造函数。
parser_e_only_one_class_destructor_allowed=03289_E_类中只能声明一个类析构函数："$1"
% 你正在尝试声明多个类析构函数，但只能声明一个类析构函数。
parser_e_no_paras_for_class_constructor=03290_E_类构造函数不能有参数
% 你正在声明一个带有参数列表的类构造函数。类构造函数方法不能有参数。
parser_e_no_paras_for_class_destructor=03291_E_类析构函数不能有参数
% 你正在声明一个带有参数列表的类析构函数。类析构函数方法不能有参数。
parser_f_modeswitch_objc_required=03292_F_此构造需要激活 \{\$modeswitch objectivec1\} 模式开关
% 当 \{\$modeswitch ObjectiveC1\} 未激活时，不支持 Objective-Pascal 构造。
parser_e_widestring_to_ansi_compile_time=03293_E_Unicodechar/string 常量不能在编译时转换为 ansi/shortstring
% 在需要在编译时转换为 ansistring 或 shortstring 的常量表达式中，不能使用 unicodechar 和 unicodestring 常量，
% 例如在类型化常量内。原因是编译器无法知道运行时的实际 ansi 编码。
parser_e_objc_enumerator_2_0=03294_E_For-in Objective-Pascal 循环需要激活 \{\$modeswitch ObjectiveC2\}
% Objective-C"快速枚举"支持是在 Objective-C 2.0 中添加的，因此必须激活相应的模式开关才能使用此功能。
% 注意，Objective-C 2.0 程序需要 Mac OS X 10.5 或更高版本。
parser_e_objc_missing_enumeration_defs=03295_E_编译器在 CocoaAll 单元中找不到 NSFastEnumerationProtocol 或 NSFastEnumerationState 类型
% Objective-C for-in 循环(快速枚举)要求编译器能够找到一个名为 CocoaAll 的单元，
% 该单元包含 NSFastEnumerationProtocol 和 NSFastEnumerationState 类型的定义。
% 如果你收到此错误，很可能是编译器找到并加载了另一个 CocoaAll 单元。
parser_e_no_procvarnested_const=03296_E_类型为"procedure is nested"的类型化常量只能用 NIL 和全局过程/函数初始化
% 嵌套过程变量由两个组件组成：要调用的过程/函数的地址(在编译时总是已知的)，
% 以及父帧指针(在编译时从不知道)，以防过程变量包含对嵌套过程/函数的引用。
% 因此，这种类型化常量只能用全局函数/过程初始化，因为它们不需要父帧指针。
parser_f_no_generic_inside_generic=03297_F_不允许在另一个泛型内部声明泛型
% 目前，扫描器一次只支持记录一个令牌缓冲区(由 tscannerfile.startrecordtokens 中的内部错误 200511173 保护)。
% 由于泛型是通过记录令牌实现的，因此不可能在另一个泛型内部声明泛型(类型或方法)。
parser_e_forward_intf_declaration_must_be_resolved=03298_E_在类可以遵循或实现接口之前，必须解析前向声明"$1"
% 在类可以遵循它之前，必须完全定义 Objective-C 协议或 Java 接口。
% 此错误在以下情况下发生(示例为 Objective-C，但对 Java 接口也是一样的)：
% \begin{verbatim}
%  Type MyProtocol = objcprotoocl;
%       ChildClass = Class(NSObject,MyProtocol)
%         ...
%       end;
% \end{verbatim}
% 其中 \var{MyProtocol} 已声明但未定义。
parser_e_no_record_published=03299_E_记录类型不能有 published 段
% published 段只能在类中使用。
parser_e_no_destructor_in_records=03300_E_记录或助手类型中不允许析构函数
% 记录或助手类型中不允许声明析构函数。
parser_e_class_methods_only_static_in_records=03301_E_记录中的类方法必须是静态的
% 记录中不允许声明没有 static 修饰符的类方法。
% 记录没有继承，因此非静态类方法对它们来说没有意义。
parser_e_no_parameterless_constructor_in_records=03302_E_记录或记录/类型助手中不允许无参数构造函数
% 记录或记录/类型助手中不允许声明无参数的构造函数。
parser_e_at_least_one_argument_must_be_of_type=03303_E_结果或至少一个参数必须是类型"$1"
% 要求例程的结果或至少一个参数是指定类型。
% 例如，类运算符要么接受它们所定义的结构化类型的实例，要么返回一个实例。
parser_e_cant_use_type_parameters_here=03304_E_类型参数可能需要初始化/终止化 - 不能在变体记录中使用
% 类型参数可能被特化为需要编译器隐式生成初始化/终止化代码的类型(如 \var{ansistring})。
parser_e_externals_no_section=03305_E_声明为外部的变量不能在自定义段中
% 段指令对声明为外部的变量无效。
parser_e_section_no_locals=03306_E_非静态和非全局变量不能有段指令
% 放在自定义段中的变量总是静态分配的，因此它必须是静态变量或全局变量。
parser_e_not_allowed_in_helper=03307_E_助手类型中不允许"$1"
% 在 ObjFPC 模式下，某些指令和说明符如"virtual"、"dynamic"、"override"在助手类型内部是不允许的
% (在 Delphi 模式下它们被忽略)，因为它们在助手中没有意义。"abstract"在两种模式下都不允许。
parser_e_no_class_constructor_in_helpers=03308_E_助手中不允许类构造函数
% 助手中不允许声明类构造函数。
parser_e_inherited_not_in_record=03309_E_记录中不允许使用"inherited"
% 由于记录不支持继承，因此禁止在记录及记录助手中使用"inherited"(仅在"Delphi"模式下)。
parser_e_no_types_in_local_anonymous_records=03310_E_局部或匿名记录中不允许类型声明
% 带有类型的记录必须全局定义。类型不能在过程或函数中定义的记录或匿名记录中定义。
parser_e_duplicate_implements_clause=03311_E_接口"$1"的重复 implements 子句
% 一个类只能使用"implements"子句将一个接口委托给单个属性。多次委托是错误的。
parser_e_mapping_no_implements=03312_E_接口"$1"不能被"$2"委托，它已经有方法解析
% 方法解析子句将接口的方法映射到当前类的方法。因此当前类必须直接实现该接口。不能进行委托。
parser_e_implements_no_mapping=03313_E_接口"$1"不能有方法解析，"$2"已经委托了它
% 方法解析只能用于直接实现的接口，不能用于通过委托实现的接口。
parser_e_invalid_codepage=03314_E_无效的代码页
% 当声明具有给定代码页的字符串时，有效代码页值的范围限制为 0 到 65535。
parser_e_final_only_const_var=03315_E_在对象类型中只有字段(var 段)和常量可以是 final
% final (类)字段必须在(类)构造函数中分配单个值，之后不能被覆盖。
% final (类型化)常量是只读的。
parser_e_final_only_external=03316_E_final 字段目前只支持外部类
% 在非外部类中支持 final 字段需要在 FPC 中实现完整的数据流分析，
% 而目前它仍然缺乏这一功能。
parser_e_no_typed_const=03317_E_这里不允许类型化常量，只允许形式常量
% Java 接口定义了一个可以定义形式常量的命名空间，
% 但由于它们不定义存储，所以不能在其中定义类型化常量
% (这些基本上与初始化的类字段相同)。
parser_e_java_no_inherited_constructor=03318_E_在 JVM 中构造函数不会自动继承；如果需要，请显式添加调用继承构造函数的构造函数
% Java 不会自动将继承的构造函数添加到子类中，这样它们就可以被隐藏。
% 为了与外部 Java 代码兼容，FPC 也这样做。如果你需要在子类中访问相同的
% 构造函数，请在子类中定义它们并从那里调用继承的构造函数。
parser_d_internal_parser_string=03319_D_正在解析内部生成的代码：$1
% 编译器有时会在内部构造 Pascal 代码，然后将其注入到程序中。
% 这些消息显示这样的代码，以帮助调试其中的错误。
parser_e_feature_unsupported_for_vm=03320_E_此语言功能在托管 VM 目标上不受支持
% 某些语言功能在托管虚拟机目标上不受支持。
parser_e_jvm_invalid_virtual_constructor_call=03321_E_在 JVM 目标上不能在另一个构造函数内部为当前实例调用虚拟构造函数
% JVM 不原生支持虚拟构造函数。不幸的是，我们不知道有什么方法可以
% 模拟它们，使得可以在另一个构造函数内部为当前实例调用虚拟构造函数。
parser_e_method_lower_visibility=03322_E_重写方法"$1"的可见性($2)不能低于父类 $3 中的可见性($4)
% JVM 不允许降低重写方法的可见性。
parser_e_nostackframe_without_assembler=03323_E_过程/函数声明了 NOSTACKFRAME 调用选项但没有 ASSEMBLER
% nostackframe 调用修饰符应该与汇编一起使用。
parser_e_nostackframe_with_locals=03324_E_过程/函数声明了 NOSTACKFRAME 调用选项但局部栈大小为 $1
% 使用 nostackframe 调用修饰符但没有汇编修饰符
% 可能仍然会生成局部栈需求。
parser_e_cannot_generate_property_getter_setter=03325_E_无法生成属性 getter/setter $1，因为其名称与现有标识符 $2 冲突
% 自动生成的 getter/setter 不能与现有标识符同名，
% 因为这可能会改变现有代码的行为。
parser_w_overriding_property_getter_setter=03326_W_自动生成的属性 getter/setter $1 重写了类 $2 中的同名 getter/setter
% 在 JVM 平台上自动生成的属性 getter/setter 是虚拟方法，因为
% JVM 不支持可以在子类中更改的非虚拟方法。这意味着
% 如果子类更改继承的属性定义，该属性的行为可能与本机目标不同，
% 因为即使变量声明为父类型，通过调用虚拟方法也会使用子类的 getter。
% 这与本机目标上的行为或不激活自动生成的 setter/getter 时的行为不同，
% 因为在那种情况下，只有变量的声明类型会影响属性行为。
parser_w_case_difference_auto_property_getter_setter_prefix=03327_W_声明的属性 getter/setter $1 与自动构造的名称 $2 大小写不匹配，不更改声明的名称
% 如果属性指定的 getter/setter 已经符合自动 getter/setter 生成设置指定的命名约定，
% 只是在大小写方面有所不同，编译器将打印警告，因为它不一定能更改该其他声明本身，
% 也不能添加使用正确大小写的声明(它可能与原始声明冲突)。
% 手动更正 getter/setter 的大小写以符合所需的编码规则。
% \var{TChild} 重写
parser_e_no_consts_in_local_anonymous_records=03328_E_局部或匿名记录中不允许常量声明
% 带有常量的记录必须全局定义。常量不能在过程或函数中定义的记录或匿名记录中定义。
parser_e_no_methods_in_local_anonymous_records=03329_E_局部或匿名记录中不允许方法声明
% 带有方法的记录必须全局定义。方法不能在过程或函数中定义的记录或匿名记录中定义。
parser_e_no_properties_in_local_anonymous_records=03330_E_局部或匿名记录中不允许属性声明
% 带有属性的记录必须全局定义。属性不能在过程或函数中定义的记录或匿名记录中定义。
parser_e_no_class_in_local_anonymous_records=03331_E_局部或匿名记录中不允许类成员声明
% 带有类成员的记录必须全局定义。类成员不能在过程或函数中定义的记录或匿名记录中定义。
parser_e_not_allowed_in_record=03332_E_记录中不允许可见性段"$1"
% \var(protected) 和  \var(strict protected) 可见性段只有在继承中才有用。
% 由于记录不支持继承，所以它们是被禁止的。
parser_e_dir_not_allowed=03333_E_这里不允许指令"$1"
% 在给定上下文中不允许此指令。例如，"static"
% 不允许用于实例方法或类运算符。
parser_e_no_assembler_in_generic=03334_E_泛型内部不允许汇编块
% 在泛型内部不允许使用汇编块/例程。
parser_e_property_only_sgr=03335_E_属性只能是静态的、全局的或在结构化类型内部
% 属性不能声明为局部的，只能是全局的，使用 static
% 指令或在结构化类型内部。
parser_e_overloaded_have_same_mangled_name=03336_E_重载例程具有相同的修饰名称
% 某些平台，如 JVM 平台，以规定的方式在例程名称中编码参数，
% 这种编码可能会将不同的 Pascal 类型映射到相同的编码
% (即"修饰")名称。这个错误只能通过删除或更改
% 冲突定义的参数声明或例程名称来解决。
parser_e_default_value_val_const=03337_E_默认值只能为值、const 和 constref 参数指定
% 默认参数值允许你在调用例程时不为此参数指定值，
% 编译器将改为传递指定的默认(常量)值。因此，默认值
% 只能为可以接受常量值的参数指定。
parser_w_ptr_type_ignored=03338_W_指针类型"$1"被忽略
% 指定的指针类型修饰符被忽略，因为它在当前平台上不受支持。
% 例如，当在非 x86 平台上声明远指针时会发生这种情况。
parser_e_global_generic_references_static=03339_E_接口段中的泛型模板引用了实现段中的符号
% 在单元的接口段中声明的泛型不能引用仅属于该单元实现段的符号。
parser_u_already_compiled=03340_UL_单元 $1 已经被编译了。
% 这告诉你递归读取 uses 子句已经触发了当前单元的编译，
% 所以当前编译可以中止。
parser_e_explicit_method_implementation_for_specializations_not_allowed=03341_E_不允许为泛型的特化显式实现方法
% 在泛型中引入的方法必须为泛型实现。不可能只为特化实现它们。
parser_e_no_genfuncs_in_interfaces=03342_E_接口中不允许泛型方法
% 接口中不允许泛型方法，因为没有办法特化合适的实现。
parser_e_genfuncs_cannot_be_virtual=03343_E_泛型方法不能是虚拟的
% 泛型方法不能声明为虚拟的，因为每个特化都需要一个 VMT 条目。
% 然而，这在静态 VMT 中是不可能的。
parser_e_packages_not_supported=03344_E_目标操作系统不支持动态包
% 对指定目标操作系统的动态包支持尚未实现，
% 或者至少未经测试因此被禁用。
parser_e_cannot_use_hardfloat_in_a_softfloat_environment=03345_E_如果生成软浮点代码或启用了 fpu 模拟，则不能使用 HardFloat 指令
% \var{HardFloat} 指令只能在使用支持浮点运算的指令集时使用。
parser_e_invalid_internal_function_index=03346_E_索引 $1 不是有效的内部函数索引
% 为 \var{compilerproc} 指令指定的索引不是编译器认可的索引。
parser_w_operator_overloaded_hidden_3=03347_W_运算符重载被内部运算符隐藏："$1" $2 "$3"
% 为指定的重载定义了运算符重载，但编译器的内部重载优先。
% 这只发生在以前可重载的运算符上(例如动态数组 + 动态数组)，
% 但由于定义了内部运算符而不再可重载，这种行为可以通过模式开关控制
% (对于动态数组，这是模式开关 \var{ArrayOperators})。
parser_e_threadvar_must_be_class=03348_E_类或记录内部的线程变量必须是类变量
% 在类或记录内部启动的 \var{threadvar} 段必须以 \var{class} 为前缀。
parser_e_only_static_members_via_object_type=03349_E_只能通过对象类型引用静态方法和静态变量
% 在以下情况下会出现此错误：
% \begin{verbatim}
% Type
%    TObj = object
%      procedure test;
%    end;
%
% begin
%   TObj.test;
% \end{verbatim}
% \var{test} 不是静态方法，因此不能通过类型调用，只能使用实例调用。
parser_e_callthrough_varargs=03350_E_在此平台上不能将 C 风格的可变参数函数"$1"重新声明为外部函数；请在第一次声明时就声明为外部函数
% 如果一个函数在接口中或作为前向声明正常声明，然后再声明为外部函数，编译器
% 必须生成一个调用外部函数的存根。由于代码生成的限制，在某些平台上
% 无法做到这一点。即使在支持的平台上，这也是相当低效的。
parser_e_unbound_attribute=03351_E_未绑定的自定义属性："$1"
% 定义了一个自定义属性，但没有标识符可以绑定。
parser_e_enumeration_out_of_range=03352_E_枚举符号只能具有 -2^31 到 2^31-1 范围内的值
% 枚举值的大小限制为 4 字节。由于值可以是有符号的，有效值的范围
% 限制为有符号的 32 位值(即 \var{longint})。
parser_w_enumeration_out_of_range=03353_W_枚举符号只能具有 -2^31 到 2^31-1 范围内的值
% 枚举值的大小限制为 4 字节。由于值可以是有符号的，有效值的范围
% 限制为有符号的 32 位值(即 \var{longint})。
parser_e_method_for_type_in_other_unit=03354_E_为在另一个单元中声明的类型"$1"实现方法
% 当试图为最初在不同单元中声明的类型定义方法时会出现此错误。
parser_e_generic_constraints_not_allowed_here=03355_E_此处不允许泛型约束
% 在当前位置不允许指定约束。例如在 delphi 模式下，
% 不能在实现的头部指定约束。
parser_e_location_size_too_small=03356_E_显式位置对于参数来说太小
% AmigaOS/MorphOS 系统调用特定：对于 int64/qword 参数只指定了单个寄存器位置
parser_e_location_size_too_large=03357_E_显式位置大小大于参数所需
% AmigaOS/MorphOS 系统调用特定：对于小于 64 位的参数，指定了一对寄存器
parser_e_location_regpair_only_data=03358_E_显式位置寄存器对只支持数据寄存器
% AmigaOS/MorphOS 系统调用特定：对于 64 位寄存器对，只支持数据寄存器
parser_e_location_regpair_only_consecutive=03359_E_显式位置寄存器对只支持连续寄存器
% MorphOS 系统调用特定：64 位寄存器对只支持连续(例如：d1-d2)寄存器
parser_e_constructurs_cannot_take_type_parameters=03360_E_构造函数不能接受类型参数
% 不允许在构造函数中使用类型参数。
parser_e_raise_with_noreturn_not_allowed=03361_E_在声明为 noreturn 的子程序中不允许 raise
% \var{noreturn} 告诉编译器子程序的激活范围永远不会离开。这包括异常、
% goto 或任何其他方式。虽然编译器无法检测所有这些情况，但有些是显而易见的，编译器会给出错误。
parser_e_section_directive_not_allowed_for_target=03362_E_此目标不允许 section 指令
% 只有某些目标(例如嵌入式和 FreeRTOS)支持 section 指令。
parser_e_absolute_sym_cannot_reference_itself=03363_E_absolute 变量不能引用自身
parser_e_syscall_format_not_support=03364_E_当前目标不支持 syscall 指令的语法
% 在某些目标上，syscall 指令的并非所有语法变体都有意义，因此
% 那些没有意义的语法不受支持
% 不允许像 \var{var i: Integer absolute i;} 这样的声明
parser_w_ignoring_published_property=03365_W_此属性将不会被发布
% published 属性被忽略
parser_e_wasm_ref_types_can_only_be_passed_by_value=03366_E_WebAssembly 引用类型只能按值传递
% WebAssembly 引用类型没有内存表示，只能按值传递。
parser_e_promising_exports_not_supported_on_current_platform=03367_E_声明导出为'promising'是 WebAssembly 特有的，当前平台不支持
% Promising 导出是 WebAssembly 特有的。在其他平台上不允许。
parser_e_suspending_externals_not_supported_on_current_platform=03368_E_声明外部函数为'suspending'是 WebAssembly 特有的，当前平台不支持
% Suspending 外部函数是 WebAssembly 特有的。在其他平台上不允许。
parser_w_widechar_set_reduced=03369_W_将宽字符集缩减为单字节 AnsiChar 集。
% 集合的基本类型最多只能有 255 个元素。宽字符集
% 被缩减为 1 字节字符集。
parser_e_nostringaliasinsystem=03370_E_system 单元中不允许使用'string'别名。请使用 short-、ansi- 或 unicodestring。
% 作为安全措施，system 单元只能使用基本字符串类型，而不能使用
% 依赖于编译单元模式的 string 别名。
parser_e_coperators_off=03371_E_C 风格赋值运算符已关闭
% 默认情况下，C 风格赋值运算符(+=、-=、*=、/=)是关闭的。可以通过命令行
% 参数 -Sc 或在源代码中通过 {\$COPERATORS ON} 打开它们
parser_e_cannot_evaluate_expression_at_compile_time=03372_E_表达式无法在编译时求值
% 某些操作(如对象的大小)无法在编译时确定，
% 因此它不能作为常量表达式的一部分。
parser_e_type_alread_forward=03373_E_重复的前向类型声明：$1
% 类型作为前向声明只能定义一次：
% \var{type myclass = class; myclass = class;} 是不允许的
%
% \end{description}
%
# Type Checking
#
# 04134 is the last used one
#
% \section{类型检查错误}
% 本节列出了执行类型检查时可能发生的所有错误。
% \begin{description}
type_e_mismatch=04000_E_类型不匹配
% 这种情况可能发生在多种场合:
% \begin{itemize}
% \item 你要赋值的变量类型与赋值表达式的类型不同。
% \item 你调用函数或过程时传入的参数与函数或过程定义中的参数类型不兼容。
% \end{itemize}
type_e_incompatible_types=04001_E_不兼容的类型：得到 "$1" 但期望 "$2"
% 这两种类型之间无法进行转换。
% 另一种可能是它们在不同的声明中声明：
% \begin{verbatim}
% Var
%    A1 : Array[1..10] Of Integer;
%    A2 : Array[1..10] Of Integer;
%
% Begin
%    A1:=A2; { 这条语句也会产生这个错误。这是由于
%              Pascal 的严格类型检查导致的 }
% End.
% \end{verbatim}
type_e_not_equal_types=04002_E_类型 "$1" 和 "$2" 不匹配
% 这些类型不相等。
type_e_type_id_expected=04003_E_需要类型标识符
% 该标识符不是类型，或者你忘记提供类型标识符。
type_e_variable_id_expected=04004_E_需要变量标识符
% 当过程(如 \var{Inc} 或 \var{Dec})期望一个变量时，你传入了一个常量。这些函数只能接受变量作为参数。
type_e_integer_expr_expected=04005_E_需要整数表达式，但得到 "$1"
% 编译器期望一个整数类型的表达式，但得到了不同的类型。
type_e_boolean_expr_expected=04006_E_需要布尔表达式，但得到 "$1"
% 表达式必须是布尔类型。它应该返回 \var{True} 或 \var{False}。
type_e_ordinal_expr_expected=04007_E_需要序数表达式
% 表达式必须是序数类型，即最大为 \var{Longint}。
% 例如，当你为 \var{Inc} 或 \var{Dec} 指定的第二个参数不是序数值时，就会发生这种情况。
type_e_pointer_type_expected=04008_E_需要指针类型，但得到 "$1"
% 变量或表达式不是 \var{pointer} 类型。当你向 \var{New}
% 或 \var{Dispose} 传递一个非指针变量时会发生这种情况。
type_e_class_type_expected=04009_E_需要类类型，但得到 "$1"
% 变量或表达式不是 \var{class} 类型。这通常发生在：
% \begin{enumerate}
% \item 类声明中的父类不是一个类。
% \item 异常处理器(\var{On})包含一个不是类的类型标识符。
% \end{enumerate}
type_e_cant_eval_constant_expr=04011_E_无法计算常量表达式
% 当你声明的数组边界无法计算为序数常量时，可能会出现此错误。
type_e_set_element_are_not_comp=04012_E_集合元素不兼容
% 你试图对两个集合进行操作，但集合元素类型不相同。在进行并集操作时，
% 集合的基本类型必须相同。
type_e_set_operation_unknown=04013_E_该操作不支持集合类型
% 某些二元运算符不适用于集合。
% 这包括：\var{div}、\var{mod}、\var{**}、\var{>=} 和 \var{<=}。
% 最后两个运算符将来可能会支持集合类型。
type_w_convert_real_2_comp=04014_W_从浮点类型自动转换到 COMP（一个整数类型）
% 遇到从实数类型到 \var{comp} 的隐式类型转换。由于 \var{comp} 是
% 64 位整数类型，这可能表明存在错误。
type_h_use_div_for_int=04015_H_使用 DIV 来获得整数结果
% 当提示开启时，使用 '/' 运算符进行整数除法将产生此消息，
% 因为结果将是实数类型。
type_e_strict_var_string_violation=04016_E_在 $V+ 模式下字符串类型必须完全匹配
% 在 \var{\{\$V+\}} 模式下编译时，作为参数传递的字符串
% 必须与过程声明的参数类型完全相同。
type_e_succ_and_pred_enums_with_assign_not_possible=04017_E_不能对带赋值的枚举类型使用 Succ 或 Pred
% 如果你声明了一个带有 C 风格赋值的枚举类型，
% 比如：
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% 那么你不能对这个枚举使用 \var{Succ} 或 \var{Pred} 函数。
type_e_cant_read_write_type=04018_E_无法读写此类型的变量
% 你试图从文本类型文件中 \var{read} 或 \var{write} 一个变量，但该文件类型
% 不支持该变量类型。只有整数类型、实数、pchar 和字符串可以从文本文件中
% 读取或写入。布尔值只能写入文本文件。
type_e_no_readln_writeln_for_typed_file=04019_E_不能对类型化文件使用 readln 或 writeln
% \var{readln} 和 \var{writeln} 只能用于文本文件。
type_e_no_read_write_for_untyped_file=04020_E_不能对无类型文件使用 read 或 write
% \var{read} 和 \var{write} 只能用于文本文件或类型化文件。
type_e_typeconflict_in_set=04021_E_集合元素之间存在类型冲突
% 至少有一个集合元素的类型错误，即不是集合的类型。
type_w_maybe_wrong_hi_lo=04022_W_lo/hi(dword/qword) 返回上/下字/双字
% \fpc 支持 \var{longint/dword/int64/qword} 的重载版本 \var{lo/hi}，
% 它返回参数的低/高字/双字。\tp 总是使用 16 位的 \var{lo/hi}，
% 对于 \var{lo} 总是返回位 0..7，对于 \var{hi} 总是返回位 8..15。
% 如果你想要 \tp 的行为，你必须将参数类型转换为 \var{word} 或 \var{integer}。
type_e_integer_or_real_expr_expected=04023_E_需要整数或实数表达式
% \var{str} 的第一个参数必须是实数或整数类型。
type_e_wrong_type_in_array_constructor=04024_E_数组构造器中类型 "$1" 错误
% 你试图在数组构造器中使用一个不允许的类型。
type_e_wrong_parameter_type=04025_E_参数 $1 的类型不兼容：得到 "$2"，期望 "$3"
% 你试图为指定的参数传递一个无效类型。
type_e_no_method_and_procedure_not_compatible=04026_E_方法（变量）和过程（变量）不兼容
% 你不能将方法赋值给过程变量，或将过程赋值给方法指针。
type_e_wrong_math_argument=04027_E_传递给内部数学函数的常量非法
% 传递给 \var{ln} 或 \var{sqrt} 函数的常量参数超出了这些函数的定义域。
type_e_no_addr_of_constant=04028_E_无法获取常量表达式的地址
% 无法获取常量表达式的地址，因为它们不存储在内存中。你可以尝试将其
% 声明为类型化常量。如果你试图将属性传递给 var 参数，也会显示此错误。
type_e_argument_cant_be_assigned=04029_E_参数不能被赋值
% 只有可以出现在赋值语句左侧的表达式才能作为引用调用的参数传递。
%
% 注意：属性可以用在赋值语句的左侧，但是它们不能用作参数。
type_e_cannot_local_proc_to_procvar=04030_E_不能将局部过程/函数赋值给过程变量
% 不允许将局部过程/函数赋值给过程变量，因为局部过程/函数的
% 调用约定不同。你只能将局部过程/函数赋值给空指针。
type_e_no_assign_to_addr=04031_E_不能向地址赋值
% 不允许向变量、常量、过程或函数的地址赋值。如果标识符
% 是过程变量，你可以尝试使用 -So 编译。
type_e_no_assign_to_const=04032_E_不能向常量变量赋值
% 不允许向声明为常量的变量赋值。这通常是一个声明为 const 的
% 参数。要允许更改值，请通过值传递参数，或使用引用参数
%（使用 var）。
type_e_array_required=04033_E_需要数组类型
% 如果你使用索引 '[<x>]' 访问变量，那么
% 类型必须是数组。在 FPC 模式下也允许使用指针。
type_e_interface_type_expected=04034_E_需要接口类型，但得到 "$1"
% 编译器期望遇到一个接口类型名，但得到了其他东西。
% 以下代码会产生此错误：
% \begin{verbatim}
% Type
%   TMyStream = Class(TStream,Integer)
% \end{verbatim}
type_h_mixed_signed_unsigned=04035_H_混合有符号表达式和 longword 会产生 64 位结果
% 如果你将有符号表达式除以 longword（或计算其模数）（反之亦然），
% 或者如果你启用了溢出和/或范围检查，并在算术表达式（+、-、*、div、mod）中
% 同时使用有符号数和 longword，那么所有运算都必须在 64 位算术中进行，
% 这比普通的 32 位算术要慢。你可以通过类型转换其中一个操作数，使其
% 匹配另一个操作数的结果类型来避免这种情况。
type_w_mixed_signed_unsigned2=04036_W_此处混合有符号表达式和 cardinal 可能导致范围检查错误
% 如果你使用二进制运算符（and、or、xor），并且其中一个
% 操作数是 longword，而另一个是有符号表达式，那么，
% 如果启用了范围检查，你可能会得到范围检查错误，因为在
% 这种情况下，两个操作数在执行操作之前都会转换为 longword。
% 你可以通过类型转换其中一个操作数，使其匹配另一个操作数的
% 结果类型来避免这种情况。
type_e_typecast_wrong_size_for_assignment=04037_E_赋值中类型转换的大小不同（$1 -> $2）
% 在赋值中使用变量时，不允许类型转换为不同大小的类型。
type_e_array_index_enums_with_assign_not_possible=04038_E_带赋值的枚举不能用作数组索引
% 当你声明了一个带有 C 风格赋值的枚举类型时，
% 如下所示：
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% 你不能将其用作数组的索引。
type_e_classes_not_related=04039_E_类或对象类型 "$1" 和 "$2" 之间没有关系
% 在一个类或对象到另一个类或对象的类型转换中，这些类/对象
% 之间没有关系。这可能会导致错误。
type_w_classes_not_related=04040_W_类类型 "$1" 和 "$2" 之间没有关系
% 在一个类到另一个类的类型转换中，这些类之间没有关系。
% 这可能会导致错误。
type_e_class_or_interface_type_expected=04041_E_需要类或接口类型，但得到 "$1"
% 编译器期望得到一个类或接口名，但得到了另一种类型或标识符。
type_e_type_is_not_completly_defined=04042_E_类型 "$1" 未完全定义
% 当类型不完整时会出现此错误：例如，指向未定义类型的指针类型。
type_w_string_too_long=04043_W_字符串字面量的字符数超过短字符串长度
% 分配给短字符串的常量字符串的大小超过了短字符串的
% 最大大小（255 个字符）。
type_w_comparison_always_false=04044_W_由于常量和表达式的范围，比较可能总是为假
% 在常量和表达式之间进行比较时，常量超出了表达式的
% 有效值范围。由于类型提升，该语句将始终计算为假。
% 如果你认为代码是正确的，请显式地将常量或表达式
% 类型转换为正确的范围以避免此警告。
type_w_comparison_always_true=04045_W_由于常量和表达式的范围，比较可能总是为真
% 在常量和表达式之间进行比较时，常量超出了表达式的
% 有效值范围。由于类型提升，该语句将始终计算为真。
% 如果你认为代码是正确的，请显式地将常量或表达式
% 类型转换为正确的范围以避免此警告。
type_w_instance_with_abstract=04046_W_构造带有抽象方法 "$2" 的类 "$1"
% 创建了一个包含未实现抽象方法的类的实例。如果调用该
% 例程，这可能会在代码中导致运行时错误 211。所有抽象
% 方法都应该被重写。
type_h_in_range_check=04047_H_IN 运算符的左操作数应该是字节大小
% \var{in} 运算符的左操作数不是适合 8 位的序数或枚举。
% 这可能会导致范围检查错误。\var{in} 运算符目前只支持
% 适合一个字节的左操作数。对于枚举，可以使用
% \var{\{\$PACKENUM\}} 或 \var{\{\$Zn\}} 开关控制枚举元素的大小。
type_w_smaller_possible_range_check=04048_W_类型大小不匹配，可能丢失数据/范围检查错误
% 赋值目标类型小于源类型。这意味着可能会导致
% 范围检查错误，或可能导致数据丢失。
type_h_smaller_possible_range_check=04049_H_类型大小不匹配，可能丢失数据/范围检查错误
% 赋值目标类型小于源类型。这意味着可能会导致
% 范围检查错误，或可能导致数据丢失。
type_e_cant_take_address_of_abstract_method=04050_E_不能获取抽象方法的地址
% 抽象方法没有主体，因此不能获取抽象方法的地址。
type_e_assignment_not_allowed=04051_E_不允许向形式参数和开放数组赋值
% 你试图向形式（无类型 var、const 或 out）参数或
% 开放数组赋值。
type_e_constant_expr_expected=04052_E_需要常量表达式
% 编译器期望一个常量表达式，但得到了一个变量表达式。
type_e_operator_not_supported_for_types=04053_E_类型 "$2" 和 "$3" 不支持运算符 "$1"
% 提供的类型不允许该操作。
type_e_illegal_type_conversion=04054_E_非法类型转换：从 "$1" 到 "$2"
% 进行类型转换时，必须确保变量和目标类型的大小相同。
type_h_pointer_to_longint_conv_not_portable=04055_H_序数和指针之间的转换不可移植
% 如果你将指针类型转换为 longint（或反之），这段代码将无法
% 在使用 64 位寻址的机器上编译。
type_w_pointer_to_longint_conv_not_portable=04056_W_序数和指针之间的转换不可移植
% 如果你将指针类型转换为不同大小的序数类型（或反之），这可能
% 会导致问题。这是一个警告，用于帮助找到使用 cardinal/longint
% 将指针类型转换为序数的 32 位特定代码。解决方案是改用
% ptrint/ptruint 类型。
type_e_cant_choose_overload_function=04057_E_无法确定要调用哪个重载函数
% 你调用重载函数时使用的参数与任何已声明的函数参数列表
% 都不对应。例如，当你声明了一个带有 \var{word} 和
% \var{longint} 参数的函数，然后用 \var{integer} 类型的
% 参数调用它。
type_e_illegal_count_var=04058_E_非法计数器变量
% \var{for} 循环变量的类型必须是序数类型。
% 循环变量不能是实数或字符串。
type_w_double_c_varargs=04059_W_将常量实数值转换为 C 可变参数的 double 类型，添加显式类型转换可以防止这种情况
% 在 C 中，常量实数值默认为 double。因此，如果你
% 将常量实数值传递给 C 函数的可变参数部分，FPC
% 默认也会将此常量转换为 double。如果你想防止
% 这种情况发生，请在常量周围添加显式类型转换。
type_e_class_or_cominterface_type_expected=04060_E_需要类或 COM 接口类型，但得到 "$1"
% 某些运算符（如 AS 运算符）只适用于类或 COM 接口。
type_e_no_const_packed_array=04061_E_尚不支持常量打包数组
% 你不能将（位）打包数组声明为类型化常量。
type_e_got_expected_packed_array=04062_E_参数 $1 的类型不兼容：得到 "$2"，期望"（位）打包数组"
% 编译器期望指定参数为（位）打包数组。
type_e_got_expected_unpacked_array=04063_E_参数 $1 的类型不兼容：得到 "$2"，期望"（非打包）数组"
% 编译器期望指定参数为常规（即非打包）数组。
type_e_no_packed_inittable=04064_E_打包数组的元素不能是需要初始化的类型
% 尚未实现对需要初始化的类型（如 ansistring，或包含
% ansistring 的记录）的打包数组的支持。
type_e_no_const_packed_record=04065_E_尚不支持常量打包记录和对象
% 目前你不能将（位）打包数组声明为类型化常量。
type_w_untyped_arithmetic_unportable=04066_W_无类型指针的算术运算 "$1" 在 {$T+} 中不可移植，建议使用类型转换
% 在无类型指针上进行加法/减法在 \var{\{\$T+\}} 中可能会有不同的行为。
% 使用类型转换为有类型指针。
type_e_cant_take_address_of_local_subroutine=04076_E_不能获取标记为局部的子程序的地址
% 不能获取标记为局部的子程序的地址。
type_e_cant_export_local=04077_E_不能从单元导出标记为局部的子程序
% 不能从单元导出标记为局部的子程序。
type_e_not_automatable=04078_E_类型不可自动化："$1"
% 只有 byte、integer、longint、smallint、currency、single、double、
% ansistring、widestring、tdatetime、variant、olevariant、wordbool
% 和所有接口是可自动化的。
type_h_convert_add_operands_to_prevent_overflow=04079_H_在进行加法之前将操作数转换为 "$1" 可以防止溢出错误
% 两个类型相加可能会导致溢出错误。由于你将结果转换为更大的类型，
% 你可以通过在进行加法之前将操作数转换为此类型来防止此类错误。
type_h_convert_sub_operands_to_prevent_overflow=04080_H_在进行减法之前将操作数转换为 "$1" 可以防止溢出错误
% 两个类型相减可能会导致溢出错误。由于你将结果转换为更大的类型，
% 你可以通过在进行减法之前将操作数转换为此类型来防止此类错误。
type_h_convert_mul_operands_to_prevent_overflow=04081_H_在进行乘法之前将操作数转换为 "$1" 可以防止溢出错误
% 两个类型相乘可能会导致溢出错误。由于你将结果转换为更大的类型，
% 你可以通过在进行乘法之前将操作数转换为此类型来防止此类错误。
type_w_pointer_to_signed=04082_W_将指针转换为有符号整数可能导致错误的比较结果和范围错误，请改用无符号类型
% 32 位机器上的虚拟地址空间从 \$00000000 到 \$ffffffff。
% 许多操作系统允许你在 \$80000000 以上分配内存。
% 例如，\windows 和 \linux 都允许指针范围在 \$0000000 到 \$bfffffff。
% 如果你将指针转换为有符号类型，这可能会导致溢出和范围检查错误，
% 而且 \$80000000 < \$7fffffff。这可能会在类似 "if p>q" 的代码中
% 导致随机错误。
type_e_interface_has_no_guid=04083_E_接口类型 $1 没有有效的 GUID
% 当对接口或类应用 as 运算符时，所需的接口（即 as 运算符的
% 右操作数）必须有一个有效的 GUID。
type_e_invalid_objc_selector_name=04084_E_无效的选择器名称 "$1"
% Objective-C 选择器不能为空，必须是有效的标识符或单个冒号，
% 如果它包含至少一个冒号，它也必须以冒号结尾。
type_e_expected_objc_method_but_got=04085_E_期望 Objective-C 方法，但得到 $1
% 选择器只能为 Objective-C 方法创建，不能为任何其他类型的
% 过程/函数/方法创建。
type_e_expected_objc_method=04086_E_期望 Objective-C 方法或常量方法名
% 选择器只能为 Objective-C 方法创建，可以使用字符串常量
% 指定名称，或使用当前作用域中可见的 Objective-C 方法标识符。
type_e_no_type_info=04087_E_此类型没有可用的类型信息
% 某些类型不生成类型信息，例如值范围有间隔的枚举
%（这包括下限不为零的枚举）。
type_e_ordinal_or_string_expr_expected=04088_E_需要序数或字符串表达式
% 表达式必须是序数或字符串类型。
type_e_string_expr_expected=04089_E_需要字符串表达式
% 表达式必须是字符串类型。
type_w_zero_to_nil=04090_W_将 0 转换为 NIL
% 初始化指针时使用 NIL 而不是 0。
type_e_protocol_type_expected=04091_E_需要 Objective-C 协议类型，但得到 "$1"
% 编译器期望得到协议类型名，但找到了其他内容。
type_e_objc_type_unsupported=04092_E_类型 "$1" 不支持与 Objective-C 和 blocks 运行时交互
% Objective-C 和 Blocks 大量使用运行时类型信息（RTTI）。这种格式
% 由运行时的维护者定义，因此不能适应所有可能的 Object Pascal 类型。
% 特别是，依赖于编译器引用计数的类型（如 ansistrings 和某些类型的
% 接口）不能用作 Objective-C 类的字段，不能直接传递给 Objective-C
% 方法或 Blocks，也不能使用 \var{objc\_encode} 进行编码。
type_e_class_or_objcclass_type_expected=04093_E_需要类或 objcclass 类型，但得到 "$1"
% 只能为 \var{class} 和 \var{objcclass} 创建类引用类型
type_e_objcclass_type_expected=04094_E_需要 objcclass 类型
% 编译器期望一个 \var{objcclass} 类型
type_w_procvar_univ_conflicting_para=04095_W_过程变量中强制转换的 univ 参数类型可能导致崩溃或内存损坏：$1 到 $2
% \var{univ} 参数隐式兼容所有相同大小的类型，
% 在过程变量定义中也是如此。这意味着以下代码是
% 合法的，因为 \var{single} 和 \var{longint} 具有相同的大小：
% \begin{verbatim}
% {$mode macpas}
% Type
%   TIntProc = procedure (l: univ longint);
%
%   procedure test(s: single);
%     begin
%       writeln(s);
%     end;
%
%   var
%     p: TIntProc;
%   begin
%     p:=test;
%     p(4);
%   end.
% \end{verbatim}
% 然而，在将整数传递到寄存器而将浮点值传递到栈的平台上，
% 这段代码可能会崩溃，因为栈将不平衡。
% 注意，此警告不会标记所有潜在的危险情况。
% 当 \var{test} 返回时。
type_e_generics_cannot_reference_itself=04096_E_泛型特化的类型参数不能引用当前正在特化的类型
% 不允许递归特化泛型，如 \var{Type MyType = specialize MyGeneric<MyType>;}。
type_e_type_parameters_are_not_allowed_here=04097_E_非泛型类/记录/对象的过程或函数不允许使用类型参数
% 类型参数只能用于泛型类、记录或对象的方法
type_e_generic_declaration_does_not_match=04098_E_"$1" 的泛型声明与先前声明不匹配
% 泛型声明与先前的声明不匹配
type_e_helper_type_expected=04099_E_需要辅助类型
% 编译器期望一个 \var{class helper} 类型。
type_e_record_type_expected=04100_E_需要记录类型
% 编译器期望一个 \var{record} 类型。
type_e_class_helper_must_extend_subclass=04101_E_派生的类辅助必须扩展 "$1" 的子类或该类本身
% 如果类辅助继承自另一个类辅助，则扩展的类必须扩展与父类辅助相同的类或其子类
type_e_record_helper_must_extend_same_record=04102_E_派生的记录或类型辅助必须扩展 "$1"
% 如果记录辅助继承自另一个记录辅助，它必须扩展与父记录辅助相同的记录。
type_e_procedures_return_no_value=04103_E_无效赋值，过程不返回值
% 当试图将过程或析构函数调用的结果赋值时会发生此错误。
% 过程或析构函数不返回值，因此这是不可能的。
type_w_implicit_string_cast=04104_W_从 "$1" 到 "$2" 的隐式字符串类型转换
% 遇到从 ansi 字符串类型到 unicode 字符串类型的隐式类型转换。
% 要避免此警告，请执行显式类型转换。
type_w_implicit_string_cast_loss=04105_W_从 "$1" 到 "$2" 的隐式字符串类型转换可能导致数据丢失
% 遇到从 unicode 字符串类型到 ansi 字符串类型的隐式类型转换。
% 此转换可能丢失数据，因为并非所有 unicode 字符都可以在目标字符串类型的代码页中表示。
type_w_explicit_string_cast=04106_-W_从 "$1" 到 "$2" 的显式字符串类型转换
% 遇到从 ansi 字符串类型到 unicode 字符串类型的显式类型转换。
% 此警告默认关闭。您可以打开它以查看所有可疑的字符串转换。
type_w_explicit_string_cast_loss=04107_-W_从 "$1" 到 "$2" 的显式字符串类型转换可能导致数据丢失
% 遇到从 unicode 字符串类型到 ansi 字符串类型的显式类型转换。
% 此转换可能丢失数据，因为并非所有 unicode 字符都可以在目标字符串类型的代码页中表示。
% 此警告默认关闭。您可以打开它以查看所有有损的字符串转换位置。
type_w_unicode_data_loss=04108_W_Unicode 常量转换可能导致数据丢失
% 从 WideChar 转换为 AnsiChar 可能丢失数据，因为并非所有 unicode 字符都可以在当前系统代码页中表示
% 您只能嵌套 31 层函数定义。
type_e_range_check_error_bounds=04109_E_计算常量时发生范围检查错误（$1 必须在 $2 和 $3 之间）
type_w_range_check_error_bounds=04110_W_计算常量时发生范围检查错误（$1 必须在 $2 和 $3 之间）
% 常量超出其允许范围。
type_e_type_not_allowed_for_default=04111_E_Default() 内部函数不支持此类型
% Default 内部函数不支持某些类型，例如 Text 和 File Of X。
type_e_java_class_method_not_static_virtual=04112_E_JVM 虚拟类方法不能是静态的
% 在针对 JVM 平台时，虚拟类方法不能是静态的，因为正确的分派需要 self 指针。
type_e_invalid_final_assignment=04113_E_final（类）字段只能在其类的（类）构造函数中赋值
% 只能在拥有类的（类）构造函数内部为 final（类）字段赋值。
type_e_no_managed_formal_assign_typecast=04114_E_在托管平台上无法对无类型参数进行类型转换，请直接为它们赋值
% 在托管平台上，无类型参数被编译器转换为等同于 \var{var x: BaseClassType}。
% 传递给此类参数的非类基类型会自动包装（或装箱）到一个类中，
% 调用后可能修改的值会被赋回原始变量。
% 在调用方，更改无类型 var/out 参数只需简单地为它们赋值（基于类或原始类型）。
% 在调用方，如果它们的类型与原始变量不匹配，将引发异常。
type_e_no_managed_assign_generic_typecast=04115_E_在托管平台上，表达式的赋值端不能类型转换为超类型
% 托管平台在字节码级别保证类型安全。这意味着虚拟机必须能够
% 静态确定不会发生类型不安全的赋值或操作。通过将父类类型赋值给
% 子类型变量并将赋值端类型转换为父类类型，类型安全将不再得到
% 保证，生成的代码将在运行时验证失败。
type_w_interface_lower_visibility=04116_-W_接口方法 "$1" 通过接口实例访问时将 "$2" 的可见性提升为 public
type_e_interface_lower_visibility=04117_E_接口方法 "$1" 的可见性（public）高于 "$2"
% 接口中的所有方法始终具有 public 可见性。这意味着如果接口方法
% 使用（严格）protected 或 private 方法实现，该方法实际上可以通过
% 接口公开访问。在 JVM 目标上，这种情况会导致错误，因为 JVM 拒绝
% 此类绕过可见性规则的尝试。在其他目标上，这是默认禁用的警告，
% 因为这种情况是常见做法，但如果您关心保持代码对 JVM 目标的
% 可编译性，可以启用它。
type_e_typeof_requires_vmt=04118_E_TYPEOF 只能用于具有 VMT 的对象类型
% Typeof() 内部函数返回其参数的 VMT 指针。它不能用于没有 VMT 的对象类型。
type_e_invalid_default_value=04119_E_不能为类型 "$1" 的参数定义默认值
% 声明为结构化类型的参数，如文件、变体、非动态数组和 TP 风格对象，
% 不能有默认值。
type_e_type_not_allowed_for_type_helper=04120_E_类型 "$1" 不能被类型辅助扩展
% 过程变量等类型不能被类型辅助扩展
type_e_procedure_must_be_far=04121_E_过程或函数必须是 far 才能获取其地址："$1"
% 在某些 i8086 内存模型（medium、large 和 huge）中，过程和函数
% 必须声明为 'far' 才能获取其地址。
type_w_instance_abstract_class=04122_W_创建抽象类 "$1" 的实例
% 指定的类被声明为 \var{abstract}，因此不应创建此类的实例。
% 这仅是为了 Delphi 兼容性而发出的警告。
type_e_function_reference_kind=04123_E_子程序引用不能声明为 "of object" 或 "is nested"，它们始终可以引用任何类型的子程序
% 子程序引用可以引用任何类型的子程序，因此不需要为方法或嵌套子程序
% 进行特殊化。
type_e_seg_procvardef_wrong_memory_model=04124_E_该内存模型中的过程变量不存储段信息
type_w_empty_constant_range_set=04125_W_集合构造器范围的第一个值大于第二个值，因此该范围描述一个空集
% 如果像这样构造集合：\var{s:=[9..7];]}，则会生成一个空集。由于这通常不是期望的结果，编译器会发出警告。
type_e_cblock_callconv=04126_E_C block 引用必须使用 CDECL 或 MWPASCAL 调用约定
% 声明 C block 引用时，确保它使用 \var{cdecl} 或 \var{mwpascal} 调用约定，
% 可以通过添加相应的函数指令或使用 \var{\{\$Calling\}} 编译器指令来实现。
type_e_forward_interface_type_does_not_match=04127_E_接口 $1 的前向声明接口类型与声明的接口类型不匹配
% 声明接口前向时，接口类型必须与接口的实际声明相同。
% 这对于父接口特别重要，因为它隐式地为子接口设置接口类型。
type_e_generic_const_type_not_allowed=04128_E_泛型常量参数不允许使用类型：$1
% 只有也可以（间接）用于无类型常量的类型才能用作泛型常量参数的类型。
type_e_cant_read_write_type_in_iso_mode=04129_E_在 iso 模式下不能读写此类型的变量
% 您正在尝试从文本类型文件 \var{read} 或 \var{write} 一个变量，
% 但在选定的语言模式（iso 模式）下不支持该变量的类型。
type_w_array_size_does_not_match_size_of_constant_string=04130_W_常量字符串的长度（长度为 $1）必须等于数组元素的数量（$2 个元素）
% ISO Pascal 要求字符串常量的长度与它们被赋值的数组长度相同。
type_e_nested_procvar_to_funcref=04131_E_嵌套函数变量不能赋值给函数引用
% 函数引用可以超出它们所在函数的作用域范围，而赋值给嵌套函数变量的
% 嵌套函数则不能。由于这种设计上的差异，禁止将嵌套函数变量赋值给
% 函数引用。
type_e_cannot_take_address_of_wasm_externref=04132_E_不能获取 WebAssembly externref 的地址
% WebAssembly externref 类型没有内存表示，因此不能获取其地址。
type_e_cannot_determine_size_of_wasm_reference_type=04133_E_WebAssembly 引用类型没有可观察的大小
% WebAssembly 引用类型是不透明的，这意味着它们的大小和位模式都不能被观察。
type_e_ordinal_or_pointer_expr_expected=04134_E_需要序数或指针表达式
% 表达式必须是序数或指针类型。
%
% \end{description}
#
# Symtable
#
# 05101 is the last used one
#
% \section{符号处理}
% 本节列出所有与符号处理相关的消息。
% 这意味着所有与过程和变量名称有关的内容。
% \begin{description}
sym_e_id_not_found=05000_E_找不到标识符 "$1"
% 编译器不认识此符号。通常发生在拼写变量或过程名称错误时，
% 或忘记声明变量时。
sym_f_internal_error_in_symtablestack=05001_F_SymTableStack() 中的内部错误
% 编译器发生内部错误；如果遇到此类错误，请联系开发人员并尽量
% 提供错误发生的具体情况描述。
sym_e_duplicate_id=05002_E_重复的标识符 "$1"
% 该标识符已在当前作用域中声明。
sym_h_duplicate_id_where=05003_H_标识符已在 $1 的第 $2 行定义
% 该标识符已在先前的作用域中声明。
sym_e_unknown_id=05004_E_未知标识符 "$1"
% 遇到的标识符尚未声明，或在其定义作用域之外使用。
sym_e_forward_not_resolved=05005_E_前向声明未解决 "$1"
% 这可能发生在两种情况下：
% \begin{itemize}
% \item 您在 \var{interface} 部分或使用 \var{forward} 指令声明了一个函数，
% 但未实现它。
% \item 您引用了当前 \var{type} 块中未声明的类型。
% \end{itemize}
sym_e_error_in_type_def=05007_E_类型定义错误
% 您的新数组类型定义中存在错误。
% 数组声明中的某个范围分隔符有误。
% 例如，\var{Array [1..1.25]} 将触发此错误。
sym_e_forward_type_not_resolved=05009_E_前向类型未解决 "$1"
% 一个符号被前向定义，但未遇到其声明。
sym_e_only_static_in_static=05010_E_静态方法中或方法外部只能使用静态变量
% 对象的静态方法只能访问静态变量。
sym_e_type_must_be_rec_or_object_or_class=05012_E_需要 record、object 或 class 类型
% 变量或表达式不是 \var{record}、\var{object} 或 \var{class} 类型。
sym_e_no_instance_of_abstract_object=05013_E_不允许创建带有抽象方法的类或对象的实例
% 您正在尝试生成一个具有未被重写的抽象方法的类的实例。
sym_w_label_not_defined=05014_W_标签未定义 "$1"
% 标签已声明但未定义。
sym_e_label_used_and_not_defined=05015_E_标签已使用但未定义 "$1"
% 标签已声明并使用，但未定义。
sym_e_ill_label_decl=05016_E_非法的标签声明
% 此错误不应发生；它发生在过程或函数外部定义标签时。
sym_e_goto_and_label_not_supported=05017_E_不支持 GOTO 和 LABEL（请使用 -Sg 开关）
% 您必须使用 -Sg 开关来编译带有 \var{label} 和 \var{goto} 语句的程序。
% 默认情况下不支持 \var{label} 和 \var{goto}。
sym_e_label_not_found=05018_E_找不到标签
% 遇到 \var{goto label}，但标签未声明。
sym_e_id_is_no_label_id=05019_E_标识符不是标签
% \var{goto} 后指定的标识符不是标签类型。
sym_e_label_already_defined=05020_E_标签已定义
% 您正在重复定义标签。标签只能定义一次。
sym_e_ill_type_decl_set=05021_E_集合元素的类型声明非法
% 集合的声明包含无效的类型定义。
sym_e_class_forward_not_resolved=05022_E_前向类定义未解决 "$1"
% 您声明了一个类，但未实现它。
sym_n_unit_not_used=05023_H_单元 "$1" 在 $2 中未使用
% \var{uses} 子句中引用的单元未被使用。
sym_h_para_identifier_not_used=05024_H_参数 "$1" 未使用
% 该标识符已（在局部或全局）声明但未（在局部或全局）使用。
sym_n_local_identifier_not_used=05025_N_局部变量 "$1" 未使用
% 您在过程或函数实现中声明了一个变量，但未使用它。
sym_h_para_identifier_only_set=05026_H_值参数 "$1" 已赋值但从未使用
% 该标识符已（在局部或全局）声明并赋值，但在赋值后未（在局部或全局）使用。
sym_n_local_identifier_only_set=05027_N_局部变量 "$1" 已赋值但从未使用
% 过程或函数实现中的变量已声明并赋值，但在赋值后未使用。
sym_h_local_symbol_not_used=05028_H_局部 $1 "$2" 未使用
% 一个局部符号从未使用。
sym_n_private_identifier_not_used=05029_N_私有字段 "$1.$2" 从未使用
% 指定的私有字段已定义，但在代码中从未使用。
sym_n_private_identifier_only_set=05030_N_私有字段 "$1.$2" 已赋值但从未使用
% 指定的私有字段已声明并赋值，但从未读取。
sym_n_private_method_not_used=05031_N_私有方法 "$1.$2" 从未使用
% 指定的私有方法已声明但在代码中从未使用。
sym_e_set_expected=05032_E_需要集合类型
% 变量或表达式不是 \var{set} 类型。这发生在 \var{in} 语句中。
sym_w_function_result_not_set=05033_W_函数结果似乎未设置
% 如果编译器认为函数返回值未设置，您可能会收到此警告。这不会显示在
% 汇编过程或包含汇编块的过程中。
sym_w_wrong_C_pack=05034_W_类型 "$1" 在当前记录中对 C 的对齐不正确
% 大小不是 4 的倍数的数组将在 C 结构中错误对齐。
sym_e_illegal_field=05035_E_未知的记录字段标识符 "$1"
% 该字段在记录/对象定义中不存在。
sym_w_uninitialized_local_variable=05036_W_局部变量 "$1" 似乎未初始化
% 如果编译器认为一个变量将被使用（即出现在表达式右侧）而它
% 之前未被初始化（即未出现在赋值的左侧），则显示此消息。
sym_w_uninitialized_variable=05037_W_变量 "$1" 似乎未初始化
% 如果编译器认为一个变量将被使用（即出现在表达式右侧）而它
% 之前未被初始化（即未出现在赋值的左侧），则显示此消息。
sym_e_id_no_member=05038_E_标识符不是成员 "$1"
% 当访问未定义的记录、字段或方法的标识符时，会生成此错误。
sym_h_param_list=05039_H_找到声明：$1
% 当您使用 \var{-vh} 开关时会得到这个。在找不到重载过程的情况下，
% 所有候选的重载过程都会列出，包括它们的参数列表。
sym_e_segment_too_large=05040_E_数据元素过大
% 当您声明的数据元素大小超过规定限制时会得到这个
%（在 80386+/68020+ 处理器上为 2 GB）。
sym_e_no_matching_implementation_found=05042_E_找不到接口方法 "$1" 的匹配实现
% 未找到可以实现接口方法的匹配方法。请检查方法的参数类型和结果类型。
sym_w_deprecated_symbol=05043_W_符号 "$1" 已弃用
% 这意味着使用了一个声明为 \var{deprecated} 的符号（变量、例程等）。
% 弃用的符号在单元/库的新版本中可能不再可用。应尽可能避免使用此符号。
sym_w_non_portable_symbol=05044_W_符号 "$1" 不可移植
% 这意味着使用了一个声明为 \var{platform} 的符号（变量、例程等）。
% 此符号的值、用法和可用性是平台特定的，如果源代码必须可移植，则不应使用。
sym_w_non_implemented_symbol=05055_W_符号 "$1" 未实现
% 这意味着使用了一个声明为 \var{unimplemented} 的符号（变量、例程等）。
% 此符号已定义，但在此特定平台上尚未实现。
sym_e_cant_create_unique_type=05056_E_无法从此类型创建唯一类型
% 使用 \var{type newtype = type oldtype;} 重定义类型时，
% 仅支持序数、浮点和字符串等简单类型。
sym_h_uninitialized_local_variable=05057_H_局部变量 "$1" 似乎未初始化
% 如果编译器认为一个变量将被使用（即出现在表达式右侧）而它
% 之前未被初始化（即未出现在赋值的左侧），则显示此消息。
sym_h_uninitialized_variable=05058_H_变量 "$1" 似乎未初始化
% 如果编译器认为一个变量将被使用（即出现在表达式右侧）而它
% 之前未被初始化（即未出现在赋值的左侧），则显示此消息。
sym_w_function_result_uninitialized=05059_W_函数结果变量似乎未初始化
% 如果编译器认为函数结果变量将在初始化之前被使用
%（即在赋值左侧出现之前出现在表达式右侧），则显示此消息。
sym_h_function_result_uninitialized=05060_H_函数结果变量似乎未初始化
% 如果编译器认为函数结果变量将在初始化之前被使用
%（即在赋值左侧出现之前出现在表达式右侧），则显示此消息。
sym_w_identifier_only_read=05061_W_变量 "$1" 已读取但从未赋值
% 您已读取变量的值，但从未为其赋值。
sym_h_abstract_method_list=05062_H_找到抽象方法：$1
% 当收到关于构造带有抽象方法的类/对象的警告时，
% 您会收到此提示以帮助您找到受影响的方法。
sym_w_experimental_symbol=05063_W_符号 "$1" 是实验性的
% 这意味着使用了一个声明为 \var{experimental} 的符号（变量、例程等）。
% 实验性符号可能在未来版本中消失或改变语义。应尽可能避免使用此符号。
sym_w_forward_not_resolved=05064_W_前向声明 "$1" 未解决，假定为外部
% 这发生在 macpas 模式下，如果您在单元的 \var{interface} 中声明了一个函数，
% 但未实现它。
sym_w_library_symbol=05065_W_符号 "$1" 属于库
% 这意味着使用了一个声明为 \var{library} 的符号（变量、例程等）。
% 库符号可能在其他库中不可用。
sym_w_deprecated_symbol_with_msg=05066_W_符号 "$1" 已弃用："$2"
% 这意味着使用了一个声明为 \var{deprecated} 的符号（变量、例程等）。
% 弃用的符号在单元/库的新版本中可能不再可用。应尽可能避免使用此符号。
sym_e_no_enumerator=05067_E_无法为类型 "$1" 找到枚举器
% 这意味着编译器无法找到适合在 for-in 循环中使用的枚举器。
% 要创建枚举器，您需要定义一个运算符枚举器或在类或对象定义中添加公共或已发布的
% GetEnumerator 方法。
sym_e_no_enumerator_move=05068_E_在枚举器 "$1" 中找不到 "MoveNext" 方法
% 这意味着编译器在枚举器类或对象定义中找不到具有布尔返回类型的公共 MoveNext 方法。
sym_e_no_enumerator_current=05069_E_在枚举器 "$1" 中找不到 "Current" 属性
% 这意味着编译器在枚举器类或对象定义中找不到公共 Current 属性。
sym_e_objc_para_mismatch=05070_E_声明的参数数量与消息字符串中的冒号数量不匹配
% 在 Objective-C 中，消息名称自动包含与参数数量相同的冒号。
% 为了防止在 FPC 中指定消息名称时出错，编译器会检查这一点。
% 请注意，对于通过 \var{array of const} 参数转换到 FPC 的可变参数消息，
% 这个最终的 \var{array of const} 参数不计入。隐藏的 \var{self} 和 \var{\_cmd}
% 参数也不计入。
sym_n_private_type_not_used=05071_N_私有类型 "$1.$2" 从未使用
% 指定的私有类型已声明但在代码中从未使用。
sym_n_private_const_not_used=05072_N_私有常量 "$1.$2" 从未使用
% 指定的私有常量已声明但在代码中从未使用。
sym_n_private_property_not_used=05073_N_私有属性 "$1.$2" 从未使用
% 指定的私有属性已声明但在代码中从未使用。
sym_w_deprecated_unit=05074_W_单元 "$1" 已弃用
% 这意味着使用了一个声明为 \var{deprecated} 的单元。
% 弃用的单元在库的新版本中可能不再可用。应尽可能避免使用此单元。
sym_w_deprecated_unit_with_msg=05075_W_单元 "$1" 已弃用："$2"
% 这意味着使用了一个声明为 \var{deprecated} 的单元。
% 弃用的单元在库的新版本中可能不再可用。应尽可能避免使用此单元。
sym_w_non_portable_unit=05076_W_单元 "$1" 不可移植
% 这意味着使用了一个声明为 \var{platform} 的单元。
% 此单元的使用和可用性是平台特定的，如果源代码必须可移植，则不应使用。
sym_w_library_unit=05077_W_单元 "$1" 属于库
% 这意味着使用了一个声明为 \var{library} 的单元。
% 库单元可能在其他库中不可用。
sym_w_non_implemented_unit=05078_W_单元 "$1" 未实现
% 这意味着使用了一个声明为 \var{unimplemented} 的单元。
% 此单元已定义，但在此特定平台上尚未实现。
sym_w_experimental_unit=05079_W_单元 "$1" 是实验性的
% 这意味着使用了一个声明为 \var{experimental} 的单元。
% 实验性单元可能在未来版本中消失或改变语义。应尽可能避免使用此单元。
sym_e_formal_class_not_resolved=05080_E_形式声明的类 "$1" 的完整定义不在作用域内。请将包含其完整定义的单元添加到 uses 子句中。
% Objecive-C 和 Java 类可以形式导入，而无需使用完整声明它的单元。
% 这使得可以对这些类进行前向引用并打破单元之间的循环依赖。
% 但是，一旦您想要实际使用此类类型的实体（如访问其字段之一、
% 向其发送消息或用它来继承），编译器就需要类的完整定义在作用域内。
sym_e_interprocgoto_into_init_final_code_not_allowed=05081_E_不允许使用 goto 跳转到单元的初始化或终止化块中
% 不允许使用 goto 跳转到单元的初始化或终止化块中。
sym_e_external_class_name_mismatch1=05082_E_形式类 "$2" 的外部名称 "$1" 无效
sym_e_external_class_name_mismatch2=05083_E_此处具有外部名称 "$1" 的完整类定义
% 当使用形式外部定义声明类时，实际外部定义（如果有）
% 必须指定与形式定义相同的外部名称（因为两个定义都引用相同的实际类类型）。
sym_w_library_overload=05084_W_可能的库冲突：库 "$2" 中的符号 "$1" 在库 "$3" 中也存在
% 某些操作系统没有库特定的命名空间，对于这些操作系统，
% 声明为 "external 'libname' name 'funcname'" 的函数中，
% 'libname' 部分仅是提示，funcname 也可能被其他库加载。
% 当 'funcname' 在两个不同的库名称中使用时会出现此警告。
sym_e_duplicate_id_create_java_constructor=05085_E_无法添加隐式构造函数 'Create'，因为标识符已被 "$1" 使用
% Java 不会自动将继承的构造函数添加到子类中，因此它们可以被隐藏。
% 但是，如果一个类没有显式声明至少一个构造函数，编译器需要
% 添加一个公共的无参数构造函数。在 Java 中，构造函数是无名的，
% 但在 FPC 中它们都叫做 ``Create''。因此，如果您没有向 Java 类添加构造函数，
% 并且还将 ``Create'' 标识符用于其他实体（例如字段或无参数方法），
% 编译器就无法满足这个要求。
sym_e_no_matching_inherited_parameterless_constructor=05086_E_无法为类生成默认构造函数，因为父类没有无参数构造函数
% Java 不会自动将继承的构造函数添加到子类中，因此它们可以被隐藏。
% 但是，如果一个类没有显式声明至少一个构造函数，编译器需要
% 添加一个公共的无参数构造函数。然后编译器必须在这个添加的构造函数中
% 调用父类的无参数构造函数。但是，如果父类没有声明这样的构造函数，
% 这就是不可能的。在这种情况下，您必须自己添加一个有效的构造函数。
sym_d_adding_helper_for=05087_D_正在为 $1 添加辅助程序
% 为提到的类型添加了一个辅助程序到当前作用域
sym_e_param_list=05088_E_找到声明：$1
% 在出现错误时显示所有重载的声明。
sym_w_uninitialized_managed_local_variable=05089_W_托管类型的局部变量 "$1" 似乎未初始化
% 当编译器认为一个变量将被使用（即出现在表达式的右侧）时显示此消息，
% 而该变量尚未初始化（即未出现在赋值的左侧）。
% 由于该变量是托管的，即由编译器隐式初始化，这可能是预期的行为，
% 并不一定意味着代码有错误。
sym_w_uninitialized_managed_variable=05090_W_托管类型的变量 "$1" 似乎未初始化
% 当编译器认为一个变量将被使用（即出现在表达式的右侧）时显示此消息，
% 而该变量尚未初始化（即未出现在赋值的左侧）。
% 由于该变量是托管的，即由编译器隐式初始化，这可能是预期的行为，
% 并不一定意味着代码有错误。
sym_h_uninitialized_managed_local_variable=05091_H_托管类型的局部变量 "$1" 似乎未初始化
% 当编译器认为一个变量将被使用（即出现在表达式的右侧）时显示此消息，
% 而该变量尚未初始化（即未出现在赋值的左侧）。
% 由于该变量是托管的，即由编译器隐式初始化，这可能是预期的行为，
% 并不一定意味着代码有错误。
sym_h_uninitialized_managed_variable=05092_H_托管类型的变量 "$1" 似乎未初始化
% 当编译器认为一个变量将被使用（即出现在表达式的右侧）时显示此消息，
% 而该变量尚未初始化（即未出现在赋值的左侧）。
% 由于该变量是托管的，即由编译器隐式初始化，这可能是预期的行为，
% 并不一定意味着代码有错误。
sym_w_managed_function_result_uninitialized=05093_W_托管类型的函数结果变量似乎未初始化
% 当编译器认为函数结果变量将被使用（即出现在表达式的右侧）时显示此消息，
% 而该变量尚未初始化（即未出现在赋值的左侧）。
% 由于该变量是托管的，即由编译器隐式初始化，这可能是预期的行为，
% 并不一定意味着代码有错误。
sym_h_managed_function_result_uninitialized=05094_H_托管类型的函数结果变量似乎未初始化
% 当编译器认为函数结果变量将被使用（即出现在表达式的右侧）时显示此消息，
% 而该变量尚未初始化（即未出现在赋值的左侧）。
% 由于该变量是托管的，即由编译器隐式初始化，这可能是预期的行为，
% 并不一定意味着代码有错误。
sym_w_duplicate_id=05095_W_重复的标识符 "$1"
% 该标识符已在当前作用域中的 Objective-C 类别中声明。
% 这是一个警告而不是错误，因为虽然这会隐藏类别中的标识符，
% 但作用域中经常有许多未使用的类别。
sym_e_generic_type_param_mismatch=05096_E_泛型类型参数 "$1" 与声明中的不匹配
% 为泛型类、记录或例程指定的泛型类型参数与泛型类、记录
% 或例程的声明中声明的不匹配。
sym_e_generic_type_param_decl=05097_E_泛型类型参数声明为 "$1"
% 当在声明和定义之间发现不匹配时，显示泛型类型参数最初是如何声明的。
sym_e_type_must_be_rec_or_object=05098_E_需要记录或对象类型
% 变量或表达式不是 \var{record} 或 \var{object} 类型。
sym_e_symbol_no_capture=05099_E_符号 "$1" 不能被捕获
% 指定的符号不能被捕获用于函数引用。
% 例如，\var{var} 或 \var{out} 参数不能以这种方式被捕获。
sym_f_systemunitnotloaded=05100_F_系统单元未加载
% 编译器使用了需要加载系统单元的函数，
% 但它尚未加载。这是一个内部编译器错误，必须报告。
sym_e_wasm_ref_types_cannot_be_used_in_records=05101_E_WebAssembly 引用类型不能在记录、对象或类中使用
% WebAssembly 引用类型没有内存中的表示形式，因此
% 不能在记录、对象或类中使用。
%
% \end{description}
#
# Codegenerator
#
# 06062 is the last used one
#
% \section{代码生成器消息}
% 本节列出了代码生成器遇到错误条件时可能显示的所有消息。
% \begin{description}
cg_e_parasize_too_big=06009_E_参数列表大小超过 65535 字节
% I386 处理器将参数列表限制为 65535 字节。（\var{RET} 指令导致这个限制。）
cg_e_file_must_call_by_reference=06012_E_文件类型必须是 var 参数
% 您不能将文件指定为值参数，即它们必须始终声明为 \var{var} 参数。
cg_e_cant_use_far_pointer_there=06013_E_此处不允许使用远指针
% Free Pascal 不支持远指针，因此您不能获取具有远引用结果的表达式的地址。
% \var{mem} 构造有一个远引用结果，所以以下代码将产生此错误：
% \begin{verbatim}
% var p : pointer;
% ...
% p:=@mem[a000:000];
% \end{verbatim}
cg_e_dont_call_exported_direct=06015_E_不能调用导出声明的函数
% 不再使用。
cg_w_member_cd_call_from_method=06016_W_可能非法调用构造函数或析构函数
% 编译器检测到在方法内调用了构造函数或析构函数。
% 这可能会导致问题，因为构造函数/析构函数在进入时需要参数。
cg_n_inefficient_code=06017_N_低效的代码
% 您的语句对编译器来说似乎有问题。
cg_w_unreachable_code=06018_W_不可达代码
% 您指定了一个永远不会执行的构造。例如：
% \begin{verbatim}
% while false do
%   begin
%   {.. code ...}
%   end;
% \end{verbatim}
cg_e_cant_call_abstract_method=06020_E_不能直接调用抽象方法
% 你不能直接调用抽象方法。相反，你必须调用一个重写的子方法，
% 因为抽象方法没有实现。
cg_d_register_weight=06027_DL_寄存器 $1 权重 $2 $3
% 调试消息。当编译器考虑将变量保存在寄存器中时显示。
cg_d_stackframe_omited=06029_DL_栈帧被省略
% 某些过程/函数不需要完整的栈帧，因此被省略。
% 当使用 {-vd} 开关时将显示此消息。
cg_e_unable_inline_object_methods=06031_E_对象或类方法不能内联
% 你不能内联对象方法。
cg_e_unable_inline_procvar=06032_E_过程变量调用不能内联
% 带有过程变量调用的过程不能内联。
cg_e_no_code_for_inline_stored=06033_E_未存储内联过程的代码
% 编译器无法存储内联过程的代码。
cg_e_can_access_element_zero=06035_E_不能访问 ansi/wide 或 longstring 的零元素，请使用 (set)length 代替
% 你应该使用 \var{setlength} 来设置 ansi/wide/longstring 的长度，
% 使用 \var{length} 来获取此类字符串类型的长度。
cg_e_cannot_call_cons_dest_inside_with=06037_E_不能在 'with' 子句内调用构造函数或析构函数
% 在 \var{with} 子句内，你不能为 \var{with} 子句中的对象调用构造函数或析构函数。
cg_e_cannot_call_message_direct=06038_E_不能直接调用消息处理方法
% 如果消息处理方法包含显式的 \var{Self} 参数，则不能直接调用它。
cg_e_goto_inout_of_exception_block=06039_E_不能跳转到异常块内部或外部
% 不允许跳转到异常块（如 \var{try..finally..end;}）的内部或外部。
% 例如，以下代码将产生此错误：

% \begin{verbatim}
% label 1;
%
% ...
%
% try
%    if not(final) then
%      goto 1;   // 这行将导致错误
% finally
%   ...
% end;
% 1:
% ...
% \end{verbatim}
cg_e_control_flow_outside_finally=06040_E_finally 块中不允许使用控制流语句
% 在 finally 语句中不允许使用控制流语句 \var{break}、
% \var{continue} 和 \var{exit}。以下示例显示了这个问题：
% \begin{verbatim}
% ...
%   try
%      p;
%   finally
%      ...
%      exit;  // 这个 exit 是不允许的
%   end;
% ...
%
% \end{verbatim}
% 如果过程 \var{p} 引发异常，则执行 finally 块。
% 如果执行到达 exit，就不清楚该怎么做：
% 是退出过程还是搜索另一个异常处理程序。
cg_w_parasize_too_big=06041_W_参数大小超过某些 CPU 的限制
% 这表示你声明的参数超过 64K，这在其他处理器目标上可能不支持。
cg_w_localsize_too_big=06042_W_局部变量大小超过某些 CPU 的限制
% 这表示你声明的局部变量超过 32K，这在其他处理器目标上可能不支持。
cg_e_localsize_too_big=06043_E_局部变量大小超过支持的限制
% 这表示你声明的局部变量超过 32K，这个处理器不支持。
cg_e_break_not_allowed=06044_E_不允许使用 BREAK
% 你试图在循环结构外使用 \var{break}。
cg_e_continue_not_allowed=06045_E_不允许使用 CONTINUE
% 你试图在循环结构外使用 \var{continue}。
cg_f_unknown_compilerproc=06046_F_未知的编译器过程 "$1"。请检查是否使用了正确的运行时库。
% 编译器期望运行时库包含某些子程序。如果你看到此错误
% 且没有更改运行时库代码，很可能是你使用的运行时库
% 与正在使用的编译器不匹配。如果你更改了运行时库，此错误表示
% 你删除了编译器内部使用所需的子程序。
cg_f_unknown_system_type=06047_F_找不到系统类型 "$1"。请检查是否使用了正确的运行时库。
% 编译器期望运行时库包含某些类型定义。如果你看到此错误
% 且没有更改运行时库代码，很可能是你使用的运行时库
% 与正在使用的编译器不匹配。如果你更改了运行时库，此错误表示
% 你删除了编译器内部使用所需的类型。
cg_h_inherited_ignored=06048_H_忽略对抽象方法的继承调用
% 此消息仅在 Delphi 模式下出现，当你通过 \var{inherited;} 调用
% 父类的抽象方法时。该调用将被忽略。
cg_e_goto_label_not_found=06049_E_goto 标签 "$1" 未定义或已被优化掉
% goto 定义中使用的标签未定义或被不可达代码消除优化掉了。
cg_f_unknown_type_in_unit=06050_F_在单元 "$2" 中找不到类型 "$1"。请检查是否使用了正确的运行时库。
% 编译器期望运行时库包含某些类型定义。如果你看到此错误
% 且没有更改运行时库代码，很可能是你使用的运行时库
% 与正在使用的编译器不匹配。如果你更改了运行时库，此错误表示
% 你删除了编译器内部使用所需的类型。
cg_e_interprocedural_goto_only_to_outer_scope_allowed=06051_E_过程间 goto 只允许跳转到外层作用域
% 过程间的 goto 只允许从内层子程序跳转到外层子程序或
% 从子程序跳转到主程序
cg_e_labels_cannot_defined_outside_declaration_scope=06052_E_标签必须在其声明的相同作用域中定义
% 在 ISO 模式下，标签必须在其声明的相同作用域中定义。
cg_e_goto_across_procedures_with_exceptions_not_allowed=06053_E_不允许使用 goto 离开包含显式或隐式异常帧的过程
% 不能使用非局部 goto 离开使用显式或隐式异常的过程。使用自动类型
% （如 ansistring）或类构造函数的过程也受此影响。
cg_e_mod_only_defined_for_pos_quotient=06054_E_在 ISO 模式下，mod 运算符仅对正商定义
% 在 ISO pascal 中，商只允许正值：\var{n mod m} 仅在 \var{m>0} 时有效。
cg_d_autoinlining=06055_DL_自动内联：$1
% 由于启用了自动内联，编译器自动内联此子程序。
cg_e_function_not_support_by_selected_instruction_set=06056_E_所选指令集不支持使用的函数：$1
% 某些函数无法为特定指令集高效实现，一个例子是融合乘加。
% 为避免非常低效的代码，编译器在这种情况下会报错，因此要么选择另一个指令集，
% 要么用替代代码替换函数调用
cg_f_max_units_reached=06057_F_当前目标已达到最大单元数 ($1)
% 根据目标架构，单元数量是有限制的。已达到此限制。
% 只有包含初始化或终止化代码的单元才计数。
cg_n_no_inline=06058_N_标记为内联的子程序 "$1" 的调用未被内联
% 内联指令只是编译器的一个提示。有时编译器会忽略这个提示，标记为内联的子程序
% 不会被内联。在这种情况下，会给出此提示。使用 \var{-vd} 编译可能会得到更多关于
% 为什么忽略内联指令的信息。
cg_e_case_missing_value=06059_E_case 语句未处理序数值 "$1"，且没有 else/otherwise 语句
# case 语句未处理指定的值且没有 else/otherwise 语句，编译器确定
# 这个值可以传递给 case 语句。这在 ISO 和扩展 Pascal 中是编译时错误。
cg_w_case_incomplete=06060_W_case 语句未处理所有可能的情况
% case 语句没有包含操作数的所有可能值的标签，且没有 else 语句。
cg_w_cannot_compile_subroutine=06061_W_当前子程序 "$1" 无法为目标 CPU 编译，创建虚拟程序
% 某些处理器的指令集非常有限，因此某些例程无法为其编译。由于从一开始
% 并不总是清楚子程序是否可以为某个 CPU 编译，编译器会在之后检查，
% 如果无法编译子程序，则创建一个虚拟程序。
cg_w_interrupt_does_not_save_registers=06062_W_目标 CPU 不支持在子程序 "$1" 中保存寄存器
% 某些处理器没有内存（例如 avr1 系列），因此它们不支持在中断例程中
% 存储/恢复使用的寄存器。程序员必须确保在有可能调用中断例程时，
% 不执行依赖于寄存器保存的其他代码。
%
% \end{description}
# EndOfTeX
#
# Assembler reader
#
# 07147 is the last used one
#
asmr_d_start_reading=07000_DL_开始解析 $1 风格的汇编
% 这表示正在解析汇编块
asmr_d_finish_reading=07001_DL_完成 $1 风格的汇编解析
% 这表示汇编块解析已完成
asmr_e_none_label_contain_at=07002_E_非标签模式包含 @
% 不是标签的标识符不能包含 @。
asmr_e_building_record_offset=07004_E_构建记录偏移时出错
% 在构建记录/对象结构的偏移时发生错误，这可能是因为
% 没有指定任何字段或使用了未知的字段标识符。
asmr_e_offset_without_identifier=07005_E_OFFSET 使用时没有标识符
% 你只能将 OFFSET 与标识符一起使用。不支持其他语法
asmr_e_type_without_identifier=07006_E_TYPE 使用时没有标识符
% 你只能将 TYPE 与标识符一起使用。不支持其他语法
asmr_e_no_local_or_para_allowed=07007_E_此处不能使用局部变量或参数
% 你不能在此处使用局部变量或参数，主要是因为局部变量和参数的
% 寻址是使用帧指针寄存器完成的，所以无法直接获取地址。
asmr_e_need_offset=07008_E_此处需要使用 OFFSET
% 你需要在此处使用 OFFSET <id> 来获取标识符的地址。
asmr_e_need_dollar=07009_E_此处需要使用 $
% 你需要在此处使用 $<id> 来获取标识符的地址。
asmr_e_cant_have_multiple_relocatable_symbols=07010_E_不能使用多个可重定位符号
% 在一个参数中不能有多个可重定位符号（变量/类型化常量）。
asmr_e_only_add_relocatable_symbol=07011_E_可重定位符号只能进行加法运算
% 可重定位符号（变量/类型化常量）不能与其他运算符一起使用。只允许加法。
asmr_e_invalid_constant_expression=07012_E_无效的常量表达式
% 常量表达式中存在错误。
asmr_e_relocatable_symbol_not_allowed=07013_E_不允许使用可重定位符号
% 你不能在此处使用可重定位符号（变量/类型化常量）。
asmr_e_invalid_reference_syntax=07014_E_无效的引用语法
% 引用中存在错误。
asmr_e_local_para_unreachable=07015_E_无法从该代码访问 $1
% 你不能在汇编代码中直接读取更高层次过程的局部变量或参数的值
%（除非是没有参数和局部变量的局部汇编代码）。
asmr_e_local_label_not_allowed_as_ref=07016_E_不允许将局部符号/标签用作引用
% 你不能将局部符号/标签用作引用
asmr_e_wrong_base_index=07017_E_基址和索引寄存器使用无效
% 基址和索引寄存器存在错误，它们可能不正确
asmr_w_possible_object_field_bug=07018_W_对象字段处理可能存在错误
% 在 normal 或 objfpc 模式下可以直接访问对象或类的字段，
% 但在 TP 和 Delphi 模式下将字段名称视为简单偏移。
asmr_e_wrong_scale_factor=07019_E_指定的比例因子错误
% 给定的比例因子错误，只允许 1、2、4 和 8
asmr_e_multiple_index=07020_E_多重索引寄存器使用
% 你试图使用多个索引寄存器
asmr_e_invalid_operand_type=07021_E_无效的操作数类型
% 操作数类型与使用的操作码不匹配
asmr_e_invalid_string_as_opcode_operand=07022_E_作为操作码操作数的字符串无效：$1
% 指定为操作数的字符串对此操作码无效
asmr_w_CODE_and_DATA_not_supported=07023_W_不支持 @CODE 和 @DATA
% 不支持 @CODE 和 @DATA，它们将被忽略。
asmr_e_null_label_ref_not_allowed=07024_E_不允许空标签引用
asmr_e_expr_zero_divide=07025_E_汇编求值器中出现除零错误
% 常量表达式中出现除零错误
asmr_e_expr_illegal=07026_E_非法表达式
% 常量表达式中存在非法表达式
asmr_e_escape_seq_ignored=07027_E_转义序列被忽略：$1
% 存在 C 风格的字符串，但字符串中的转义序列
% 未知，因此被忽略
asmr_e_invalid_symbol_ref=07028_E_无效的符号引用
asmr_w_fwait_emu_prob=07029_W_Fwait 可能在 emu387 中导致模拟问题
asmr_w_fadd_to_faddp=07030_W_没有操作数的 $1 被转换为 $1P
asmr_w_enter_not_supported_by_linux=07031_W_Linux 内核不支持 ENTER 指令
% ENTER 指令可能生成 i386 Linux 页处理程序无法
% 正确捕获的栈页面错误。
asmr_w_calling_overload_func=07032_W_在汇编中调用重载函数
% 在汇编块中调用了重载方法，
% 这可能表明存在问题
asmr_e_unsupported_symbol_type=07033_E_操作数的符号类型不受支持
asmr_e_constant_out_of_bounds=07034_E_常量值超出范围
asmr_e_error_converting_decimal=07035_E_转换十进制数 $1 时出错
% 常量十进制值语法不正确
asmr_e_error_converting_octal=07036_E_转换八进制数 $1 时出错
% 常量八进制值语法不正确
asmr_e_error_converting_binary=07037_E_转换二进制数 $1 时出错
% 常量二进制值语法不正确
asmr_e_error_converting_hexadecimal=07038_E_转换十六进制数 $1 时出错
% 常量十六进制值语法不正确
asmr_h_direct_global_to_mangled=07039_H_$1 被转换为 $2
asmr_w_direct_global_is_overloaded_func=07040_W_$1 与重载函数关联
asmr_e_cannot_use_SELF_outside_a_method=07041_E_不能在方法外使用 SELF
% 在不允许的地方引用了 \var{self} 符号。\var{self} 只能在方法内部引用
asmr_e_cannot_use_OLDEBP_outside_nested_procedure=07042_E_不能在嵌套过程外使用 OLDEBP
% 在不允许的地方引用了 \var{oldebp} 符号。\var{oldebp} 只能在嵌套例程内部引用
asmr_e_void_function=07043_W_过程不能在汇编代码中返回任何值
% 试图在过程中返回值。过程没有任何返回值
asmr_e_SEG_not_supported=07044_E_不支持 SEG
asmr_e_size_suffix_and_dest_dont_match=07045_E_大小后缀与目标或源大小不匹配
% 寄存器大小和操作码大小后缀不匹配。这可能是
% 汇编语句中的错误
asmr_w_size_suffix_and_dest_dont_match=07046_W_大小后缀与目标或源大小不匹配
% 寄存器大小和操作码大小后缀不匹配。这可能是
% 汇编语句中的错误
asmr_e_syntax_error=07047_E_汇编语法错误
% 存在汇编语法错误
asmr_e_invalid_opcode_and_operand=07048_E_操作码和操作数组合无效
% 该操作码不能与此类型的操作数一起使用
asmr_e_syn_operand=07049_E_操作数中的汇编语法错误
asmr_e_syn_constant=07050_E_常量中的汇编语法错误
asmr_e_invalid_string_expression=07051_E_无效的字符串表达式
asmr_w_const32bit_for_address=07052_W_符号 $1 的常量用于非指针地址
% 常量表达式表示不适合指针的地址。
% 该地址可能不正确
asmr_e_unknown_opcode=07053_E_无法识别的操作码 $1
% 此操作码未知
asmr_e_invalid_or_missing_opcode=07054_E_无效或缺少操作码
asmr_e_invalid_prefix_and_opcode=07055_E_前缀和操作码组合无效：$1
asmr_e_invalid_override_and_opcode=07056_E_覆盖和操作码组合无效：$1
asmr_e_too_many_operands=07057_E_行中操作数过多
% 此操作码的操作数太多。请检查你的
% 汇编语法
asmr_w_near_ignored=07058_W_NEAR 被忽略
asmr_w_far_ignored=07059_W_FAR 被忽略
asmr_e_dup_local_sym=07060_E_重复的局部符号 $1
asmr_e_unknown_local_sym=07061_E_未定义的局部符号 $1
asmr_e_unknown_label_identifier=07062_E_未知的标签标识符 $1
asmr_e_invalid_register=07063_E_无效的寄存器名称
% 使用了未知的寄存器名称作为操作数。
asmr_e_invalid_fpu_register=07064_E_无效的浮点寄存器名称
% 使用了未知的寄存器名称作为操作数。
asmr_w_modulo_not_supported=07066_W_不支持取模运算
asmr_e_invalid_float_const=07067_E_无效的浮点常量 $1
% 在汇编块中声明的浮点常量无效。
asmr_e_invalid_float_expr=07068_E_无效的浮点表达式
% 在汇编块中声明的浮点表达式无效。
asmr_e_wrong_sym_type=07069_E_错误的符号类型
asmr_e_cannot_index_relative_var=07070_E_不能用寄存器索引局部变量或参数
% 试图使用基址寄存器索引一个已经相对于寄存器的符号。这是不可能的，
% 并且可能导致崩溃。
asmr_e_invalid_seg_override=07071_E_无效的段覆盖表达式
asmr_w_id_supposed_external=07072_W_标识符 $1 被认为是外部的
% 引用了一个未定义的符号。这不会导致错误，因为该符号可能是外部的，
% 但如果该符号在任何地方都未定义，可能会在链接时出现问题。
asmr_e_string_not_allowed_as_const=07073_E_字符串不允许作为常量
% 字符串不允许作为常量。
asmr_e_no_var_type_specified=07074_E_未指定变量类型
% 语法需要在点号后面有一个类型标识符，但没有找到。
asmr_w_assembler_code_not_returned_to_text=07075_E_汇编代码未返回到文本段
% 汇编块中有一个改变段的指令，但在汇编块结束时没有返回到文本段。
% 这可能在链接时导致错误。
asmr_e_not_directive_or_local_symbol=07076_E_$1 不是指令或局部符号
% 此符号未知。
asmr_w_using_defined_as_local=07077_E_使用已定义的名称作为局部标签
asmr_e_dollar_without_identifier=07078_E_美元符号使用时没有标识符
% 常量表达式有一个不以 $ 符号开头的标识符。
asmr_w_32bit_const_for_address=07079_W_为地址创建了32位常量
% 常量被用作地址。这可能是一个错误，因为使用绝对地址可能无法工作。
asmr_n_align_is_target_specific=07080_N_.align 是目标特定的，请使用 .balign 或 .p2align
% 使用 .align 指令是平台特定的，其含义在不同平台上会有所不同。
asmr_e_cannot_access_field_directly_for_parameters=07081_E_不能直接访问基于指针的参数的字段
% 你应该先将参数加载到寄存器中，然后使用该寄存器访问字段。
asmr_e_cannot_access_object_field_directly=07082_E_不能直接访问对象/类的字段
% 你应该先将 self 指针加载到寄存器中，然后使用该寄存器作为基址访问字段。
% 在 i386 上，默认情况下 self 指针在 esi 寄存器中。
asmr_e_unable_to_determine_reference_size=07083_E_未指定大小且无法确定操作数的大小
% 你应该明确指定引用的大小，因为编译器无法确定应该使用什么大小
% (字节、字、双字等)进行引用。
asmr_e_cannot_use_RESULT_here=07084_E_不能在此函数中使用 RESULT
% 一些返回复杂类型的函数不能使用 \var{result} 关键字。
asmr_w_adding_explicit_args_fXX=07086_W_"$1" 没有操作数被转换为 "$1 %st,%st(1)"
asmr_w_adding_explicit_first_arg_fXX=07087_W_"$1 %st(n)" 被转换为 "$1 %st,%st(n)"
asmr_w_adding_explicit_second_arg_fXX=07088_W_"$1 %st(n)" 被转换为 "$1 %st(n),%st"
asmr_e_invalid_char_smaller=07089_E_此处不允许字符 <
% 移位运算符需要 << 字符。只找到了其中一个字符。
asmr_e_invalid_char_greater=07090_E_此处不允许字符 >
% 移位运算符需要 >> 字符。只找到了其中一个字符。
asmr_w_align_not_supported=07093_W_不支持 ALIGN
asmr_e_no_inc_and_dec_together=07094_E_Inc 和 Dec 不能同时使用
% 试图在 680x0 上的同一操作码中同时使用增量和减量。这是不可能的。
asmr_e_invalid_reg_list_in_movem_or_fmovem=07095_E_MOVEM 或 FMOVEM 的寄存器列表无效
% 试图使用 \var{movem} 或 \var{fmovem} 操作码保存或恢复无效的寄存器。
asmr_e_invalid_reg_list_for_opcode=07096_E_操作码的寄存器列表无效
asmr_e_higher_cpu_mode_required=07097_E_需要更高的 CPU 模式 ($1)
% 试图使用当前 CPU 模式不支持的指令。使用更高的 CPU 代来在你的
% 汇编块中使用此操作码
asmr_w_unable_to_determine_reference_size_using_dword=07098_W_未指定大小且无法确定操作数的大小，默认使用 DWORD
% 你应该明确指定引用的大小，因为编译器无法确定应该使用什么大小
% (字节、字、双字等)进行引用。此警告仅在 Delphi 模式下使用，
% 其中默认使用 DWORD。
asmr_e_illegal_shifterop_syntax=07099_E_解析移位操作数时出现语法错误
% 仅限 ARM；ARM 汇编支持所谓的移位操作数。使用的语法不是
% 有效的移位操作数。带移位操作数的操作示例：
% \begin{verbatim}
% asm
%   orr     r2,r2,r2,lsl #8
% end;
% \end{verbatim}
asmr_e_packed_element=07100_E_打包组件的地址不在字节边界上
% 打包组件(记录字段和数组元素)可能从字节内的任意位开始。
% 在不支持位寻址内存的 CPU 上(包括 FPC 当前支持的所有 CPU)，
% 当试图索引大小不是 8 位倍数的数组元素时，你会收到错误消息。
% 访问具有此类地址的记录字段也是如此。
asmr_w_unable_to_determine_reference_size_using_byte=07101_W_未指定大小且无法确定操作数的大小，默认使用 BYTE
% 你应该明确指定引用的大小，因为编译器无法确定应该使用什么大小
% (字节、字、双字等)进行引用。此警告仅在 Delphi 模式下使用，
% 其中默认使用 BYTE。
asmr_w_no_direct_ebp_for_parameter=07102_W_此处使用 $1 访问参数无效
% 如果参数在寄存器中，使用直接的 8(%ebp) 引用函数/过程参数是无效的。
asmr_w_direct_ebp_for_parameter_regcall=07103_W_使用 $1 与 regcall 约定不兼容
% 如果参数在寄存器中，使用直接的 8(%ebp) 引用函数/过程参数是无效的。
asmr_w_direct_ebp_neg_offset=07104_W_不建议使用 $1 访问局部变量
% 不建议使用 -8(%ebp) 访问局部变量
asmr_w_direct_esp_neg_offset=07105_W_使用 $1 可能导致崩溃或值丢失
% 不建议使用 -8(%esp) 访问局部栈，因为这部分栈可能被任何函数调用
% 或中断覆盖。
asmr_e_no_vmtoffset_possible=07106_E_VMTOffset 必须与虚方法一起使用，而 "$1" 不是虚方法
% 只有虚方法才有 VMT 偏移
asmr_e_need_pic_ref=07107_E_生成 PIC，但引用不是 PIC 安全的
% 编译器已配置为生成位置无关代码(PIC)，但当前手写的汇编指令中
% 存在位置相关的引用。
asmr_e_mixing_regtypes=07108_E_寄存器集中的所有寄存器必须是相同类型和宽度
% ARM 架构上接受寄存器集作为参数的指令要求该集合中的所有寄存器
% 都是相同类型(如整数、vfp)和宽度(如单精度、双精度)。
asmr_e_empty_regset=07109_E_寄存器集不能为空
% ARM 架构上接受寄存器集作为参数的指令要求该集合至少包含一个寄存器。
asmr_w_useless_got_for_local=07110_W_@GOTPCREL 对局部符号无用且可能有危险
% 使用 @GOTPCREL 假定了一个额外的间接引用，但对于局部符号来说
% 这个间接引用并不存在，这可能导致错误的汇编代码
asmr_w_general_segment_with_constant=07111_W_通用段寄存器与常量一起使用
% 通用寄存器不应该有常量偏移，因为操作系统的内存分配可能与此不兼容。
asmr_e_bad_seh_directive_offset=07112_E_$1 的偏移值无效
% Win64 SEH 指令对可能的偏移值有某些限制，例如它们应该是正数
% 并且低 3 或 4 位应该为零。
asmr_e_bad_seh_directive_register=07113_E_$1 的寄存器无效
% Win64 SEH 指令只接受 64 位整数寄存器或 XMM 寄存器。
asmr_e_seh_in_pure_asm_only=07114_E_SEH 指令只允许在纯汇编过程中使用
% Win64 SEH 指令只允许在纯汇编过程中使用，不能在常规过程的汇编块中使用。
asmr_e_unsupported_directive=07115_E_当前目标不支持指令 "$1"
asmr_e_complex_function_result_location=07116_E_使用 "nostackframe" 时，此函数的结果位置不能直接编码为单个操作数
% 使用 \var{nostackframe} 修饰符声明的函数没有栈帧，因此没有局部变量
% 来保存临时函数结果。在这样的例程中，对函数结果的访问直接转到将用于
% 向调用者返回结果的寄存器或内存位置。在某些情况下，这个结果可能分散在
% 多个寄存器中，这种情况下不可能通过其符号名称访问该位置。
% 在这种情况下，你必须直接使用适当的寄存器名称。
asmr_e_wrong_gotpcrel_intel_syntax=07117_E_Intel 汇编语法中的 GOTPCREL 引用不能包含基址或索引寄存器，且其偏移必须为 0
% Intel 汇编语法中 gotpcrel PIC 内存表达式的语法是
% \verb*[global_symbol wrt ..gotpcrel]*
asmr_e_no_gotpcrel_support=07118_E_当前目标不支持 GOTPCREL 重定位
% 并非所有目标都支持使用全局偏移表的位置无关代码。
% 在这些情况下，请使用其他方式以位置无关的方式访问符号。
asmr_w_global_access_without_got=07119_W_导出/全局符号应通过 GOT 访问
% 在生成位置无关代码时，全局符号(来自单元接口或在程序或库中定义的符号)
% 应该通过 GOT 访问。
asmr_w_check_mem_operand_size=07120_W_检查内存操作数 "$1" 的大小
% 内存操作数的大小可能无效。这可能是汇编语句中的错误
asmr_w_check_mem_operand_size3=07121_W_检查内存操作数 "$1: 内存操作数大小为 $2 位，但期望 [$3 位]"
% 内存操作数的大小可能无效。这可能是汇编语句中的错误
asmr_w_check_mem_operand_size_offset=07122_W_检查内存操作数 "$1: 内存操作数大小为 $2 位，但期望 [$3 位 + $4 字节偏移]"
% 内存操作数的大小可能无效。这可能是汇编语句中的错误
asmr_w_check_mem_operand_negative_offset=07123_W_检查 "$1: 内存操作数的偏移为负值 "$2 字节"
% 内存操作数的偏移可能无效。这可能是汇编语句中的错误
asmr_w_check_mem_operand_automap_multiple_size=07124_W_检查 "$1: 内存操作数大小为空，但存在不同的内存大小定义 =>> 映射到 $2 (最小选项)"
% 内存操作数大小为空，且我们有不同的可能内存大小定义。请检查！
asmr_e_invalid_ref_register=07125_E_内存引用表达式中使用了无效的寄存器："$1"
% 由于 CPU 架构的限制或简单地因为没有意义，FPU、向量和有时整数寄存器
% 不能在内存引用表达式中使用。
asmr_e_seg_without_identifier=07126_E_SEG 使用时没有标识符
% 你只能将 SEG 与标识符一起使用。不支持其他语法
asmr_e_CODE_or_DATA_without_SEG=07127_E_@CODE 和 @DATA 只能与 SEG 运算符一起使用
% @CODE 和 @DATA 符号只能与 SEG 运算符一起使用
asmr_e_const16bit_for_segment=07128_E_符号 $1 的段常量空间不足(需要 16 位)
% 通过 SEG 运算符为符号指定段常量需要至少 16 位的空间。如果指定的空间
% 不足就会出现此错误，例如使用 'DB SEG symbol' 而不是 'DW SEG symbol'。
asmr_e_invalid_code_value=07129_E_.code 指令常量的值无效
% ARM 汇编器只允许使用 16 和 32 作为 .code 指令的参数
asmr_w_unable_to_determine_constant_size_using_byte=07130_W_未指定大小且无法确定常量大小，默认使用 BYTE
% 你应该明确指定引用的大小，因为编译器无法确定应该使用什么大小
% (byte、word、dword 等)来表示常量。基于其值，使用 BYTE。
asmr_w_unable_to_determine_constant_size_using_word=07131_W_未指定大小且无法确定常量大小，默认使用 WORD
% 你应该明确指定引用的大小，因为编译器无法确定应该使用什么大小
% (byte、word、dword 等)来表示常量。基于其值，使用 WORD。
asmr_e_cannot_override_es_segment=07132_E_无法覆盖 ES 段
% x86 字符串指令中 ES:[EDI] 引用的 ES 段不能被覆盖。
asmr_w_invalid_reference=07133_W_引用在此处无效(应为 "$1")
% 某些 x86 指令需要固定的源或目标引用
% (例如字符串指令的源和目标需要 [ESI] 或 [EDI])
asmr_e_address_sizes_do_not_match=07134_E_地址大小不匹配
% 在同一条指令中使用两个内存操作数时地址大小不匹配
% (例如 movs byte ptr [EDI], byte ptr [SI])
asmr_e_pop_cs_not_valid=07135_E_指令 "POP CS" 对当前目标无效
% 'pop cs' 指令只在 8086 和 8088 CPU 上工作，这些 CPU 在 i386 或 x86_64
% 目标上不受支持。
asmr_w_pop_cs_not_portable=07136_W_指令 "POP CS" 不可移植(仅在 8086 和 8088 CPU 上工作)
% 'pop cs' 指令在 8086 和 8088 之外的任何 CPU 上都不工作。
asmr_e_public_must_be_used_before_label_definition=07137_E_标签 $1 只能在定义前声明为 public
asmr_e_local_label_cannot_be_declared_public=07138_E_局部标签 $1 不能声明为 public
asmr_e_multiple_segment_overrides=07139_E_不能使用多个段覆盖
asmr_w_multiple_segment_overrides=07140_W_多个段覆盖(只有最后一个会生效)
asmr_w_segment_override_ignored_in_64bit_mode=07141_W_段基址 $1 将被生成，但在 64 位模式下被 CPU 忽略
asmr_e_mismatch_broadcasting_elements=07142_E_广播元素不匹配(期望: {$1} 实际: {$2})
asmr_e_invalid_arrangement=07143_E_无效的排列说明符 "$1"
asmr_e_a64_invalid_regset=07144_E_寄存器集中的寄存器必须连续。
asmr_e_unknown_field=07145_E_未知的字段标识符
% 内部汇编器无法找到传递的字段标识符。
asmr_e_not_supported_combination_attsuffix_memrefsize_type=07146_E_不支持的操作码组合: $1 - att-后缀类型 {$2} 和内存引用大小类型 {$3}
asmr_e_a64_regset_too_large=07147_E_寄存器集中的寄存器太多。
#
# Assembler/binary writers
#
# 08037 is the last used one
#
asmw_f_too_many_asm_files=08000_F_汇编文件太多
% 启用智能链接时，生成的汇编文件太多。请禁用智能链接。
asmw_f_assembler_output_not_supported=08001_F_不支持所选的汇编器输出
asmw_f_comp_not_supported=08002_F_不支持 Comp
asmw_f_direct_not_supported=08003_F_二进制写入器不支持直接模式
% 二进制写入器不支持直接汇编模式。
asmw_e_alloc_data_only_in_bss=08004_E_只允许在 bss 段中分配数据
asmw_f_no_binary_writer_selected=08005_F_未选择二进制写入器
asmw_e_opcode_not_in_table=08006_E_汇编: 操作码 $1 不在表中
asmw_e_invalid_opcode_and_operands=08007_E_汇编: $1 操作码和操作数组合无效
asmw_e_16bit_not_supported=08008_E_汇编: 不支持 16 位引用
asmw_e_invalid_effective_address=08009_E_汇编: 无效的有效地址
asmw_e_immediate_or_reference_expected=08010_E_汇编: 需要立即数或引用
asmw_e_value_exceeds_bounds=08011_E_汇编: $1 值超出范围 $2
asmw_e_short_jmp_out_of_range=08012_E_汇编: 短跳转超出范围 $1
asmw_e_undefined_label=08013_E_汇编: 未定义的标签 $1
asmw_e_comp_not_supported=08014_E_汇编: 此目标不支持 Comp 类型
asmw_e_extended_not_supported=08015_E_汇编: 此目标不支持扩展类型
asmw_e_duplicate_label=08016_E_汇编: 重复的标签 $1
asmw_e_redefined_label=08017_E_汇编: 重定义的标签 $1
asmw_e_first_defined_label=08018_E_汇编: 首次在此处定义
asmw_e_invalid_register=08019_E_汇编: 无效的寄存器 $1
asmw_e_16bit_32bit_not_supported=08020_E_汇编: 不支持 16 位或 32 位引用
asmw_e_64bit_not_supported=08021_E_汇编: 不支持 64 位操作数
asmw_e_bad_reg_with_rex=08022_E_汇编: 在需要 REX 前缀的指令中不能使用 AH,BH,CH 或 DH
% 仅限 x86_64: 此平台的指令编码不允许在单个指令中同时使用
% 8086 高字节寄存器(AH,BH,CH 或 DH)和 REX 前缀。
% 当指令操作数大小为 64 位时，或使用扩展的 x86_64 寄存器
% (R8-R15 或 XMM8-XMM15)时，需要 REX 前缀。
asmw_e_missing_endprologue=08023_E_缺少 .seh_endprologue 指令
% 仅限 x86_64-win64: 通常，SEH 指令由编译器内部处理。
% 但在纯汇编过程中，如果存在其他 SEH 指令，则需要 .seh_endprologue 指令。
asmw_e_prologue_too_large=08024_E_函数序言超过 255 字节
% x86_64-win64: .seh_prologue 指令必须位于函数开始后的 255 字节内。
asmw_e_handlerdata_no_handler=08025_E_.seh_handlerdata 指令前面没有 .seh_handler
% x86_64-win64: 如果使用 .seh_handlerdata 指令，则必须在同一函数中
% 之前存在 .seh_handler 指令。
asmw_f_too_many_relocations=08026_F_段 $1 的重定位数量超过 65535
% 旧的 COFF 目标将每个段的重定位数量限制为 65535，因为它们使用 2 字节字段
% 来存储重定位计数。使用较新 PECOFF 格式的目标没有此限制。
asmw_w_changing_bind_type=08027_N_符号 $1 的绑定类型在使用后从 $2 更改为 $3
asmw_h_changing_bind_type=08028_H_符号 $1 的绑定类型在使用后从 $2 更改为 $3
% 汇编器符号绑定类型在使用后被更改，这可能导致错误的代码。
% 第一个版本保留用于更改为局部标签，这是最可能导致错误代码生成的原因，
% 但目前设置为 Note 级别，因为它出现在编译器编译内部。
asmw_e_32bit_not_supported=08029_E_汇编: 不支持 32 位引用
asmw_f_code_segment_too_large=08030_F_代码段太大
asmw_f_data_segment_too_large=08031_F_数据段太大
asmw_e_instruction_not_supported_by_cpu=08032_E_所选指令集不支持该指令
asmw_e_destination_out_of_range=08033_E_汇编: $1 目标超出范围
asmw_e_illegal_use_of_rip=08034_E_汇编: RIP 不能用作索引寄存器或在引用中与其他寄存器一起使用
asmw_e_seh_invalid_data_size=08035_F_SEH 函数的函数大小非法
asmw_e_illegal_use_of_sp=08036_E_汇编: ESP/RSP 不能用作索引寄存器
asmw_e_illegal_unset_index=08037_E_Wasm32 符号 $1 无索引值错误
#
# Executing linker/assembler
#
# 09036 is the last used one
#
# BeginOfTeX
%
% \section{汇编/链接阶段的错误}
% 本节列出了编译器处理命令行或处理配置文件时出现的错误。
% \begin{description}
exec_w_source_os_redefined=09000_W_源操作系统被重新定义
% 源操作系统被重新定义。
exec_i_assembling_pipe=09001_I_正在通过管道汇编 $1
% 使用管道连接外部汇编器进行汇编。
exec_d_cant_create_asmfile=09002_E_无法创建汇编文件: $1
% 无法创建指定的文件。请检查是否有创建此文件的访问权限。
exec_e_cant_create_objectfile=09003_E_无法创建目标文件: $1 (错误代码: $2)
% 无法创建指定的文件。请检查是否有创建此文件的访问权限。
exec_e_cant_create_archivefile=09004_E_无法创建归档文件: $1
% 无法创建指定的文件。请检查是否有创建此文件的访问权限。
exec_e_assembler_not_found=09005_E_找不到汇编器 $1，切换到外部汇编
% 找不到汇编器程序。编译器将生成一个可用于汇编和链接程序的脚本。
exec_t_using_assembler=09006_T_使用汇编器: $1
% 一条信息消息，说明正在使用哪个汇编器。
exec_e_error_while_assembling=09007_E_汇编时出错，退出代码 $1
% 使用外部汇编器汇编文件时出错。
% 请查阅汇编器工具的文档以获取有关此错误的更多信息。
exec_e_cant_call_assembler=09008_E_无法调用汇编器，错误 $1，切换到外部汇编
% 调用外部汇编器时出错。编译器将生成一个可用于汇编和链接程序的脚本。
exec_i_assembling=09009_I_正在汇编 $1
% 一条信息消息，说明正在汇编哪个文件。
exec_i_assembling_smart=09010_I_正在使用智能链接汇编 $1
% 一条信息消息，说明正在使用智能链接汇编哪个文件。
exec_w_objfile_not_found=09011_W_找不到目标文件 $1，链接可能会失败！
% 缺少一个目标文件，链接可能会失败。
% 请检查路径。
exec_w_libfile_not_found=09012_W_找不到库文件 $1，链接可能会失败！
% 缺少一个库文件，链接可能会失败。
% 请检查路径。
exec_e_error_while_linking=09013_E_链接时出错
% 链接时的通用错误。
exec_e_cant_call_linker=09014_E_无法调用链接器，切换到外部链接(错误为 "$1")
% 调用外部链接器时出错。编译器将生成一个可用于汇编和链接程序的脚本。
exec_i_linking=09015_I_正在链接 $1
% 一条信息消息，显示正在链接哪个程序或库。
exec_e_util_not_found=09016_E_找不到工具 $1，切换到外部链接
% 找不到外部工具。编译器将生成一个可用于汇编、链接或后处理程序的脚本。
exec_t_using_util=09017_T_使用工具 $1
% 一条信息消息，显示正在使用哪个外部程序(通常是后处理器)。
exec_e_exe_not_supported=09018_E_不支持创建可执行文件
% 不支持为此平台创建可执行程序，因为编译器尚未实现此功能。
exec_e_dll_not_supported=09019_E_不支持创建动态/共享库
% 不支持为此平台创建动态加载库，因为编译器尚未实现此功能。
exec_i_closing_script=09020_I_正在关闭脚本 $1
% 信息消息，显示外部汇编和链接脚本的写入何时完成。
exec_e_res_not_found=09021_E_找不到资源编译器 "$1"，切换到外部模式
% 找不到外部资源编译器。编译器将生成一个可用于汇编、编译资源和链接或后处理程序的脚本。
exec_i_compilingresource=09022_I_正在编译资源 $1
% 一条信息消息，显示正在编译哪个资源。
exec_t_unit_not_static_linkable_switch_to_smart=09023_T_单元 $1 无法静态链接，切换到智能链接
% 请求了静态链接，但使用了一个不可静态链接的单元。
exec_t_unit_not_smart_linkable_switch_to_static=09024_T_单元 $1 无法智能链接，切换到静态链接
% 请求了智能链接，但使用了一个不可智能链接的单元。
exec_t_unit_not_shared_linkable_switch_to_static=09025_T_单元 $1 无法共享链接，切换到静态链接
% 请求了共享链接，但使用了一个不可共享链接的单元。
exec_e_unit_not_smart_or_static_linkable=09026_E_单元 $1 无法智能或静态链接
% 请求了智能或静态链接，但使用了一个不能用于这两种方式的单元。
exec_e_unit_not_shared_or_static_linkable=09027_E_单元 $1 无法共享或静态链接
% 请求了共享或静态链接，但使用了一个不能用于这两种方式的单元。
exec_d_resbin_params=09028_D_使用命令行 "$2" 调用资源编译器 "$1"
% 一条信息消息，显示用于资源编译器的命令行。
exec_e_error_while_compiling_resources=09029_E_编译资源时出错
% 资源编译器或转换器返回了一个错误。
exec_e_cant_call_resource_compiler=09030_E_无法调用资源编译器 "$1"，切换到外部模式
% 调用资源编译器时出错。编译器将生成一个可用于汇编、编译资源和链接或后处理程序的脚本。
exec_e_cant_open_resource_file=09031_E_无法打开资源文件 "$1"
% 无法打开资源文件时出错。
exec_e_cant_write_resource_file=09032_E_无法写入资源文件 "$1"
% 无法写入资源文件时出错。
exec_n_backquote_cat_file_not_found=09033_N_找不到反引号 cat 命令的文件 "$1"
% 编译器找不到应该展开为链接器参数的文件
exec_w_init_file_not_found=09034_W_找不到 "$1"，这可能会导致链接失败
% 编译器只在找到某些启动代码文件时才会将它们添加到链接器。
% 如果找不到它们，它们就不会被添加，这可能会导致链接失败。
% 如果系统安装了 gcc，运行 \var{gcc --print-file-name <filename>} 可能会返回文件的路径。
% 在 \var{fpc.cfg} 中使用 \var{-Fl} 开关添加此路径可以修复此错误。不过，这要求
% gcc 的目标与 FPC 的目标相同。
exec_e_static_lib_not_supported=09035_E_不支持创建静态库
% 不支持为此平台创建静态库，因为编译器尚未实现此功能。
exec_f_controllertype_expected=09036_F_要生成正确的链接器调用，必须通过指令 -Wp 设置控制器类型
% Xtensa 微控制器需要一个依赖于目标控制器的详细规范链接器命令。
% 如果没有设置目标控制器，就无法构建此命令，因此无法进行链接。
%
% \end{description}
# EndOfTeX

#
# Executable information
#
# 09134 is the last used one
#
# BeginOfTeX
% \section{可执行文件信息消息}
% 本节列出了编译器在生成可执行程序时发出的所有消息，
% 且仅在使用内部链接器时发出。
% \begin{description}
execinfo_f_cant_process_executable=09128_F_无法后处理可执行文件 $1
% 编译器无法后处理可执行文件时的致命错误。
execinfo_f_cant_open_executable=09129_F_无法打开可执行文件 $1
% 编译器无法打开可执行文件时的致命错误。
execinfo_x_codesize=09130_X_代码大小：$1 字节
% 显示生成的代码段大小的信息消息。
execinfo_x_initdatasize=09131_X_已初始化数据大小：$1 字节
% 显示已初始化数据段大小的信息消息。
execinfo_x_uninitdatasize=09132_X_未初始化数据大小：$1 字节
% 显示未初始化数据段大小的信息消息。
execinfo_x_stackreserve=09133_X_已保留的栈空间：$1 字节
% 显示编译器为可执行文件保留的栈大小的信息消息。
execinfo_x_stackcommit=09134_X_已提交的栈空间：$1 字节
% 显示编译器为可执行文件提交的栈大小的信息消息。
% \end{description}
# EndOfTeX

#
# Internal linker messages
#
# 09224 is the last used one
#
# BeginOfTeX
% \section{链接器消息}
% 本节列出了内部链接器产生的消息。
% \begin{description}
link_f_executable_too_big=09200_F_可执行文件映像对于 $1 目标来说太大。
% 当生成的可执行文件太大时的致命错误。
link_w_32bit_absolute_reloc=09201_W_对象文件 "$1" 包含指向符号 "$2" 的32位绝对重定位。
% 当64位对象文件包含32位绝对重定位时的警告。
% 在这种情况下，可执行文件映像只能加载到地址空间的低4GB中。
link_e_program_segment_too_large=09202_E_程序段太大（超出64k $1 字节）
% 当16位程序在微型内存模型下编译，但其大小超过64k时的错误
link_e_code_segment_too_large=09203_E_代码段 "$1" 太大（超出64k $2 字节）
% 当16位程序的代码段超过64k字节时的错误
link_e_data_segment_too_large=09204_E_数据段 "$1" 太大（超出64k $2 字节）
% 当16位程序的数据段超过64k字节时的错误
link_e_segment_too_large=09205_E_段 "$1" 太大（超出64k $2 字节）
% 当16位程序包含超过64k字节的段时的错误
link_e_group_too_large=09206_E_组 "$1" 太大（超出64k $2 字节）
% 当16位程序的对象模块定义的段组超过64k字节时的错误
link_e_com_program_uses_segment_relocations=09207_E_无法创建 .COM 文件，因为程序包含段重定位
% 当创建微型模型 DOS .COM 文件时，如果程序的对象模块中至少有一个包含段重定位就会发生错误。
% 段重定位可能由使用 Seg() 函数或 SEG 汇编指令（无论是在 pascal 的内置内联汇编中，
% 还是在外部链接的汇编模块中）引起。
link_w_program_uses_checkpointer=09208_W_程序 "$1" 使用实验性的 CheckPointer 选项
link_e_duplicate_symbol=09209_E_多重定义的符号 "$1"
% 指定的符号在整个对象文件集合中已经定义。
link_e_comdat_select_unsupported=09210_E_不支持 COMDAT 选择模式 $1（段："$1"）
% 不支持指定的 COMDAT 选择模式。
link_e_comdat_associative_section_expected=09211_E_COMDAT 段 "$1" 需要关联段
% 指定的 COMDAT 段被指定为需要关联段，但未指定。
link_e_comdat_not_matching=09212_E_段 "$1" 和符号 "$2" 的 COMDAT 段选择模式不匹配
% 所有 COMDAT 符号/段需要使用相同的选择模式。
link_e_comdat_associative_section_not_found=09213_E_找不到段 "$1" 的关联 COMDAT 段
% COMDAT 段需要一个关联段，但在对象文件中未找到。
link_d_comdat_discard_any=09214_D_由于 COMDAT 选择模式，丢弃重复的符号 "$1"
% COMDAT 段指定可以选择任何具有相同名称的段，此特定段被选择丢弃。
link_d_comdat_discard_size=09215_D_由于 COMDAT 选择模式，丢弃具有相同大小的重复符号 "$1"
% COMDAT 段指定可以选择任何具有相同名称和大小的段，此特定段被选择丢弃。
link_d_comdat_discard_content=09216_D_由于 COMDAT 选择模式，丢弃具有相同内容的重复符号 "$1"
% COMDAT 段指定可以选择任何具有相同名称和内容的段，此特定段被选择丢弃。
link_d_comdat_replace_size=09217_D_由于 COMDAT 选择模式，用较小大小替换重复符号 "$1"
% COMDAT 段指定应选择具有相同名称的最大段，此特定段比之前最大的段大。
link_e_comdat_size_differs=09218_E_重复的 COMDAT 符号 "$1" 大小不同
% COMDAT 段指定具有相同名称的所有段需要具有相同的大小，但此段具有不同的大小。
link_e_comdat_content_differs=09219_E_重复的 COMDAT 符号 "$1" 内容不同
% COMDAT 段指定具有相同名称的所有段需要具有相同的内容，但此段具有不同的大小。
link_e_comdat_selection_differs=09220_E_符号 "$1" 的 COMDAT 选择模式不同
% 具有相同名称的两个 COMDAT 段具有不同的 COMDAT 选择模式。
link_e_undefined_symbol_in_obj=09221_E_未定义的符号：$1（首次在 $2 中出现）
% 指定的符号被使用但未定义，首次在指定的对象文件中出现。
link_e_undefined_symbol=09222_E_未定义的符号：$1
% 指定的符号被使用但未定义。
link_w_unsupported_cross_endian_internal_linker=09223_W_交叉字节序配置下的内部链接器仍在开发中
% 内部链接器不能正确处理字节序转换。
link_f_executable_too_big_exceeds_X_by_Y_bytes=09224_F_可执行文件映像对于 $1 目标来说太大（超出 $2 $3 字节）。
% 结果文件太大
% \end{description}
# EndOfTeX

#
# Unit loading
#
# 10070 is the last used one
#
# BeginOfTeX
% \section{单元加载消息}
% 本节列出了当编译器从磁盘加载单元到内存时可能出现的所有消息。
% 其中许多是信息性消息。
% \begin{description}
unit_t_unitsearch=10000_T_单元搜索：$1
% 当你使用 \var{-vt} 选项时，编译器会告诉你它在哪里尝试查找单元文件。
unit_t_ppu_loading=10001_T_正在加载 PPU $1
% 当使用 \var{-vt} 开关时，编译器会告诉你它加载了哪些单元。
unit_u_ppu_name=10002_U_PPU 名称：$1
% 当你使用 \var{-vu} 标志时，会显示单元名称。
unit_u_ppu_flags=10003_U_PPU 标志：$1
% 当你使用 \var{-vu} 标志时，会显示单元标志。
unit_u_ppu_crc=10004_U_PPU CRC：$1
% 当你使用 \var{-vu} 标志时，会显示单元 CRC 校验。
unit_u_ppu_time=10005_U_PPU 时间：$1
% 当你使用 \var{-vu} 标志时，会显示单元的编译时间。
unit_u_ppu_file_too_short=10006_U_PPU 文件太短
% PPU 文件太短，不是所有声明都存在。
unit_u_ppu_invalid_header=10007_U_PPU 无效的头部（开头没有 PPU）
% 单元文件的前三个字节应该是字符 \var{PPU} 的 ASCII 码。
unit_u_ppu_invalid_version=10008_U_PPU 版本无效 $1
% 此单元文件是用不同版本的编译器编译的，无法读取。
unit_u_ppu_invalid_processor=10009_U_PPU 是为其他处理器 $1 编译的
% 此单元文件是为不同的处理器类型编译的，无法读取。
unit_u_ppu_invalid_target=10010_U_PPU 是为其他目标 $1 编译的
% 此单元文件是为不同的目标编译的，无法读取。
unit_u_ppu_source=10011_U_PPU 源文件：$1
% 当你使用 \var{-vu} 标志时，会显示单元源文件名。
unit_u_ppu_write=10012_U_正在写入 $1
% 当你指定 \var{-vu} 开关时，编译器会告诉你它在哪里写入单元文件。
unit_f_ppu_cannot_write=10013_F_无法写入 PPU 文件
% 写入单元文件时发生错误。
unit_f_ppu_read_error=10014_F_读取 PPU 文件时出错
% 这意味着单元文件已损坏，包含无效信息。需要重新编译。
unit_f_ppu_read_unexpected_end=10015_F_PPU 文件意外结束
% 文件意外结束。这可能意味着 PPU 文件已损坏。
unit_f_ppu_invalid_entry=10016_F_无效的 PPU 文件条目：$1
% 编译器试图读取的单元已损坏，或由较新版本的编译器生成。
unit_f_ppu_dbx_count_problem=10017_F_PPU Dbx 计数问题
% 单元的调试信息中存在不一致。
unit_e_illegal_unit_name=10018_E_非法的单元名称：$1（期望 $2）
% 单元的名称与文件名不匹配。
% 可能有两个原因：单元名称中有拼写错误，或者存在一个 8.3 名称的单元，其 8 个字符与较长名称单元的前 8 个字符相同。但是找不到该单元。例如：程序包含 \var{uses mytestunit;}，但 mytestunit 的单元文件或源代码不可用，但存在名为 \var{mytestun} 的源文件。然后编译器尝试编译并使用该文件，但预期的单元名称不匹配。
unit_f_too_much_units=10019_F_单元太多
% \fpc 在一个程序中最多允许 1024 个单元。你可以通过修改编译器 \file{fmodule.pas} 文件中的 \var{maxunits} 常量并重新编译编译器来更改此行为。
unit_f_circular_unit_reference=10020_F_$1 和 $2 之间存在循环单元引用
% 两个单元在接口部分相互使用。这只允许在 \var{implementation} 部分中。至少一个单元必须在 \var{implementation} 部分包含另一个。
unit_f_cant_compile_unit=10021_F_无法编译单元 $1，没有可用的源代码
% 发现需要重新编译的单元，但没有可用的源代码。
unit_f_cant_find_ppu=10022_F_找不到 $2 使用的单元 $1
% 你试图使用一个编译器找不到其 PPU 文件的单元。检查配置文件中的单元路径。
unit_w_unit_name_error=10023_W_未找到单元 $1，但存在 $2
% 此错误消息不再使用。
unit_f_unit_name_error=10024_F_搜索单元 $1 但找到 $2
% 单元 PPU 文件的 \dos 8 字母截断可能在单元名称超过 8 个字母时导致问题。
unit_w_switch_us_missed=10025_W_编译系统单元需要 -Us 开关
% 重新编译系统单元（需要特殊处理）时，必须指定 \var{-Us} 开关。
unit_f_errors_in_unit=10026_F_编译模块时有 $1 个错误，停止
% 当编译器在模块中遇到致命错误或太多错误时，会显示此消息并停止。
unit_u_load_unit=10027_U_从 $1 ($2) 加载单元 $3
% 当你使用 \var{-vu} 标志时，会显示从哪个单元加载了哪个单元。
unit_u_recompile_crc_change=10028_U_重新编译 $1，$2 的校验和已更改
% 由于它依赖的单元的校验和已更改，该单元需要重新编译。
unit_u_recompile_source_found_alone=10029_U_重新编译 $1，仅找到源代码
% 当你使用 \var{-vu} 标志时，这些消息告诉你当前单元为什么要重新编译。
unit_u_recompile_staticlib_is_older=10030_U_重新编译单元，静态库比 PPU 文件旧
% 当你使用 \var{-vu} 标志时，如果单元的静态库比单元文件本身旧，编译器会发出警告。
unit_u_recompile_sharedlib_is_older=10031_U_重新编译单元，共享库比 PPU 文件旧
% 当你使用 \var{-vu} 标志时，如果单元的共享库比单元文件本身旧，编译器会发出警告。
unit_u_recompile_obj_and_asm_older=10032_U_重新编译单元，obj 和 asm 文件比 PPU 文件旧
% 当你使用 \var{-vu} 标志时，如果单元的汇编或目标文件比单元文件本身旧，编译器会发出警告。
unit_u_recompile_obj_older_than_asm=10033_U_重新编译单元，obj 文件比 asm 文件旧
% 当你使用 \var{-vu} 标志时，如果单元的汇编文件比目标文件新，编译器会发出警告。
unit_u_parsing_interface=10034_U_正在解析 $1 的接口部分
% 当你使用 \var{-vu} 标志时，编译器会警告它开始解析单元的接口部分。
unit_u_parsing_implementation=10035_U_正在解析 $1 的实现部分
% 当你使用 \var{-vu} 标志时，编译器会警告它开始解析单元的实现部分。
unit_u_second_load_unit=10036_U_第二次加载单元 $1
% 当你使用 \var{-vu} 标志时，编译器会警告它开始第二次重新编译一个单元。这在相互依赖的单元中可能发生。
unit_u_check_time=10037_U_PPU 检查文件 $1 时间 $2
% 当你使用 \var{-vu} 标志时，编译器会显示文件名以及重新编译所依赖的文件的日期和时间。
### 以下两个错误消息当前已禁用。
#unit_h_cond_not_set_in_last_compile=10038_H_条件 $1 在上次编译 $2 时未在启动时设置
#% 当需要重新编译单元时，编译器会检查重新编译时是否设置了相同的条件。编译器发现
#% 当前定义了一个条件，但在上次编译单元时未使用。
#unit_h_cond_set_in_last_compile=10039_H_条件 $1 在上次编译 $2 时在启动时已设置
#% 当需要重新编译单元时，编译器会检查重新编译时是否设置了相同的条件。编译器发现
#% 上次编译单元时使用了一个条件，但该条件当前未定义。
unit_w_cant_compile_unit_with_changed_incfile=10040_W_无法重新编译单元 $1，但发现已修改的包含文件
% 发现单元有已修改的包含文件，但找不到某些源文件，因此无法重新编译。
unit_u_source_modified=10041_U_文件 $1 比用于创建 PPU 文件 $2 的文件更新
% 发现编译器单元的源文件已修改。
unit_u_ppu_invalid_fpumode=10042_U_尝试使用以不同 FPU 模式编译的单元
% 尝试编译代码时使用了未以相同浮点格式模式编译的单元。所有代码都应该使用 FPU 仿真开启或关闭来编译。
unit_u_loading_interface_units=10043_U_正在从 $1 加载接口单元
% 当你使用 \var{-vu} 标志时，编译器会警告它正在开始加载单元接口部分中定义的单元。
unit_u_loading_implementation_units=10044_U_正在从 $1 加载实现单元
% 当你使用 \var{-vu} 标志时，编译器会警告它正在开始加载单元实现部分中定义的单元。
unit_u_interface_crc_changed=10045_U_单元 $1 的接口 CRC 已更改
% 当你使用 \var{-vu} 标志时，编译器会警告在解析实现部分后，为接口计算的 CRC 已更改。
unit_u_implementation_crc_changed=10046_U_单元 $1 的实现 CRC 已更改
% 当你使用 \var{-vu} 标志时，编译器会警告在解析实现部分后，
% 为实现计算的 CRC 已更改。
unit_u_finished_compiling=10047_U_完成编译单元 $1
% 当你使用 \var{-vu} 标志时，编译器会警告它已完成编译该单元。
unit_u_add_depend_to=10048_U_添加依赖关系：$1 依赖于 $2
% 当你使用 \var{-vu} 标志时，编译器会警告它已在两个单元之间添加了依赖关系。
unit_u_no_reload_is_caller=10049_U_不重新加载，是调用者：$1
% 当你使用 \var{-vu} 标志时，编译器会警告它不会重新加载该单元，因为它是想要加载此单元的单元。
unit_u_no_reload_in_second_compile=10050_U_不重新加载，已在第二次编译中：$1
% 当你使用 \var{-vu} 标志时，编译器会警告它不会重新加载该单元，因为它已经在第二次重新编译中。
unit_u_flag_for_reload=10051_U_标记为重新加载：$1
% 当你使用 \var{-vu} 标志时，编译器会警告它必须重新加载该单元。
unit_u_forced_reload=10052_U_强制重新加载
% 当你使用 \var{-vu} 标志时，编译器会警告它正在重新加载该单元，因为这是必需的。
unit_u_previous_state=10053_U_$1 的先前状态：$2
% 当你使用 \var{-vu} 标志时，编译器会显示该单元的先前状态。
unit_u_second_compile_unit=10054_U_已在编译 $1，设置为第二次编译
% 当你使用 \var{-vu} 标志时，编译器会警告它正在开始第二次重新编译一个单元。这在相互依赖的单元中可能发生。
unit_u_loading_unit=10055_U_正在加载单元 $1
% 当你使用 \var{-vu} 标志时，编译器会警告它开始加载该单元。
unit_u_finished_loading_unit=10056_U_完成加载单元 $1
% 当你使用 \var{-vu} 标志时，编译器会警告它已完成加载该单元。
unit_u_registering_new_unit=10057_U_正在注册新单元 $1
% 当你使用 \var{-vu} 标志时，编译器会警告它已找到一个新单元并正在将其注册到内部列表中。
unit_u_reresolving_unit=10058_U_正在重新解析单元 $1
% 当你使用 \var{-vu} 标志时，编译器会警告它必须重新计算该单元的内部数据。
unit_u_skipping_reresolving_unit=10059_U_跳过重新解析单元 $1，仍在加载使用的单元
% 当你使用 \var{-vu} 标志时，编译器会警告它正在跳过重新计算该单元的内部数据，因为没有数据需要重新计算。
unit_u_unload_resunit=10060_U_正在卸载资源单元 $1（不需要）
% 当你使用 \var{-vu} 标志时，编译器会警告它正在卸载资源处理单元，因为没有使用任何资源。
unit_e_different_wpo_file=10061_E_单元 $1 是使用不同的全程序优化反馈输入编译的（$2，$3）；请不使用 wpo 重新编译或在此次编译中使用相同的 wpo 反馈输入文件
% 当一个单元是使用特定的全程序优化（wpo）反馈文件（\var{-FW<x>} \var{-OW<x>}）编译时，
% 这个编译版本的单元是为该特定编译场景专门化的，不能在任何其他上下文中使用。在另一个程序中使用它或使用另一个 wpo 反馈输入文件之前，必须重新编译它。
unit_u_indirect_crc_changed=10062_U_单元 $1 的间接接口（对象/类）CRC 已更改
% 当你使用 \var{-vu} 标志时，编译器会警告为该单元计算的间接 CRC（这是该单元在接口中直接或间接使用的所有类/对象/接口等的 CRC）
% 在解析实现部分后已更改。
unit_u_ppu_invalid_memory_model=10063_U_PPU 是为另一个 i8086 内存模型编译的
% 此单元文件是为不同的 i8086 内存模型编译的，无法读取。
unit_u_loading_from_package=10064_U_正在从包 $2 加载单元 $1
% 该单元是从包中加载的。
cg_f_internal_type_not_found=10065_F_找不到内部类型"$1"。请检查是否使用了正确的运行时库。
% 编译器期望运行时库包含某些类型。如果你看到此错误且没有更改运行时库代码，很可能你使用的运行时库与正在使用的编译器不匹配。
% 如果你更改了运行时库，此错误意味着你删除了编译器内部使用所需的类型。
cg_f_internal_type_does_not_match=10066_F_内部类型"$1"与预期不符。请检查是否使用了正确的运行时库。
% 编译器期望运行时库包含某些类型。如果你看到此错误且没有更改运行时库代码，很可能你使用的运行时库与正在使用的编译器不匹配。
% 如果你更改了运行时库，此错误意味着你更改了编译器内部使用所需且必须具有特定结构的类型。
unit_u_ppu_llvm_mismatch=10067_U_跳过单元，PPU 和编译器必须都使用或都不使用 LLVM 支持进行编译
% 由使用 LLVM 代码生成器构建的编译器编译的单元不能与常规编译器一起使用，反之亦然。
unit_u_ppu_invalid_wasm_exceptions_mode=10068_U_PPU 是为另一个 WebAssembly 异常模式编译的
% 此单元文件是为不同的 WebAssembly 异常模式编译的。
unit_u_ppu_symansistr_mismatch=10069_U_跳过单元，PPU 和编译器必须都使用或都不使用 SYMANSISTR 支持进行编译
% 由使用 -dSYMANSISTR 构建的编译器编译的单元不能与不使用该选项构建的常规编译器一起使用，反之亦然。
unit_u_ppu_wasm_threads_mismatch=10070_U_PPU 和程序必须都使用或都不使用 WebAssembly 多线程支持进行编译
% 用户正在使用启用多线程的方式编译程序，但该单元是在禁用多线程的情况下编译的，或相反。
unit_w_unsupported_esp_idf_version=10071_W_不支持的 esp-idf 版本
% FPC 仅支持某些 ESP-IDF 版本。如果出现此警告，生成的程序很可能无法工作。
% \end{description}
# EndOfTeX

#
#  Options
#
# 11068 is the last used one
#
# BeginOfTeX
%
% \section{命令行处理错误}
% 本节列出了编译器在处理命令行或处理配置文件时可能出现的错误。
% \begin{description}
option_usage=11000_O_$1 [选项] <输入文件> [选项]
option_only_one_source_support=11001_W_仅支持一个源文件，将要编译的源文件从"$1"更改为"$2"
% 你只能在命令行上指定一个源文件。将编译最后一个文件，其他文件将被忽略。
% 这可能表明你忘记了 \var{'-'} 符号。
option_def_only_for_os2=11002_W_DEF 文件只能为 OS/2 创建
% 此选项只能在为 OS/2 编译时指定。
option_no_nested_response_file=11003_E_不支持嵌套响应文件
% 你不能在 \var{@file} 命令行选项中嵌套响应文件。
option_no_source_found=11004_F_命令行中没有源文件名
% 编译器期望在命令行中有一个源文件名。
option_no_option_found=11005_N_$1 配置文件中没有选项
% 编译器在该配置文件中没有找到任何选项。
option_illegal_para=11006_E_非法参数：$1
% 你指定了一个未知的选项。
option_help_pages_para=11007_H_-? 显示帮助页面
% 当给出未知选项时，会显示此消息。
option_too_many_cfg_files=11008_F_嵌套的配置文件过多
% 你最多只能嵌套 16 个配置文件。
option_unable_open_file=11009_F_无法打开文件 $1
% 找不到选项文件。
option_reading_further_from=11010_D_正在从 $1 读取更多选项
% 当你启用了注释时，编译器切换到另一个选项文件时会显示此消息。
option_target_is_already_set=11011_W_目标已设置为：$1
% 当指定了多个 \var{-T} 选项时显示。
option_no_shared_lib_under_dos=11012_W_DOS 平台不支持共享库，正在恢复为静态库
% 如果你为 \dos 平台指定 \var{-CD}，会显示此消息。
% 编译器在 \dos 下只支持静态库。
option_too_many_ifdef=11013_F_在选项文件 $1 的第 $2 行遇到过多的 #IF(N)DEF
% 选项文件中的 \var{\#IF(N)DEF} 语句与 \var{\#ENDIF} 语句不匹配。
option_too_many_endif=11014_F_在选项文件 $1 的第 $2 行遇到意外的 #ENDIF
% 选项文件中的 \var{\#IF(N)DEF} 语句与 \var{\#ENDIF} 语句不匹配。
option_too_less_endif=11015_F_选项文件末尾存在未闭合的条件语句
% 选项文件中的 \var{\#IF(N)DEF} 语句与 \var{\#ENDIF} 语句不匹配。
option_no_debug_support=11016_W_此可执行文件不支持调试信息生成
% 可能存在不支持生成调试信息的编译器可执行文件。如果你在使用这样的可执行文件时使用 \var{-g} 开关，将显示此警告。
option_no_debug_support_recompile_fpc=11017_H_尝试使用 -dGDB 重新编译
% 可能存在不支持生成调试信息的编译器可执行文件。如果你在使用这样的可执行文件时使用 \var{-g} 开关，将显示此警告。
option_obsolete_switch=11018_W_你正在使用过时的开关 $1
% 这会在你使用不再需要/支持的开关时警告你。
% 建议你删除该开关以避免将来出现问题，因为开关的含义可能会改变。
option_obsolete_switch_use_new=11019_W_你正在使用过时的开关 $1，请使用 $2
% 这会在你使用不再支持的开关时警告你。你现在必须使用第二个开关代替。
% 建议你更改开关以避免将来出现问题，因为开关的含义可能会改变。
option_switch_bin_to_src_assembler=11020_N_正在将汇编器切换为默认的源代码写入汇编器
% 这会通知你汇编器已被更改，因为你使用了 -a 开关，该开关不能与二进制汇编器写入器一起使用。
option_incompatible_asm=11021_W_选择的汇编器输出"$1"与"$2"不兼容
option_asm_forced=11022_W_强制使用"$1"汇编器
% 选择的汇编器输出无法生成正确格式的目标文件。
% 因此，将使用此目标的默认汇编器。
option_using_file=11026_T_正在从文件 $1 读取选项
% 选项也从此文件中读取。
option_using_env=11027_T_正在从环境变量 $1 读取选项
% 选项也从此环境字符串中读取。
option_handling_option=11028_D_正在处理选项"$1"
% 找到并将处理选项的调试信息。
option_help_press_enter=11029_O_*** 按回车键 ***
% 按页显示帮助时显示的消息。按 ENTER 键时，将显示下一页帮助。如果按 q 然后按 ENTER，编译器将退出。
option_start_reading_configfile=11030_H_开始读取配置文件 $1
% 开始解析配置文件。
option_end_reading_configfile=11031_H_结束读取配置文件 $1
% 结束解析配置文件。
option_interpreting_option=11032_D_正在解释选项"$1"
% 编译器正在解释一个选项
option_interpreting_firstpass_option=11036_D_正在解释首次传递选项"$1"
% 编译器首次解释一个选项。
option_interpreting_file_option=11033_D_正在解释文件选项"$1"
% 编译器正在解释从配置文件中读取的选项。
option_read_config_file=11034_D_正在读取配置文件"$1"
% 编译器正在开始读取配置文件。
option_found_file=11035_D_找到源文件名"$1"
% 关于选项的附加信息。
% 当你启用调试选项时显示。
option_code_page_not_available=11039_E_未知的代码页"$1"
% 请求了一个未知的源文件代码页。
% 编译器编译时内置了对多个代码页的支持。
% 请求的代码页不在该列表中。你需要重新编译编译器以支持你需要的代码页。
option_config_is_dir=11040_F_配置文件 $1 是一个目录
% 目录不能用作配置文件。
option_confict_asm_debug=11041_W_选择的汇编器输出"$1"无法生成调试信息，已禁用调试
% 选择的汇编器输出无法生成调试信息，因此禁用了调试选项。
option_ppc386_deprecated=11042_W_使用 ppc386.cfg 已过时，请改用 fpc.cfg
% 出于历史原因仍支持使用 ppc386.cfg，但对于多平台系统来说，这种命名已不再有意义。请继续使用 fpc.cfg。
option_else_without_if=11043_F_在选项文件 $1 的第 $2 行发现没有 #IF(N)DEF 的 #ELSE 指令
% 在选项文件中发现了一个 \var{\#ELSE} 语句，但没有匹配的 \var{\#IF(N)DEF} 语句。
option_unsupported_target=11044_F_选项"$1"在当前目标平台上不支持或尚未支持
% 并非所有选项都在所有目标平台上受支持或已实现。此消息通知你所选择的选项与当前选择的目标平台不兼容。
option_unsupported_target_for_feature=11045_F_功能"$1"在所选目标平台上不支持或尚未支持
% 并非所有功能都在所有目标平台上受支持或已实现。此消息通知你所选择的功能与当前选择的目标平台不兼容。
option_dwarf_smart_linking=11046_N_在此目标上 DWARF 调试信息不能与智能链接一起使用，正在切换到静态链接
% 在大多数平台上，智能链接目前与 DWARF 调试信息不兼容，因此在这种情况下禁用智能链接。
option_ignored_target=11047_W_选项"$1"在当前目标平台上被忽略。
% 并非所有选项都在所有目标平台上受支持或已实现。此消息通知你所选择的选项在当前选择的目标平台上被忽略。
option_debug_external_unsupported=11048_W_正在禁用外部调试信息，因为所选目标/调试格式组合不支持它。
% 并非所有调试格式都可以在所有平台上存储在外部文件中。特别是在 Mac OS X 上，只有 DWARF 调试信息可以存储在外部。
option_dwarf_smartlink_creation=11049_N_在使用外部汇编器时，DWARF 调试信息不能与智能链接一起使用，正在禁用静态库创建。
% 在大多数平台上，智能链接目前与 DWARF 调试信息不兼容，因此在这种情况下禁用智能链接。
option_invalid_macosx_deployment_target=11050_E_MACOSX_DEPLOYMENT_TARGET 环境变量的值无效：$1
option_invalid_iphoneos_deployment_target=11051_E_IPHONEOS_DEPLOYMENT_TARGET 环境变量的值无效：$1
% 在 Mac OS X 上，MACOSX\_DEPLOYMENT\_TARGET/IPHONEOS\_DEPLOYMENT\_TARGET 环境变量可用于设置默认目标操作系统版本。
% 对于 Mac OS X，它必须是 XY.Z 或 XY.Z.AB 格式，其中 X、Y、Z、A 和 B 都是 0-9 的数字。
% 对于 iOS，它必须是 X.Z.A 格式，其中 X、Z 和 A 都可以是 0-9 的 1 或 2 位数字。
option_illegal_fpu_eabihf=11052_E_使用 EABIHF ABI 目标时必须使用 VFP FPU 类型之一
% EABIHF (VFP hardfloat) ABI 目标只能与 VFP FPU 一起使用。
option_w_unsupported_debug_format=11053_W_所选调试格式在当前目标平台上不支持，保持当前设置不变
% 并非所有目标平台都支持所有调试格式(特别是 Stabs 在 64 位目标平台上不支持)。
option_missing_arg=11054_E_"$1"缺少参数
% 当参数后必须跟随一个参数值时显示。
option_malformed_para=11055_E_参数格式错误: $1
% 给定的参数对该参数无效。
option_smart_link_requires_external_linker=11056_W_智能链接需要外部链接器
option_com_files_require_tiny_model=11057_E_在当前内存模型下不支持创建 .COM 文件。只有 tiny 内存模型支持创建 .COM 文件。
% 如果给定了 -Ur 选项，请不要启用实验性的 -gc 选项。
option_gc_incompatible_with_release_flag=11058_W_由于与 -Ur 选项不兼容，未启用实验性的 CheckPointer 选项。
% 编译器二进制文件只支持单一目标架构。如果你想为不同的目标架构选择编译器二进制文件，请调用 fpc 二进制文件。
option_invalid_target_architecture=11059_E_不支持的目标架构 -P$1，请改用"fpc"编译器驱动程序。
% ppc<target> 可执行文件只支持单一目标架构。它们不支持 -P 开关。编译器驱动程序"fpc"
% 处理这个开关，所以你必须使用"fpc"来进行编译
option_features_only_for_system_unit=11060_E_功能开关仅在编译系统单元时支持。
% 要选择某个功能，系统单元必须在启用该功能的情况下编译。所有其他单元通过系统单元的 ppu 继承系统单元设置的功能。
option_debug_info_requires_external_linker=11061_N_内部链接器不支持所选的调试格式，正在切换到外部链接
option_valgrind_heaptrc_mismatch=11062_E_不能同时使用这两个选项 ($1) ($2)。
option_unsupported_fpu=11063_F_所选指令集"$2"不支持所选的 FPU 类型"$1"
% 并非所有指令集都支持所有 FPU 类型。例如在 ARM 上，Thumb(-1) 不支持任何 FPU/VFP 指令集
option_too_many_exception_modes=11064_E_只能指定一种 WebAssembly 异常支持模式。
% 只能指定一种 WebAssembly 异常支持模式(NOEXCEPTIONS、JSEXCEPTIONS、BFEXCEPTIONS 或 NATIVEEXCEPTIONS)。
option_subtarget_is_already_set=11065_W_子目标已设置为: $1
% 当指定了多个 \var{-t} 选项时显示。
option_subtarget_config_not_found=11066_E_指定了子目标 $1 但未找到对应的配置文件 $2。
% 当指定了多个 \var{-t} 选项时显示。
option_x_ignored=11067_N_忽略编译器可执行文件后缀 $1。
% 当指定了多个 \var{-t} 选项时显示。
option_switch_bin_to_src_assembler_cross_endian=11068_N_在交叉字节序配置中切换汇编器为默认源代码写入汇编器
% 这通知你汇编器已被更改，因为
% 二进制汇编器写入器尚未准备好用于交叉字节序配置。
%
% \end{description}
# EndOfTeX

#
#  Whole program optimization
#
# 12019 is the last used one
#
# BeginOfTeX
%
% \section{Whole program optimization messages}
% This section lists errors that occur when the compiler is performing
% whole program optimization.
% \begin{description}
wpo_cant_find_file=12000_F_无法打开整体程序优化反馈文件"$1"
% 编译器无法打开指定的包含整体程序优化信息的反馈文件。
wpo_begin_processing=12001_D_正在处理 wpo 反馈文件"$1"中的整体程序优化信息
% 编译器开始处理在命名文件中找到的整体程序优化信息。
wpo_end_processing=12002_D_已完成处理 wpo 反馈文件"$1"中的整体程序优化信息
% 编译器已完成处理在命名文件中找到的整体程序优化信息。
wpo_expected_section=12003_E_期望节头，但在 wpo 反馈文件第 $1 行得到"$2"
% 编译器在整体程序优化文件中期望一个节头(以 \% 开头)，
% 但未找到。
wpo_no_section_handler=12004_W_在 wpo 反馈文件第 $1 行没有为整体程序优化节"$2"注册处理程序，忽略
% 编译器没有处理程序来处理提到的整体程序优化信息
% 节，因此将忽略它并跳到下一节。
wpo_found_section=12005_D_找到包含"$2"信息的整体程序优化节"$1"
% 编译器遇到了一个包含整体程序优化信息的节，根据
% 其处理程序，此节包含可用于所述目的的信息。
wpo_no_input_specified=12006_F_所选的整体程序优化需要先前生成的反馈文件(使用 -Fw 指定)
% 编译器需要在先前编译运行期间收集的信息来执行所选的
% 整体程序优化。你可以使用 -Fw 开关指定包含此信息的反馈文件位置。
wpo_not_enough_info=12007_E_未找到执行"$1"整体程序优化所需的收集信息
% 虽然你向编译器指出了包含整体程序优化反馈的文件，但它
% 不包含执行所选优化所需的信息。你很可能
% 需要使用适当的 -OWxxx 开关重新编译程序。
wpo_no_output_specified=12008_F_请指定用于存储生成信息的整体程序优化反馈文件(使用 -FW)
% 你必须指定编译器必须存储整体程序优化
% 反馈的反馈文件，该反馈在编译运行期间生成。这可以使用 -FW 开关完成。
wpo_output_without_info_gen=12009_E_未生成任何整体程序优化信息，但指定了反馈文件(使用 -FW)
% 编译器被指示将整体程序优化反馈存储到使用 -FW 指定的文件中，
% 但实际上没有生成任何整体程序优化反馈。要生成的信息类别
% 可以使用 -OWxxx 指定。
wpo_input_without_info_use=12010_E_未执行任何整体程序优化，但指定了输入反馈文件(使用 -Fw)
% 编译器未被指示执行任何整体程序优化(没有 -Owxxx 参数)，
% 但仍然指定了带有此类反馈的输入文件(使用 -Fwyyy)。由于这可能
% 表明你忘记指定 -Owxxx 参数，编译器在这种情况下会生成错误。
wpo_skipping_unnecessary_section=12011_D_跳过整体程序优化节"$1"，因为所请求的优化不需要
% 整体程序优化反馈文件包含一个信息节，该节不是
% 所选整体程序优化所需的。
wpo_duplicate_wpotype=12012_W_使用节"$2"中的信息覆盖从反馈输入文件读取的"$1"的先前信息
% 反馈文件包含多个提供相同类别信息的节(例如，
% 关于哪些虚拟方法可以去虚拟化的信息)。在这种情况下，
% 使用最后遇到的节中的信息。打开调试输出(-vd)以查看每个节提供的信息类别。
wpo_cannot_extract_live_symbol_info_strip=12013_E_在剥离符号时无法从程序中提取符号活跃度信息，请使用 -Xs-
% 某些符号活跃度收集器从链接的程序中提取符号信息。如果符号信息
% 被剥离(选项 -Xs)，这是不可能的。
wpo_cannot_extract_live_symbol_info_no_link=12014_E_在不链接时无法从程序中提取符号活跃度信息
% 某些符号活跃度收集器从链接的程序中提取符号信息。如果程序不是
% 由编译器链接的，这是不可能的。
wpo_cannot_find_symbol_progs=12015_F_无法找到"$1"或"$2"以从链接的程序中提取符号活跃度信息
% 某些符号活跃度收集器需要一个辅助程序来从链接的程序中提取符号信息。
% 这个辅助程序通常是'nm'，它是 GNU binutils 的一部分。
wpo_error_reading_symbol_file=12016_E_读取由"$1"生成的符号活跃度信息时出错
% 在读取使用'nm'或'objdump'程序生成的符号活跃度文件时发生错误。原因
% 可能是它比预期的短，或者其格式无法理解。
wpo_error_executing_symbol_prog=12017_F_执行"$1"(退出代码: $2)以从链接的程序中提取符号信息时出错
% 某些符号活跃度收集器需要一个辅助程序来从链接的程序中提取符号信息。
% 辅助程序在运行于链接程序时产生了报告的错误代码。
wpo_symbol_live_info_needs_smart_linking=12018_E_符号活跃度信息的收集只在使用智能链接时有帮助，请使用 -CX -XX
% 符号是否活跃是通过查看它在最终链接的程序中是否存在来确定的。
% 没有智能链接/死代码剥离，所有符号总是被包含，无论它们是否
% 实际使用。所以在这种情况下所有符号都将被视为活跃的，这使得这种优化无效。
wpo_cant_create_feedback_file=12019_E_无法创建指定的整体程序优化反馈文件"$1"
% 编译器无法创建使用 -FW 参数指定的文件来存储整体程序优化信息。
% \end{description}
# EndOfTeX


#
# Package loading and handling
#
# 13029 is the last used one
#
# BeginOfTeX
% \section{包加载消息}
% 本节列出了当编译器从磁盘加载包到内存中、从内存保存包到磁盘或者
% 解析包时可能出现的所有消息。其中许多是信息性消息。
% \begin{description}
package_f_cant_find_pcp=13001_F_找不到包 $1
% 你试图使用一个编译器找不到其 PCP 文件的包。请检查配置文件中的包路径。
package_u_pcp_found=13002_U_找到包 $1 的 PCP 文件
% 找到了指定包的 PCP 文件。
package_e_duplicate_package=13003_E_重复的包 $1
% 该包已经被指定为必需包，不能重复指定。
package_e_unit_deny_package=13004_E_单元 $1 不能作为包的一部分
% 由于该单元启用了 DenyPackageUnit 指令，因此它不能作为包的一部分。
package_n_implicit_unit_import=13005_N_单元 $1 被隐式导入到包 $2 中
% 该单元未在 \var{contains} 部分指定，也未包含在任何必需包中。
% 将该单元添加到 \var{contains} 部分以提高与其他包的兼容性。
package_f_cant_create_pcp=13006_F_无法为包 $1 创建 PCP 文件 $2
% 无法创建包的 PCP 文件。
package_f_cant_read_pcp=13007_F_无法读取包 $1 的 PCP 文件
% 无法读取包的 PCP 文件。
package_t_pcp_loading=13008_T_正在加载 PCP $1
% 当使用 \var{-vt} 开关时，编译器会告诉你它正在加载哪些包。
package_u_pcp_name=13009_U_PCP 名称：$1
% 当你使用 \var{-vu} 标志时，会显示包名。
package_u_pcp_flags=13010_U_PCP 标志：$1
% 当你使用 \var{-vu} 标志时，会显示包标志。
package_u_pcp_crc=13011_U_PCP CRC：$1
% 当你使用 \var{-vu} 标志时，会显示包的 CRC 校验。
package_u_pcp_time=13012_U_PCP 时间：$1
% 当你使用 \var{-vu} 标志时，会显示包的编译时间。
package_u_pcp_file_too_short=13013_U_PCP 文件太短
% PCP 文件太短，不包含所有声明。
package_u_pcp_invalid_header=13014_U_PCP 头无效（开头不是 PCP）
% 包文件的前三个字节必须是字符 \var{PCP} 的 ASCII 码。
package_u_pcp_invalid_version=13015_U_PCP 版本无效 $1
% 此包文件是用不同版本的编译器编译的，无法读取。
package_u_pcp_invalid_processor=13016_U_PCP 是为其他处理器编译的
% 此包文件是为不同的处理器类型编译的，无法读取。
package_u_pcp_invalid_target=13017_U_PCP 是为其他目标编译的
% 此包文件是为不同的目标编译的，无法读取。
package_u_pcp_write=13018_U_正在写入 $1
% 当你指定 \var{-vu} 开关时，编译器会告诉你它在哪里写入包文件。
package_f_pcp_cannot_write=13019_F_无法写入 PCP 文件
% 写入包文件时发生错误。
package_f_pcp_read_error=13020_F_读取 PCP 文件时出错
% 这意味着包文件已损坏，包含无效信息。需要重新编译。
package_f_pcp_read_unexpected_end=13021_F_PCP 文件意外结束
% 文件意外结束。这可能意味着 PCP 文件已损坏。
package_f_pcp_invalid_entry=13022_F_无效的 PCP 文件条目：$1
% 编译器试图读取的单元已损坏，或是用较新版本的编译器生成的。
package_u_pcp_invalid_fpumode=13023_U_试图使用以不同 FPU 模式编译的单元
% 试图在使用不同浮点格式模式编译的单元时编译代码。所有代码应该
% 都使用 FPU 仿真开启，或都使用 FPU 仿真关闭。
package_t_packagesearch=13024_T_包搜索：$1
% 当你使用 \var{-vt} 选项时，编译器会告诉你它在哪里尝试查找包文件。
package_u_required_package=13025_U_需要的包 $1
% 当你指定 \var{-vu} 开关时，编译器会告诉你一个包需要哪些包。
package_u_contained_unit=13026_U_包含的单元 $1
% 当你指定 \var{-vu} 开关时，编译器会告诉你一个包包含哪些单元。
package_e_unit_already_contained_in_package=13027_E_单元 $1 已经包含在包 $2 中
% contains 部分中指定的单元不能是必需包的一部分。注意，
% 一个单元可能通过间接包含而成为另一个包的一部分。
package_w_unit_from_indirect_package=13028_W_单元 $1 是从间接需要的包 $2 导入的
% 如果使用了不在 \var{requires} 部分的包中的单元，那么该包
% 应该直接需要这个单元以避免混淆。
package_u_ppl_filename=13029_U_PPL 文件名 $1
% 存储在 PCP 中的二进制包库的名称。
%
% \end{description}
# EndOfTeX


#
# Logo (option -l)
#
option_logo=11023_[
Free Pascal 编译器版本 $FPCFULLVERSION [$FPCDATE] 用于 $FPCCPU
版权所有 (c) 1993-2025 Florian Klaempfl 和其他人
]

#
# Info (option -i)
#
option_info=11024_[
Free Pascal 编译器版本 $FPCVERSION

编译器日期      : $FPCDATE
编译器 CPU 目标: $FPCCPU

支持的目标平台(标有 '{*}' 的目标正在开发中):
  $OSTARGETS

支持的 CPU 指令集:
  $INSTRUCTIONSETS

支持的 FPU 指令集:
  $FPUINSTRUCTIONSETS

支持的内联汇编模式:
  $ASMMODES

已识别的编译器和运行时库特性:
  $FEATURELIST

已识别的模式开关:
  $MODESWITCHES

支持的 ABI 目标:
  $ABITARGETS

支持的优化:
  $OPTIMIZATIONS

支持的整体程序优化:
  全部
  $WPOPTIMIZATIONS

代码生成后端:
  $CODEGENERATIONBACKEND

支持的 LLVM/Xcode 版本(仅适用于 LLVM 代码生成后端):
  $LLVMVERSIONS

支持的微控制器类型:$\n  $CONTROLLERTYPES$\n
本程序遵循 GNU 通用公共许可证
更多信息请阅读 COPYING.v2

请在我们的 bug 跟踪系统上报告 bug:
                 https://bugs.freepascal.org

更多信息可以在我们的网站上找到(包括邮件列表的使用说明，
可用于提问或讨论潜在的新功能等):
                 https://www.freepascal.org
]

#
# Help pages (option -? and -h)
#
# The first character on the line indicates who will display this
# line, the current possibilities are:
#    * = every target
#    3 = 80x86 targets
#    4 = x86_64
#    6 = 680x0 targets
#    8 = 8086 (16 bit) targets
#    a = AArch64
#    A = ARM
#    e = in extended debug mode only
#    F = help for the 'fpc' binary (independent of the target compiler)
#    I = VIS
#    J = JVM
#    L = LLVM variant
#    l = loongarch64 targets
#    M = MIPS (MIPSEB) targets
#    m = MIPSEL targets
#    P = PowerPC targets
#    p = PowerPC64 targets
#    R = RiscV32 targets
#    r = RiscV64 targets
#    S = Sparc targets
#    s = Sparc64 targets
#    V = AVR
#    x = xtensa targets
#    Z = Z80
# The second character also indicates who will display this line,
# (if the above character was TRUE) the current possibilities are :
#    * = everyone
#    g = with GDB info supported by the compiler
#    O = OS/2
#    L = UNIX systems
#  The third character represents the indentation level.
#
option_help_pages=11025_[
# 在此部分也可以插入注释
F*0*_仅列出对默认或选定平台有效的选项。
**0*_在布尔开关选项后加 + 启用它，加 - 禁用它。
**1@<x>_除默认的 fpc.cfg 外，还从 <x> 读取编译器选项
# 汇编器相关选项
**1a_编译器不删除生成的汇编文件，自动切换到外部汇编器写入器
**2a5_对于 GNU Binutils 2.25 之前的版本不生成 Big Obj COFF 文件(Windows, NativeNT)
**2al_在汇编文件中列出源代码行
**2an_在汇编文件中列出节点信息(-dEXTDEBUG 编译器)
**2ao_为外部汇编器调用添加额外选项(对内部汇编器无效)
*L2ap_使用管道而不是创建临时汇编文件
**2ar_在汇编文件中列出寄存器分配/释放信息
**2aR_在汇编文件中列出 RTTI 信息
**2at_在汇编文件中列出临时分配/释放信息
# 使用的汇编器选择
**1A<x>_输出格式:
**2Adefault_使用默认汇编器
3*2Aas_使用 GNU AS 汇编
3*2Aas-darwin_使用 GNU GAS 汇编 Darwin Mach-O
3*2Acoff_使用内部写入器生成 COFF (Go32v2)
3*2Aelf_使用内部写入器生成 ELF (Linux)
3*2Amacho_使用内部写入器生成 Mach-O (Darwin, Intel 32 位)
3*2Amasm_使用 Masm (Microsoft) 生成 Obj 文件
3*2Anasm_使用 Nasm 汇编
3*2Anasmcoff_使用 Nasm 生成 COFF (Go32v2) 文件
3*2Anasmdarwin_使用 Nasm 生成 macho32 目标文件(实验性)
3*2Anasmelf_使用 Nasm 生成 ELF32 (Linux) 文件
3*2Anasmobj_使用 Nasm 生成 Obj 文件
3*2Anasmwdosx_使用 Nasm 生成 Win32/WDOSX 目标文件
3*2Anasmwin32_使用 Nasm 生成 Win32 目标文件
3*2Apecoff_使用内部写入器生成 PE-COFF (Win32)
3*2Atasm_使用 Tasm (Borland) 生成 Obj 文件
3*2Awasm_使用 Wasm (Watcom) 生成 Obj 文件
3*2Ayasm_使用 Yasm 汇编(实验性)
4*2Aas_使用 GNU AS 汇编
4*2Aas-darwin_使用 GNU GAS 汇编 Darwin Mach-O
4*2Aelf_使用内部写入器生成 ELF (Linux 64 位)
4*2Agas_使用 GNU GAS 汇编
4*2Amasm_使用 ml64 (Microsoft) 生成 Win64 目标文件
4*2Anasm_使用 Nasm 汇编(实验性)
4*2Anasmdarwin_使用 Nasm 汇编 darwin macho64 目标文件(实验性)
4*2Anasmelf_使用 Nasm 汇编 Linux 64 位目标文件(实验性)
4*2Anasmwin64_使用 Nasm 汇编 Win64 目标文件(实验性)
4*2Apecoff_使用内部写入器生成 PE-COFF (Win64)
4*2Ayasm_使用 Yasm 汇编(实验性)
6*2Aas_使用 GNU AS 生成 Unix o-文件
6*2Agas_GNU Motorola 汇编器
6*2Amit_MIT 语法(旧版 GAS)
6*2Amot_标准 Motorola 汇编器
6*2Avasm_使用 vasm 进行汇编
8*2Anasm_使用 Nasm 汇编
8*2Anasmobj_使用 Nasm 汇编
A*2Aas_使用 GNU AS 汇编
A*2Aas-darwin_使用 GNU AS 为 Darwin 目标汇编
A*2Aclang_使用 clang 汇编
A*2Aelf_使用内部 ELF 写入器汇编
a*2Aas_使用 GNU AS 汇编
a*2Aas-clang_使用 clang 为其他目标汇编
a*2Aclang_使用 clang 为 darwin/ios 目标汇编
P*2Aas_使用 GNU AS 汇编
S*2Aas_使用 GNU AS 汇编
s*2Aas_使用 GNU AS 汇编
v*2Aas_使用 GNU AS 汇编
W*2Awasm_使用内部目标文件写入器为 wasm32 汇编
W*2Abinaryen_使用 GNU AS 为 wasm32 汇编(wasm-as)(实验性)
W*2Allvm-mc-10_使用 llvm-mc-10 汇编
W*2Allvm-mc-11_使用 llvm-mc-11 汇编
W*2Allvm-mc-12_使用 llvm-mc-12 汇编
W*2Allvm-mc-13_使用 llvm-mc-13 汇编(由于 LLVM 中的错误而损坏)
W*2Allvm-mc-14_使用 llvm-mc-14 汇编(由于 LLVM 中的错误而损坏)
W*2Allvm-mc-15_使用 llvm-mc-15 汇编
W*2Allvm-mc-16_使用 llvm-mc-16 汇编
W*2Allvm-mc-17_使用 llvm-mc-17 汇编
W*2Allvm-mc_使用 llvm-mc 汇编(版本 18 或更高)
W*2Awasa_使用 wasa 汇编(实验性)
x*2Aas_使用 GNU AS 汇编
Z*2Asdcc-sdasz80_使用 SDCC-SDASZ80 汇编
Z*2Avasm_使用 Vasm 汇编
Z*2Az80asm_使用 z80asm 汇编
# 仅由 IDE 内部使用
**1b_生成浏览器信息
**2bl_生成本地符号信息
**1B_构建所有模块
**1C<x>_代码生成选项:
**2C3_启用常量的 IEEE 错误检查
**2Ca<x>_选择 ABI；使用 fpc -i 或 fpc -ia 查看可能的值
**2Cb_为目标架构的大端变体生成代码
**2Cc<x>_将默认调用约定设置为 <x>
V*2Cd<x>_丢弃选定的 RTL 启动段(谨慎使用)
V*3Cdc_丢弃初始化数据。数据默认为 noinit 段
V*3Cdj_丢弃跳转到 PASCALMAIN。仅在 PASCALMAIN 直接跟在启动代码后时使用
V*3Cds_丢弃 _START 代码。仅在所有中断都被禁用时使用
V*3Cdz_丢弃初始化零寄存器和栈指针的代码
**2CD_同时创建动态库(不支持)
**2Ce_使用模拟浮点操作码进行编译
**2CE_生成可能引发异常的 FPU 代码
**2Cf<x>_选择要使用的 fpu 指令集；使用 fpc -i 或 fpc -if 查看可能的值
**2CF<x>_最小浮点常量精度(默认、32、64)
**2Cg_生成 PIC 代码
**2Ch<n>[,m]_<n> 字节最小堆大小(在 1023 和 67107840 之间)以及可选的 [m] 最大堆大小
**2Ci_IO 检查
A*2CI<x>_在 ARM 上选择指令集: ARM 或 THUMB
L*2Cl<x>_LLVM 代码生成选项
L*3Clflto_启用链接时优化(编译单元和程序/库时都需要)
L*3Clfltonosystem_为系统单元禁用 LTO(由于链接器错误，至少在 Xcode 10.2 及更早版本中需要)
L*3Clflsanitize=address_启用地址检查器
L*3Clv<x>_LLVM 目标版本: Xcode-10.1, 7.0, 8.0, .., 10.0
**2Cn_省略链接阶段
P*2CN_生成空指针检查(仅限 AIX)
W*2CN_生成空指针检查
**2Co_检查整数运算的溢出
**2CO_检查整数运算可能的溢出
**2Cp<x>_选择指令集；使用 fpc -i 或 fpc -ic 查看可能的值
**2CP<x>=<y>_打包设置
**3CPPACKENUM=<y>_<y> 枚举打包: 0、1、2 和 4 或 DEFAULT 或 NORMAL
**3CPPACKRECORD=<y>_<y> 记录打包: 0 或 DEFAULT 或 NORMAL、1、2、4、8、16 和 32
**3CPPACKSET=<y>_<y> 集合分配: 0、1 或 DEFAULT 或 NORMAL、2、4 和 8
**2Cr_范围检查
**2CR_验证对象方法调用的有效性
**2Cs<n>_将栈检查大小设置为 <n>
**2Ct_栈检查(仅用于测试，请参阅手册)
8*2CT<x>_目标特定的代码生成选项
3*2CT<x>_目标特定的代码生成选项
4*2CT<x>_目标特定的代码生成选项
p*2CT<x>_目标特定的代码生成选项
P*2CT<x>_目标特定的代码生成选项
J*2CT<x>_目标特定的代码生成选项
A*2CT<x>_目标特定的代码生成选项
W*2CT<x>_目标特定的代码生成选项
p*3CTsmalltoc_以牺牲执行速度为代价生成更小的 TOC (AIX)
P*3CTsmalltoc_以牺牲执行速度为代价生成更小的 TOC (AIX)
J*3CTautogetterprefix=X_使用前缀 X 自动为属性创建 getter (空字符串禁用)
J*3CTautosetterprefix=X_使用前缀 X 自动为属性创建 setter (空字符串禁用)
8*3CTcld_在使用 x86 字符串指令之前发出 CLD 指令
3*3CTcld_在使用 x86 字符串指令之前发出 CLD 指令
4*3CTcld_在使用 x86 字符串指令之前发出 CLD 指令
8*3CTfarprocspushoddbp_在远程函数序言中增加 BP 后再压入栈
J*3CTcompactintarrayinit_为初始化整数数组常量生成更小(但可能更慢)的代码
J*3CTenumfieldinit_在调用继承构造函数后，在构造函数中将枚举字段初始化为 enumtype(0)
J*3CTinitlocals_初始化未初始化使用时会触发 JVM 字节码验证错误的局部变量(会降低代码速度)
J*3CTlowercaseprocstart_将过程/函数/方法名称的第一个字符小写
A*3CTthumbinterworking_尽可能生成 Thumb 互操作安全代码
W*3CTnoexceptions_禁用异常支持。引发异常将中止程序。
W*3CTbfexceptions_为 WebAssembly 启用分支异常支持
W*3CTjsexceptions_为 WebAssembly 启用基于 JavaScript 的异常支持(实验性)
W*3CTwasmexceptions_启用原生 WebAssembly 异常支持
W*3CTwasmthreads_启用 WebAssembly 线程支持(实验性)
W*3CTsaturatingfloattoint_使用饱和(非陷阱)浮点到整数转换指令
J*2Cv_Var/out 参数复制检查
A*2CV<x>_将线程变量模型设置为 <x>
**2CX_同时创建智能链接库
**1d<x>_定义符号 <x>
**1D_生成 DEF 文件
**2Dd<x>_将描述设置为 <x>
**2DD<x>_将 %DATE% 返回的日期字符串设置为 x，不检查是否为有效日期字符串
**2DT<x>_将 %TIME% 返回的时间字符串设置为 x，不检查是否为有效时间字符串
**2Dv<x>_将 DLL 版本设置为 <x>
*O2Dw_PM 应用程序
**1e<x>_设置可执行文件路径
**1E_与 -Cn 相同
**1fPIC_与 -Cg 相同
**1F<x>_设置文件名和路径:
**2Fa<x>[,y]_(对于程序)在解析 uses 之前加载单元 <x> 和 [y]
**2Fc<x>_将输入代码页设置为 <x>
**2FC<x>_将 RC 编译器二进制名称设置为 <x>
**2Fd_禁用编译器的内部目录缓存
**2FD<x>_设置搜索编译器工具的目录
**2Fe<x>_将错误输出重定向到 <x>
**2FE<x>_将可执行文件/单元输出路径设置为 <x>
**2Ff<x>_将 <x> 添加到框架路径(仅限 Darwin)，或将 IDF 路径设置为 <x> (Xtensa-FreeRTOS)
**2FF_使用 fpcres 作为 RC 到 RES 编译器，而不是 windres 或 gorc
**2Fi<x>_将 <x> 添加到包含路径
**2Fl<x>_将 <x> 添加到库路径
**2FL<x>_使用 <x> 作为动态链接器
**2Fm<x>_从编译器目录中的 <x>.txt 加载 unicode 转换表
**2FM<x>_设置搜索 unicode 二进制文件的目录
**2FN<x>_将 <x> 添加到默认单元作用域(命名空间)列表
**2Fo<x>_将 <x> 添加到对象路径
**2Fr<x>_加载错误消息文件 <x>
**2FR<x>_将资源(.res)链接器设置为 <x>
**2Ft_在区分大小写的文件系统上不搜索大写或 8.3 单元文件名
**2Fu<x>_将 <x> 添加到单元路径
**2FU<x>_将单元输出路径设置为 <x>，覆盖 -FE
**2Fw<x>_从 <x> 加载先前存储的整体程序优化反馈
**2FW<x>_将生成的整体程序优化反馈存储在 <x> 中
*g1g_生成调试信息(目标的默认格式)
*g2gc_生成指针检查(实验性，仅在某些目标上可用，可能产生误报)
*g2gh_使用 heaptrace 单元(用于内存泄漏/损坏调试)
*g2gl_使用行信息单元(在回溯中显示更多信息)
*g2gm_生成 Microsoft CodeView 调试信息(实验性)
*g2go<x>_设置调试信息选项
*g3godwarfcpp_在 DWARF 中模拟 C++ 调试信息
*g3godwarfmethodclassprefix_在 DWARF 中用类名作为方法名的前缀
*g3godwarfomflinnum_除了 DWARF 调试信息外，还在 MS LINK 格式的 OMF LINNUM 记录中生成行号信息(Open Watcom 调试器/链接器兼容性)
*g3godwarfsets_启用 DWARF 'set' 类型调试信息(破坏 gdb < 6.5)
*g3gostabsabsincludes_在 Stabs 中存储绝对/完整包含文件路径
*g2gp_在 stabs 符号名中保留大小写
*g2gs_生成 Stabs 调试信息
*g2gt_破坏局部变量(用于检测未初始化的使用；多个 't' 改变破坏值)
*g2gv_生成可用 Valgrind 跟踪的程序
*g2gw_生成 DWARFv2 调试信息(与 -gw2 相同)
*g2gw2_生成 DWARFv2 调试信息
*g2gw3_生成 DWARFv3 调试信息
*g2gw4_生成 DWARFv4 调试信息(实验性)
**1i_信息
**2ia_返回支持的 ABI 目标列表
**2ib_返回使用的代码生成后端类型
**2ic_返回支持的 CPU 指令集列表
**2iD_返回编译器日期
**2if_返回支持的 FPU 指令集列表
**2ii_返回支持的内联汇编器模式列表
L*2il_返回 LLVM 后端支持的 LLVM/Xcode 版本列表
**2im_返回支持的模式开关列表
**2io_返回支持的优化列表
**2ir_返回已识别的编译器和 RTL 功能列表
**2iSO_返回编译器操作系统
**2iSP_返回编译器主机处理器
**2it_返回支持的目标列表
**2iTO_返回目标操作系统
**2iTP_返回目标处理器
**2iu_返回支持的微控制器类型列表
**2iV_返回编译器简短版本
**2iw_返回支持的整体程序优化列表
**2iW_返回编译器完整版本
**1I<x>_将 <x> 添加到包含路径
**1k<x>_将 <x> 传递给链接器
**1l_写入标志
**1M<x>_将语言模式设置为 <x> / 启用模式开关 <x> (参见选项 -im)
**2Mdelphi_Delphi 7 兼容模式
**2Mdelphiunicode_Delphi 2009 及更高版本兼容模式
**2Mextendedpascal_ISO 10206 模式
**2Mfpc_Free Pascal 方言(默认)
**2Miso_ISO 7185 模式
**2Mmacpas_Macintosh Pascal 方言兼容模式
**2Mobjfpc_带对象 Pascal 支持的 FPC 模式
**2Mtp_TP/BP 7.0 兼容模式
**2*_每个模式(如上所列)都启用其默认的模式开关集。
**2*_其他模式开关默认禁用，需要一个一个启用。
**1M<x>-_禁用模式开关 <x> (参见选项 -im)
**1n_不读取默认配置文件
**1o<x>_将生成的可执行文件名更改为 <x>
**1O<x>_优化：
**2O-_禁用优化
**2O1_级别 1 优化(快速且调试器友好)
**2O2_级别 2 优化(-O1 + 快速优化)
**2O3_级别 3 优化(-O2 + 慢速优化)
**2O4_级别 4 优化(-O3 + 可能有意外副作用的优化)
**2Oa<x>=<y>_设置对齐
**2Oo[NO]<x>_启用或禁用优化；可能的值请参见 fpc -i 或 fpc -io
**2Op<x>_设置优化目标 CPU；可能的值请参见 fpc -i 或 fpc -ic
**2Os_优化大小而不是速度
**2Ow<x>_执行整体程序优化 <x>；可能的值请参见 fpc -i 或 fpc -iw
**2OW<x>_为优化 <x> 生成整体程序优化反馈；可能的值请参见 fpc -i 或 fpc -iw
**1pg_生成用于 gprof 的配置代码(定义 FPC_PROFILE)
F*1P<x>_目标 CPU / 编译器相关选项：
F*2PB_显示默认编译器二进制文件
F*2PP_显示默认目标 CPU
F*2P<x>_设置目标 CPU (aarch64,arm,avr,i386,i8086,jvm,loongarch64,m68k,mips,mipsel,powerpc,powerpc64,riscv32,riscv64,sparc,sparc64,wasm32,x86_64,xtensa,z80)
**1R<x>_汇编器读取风格：
**2Rdefault_使用目标的默认汇编器
3*2Ratt_读取 AT&T 风格汇编器
3*2Rintel_读取 Intel 风格汇编器
4*2Ratt_读取 AT&T 风格汇编器
4*2Rintel_读取 Intel 风格汇编器
8*2Ratt_读取 AT&T 风格汇编器
8*2Rintel_读取 Intel 风格汇编器
6*2RMOT_读取 Motorola 风格汇编器
**1s_不调用汇编器和链接器
**2sh_生成在主机上链接的脚本，自动切换到外部汇编器写入器
**2sr_跳过寄存器分配阶段(与 -alr 一起使用)，自动切换到外部汇编器写入器
**2st_生成在目标上汇编和链接的脚本，自动切换到外部汇编器写入器
**2sT_仅生成在目标上链接的脚本
**1S<x>_语法选项：
**2S2_等同于 -Mobjfpc
**2Sa_启用断言
**2Sc_支持 C 语言风格的运算符(*=, +=, /= 和 -=)
**2Sd_等同于 -Mdelphi
**2Se<x>_错误选项。<x> 是以下选项的组合：
**3*_<n>：编译器在 <n> 个错误后停止(默认为 1)
**3*_h：编译器在提示后也停止
**3*_n：编译器在注释后也停止
**3*_w：编译器在警告后也停止
**2Sf_在编译器和 RTL 中启用某些功能；可能的值请参见 fpc -i 或 fpc -ir
**2Sg_启用 LABEL 和 GOTO (在 -Mtp 和 -Mdelphi 中默认启用)
**2Sh_使用引用计数字符串(默认为 ansistring)而不是 shortstrings
**2Si_启用声明为"inline"的过程/函数的内联
**2SI<x>_将接口风格设置为 <x>
**3SIcom_COM 兼容接口(默认)
**3SIcorba_CORBA 兼容接口
**2Sj_允许类型化常量可写(在所有模式中默认启用)
**2Sk_加载 fpcylix 单元
**2Sm_支持 C 语言风格的宏(全局)
**2So_等同于 -Mtp
**2Sr_ISO 模式下的透明文件名
**2Ss_构造函数名必须为"Init"(析构函数名必须为"Done")
**2Sv_支持向量处理(如果可用则使用 CPU 向量扩展)
**2Sx_启用异常关键字(在 Delphi/ObjFPC 模式下默认启用)
**2Sy_@<pointer> 返回类型化指针，等同于 $T+
**1T<x>_目标操作系统：
# i386 目标
3*2Tandroid_Android
3*2Taros_AROS
3*2Tbeos_BeOS
3*2Tdarwin_Darwin/Mac OS X
3*2Tembedded_嵌入式
3*2Temx_通过 EMX 的 OS/2 (包括 EMX/RSX 扩展器)
3*2Tfreebsd_FreeBSD
3*2Tgo32v2_DJ Delorie DOS 扩展器版本 2
3*2Thaiku_Haiku
3*2Tiphonesim_iOS SDK 3.2+ 的 iPhoneSimulator (旧版本：-Tdarwin)
3*2Tlinux_Linux
3*2Tnativent_原生 NT API (实验性)
3*2Tnetbsd_NetBSD
3*2Tnetware_Novell Netware 模块 (clib)
3*2Tnetwlibc_Novell Netware 模块 (libc)
3*2Topenbsd_OpenBSD
3*2Tos2_OS/2 / eComStation
3*2Tsolaris_Solaris
3*2Tsymbian_Symbian OS
3*2Twatcom_Watcom 兼容 DOS 扩展器
3*2Twdosx_WDOSX DOS 扩展器
3*2Twin32_Windows 32 位
3*2Twince_Windows CE
# x86_64 目标
4*2Tandroid_Android
4*2Taros_AROS
4*2Tdarwin_Darwin/Mac OS X
4*2Tdragonfly_DragonFly BSD
4*2Tembedded_嵌入式
4*2Tfreebsd_FreeBSD
4*2Thaiku_Haiku
4*2Tiphonesim_iPhoneSimulator
4*2Tlinux_Linux
4*2Tnetbsd_NetBSD
4*2Topenbsd_OpenBSD
4*2Tsolaris_Solaris
4*2Twin64_Win64 (64 位 Windows 系统)
# m68k 目标
6*2Tamiga_Commodore Amiga
6*2Tatari_Atari ST/STe/TT
6*2Tembedded_嵌入式
6*2Thuman68k_Human 68k
6*2Tlinux_Linux
6*2Tnetbsd_NetBSD
6*2Tmacosclassic_经典 Mac OS
6*2Tpalmos_PalmOS
6*2Tsinclairql_Sinclair QL
# i8086 目标
8*2Tembedded_嵌入式
8*2Tmsdos_MS-DOS (及兼容系统)
8*2Twin16_Windows 16 位
# arm 目标
A*2Tandroid_Android
A*2Taros_AROS
A*2Tembedded_嵌入式
A*2Tfreertos_FreeRTOS
A*2Tgba_Game Boy Advance
A*2Tios_iOS
A*2Tlinux_Linux
A*2Tnds_Nintendo DS
A*2Tnetbsd_NetBSD
A*2Tpalmos_PalmOS
A*2Tsymbian_Symbian
A*2Twince_Windows CE
# aarch64 目标
a*2Tandroid_Android
a*2Tdarwin_Darwin/Mac OS X
a*2Tembedded_嵌入式
a*2Tfreebsd_FreeBSD
a*2Tiphonesim_iPhoneSimulator
a*2Tios_iOS
a*2Tlinux_Linux
a*2Twin64_Windows 64
# jvm 目标
J*2Tandroid_Android
J*2Tjava_Java
# loongarch64 目标
l*2Tlinux_Linux
# mipsel 目标
m*2Tandroid_Android
m*2Tembedded_嵌入式
m*2Tlinux_Linux
m*2Tps1_PlayStation 1
# mipseb 目标
M*2Tembedded_嵌入式
M*2Tlinux_Linux
# powerpc 目标
P*2Taix_AIX
P*2Tamiga_AmigaOS
P*2Tdarwin_Darwin/Mac OS X
P*2Tembedded_嵌入式
P*2Tlinux_Linux
P*2Tmacosclassic_经典 Mac OS
P*2Tmorphos_MorphOS
P*2Tnetbsd_NetBSD
P*2Twii_Wii
# powerpc64 目标
p*2Taix_AIX
p*2Tdarwin_Darwin/Mac OS X
p*2Tembedded_嵌入式
p*2Tlinux_Linux
# riscv32 目标
R*2Tembedded_嵌入式
R*2Tlinux_Linux
# riscv64 目标
r*2Tembedded_嵌入式
r*2Tlinux_Linux
# sparc 目标
S*2Tlinux_Linux
S*2Tsolaris_Solaris
# sparc64 目标
s*2Tlinux_Linux
# 尚未就绪 s*2Tsolaris_Solaris
# avr 目标
V*2Tembedded_嵌入式
# xtensa 目标
x*2Tembedded_嵌入式
x*2Tfreertos_FreeRTOS
x*2Tlinux_Linux
# z80 目标
Z*2Tembedded_嵌入式
Z*2Tamstradcpc_Amstrad CPC
Z*2Tmsxdos_MSX-DOS
Z*2Tzxspectrum_ZX Spectrum
# wasm32 目标
W*2Tembedded_嵌入式
W*2Twasip1_WebAssembly 系统接口预览版 1 (WASI 0.1)
W*2Twasip1threads_带多线程的 WebAssembly 系统接口预览版 1 (WASI 0.1 + wasi-threads)
W*2Twasip2_WebAssembly 系统接口预览版 2 (WASI 0.2) (实验性)
# 目标部分结束
**1t<x>_目标架构
**2*_ * 定义 FPC_SUBTARGET_<x>
**2*_ * 定义 FPC_SUBTARGET 为 <arg>
**2*_ * 另外读取配置文件 fpc-<subtarget>.cfg
**1u<x>_取消定义符号 <x>
**1U_单元选项：
**2Un_不检查单元名是否与文件名匹配
**2Ur_生成发布单元文件(永不自动重新编译)
**2Us_编译系统单元
**1v<x>_详细输出。<x> 是以下字母的组合：
**2*_0 : 不显示任何内容(除了错误)
**2*_a : 显示所有内容
**2*_b : 显示文件名消息时使用完整路径
**2*_c : 显示条件编译信息
**2*_d : 显示调试信息
**2*_e : 显示错误(默认)
#**2*_f : 尚未使用
#**2*_g : 尚未使用
**2*_h : 显示提示
**2*_i : 显示常规信息
**2*_j : 始终添加主源文件(在并行 make 时有用)
#**2*_k : 尚未使用
**2*_l : 显示行号
**2*_m<x>,<y> : 不显示编号为 <x> 和 <y> 的消息
**2*_n : 显示注意事项
#**2*_o : 尚未使用
**2*_p : 写入包含解析树的 tree.log
**2*_q : 显示消息编号
**2*_r : Rhide/GCC 兼容模式
**2*_s : 显示时间戳
**2*_t : 显示尝试/使用的文件
**2*_u : 显示单元信息
**2*_v : 写入包含大量调试信息的 fpcdebug.txt
**2*_w : 显示警告
**2*_x : 显示关于调用工具的信息
#**2*_y : 尚未使用
**2*_z : 将输出写入标准错误
F*1V<x>_在使用的编译器二进制名称后附加 '-<x>'(例如版本号)
**1W<x>_目标特定选项(目标)
3*2WA_指定原生类型应用程序(Windows)
4*2WA_指定原生类型应用程序(Windows)
A*2WA_指定原生类型应用程序(Windows)
3*2Wb_创建包而不是库(Darwin)
P*2Wb_创建包而不是库(Darwin)
p*2Wb_创建包而不是库(Darwin)
a*2Wb_创建包而不是库(Darwin)
A*2Wb_创建包而不是库(Darwin)
4*2Wb_创建包而不是库(Darwin)
3*2WB_创建可重定位映像(Windows, Symbian)
3*2WB<x>_将映像基址设置为 <x>(Windows, Symbian)
4*2WB_创建可重定位映像(Windows)
4*2WB<x>_将映像基址设置为 <x>(Windows)
A*2WB_创建可重定位映像(Windows, Symbian)
A*2WB<x>_将映像基址设置为 <x>(Windows, Symbian)
Z*2WB<x>_将映像基址设置为 <x>(ZX Spectrum)
3*2WC_指定控制台类型应用程序(EMX, OS/2, Windows)
4*2WC_指定控制台类型应用程序(Windows)
A*2WC_指定控制台类型应用程序(Windows)
P*2WC_指定控制台类型应用程序(经典 Mac OS)
3*2WD_使用 DEFFILE 导出 DLL 或 EXE 的函数(Windows)
4*2WD_使用 DEFFILE 导出 DLL 或 EXE 的函数(Windows)
A*2WD_使用 DEFFILE 导出 DLL 或 EXE 的函数(Windows)
3*2We_使用外部资源(Darwin)
4*2We_使用外部资源(Darwin)
a*2We_使用外部资源(Darwin)
A*2We_使用外部资源(Darwin)
P*2We_使用外部资源(Darwin)
p*2We_使用外部资源(Darwin)
3*2WF_指定全屏类型应用程序(EMX, OS/2)
6*2WF<x>_将 TOS 程序标志设置为 <x>(Atari)
3*2WG_指定图形类型应用程序(EMX, OS/2, Windows)
4*2WG_指定图形类型应用程序(Windows)
A*2WG_指定图形类型应用程序(Windows)
P*2WG_指定图形类型应用程序(经典 Mac OS)
3*2Wi_使用内部资源(Darwin)
4*2Wi_使用内部资源(Darwin)
a*2Wi_使用内部资源(Darwin)
A*2Wi_使用内部资源(Darwin)
P*2Wi_使用内部资源(Darwin)
p*2Wi_使用内部资源(Darwin)
3*2WI_开启/关闭导入节的使用(Windows)
4*2WI_开启/关闭导入节的使用(Windows)
A*2WI_开启/关闭导入节的使用(Windows)
8*2Wh_为单元使用巨大代码(对于代码在单一段中的模型忽略)
8*2Wm<x>_设置内存模型
8*3WmTiny_微型内存模型
8*3WmSmall_小型内存模型(默认)
8*3WmMedium_中型内存模型
8*3WmCompact_紧凑内存模型
8*3WmLarge_大型内存模型
8*3WmHuge_巨型内存模型
3*2WM<x>_最低 Mac OS X 部署版本：10.4、10.5.1、...(Darwin)
4*2WM<x>_最低 Mac OS X 部署版本：10.4、10.5.1、...(Darwin)
p*2WM<x>_最低 Mac OS X 部署版本：10.4、10.5.1、...(Darwin)
P*2WM<x>_最低 Mac OS X 部署版本：10.4、10.5.1、...(Darwin)
3*2WN_不生成重定位代码，调试时需要(Windows)
4*2WN_不生成重定位代码，调试时需要(Windows)
A*2WN_不生成重定位代码，调试时需要(Windows)
A*2Wp<x>_指定控制器类型；使用 fpc -i 或 fpc -iu 查看可用值
m*2Wp<x>_指定控制器类型；使用 fpc -i 或 fpc -iu 查看可用值
R*2Wp<x>_指定控制器类型；使用 fpc -i 或 fpc -iu 查看可用值
V*2Wp<x>_指定控制器类型；使用 fpc -i 或 fpc -iu 查看可用值
x*2Wp<x>_指定控制器类型；使用 fpc -i 或 fpc -iu 查看可用值
3*2WP<x>_最低 iOS 部署版本：3.0、5.0.1、...(iphonesim)
4*2WP<x>_最低 iOS 部署版本：8.0、8.0.2、...(iphonesim)
a*2WP<x>_最低 iOS 部署版本：7.0、7.1.2、...(Darwin)
A*2WP<x>_最低 iOS 部署版本：3.0、5.0.1、...(Darwin)
R*2WP<x>_esp-idf 版本号：4.4、5.0.6、...(esp32c3)
x*2WP<x>_Esp8266-rtos-sdk 或 esp-idf 版本号：3.3、3.4(esp8266)或 4.2、4.3(esp32)
3*2WR_生成重定位代码(Windows)
4*2WR_生成重定位代码(Windows)
A*2WR_生成重定位代码(Windows)
8*2Wt<x>_设置目标可执行文件格式
8*3Wtcom_创建 DOS .COM 文件(需要微型内存模型)
8*3Wtexe_创建 DOS .EXE 文件(默认)
6*2Wt<x>_设置目标可执行文件格式(Atari)
6*3Wttos_创建 TOS 可执行文件(默认)
6*3Wtmint_创建 MiNT 可执行文件
P*2WT_指定 MPW 工具类型应用程序(经典 Mac OS)
6*2WQ<x>_设置可执行文件元数据格式(Sinclair QL)
6*3WQqhdr_将元数据设置为 QDOS 文件头样式(默认)
6*3WQxtcc_将元数据设置为 XTcc 样式
**2WX_启用可执行栈(Linux)
**1x<suff>_设置编译器可执行文件的后缀(仅限 fpc 命令)
**1X_可执行文件选项：
**2X9_为版本低于 2.19.1 的 GNU Binutils ld 生成链接器脚本(Linux)
**2Xa_生成允许在 64 位目标上使用超过 2 GB 静态数据的代码(Linux)
**2Xc_将 --shared/-dynamic 传递给链接器(BeOS, Darwin, FreeBSD, Linux)
**2Xd_不搜索默认库路径(交叉编译时如果不使用 -XR 可能需要)
**2XD_尝试动态链接单元(定义 FPC_LINK_DYNAMIC)
**2Xe_使用外部链接器
**2Xf_替换用于链接的 pthread 库名称(BSD)
**2Xg_在单独的文件中创建调试信息并向可执行文件添加调试链接部分
**2Xi_使用内部链接器
L*2XlS<x>_LLVM 工具后缀(例如，如果 clang 被称为 clang-7，则为 -7)
**2XLA_定义用于链接的库替换
**2XLD_排除标准库的默认顺序
**2XLO_定义库链接顺序
**2Xm_生成链接映射
**2XM<x>_设置'main'程序例程的名称(默认为'main')
**2Xn_使用目标系统原生链接器而不是 GNU ld (Solaris, AIX)
F*2Xp<x>_首先在目录 <x> 中搜索编译器二进制文件
**2XP<x>_在 binutils 名称前加上前缀 <x>
**2Xr<x>_将链接器的 rlink-path 设置为 <x> (交叉编译需要，更多信息请参见 ld 手册) (BeOS, Linux)
**2XR<x>_在所有链接器搜索路径前加上 <x> (BeOS, Darwin, FreeBSD, Linux, Mac OS, Solaris)
**2Xs_从可执行文件中删除所有符号
**2XS_尝试静态链接单元(默认，定义 FPC_LINK_STATIC)
**2Xt_与静态库链接(-static 传递给链接器)
**2Xu_以 UF2 格式生成可执行文件(仅限嵌入式目标)
**2Xv_生成虚拟入口调用表
**2XV_使用 VLink 作为外部链接器(在 Amiga, Atari, Sinclair QL 上默认)
**2XX_尝试智能链接单元(定义 FPC_LINK_SMART)
**1*_
**1?_显示此帮助
**1h_显示此帮助而不等待
]

#
# The End...
