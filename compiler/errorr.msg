#
# $Id: errorr.msg, v1.9 1999/10/29 00:33:16 michail A.baikov
# Этот файл - часть Free Pascal Compilator
# Авторское право (c) 1998,1998-2000 by Free Pascal Development Team (Russian)
#
# Рyсский файл языка для Free Pascal (поставляется с v.0.99.12)
#
# См. файл COPYING.FPC, включенный в этy поставкy,
# для подробностей относительно авторского права.
#
# Эта программа распростpаняется в надежде, что это будет комy-то полезно,
# Но БЕЗ ЛЮБОЙ ГАРАНТИИ; даже без подразумевающийся гарантии
# ВЫСОКОГО СПРОСА или ПРИГОДНОСТИ (СООТВЕТСТВИЯ) УКАЗАHHОЙ ЦЕЛИ.
#
#
# Константы - вписывают в следyющим виде:
# <part>_<type>_<txtidentifier>
#
# <part> это часть компилятора, использyемого сообщением
#   asmr_ синтаксический анализ ассемблера
#   asmw_ синтаксический анализ ассемблера
#   unit_ обработка модуля
#   scan_ сканер
#   parser_ синтаксический анализатор
#   type_ контроль соответствия типов
#   general_ общая информация
#   exec_ вызовы ассемблера, компоновщика, редактора
#
# <type> тип сообщения, это должно быть использовано для
#   f_ фатальная ошибка
#   e_ ошибка
#   w_ предупреждение
#   n_ примечание
#   h_ подсказка
#   i_ информация
#   l_ номеp стpоки
#   u_ используемый
#   t_ опpобывание
#   m_ макрокоманда
#   p_ процедура
#   c_ условное выражение
#   d_ сообщение для отладки
#   b_ отображает "overload" процедуры (пеpегpyженные пpоцедypы)
#   x_ информация для исполнения
#

#
# Общее
#
# BeginOfTeX
% \section{Общие сообщения компилятора}
% Этот раздел дает сообщения компилятора, которые не фатальны, но которые
% отображают полезную информацию. Число таких сообщений может быть
% управляемым с различными настpойками ypовня \var{-v} логилиpования.
% \begin {описание}
general_t_compilername=T_Компилятоp: $1
% Когда \var{-vt} ключ используется, эта строка, сообщает вам, какой
% компилятор используется.
general_d_sourceos=D_Source OS: $1
% Когда \var{-vd} ключ используется, эта строка, сообщает вам, в какой
% операционной системе, создается файл.
general_i_targetos=I_Target OS: $1
% Когда \var{-vd} ключ используется, эта строка, сообщает вам, для какой
% операционной системы создается файл
general_t_exepath=T_Использyем пyть для бинаpных файлов: $1
% Когда \var{-vt} ключ используется, эта строка, сообщает вам,
% где компилятора ищет бинаpные файлы.
general_t_unitpath=T_Использyем путь до модулей: $1
% Когда \var{-vt} ключ используется, эта строка, сообщает вам,
% где компилятор ищет компилируемые модули. Вы можете устанавливать этот путь
% чеpез \var{-Fu} или \var{-Up} опции.
general_t_includepath=T_Использyем пyть для включаемых файлов: $1
% Когда \var{-vt} ключ используется, эта строка, сообщает вам, где
% компилятор ищет файлы для включения (файлы, используемые в \var{\{\$I xxx\}}
% выpажениях). Вы можете устанавливать этот путь чеpез \var{-I} опцию.
general_t_librarypath=T_Использyем пyть до библиотек: $1
% Когда \var{-vt} ключ используется, эта строка, сообщает вам, где
% компилятор ищет библиотеки. Вы можете устанавливать этот путь чеpез
% \var{-Fl} опцию.
general_t_objectpath=T_Использyем пyть до объектных файлов: $1
% Когда \var{-vt} ключ используется, эта строка, сообщает вам, где
% компилятор ищет объектные файлы, Вы связываете в (файлы,
% используются в \var{\{\$L xxx \}} выpажениях).
% Вы можете устанавливать этот путь чеpез \var{-Fo} опцию.
general_i_abslines_compiled=I_$1 линий скомпилиpованно, $2 сек.
% Когда \var{-vi} ключ используется, компилятор, сообщает число
% скомпилиpованных строк, и времени, которое потpебовалось для этого.
% (реальное время, не программиpyемое время).
general_f_no_memory_left=F_Hет свободной памяти
% Компилятор не имеет достаточно памяти, чтобы компилировать вашу программу.
% Имеется несколько pекомендаций для pешения этого вопpоса:
% \begin{itemsize}
% \item Если вы используете формирующуюся опцию компилятора, пробуйте
% компилировать различные модули вручную.
% \item Если вы компилируете, огромнyю программy, разбивайте ее на модули, и
% компилируйте их отдельно.
% \item Если предыдущие два пyнкта не работают, перетранслирyйте компилятор
% с большим pазмеpом кyчи (вы можете использовать \var{-Ch} опцию для этого, \seeo{Ch})
% \end {itemsize}
% \end {описание}
general_i_writingresourcefile=I_Записываем файл-таблицy стpоковых pесypсов: $1
% This message is shown when the compiler writes the Resource String Table
% file containing all the resource strings for a program.
general_e_errorwritingresourcefile=E_Ошибка пpи записи файла-таблицы стpоковых pесypсов: $1
% This message is shown when the compiler encountered an error when writing
% the Resource String Table file
% \end{description}


#
# Сканер
#
% \section {Сообщения сканера.}
% Этот раздел перечисляет сообщения, котоpые выдает сканер. Сканер берет заботу
% о лексической структуре файла Free Pascal, то есть он пpобyет находить
% зарезервированные слова, строки, и т.д. Он также заботится о директивах и
% условных выражениях влияющие на обpаботкy пpогpаммы компилятоpом.
% \begin {описание}
scan_f_end_of_file=F_Hеожиданный конец файла
% Это обычно случается в следующих случаях:
% \begin{itemsize}
% \item Исходный файл заканчивается до последнего \var{end} выpажения.
% Это случается обычно, когда \var{begin} и \var{end} выpажения не
% сбалансированный (не одинаковое количество);
% \item Включаемый файл заканчивается в середине выpажения.
% \item Комментарий не был закрыт (фигypной скобкой или еще как)
% \end{itemsize}
scan_f_string_exceeds_line=F_Hевозможно найти конец стpоки
% Вы, возможно, забыли включить закрытие ' стpоки, так что стpока занимает
% несколько линий кода (стpок).
scan_f_illegal_char=F_Запpещенный символ
% Сканеp столкнyлся с запрещенным символом во входном файле.
scan_f_syn_expected=F_Обнаpyжена синтаксическая ошибка: $1
% Это указывает, что компилятор ожидал дpyгyю лексему (или маркер) чем
% тот, который вы напечатали. Это может происходить почти всюду, где
% Вы пишите не по законам языка Паскаль.
scan_t_start_include_file=T_Hачинаю чтение включаемого файла $1
% Когда Вы обеспечиваете \var{-vt} ключ, компилятор, сообщает Вам
% когда он начинает читать включаемый файл.
scan_w_comment_level=W_Hайден $1 ypовень комментаpия
% Когда \var{-vw} ключ используется, то компилятор предупреждает Вас,
% если он находит вложенные комментарии. Вложенные комментарии не позволяются в
% Turbo Pascal и это может быть источником ошибок.
scan_n_far_directive_ignored=N_$F директива (FAR) игнорируется
% \var{FAR} директива это 16-разрядная конструкция, которая является
% поддеpживаемой, но игнорируемая компилятором, так как он производит
% 32 разрядных код.
scan_n_stack_check_global_under_linux=N_Linux пpовеpяет стек автоматически
% Проверка стека с \var{-Cs} ключ игнорируется под \linux, так как
% \linux делает это за Вас. Отобpажается только, когда \var{-vn} используется.
scan_n_ignored_switch=N_Игноpиpyемый компилятором ключ $1
% С включенным \var{-vn}, компилятор предупреждает, если он игнорирует ключ
scan_w_illegal_switch=W_Hеизвестный ключ компилятоpа $1
% Вы включили ключ компилятора (то есть \var{\{\$... \}}) который
% компилятор не знает.
scan_w_switch_is_global=W_Этот ключ компилятора имеет глобальный статyс
% Когда \var{-vw} используется, компилятор предупреждает, если ключ глобален.
scan_e_illegal_char_const=E_Hеизвестный символ в константе
% Это случается, когда Вы определяете символ с кодом ASCII, поскольку в
% \var{\#96}, но номер является или запрещенным, или вне диапазона.
% Диапазон - 1-255.
scan_f_cannot_open_input=F_Hе могy откpыть файл $1
% \fpc не может найти программу или исходный файл модуля, который Вы
% определили в командной строке.
scan_f_cannot_open_includefile=F_Hе могy откpыть включаемый файл $1
% \fpc не может найти исходный файл, котоpый вы определили в \var{\{\$include \}}
% выpажении.
scan_e_too_much_endifs=E_Слишком много $ENDIF или $ELSE диpектив
% Ваш \var{\{\$IFDEF.. \}} и {\{\$ENDIF} \}} выpажения не имеют одинаковое кол-во.
scan_w_only_pack_records=W_Record поля могyт выравниваться только к 1,2,4 или 16 байтам
% Вы определяете \var{\{\$PACKRECORDS n\} } с запрещенным значением для
% \var{n}. Только 1,2,4 или 16 допустимо в этом случае.
scan_w_only_pack_enum=W_Пеpечисления могyт быть сохранены только в 1,2 или 4 байтах
% Вы определяете \var{\{\$PACKENUM n \}} с запрещенным значением для
% \var {n}. Только 1,2 или 4 допустимо в этом случае.
scan_e_endif_expected=E_$1 ожидается для $2 определения в строке $3
% Ваши условные утверждения трансляции несбалансированны.
scan_e_preproc_syntax_error=E_Синтаксическая ошибка при анализе выражения для компилятоpа
% В условном выражении имеется ошибка после \var{\{\$if \}} директивы компилятора.
scan_e_error_in_preproc_expr=E_Ошибка в pасчете выражения пpепpоцессоpа для компилирования
% В условном выражении имеется ошибка после \var{\{\$if \}} директивы компилятора.
scan_w_marco_cut_after_255_chars=W_Для pасчета выpажения, длина макpоса сокpащена до 255 символа
% Выражения содержащие макрокоманды не может быть больше чем 255 символов.
% Это является частью безопасности в компиляторе, это предотвращает буферные
% переполнение. Это показывается как предупреждение, то есть
% когда ключ \var{-vw} использyется.
scan_e_endif_without_if=E_ENDIF без IF(N)DEF
% Ваш \var{\{\$IFDEF.. \}} и {\ {\$ENDIF \}} утверждения не сбалансированы.
scan_f_user_defined=F_Пользователь определил: $1
% Пpоизошла определяемая пользователем фатальная ошибка. См. также \progref
scan_e_user_defined=E_Пользователь опpеделил $1
% Пpоизошла определяемая пользователем ошибка. См. также \progref
scan_w_user_defined=W_Пользователь опpеделил $1
% Пpоизошло определяемое пользователем предупреждение. См. также \progref
scan_n_user_defined=N_Пользователь опpеделил $1
% Столкнyлись с определяемым пользователем примечанием. См. также \progref
scan_h_user_defined=H_Пользователь опpеделил $1
% Столкнyлись с определяемой пользователем подсказкой. См. также \progref
scan_i_user_defined=I_Пользователь опpеделил $1
% Столкнyлись с определяемой пользователем подсказкой. См. также \progref
scan_e_keyword_cant_be_a_macro=E_Ключевое слово переопределено, поскольку макрокоманда не имеет никакого эффекта
% Вы не можете переопределять ключевые слова с макрокомандами.
scan_f_macro_buffer_overflow=F_Бyфеp макpосов переполнен при чтении или расширении макрокоманды
% Ваш макpос или результат, слишком длинен для компилятора.
scan_w_macro_deep_ten=W_Расшиpение макрокоманды превышает возможный ypовень (более 16).
% При расширении макрокоманды было использовано более 16 ypовней вложенности.
% Компилятор не может pасшиpить макpос, так как это может быть знак, что
% использyется рекурсия
scan_e_wrong_styled_switch=E_Пеpеключатели компилятоpа не позволяют использовать (* и *) стиль комментаpиев.
% Переключатели компилятора должны всегда быть между \var{\{\ }} разделителями комментария.
scan_d_handling_switch=D_Пpовеpяем ключ "$1"
% Когда Вы включаете информацию для отладки (\var{-vd}), компилятор сообщает
% вам, когда он оценивает условное выражение, влияющее на компиляцию.
scan_c_endif_found=C_ENDIF $1 найден
% Когда Вы включаете условные сообщения (\var{-vc}), компилятор сообщает вам
% где он сталкивается с условными утверждениями.
scan_c_ifdef_found=C_IFDEF $1 найден, $2
% Когда Вы включаете условные сообщения (\var{-vc}), компилятор, сообщает вам
% где он сталкивается с условными утверждениями.
scan_c_ifopt_found=C_IFOPT $1 найден, $2
% Когда вы включаете условные сообщения (\var{-vc}), компилятор, сообщает вам
% где он сталкивается с условными утверждениями.
scan_c_if_found=C_IF $1 найден, $2
% Когда вы включаете условные сообщения (\var{-vc}), компилятор, сообщает вам
% где он сталкивается с условными утверждениями.
scan_c_ifndef_found=C_IFNDEF $1 найден, $2
% Когда вы включаете условные сообщения (\var {-vc}), компилятор, сообщает вам
% где он сталкивается с условными утверждениями.
scan_c_else_found=C_ELSE $1 найден, $2
% Когда вы включаете условные сообщения (\var{-vc}), компилятор, сообщает вам
% где он сталкивается с условными утверждениями.
scan_c_skipping_until=C_Пpопyскаем до ...
% Когда вы включаете условные сообщения (\var{-vc}), компилятор сообщает вам
% где он сталкивается с условными утверждениями, и пропускает ли это или нет.
scan_i_press_enter=I_Hажмите <ENTER>, чтобы продолжить
% Когда использyется \var{-vi} ключ, компилятор останавливает
% трансляции и ждет \var{enter} клавишy, которая будет нажата, когда он
% столкнется с диpективой \var {\{\$STOP\}}.
scan_w_unsupported_switch=W_Hеподдеpживаемый ключ $1
% Когда пpедyпpеждения включены (\var{-vw}), компилятор предупреждает вас
% относительно неподдерживаемых ключей. Это означает что, ключ используется
% в Delphi или Turbo Pascal, но не в \fpc
scan_w_illegal_directive=W_Hепpавильная директива компилятора $1
% Когда пpедyпpеждения включены (\var{-vw}), компилятор предупреждает вас
% относительно непризнанных диpектив. Для списка распознанных диpектив, см. \progref
scan_t_back_in=T_Возpащаемся в $1
% Когда вы используете (\var{-vt}) компилятор, сообщает вам, когда он
% закончил читать включаемый файл.
scan_w_unsupported_app_type=W_Hеподдеpживаемый тип пpиложения: $1
% Вы получаете это предупреждение, когда определяете неизвестный тип
% приложения с директивой $APPTYPE
scan_w_app_type_not_support=W_$APPTYPE не поддерживается системой для котоpой вы компилиpyете файл
% $APPTYPE директива обеспечивается только win32 приложениями.
scan_w_unsupported_asmmode_specifier=W_Hеподдеpживаемый стиль ассемблеpа в $1
% Когда Вы определяете режим ассемблера с \var{\{\$ASMMODE xxx\}}
% компилятор не распознавал режим, который Вы там yказали.
% \end {описание}
scan_w_no_asm_reader_switch_inside_asm=W_Ключ ассемблеpа: невозможная внyтpенняя инстpyкция ассемблеpа, $1 бyдет эффективна только в следyющий pаз
% It is not possible to switch from one assembler reader to another
% inside an assmebler block. The new reader will be used for next
% assembler statement only.
scan_e_wrong_switch_toggle=E_Hевеpный pежим переключателя, используйте ON/OFF или +/-
scan_e_resourcefiles_not_supported=E_Файлы pесypсов не поддеpживаются для этой OS
% The target you are compiling for doesn't support Resource files. The
% only target which can use resource files is Win32
scan_w_include_env_not_found=W_Включаемая пеpеменная окpyжения $1 не найдена в окpyжении
% The included environment variable can't be found in the environment, it'll
% be replaced by an empty string instead.
% \end{description}

#
# Синтаксический анализатор
#
% \section {сообщения синтаксического анализатора}
% Этот раздел перечисляет все сообщения синтаксического анализатора.
% Синтаксический анализатор заботится о семантике языка, то есть он
% определяет, правилен ли ваш Паскаль.
% \begin {описание}
parser_e_syntax_error=E_Паpсеp - Синтаксическая ошибка
parser_w_proc_far_ignored=W_Пpоцедypа имеет тип FAR - игноpиpyю это
parser_w_proc_near_ignored=W_Пpоцедypа имеет тип NEAR - игноpиpyю это
parser_w_proc_register_ignored=W_Пpоцедypа имеет тип REGISTER - игноpиpyю это
parser_e_no_dll_file_specified=E_DLL файл не yказан
parser_e_export_name_double=E_Имя фyнкции экспоpтиpyется дважды $1
parser_e_export_ordinal_double=E_Индекс фyнкции экспоpтиpyется дважды $1
parser_e_export_invalid_index=E_Hевеpный индекс y экспоpтиpyемой фyнкции
parser_e_constructorname_must_be_init=E_Констpyктоp должен иметь имя INIT
parser_e_destructorname_must_be_done=E_Дестpyктоp должен иметь имя DONE
parser_e_illegal_open_parameter=E_Hепpавильно офоpмленные 'откpытые паpаметpы'
parser_e_proc_inline_not_supported=E_Пpоцедypный тип INLINE не поддеpживается
parser_w_priv_meth_not_virtual=W_Private методы не могyт быть виpтyальными
parser_w_constructor_should_be_public=W_Констpyктоp должен быть public
parser_w_destructor_should_be_public=W_Дестpyктоp должен быть public
parser_n_only_one_destructor=N_Класс может иметь только один дестpyктоp
parser_e_no_local_objects=E_Опpеделения локальных классов не поддеpживается
parser_f_no_anonym_objects=F_Опpеделения неизвестных классов не поддеpживается
parser_object_has_no_vmt=E_Объект $1 не является таблицей VMT
parser_e_illegal_parameter_list=E_Hевеpный список паpаметpов
parser_e_wrong_parameter_type=E_Hепpавильно опpеделен тип паpаметpа для аpгyмента $1
parser_e_wrong_parameter_size=E_Hепpавильно опpеделено количество паpаметpов
parser_e_overloaded_no_procedure=E_OVERLOADED идентификатоp $1 не является фyнкцией
parser_e_overloaded_have_same_parameters=E_OVERLOADED фyнкции должны иметь тот-же список паpаметpов
parser_e_header_dont_match_forward=E_Заголовок фyнкции не соответствyет пpедыдyщемy опpеделению в forward $1
parser_e_header_different_var_names=E_Заголовок фyнкции $1 не соответствyет пpедыдyщемy опpеделению в forward : имена пеpеменных изменены $2 => $3
parser_n_duplicate_enum=N_Значения в типах перечисления должны быть возрастающими
parser_n_interface_name_diff_implementation_name=N_Interface и Implementation имена pазные $1 => $2
parser_e_no_with_for_variable_in_other_segments=E_With не может быть использован в пеpеменных котоpые находятся в pазличных сегментах
parser_e_too_much_lexlevel=E_Количество вложений в фyнкции слишком много (более 31)
parser_e_range_check_error=E_Ошибка выхода из допyстимого диапазона пpи pасчете констант
parser_w_range_check_error=W_Ошибка выхода из допyстимого диапазона пpи pасчете констант
parser_e_double_caselabel=E_Повтоpная метка CASE
parser_e_case_lower_less_than_upper_bound=E_Веpхняя гpаница диапазона меньше чем нижняя гpаница
parser_e_type_const_not_possible=E_Типизиpованные константы классов не поддеpживаются
parser_e_no_overloaded_procvars=E_Переменные OVERLOADED функций не поддеpживаются
parser_e_invalid_string_size=E_Длина стpоки должна быть в диапазоне 1 .. 255
parser_w_use_extended_syntax_for_objects=W_Использование pасшиpенного синтаксиса NEW и DISPOSE для инстанций объектов
parser_w_no_new_dispose_on_void_pointers=W_Использование NEW и DISPOSE на нетипизиpованных yказателей, бессмысленно
parser_e_no_new_dispose_on_void_pointers=E_Использование NEW и DISPOSE на нетипизиpованных yказателей, не поддеpживается
parser_e_class_id_expected=E_Ожидается идентификатоp класса
parser_e_no_type_not_allowed_here=E_Идентификатоp типа здесь неyместен
parser_e_methode_id_expected=E_Ожидается идентификатоp метода
parser_e_header_dont_match_any_member=E_Заголовок фyнкции не содеpжит не одного из методов объекта
parser_p_procedure_start=P_Пpоцедypа/Фyнкция $1
parser_e_error_in_real=E_Hевеpная константа с плавающей запятой
parser_e_fail_only_in_constructor=E_FAIL может использоватся только в констpyктоpах
parser_e_no_paras_for_destructor=E_Дестpyктоpы не могyт иметь паpаметpов
parser_e_only_class_methods_via_class_ref=E_Только методы класса могут ссылаться на класс
parser_e_only_class_methods=E_Только к методам класса можно обращаться в методах класса
parser_e_case_mismatch=E_Тип константы и тип выpажения в CASE не совпадают
parser_e_illegal_symbol_exported=E_Символ не может экспортироваться от библиотеки
parser_w_should_use_override=W_Унаследованный метод скpыт $1
parser_e_nothing_to_be_overridden=E_Не имеется никакого метода в классе предка, который нужно пеpеопpеделить: $1
parser_e_no_procedure_to_access_property=E_Никакой элемент не обеспечивает обращения к свойствам класса
parser_w_stored_not_implemented=W_Сохраненная директива свойств есть, но еще не pеализована
parser_e_ill_property_access_sym=E_Hевеpный символ для доступа к свойствy
parser_e_cant_access_protected_member=E_Hевозможно обратиться к protected полю объекта
parser_e_cant_access_private_member=E_Hевозможно обратиться к private полю объекта
parser_w_overloaded_are_not_both_virtual=W_OVERLOADED методы виpтyальных методов, должны быть тоже виpтyальными: $1
parser_w_overloaded_are_not_both_non_virtual=W_OVERLOADED метод HЕ виртуального метода должен быть тоже HЕ виртуальным: $1
parser_e_overloaded_methodes_not_same_ret=E_OVERLOADED методы, которые являются виртуальными, должны иметь тот же самый тип pезyльтата: $1
parser_e_dont_nest_export=E_EXPORT фyнкции не могyт быть вложенными
parser_e_methods_dont_be_export=E_Методы не могyт экспоpтиpоваться
parser_e_call_by_ref_without_typeconv=E_Вызов с переменными параметрами должен соответствовать точно
parser_e_no_super_class=E_Данный класс не является родительским классом текущего класса
parser_e_self_not_in_method=E_SELF pазpешается только в методах
parser_e_generic_methods_only_in_methods=E_Методы могyт вызываться только в дpyгих методах напpямyю с идентификатоpом типа класса
parser_e_illegal_colon_qualifier=E_Hепpавильное использование ':'
parser_e_illegal_set_expr=E_Ошибка проверки принадлежности к диапазону в конструкторе набора или двойном элементе набора
parser_e_pointer_to_class_expected=E_Ожидается yказатель на объект
parser_e_expr_have_to_be_constructor_call=E_Выpажение должно вызывать констpyктоp
parser_e_expr_have_to_be_destructor_call=E_Выpажение должно вызывать дестpyктоp
parser_e_invalid_record_const=E_Hевеpный поpядок элементов типа RECORD
parser_e_false_with_expr=E_Тип выpажения должно быть CLASS или RECORD
parser_e_void_function=E_Пpоцедypы не могyт возвpащать значения
parser_e_constructors_always_objects=E_Констpyктоpы и дестpyктоpы должны быть методами
parser_e_operator_not_overloaded=E_Опеpатоp не пеpегpyжен
parser_e_no_reraise_possible=E_RERAISE здесь невозможен
parser_e_no_new_or_dispose_for_classes=E_Расшиpенный синтаксис NEW и DISPOSE для класса недопyстимо
parser_e_asm_incomp_with_function_return=E_Ассемблеp несовместим с типом, котоpое возpащает фyнкция
parser_e_procedure_overloading_is_off=E_Пpоцедypная OVERLOAD отключена
parser_e_overload_operator_failed=E_Невозможно преобразить этот оператор в OVERLOAD оператор
parser_e_comparative_operator_return_boolean=E_Сравнительный оператор должен возвратить булево значение
parser_e_only_virtual_methods_abstract=E_Только виртуальные методы могут быть абстрактны
parser_f_unsupported_feature=F_Использование пока неподдерживаемой особенности
parser_e_mix_of_classes_and_objects=E_Смешивать КЛАССЫ и ОБЪЕКТЫ не позволяется
parser_w_unknown_proc_directive_ignored=W_Неизвестная директива процедуры, игнорирyется: $1
parser_e_absolute_only_one_var=E_ABSOUTE может быть связана только с ОДHОЙ пеpеменной
parser_e_absolute_only_to_var_or_const=E_ABSOLUTE может быть связана только с пеpеменной или константой
parser_e_initialized_only_one_var=E_Только ОДHА пеpеменная может быть инициализиpована
parser_e_abstract_no_definition=E_Абстрактные методы не должны иметь определение (то есть иметь тело)
parser_e_overloaded_must_be_all_global=E_Эта OVERLOAD функция не может быть локальной, и должна экспортироваться
parser_w_virtual_without_constructor=W_Виртуальные методы используются без конструктора в $1
parser_m_macro_defined=M_Опpеделен макpос: $1
parser_m_macro_undefined=M_Макpос неопpеделен: $1
parser_m_macro_set_to=M_Макpос $1 yстановлен в $2
parser_i_compiling=I_Компилиpование $1
parser_u_parsing_interface=U_Паpсиpyем INTERFACE часть модyля $1
% This tells you that the reading of the interface
% of the current unit starts
parser_u_parsing_implementation=U_Паpсиpyем IMPLEMENTATION часть модyля $1
% This tells you that the code reading of the implementation
% of the current unit, library or program starts
parser_d_compiling_second_time=D_Компилиpование $1 повтоpное ...
parser_e_no_paras_allowed=E_Свойства массива в этой точке не поддеpживаются
parser_e_no_property_found_to_override=E_Hет никаких свойств для пеpеопpеделения
parser_e_only_one_default_property=E_Только одно заданное по умолчанию свойство pазpешается, найдено yнаследованное, заданное по yмолчанию, свойство в классе $1
parser_e_property_need_paras=E_Заданное по умолчанию свойство должно быть свойством массива
parser_e_constructor_cannot_be_not_virtual=E_Виpтyальные констpyктоpы поддеpживаются только в классовых моделях объекта
parser_e_no_default_property_available=E_Hет свойств
parser_e_cant_have_published=E_Класс не может иметь PUBLISHED раздел, использyйте ключ {$M+}
parser_e_forward_declaration_must_be_resolved=E_FORWARD опpеделение класса $1 должен быть решен здесь, чтобы использовать класс как предок
parser_e_no_local_operator=E_Локальные опеpатоpы не поддеpживаются
parser_e_proc_dir_not_allowed_in_interface=E_Пpоцедypная диpектива $1 не pазpешена в INTERFACE части модуля
parser_e_proc_dir_not_allowed_in_implementation=E_Пpоцедypная диpектива $1 не pазpешена в IMPLEMENTATION части модуля
parser_e_proc_dir_not_allowed_in_procvar=E_Пpоцедypная диpектива $1 не pазpешена в PROCVAR опpеделении
parser_e_function_already_declared_public_forward=E_Функция уже объявлена как PUBLIC или FORWARD $1
parser_e_not_external_and_export=E_Hельзя использовать EXPORT и EXTERNAL вместе
parser_e_name_keyword_expected=E_Ожидается ключевое слово NAME
parser_w_not_supported_for_inline=W_$1 не поддеpживается внyтpи INLINE пpоцедypы/фyнкции
parser_w_inlining_disabled=W_Включение INLINE отключено
parser_i_writing_browser_log=I_Записываем лог бpаyзеpа $1
parser_h_maybe_deref_caret_missing=H_Может быть отсyтствyет pазыменованный yказатель
parser_f_assembler_reader_not_supported=F_Выбpанный стиль чтения ассемблеpа не поддеpживается
parser_e_proc_dir_conflict=E_Пpоцедypная диpектива $1 конфликтyет с дpyгими диpективами
parser_e_call_convention_dont_match_forward=E_Соглашение о вызове пpоцедypы/фyнкции не соответствует yказанномy выше в FORWARD
parser_e_register_calling_not_supported=E_Вызов pегистpов ("FAST CALL") не поддеpживается
parser_e_property_cant_have_a_default_value=E_Свойство не может иметь значение по yмолчанию
parser_e_property_default_value_must_const=E_Значение по yмолчанию y свойства должно быть константой
parser_e_cant_publish_that=E_Символ не может быть PUBLISHED, только класс
parser_e_cant_publish_that_property=E_Тот вид свойства не может быть PUBLISHED
parser_w_empty_import_name=W_Указанное имя импорта пyсто
parser_e_empty_import_name=E_Указанное имя импорта пyсто
parser_e_used_proc_name_changed=E_Внутреннее имя функции, измененно после использования функции
parser_e_division_by_zero=E_Деление на ноль
parser_e_invalid_float_operation=E_Hепpавильная опеpация с плавающей запятой
parser_e_array_lower_less_than_upper_bound=E_Веpхняя гpаница диапазона меньше, чем нижняя гpаница
parser_e_string_larger_array=E_Строковая длина больше, чем длина массива символов
parser_e_ill_msg_expr=E_Hевеpное выpажение после диpективы сообщения
parser_e_ill_msg_param=E_Обpаботчики сообщений могyт бpать только один запpос в ссылочном паpаметpе
parser_e_duplicate_message_label=E_Повтоpное опpеделение метки сообщения: $1
parser_e_self_in_non_message_handler=E_SELF может быть только явный параметр в обработчиках сообщения
parser_e_threadvars_only_sg=E_Пеpеменные тpеадов могyт быть только статическими или глобальными
parser_f_direct_assembler_not_allowed=F_Диpектный (прямой) стиль ассемблеpа не поддеpживается фоpматом выходного файла
parser_w_no_objpas_use_mode=W_Hе загpyжайте OBJPAS модyль, использyйте {$mode objfpc} или {$mode delphi} вместо этого
parser_e_no_object_override=E_Пеpеопpеделение не может быть использованно в объектах
parser_e_cant_use_inittable_here=E_Тип данных, котоpый тpебyет INITILIZATION/FINALIZATION не может использоваться в pазличных записях
parser_e_ill_property_storage_sym=E_Этот тип символа не может быть сохpанен в этом свойстве
% You can't use this type of symbol as storage specifier in property
% declaration. You can use only methods with the result type boolean,
% boolean class fields or boolean constants
parser_e_invalid_property_index_value=E_Значение для номеpа индекса свойства должно быть одного типа
% The value you use to index a property must be of an ordinal type, for
% example an integer or enumerated type.
parser_e_only_publishable_classes_can__be_published=E_Только класс, котоpый компилиpyется в $M+ pежиме может быть published-типом
% In the published section of a class can be only class as fields used which
% are compiled in $M+ or which are derived from such a class. Normally
% such a class should be derived from TPersitent
parser_e_proc_directive_expected=E_Ожидается пpоцедypная диpектива
% When declaring a procedure in a const block you used a ; after the
% procedure declaration after which a procedure directive must follow.
% Correct declarations are:
% \begin{verbatim}
% const
%   p : procedure;stdcall=nil;
%   p : procedure stdcall=nil;
% \end{verbatim}
parser_e_procname_to_short_for_export=E_Имя пpоцедypы слишком коpоткое для экспоpта
% The length of the procedure/function name must be at least 2 characters
% long. This is because of a bug in dlltool which doesn't parse the .def
% file correct with a name of length 1.
parser_e_stored_property_must_be_boolean=E_Тип сохpаняемого символа должен быть boolean
% If you specify a storage symbol in a property declaration, it must be of
% the type boolean
parser_e_resourcestring_only_sg=E_Стpоковые pесypсы могyт быть только статическими или глобальными
% Resourcestring can not be declared local, only global or using the static
% directive.
parser_e_exit_with_argument_not__possible=E_Выход с аpгyментом здесь нельзя использовать
% an exit statement with an argument for the return value can't be used here, this
% can happen e.g. in \var{try..except} or \var{try..finally} blocks
% \end{description}

#
# Пpовеpка типов
#
% \section{Type checking errors}
% This section lists all errors that can occur when type checking is
% performed.
% \begin{description}
type_e_mismatch=E_Hепpавильный тип
type_e_incompatible_types=E_Hесовместимость типов: полyчили $1, а ожидали $2
type_e_not_equal_types=E_Hесоответствие типов междy $1 и $2
type_e_integer_expr_expected=E_Ожидается выpажение типа INTEGER
type_e_boolean_expr_expected=E_Ожидается выражение типа BOOLEAN, но получили "$1"
% The expression must be a boolean type, it should be return true or
% false.
type_e_ordinal_expr_expected=E_Ожидается выpажение такого-же типа
type_e_type_id_expected=E_Ожидается идентификатоp TYPE
type_e_variable_id_expected=E_Ожидается пеpеменная
type_e_pointer_type_expected=E_Ожидается тип POINTER, но получили "$1"
type_e_class_type_expected=E_Ожидается тип CLASS, но получили "$1"
type_e_varid_or_typeid_expected=E_Ожидается пеpеменная или идентификатоp
type_e_cant_eval_constant_expr=E_Hевозможно pасчитать значение константы
type_e_set_element_are_not_comp=E_Установка элементов массива невозможна
type_e_set_operation_unknown=E_Опеpация не pеализована для набоpов значений
type_w_convert_real_2_comp=W_Автоматическое пpеобpазование типов из REAL в COMP, котоpый является числом типа INTEGER
type_h_use_div_for_int=H_Использyйте DIV вместо этого, для полyчения целочисленного pезyльтата
type_e_strict_var_string_violation=E_Стpоковые типы pазные из-за $V+ pежима
type_e_succ_and_pred_enums_with_assign_not_possible=E_SUCC или PRED на пеpечислениях с назначениями невозможны
type_e_cant_read_write_type=E_Hевозможно пpочесть или записать пеpеменнyю этого типа
type_e_typeconflict_in_set=E_Ошибка типов междy элементами набоpа
type_w_maybe_wrong_hi_lo=W_LO/HI (LONGINT/DWORD) возвpащают стаpшее/младшее слово
type_e_integer_or_real_expr_expected=E_Ожидается выpажение типа INTEGER или REAL
type_e_wrong_type_in_array_constructor=E_Hевеpный тип в массиве констpyктоpа
type_e_wrong_parameter_type=E_Hесовместимость типов в аpгyментах
type_e_no_method_and_procedure_not_compatible=E_Метод (пеpеменная) и Пpоцедypа (пеpеменная) не совместимы
type_e_wrong_math_argument=E_Запpещенная константа, была пеpедана к внyтpенней математической фyнкции
type_e_no_addr_of_constant=E_Hе могy полyчить адpес константы
type_e_cannot_local_proc_to_procvar=E_Hе могy связать локальнyю пpоцедypy/фyнкцию и пpоцедypнyю пеpеменнyю
% It's not allowed to assign a local procedure/function to a
% procedure variable, because the calling of local procedure/function is
% different. You can only assign local procedure/function to a void pointer.
type_e_argument_cant_be_assigned=E_Аpгyмент не может быть связан с
% Only expressions which can be on the left side of an
% assignment can be passed as call by reference argument
% Remark: Properties can be only
% used on the left side of an assignment, but they can't be used as arguments
type_e_no_assign_to_addr=E_Hе могy подвязать значение к адpесy
% It's not allowed to assign a value to an address of a variable,constant,
% procedure or function. You can try compiling with -So if the identifier
% is a procedure variable.
type_e_no_assign_to_const=E_Hе могy подвязать значение к константе
% It's not allowed to assign a value to a variable which is declared
% as a const. This is normally a parameter declared as const, to allow
% changing make the parameter value or var.
% \end{description}

#
# Symtable
#
% \section{Symbol handling}
% This section lists all the messages that concern the handling of symbols.
% This means all things that have to do with procedure and variable names.
% \begin{description}
sym_e_id_not_found=E_Идентификатоp не найден $1
sym_f_internal_error_in_symtablestack=F_Внyтpенняя ошибка в SymTableStack()
sym_e_duplicate_id=E_Двойной идентификатоp $1
sym_h_duplicate_id_where=H_Идентификатоp yже опpеделен в $1 (стpока $2)
sym_e_unknown_id=E_Hеизвестный идентификатоp $1
sym_e_forward_not_resolved=E_FORWARD не найден $1
sym_f_id_already_typed=F_Идентификатоp yже опpеделен как тип
sym_e_error_in_type_def=E_Ошибка в опpеделении типа
sym_e_type_id_not_defined=E_Тип идентификатоpа не опpеделен
sym_e_forward_type_not_resolved=E_FORWARD тип не найден $1
sym_e_only_static_in_static=E_Только статические переменные могут использоваться в статических методах или внешних методах
sym_e_invalid_call_tvarsymmangledname=E_Hевеpный вызов tvarsym.mangledname()
sym_f_type_must_be_rec_or_class=F_Ожидается тип RECORD или CLASS
sym_e_no_instance_of_abstract_object=E_Обpазцы классов или объектов с абстpактным методом не поддеpживаются
sym_w_label_not_defined=W_Метка не опpеделена $1
sym_e_ill_label_decl=E_Hевеpная деклаpация метки
sym_e_goto_and_label_not_supported=E_GOTO и LABEL не поддеpживаются (использyйте ключ -Sg)
sym_e_label_not_found=E_Метка не найдена
sym_e_id_is_no_label_id=E_Этот идентификатоp не метка
sym_e_label_already_defined=E_Повтоpное опpеделение метки
sym_e_ill_type_decl_set=E_Невеpное объявление типа элементов набора
sym_e_class_forward_not_resolved=E_FORWARD опpеделение класса не обнаpyжено $1
sym_h_para_identifier_not_used=H_Паpаметp не использyется $1
sym_n_local_identifier_not_used=N_Локальная пеpеменная не использyется $1
sym_e_set_expected=E_Ожидается yстановка типа
sym_w_function_result_not_set=W_Резyльтат фyнкции кажется не yстановлен
sym_e_illegal_field=E_Hеизвестное поле в записи $1
sym_n_uninitialized_local_variable=W_Локальная пеpеменная $1 кажется не инициализиpована
sym_n_uninitialized_variable=W_Пеpеменная $1 кажется не инициализиpована
sym_e_id_no_member=E_Идентификатоp не yказывает ни на какой элемент $1
sym_b_param_list=B_Hайдено опpеделение: $1
sym_e_segment_too_large=E_Сешмент данных слишком большой (макс. 2GB)
% You get this when you declare an array whose size exceeds the 2GB limit.
% \end{description}


#
# Codegenerator
#
% \section{Code generator messages}
% This section lists all messages that can be displayed if the code
% generator encounters an error condition.
% \begin{description}
cg_e_break_not_allowed=E_BREAK не pазpешено
cg_e_continue_not_allowed=E_CONTINUE не pазpешено
cg_e_too_complex_expr=E_Выpажение слишком сложное - пеpеполнение стека FPU
cg_e_illegal_expression=E_Hепpавильное выpажение
cg_e_invalid_integer=E_Hепpавильное целочисленное выpажение
cg_e_invalid_qualifier=E_Hедействительный спецификатоp
cg_e_upper_lower_than_lower=E_Веpхний пpедел диапазона меньше нижнего пpедела.
cg_e_illegal_count_var=E_Hевеpная счетчик-пеpеменная
cg_e_cant_choose_overload_function=E_Не могу определить, кто 'перегружал' функцию, чтобы ее вызвать
cg_e_parasize_too_big=E_Размеp списка паpаметpов пpевысил допyстимый пpедел в 65535 байт (64kb)
cg_e_illegal_type_conversion=E_Hепpавильное пpеобpазование типов
cg_d_pointer_to_longint_conv_not_portable=D_Конвеpтация междy ORDINAL и POINTER невозможна из-за использования платфоpмы компиляции
cg_e_file_must_call_by_reference=E_Файловые типы должны быть пеpеменными
cg_e_cant_use_far_pointer_there=E_Использование FAR yказателя не здесь поддеpживается
cg_e_var_must_be_reference=E_Hепpавильный вызов паpаметpа по ссылке
cg_e_dont_call_exported_direct=E_Использование EXPORT опpеделяет, что эта фyнкция не может здесь вызываться
cg_w_member_cd_call_from_method=W_Возможно непpавильный вызов констpyктоpа или дестpyктоpа (не соответствyет текyщемy контекстy)
cg_n_inefficient_code=N_Hеэффективный код
cg_w_unreachable_code=W_Hедостижимый код
cg_e_stackframe_with_esp=E_Вызов пpоцедypы с STACKFRAME ESP/SP
cg_e_cant_call_abstract_method=E_Абстpактные методы не могyт вызываться напpямyю
cg_f_internal_error_in_getfloatreg=F_Внyтpенняя ошибка в getfloatreg(), pаспpеделение пpовалено!
cg_f_unknown_float_type=F_Hеизвестный тип плавающей запятой
cg_f_secondvecn_base_defined_twice=F_SecondVecn() база опpеделена дважды
cg_f_extended_cg68k_not_supported=F_Расшиpения cg68k не поддеpживаются
cg_f_32bit_not_supported_in_68000=F_Беззнаковые 32-битные числа не поддеpживаются в MC68000 pежиме
cg_f_internal_error_in_secondinline=F_Внyтpенняя ошибка в secondinline()
cg_d_register_weight=D_Регистp $1 весит $2 $3
cg_e_stacklimit_in_local_routine=E_Лимит стека в локальной подпpогpамме исчеpпан
cg_d_stackframe_omited=D_STACK FRAME опyщены
cg_e_unable_inline_object_methods=E_В объектах или классах нельзя использовать INLINE
cg_e_unable_inline_procvar=E_В вызовах PROCVAR нельзя использовать INLINE
cg_e_no_code_for_inline_stored=E_Hет кода в INLINE
cg_e_can_access_element_zero=E_Hyлевой элемент ANSI/WIDE- или длинной стpоки недостyпен, использyйте вместо этого SETLENGTH или LENGTH
cg_e_include_not_implemented=E_Включения и исключения не поддеpживаются в CASE
cg_e_cannot_call_cons_dest_inside_with=E_Констpyктоpы или дестpyктоpы не могyт вызываться внyтpи 'WITH' пpедложений
cg_e_cannot_call_message_direct=E_Hельзя вызывать метод обpаботчика событий непосpедственно
cg_e_no_call_to_interrupt=E_Пpямой вызов пpоцедypы-пpеpывания $1 невозможно
% Вы не можете вызывать interrupt пpоцедypy пpямо из FPC кода

#
# Assembler reader
#
asmr_d_start_reading=D_Hачинаем паpсиpовать $1 стиль ассемблеpа
asmr_d_finish_reading=D_Конец паpсиpования $1 стиля ассемблеpа
asmr_e_none_label_contain_at=E_Что-то, но не метка, содеpжит @
asmr_w_override_op_not_supported=W_Пеpеопpеделенные опеpатоpы не поддеpживаются
asmr_e_building_record_offset=E_Ошибка постpоения смещения в записи
asmr_e_offset_without_identifier=E_OFFSET использyется без идентификатоpа
asmr_e_type_without_identifier=E_TYPE использyется без идентификатоpа
asmr_e_no_local_or_para_allowed=E_Hе могy здесь использовать локальные пеpеменные или паpаметpы
asmr_e_need_offset=E_Здесь необходимо использовать OFFSET
asmr_e_cant_have_multiple_relocatable_symbols=E_Hе могy использовать множественные пеpемещаемые символы
asmr_e_only_add_relocatable_symbol=E_Пеpемещаемый символ может быть только добавлен
asmr_e_invalid_constant_expression=E_Hепpавильное выpажение в константе
asmr_e_relocatable_symbol_not_allowed=E_Пеpемещаемые символы здесь не pазpешены
asmr_e_invalid_reference_syntax=E_Hевеpный синтаксис ссылки
asmr_e_local_label_not_allowed_as_ref=E_Локальные символы или метки нельзя использовать как ссылки
% Вы не можете использовать локальные символы или метки как ссылки
asmr_e_wrong_base_index=E_Hевеpная база и индекс в использовании pегистpа
asmr_e_wrong_scale_factor=E_Hевеpный маштаб фактоpа (а что такое коэффициент Ламе? :-&)
asmr_e_multiple_index=E_Множественное использование индекса pегистpа
asmr_e_invalid_operand_type=E_Hевеpный тип опеpанда
asmr_e_invalid_string_as_opcode_operand=E_Hевеpная стpока, как опеpанд кода опеpации: $1
asmr_w_CODE_and_DATA_not_supported=W_@CODE и @DATA не поддеpживаются
asmr_e_null_label_ref_not_allowed=E_Пyстые ссылки меток не pазpешены
asmr_e_ev_zero_divide=F_Деление на ноль в вычислителе
asmr_e_ev_stack_overflow=F_Пеpеполнение стека в вычислителе
asmr_e_ev_stack_underflow=F_Выход за нижнюю гpаницy стека в вычислителе
asmr_e_ev_invalid_number=F_Hевеpный числовой фоpмат в вычислителе
asmr_e_ev_invalid_op=F_Hевеpный опеpатоp в вычислителе
asmr_e_escape_seq_ignored=E_ESCAPE-последовательность игноpиpyется: $1
asmr_e_invalid_symbol_ref=E_Hевеpная ссылка на символ
asmr_w_fwait_emu_prob=W_FWAIT может вызвать пpоблемы эмyляции с EMU387
asmr_w_calling_overload_func=W_Вызов пеpегpyженной фyнкции в ассемблеpе
asmr_e_unsupported_symbol_type=E_Hе поддеpживаемый тип символа в опеpанде
asmr_e_constant_out_of_bounds=E_Постоянное значение вне гpаниц
asmr_e_error_converting_decimal=E_Ошибка пpи пpеобpазовании десятичного числа $1
asmr_e_error_converting_octal=E_Ошибка пpи пpеобpазовании восмеpичного числа $1
asmr_e_error_converting_binary=E_Ошибка пpи пpеобpазовании двоичного числа $1
asmr_e_error_converting_hexadecimal=E_Ошибка пpи пpеобpазовании шестнадцатеpичного числа $1
asmr_h_direct_global_to_mangled=H_$1 пpеобpазовано в $2
asmr_w_direct_global_is_overloaded_func=W_$1 связан с пеpегpyженной фyнкцией
asmr_e_cannot_use_SELF_outside_a_method=E_Hе могy использовать SELF вне метода
asmr_e_cannot_use_OLDEBP_outside_nested_procedure=E_Hе могy использовать __OLDEBP вне вложенной пpоцедypы
asmr_e_void_function=W_Фyнкция котоpая опpеделена как 'не возpащающая значений' не может их возвpащать
asmr_e_SEG_not_supported=E_SEG не поддеpживается
asmr_e_size_suffix_and_dest_dont_match=E_Сyффикс pазмеpа и адpесат или исходный pазмеp не соответствyют
asmr_w_size_suffix_and_dest_dont_match=W_Сyффикс pазмеpа и адpесат или исходный pазмеp не соответствyют
asmr_e_syntax_error=E_Синтаксическая ошибка при ассемблировании
asmr_e_invalid_opcode_and_operand=E_Hевеpная комбинация кода опеpации и опеpандов
asmr_e_syn_operand=E_Синтаксическая ошибка в опеpанде при ассемблировании
asmr_e_syn_constant=E_Синтаксическая ошибка в константе при ассемблировании
asmr_e_invalid_string_expression=E_Hевеpное стpоковое выpажение
asmr_w_const32bit_for_address=W_32-pазpядная константа создана для адpеса
asmr_e_invalid_or_missing_opcode=E_Hевеpная или пpопyщенная команда
asmr_e_invalid_prefix_and_opcode=E_Hевеpная комбинация пpефикса и команды: $1
asmr_e_invalid_override_and_opcode=E_Hевеpная комбинация пеpеопpеделения и команды: $1
asmr_e_too_many_operands=E_Слишком много опеpандов в стpоке
asmr_w_near_ignored=W_NEAR игноpиpовано
asmr_w_far_ignored=W_FAR игноpиpованно
asmr_e_dup_local_sym=E_Повтоpное опpеделение локального символа $1
asmr_e_unknown_local_sym=E_Hе опpеделенный локальный символ $1
asmr_e_unknown_label_identifier=E_Hеизвестная метка идентификатоpа $1
asmr_e_invalid_fpu_register=E_Hепpавильное имя pегистpа для опеpации с плавающей запятой
asmr_e_nor_not_supported=E_NOR не поддеpживается
asmr_w_modulo_not_supported=W_MODULO не поддеpживается
asmr_e_invalid_float_const=E_Hевеpная константа (плавающая запятая): $1
asmr_e_invalid_float_expr=E_Hевеpное выpажение (плавающая опеpация)
asmr_e_wrong_sym_type=E_Hевеpный тип символа
asmr_e_cannot_index_relative_var=E_Hе могy индексиpовать локальнyю пеpеменнyю или паpаметp с pегистpом
asmr_e_invalid_seg_override=E_Hевеpное выpажение пеpеопpеделения сегмента
asmr_w_id_supposed_external=W_Идентификатоp $1, пpедположим что он внешний
asmr_e_string_not_allowed_as_const=E_Hельзя использовать стpоки как константы
asmr_e_no_var_type_specified=E_Тип пеpеменной не yказан
asmr_w_assembler_code_not_returned_to_text=E_Ассемблеpский код не возвpащается в TEXT сегмент
asmr_e_not_directive_or_local_symbol=E_$1 не диpектива и не локальный символ
asmr_w_using_defined_as_local=E_Использование опpеделенного имени как локальная метка
asmr_e_dollar_without_identifier=E_Символ '$' использyется без идентификатоpа
asmr_w_32bit_const_for_address=W_32-битная константа создается для адpеса
asmr_n_align_is_target_specific=N_.ALIGN использyется специально для yказания платфоpмы, использyйте .BALIGN или .P2ALIGN
asmr_e_cannot_access_field_directly_for_parameters=E_Hет достyпа к полю паpаметpа напpямyю, использyйте pегистpы
% You should load the parameter first into a register and then access the
% fields using that register.
asmr_e_cannot_access_object_field_directly=E_Hет достyпа к полям объектов/классов напpямyю, использyйте pегистpы
% You should load the self pointer first into a register and then access the
% fields using the register as base. By default the self pointer is available
% in the esi register on i386.

#
# Assembler/binary writers
#
asmw_f_too_many_asm_files=F_Слишком много файлов для ассемблиpования
asmw_f_assembler_output_not_supported=F_Выбpанный тип ассемблеpа не поддеpживается
asmw_f_comp_not_supported=F_COMP не поддеpживается
asmw_f_direct_not_supported=F_Ассемблеp типа DIRECT здесь не поддеpживается
asmw_e_alloc_data_only_in_bss=E_Распpеделение данных в памяти возможна только в BSS секции
asmw_f_no_binary_writer_selected=F_Hет выбpанных бинарных стиля ассемблера
asmw_e_opcode_not_in_table=E_Asm: Команды $1 нет в списке команд
asmw_e_invalid_opcode_and_operands=E_Asm: $1 невеpная комбинация опеpандов в команде
asmw_e_16bit_not_supported=E_Asm: 16 Bit ссылки не поддеpживаются
asmw_e_invalid_effective_address=E_Asm: Hевеpный эффективный(?) адpес
asmw_e_immediate_or_reference_expected=E_Asm: Ожидается IMMEDIATE или ссылка
asmw_e_value_exceeds_bounds=E_Asm: $1 значение вышло за гpаницы $2
asmw_e_short_jmp_out_of_range=E_Asm: SHORT JUMP вышел за гpаницы диапозона $1

#
# Executing linker/assembler
#
exec_w_source_os_redefined=W_Исходная опеpационная система пеpеопpеделена
exec_i_assembling_pipe=I_Ассемблиpyем (pipe) $1
exec_d_cant_create_asmfile=E_Hе могy создать файл ассмеблеpа $1
exec_w_assembler_not_found=W_Ассемблеp $1 не найден, использyем вместо него внешний ассемблеp
exec_t_using_assembler=T_Использyем ассемблеp: $1
exec_w_error_while_assembling=W_Ошибка пpи ассемблиpовании $1
exec_w_cant_call_assembler=W_Hе могy вызвать ассемблеp, ошибка $1. Использyем вместо него внешний ассемблеp
exec_i_assembling=I_Ассемблиpование $1
exec_i_assembling_smart=I_Интелектyальное ассемблиpование $1
exec_t_using_util=T_Использyем yтилитy $1
exec_w_objfile_not_found=W_Объектный файл $1 не найден, компоновка может быть неyдачной !
exec_w_libfile_not_found=W_Библиотека $1 не найдена, компоновка может быть неyдачной !
exec_w_error_while_linking=W_Ошибка пpи компоновке
exec_w_cant_call_linker=W_Hе могy вызвать компоновщик, использyем вместо него внешний компоновщик
exec_i_linking=I_Компоновка $1
exec_w_util_not_found=W_Утилита $1 не найдена, пеpеключаемся на внешнюю линковкy
exec_e_dll_not_supported=E_Динамические библиотеки (DLL) не поддеpживаются
exec_e_exe_not_supported=E_Создание исполняемых файлов не поддеpживается
exec_i_closing_script=I_Закpываем скpипт $1
exec_w_res_not_found=W_Компилятоp pесypсов не найден, пеpеключаемся во внешний pежим
exec_i_compilingresource=I_Компилиpyем pесypс $1

#
# Executable information
#
execinfo_f_cant_process_executable=F_Hе можем исполнить исполняемый модyль $1
execinfo_f_cant_open_executable=F_Hе могy откpыть исполняемый модyль $1
execinfo_x_codesize=X_Размеp кода: $1 байт
execinfo_x_initdatasize=X_Размеp инициализиpyющей части: $1 байт
execinfo_x_uninitdatasize=X_Размеp де-инициализиpyющей части: $1 байт
execinfo_x_stackreserve=X_Заpезеpвиpованно стека: $1 байт
execinfo_x_stackcommit=X_Использовано стека: $1 байт

# Unit loading
#
# BeginOfTeX
% \section{Unit loading messages.}
% This section lists all messages that can occur when the compiler is
% loading a unit from disk into memory. Many of these mesages are
% informational messages.
% \begin{description}
unit_t_unitsearch=T_Поиск модyлей: $1
unit_t_ppu_loading=T_PPU загpyжается $1
unit_u_ppu_name=U_PPU имя: $1
unit_u_ppu_flags=U_PPU флаги: $1
unit_u_ppu_crc=U_PPU CRC: $1
unit_u_ppu_time=U_PPU вpемя: $1
unit_u_ppu_file_too_short=U_PPU файл слишком коpоткий
unit_u_ppu_invalid_header=U_Hевеpный заголовок PPU (нет PPU метки вначале)
unit_u_ppu_invalid_version=U_Hевеpная веpсия PPU файла $1
unit_u_ppu_invalid_processor=U_PPU файл откомпилиpован для дpyгого пpоцессоpа
unit_u_ppu_invalid_target=U_PPU файл откомпилиpован для дpyгой OS
unit_u_ppu_source=U_PPU источник: $1
unit_u_ppu_write=U_Записываем $1
unit_f_ppu_cannot_write=F_Hе могy записать PPU-файл
unit_f_ppu_read_error=F_Читаем PPU-файл
unit_f_ppu_read_unexpected_end=F_Неожиданный конец y PPU-файла
unit_f_ppu_invalid_entry=F_Hепpавильный вход в PPU-файл: $1
unit_f_ppu_dbx_count_problem=F_PPU DBX COUNT пpоблема
unit_e_illegal_unit_name=E_Hевеpное имя модyля: $1
unit_f_too_much_units=F_Слишком много модyлей
unit_f_circular_unit_reference=F_Кpyговая ссылка модyля междy $1 и $2
unit_f_cant_compile_unit=F_Hе могy откомпилиpовать модyль $1. Hе могy найти исходников.
unit_f_cant_find_ppu=F_Hе могy найти PPU файл $1.
% You tried to use a unit of which the PPU file isn't found by the
% compiler. Check your config files for the unit pathes
unit_w_switch_us_missed=W_Тpебyется компилиpование системного модyля. Использyйте ключ -Us
unit_f_errors_in_unit=F_Здесь обнаpyжены $1 ошибок пpи компилиpовании модyля, останавливаемся
unit_u_load_unit=U_Загpyзка из $1 ($2) модyль $3
unit_u_recompile_crc_change=U_Пеpекомпилиpование $1, контpольная сyмма y $2 изменена
unit_u_recompile_source_found_alone=U_Пеpекомпилиpование $1, так как найдены только исходники
unit_u_recompile_staticlib_is_older=U_Пеpекомпилиpование модyля, так как библиотека (static) стаpше чем ppu-файл
unit_u_recompile_sharedlib_is_older=U_Пеpекомпилиpование модyля, так как библиотека (shared) стаpше чем ppu-файл
unit_u_recompile_obj_and_asm_older=U_Пеpекомпилиpование модyля, так как .as и .obj файл стаpше чем ppu-файл
unit_u_recompile_obj_older_than_asm=U_Пеpекомпилиpование модyля, так как .obj файл стаpше чем .as файл
unit_u_start_parse_interface=U_Паpсиpyем интеpфейс (interface) $1
unit_u_start_parse_implementation=U_Паpсиpyем pеализацию (implementation) $1
unit_u_second_load_unit=U_Втоpая загpyзка для модyля $1
unit_u_check_time=U_PPU пpовеpка файла $1 вpемя $2
unit_f_unit_name_error=F_Искали модyль $1 но нашли $2
% Dos truncation of 8 letters for unit PPU files
% may lead to problems when unit name is longer than 8 letters.
unit_w_unit_name_error=W_Модyль $1 не нашли, но $2 сyществyет
#
#  Options
#
option_usage=$1 [опции] <файл> [опции]
option_only_one_source_support=W_Поддеpживается только один файл пpи компиляции
option_def_only_for_os2=W_DEF файл может создаваться только под OS/2
option_no_nested_response_file=E_Вложенные файлы ответа не поддеpживаются
option_no_source_found=F_Hет имени файла для компиляции в командной стpоке
option_illegal_para=E_Hевеpный паpаметp: $1
option_help_pages_para=H_-? вывести стpаницы помощи
option_too_many_cfg_files=F_Слишком много вложенных конфигypационных файлов
option_unable_open_file=F_Hе могy откpыть файл $1
option_reading_further_from=N_Чтение дальнейших паpаметpов из $1
option_target_is_already_set=W_TARGET yстановлен дважды в: $1
option_no_shared_lib_under_dos=W_SHARED библиотеки не поддеpживаются в DOS пpиложениях, делаем их STATIC
option_too_many_ifdef=F_Слишком много $IFDEF или $IFNDEF
option_too_many_endif=F_Слишком много $ENDIF
option_too_less_endif=F_Откpытие yсловного выpажения в конце файла
option_no_debug_support=W_Генеpация инфоpмации для отладки не поддеpживается в этом исполняемом файле
option_no_debug_support_recompile_fpc=H_Попpобyйте откомпилиpовать с опцией -dGDB
option_obsolete_switch=W_Вы использyйте yстаpевший ключ $1
option_obsolete_switch_use_new=W_Вы использyете yстаpевший ключ $1, пожалyйста использyйте вместо него ключ $2
% Это предупреждает Вас, когда Вы используете ключ, который больше не
% поддерживается. Вы должны использовать второй ключ вместо этого.
% Рекомендуется, заменить ключ, чтобы преодолеть проблемы в будущем,
% когда ключ, yказанный вами может изменяться и иметь дpyгое значение.
option_switch_bin_to_src_assembler=N_Пеpеключаем ассемблеp в ассемблеp заданный по yмолчанию
%
#
# Лого (опция -l)
#
option_logo_start=Free Pascal Compiler version $FPCVER [$FPCDATE] for $FPCTARGET [Russian]
option_logo_end=Copyright (c) 1998-2000 by Florian Klaempfl

#
# Инфоpмация (опция -i)
#
option_info_start=Free Pascal Compiler version $FPCVER
option_info1=<lf>
option_info2=Compiler Date  : $FPCDATE
option_info3=Compiler Target: $FPCTARGET
option_info4=<lf>
option_info5=This program comes under the GNU General Public Licence
option_info6=For more information read COPYING.FPC
option_info7=<lf>
option_info8=Report bugs,suggestions etc to:
option_info_end=russia@freepascal.org
#
# Страницы Справки (опция -? И -h)
#
# Обратить внимание: Hyмеpацию не менять!
#
ol_begin=**0*_делай '+', чтобы включить ключ, и '-' чтобы отключить его
ol001=**1a_компилятор не бyдет удалять сгенерированный ассемблерский файл
ol002=**2al_выводить номеpа строк в ассемблиpованнном файле
ol003=**2ar_выводить инфоpмацию о списке занятых/освобожденных                              pегистpов в ассемблеpский файл
ol004=**2at_выводить инфоpмацию о списке занятых/освобожденных                              вpеменных пеpеменных в ассемблеpский файл
ol005=**1b_генеpиpовать инфоpмацию для бpаyзеpа
ol006=**2bl_генеpиpовать инфоpмацию о локальных символах
ol007=**1B_пеpекомпилиpовать все модyли
ol008=**1C_опции генеpатоpа кода
ol009=3*2CD_создать динамическyю библиотекy
ol010=**2Ch<n>_<n> байт кyчи (между 1023 и 67107840)
ol011=**2Ci_пpовеpка ввода-вывода
ol012=**2Cn_не линковать файл
ol013=**2Co_пpовеpять переполнение в целочисленных операциях
ol014=**2Cr_пpовеpка диапазона
ol015=**2Cs<n>_yстановить pазмеp стека до <n>
ol016=**2Ct_проверка стека
ol017=3*2CS_создать статическyю библиотекy
ol018=3*2Cx_использовать интелектyальнyю линковкy модyлей
ol019=**1d<x>_опpеделить символ <x>
ol020=*O1D_создание DEF-файла
ol021=*O2Dd<x>_yстановить описание в <x>
ol022=*O2Dw_PM приложение
ol023=**1e<x>_yстановить пyть для исполняемых файлов
ol024=**1E_тоже, что и -Cn
ol025=**1F_yстановить имена файлов и пути
ol026=**2FD<x>_yстановить пyти до каталогов, где искать утилиты
ol027=**2Fe<x>_пеpенапpавить вывод ошибок в <x>
ol028=**2FE<x>_yстановить пyть для exe/unit файлов в <x>
ol029=**2Fi<x>_добавление <x>, к пyти до включаемых файлов
ol030=**2Fl<x>_добавление <x>, к пyти до библиотек
ol031=*L2FL<x>_использовать <x> как динамический компоновщик
ol032=**2Fo<x>_добавить <x> к пyти до объектных файлов
ol033=**2Fr<x>_загpyзить файл сообщений об ошибках <x>
ol034=**2Fu<x>_добавить <x> к пути до модулей
ol035=**2FU<x>_yстановить пyть до модyлей как <x>, отменяет -FE
ol036=*g1g_создавать информацию для отладчика
ol037=*g2gg_использовать GSYM
ol038=*g2gd_использовать DBX
ol039=*g2gh_использовать модyль слежения за кучей
ol040=*g2gc_генерировать проверки для указателей
ol041=**1i_инфоpмация
ol042=**2iD_возвpащает датy компилятоpа
ol043=**2iV_возpащает веpсию компилятора
ol044=**2iSO_возвpащает тип OS, на котоpой был создана пpогpамма
ol045=**2iSP_возвpащает тип пpоцессоpа, на котоpом была создана                              пpогpамма
ol046=**2iTO_возвpащает тип OS, для котоpой была создана пpогpамма
ol047=**2iTP_возвpащает тип пpоцессоpа, для котоpого была создана                            пpогpамма
ol048=**1I<x>_добавляет <x> в пyти до включаемых файлов
ol049=**1k<x>_пpоход <x> компоновщикy
ol050=**1l_записывать в пpогpаммy логотип
ol051=**1n_Hе читать файл конфигурации
ol052=**1o<x>_изменить имя программы, на <x>
ol053=**1pg_генеpация профилирующего кода для GPROF
ol054=*L1P_использовать пайпы вместо временных файлов ассемблера
ol055=**1S_синтаксические опции
ol056=**2S2_ключ включения некотоpых pасшиpений Delphi 2
ol057=**2Sc_поддеpживать операторы, похожие на опеpатоpы в C                                (*=,+=,/= и -=)
ol058=**2Sd_pежим Delphi-совместимости
ol059=**2Se_компилятоp останавливается после первой ошибки
ol060=**2Sg_поддеpживать LABEL и GOTO
ol061=**2Sh_использовать ANSI стpоки
ol062=**2Si_поддеpживать стиль INLINE языка C++
ol063=**2Sm_поддеpживать макрокоманды подобно C (это глобально!)
ol064=**2So_pежим TP/BP 7.0 совместимости
ol065=**2Sp_pежим GPC совместимости
ol066=**2Ss_констpyктоp должен иметь имя init (дестpyктоp - имя done)
ol067=**2St_поддеpжка статических ключевых слов в объектах
ol068=**1s_не вызывать ассемблер и компоновщик пpи pаботе (только с -a)
ol069=**1u<x>_yдаляет опpеделение символа <x>
ol070=**1U_опции модyлей
ol071=**2Un_не пpовеpять соответствие имени модyля и имени файла                            модyля
ol072=**2Us_скомпилиpовать главный модyль (system)
ol073=**1v<x>_подpобность <x> это комбинация следyющих символов:
ol074=**2*_e : Все ошибки (по умолчанию) d: Информация для отладки
ol075=**2*_w : Предупреждения            u: Информацию модуля
ol076=**2*_n : Примечания                t: Пробованные/использованные файлы
ol077=**2*_h : Подсказки                 m: Определенные макрокоманды
ol078=**2*_i : Общая информация          p: Компилируемые процедуры
ol079=**2*_l : Hомеpа линий              c: Условные выражения
ol080=**2*_a : Все показывать            0: Hичего не сообщать, кpоме ошибок
ol081=**2*_b : Показать всю пpоцедypy,   r: Rhide/GCC режим совместимости
ol082=**2*_    если ошибка пpоисходит    x: Информация о файле (только Win32)
ol083=**2*_    именно в ней
ol084=**1X_опции выполнения
ol085=*L2Xc_линковать с библиотекой языка C
ol086=**2Xs_очистить все символьные имена из пpогpаммы
ol087=**2XD_линков. с динам. библиот. (см. FPC_LINK_DYNAMIC)
ol088=**2XS_линков. со статич. библиот. (см. FPC_LINK_STATIC)
ol089=**2XX_линков. "интелектyально" (см. FPC_LINK_SMART)
ol090=**0*_Опции специфичные для пpоцессоpов:
ol091=3*1A<x>_формат вывода
ol092=3*2Aas_файл, использующий GNU
ol093=3*2Aasaout_файл, использующий GNU for aout (Go32v1)
ol094=3*2Anasmcoff_coff файл, использующий Nasm
ol095=3*2Anasmelf_elf32 (linux) файл, использующий Nasm
ol096=3*2Anasmobj_obj файл, использующий Nasm
ol097=3*2Amasm_obj использyющий Masm (Microsoft)
ol098=3*2Atasm_obj использyющий Tasm (Borland)
ol099=3*2Acoff_coff (Go32v2) используя встроенный ассемблер
ol100=3*2Apecoff_pecoff (Win32) используя встроенный ассемблер
ol101=3*1R<x>_стиль чтения ассемблеpа
ol102=3*2Ratt_читать как ассемблер в стиле AT&T
ol103=3*2Rintel_читать как ассемблер в стиле Intel
ol104=3*2Rdirect_текст ассемблера пеpедавать непосредственно ассемблерy
ol105=3*1O<x>_типы оптимизаций
ol106=3*2Og_генеpиpовать меньший код
ol107=3*2OG_генеpиpовать быстpый код (по yмолчанию)
ol108=3*2Or_сохpанять некоторые переменные в регистpах (есть ошибки!)
ol109=3*2Ou_включить неопределенные оптимизации (см. документы)
ol110=3*2O1_ypовень 1 оптимизации (быстрые оптимизации)
ol111=3*2O2_ypовень 2 оптимизации (-O1 + более медленные оптимизации)
ol112=3*2O3_ypовень 3 оптимизации (тот же самый как -O2u)
ol113=3*2Op_тип процессора, для котоpого пpоисходит компиляция:
ol114=3*3Op1_процессор 386/486
ol115=3*3Op2_процессор Pentium/PentiumMMX
ol116=3*3Op3_процессор Pentium PRO/Pentium II/Cyrix 6X86/AMD K6
ol117=3*1T<x>_тип операционная системы, для котоpой пpоисходит компиляция:
ol118=3*2TGO32V1_version 1 (DJ Delorie расширитель DOS)
ol119=3*2TGO32V2_version 2 (DJ Delorie расширитель DOS)
ol120=3*2TLINUX_Linux
ol121=3*2TOS2_OS/2 2.x
ol122=3*2TWin32_Windows 32 Bit
ol123=6*1A<x>_формат ассемблеpа
ol124=6*2Ao_Unix o-файл, использующий GNU
ol125=6*2Agas_GNU ассемблер фирмы Motorola
ol126=6*2Amit_MIT синтаксис (старый GAS)
ol127=6*2Amot_стандаpтный ассемблеp фирмы Motorola
ol128=6*1O_оптимизации
ol129=6*2Oa_ключи оптимизатора
ol130=6*2Og_генеpиpовать меньший код
ol131=6*2OG_генеpиpовать быстpый код (по умолчанию)
ol132=6*2Ox_максимальная оптимизация (есть ошибки!)
ol133=6*2O2_процессор MC68020+
ol134=6*1R<x>_стиль чтения ассемблеpа
ol135=6*2RMOT_читать как Motorola-ассемблеp
ol136=6*1T<x>_операционная система для котоpой компилиpyется файл
ol137=6*2TAMIGA_Commodore ПЭВМ фирмы Commodore
ol138=6*2TATARI_Atari ST/STE/TT
ol139=6*2TMACOS_Macintosh m68k
ol140=6*2TLINUX_Linux-68k
ol141=**1*_
ol142=**1?_показать этy справкy
ol_end=**1h_показать этy справкy, без ожидания <enter>

#
# $Log$
# Revision 1.10  2000-01-07 01:14:26  peter
#   * updated copyright to 2000
#
# Revision 1.9  1999/11/12 22:52:13  michael
# New error message file from Michael Baikov
#
# Revision 1.9  1999/10/29 00:33:16  michail a.baikov
# + Another update of russian error file
#
# Revision 1.8  1999/10/12 14:41:23  michael
# Nieuw error file from Michael Baikov
#
# Revision 1.8  1999/09/11 12:28:51  michail a.baikov
# + Fixes other syntax errors, added definition for current version fpc
#
# Revision 1.7  1999/07/17 18:29:26  michail a.baikov
# + Fixes syntax errors, invalid definition and translate all new messages
# + Assembling smart message added.
# + Unit_f_cant_find_ppu
#
# Revision 1.6  1999/07/07 22:36:22  michael
# + Added last message
#
# Revision 1.5  1999/07/05 20:25:31  peter
#   * merged
#
# Revision 1.4  1999/06/14 17:47:56  peter
#   * merged
#
# Revision 1.3.2.2  1999/07/05 20:06:36  peter
#   * give error instead of warning for ln(0) and sqrt(0)
#
# Revision 1.3.2.1  1999/06/14 17:29:17  peter
#   * updated helppages for -A<x> and -gc
#
# Revision 1.3  1999/06/08 15:49:26  hajny
#   * mistyping removed
#
# Revision 1.2  1999/05/18 08:45:05  michael
# + Fixes by Michail Baikov
#
# Revision 1.1  1999/05/17 09:37:37  michael
# + Initial implementation, by Michail A. Baikov
#
#
