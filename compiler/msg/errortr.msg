#
#   This file is part of the Free Pascal Compiler
#   Copyright (c) 1999-2019 by the Free Pascal Development team
#
#   English (default) Language File for Free Pascal
#   Türkçe çeviri: Onur ERÇELEN <onur2x at gmail.com>
#
#   See the file COPYING.v2, included in this distribution,
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
#   option_   command line parameter parsing
#   scan_     scanner
#   parser_   parser
#   type_     type checking
#   general_  general info
#   exec_     calls to assembler, external linker, binder
#   link_     internal linker
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
#   o_   normal (e.g., "press enter to continue")
#
# <type> can contain a minus sign at the beginning to mark that
# the message is off by default. Look at type_w_explicit_string_cast
# for example.

#
# General
#
# 01023 is the last used one
#
# BeginOfTeX
% \section{Genel derleyici mesajları}
% Bu bölüm derleyici mesajlarını ölümcül olmayan ancak
% yararlı bilgileri göster. Bu tür mesajların sayısı
% çeşitli ayrıntı seviyesi \var{-v} anahtarlarıyla kontrol edilir.
% \begin{description}
general_t_compilername=01000_T_Derleyici: $1
% \var{-vt} anahtarı kullanıldığında, bu satır hangi derleyicinin kullanıldığını 
% bildirir. 
general_d_sourceos=01001_D_Derleyici işletim sistemi: $1
% \var{-vd} anahtarı kullanıldığında, bu satır kaynak işletim sisteminin 
% ne olduğunu gösterir. 
general_i_targetos=01002_I_Hedef işletim sistemi: $1
% \var{-vd} anahtarı kullanıldığında, bu satır size hedef işletim sisteminin 
% ne olduğunu söyler.
general_t_exepath=01003_T_Yürütülebilir yolu kullanma: $1
% \var{-vt} anahtarı kullanıldığında, bu satır derleyicinin ikili dosyalarını 
% nerede aradığını gösterir.
general_t_unitpath=01004_T_Birim yolunu kullanma: $1
% \var{-vt} anahtarı kullanıldığında, bu satır derleyicinin derlenmiş birimleri 
% nerede aradığını gösterir. Bu yolu \var{-Fu} seçeneğiyle ayarlayabilirsiniz.
general_t_includepath=01005_T_Dahili dosya yolunu kullanma: $1
% \var{-vt} anahtarı kullanıldığında, bu satır derleyicinin içerme dosyalarını 
% nerede aradığını gösterir (\var{\{\$I xxx\}} deyimlerinde kullanılan dosyalar). 
% Bu yolu \var{-Fi} seçeneğiyle ayarlayabilirsiniz.
general_t_librarypath=01006_T_Kütüphane yolunu kullanma: $1
% \var{-vt} anahtarı kullanıldığında, bu satır derleyicinin kitaplıkları nerede 
% aradığını gösterir. Bu yolu \var{-Fl} seçeneğiyle ayarlayabilirsiniz.
general_t_objectpath=01007_T_Nesne yolunu kullanma: $1
% \var{-vt} anahtarı kullanıldığında, bu satır derleyicinin bağladığınız 
% nesne dosyalarını nerede aradığını gösterir (\var{\{\$L xxx\}} deyimlerinde 
% kullanılan dosyalar). Bu yolu \var{-Fo} seçeneğiyle ayarlayabilirsiniz.
general_i_abslines_compiled=01008_I_$1 derlenmiş satırlar, $2 sec$3
% \var{-vi} anahtarı kullanıldığında, derleyici derlenen satır sayısını 
% ve derlenmesi için geçen süreyi (program saati değil gerçek zaman) bildirir.
general_f_no_memory_left=01009_F_Hafıza kalmadı
% Derleyici, programınızı derlemek için yeterli belleğe sahip değil. 
% Bunun için birkaç çözüm var: 
% \begin{itemize}
% \öğe Derleyicinin derleme seçeneğini kullanıyorsanız, farklı birimleri
% elle derlemeyi deneyin.
% \öğe Büyük bir program derliyorsanız, birimlere ayırın ve ayrı ayrı derleyin.
% \öğe Önceki iki işe yaramazsa, derleyiciyi daha büyük bir yığınla yeniden derleyin.
% (Bunun için \var{-Ch} seçeneğini kullanabilirsiniz, \seeo{Ch}.)
% \end{itemize}
general_i_writingresourcefile=01010_I_Kaynak String Tablosu dosyası yazılıyor: $1
% Bu mesaj, derleyici bir programın tüm kaynak dizelerini içeren 
% Kaynak String Tablosu dosyasını yazdığında gösterilir.
general_e_errorwritingresourcefile=01011_E_Kaynak String Tablosu dosyası yazılıyor: $1
% Derleyici, Kaynak String Tablosu dosyasını yazarken bir hatayla karşılaştığında
% bu mesaj gösterilir.
general_i_fatal=01012_I_Ölümcül:
% Ölümcül Hatalar için Önek.
general_i_error=01013_I_Hata:
% Hatalar için Önek.
general_i_warning=01014_I_Uyarı:
% Uyarılar için Önek.
general_i_note=01015_I_Not:
% Notlar için önek.
general_i_hint=01016_I_İpucu:
% İpucu için önek.
general_e_path_does_not_exist=01017_E_Yol "$1" bulunamadı
% Belirtilen yol mevcut değil.
general_f_compilation_aborted=01018_F_Derleme iptal edildi
% Derleme iptal edildi.
general_text_bytes_code=01019_bayt kodu
% Oluşturulan yürütülebilir kodun bayt cinsinden boyutu.
general_text_bytes_data=01020_bayt veri
% Oluşturulan program verilerinin bayt cinsinden boyutu.
general_i_number_of_warnings=01021_I_$1 uyarılar verildi
% Derleme sırasında verilen toplam uyarı sayısı.
general_i_number_of_hints=01022_I_$1 verilen ipuçları
% Derleme sırasında verilen toplam ipucu sayısı.
general_i_number_of_notes=01023_I_$1 verilen notlar
% Derleme sırasında verilen toplam not sayısı.
general_f_ioerror=01024_F_I/O hatası: $1
% Derleme sırasında, başka derlemeye izin vermeyen bir G/Ç hatası oluştu.
general_f_oserror=01025_F_İşletim sistemi hatası: $1
% Derleme sırasında, başka derlemeye izin vermeyen bir işletim sistemi hatası oluştu.
% \end{description}
#
# Scanner
#
# 02098 is the last used one
#
% \section{Tarayıcı mesajları.}
% Bu bölüm tarayıcının yaydığı mesajları listeler. Tarayıcı, pascal dosyasının
% sözcük yapısına bakar, yani ayrılmış kelimeleri, karakterleri vb. Bulmaya çalışır.
% Ayrıca yönergelere ve koşullu derleme işlemlerine de özen gösterir.
% \begin{description}
scan_f_end_of_file=02000_F_Beklenmedik dosya sonu
% Bu genellikle aşağıdaki durumlardan birinde olur:
% \begin{itemize}
% \item Kaynak dosya final \var{end.} ifadesinden önce biter. Bu
% çoğunlukla \var{begin} ve \var{end} ifadeleri dengeli olmadığında olur;
% \item Bir include dosyası ifadenin ortasında biter.
% \item Bir yorum kapatılmadı.
% \end{itemize}
scan_f_string_exceeds_line=02001_F_String çizgiyi aşıyor
% Bir string de eksik bir kapanış var, bu yüzden birden fazla satır kaplıyor.
scan_f_illegal_char=02002_F_illegal karakter "$1" ($2)
% Giriş dosyasında geçersiz bir karakterle karşılaşıldı.
scan_f_syn_expected=02003_F_Sözdizimi hatası, "$1" olması gerekli ama "$2" var
% Bu, derleyicinin yazdığınızdan farklı bir simge beklediğini gösterir.
% Pascal diline karşı hata yapmak mümkün olan hemen hemen her yerde ortaya çıkabilir.
scan_t_start_include_file=02004_TL_Dahil bir dosya okunuyor $1
% \var{-vt} anahtarını sağladığınızda, derleyici içerdiği dosyayı okumaya 
% başladığında size bunu bildirir.
scan_w_comment_level=02005_W_Yorum seviyesi $1 bulundu
% \var{-vw} anahtarı kullanıldığında, derleyici iç içe geçmiş yorumlar bulursa
% derleyici sizi uyarır. Turbo Pascal'da iç içe yorumlara izin verilmez
% Delphi'de izin verilmez ve olası bir hata kaynağı olabilir.
scan_n_ignored_switch=02008_N_Derleyici anahtarı "$1" yoksayıldı
% \var {-vn} açık durumdayken, derleyici bir anahtarı yoksayarsa uyarır.
scan_w_illegal_switch=02009_W_Illegal derleyici anahtarı "$1"
% Derleyicinin tanımadığı bir derleyici anahtarı (ör. \var{\{\$... \}}) eklediniz.
scan_w_switch_is_global=02010_W_Yanlış yerleştirilmiş global derleyici anahtarı, yoksayıldı
% Derleyici anahtarı yanlış yerleştirilmiş. Kullanım cümlesi veya
% herhangi bir beyandan önce derleme ünitesinin başında bulunmalıdır.
scan_e_illegal_char_const=02011_E_Illegal karakter sabiti
% Bu, \var{\#96} 'da olduğu gibi ASCII koduyla bir karakter
% belirttiğinizde gerçekleşir, ancak sayı geçersiz veya aralık dışıdır.
scan_f_cannot_open_input=02012_F_Dosya açılamıyor "$1"
% \fpc, komut satırında belirttiğiniz program veya birim kaynak dosyasını bulamıyor.
scan_f_cannot_open_includefile=02013_F_Dahili dosya açılamıyor "$1"
% \ fpc, bir \var{\{\$include ..\}} ifadesinde belirttiğiniz kaynak dosyayı bulamıyor.
scan_e_illegal_pack_records=02015_E_Geçersiz kayıt hizalama belirticisi "$1"
% \var{n} için geçersiz bir değerle \var{\{\$PACKRECORDS n\} } veya \var{\{\$ALIGN n\} } 
% belirtiyorsunuz. \$PACKRECORDS için geçerli hizalamalar 
% 1, 2, 4, 8, 16, 32, C, NORMAL, DEFAULT ve \$ALIGN için geçerli hizalamalar 
% 1, 2, 4, 8, 16, 32, ON, OFF. MacPas \$ALIGN kipinde MAC68K, POWER ve RESET
% seçeneklerini de destekler.
scan_e_illegal_pack_enum=02016_E_Illegal enum minimum boyut belirteci "$1"
% \var{\{\$PACKENUM n\}} için \var{n} için geçersiz bir değer belirtiyorsunuz.
% Burada sadece 1,2,4, NORMAL veya DEFAULT geçerlidir.
scan_e_endif_expected=02017_E_$ENDIF $1 için bekleniyordu $2 tanımlanmış  $3 satır $4
% Koşullu derleme ifadeleriniz dengesiz.
scan_e_preproc_syntax_error=02018_E_Koşullu bir derleme ifadesini ayrıştırırken sözdizimi hatası
% İfadede \var{\{\$if ..\}}, \var{\{\$ifc \}} veya \var{\{\$setc \}}
% derleyici yönergelerini izleyen bir hata var.
scan_e_error_in_preproc_expr=02019_E_Koşullu bir derleyici ifadeyi değerlendirme
% İfadede \var{\{\$if ..\}}, $ifc veya $setc derleyici yönergelerini izleyen bir hata var.
scan_w_macro_cut_after_255_chars=02020_W_Makro içeriği 255 karakter uzunluğunda
% Makroların içeriği 255 karakterden uzun olamaz.
scan_e_endif_without_if=02021_E_ENDIF olmadan IF(N)DEF
% \var{\{\$IFDEF.. \}} ve {\{\$ENDIF \}} ifadeleriniz dengeli değil.
scan_f_user_defined=02022_F_Kullanıcı tanımlı: $1
% Kullanıcı tanımlı önemli bir hata oluştu. Ayrıca bkz. \progref.
scan_e_user_defined=02023_E_Kullanıcı tanımlı: $1
% Kullanıcı tanımlı bir hata oluştu. Ayrıca bkz. \progref.
scan_w_user_defined=02024_W_Kullanıcı tanımlı: $1
% Kullanıcı tanımlı bir hata oluştu. Ayrıca bkz. \progref.
scan_n_user_defined=02025_N_Kullanıcı tanımlı: $1
% Kullanıcı tanımlı bir notla karşılaşıldı. Ayrıca bkz. \progref.
scan_h_user_defined=02026_H_Kullanıcı tanımlı: $1
% Kullanıcı tanımlı bir ipucu ile karşılaşıldı. Ayrıca bkz. \progref.
scan_i_user_defined=02027_I_Kullanıcı tanımlı: $1
% Kullanıcı tanımlı bilgi ile karşılaşıldı. Ayrıca bkz. \progref.
scan_e_keyword_cant_be_a_macro=02028_E_Makro olarak yeniden tanımlanan anahtar kelimenin etkisi yoktur
% Anahtar kelimeleri makrolarla yeniden tanımlayamazsınız.
scan_f_macro_buffer_overflow=02029_F_Bir makroyu okurken veya genişletirken makro arabellek taşması
% Makronuz veya sonucu derleyici için çok uzundu.
scan_w_macro_too_deep=02030_W_Makroların genişletilmesi, 16 derinliğini aşıyor.
% Bir makroyu genişletirken, makrolar 16 seviyesine yerleştirildi. 
% Derleyici daha fazla genişlemeyecek, çünkü bu özyinelemenin 
% kullanıldığının bir işareti olabilir.
scan_w_wrong_styled_switch=02031_W_derleyici anahtarları içinde // tarz yorumlar desteklenmiyor 
% Derleyici anahtarları normal Pascal tarzı yorumlarda olmalıdır.
scan_d_handling_switch=02032_DL_Taşıma anahtarı "$1"
% Hata ayıklama bilgisini (\var{-vd}) üzerine ayarladığınızda,
% derleyici koşullu derleme deyimlerini değerlendirirken size bildirir.
scan_c_endif_found=02033_CL_ENDIF $1 bulundu
% Koşullu mesajları (\var{-vc}) açtığınızda,
% derleyici koşullu ifadelerle nerede karşılaştığını bildirir.
scan_c_ifdef_found=02034_CL_IFDEF $1 bulundu, $2
% Koşullu mesajları (\var{-vc}) açtığınızda,
% derleyici koşullu ifadelerle nerede karşılaştığını bildirir.
scan_c_ifopt_found=02035_CL_IFOPT $1 bulundu, $2
% Koşullu mesajları (\var{-vc}) açtığınızda,
% derleyici koşullu ifadelerle nerede karşılaştığını bildirir.
scan_c_if_found=02036_CL_IF $1 bulundu, $2
% Koşullu mesajları (\var{-vc}) açtığınızda,
% derleyici koşullu ifadelerle nerede karşılaştığını bildirir.
scan_c_ifndef_found=02037_CL_IFNDEF $1 bulundu, $2
% Koşullu mesajları (\var{-vc}) açtığınızda,
% derleyici koşullu ifadelerle nerede karşılaştığını bildirir.
scan_c_else_found=02038_CL_ELSE $1 bulundu, $2
% Koşullu mesajları (\var{-vc}) açtığınızda,
% derleyici koşullu ifadelerle nerede karşılaştığını bildirir.
scan_c_skipping_until=02039_CL_Kadar atlama...
% Koşullu mesajları (\var{-vc}) açtığınızda, derleyici,
% koşullu ifadelerle nerede karşılaştığını ve parçaları atlayıp atmadığını belirtir.
scan_i_press_enter=02040_I_Devam etmek için <enter> tuşuna basın
% \var{-vi} anahtarı kullanıldığında, derleyici derlemeyi durdurur
% ve \var{\{\$STOP\}} yönergesiyle karşılaştığında \var{Enter} tuşuna basılmasını bekler.
scan_w_unsupported_switch=02041_W_Desteklenmeyen anahtar "$1"
% Uyarılar açıldığında (\ var {-vw}), derleyici desteklenmeyen anahtarlar hakkında sizi uyarır.
% Bu, anahtarın Delphi veya Turbo Pascal'da kullanıldığı, ancak \fpc'de olmadığı anlamına gelir.
scan_w_illegal_directive=02042_W_Illegal derleyici yönergesi "$1"
% Uyarılar açıldığında (\var{-vw}), derleyici sizi tanınmayan anahtarlar hakkında uyarır.
% Tanınan anahtarların bir listesi için, bkz. \progref.
scan_t_back_in=02043_TL_Geride $1
% \var{-vt} anahtarını kullandığınızda, derleyici, bir dahil edilen dosyayı
% okumayı bitirdiğinde size haber verir.
scan_w_unsupported_app_type=02044_W_Desteklenmeyen uygulama türü: "$1"
% \var{\{\$APPTYPE\}} yönergesiyle bilinmeyen bir uygulama tipi
% belirtirseniz bu uyarıyı alırsınız.
scan_w_app_type_not_support=02045_W_APPTYPE hedef işletim sistemi tarafından desteklenmiyor
% \var{\{\$APPTYPE\}} Bu direktif hedef işletim sisteminde desteklenmiyor.
scan_w_description_not_support=02046_W_DESCRIPTION hedef işletim sistemi tarafından desteklenmiyor
% \var{\{\$DESCRIPTION\}} Bu direktif hedef işletim sisteminde desteklenmiyor.
scan_n_version_not_support=02047_N_VERSION is hedef işletim sistemi tarafından desteklenmiyor
% \var{\{\$VERSION\}} Bu direktif hedef işletim sisteminde desteklenmiyor.
scan_n_only_exe_version=02048_N_VERSION yalnızca exe veya DLL'ler için
% \var{\{\$VERSION\}} Yönerge sadece yürütülebilir veya DLL kaynakları için kullanılır.
scan_w_wrong_version_ignored=02049_W_VERSION direktifi için yanlış format "$1"
% \var{\{\$VERSION\}} yönerge formatı majorversion.minorversion olup,
% majorversion ve minorversion kelimedir.
scan_e_illegal_asmmode_specifier=02050_E_Illegal assembler stili belirtildi "$1"
% \var{\{\$ASMMODE xxx\}} yönergesine sahip bir montajcı modu
% belirttiğinizde, derleyici belirttiğiniz modu tanımıyordu.
scan_w_no_asm_reader_switch_inside_asm=02051_W_ASM okuyucu anahtarı asm ifadesinde mümkün değildir, "$1" sadece bir sonraki için etkili olacaktır
% Bir montajcı bloğu içinde bir montajcı okuyucusundan diğerine geçmek mümkün değildir.
% Yeni okuyucu yalnızca sonraki montajcı ifadeleri için kullanılacaktır.
scan_e_wrong_switch_toggle=02052_E_Yanlış anahtar değiştirme, ON/OFF veya +/- kullanın
% Düğmeyi değiştirmek için AÇIK veya KAPALI veya + veya - kullanmanız gerekir.
scan_e_resourcefiles_not_supported=02053_E_Kaynak dosyaları bu hedef için desteklenmiyor
% Derlemekte olduğunuz hedef kaynak dosyalarını desteklemiyor.
scan_w_include_env_not_found=02054_W_Çevrede bulunmayan "$1" ortamını dahil et
% Dahil edilen ortam değişkeni çevrede bulunamıyor; 
% bunun yerine boş bir string ile değiştirilecektir.
scan_e_invalid_maxfpureg_value=02055_E_FPU sicil limiti için geçersiz değer
% Bu direktif için geçerli değerler 0..8 ve NORMAL/DEFAULT.
scan_w_only_one_resourcefile_supported=02056_W_Bu hedef için yalnızca bir kaynak dosyası destekleniyor
% Derlediğiniz hedef yalnızca bir kaynak dosyasını destekler.
% Bulunan ilk kaynak dosya kullanılır, diğerleri atılır.
scan_w_macro_support_turned_off=02057_W_Makro desteği kapatıldı
% Bir makro bildirimi bulundu, ancak makro desteği şu anda kapalı,
% bu nedenle bildirim dikkate alınmayacak. 
% Makro desteğini komut satırında -Sm ile derlemek için veya kaynağa \{\$MACRO ON\} ekleyin.
scan_e_invalid_interface_type=02058_E_Geçersiz arabirim türü belirtildi. Geçerli değerler COM, CORBA veya DEFAULT'dur.
% Belirtilen arabirim türü desteklenmiyor.
scan_w_appid_not_support=02059_W_APPID yalnızca PalmOS için desteklenir
% \var{\{\$APPID\}} yönergesi yalnızca PalmOS hedefi için desteklenir.
scan_w_appname_not_support=02060_W_APPNAME yalnızca PalmOS için desteklenir
% \var{\{\$APPNAME\}} direktif yalnızca PalmOS hedefi için desteklenir.
scan_e_string_exceeds_255_chars=02061_E_Sabit stringler 255 karakterden uzun olamaz
% Tek bir dize sabiti en fazla 255 karakter içerebilir.
% Dize birden çok küçük parçaya bölün ve bir + işleci ile birleştirilir.
scan_f_include_deep_ten=02062_F_Dahil dosyaları dahil 16 derinliğini aşıyor.
% Dahil dosyaları dahil ederken, dosyalar 16 seviyesine yerleştirildi.
% Derleyici daha fazla genişletmeyecek, çünkü bu özyinelemenin kullanıldığının bir işareti olabilir.
scan_e_too_many_push=02063_F_Çok fazla PUSH seviyesi
% En fazla 20 seviyeye izin verilir. Bu hata yalnızca MacPas modunda gerçekleşir.
scan_e_too_many_pop=02064_E_A Önceki PUSH'siz POP
% Bu hata yalnızca MacPas modunda gerçekleşir.
scan_e_error_macro_lacks_value=02065_E_Makro veya derleme zamanı değişkeni "$1" herhangi bir değere sahip değil
% Bu nedenle koşullu derleme zamanı ifadesi değerlendirilemez.
scan_e_wrong_switch_toggle_default=02066_E_Yanlış düğme geçiş, ON/OFF/DEFAULT veya +/-/* kullanın
% Anahtarı değiştirmek için ON veya OFF veya DEFAULT veya a + veya - veya * kullanmanız gerekir.
scan_e_mode_switch_not_allowed=02067_E_"$1" mod anahtarına burada izin verilmez
% Bir mod anahtarına daha önce rastlanmış ya da -Mmacpas seçeneğinde, UNIT'den sonra bir mod anahtarı ortaya çıkar.
scan_e_error_macro_undefined=02068_E_Derleme zamanı değişkeni veya makro "$1" tanımlanmadı.
% Bu nedenle koşullu derleme zamanı ifadesi değerlendirilemez. Sadece MacPas modunda.
scan_e_utf8_bigger_than_65535=02069_E_UTF-8 65535'ten büyük kod bulundu
% \fpc UTF-8 dizelerini dahili olarak en geniş dizeler olarak işler, yani karakter kodları 65535 ile sınırlıdır.
scan_e_utf8_malformed=02070_E_Hatalı biçimlendirilmiş UTF-8 dizesi
% Verilen dize geçerli bir UTF-8 dizesi değil.
scan_c_switching_to_utf8=02071_C_UTF-8 kodlamasını kullanarak, UTF8 imza bulundu
% Derleyici, bir dosyanın başında bir UTF-8 kodlama imzası (\$ef, \$bb, \$bf) buldu,
% bu nedenle dosyayı UTF-8 dosyası olarak yorumluyor.
scan_e_compile_time_typeerror=02072_E_Derleme zamanı ifadesi: $1 istedi, ancak $3 'da $2 aldı 
% Derleme zamanı ifadesinin tür denetimi başarısız oldu.
scan_n_app_type_not_support=02073_N_APPTYPE hedef işletim sistemi tarafından desteklenmiyor
% \var{\{\$APPTYPE\}} yönergesi yalnızca belirli işletim sistemleri tarafından desteklenir.
scan_e_illegal_optimization_specifier=02074_E_Geçersiz optimizasyon "$1" olarak belirlendi
% \var{\{\$OPTIMIZATION xxx\}} yönergesiyle bir optimizasyon belirlediniz ve 
% derleyici belirttiğiniz optimizasyonu tanımıyor.
scan_w_setpeflags_not_support=02075_W_SETPEFLAGS hedef işletim sistemi tarafından desteklenmiyor
% \var{\{\$SETPEFLAGS\}} direktif, hedef işletim sistemi tarafından desteklenmiyor.
scan_w_imagebase_not_support=02076_W_IMAGEBASE hedef işletim sistemi tarafından desteklenmiyor
% \var{\{\$IMAGEBASE\}} direktif, hedef işletim sistemi tarafından desteklenmiyor.
scan_w_minstacksize_not_support=02077_W_MINSTACKSIZE hedef işletim sistemi tarafından desteklenmiyor
% \var{\{\$MINSTACKSIZE\}} direktif, hedef işletim sistemi tarafından desteklenmiyor.
scan_w_maxstacksize_not_support=02078_W_MAXSTACKSIZE hedef işletim sistemi tarafından desteklenmiyor
% \var{\{\$MAXSTACKSIZE\}} direktif, hedef işletim sistemi tarafından desteklenmiyor.
scanner_e_illegal_warn_state=02079_E_$WARN yönergesi için geçersiz "$1" durumu
% \Var{\{\$WARN\}} derleyici yönergesi yalnızca ON ve OFF durum olarak kullanılabilir.
scan_e_only_packset=02080_E_Illegal küme paketleme değeri
% Paket seti parametreleri olarak sadece 0, 1, 2, 4, 8, DEFAULT ve NORMAL izin verilir.
scan_w_pic_ignored=02081_W_PIC yönerge veya anahtar yoksayıldı
% \Windows gibi bazı hedefler PIC'yi desteklemez ve buna ihtiyaç duymaz,
% bu nedenle PIC yönergesi ve anahtar dikkate alınmaz.
scan_w_unsupported_switch_by_target=02082_W_"$1" anahtarı seçili olan hedef tarafından desteklenmiyor
% \$E gibi bazı derleyici anahtarları tüm hedefler tarafından desteklenmez.
scan_w_frameworks_darwin_only=02084_W_Çerçeve ile ilgili seçenekler sadece Darwin / Mac OS X için desteklenir
% Altyapılar bilinen bir kavram değil ya da en azından FPC tarafından
% Darwin /Mac OS X dışındaki işletim sistemlerinde desteklenmiyor.
scan_e_illegal_minfpconstprec=02085_E_Illegal minimum FLOOAT sabit hassasiyeti "$1"
% Float sabitleri için geçerli minimum hassasiyetler varsayılan, 32 ve 64'tür;
% bu, sırasıyla minimum (genellikle 32 bit), 32 bit ve 64 bit hassasiyet anlamına gelir.
scan_w_multiple_main_name_overrides=02086_W_"Ana" işlemin birden çok kez geçersiz kılma adı, önceden "$1" olarak ayarlandı.
% Ana giriş prosedürünün adı bir kereden fazla belirtildi. Sadece soyadı kullanılacak.
scanner_w_illegal_warn_identifier=02087_W_$WARN yönergesi için "$1" geçersiz kimliği
% Tanımlayıcı \var{\{\$WARN\}} derleyici yönergesi tarafından bilinmiyor.
scanner_e_illegal_alignment_directive=02088_E_Geçersiz hizalama yönergesi
% Hizalama yönergesi geçerli değil. Hizalama türü bilinmiyor
% veya hizalama değeri iki kişilik bir güç değil.
scanner_f_illegal_utf8_bom=02089_F_UTF-8 BOM ile başlayan bir dosyayı farklı bir kod sayfası kullanan bir modüle dahil etmek mümkün değildir
% Tek bir derleme öğesinin parçası olan tüm kaynak kodların
% (program, kütüphane, birim) aynı kod sayfasında kodlanması gerekir
scanner_w_directive_ignored_on_target=02090_W_Mevcut hedef platform için "$1" yönergesi yok sayılıyor
% Yönetilen platformlardaki paketlerin ve paket ayarlarının 
% değiştirilmesi gibi bazı hedefler için bazı yönergeler dikkate alınmaz.
scan_w_unavailable_system_codepage=02091_W_Mevcut sistem kod sayfası "$1" derleyici için mevcut değil. Varsayılan kod sayfasını tekrar "$2" olarak değiştiriyorum.
% Geçerli sistem kod sayfası derleyici tarafından bilinmiyor.
% Derleyici, yerleşik çeşitli kod sayfaları için destekle derlenir.
% İşletim sisteminin kod sayfası o listede yok.
% Derleyiciyi bu kod sayfasının desteğiyle yeniden derlemeniz gerekecek.
scan_w_setpeoptflags_not_support=02092_W_SETPEOPTFLAGS hedef işletim sistemi tarafından desteklenmiyor
% \var{\{\$SETPEOPTFLAGS\}} yönergesi hedef işletim sistemi tarafından desteklenmiyor.
scan_e_illegal_peflag=02093_E_SETPEFLAGS için geçersiz değişken
% SETPEFLAGS için verilen argüman ne doğru bir isim ne de bir sıra değeridir.
scan_e_illegal_peoptflag=02094_E_SETPEOPTFLAGS için illegal argüman
% SETPEOPTFLAGS için verilen argüman ne doğru bir isim ne de bir sıra değeridir.
scan_e_unsupported_switch=02095_E_$1 Direktifi bu hedefte desteklenmiyor
% Tüm derleyici yönergeleri tüm hedeflerde desteklenmemektedir.
scan_w_invalid_stacksize=02096_W_Belirtilen yığın boyutu, platform için geçerli aralıkta değil. Yığın boyutunu ayarlamak yoksayıldı.
% Yığın boyutu için geçerli aralık 32 bit ve 64 bit platformlarda 1024 - 67107839
% ve 16 bit platformlarda 1024 - 65520'dir. Ek olarak, Turbo Pascal 7 uyumluluk
% nedenlerinden ötürü, 16 bit platformlarda 65521 yığın boyutunu belirlemek,
% yığın boyutunu 65520 olarak ayarlar....
scan_w_heapmax_lessthan_heapmin=02097_W_Belirtilen HeapMax değeri, HeapMin değerinden daha küçük. HeapMax ayarı yoksayıldı.
% HeapMax değeri (belirtilmişse) HeapMin değerinden büyük veya ona eşit olmalıdır.
% Aksi takdirde, HeapMax değeri yoksayılır.
scan_e_illegal_hugepointernormalization=02098_E_HUGEPOINTERNORMALIZATION için geçersiz argüman
% HUGEPOINTERNORMALISATION için izin verilen tek değerler BORLANDC, MICROSOFTC ve WATCOMC'dir.
% \end{description}
#
# Parser
#
# 03338 is the last used one
#
% \section{Ayrıştırıcı mesajlar}
% Bu bölüm tüm ayrıştırıcı mesajlarını listeler. Ayrıştırıcı,
% dilinizin anlamını önemser, yani Pascal yapılarınızın doğru olup olmadığını belirler.
% \begin{description}
parser_e_syntax_error=03000_E_Ayrıştırıcı - Sözdizimi Hatası
% Turbo Pascal diline karşı bir hatayla karşılaşıldı.
% Bu genellikle kaynak dosyada geçersiz bir karakter bulunduğunda olur.
parser_e_dont_nest_interrupt=03004_E_INTERRUPT prosedürü yuvalanamıyor
% Bir \var{INTERRUPT} prosedürü global olmalıdır.
parser_w_proc_directive_ignored=03005_W_Prosedür tipi "$1" yoksayıldı
% Belirtilen prosedür yönergesi FPC programları tarafından göz ardı edilir.
parser_e_no_overload_for_all_procs=03006_E_"$1" bildirimlerinin tümü OVERLOAD ile bildirilmemiştir.
% \var{OVERLOAD} yönergesini kullanarak aşırı yüklemeyi kullanmak
% istediğinizde, tüm bildirimlerde \var{OVERLOAD} belirtilmiş olması gerekir.
parser_e_export_name_double=03008_E_Tekrar eden dışa aktarılan fonksiyon adı "$1"
% Belirli bir DLL içindeki verilen fonksiyon isimlerinin hepsi farklı olmalıdır.
parser_e_export_ordinal_double=03009_E_Tekrar eden dışa aktarılan fonksiyon indexi $1
% Belirli bir DLL içindeki verilen fonksiyon indexlerinin hepsi farklı olmalıdır.
parser_e_export_invalid_index=03010_E_Dışa aktarılan fonksiyon için geçersiz dizin
% DLL fonksiyon indexi \var{1..\$FFFF} aralığında olmalıdır.
parser_w_parser_reloc_no_debug=03011_W_Değiştirilebilir DLL veya çalıştırılabilir $1 hata ayıklama bilgisi çalışmıyor, devre dışı bırakılmış.
% Şu anda bir hata ayıklama bilgisini yer değiştirebilir bir DLL dosyasına dahil etmek mümkün değildir.
parser_w_parser_win32_debug_needs_WN=03012_W_Win32 kodu için hata ayıklamaya izin vermek için -WN seçeneğiyle yer değiştirmeyi devre dışı bırakmanız gerekir
% Stabs hata ayıklama bilgisi yer değiştirebilir DLL veya EXES için yanlıştır.
% Win32 çalıştırılabilir dosyalarında hata ayıklamak istiyorsanız -WN kullanın.
parser_e_constructorname_must_be_init=03013_E_constructor adı INIT olmalıdır
% \var{init} olmayan bir ada sahip bir nesne oluşturucuyu ilan ediyorsunuz
% ve \var{-Ss} anahtarı etkin. Bkz. \seeo{Ss} anahtarı.
parser_e_destructorname_must_be_done=03014_E_Destructor adı DONE olmalıdır
% \var{done} olmayan bir ada sahip bir nesne imha edici bildirirsiniz ve
% \var{-Ss} anahtarı etkindir. Bkz. \seeo{Ss} anahtarı.
parser_e_proc_inline_not_supported=03016_E_Prosedür tipi INLINE desteklenmiyor
% Bir programı C ++ stili satır içi ile derlemeye çalıştınız ve
% \var{-Si} seçeneğini (\seeo{Si}) belirtmeyi unutmuşsunuz.
% Derleyici, varsayılan olarak inlineing C ++ stilini desteklemiyor.
parser_w_constructor_should_be_public=03018_W_Constructor public de olmalı
% Constructors object (class) bildiriminin 'public' bölümünde bulunmalıdır.
parser_w_destructor_should_be_public=03019_W_Destructor public de olmalı
% Destructors object (class) bildiriminin 'public' bölümünde bulunmalıdır.
parser_n_only_one_destructor=03020_N_Class sadece bir destructor 'e sahip olmalı
% Class sadece bir destructor 'e sahip olmalı
parser_e_no_local_objects=03021_E_Yerel sınıf tanımlarına izin verilmiyor
% Classes küresel olarak tanımlanmalıdır. Bir prosedür veya fonksiyon içinde tanımlanamazlar.
parser_f_no_anonym_objects=03022_F_Anonim sınıf tanımlarına izin verilmez
% Geçersiz bir nesne (sınıf) bildirimi ile karşılaşıldı, yani bir
% başka bir nesne veya sınıftan türetilmemiş yöntemleri olmayan nesne veya sınıf. 
% Örneğin:
% \Begin{verbatim}
% O = nesne yazın
% a: longint;
% end;
% \end{verbatim}
% bu hatayı tetikleyecektir.
parser_n_object_has_no_vmt=03023_N_"$1" nesnesinin VMT'si yok
% Bu, bildirilen nesnenin sanal yöntem tablosu olmadığını gösteren bir nottur.
parser_e_illegal_parameter_list=03024_E_Geçersiz parametre listesi
% Fonksiyonun beyan ettiği parametrelerden farklı tipte
% parametrelere sahip bir fonksiyon çağırıyorsunuz.
parser_e_wrong_parameter_size=03026_E_"$1" çağrısı için belirtilen hatalı parametre sayısı
% İşlev veya prosedürün parametre listesinde bir hata var -
% Parametre sayısı doğru değil.
parser_e_overloaded_no_procedure=03027_E_aşırı yüklenen tanımlayıcı "$1" bir fonksiyon değil
% Derleyici, aşırı yüklenmiş bir fonksiyonla aynı isimde bir sembolle karşılaştı,
% ancak aşırı yüklenebileceği bir fonksiyon değil.
parser_e_overloaded_have_same_parameters=03028_E_aşırı yüklenmiş fonksiyonlar aynı parametre listesine sahiptir
% Aşırı yüklenmiş fonksiyonlar ilan ediyorsunuz ancak aynı parametre listesiyle.
% Aşırı yüklenmiş fonksiyonun beyanında en az 1 farklı parametre bulunmalıdır.
parser_e_header_dont_match_forward=03029_E_fonksiyon başlığı önceki bildirimde "$1" ile eşleşmiyor
% Aynı parametrelere sahip, ancak farklı sonuç türlerine veya
% fonksiyon değiştiricilere sahip bir fonksiyon açıkladınız.
parser_e_header_different_var_names=03030_E_"$1" fonksiyon başlığı ileriye uymuyor: var isim değişikliği $2 => $3
% Fonksiyonu \var{interface} bölümünde veya
% \var{forward} yönergesi, ancak farklı bir parametre listesiyle tanımlandı.
parser_n_duplicate_enum=03031_N_Numaralandırma türlerindeki değerler artan olmalıdır
% \fpc numaralandırma yapılarına C'deki gibi izin verir. Aşağıdakileri inceleyin
% iki bildirim:
% \begin{verbatim}
% yazın a = (A_A,A_B,A_E:=6,A_UAS:=200);
% yazın a = (A_A,A_B,A_E:=6,A_UAS:=4);
% \end{verbatim}
% İkinci bildiri hata verirdi. \var{A\_UAS} değerinin \var{A\_E} değerinden yüksek olması, yani en az 7 olması gerekir.
parser_e_no_with_for_variable_in_other_segments=03033_E_Farklı bir segmentteki değişkenler için kullanılamaz
% Yığında yerel olarak bir değişken depolar,
% ancak değişken başka bir segmente aitse bu mümkün değildir.
parser_e_too_much_lexlevel=03034_E_fonksiyon yerleştirme > 31
% fonksiyon tanımlarını yalnızca 31 seviye derinlikte yerleştirebilirsiniz.
parser_e_range_check_error=03035_E_sabitleri değerlendirirken aralık kontrol hatası
% Sabitler izin verilen aralığın dışında.
parser_w_range_check_error=03036_W_sabitleri değerlendirirken aralık kontrol hatası
% Sabitler izin verilen aralığın dışında.
parser_e_double_caselabel=03037_E_yinelenen case etiketi
% Aynı etiketi bir \var{case} deyiminde 2 kez belirtiyorsunuz.
parser_e_case_lower_less_than_upper_bound=03038_E_Case aralığının üst sınırı alt sınırdan daha az
% Bir \var{case} etiketinin üst sınırı alt sınırdan daha küçüktür ve bu işe yaramaz.
parser_e_type_const_not_possible=03039_E_classes ya da interfaces yazılan sabitlerine izin verilmez
% Bir tür veya nesne sabiti bildiremezsiniz.
parser_e_no_overloaded_procvars=03040_E_aşırı yüklenmiş fonksiyonların, fonksiyon değişkenlerine izin verilmez
% Bir yordamsal değişkene aşırı yüklenmiş bir fonksiyon atamaya çalışıyorsunuz.
% Buna izin verilmiyor.
parser_e_invalid_string_size=03041_E_string uzunluğu 1 ile 255 arasında bir değer olmalıdır
% Pascal'da bir kısa yolun uzunluğu 255 karakterle sınırlıdır.
% Uzunluğu 1'den küçük veya 255'ten büyük olan bir string bildirmeye çalışıyorsunuz.
parser_w_use_extended_syntax_for_objects=03042_W_Nesne örnekleri için genişletilmiş NEW ve DISPOSE sözdizimini kullanın.
% Bir nesne türüne \ var {a} işaretçiniz varsa, o zaman alan tahsis edilse de, \var{new(a)} ifadesi nesneyi başlatmaz (yani, yapıcı çağrılmaz).
% \var{new(a,init)} deyimini yayınlamalısınız. Bu alan tahsis edecek ve nesnenin yapıcısını arayacaktır.
parser_w_no_new_dispose_on_void_pointers=03043_W_yazılmamış işaretçiler için NEW veya DISPOSE kullanımı anlamsızdır
parser_e_no_new_dispose_on_void_pointers=03044_E_NEW veya DISPOSE kullanımı, yazılmamış işaretçiler için mümkün değildir.
%  if \var{p} bir yazılmamış işaretçi ise, \var{new(p)} veya \var{dispose(p)} kullanamazsınız,
% çünkü yazılmamış bir işaretçiyle hiçbir boyut ilişkilendirilmez. \var{TP} ve \var{DELPHI} modlarında uyumluluk için kabul edilir,
% ancak böyle bir yapı bulduğunda derleyici sizi uyaracaktır.
parser_e_class_id_expected=03045_E_sınıf tanımlayıcısı bekleniyor
% Bu, derleyici bir nokta, yani bir nesne veya sınıf yöntemi içeren bir işlem bildirimi taradığında gerçekleşir,
% ancak noktanın önündeki tür bilinen bir tür değildir.
parser_e_no_type_not_allowed_here=03046_E_tür tanımlayıcısına burada izin verilmiyor
% İfade içindeki bir türü kullanamazsınız.
parser_e_methode_id_expected=03047_E_yöntem tanımlayıcısı bekleniyor
% Bu tanımlayıcı bir yöntem değildir.
% Bu, derleyici nokta, yani bir nesne veya sınıf yöntemi içeren bir işlem bildirimi taradığında gerçekleşir,
% ancak işlem adı bu tür bir işlem değildir.
parser_e_header_dont_match_any_member=03048_E_fonksiyon başlığı "$1" sınıfının hiçbir metoduyla eşleşmiyor
% Bu tanımlayıcı bir yöntem değildir.
% Bu, derleyici nokta, yani bir nesne veya sınıf yöntemi içeren bir işlem bildirimi taradığında gerçekleşir,
% ancak işlem adı bu tür bir işlem değildir.
parser_d_procedure_start=03049_DL_prosedür/fonksiyon $1
% \var{-vd} anahtarını kullanırken, derleyici size bir prosedür veya
& fonksiyon uygulamasının ne zaman işlemeye başladığını bildirir.
parser_e_error_in_real=03050_E_Geçersiz floating nokta sabiti
% Derleyici bir floating nokta ifadesi bekliyor ve başka bir şey alıyor.
parser_e_fail_only_in_constructor=03051_E_FAIL sadece constructors da kullanılabilir
% \var{fail} anahtar sözcüğünü bir constructor yönteminin dışında kullanıyorsunuz.
parser_e_no_paras_for_destructor=03052_E_Destructors parametrelere sahip olamaz
% Destructor da Parametre listesi ilan ediyorsunuz. Destructor yöntemlerin parametreleri olamaz.
parser_e_only_class_members_via_class_ref=03053_E_Sadece sınıf yöntemleri, sınıf özellikleri ve sınıf değişkenleri sınıf referansları ile ilişkilendirilebilir.
% Bu hata aşağıdaki gibi bir durumda oluşur:
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
% \var{Free} bir sınıf yöntemi değildir ve bu nedenle bir sınıfla birlikte referans çağırılamaz.
parser_e_only_class_members=03054_E_Sınıf yöntemlerinde sadece sınıf yöntemleri, sınıf özellikleri ve sınıf değişkenleri erişilebilir.
% Bu önceki hata ile ilgilidir. Bir nesnenin yöntemini sınıf yönteminin içinden çağıramazsınız.
% Aşağıdaki kod bu hatayı üretecektir:
% \begin{verbatim}
% class procedure tobject.x;
%
% begin
%   free
% \end{verbatim}
% Çünkü Free, bir sınıfın normal bir yöntemi olduğundan, bir sınıf yönteminden çağrılamaz.
parser_e_case_mismatch=03055_E_Sabit ve CASE tipleri eşleşmiyor
% Etiketlerden biri Case değişkeni ile aynı türde değildir.
parser_e_illegal_symbol_exported=03056_E_Sembol bir kütüphaneden dışa aktarılamaz
% Bir kütüphane yazarken yalnızca prosedürleri ve fonksiyonları dışa aktarabilirsiniz.
% Değişkenleri veya sabitleri dışa aktaramazsınız.
parser_w_should_use_override=03057_W_inherited bir yöntem "$1" ile gizlenir
% Bir üst sınıfta \var{virtual} olarak bildirilen bir yöntem, soyundan gelen sınıfta
% \var{override} yönergesi ile geçersiz kılınmalıdır. \var{override} direktifini belirtmezseniz,
% ana yöntemi gizlersiniz; geçersiz kılamazsınız.
parser_e_nothing_to_be_overridden=03058_E_Ata sınıfında geçersiz kılınacak bir yöntem yoktur: "$1"
% Varolmayan bir ana sınıfın sanal yöntemini \var{override}.
parser_e_no_procedure_to_access_property=03059_E_Property erişmek için üye yok
% Bir özellik için \var{read} yönergesi belirtmediniz.
parser_w_stored_not_implemented=03060_W_Saklanan mülk direktifi henüz uygulanmadı
% \var{stored} yönergesi uygulandığından, bu mesaj artık kullanılmamaktadır.
parser_e_ill_property_access_sym=03061_E_Property erişimi için geçersiz sembol
% Bir dizi özelliği için \var{read} veya \var{write} yönergelerinde bir hata var.
% Bir dizi özelliği bildirdiğinizde, yalnızca prosedür ve fonksiyonlarla erişebilirsiniz.
% Aşağıdaki kod böyle bir hataya neden olur.
% \begin{verbatim}
% tmyobject = class
%   i : integer;
%   property x [i : integer]: integer read I write i;
% \end{verbatim}
%
parser_e_cant_access_protected_member=03062_E_Burada bir nesnenin protected alanına erişilemiyor
% Bir nesnenin veya sınıf bildiriminin bir \var{protected} bölümünde bildirilen alanlara,
% nesnenin tanımlandığı modülün dışına veya inen nesne yöntemlerinin dışına erişilemez.
parser_e_cant_access_private_member=03063_E_Burada bir nesnenin private alanına erişilemiyor
% Bir nesne veya sınıf bildiriminin \var{private} bölümünde bildirilen alanlara,
% sınıfın tanımlandığı modülün dışına erişilemez.
parser_e_overridden_methods_not_same_ret=03066_E_Geçersiz kılma yöntemlerinin aynı dönüş türüne sahip olması gerekir: "$2", başka bir dönüş türüne sahip "$1" tarafından geçersiz kılınır
% Geçersiz kılınan yöntemleri bir sınıf tanımında bildirirseniz, aynı dönüş türüne sahip olmaları gerekir.
parser_e_dont_nest_export=03067_E_EXPORT bildirilen fonksiyonlar yuvalanamıyor
% Dışa aktarma prosedürü olarak bildirilen bir fonksiyon veya prosedür içindeki bir fonksiyonu veya prosedürü ilan edemezsiniz.
parser_e_methods_dont_be_export=03068_E_Yöntem EXPORT edlemedi
% Bir nesnenin yöntemi olan bir prosedürü \var{export} olarak ilan edemezsiniz.
parser_e_call_by_ref_without_typeconv=03069_E_Arg no için var ile çağırın. $1 tam olarak eşleşmelidir: "$2" bekleniyor "$3" bekleniyor
% \var{var} parametreleriyle bildirilen bir fonksiyonu çağırırken, fonksiyon çağrısındaki değişkenlerin tamamen aynı türde olması gerekir.
% Otomatik tür dönüşümü yoktur.
parser_e_no_super_class=03070_E_Sınıf, geçerli sınıfın üst sınıfı değil
% inherited yöntemleri çağırırken, ilgili olmayan bir sınıfın yöntemini çağırmaya çalışıyorsunuz.
% Yalnızca bir ana sınıfın inherited bir yöntemini çağırabilirsiniz.
parser_e_self_not_in_method=03071_E_SELF sadece yöntemlerde izin verilir
% Bir nesnenin metodunun dışında \var{self} parametresini kullanmaya çalışıyorsunuz.
% Yalnızca yöntemler \var{self} parametrelerini aştı.
parser_e_generic_methods_only_in_methods=03072_E_Metotlar sadece sınıfın tip tanımlayıcısı ile direkt olarak adlandırılan diğer metodlarda olabilir.
% \var{sometype.somemethod} gibi bir yapıya yalnızca bir yöntemde izin verilir.
parser_e_illegal_colon_qualifier=03073_E_Geçersiz kullanım ':'
% \var{:} (colon) biçimini, gerçek ifade olmayan bir ifadede 2 kez kullanıyorsunuz.
parser_e_illegal_set_expr=03074_E_range check error in set constructor or duplicate set element
% Bir kümenin bildirimi bir hata içeriyor. Öğelerden biri ayarlanan tür
% aralığının dışında veya öğelerin ikisi aslında aynı.
parser_e_pointer_to_class_expected=03075_E_Pointer için beklenen nesne
% Bir \var{new} ifadesinde geçersiz bir tür belirttiniz.
% \var{new} genişletilmiş sözdizimi parametre olarak bir nesneye ihtiyaç duyar.
parser_e_expr_have_to_be_constructor_call=03076_E_İfade de constructor çağrısı olmalıdır
% \var{new}, genişletilmiş sözdizimini kullanırken, oluşturmaya çalıştığınız
% nesnenin constructor yöntemini belirtmelisiniz. Belirttiğiniz işlem bir constructor değil.
parser_e_expr_have_to_be_destructor_call=03077_E_İfade de destructor çağrısı olmalıdır
% \var{dispose}, dizgesinin genişletilmiş sözdizimini kullanırken,
% elden çıkarmaya çalıştığınız nesnenin destructor yöntemini belirtmelisiniz.
% Belirttiğiniz prosedür bir destructor değil.
parser_e_invalid_record_const=03078_E_Kayıt elemanlarının illegal düzeni
% Sabit bir kayıt bildirirken, alanları yanlış sırayla belirlediniz.
parser_e_false_with_expr=03079_E_İfade türü sınıf veya kayıt türü olmalı, $1
% Bir \var{with} deyimi, \var{record} veya \var{class} türünde bir argüman gerektirir.
% Bu var olmayan bir ifadede \var{with} kullanıyorsunuz.
parser_e_void_function=03080_E_Prosedürler bir değer döndüremez
% \Fpc'de, \var{exit} deyimini kullanırken bir fonksiyon için bir dönüş değeri belirleyebilirsiniz.
% Bu hata, bir yordamla yapmaya çalıştığınızda oluşur. Prosedürler bir değer döndüremez.
parser_e_only_methods_allowed=03081_E_constructors, destructors ve sınıf operatörleri yöntem olmalı
% Prosedür bir sınıf metodu değilse, bir prosedürü constructors, destructors
% veya sınıf operatörü olarak ilan ediyorsunuz.
parser_e_operator_not_overloaded=03082_E_Operatör aşırı yüklenmedi
% Bu tip için aşırı yüklenmediğinde aşırı yüklenmiş bir operatör kullanmaya çalışıyorsunuz.
parser_e_no_such_assignment=03083_E_Eşit tipler için atamayı aşırı yüklemek imkansız
% Derleyicinin eşit kabul ettiği türler için atamayı aşırı yükleyemezsiniz.
parser_e_overload_impossible=03084_E_Operatör aşırı yüklenmesi imkansız
% İşleç, argümanlar ve dönüş tipi birleşimi uyumsuz.
parser_e_no_reraise_possible=03085_E_Yeniden yükseltme orada mümkün değil
% İzin verilmediği bir istisnayı yeniden düzenlemeye çalışıyorsunuz.
% İstisnaları yalnızca bir \var{except} bloğunda yeniden kaldırabilirsiniz.
parser_e_no_new_or_dispose_for_classes=03086_E_Genişletilmiş yeni veya elden çıkarma sözdizimi bir sınıf için uygun değil
% \var{new} genişletilmiş sözdizimine sahip bir sınıf örneği oluşturamazsınız.
% Bunun için yapıcı kullanılmalıdır. Aynı nedenle, sınıfın bir örneğini ayırmak için
% \var{dispose} 'ı arayamazsınız, destructors bunun için kullanılmalıdır.
parser_e_procedure_overloading_is_off=03088_E_Prosedür aşırı yüklemesi kapatıldı
% \var{-So} anahtarını kullanırken, prosedür aşırı yüklemesi kapatılır.
% Turbo Pascal, fonksiyon aşırı yüklenmesini desteklemiyor.
parser_e_overload_operator_failed=03089_E_Bu operatörü aşırı yüklemek mümkün değildir. İlgili aşırı yüklenebilir operatörler (varsa): $1
% Aşırı yüklenemeyen bir operatörü aşırı yüklemeye çalışıyorsunuz.
% Aşağıdaki operatörler aşırı yüklenebilir:
% \begin{verbatim}
%    +, -, *, /, =, >, <, <=, >=, is, as, in, **, :=
% \end{verbatim}
parser_e_comparative_operator_return_boolean=03090_E_Karşılaştırma operatörü bir boolean değeri döndürmelidir
% \var{=} operatörüne aşırı yüklenirken, fonksiyon bir boolean değeri döndürmelidir.
parser_e_only_virtual_methods_abstract=03091_E_Sadece sanal yöntemler soyut olabilir
% Sanal olarak bildirilmediğinde, bir yöntemi soyut olarak ilan ediyorsunuz.
parser_f_unsupported_feature=03092_F_Desteklenmeyen özellik kullanımı: "$1".
% Derleyiciyi henüz yapamadığı bir şeyi yapmaya zorlamaya çalışıyorsun.
parser_e_mix_of_classes_and_objects=03093_E_Farklı tür nesnelerin (class, object, interface, vb.) Karıştırılmasına izin verilmez.
% İç içe \var{objects}, \var{classes}, \var{cppclasses} ve \var{interfaces} türetemezsiniz.
% Örneğin. Bir sınıf, ebeveyne sahip bir nesneye sahip olamaz ve bunun tersi de geçerlidir.
parser_w_unknown_proc_directive_ignored=03094_W_Bilinmeyen prosedür direktifinin göz ardı edilmesi gerekiyor: "$1"
% Belirttiğiniz prosedür yönergesi bilinmiyor.
parser_e_directive_only_one_var=03095_E_$1 sadece bir değişkenle ilişkilendirilebilir
% \var{absolute}, \var{export}, \var{external}, \var{weakexternal}, \var{public} ve
% \var{cvar} yönergelerinden önce birden fazla değişken belirtemezsiniz.
% Sonuç olarak, örneğin aşağıdaki yapı bu hatayı sağlayacaktır:
% \begin{verbatim}
% Var Z : Longint;
%     X,Y : Longint absolute Z;
% \end{verbatim}
parser_e_absolute_only_to_var_or_const=03096_E_absolute sadece bir var veya const ile ilişkili olabilir
% Bir \var{absolute} yönergesinin adresi sadece bir değişkeni veya sabiti işaret edebilir.
% Bu nedenle, aşağıdaki kod bu hatayı üretecektir:
% \begin{verbatim}
%   Procedure X;
%
%  var p : longint absolute x;
% \end{verbatim}
parser_e_initialized_only_one_var=03097_E_Sadece bir değişken başlatılabilir
% Delphi modunda başlangıç değeri olan birden fazla değişken belirtemezsiniz.
parser_e_abstract_no_definition=03098_E_Abstract yöntemlerinin herhangi bir tanımı olmamalıdır (fonksiyon gövdeli)
% Soyut yöntemler ancak ilan edilebilir, uygulayamazsınız.
% Soylu bir sınıf tarafından geçersiz kılınmaları gerekir.
parser_e_overloaded_must_be_all_global=03099_E_Bu aşırı yüklenmiş fonksiyon yerel olamaz (dışa aktarılmalıdır)
% Birimin uygulama bölümünde aşırı yüklenmiş bir fonksiyon tanımlıyorsunuz,
% ancak cihazın arayüz kısmında ilgili bir bildirim yok.
parser_w_virtual_without_constructor=03100_W_Sanal yöntemler "$1" de bir constructor olmadan kullanılır
% Sanal yöntemler içeren nesneleri veya sınıfları bildirirseniz,
% bunları başlatmak için bir constructor ve destructor sahip olmanız gerekir.
% Derleyici, constructor / destructor çiftine sahip olmayan
% sanal yöntemlerle bir nesneyle veya sınıfla karşılaştı.
parser_c_macro_defined=03101_CL_Makro tanımlı: $1
% \var{-vc} kullanıldığında, derleyici size makroları ne zaman tanımladığını bildirir.
parser_c_macro_undefined=03102_CL_Makro tanımsız: $1
% \var{-vc} kullanıldığında, derleyici tanımlanamayan makroları size bildirir.
parser_c_macro_set_to=03103_CL_$1 makro $2 'ye ayarlandı
% \var{-vc} kullanıldığında, derleyici size makroların hangi değerleri alacağını söyler.
parser_i_compiling=03104_I_Derleniyor $1
% Bilgi mesajlarını açtığınızda (\var {-vi}),
% derleyici size hangi birimleri derlediğini bildirir.
parser_u_parsing_interface=03105_UL_$1 birimi interface ayrıştırılıyor
% Bu, mevcut ünitenin interface okunmaya başladığını gösterir.
parser_u_parsing_implementation=03106_UL_İmplementation $1 ayrıştırılıyor
% Bu, geçerli birim, kütüphane veya programın uygulanmasının kod okumalarının başladığını gösterir.
parser_d_compiling_second_time=03107_DL_$1 ikinci kez derleniyor
% Hata ayıklama mesajları istediğinde (\var {-vd}) derleyici,
% hangi birimlerin ikinci kez yeniden derlendiğini size söyler.
parser_e_no_property_found_to_override=03109_E_Geçersiz kılınacak özellik bulunamadı
% Bir ana sınıfın bir özelliğini geçersiz kılmak istiyorsunuz,
% aslında, ana sınıfta böyle bir özellik yokken.
parser_e_only_one_default_property=03110_E_Yalnızca bir varsayılan özelliğe izin verilir
% Bir özelliği \var {Default} olarak belirlediniz, ancak sınıf zaten bir
% varsayılan özelliğe sahip ve bir sınıf yalnızca bir varsayılan özelliğe sahip olabilir.
parser_e_property_need_paras=03111_E_Varsayılan özellik bir dizi özelliği olmalıdır
% Sınıfların sadece dizi özellikleri \var {default} özellikler yapılabilir.
parser_e_constructor_cannot_be_not_virtual=03112_E_Sanal constructors yalnızca sınıf nesne modelinde desteklenir
% Nesnelerde sanal kurucularınız olamaz. Onları sadece sınıfta alabilirsin.
parser_e_no_default_property_available=03113_E_Varsayılan özellik yok
% Bir sınıfın varsayılan özelliğine erişmeye çalışıyorsunuz,
% ancak bu sınıfın (veya atalarından birinin) varsayılan bir özelliği yok.
parser_e_cant_have_published=03114_E_Sınıfın yayınlanmış bir bölümü olamaz, {$M+} anahtarını kullanın
% Sınıf tanımında bir \var {Published} bölümü istiyorsanız,
% tür bilgisinin oluşturulmasını sağlayan \var {\{\$M+\}} anahtarını kullanmanız gerekir.
parser_e_forward_declaration_must_be_resolved=03115_E_Sınıfı ata olarak kullanmak için "$1" sınıfının ileri bildirimi burada çözülmeli
% Bir nesneyi ata nesnesi olarak kullanabilmek için önce tanımlanması gerekir.
% Bu hata aşağıdaki durumda ortaya çıkar:
% \begin{verbatim}
%  Type ParentClas = Class;
%       ChildClass = Class(ParentClass)
%         ...
%       end;
% \end{verbatim}
% burada \var {ParentClass} bildirildi ancak tanımlanmadı.
parser_e_no_local_operator=03116_E_Yerel operatörler desteklenmiyor
% Prosedürlerin içinde veya fonksiyon tanımlarında Yerel olarak aşırı yükleme yapamazsınız.
parser_e_proc_dir_not_allowed_in_interface=03117_E_Interface bölümünde "$1" prosedür yönergesine izin verilmiyor
% This procedure directive is not allowed in the \var{interface} section of
% a unit. You can only use it in the \var{implementation} section.
parser_e_proc_dir_not_allowed_in_implementation=03118_E_Implementation bölümünde "$1" prosedür yönergesine izin verilmiyor
% Bu prosedür yönergesinin bir birimin \var {application} bölümünde
% bulunmasına izin verilmez. Sadece \var {interface} bölümünde kullanabilirsiniz.
parser_e_proc_dir_not_allowed_in_procvar=03119_E_Procvar bildirgesinde "$1" sayılı prosedür direktifine izin verilmiyor
% Bu prosedür yönergesi bir prosedür ya da fonksiyon türü bildiriminin parçası olamaz.
parser_e_function_already_declared_public_forward=03120_E_Fonksiyon zaten Public/Forward "$1" olarak tanımlandı 
% Bir fonksiyon iki kez \var {forward} olarak tanımlanırsa bu hatayı alırsınız.
% Veya, \var {interface} bölümünde ve yine \var {application} bölümünde bir \var {forward}
% bildirimi olarak gerçekleşirse bu hatayı alırsınız.
parser_e_not_external_and_export=03121_E_Hem EXPORT hemde EXTERNAL kullanılamaz
% Bu iki prosedür yönergesi karşılıklı olarak sınırlıdır.
parser_h_not_supported_for_inline=03123_H_"$1" satır içi procedure/function içinde henüz desteklenmiyor 
% Satır içi prosedürler bu bildirimi desteklemez.
parser_h_inlining_disabled=03124_H_Satır içi devre dışı
% İşlemlerin dizilmesi devre dışıdır.
parser_i_writing_browser_log=03125_I_Tarayıcı günlüğü $1 yazılıyor
% Bilgi mesajları açıkken, derleyici tarayıcı günlüğünü
% yazdığında sizi uyarır (\var {\{\$Y+\}} anahtarıyla oluşturulur).
parser_h_maybe_deref_caret_missing=03126_H_Pointer da referans eksik olabilir
% Derleyici, bir Pointer' in bir değişkenliğe ihtiyaç duyabileceğini düşünüyor.
parser_f_assembler_reader_not_supported=03127_F_Seçilen assembler okuyucusu desteklenmiyor
% Seçilen assembler okuyucusu (\var {\{\$ASMMODE xxx\}} ile desteklenmez.
% Derleyici, belirli bir assembler okuyucusu için veya desteğiyle derlenebilir.
parser_e_proc_dir_conflict=03128_E_"$1" numaralı prosedür yönergesi diğer yönergelerle çelişiyor
% Diğer yönergelerle çelişen bir prosedür yönergesi belirlediniz.
% Örneğin, \var {cdecl} ve \var {pascal} birbirini dışlayanlardır.
parser_e_call_convention_dont_match_forward=03129_E_Çağırma sözleşmesi ileriye uymuyor
% Bu hata, örneğin bir fonksiyon veya prosedür ilan ettiğinizde meydana gelir.
% \var {cdecl;} ancak uygulamada bu yönergeyi göz ardı edin veya bunun tersi de geçerlidir.
% Çağıran kural, fonksiyon bildiriminin bir parçasıdır ve fonksiyon tanımında tekrarlanmalıdır.
parser_e_property_cant_have_a_default_value=03131_E_Property varsayılan değeri olamaz
% Özellikleri ayarla veya dizine eklenen özellikler varsayılan değere sahip olamaz.
parser_e_property_default_value_must_const=03132_E_Bir Property varsayılan değeri sabit olmalıdır
% Derleme zamanında \ var {default} bildirilen özelliğin değeri bilinmelidir.
% Belirttiğiniz değer yalnızca çalışma zamanında bilinir.
% Bu, örneğin olur. Değişken adını varsayılan değer olarak belirtirseniz.
parser_e_cant_publish_that=03133_E_Sembol yayınlanamıyor, sadece bir sınıf olabilir
% Yalnızca sınıf tipi değişkenleri, bir özellik olarak bildirilmezlerse,
% bir sınıfın \var{published} bölümünde olabilir.
parser_e_cant_publish_that_property=03134_E_Bu tür bir mülk yayınlanamaz
% \var{published} bölümündeki özellikler, dizi özellikleri olamaz.
% Ortak bölümlere taşınmaları gerekir. Bir \var{published} bölümündeki
% özellikler sıralı bir tür, gerçek bir tür, dizeler veya kümeler olmalıdır.ts.
parser_e_empty_import_name=03136_E_Bir içe aktarma adı gerekli
% Bazı hedeflerin içe aktarma işlemi için bir isme veya bir \var{cdecl} belirticisine ihtiyacı vardır.
parser_e_division_by_zero=03138_E_Sıfıra bölüm
% Sıfıra bölünme ile karşılaşıldı.
parser_e_invalid_float_operation=03139_E_Floating operatoründe geçersiz  nokta
% İki gerçek tip değerdeki bir işlem bir taşma veya sıfıra bölünme üretti.
parser_e_array_lower_less_than_upper_bound=03140_E_Üst sınır aralığı alt sınırdan daha az
% Bir dizi bildiriminin üst sınırı, alt sınırdan daha az ve bu mümkün değil.
parser_w_string_too_long=03141_W_"$1" string "$2" dan daha uzun
% Sabit stringin boyutu, string türü tanımlamasında belirttiğiniz boyuttan daha büyük.
parser_e_string_larger_array=03142_E_string uzunluğu, karakter uzunluğu dizisinden daha büyük
% Sabit stringin boyutu, \var{Array[x..y] of char} öğesinde belirttiğiniz boyuttan daha büyük.
parser_e_ill_msg_expr=03143_E_Mesaj yönergesi sonrası Illegal ifade
% \fpc Mesaj sabitleri olarak sadece tamsayı veya string değerlerini destekler.
parser_e_ill_msg_param=03144_E_Mesaj işleyicileri ref parametresi ile sadece bir çağrı alabilir.
% İleti işleyicisi olarak \var{message} yönergesiyle bildirilen bir yöntem,
% başvuru çağrısı olarak bildirilmesi gereken yalnızca bir parametre alabilir.
% Parametreler \var{var} -directive kullanılarak çağrı olarak çağrıldı.
parser_e_duplicate_message_label=03145_E_Yinelenen ileti LABEL: "$1"
% Bir mesaj için bir LABEL bir object/class da iki kez kullanılır.
parser_e_self_in_non_message_handler=03146_E_Self, yalnızca mesaj işleyicileri olan yöntemlerde açık bir parametre olabilir.
% \var{Self} parametresi yalnızca ileti işleyicisi olarak bildirilen bir yönteme açıkça iletilebilir.
parser_e_threadvars_only_sg=03147_E_Threadvarlar sadece statik veya global olabilir
% Threadvars statik veya global olmalıdır; Yerel bir thread bir procedure bildiremezsiniz.
% Yerel değişkenler her zaman bir thread yereldir,
% çünkü her threadın kendi yığını vardır ve yerel değişkenler yığında depolanır.
parser_f_direct_assembler_not_allowed=03148_F_Doğrudan assembler binary çıkış formatı için desteklenmiyor
% Binary bir yazıcı kullanırken doğrudan assembler kullanamazsınız.
% Başka bir çıktı formatı seçin veya başka bir assembler okuyucusu kullanın.
parser_w_no_objpas_use_mode=03149_W_OBJPAS birimini manuel olarak yüklemeyin, bunun yerine {$mode objfpc} veya {$mode delphi} kullanın
% \file{ObjPas} birimini bir \var{uses} yan tümcesinden el ile yüklemeye çalışıyorsunuz.
% Bu iyi bir fikir değil. Üniteyi otomatik olarak yükleyen \var{\{\$MODE OBJFPC\}} veya \var{\{\$mode delphi\}} yönergelerini kullanın.
parser_e_no_object_override=03150_E_OVERRIDE nesnelerde kullanılamaz
% \var{Override} nesneler için desteklenmiyor, üst nesnenin bir yöntemini geçersiz kılmak yerine \var{virtual} kullanın.
parser_e_cant_use_inittable_here=03151_E_initialization/finalization gerektiren veri türleri değişken kayıtlarda kullanılamaz
% Bazı veri türleri (örneğin, \var{ansistring}) derleyici tarafından dolaylı olarak üretilen
% initialization/finalization koduna ihtiyaç duyar. Bu tür veri türleri bir kaydın değişken bölümünde kullanılamaz.
parser_e_resourcestring_only_sg=03152_E_Resourcestrings yalnızca statik veya genel olabilir
% Resourcestring araştırması yerel olarak ilan edilemez, yalnızca genel veya statik direktif kullanılarak yapılabilir.
parser_e_exit_with_argument_not__possible=03153_E_Exit argumanı burada kullanılamaz
% Burada dönüş değeri için argüman olan bir exit ifadesi kullanılamaz.
% Bu, örneğin \var{try..except} veya \var{try..finally} bloklarında olabilir.
parser_e_stored_property_must_be_boolean=03154_E_Depolama sembolünün tipi boolean olmalıdır
% Bir özellik bildiriminde bir depolama simgesi belirtirseniz, bir boolean türü olmalıdır.
parser_e_ill_property_storage_sym=03155_E_Bu sembole depolama sembolü olarak izin verilmiyor
% Bu tür bir simgeyi özellik bildiriminde depolama belirteci olarak kullanamazsınız.
% Yalnızca boolean, boolean class alanları veya boolean sabitleri türündeki yöntemleri kullanabilirsiniz.
parser_e_only_publishable_classes_can_be_published=03156_E_Yalnızca $M+ modunda derlenen sınıflar yayınlanabilir
% Bir sınıfın yayınlanmış bölümündeki bir sınıf tipi alan sadece \var{\{\$M+\}}
% içerisinde derlenmiş veya böyle bir sınıftan türetilmiş bir sınıf olabilir.
% Normalde böyle bir sınıf \var{TPersistent} 'dan türetilmelidir.
parser_e_proc_directive_expected=03157_E_Procedure direktifi bekleniyor
% Bu hata, belirtilen bir çağrı kuralı olmadan bir \var{\{\$Calling\}} yönergesine sahip
% olduğunuzda tetiklenir. Ayrıca bir const bloğunda bir prosedür
% bildirilirken de olur ve a; bir prosedürün ardından, takip edilmesi gereken bir
% prosedür yönergesi.
% Doğru beyanlar:
% \begin{verbatim}
% const
%   p : procedure;stdcall=nil;
%   p : procedure stdcall=nil;
% \end{verbatim}
parser_e_invalid_property_index_value=03158_E_Bir özellik endeksinin değeri sıralı bir tür olmalıdır
% Bir özelliği dizine almak için kullandığınız değer, örneğin bir tam sayı veya numaralandırılmış tür gibi sıralı bir tür olmalıdır.
parser_e_procname_to_short_for_export=03159_E_Procedure adı dışa aktarılmayacak kadar kısa
% Prosedür/fonksiyon adının uzunluğu en az 2 karakter uzunluğunda olmalıdır.
% Bu, dlltool içindeki .def dosyasını 1 uzunluğunda bir adla doğru bir şekilde ayrıştırmayan bir hata nedeniyledir.
parser_e_dlltool_unit_var_problem=03160_E_Unit genel değişkenleri için DEFFILE girişi oluşturulamıyor
parser_e_dlltool_unit_var_problem2=03161_E_-WD seçeneği olmadan derle
% Bu dosyayı komut satırında -WD anahtarı olmadan derlemeniz gerekir.
parser_f_need_objfpc_or_delphi_mode=03162_F_Bu modülü derlemek için ObjFpc(-S2) veya Delphi(-Sd) moduna ihtiyacınız var
% Bu dosyayı derlemek için \var{\{\$MODE OBJFPC\}} veya \var{\{\$MODE DELPHI\}} kullanmanız gerekir.
% Veya karşılık gelen komut satırı anahtarını kullanın, \var{-Mobjfpc} veya \var{-MDelphi.}
parser_e_no_export_with_index_for_target=03163_E_$1 'ın altındaki endeks ile dışa aktarılamıyor
% Fonksiyonların veya prosedürlerin belirli bir dizine sahip dışa aktarılması bu hedefte desteklenmiyor.
parser_e_no_export_of_variables_for_target=03164_E_Değişkenlerin dışa aktarılması $1 altında desteklenmiyor
% Değişkenlerin dışa aktarılması bu hedefte desteklenmiyor.
parser_e_improper_guid_syntax=03165_E_Yanlış GUID sözdizimi
% GUID göstergesi doğru sözdizimine sahip değil. Aşağıdaki Şekilde olmalı
% \begin{verbatim}
% {XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}
% \end{verbatim}
% Her bir var \var{X} onaltılık bir basamağı temsil eder.
parser_w_interface_mapping_notfound=03168_W_"$1" adlı prosedür, $2 'ı uygulamak için uygun bulunmadı. $3
% Derleyici, bir arayüz için verilen yöntemi uygulayan uygun bir prosedür bulamamaktadır.
% Aynı ada sahip bir prosedür bulundu, ancak argümanlar eşleşmiyor.
parser_e_interface_id_expected=03169_E_interface tanımlayıcısı bekleniyor
% Bu, derleyici, \var{interface} fonksiyon adı eşleme kodunu içeren
% bir \var{class} bildirimini taradığında gerçekleşir:
% \begin{verbatim}
% type
%   TMyObject = class(TObject, IDispatch)
%     function IUnknown.QueryInterface=MyQueryInterface;
%     ....
% \end{verbatim}
% ve noktadan önce \var{interface} ve kalıtım listesinde listelenmez.
parser_e_type_cant_be_used_in_array_index=03170_E_"$1" tipi dizi indeks tipi olarak kullanılamaz
% \var{qword} veya \var{int64} gibi türlere dizi dizini türü olarak izin verilmez.
parser_e_no_con_des_in_interfaces=03171_E_Interface lerde constructor ve destructor e izin verilmez
% Interface lerde constructor ve destructor bildirimlerine izin verilmez.
% Çoğu durumda, yeni bir arayüz oluşturmak için \var{IUnknown} öğesinin \var{QueryInterface} yöntemi kullanılabilir.
parser_e_no_access_specifier_in_interfaces=03172_E_Erişim belirticileri INTERFACE ve OBJCPROTOCOL'larda kullanılamaz.
% \var{public}, \var{private}, \var{protected} ve \var{published} erişim belirticileri,
% arabirimlerde, Objective-C protokollerinde ve kategorilerinde kullanılamaz.
% çünkü bir interface/protocol/category nin tüm yöntemleri public de olmalıdır. 
parser_e_no_vars_in_interfaces=03173_E_Bir interface, yardımcı veya Objective-C protokolü veya kategorisi alanlar içeremez
% Arabirimler, yardımcılar ve Objective-C protokollerinde ve kategorilerinde alan bildirimlerine izin verilmez.
% Bir interface/helper/protocol/category, yalnızca yöntem okuma/yazma belirticilerini içeren methods ve properties içerebilir.
parser_e_no_local_proc_external=03174_E_Cannot declare local procedure as EXTERNAL
% Declaring local procedures as external is not possible. Local procedures
% get hidden parameters that will make the chance of errors very high.
parser_w_skipped_fields_before=03175_W_Some fields coming before "$1" were not initialized
% In Delphi mode, not all fields of a typed constant record have to be
% initialized, but the compiler warns you when it detects such situations.
parser_e_skipped_fields_before=03176_E_Some fields coming before "$1" were not initialized
% In all syntax modes but Delphi mode, you cannot leave some fields uninitialized
% in the middle of a typed constant record.
parser_w_skipped_fields_after=03177_W_Some fields coming after "$1" were not initialized
% You can leave some fields at the end of a type constant record uninitialized
% (The compiler will initialize them to zero automatically). This may be the cause
% of subtle problems.
parser_e_varargs_need_cdecl_and_external=03178_E_VarArgs directive (or '...' in MacPas) without CDecl/CPPDecl/MWPascal and External
% The varargs directive (or the ``...'' varargs parameter in MacPas mode) can only be
% used with procedures or functions that are declared with \var{external} and one of
% \var{cdecl}, \var{cppdecl} and \var{mwpascal}. This functionality
% is only supported to provide a compatible interface to C functions like printf.
parser_e_self_call_by_value=03179_E_Self must be a normal (call-by-value) parameter
% You cannot declare \var{Self} as a const or var parameter, it must always be
% a call-by-value parameter.
parser_e_interface_has_no_guid=03180_E_Interface "$1" has no interface identification
% When you want to assign an interface to a constant, then the interface
% must have a GUID value set.
parser_e_illegal_field_or_method=03181_E_Unknown class field or method identifier "$1"
% Properties must refer to a field or method in the same class.
parser_w_proc_overriding_calling=03182_W_Overriding calling convention "$1" with "$2"
% There are two directives in the procedure declaration that specify a calling
% convention. Only the last directive will be used.
parser_e_no_procvarobj_const=03183_E_Typed constants of the type "procedure of object" can only be initialized with NIL
% You cannot assign the address of a method to a typed constant which has a
% 'procedure of object' type, because such a constant requires two addresses:
% that of the method (which is known at compile time) and that of the object or
% class instance it operates on (which cannot be known at compile time).
parser_e_default_value_only_one_para=03184_E_Default value can only be assigned to one parameter
% It is not possible to specify a default value for several parameters at once.
% The following is invalid:
% \begin{verbatim}
% Procedure MyProcedure (A,B : Integer = 0);
% \end{verbatim}
% Instead, this should be declared as
% \begin{verbatim}
% Procedure MyProcedure (A : Integer = 0; B : Integer = 0);
% \end{verbatim}
parser_e_default_value_expected_for_para=03185_E_Default parameter required for "$1"
% The specified parameter requires a default value.
parser_w_unsupported_feature=03186_W_Use of unsupported feature!
% You're trying to force the compiler into doing something it cannot do yet.
parser_h_c_arrays_are_references=03187_H_C arrays are passed by reference
% Any array passed to a C function is passed
% by a pointer (i.e. by reference).
parser_e_C_array_of_const_must_be_last=03188_E_C array of const must be the last argument
% You cannot add any other argument after an \var{array of const} for
% \var{cdecl} functions, as the size pushed on stack for this argument is
% not known.
parser_h_type_redef=03189_H_Type "$1" redefinition
% This is an indicator that a previously declared type is
% being redefined as something else. This may, or may not
% be, a potential source of errors.
parser_w_cdecl_has_no_high=03190_W_cdecl'ared functions have no high parameter
% Functions declared with the \var{cdecl} modifier do not pass an extra implicit parameter.
parser_w_cdecl_no_openstring=03191_W_cdecl'ared functions do not support open strings
% Openstring is not supported for functions that have the \var{cdecl} modifier.
parser_e_initialized_not_for_threadvar=03192_E_Cannot initialize variables declared as threadvar
% Variables declared as threadvar cannot be initialized with a default value.
% The variables will always be filled with zero at the start of a new thread.
parser_e_msg_only_for_classes=03193_E_Message directive is only allowed in Classes
% The message directive is only supported for Class types.
parser_e_procedure_or_function_expected=03194_E_Procedure or Function expected
% A class method can only be specified for procedures and functions.
parser_e_illegal_calling_convention=03195_W_Calling convention directive ignored: "$1"
% Some calling conventions are supported only by certain CPUs. I.e. most non-i386 ports support
% only the standard ABI calling convention of the CPU.
parser_e_no_object_reintroduce=03196_E_REINTRODUCE cannot be used in objects
% \var{reintroduce} is not supported for objects, Objective-C classes and Objective-C protocols.
parser_e_paraloc_only_one_para=03197_E_Each argument must have its own location
% If locations for arguments are specified explicitly as it is required by
% some syscall conventions, each argument must have its own location. Things
% like
% \begin{verbatim}
% procedure p(i,j : longint 'r1');
% \end{verbatim}
% are not allowed.
parser_e_paraloc_all_paras=03198_E_Each argument must have an explicit location
% If one argument has an explicit argument location, all arguments of a procedure
% must have one.
parser_e_illegal_explicit_paraloc=03199_E_Unknown argument location
% The location specified for an argument isn't recognized by the compiler.
parser_e_32bitint_or_pointer_variable_expected=03200_E_32 Bit-Integer or pointer variable expected
% The libbase for MorphOS/AmigaOS can be given only as \var{longint}, \var{dword} or any pointer variable.
parser_e_goto_outside_proc=03201_E_Goto statements are not allowed between different procedures
% It isn't allowed to use \var{goto} statements referencing labels outside the
% current procedure. The following example shows the problem:
% \begin{verbatim}
% ...
%   procedure p1;
%   label
%     l1;
%
%     procedure p2;
%     begin
%       goto l1; // This goto ISN'T allowed
%     end;
%
%   begin
%     p2
%   l1:
%   end;
% ...
%
% \end{verbatim}
parser_f_too_complex_proc=03202_F_Procedure too complex, it requires too many registers
% Your procedure body is too long for the compiler. You should split the
% procedure into multiple smaller procedures.
parser_e_illegal_expression=03203_E_Illegal expression
% This can occur under many circumstances. Usually when trying to evaluate
% constant expressions.
parser_e_invalid_integer=03204_E_Invalid integer expression
% You made an expression which isn't an integer, and the compiler expects the
% result to be an integer.
parser_e_invalid_qualifier=03205_E_Illegal qualifier
% One of the following is happening :
% \begin{itemize}
% \item You're trying to access a field of a variable that is not a record.
% \item You're indexing a variable that is not an array.
% \item You're dereferencing a variable that is not a pointer.
% \end{itemize}
parser_e_upper_lower_than_lower=03206_E_High range limit < low range limit
% You are declaring a subrange, and the high limit is less than the low limit of
% the range.
parser_e_macpas_exit_wrong_param=03207_E_Exit's parameter must be the name of the procedure it is used in or of a surrounding procedure
% The parameter of a exit call in macpas mode must be either the name of the current subroutine or of a surrounding one
parser_e_illegal_assignment_to_count_var=03208_E_Illegal assignment to for-loop variable "$1"
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings. You also cannot assign values to
% loop variables inside the loop (Except in Delphi and TP modes). Use a while or
% repeat loop instead if you need to do something like that, since those
% constructs were built for that.
parser_e_no_local_var_external=03209_E_Yerel değişken EXTERNAL olarak bildirilemiyor
% Declaring local variables as external is not allowed. Only global variables can reference
% external variables.
parser_e_proc_already_external=03210_E_Procedure is already declared EXTERNAL
% The procedure is already declared with the EXTERNAL directive in an interface or
% forward declaration.
parser_w_implicit_uses_of_variants_unit=03211_W_Implicit uses of Variants unit
% The Variant type is used in the unit without any used unit using the Variants unit. The
% compiler has implicitly added the Variants unit to the uses list. To remove this warning
% the Variants unit needs to be added to the uses statement.
parser_e_no_static_method_in_interfaces=03212_E_Class and static methods cannot be used in INTERFACES
% The specifier \var{class} and directive \var{static} cannot be used in interfaces
% because all methods of an interface must be public.
parser_e_arithmetic_operation_overflow=03213_E_Overflow in arithmetic operation
% An operation on two integer values produced an overflow.
parser_e_protected_or_private_expected=03214_E_Protected or private expected
% \var{strict} can be only used together with \var{protected} or \var{private}.
parser_e_illegal_slice=03215_E_SLICE cannot be used outside of parameter list
% \var{slice} can be used only for arguments accepting an open array parameter.
parser_e_dispinterface_cant_have_parent=03216_E_A DISPINTERFACE cannot have a parent class
% A DISPINTERFACE is a special type of interface which cannot have a parent class. Dispinterface always derive from IDispatch type.
parser_e_dispinterface_needs_a_guid=03217_E_A DISPINTERFACE needs a guid
% A DISPINTERFACE always needs an interface identification (a GUID).
parser_w_overridden_methods_not_same_ret=03218_W_Overridden methods must have a related return type. This code may crash, it depends on a Delphi parser bug ("$2" is overridden by "$1" which has another return type)
% If you declare overridden methods in a class definition, they must
% have the same return type. Some versions of Delphi allow you to change the
% return type of interface methods, and even to change procedures into
% functions, but the resulting code may crash depending on the types used
% and the way the methods are called.
parser_e_dispid_must_be_ord_const=03219_E_Dispatch IDs must be ordinal constants
% The \var{dispid} keyword must be followed by an ordinal constant (the dispid index).
parser_e_array_range_out_of_bounds=03220_E_The range of the array is too large
% Regardless of the size taken up by its elements, an array cannot have more
% than high(ptrint) elements. Additionally, the range type must be a subrange
% of ptrint.
parser_e_packed_element_no_var_addr=03221_E_The address cannot be taken of bit packed array elements and record fields
% If you declare an array or record as \var{packed} in Mac Pascal mode
% (or as \var{packed} in any mode with \var{\{\$bitpacking on\}}), it will
% be packed at the bit level. This means it becomes impossible to take addresses
% of individual array elements or record fields. The only exception to this rule
% is in the case of packed arrays elements whose packed size is a multple of 8 bits.
parser_e_packed_dynamic_open_array=03222_E_Dynamic arrays cannot be packed
% Only regular (and possibly in the future also open) arrays can be packed.
parser_e_packed_element_no_loop=03223_E_Bit packed array elements and record fields cannot be used as loop variables
% If you declare an array or record as \var{packed} in Mac Pascal mode
% (or as \var{packed} in any mode with \var{\{\$bitpacking on\}}), it will
% be packed at the bit level. For performance reasons, they cannot be
% used as loop variables.
parser_e_type_var_const_only_in_records_and_classes=03224_E_VAR, TYPE and CONST are allowed only in records, objects and classes
% The usage of VAR, TYPE and CONST to declare new types inside an object is allowed only inside
% records, objects and classes.
parser_e_cant_create_generics_of_this_type=03225_E_This type cannot be a generic
% Only Classes, Objects, Interfaces and Records are allowed to be used as generic.
parser_w_no_lineinfo_use_switch=03226_W_Don't load LINEINFO unit manually, Use the -gl compiler switch instead
% Do not use the \file{lineinfo} unit directly, Use the \var{-gl} switch which
% automatically adds the correct unit for reading the selected type of debugging
% information. The unit that needs to be used depends on the type of
% debug information used when compiling the binary.
parser_e_no_funcret_specified=03227_E_No function result type specified for function "$1"
% The first time you declare a function you have to declare it completely,
% including all parameters and the result type.
parser_e_special_onlygenerics=03228_E_Specialization is only supported for generic types
% Types which are not generics cannot be specialized.
parser_e_no_generics_as_params=03229_E_Generics cannot be used as parameters when specializing generics
% When specializing a generic, only non-generic types can be used as parameters.
parser_e_type_object_constants=03230_E_Constants of objects containing a VMT are not allowed
% If an object requires a VMT either because it contains a constructor or virtual methods,
% it's not allowed to create constants of it. In TP and Delphi mode this is allowed
% for compatibility reasons.
parser_e_label_outside_proc=03231_E_Taking the address of labels defined outside the current scope isn't allowed
% It isn't allowed to take the address of labels outside the
% current procedure.
parser_e_initialized_not_for_external=03233_E_Cannot initialize variables declared as external
% Variables declared as external cannot be initialized with a default value.
parser_e_illegal_function_result=03234_E_Illegal function result type
% Some types like file types cannot be used as function result.
parser_e_no_common_type=03235_E_No common type possible between "$1" and "$2"
% To perform an operation on integers, the compiler converts both operands
% to their common type, which appears to be an invalid type. To determine the
% common type of the operands, the compiler takes the minimum of the minimal values
% of both types, and the maximum of the maximal values of both types. The common
% type is then minimum..maximum.
parser_e_no_generics_as_types=03236_E_Generics without specialization cannot be used as a type for a variable
% Generics must be always specialized before being used as variable type.
parser_w_register_list_ignored=03237_W_Register list is ignored for pure assembler routines
% When using pure assembler routines, the list with modified registers is ignored.
parser_e_implements_must_be_class_or_interface=03238_E_Implements property must have class or interface type
% A property which implements an interface must be of type class or interface.
parser_e_implements_must_have_correct_type=03239_E_Implements-property must implement interface of correct type, found "$1" expected "$2"
% A property which implements an interface actually implements a different interface.
parser_e_implements_must_read_specifier=03240_E_Implements-property must have read specifier
% A property  which implements an interface must have at least a read specifier.
parser_e_implements_must_not_have_write_specifier=03241_E_Implements-property must not have write-specifier
% A property  which implements an interface may not have a write specifier.
parser_e_implements_must_not_have_stored_specifier=03242_E_Implements-property must not have stored-specifier
% A property  which implements an interface may not have a stored specifier.
parser_e_implements_uses_non_implemented_interface=03243_E_Implements-property used on unimplemented interface: "$1"
% The interface which is implemented by a property is not an interface implemented by the class.
parser_e_unsupported_real=03244_E_Floating point not supported for this target
% The compiler parsed a floating point expression, but it is not supported.
parser_e_class_doesnt_implement_interface=03245_E_Class "$1" does not implement interface "$2"
% The delegated interface is not implemented by the class given in the implements clause.
parser_e_class_implements_must_be_interface=03246_E_Type used by implements must be an interface
% The \var{implements} keyword must be followed by an interface type.
parser_e_cant_export_var_different_name=03247_E_Variables cannot be exported with a different name on this target, add the name to the declaration using the "export" directive (variable name: $1, declared export name: $2)
% On most targets it is not possible to change the name under which a variable
% is exported inside the \var{exports} statement of a library.
% In that case, you have to specify the export name at the point where the
% variable is declared, using the \var{export} and \var{alias} directives.
parser_e_weak_external_not_supported=03248_E_Weak external symbols are not supported for the current target
% A "weak external" symbol is a symbol which may or may not exist at (either static
% or dynamic) link time. This concept may not be available (or implemented yet)
% on the current cpu/OS target.
parser_e_forward_mismatch=03249_E_Forward type definition does not match
% Classes and interfaces being defined forward must have the same type
% when being implemented. A forward interface cannot be changed into a class.
parser_n_ignore_lower_visibility=03250_N_Virtual method "$1" has a lower visibility ($2) than parent class $3 ($4)
% The virtual method overrides an method that is declared with a higher visibility. This might give
% unexpected results. E.g., in case the new visibility is private then a call to ``inherited'' in a
% new child class will call the higher-visible method in a parent class and ignores the private method.
parser_e_field_not_allowed_here=03251_E_Fields cannot appear after a method or property definition, start a new visibility section first
% Once a method or property has been defined in a class or object, you cannot define any fields afterwards
% without starting a new visibility section (such as \var{public}, \var{private}, etc.). The reason is
% that otherwise the source code can appear ambiguous to the compiler, since it is possible to use modifiers
% such as \var{default} and \var{register} also as field names.
parser_e_no_local_para_def=03252_E_Parameters or result types cannot contain local type definitions. Use a separate type definition in a type block.
% In Pascal, types are not considered to be identical simply because they are semantically equivalent.
% Two variables or parameters are only considered to be of the same type if they refer to the
% same type definition.
% As a result, it is not allowed to define new types inside parameter lists, because then it is impossible to
% refer to the same type definition in the procedure headers of the interface and implementation of a unit
% (both procedure headers would define a separate type). Keep in mind that expressions such as
% ``file of byte'' or ``string[50]'' also define a new type.
parser_e_abstract_and_sealed_conflict=03253_E_ABSTRACT and SEALED conflict
% ABSTRACT and SEALED cannot be used together in one declaration
parser_e_sealed_descendant=03254_E_Cannot create a descendant of the sealed class "$1"
% Sealed means that class cannot be derived by another class.
parser_e_sealed_class_cannot_have_abstract_methods=03255_E_SEALED class cannot have an ABSTRACT method
% Sealed means that class cannot be derived. Therefore no one class is able to override an abstract method in a sealed class.
parser_e_only_virtual_methods_final=03256_E_Only virtual methods can be final
% You are declaring a method as final, when it is not declared to be
% virtual.
parser_e_final_can_no_be_overridden=03257_E_Final method cannot be overridden: "$1"
% You are trying to \var{override} a virtual method of a parent class that does
% not exist.
parser_e_multiple_messages=03258_E_Only one message can be used per method.
% It is not possible to associate multiple messages with a single method.
parser_e_invalid_enumerator_identifier=03259_E_Invalid enumerator identifier: "$1"
% Only "MoveNext" and "Current" enumerator identifiers are supported.
parser_e_enumerator_identifier_required=03260_E_Enumerator identifier required
% "MoveNext" or "Current" identifier must follow the \var{enumerator} modifier.
parser_e_enumerator_movenext_is_not_valid=03261_E_Enumerator MoveNext pattern method is not valid. Method must be a function with the Boolean return type and no required arguments.
% "MoveNext" enumerator pattern method must be a function with Boolean return type and no required arguments
parser_e_enumerator_current_is_not_valid=03262_E_Enumerator Current pattern property is not valid. Property must have a getter.
% "Current" enumerator pattern property must have a getter
parser_e_only_one_enumerator_movenext=03263_E_Only one enumerator MoveNext method is allowed per class/object
% Class or Object can have only one enumerator MoveNext declaration.
parser_e_only_one_enumerator_current=03264_E_Only one enumerator Current property is allowed per class/object
% Class or Object can have only one enumerator Current declaration.
parser_e_for_in_loop_cannot_be_used_for_the_type=03265_E_For in loop cannot be used for the type "$1"
% For in loop can be used not for all types. For example it cannot be used for the enumerations with jumps.
parser_e_objc_requires_msgstr=03266_E_Objective-C messages require their Objective-C selector name to be specified using the "message" directive.
% Objective-C messages require their Objective-C name (selector name) to be specified using the \var{message `someName:'} procedure directive.
% While bindings to other languages automatically generate such names based on the identifier you use (by replacing
% all underscores with colons), this is unsafe since nothing prevents an Objective-C method name to contain actual
% colons.
parser_e_objc_no_constructor_destructor=03267_E_Objective-C does not have formal constructors nor destructors. Use the alloc, initXXX and dealloc messages.
% The Objective-C language does not have any constructors or destructors. While there are some messages with a similar
% purpose (such as \var{init} and \var{dealloc}), these cannot be identified using automatic parsers and do not
% guarantee anything like Pascal constructors/destructors (e.g., you have to take care of only calling ``designated''
% inherited ``constructors''). For these reasons, we have opted to follow the standard Objective-C patterns for
% instance creation/destruction.
parser_e_message_string_too_long=03268_E_Message name is too long (max. 255 characters)
% Due to compiler implementation reasons, message names are currently limited to 255 characters.
parser_e_objc_message_name_too_long=03269_E_Objective-C message symbol name for "$1" is too long
% Due to compiler implementation reasons, mangled message names (i.e., the symbol names used in the assembler
% code) are currently limited to 255 characters.
parser_h_no_objc_parent=03270_H_Defining a new Objective-C root class. To derive from another root class (e.g., NSObject), specify it as the parent class.
% If no parent class is specified for an Object Pascal class, then it automatically derives from TObject.
% Objective-C classes however do not automatically derive from NSObject, because one can have multiple
% root classes in Objective-C. For example, in the Cocoa framework both NSObject and NSProxy are root classes.
% Therefore, you have to explicitly define a parent class (such as NSObject) if you want to derive your
% Objective-C class from it.
parser_e_no_objc_published=03271_E_Objective-C classes cannot have published sections.
% In Object Pascal, ``published'' determines whether or not RTTI is generated. Since the Objective-C runtime always needs
% RTTI for everything, this specified does not make sense for Objective-C classes.
parser_f_need_objc=03272_F_This module requires an Objective-C mode switch to be compiled
% This error indicates the use of Objective-C language features without an Objective-C mode switch
% active. Enable one via the -M command line switch, or the {\$modeswitch x} directive.
parser_e_must_use_override=03273_E_Inherited methods can only be overridden in Objective-C and Java, add "override" (inherited method defined in $1)
parser_h_should_use_override=03274_H_Inherited methods can only be overridden in Objective-C and Java, add "override" (inherited method defined in $1).
% It is not possible to \var{reintroduce} methods in Objective-C or Java like in Object Pascal. Methods with the same
% name always map to the same virtual method entry. In order to make this clear in the source code,
% the compiler always requires the \var{override} directive to be specified when implementing overriding
% Objective-C or Java methods in Pascal. If the implementation is external, this rule is relaxed because Objective-C and Java
% do not have any \var{override}-style keyword (since it's the default and only behaviour in these languages),
% which makes it hard for automated header conversion tools to include it everywhere.
% The type in which the inherited method is defined is explicitly mentioned, because this may either
% be an objcclass or an objccategory in case of Objective-C.
parser_e_objc_message_name_changed=03275_E_Message name "$1" in inherited class is different from message name "$2" in current class.
% An overriding Objective-C method cannot have a different message name than an inherited method. The reason
% is that these message names uniquely define the message to the Objective-C runtime, which means that
% giving them a different message name breaks the ``override'' semantics.
parser_e_unique_unsupported=03276_E_It is not yet possible to make unique copies of Objective-C or Java types
% Duplicating an Objective-C or Java type using \var{type x = type y;} is not yet supported. You may be able to
% obtain the desired effect using \var{type x = objcclass(y) end;} resp.{} \var{type x = class(y) end;} instead.
parser_e_no_category_as_types=03277_E_Objective-C categories and Object Pascal class helpers cannot be used as types
% It is not possible to declare a variable as an instance of an Objective-C
% category or an Object Pascal class helper. A category/class helper adds
% methods to the scope of an existing class, but does not define a type by
% itself. An exception of this rule is when inheriting an Object Pascal class
% helper from another class helper.
parser_e_no_category_override=03278_E_Categories do not override, but replace methods. Use "reintroduce" instead.
parser_e_must_use_reintroduce_objc=03279_E_Replaced methods can only be reintroduced in Objective-C, add "reintroduce" (replaced method defined in $1).
parser_h_should_use_reintroduce_objc=03280_H_Replaced methods can only be reintroduced in Objective-C, add "reintroduce" (replaced method defined in $1).
% A category replaces an existing method in an Objective-C class, rather than that it overrides it.
% Calling an inherited method from an category method will call that method in
% the extended class' parent, not in the extended class itself. The
% replaced method in the original class is basically lost, and can no longer be
% called or referred to. This behaviour corresponds somewhat more closely to
% \var{reintroduce} than to \var{override} (although in case of \var{reintroduce}
% in Object Pascal, hidden methods are still reachable via inherited).
% The type in which the inherited method is defined is explicitly mentioned, because this may either
% be an objcclass or an objccategory.
parser_e_implements_getter_not_default_cc=03281_E_Getter for implements interface must use the target's default calling convention.
% Interface getters are called via a helper in the run time library, and hence
% have to use the default calling convention for the target (\var{register} on
% i386 and x86\_64, \var{stdcall} on other architectures).
parser_e_no_refcounted_typed_file=03282_E_Typed files cannot contain reference-counted types.
% The data in a typed file cannot be of a reference counted type (such as
% \var{ansistring} or a record containing a field that is reference counted).
parser_e_operator_not_overloaded_2=03283_E_Operator is not overloaded: $2 "$1"
% You are trying to use an overloaded operator when it is not overloaded for
% this type.
parser_e_operator_not_overloaded_3=03284_E_Operator is not overloaded: "$1" $2 "$3"
% You are trying to use an overloaded operator when it is not overloaded for
% this type.
parser_e_more_array_elements_expected=03285_E_Expected another $1 array elements
% When declaring a typed constant array, you provided to few elements to initialize the array
parser_e_string_const_too_long=03286_E_Cevaplar devre dışı bırakılmışken String sabiti çok uzun
% Only when a piece of code is compiled with ansistrings enabled (\var{\{\$H+\}}), string constants
% longer than 255 characters are allowed.
parser_e_invalid_univ_para=03287_E_Derleme zamanında boyutu bilinmiyor olduğundan, tür univ parametresi olarak kullanılamaz: "$1"
% \var{univ} parameters are compatible with all values of the same size, but this
% cannot be checked in case a parameter's size is unknown at compile time.
parser_e_only_one_class_constructor_allowed=03288_E_Class da sadece bir sınıf constructor deklare edilebilir: "$1"
% You are trying to declare more than one class constructor but only one class constructor can be declared.
parser_e_only_one_class_destructor_allowed=03289_E_Class da sadece bir sınıf destructor deklare edilebilir: "$1"
% You are trying to declare more than one class destructor but only one class destructor can be declared.
parser_e_no_paras_for_class_constructor=03290_E_Class constructors parametreleri olamaz
% You are declaring a class constructor with a parameter list. Class constructor methods
% cannot have parameters.
parser_e_no_paras_for_class_destructor=03291_E_Class destructors parametreleri olamaz
% You are declaring a class destructor with a parameter list. Class destructor methods
% cannot have parameters.
parser_f_modeswitch_objc_required=03292_F_Bu yapı, \{\$modeswitch objectivec1\} mode anahtarının etkin olmasını gerektirir
% Objective-Pascal constructs are not supported when \{\$modeswitch ObjectiveC1\}
% is not active.
parser_e_widestring_to_ansi_compile_time=03293_E_Unicodechar/string sabitleri derleme zamanında ansi/shortstring'e dönüştürülemez
% It is not possible to use unicodechar and unicodestring constants in
% constant expressions that have to be converted into an ansistring or shortstring
% at compile time, for example inside typed constants. The reason is that the
% compiler cannot know what the actual ansi encoding will be at run time.
parser_e_objc_enumerator_2_0=03294_E_For-in Objective-Pascal döngüleri, \{\$modeswitch ObjectiveC2\} etkin olmasını gerektirir
% Objective-C ``fast enumeration'' support was added in Objective-C 2.0, and
% hence the appropriate modeswitch has to be activated to expose this feature.
% Note that Objective-C 2.0 programs require Mac OS X 10.5 or later.
parser_e_objc_missing_enumeration_defs=03295_E_Derleyici, CocoaAll ünitesinde NSFastEnumerationProtocol veya NSFastEnumerationState türünü bulamıyor
% Objective-C for-in loops (fast enumeration) require that the compiler can
% find a unit called CocoaAll that contains definitions for the
% NSFastEnumerationProtocol and NSFastEnumerationState types. If you get this
% error, most likely the compiler is finding and loading an alternate CocoaAll
% unit.
parser_e_no_procvarnested_const=03296_E_Type sabitleri "İç içe geçmiş prosedür" yalnızca NIL ve genel prosedür/fonksiyonlarla başlatılabilir.
% A nested procedural variable consists of two components: the address of the
% procedure/function to call (which is always known at compile time), and also
% a parent frame pointer (which is never known at compile time) in case the
% procedural variable contains a reference to a nested procedure/function.
% Therefore such typed constants can only be initialized with global
% functions/procedures since these do not require a parent frame pointer.
parser_f_no_generic_inside_generic=03297_F_Başka bir genel sınıf içindeki jenerik sınıfın ilanına izin verilmez
% At the moment, scanner supports recording of only one token buffer at the time
% (guarded by internal error 200511173 in tscannerfile.startrecordtokens).
% Since generics are implemented by recording tokens, it is not possible to
% have declaration of generic class inside another generic class.
parser_e_forward_intf_declaration_must_be_resolved=03298_E_Bir sınıfa uymadan veya uygulayabilmeden önce, "$1" tutarındaki iletme bildirimi çözülmeli
% An Objective-C protocol or Java Interface must be fully defined before classes can conform to it.
% This error occurs in the following situation (example for Objective-C, but the same goes for Java interfaces):
% \begin{verbatim}
%  Type MyProtocol = objcprotoocl;
%       ChildClass = Class(NSObject,MyProtocol)
%         ...
%       end;
% \end{verbatim}
% where \var{MyProtocol} is declared but not defined.
parser_e_no_record_published=03299_E_Kayıt türleri bölüm yayınlamış olamaz
% Published sections can be used only inside classes.
parser_e_no_destructor_in_records=03300_E_Destructors kayıtlara veya yardımcılara izin verilmez
% Destructor declarations are not allowed in records or helpers.
parser_e_class_methods_only_static_in_records=03301_E_Sınıf yöntemleri kayıtlarda statik olmalı
% Class methods declarations are not allowed in records without static modifier.
% Records have no inheritance and therefore non static class methods have no sence for them.
parser_e_no_parameterless_constructor_in_records=03302_E_Parametresiz kuruculara kayıtlara veya kayıt / tür yardımcılarına izin verilmez
% Constructor declarations with no arguments are not allowed in records or record/type helpers.
parser_e_at_least_one_argument_must_be_of_type=03303_E_Sonuç veya en az bir parametre "$1" türünde olmalıdır
% It is required that either the result of the routine or at least one of its parameters be of the specified type.
% For example class operators either take an instance of the structured type in which they are defined, or they return one.
parser_e_cant_use_type_parameters_here=03304_E_Tip parametreleri initialization/finalization gerektirebilir - değişken kayıtlarında kullanılamaz
% Type parameters may be specialized with types which (e.g. \var{ansistring}) need initialization/finalization
% code which is implicitly generated by the compiler.
parser_e_externals_no_section=03305_E_Dış olarak bildirilen değişkenler özel bir bölümde olamaz
% A section directive is not valid for variables being declared as external.
parser_e_section_no_locals=03306_E_Statik olmayan ve global olmayan değişkenlerin bölüm yönergesi olamaz
% A variable placed in a custom section is always statically allocated so it must be either a static or global variable.
parser_e_not_allowed_in_helper=03307_E_"$1" yardımcı türlerinde kullanılamaz
% Some directives and specifiers like "virtual", "dynamic", "override" are not
% allowed inside helper types in mode ObjFPC (they are ignored in mode Delphi),
% because they have no meaning within helpers. Also "abstract" isn't allowed in
% either mode.
parser_e_no_class_constructor_in_helpers=03308_E_Class constructors yardımlarına izin verilmez
% Class constructor declarations are not allowed in helpers.
parser_e_inherited_not_in_record=03309_E_Bir kayıtta "inherited" kullanımına izin verilmiyor
% As records don't suppport inheritance the use of "inherited" is prohibited for
% these as well as for record helpers (in mode "Delphi" only).
parser_e_no_types_in_local_anonymous_records=03310_E_Yerel veya isimsiz kayıtlarda tür bildirimlerine izin verilmez
% Records with types must be defined globally. Types cannot be defined inside records which are defined in a
% procedure or function or in anonymous records.
parser_e_duplicate_implements_clause=03311_E_Yinelenen uygular interface için madde "$1"
% A class may delegate an interface using the "implements" clause only to a single property. Delegating it multiple times
% is a error.
parser_e_mapping_no_implements=03312_E_"$1" Interface "$2" tarafından gösterilemiyor, zaten yöntem çözümlemesi var
% Method resolution clause maps a method of an interface to a method of the current class. Therefore the current class
% has to implement the interface directly. Delegation is not possible.
parser_e_implements_no_mapping=03313_E_"$1" Interface "$2" yöntem çözünürlüklerine sahip olamaz
% Method resoulution is only possible for interfaces that are implemented directly, not by delegation.
parser_e_invalid_codepage=03314_E_Geçersiz kod sayfası
% When declaring a string with a given codepage, the range of valid codepages values is limited
% to 0 to 65535.
parser_e_final_only_const_var=03315_E_Nesne türlerinde yalnızca alanlar (var-bölümler) ve sabitler son olabilir
% A final (class) field must be assigned a single value in the (class) constructor, and cannot
% be overwritten afterwards. A final (typed) constant is read-only.
parser_e_final_only_external=03316_E_Nihai alanlar şu anda yalnızca harici sınıflar için desteklenmektedir
% Support for final fields in non-external classes requires a full data flow
% analysis implementation in FPC, which it currently still lacks.
parser_e_no_typed_const=03317_E_Burada yazılan sabitlere izin verilmez, yalnızca resmi sabitlere izin verilir
% Java interfaces define a namespace in which formal constant can be defined,
% but since they define no storage it is not possible to define typed constants
% in them (those are more or less the same as initialised class fields).
parser_e_java_no_inherited_constructor=03318_E_Yapıcılar JVM'de otomatik olarak miras alınmaz; Açıkça ihtiyacınız varsa, miras kalanı çağıran bir yapıcı ekleyin.
% Java does not automatically add inherited constructors to child classes, so that they can be hidden.
% For compatibility with external Java code, FPC does the same. If you require access to the same
% constructors in a child class, define them in the child class and call the inherited one from
% there.
parser_d_internal_parser_string=03319_D_Dahili olarak oluşturulan kodu ayrıştırma: $1
% The compiler sometimes internally constructs Pascal code that is subsequently
% injected into the program. These messages display such code, in order to help
% with debugging errors in them.
parser_e_feature_unsupported_for_vm=03320_E_Bu dil özelliği, yönetilen VM hedeflerinde desteklenmiyor
% Certain language features are not supported on targets that are managed virtual machines.
parser_e_jvm_invalid_virtual_constructor_call=03321_E_JVM hedefinde, geçerli örnek için başka bir kurucu içindeki sanal kurucu çağırmak mümkün değildir
% The JVM does not natively support virtual constructor. Unforunately, we are not aware of a way to
% emulate them in a way that makes it possible to support calling virtual constructors
% for the current instance inside another constructor.
parser_e_method_lower_visibility=03322_E_Overriding yöntemi "$1", üst sınıf $3 ($4) sınıfından daha düşük bir görünürlük seviyesine ($2) sahip olamaz
% The JVM does not allow lowering the visibility of an overriding method.
parser_e_nostackframe_without_assembler=03323_E_Prosedür/fonksiyon NOSTACKFRAME çağrı seçeneğiyle ancak ASSEMBLER olmadan bildirildi
% nostackframe call modifier is supposed to be used in conjunction with assembler.
parser_e_nostackframe_with_locals=03324_E_Prosedür/Fonksiyon  NOSTACKFRAME çağrısı seçeneğiyle ilan edildi ancak yerel yığın boyutu $1
% nostackframe call modifier used without assembler modifier
% might still generate local stack needs.
parser_e_cannot_generate_property_getter_setter=03325_E_Özellik getter/setter $1 oluşturulamıyor. Çünkü adı mevcut $2 tanımlayıcısıyla çakışıyor.
% Automatically generated getters/setters cannot have the same name as existing
% identifiers, because this may change the behaviour of existing code.
parser_w_overriding_property_getter_setter=03326_W_Otomatik olarak oluşturulan özellik getter/setter $1, $2 sınıfındaki aynı getter/setter'ı geçersiz kılar
% Automatically generated property getters/setters on the JVM platform are virtual methods, because
% the JVM does not support non-virtual methods that can be changed in child classes. This means
% that if a child class changes an inherited property definition, the behaviour of that property
% can change compared to native targets since even if a variable is declared as the parent type,
% by calling the virtual method the getter from the child will be used. This is different from
% the behaviour on native targets or when not activating automatically generated setters/getters,
% because in that case only the declared type of a variable influences the property behaviour.
parser_w_case_difference_auto_property_getter_setter_prefix=03327_W_Beyan edilen property getter/setter $1 ile otomatik olarak oluşturulan isim olan $2 arasında büyük/küçük harf uyumsuzluğu
% If a property's specified getter/setter already corresponded to the naming convention specified
% by the automatic getter/setter generation setting except in terms of upper/lowercase, the
% compiler will print a warning because it cannot necessarily change that other declaration itself
% not can it add one using the correct case (it could conflict with the original declaration).
% Manually correct the case of the getter/setter to conform to the desired coding rules.
% \var{TChild} overrides
parser_e_no_consts_in_local_anonymous_records=03328_E_Yerel veya anonim kayıtlarda Sabit beyanlarına izin verilmez
% Records with constants must be defined globally. Constants cannot be defined inside records which are defined in a
% procedure or function or in anonymous records.
parser_e_no_methods_in_local_anonymous_records=03329_E_Yerel veya anonim kayıtlarda Method beyanlarına izin verilmez
% Records with methods must be defined globally. Methods cannot be defined inside records which are defined in a
% procedure or function or in anonymous records.
parser_e_no_properties_in_local_anonymous_records=03330_E_Yerel veya anonim kayıtlarda Property beyanlara izin verilmez
% Records with properties must be defined globally. Properties cannot be defined inside records which are defined in a
% procedure or function or in anonymous records.
parser_e_no_class_in_local_anonymous_records=03331_E_Class üyesi beyanlarına yerel veya isimsiz kayıtlarda izin verilmez.
% Records with class members must be defined globally. Class members cannot be defined inside records which are defined in a
% procedure or function or in anonymous records.
parser_e_not_allowed_in_record=03332_E_"$1" görünürlük bölümüne kayıtlarda izin verilmiyor
% The visibility sections \var(protected) and \var(strict protected) are only
% useful together with inheritance. Since records do not support that they are
% forbidden.
parser_e_dir_not_allowed=03333_E_"$1" direktifine burada izin verilmiyor
% This directive is not allowed in the given context. E.g. "static"
% is not allowed for instance methods or class operators.
parser_e_no_assembler_in_generic=03334_E_Assembler blocks not allowed inside generics
% The use of assembler blocks/routines is not allowed inside generics.
parser_e_property_only_sgr=03335_E_Özellikler sadece statik, global veya yapısal yapılar içinde olabilir.
% Properties cannot be declared local, only global, using the static
% directive or inside structured types.
parser_e_overloaded_have_same_mangled_name=03336_E_Aşırı yüklenmiş rutinler aynı karışık isme sahip
% Some platforms, such as the JVM platform, encode the parameters in the routine name in
% a prescribed way, and this encoding may map different Pascal types to the same encoded
% (a.k.a.\ ``mangled'') name. This error can only be solved by removing or changing the
% conflicting definitions' parameter declarations or routine names.
parser_e_default_value_val_const=03337_E_Varsayılan değerler sadece değer, const ve constref parametreleri için belirtilebilir.
% A default parameter value allows you to not specify a value for this parameter
% when calling the routine, and the compiler will instead pass the specified
% default (constant) value. As a result, default values can only be specified
% for parameters that can accept constant values.
parser_w_ptr_type_ignored=03338_W_İşaretçi türü "$1" yoksayıldı
% The specified pointer type modifier is ignored, because it is not supported on
% the current platform. This happens, for example, when a far pointer is
% declared on a non-x86 platform.
%
%
%
% \end{description}
%
# Type Checking
#
# 04122 is the last used one
#
% \section{Type checking errors}
% This section lists all errors that can occur when type checking is
% performed.
% \begin{description}
type_e_mismatch=04000_E_Tür uyumsuzluğu
% This can happen in many cases:
% \begin{itemize}
% \item The variable you're assigning to is of a different type than the
% expression in the assignment.
% \item You are calling a function or procedure with parameters that are
% incompatible with the parameters in the function or procedure definition.
% \end{itemize}
type_e_incompatible_types=04001_E_Uyumsuz tipler: varolan "$1" beklenen "$2"
% There is no conversion possible between the two types.
% Another possiblity is that they are declared in different
% declarations:
% \begin{verbatim}
% Var
%    A1 : Array[1..10] Of Integer;
%    A2 : Array[1..10] Of Integer;
%
% Begin
%    A1:=A2; { This statement also gives this error. It
%              is due to the strict type checking of Pascal }
% End.
% \end{verbatim}
type_e_not_equal_types=04002_E_"$1" ile "$2" arasında Tip uyumsuzluğu
% Türler eşit değil.
type_e_type_id_expected=04003_E_Tip tanımlayıcı bekleniyor
% Tanımlayıcı bir tür değil veya bir tanımlayıcı belirtmeyi unuttunuz.
type_e_variable_id_expected=04004_E_Değişken tanımlayıcı bekleniyor
% Bu bir değişkeni beklerken bir rutine bir sabit ilettiğinizde
% (\var{Inc} var veya \var{Dec} gibi) gerçekleşir. Değişkenleri yalnızca
% bu fonksiyonlara argüman olarak iletebilirsiniz.
type_e_integer_expr_expected=04005_E_Integer ifadesi bekleniyor, ancak "$1" aldı
% Derleyici, integer türünde bir ifade bekliyor ancak farklı bir tür alıyor.
type_e_boolean_expr_expected=04006_E_Boolean ifadesi bekleniyor, ancak "$1" aldı
% İfade bir boolean türü olmalıdır. \var{True} veya \var{False} döndürülmelidir.
type_e_ordinal_expr_expected=04007_E_Sıralı ifade bekleniyor
% İfade, sıra türünde, yani maksimum a \var{Longint} olmalıdır.
% Bu, örneğin, sıralı bir değere sahip olmayan \var{Inc} veya \var{Dec} için
% ikinci bir argüman belirttiğinizde gerçekleşir.
type_e_pointer_type_expected=04008_E_pointer tipi bekleniyor, ancak "$1" aldı
% Değişken veya ifade, \var{pointer} türünde değil. Bu, \var{New} veya \var{Dispose}
% işaretçisi olmayan bir değişkeni ilettiğinizde gerçekleşir.
type_e_class_type_expected=04009_E_class tipi bekleniyor, ancak "$1" aldı
% Değişken veya ifade, \var{class} türünde değil. Bu genellikle ne zaman olur
% \begin{enumerate}
% \item Bir sınıf bildirgesindeki ana sınıf bir sınıf değildir.
% \item Bir istisna işleyici (\var{On}), sınıf olmayan bir tür tanımlayıcı içeriyor. 
% \end{enumerate}
type_e_cant_eval_constant_expr=04011_E_Sürekli ifade değerlendirilemiyor
% Bu hata, bildirdiğiniz bir dizinin sınırları sıra sabitlerini değerlendirmediğinde ortaya çıkabilir.
type_e_set_element_are_not_comp=04012_E_Ayarlanan elemanlar uyumlu değil
% Ayarlanan eleman tipleri aynı olmadığında iki takım üzerinde bir işlem
% gerçekleştirmeye çalışıyorsunuz. Birliğin temel türü sendika alırken aynı olmalıdır.
type_e_set_operation_unknown=04013_E_İşlem kümeler için uygulanmadı
% Kümeler için birkaç ikili işlem tanımlanmamıştır.
% Bunlar şunları içerir: \var{div}, \var{mod}, \var{**}, \var{> =} ve \var{<=}.
% Sondaki iki küme gelecekte tanımlanabilir.
type_w_convert_real_2_comp=04014_W_Bir tam sayı türü olan kayan türden COMP'a otomatik tür dönüştürme
% Gerçek bir türden \var{comp} biçimine örtük tür dönüşümü ile karşılaşıldı.
% \var{comp} 64 bit bir tamsayı tipi olduğundan, bu bir hatayı gösterebilir.
type_h_use_div_for_int=04015_H_tamsayı bir sonuç elde etmek için DIV kullanın
% İpuçları açıkken, o zaman '/' operatörüyle bir tamsayı bölümü
% bu mesajı üretecektir, çünkü sonuç Real tipte olacaktır.
type_e_strict_var_string_violation=04016_E_String türlerinin $V+ modunda tam olarak eşleşmesi gerekir
% \var{\{\$V+\}} modunda derlerken, parametre olarak ilettiğiniz
% dizinin işlemin bildirilen parametresiyle aynı tipte olması gerekir.
type_e_succ_and_pred_enums_with_assign_not_possible=04017_E_mümkün olmayan atamalar ile başarılı değil
% Aşağıdakiler gibi, içinde C benzeri atamaları olan bir numaralandırma türünü bildirirseniz:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% o zaman bu numaralandırma ile \var{Succ} veya \var{Pred} fonksiyonlarını kullanamazsınız.
type_e_cant_read_write_type=04018_E_Bu tür değişkenleri okuyamıyor veya yazamıyor
% \var{read} veya \var{write} değişkenine, bu değişkenin türünü desteklemeyen
% bir metin türü dosyasından veya bu dosyaya gitmeye çalışıyorsunuz.
% Bir metin dosyasından sadece tamsayı türleri, gerçekler, bilgisayar
% kartları ve dizgiler okunabilir veya yazılabilir.
% Boolean'ler yalnızca metin dosyalarına yazılabilir.
type_e_no_readln_writeln_for_typed_file=04019_E_Yazılan dosya üzerinde readln veya writeln kullanılamaz
% \var{readln} ve \var{writeln} sadece metin dosyaları için izin verilir.
type_e_no_read_write_for_untyped_file=04020_E_Yazılmamış dosya üzerinde okuma veya yazma kullanılamaz.
% \var{read} ve \var{write} yalnızca metin veya yazılan dosyalar için izin verilir.
type_e_typeconflict_in_set=04021_E_Ayarlanan elemanlar arasındaki Tip çakışması
% Yanlış tipte, yani set tipinde olmayan en az bir set elemanı vardır.
type_w_maybe_wrong_hi_lo=04022_W_lo/hi(dword/qword) upper/lower word/dword değerini döndürür
% \fpc supports an overloaded version of \var{lo/hi} for \var{longint/dword/int64/qword}
% which returns the lower/upper word/dword of the argument. \tp always uses
% a 16 bit \var{lo/hi} which always returns bits 0..7 for \var{lo} and the
% bits 8..15 for \var{hi}. If you want the \tp behavior you have
% to type cast the argument to a \var{word} or \var{integer}.
type_e_integer_or_real_expr_expected=04023_E_Integer veya real ifade bekleniyor
% \var{str} 'a olan ilk argüman gerçek veya tamsayı tipinde olmalıdır.
type_e_wrong_type_in_array_constructor=04024_E_Array constructor da yanlış tip "$1"
% Bir dizi constructor da, izin verilmeyen bir tür kullanmaya çalışıyorsunuz.
type_e_wrong_parameter_type=04025_E_Arg no için uyumsuz tür. $1: "$2" aldı, "$3" bekleniyor
% Belirtilen parametre için geçersiz bir tür iletmeye çalışıyorsunuz.
type_e_no_method_and_procedure_not_compatible=04026_E_Method (değişkeni) ve Prosedür (değişkeni) uyumlu değil
% Bir prosedür değişkenine bir Method atayamazsınız veya bir Methoda bir prosedür atayamazsınız.
type_e_wrong_math_argument=04027_E_Geçersiz sabit iç matematik fonksiyonuna geçti
% Bir \var{ln} veya \var{sqrt} işlevine iletilen sabit argüman bu işlevlerin tanım aralığı dışında.
type_e_no_addr_of_constant=04028_E_Sabit ifadelerin adresini alamıyor
% Sabit bir ifadenin adresini almak mümkün değildir, çünkü bellekte saklanmazlar.
% Yazılan bir sabit yapmayı deneyebilirsiniz. Bir özellik var parametresine
% bir özellik iletmeye çalışırsanız, bu hata da görüntülenebilir.
type_e_argument_cant_be_assigned=04029_E_Argüman atanamaz
% Only expressions which can be on the left side of an
% assignment can be passed as call by reference arguments.
%
% Remark: Properties can be used on the left side of an assignment,
% nevertheless they cannot be used as arguments.
type_e_cannot_local_proc_to_procvar=04030_E_Prosedür değişkenine yerel prosedür/fonksiyon atanamıyor
% It's not allowed to assign a local procedure/function to a
% procedure variable, because the calling convention of a local procedure/function is
% different. You can only assign local procedure/function to a void pointer.
type_e_no_assign_to_addr=04031_E_Bir adrese değer atayamıyor
% It is not allowed to assign a value to an address of a variable, constant,
% procedure or function. You can try compiling with -So if the identifier
% is a procedure variable.
type_e_no_assign_to_const=04032_E_Const değişkenine değer atayamazsınız
% It's not allowed to assign a value to a variable which is declared
% as a const. This is normally a parameter declared as const. To allow
% changing the value, pass the parameter by value, or a parameter by reference
% (using var).
type_e_array_required=04033_E_Array türü gerekli
% If you are accessing a variable using an index '[<x>]' then
% the type must be an array. In FPC mode a pointer is also allowed.
type_e_interface_type_expected=04034_E_interface tür bekleniyor, ancak "$1" var
% The compiler expected to encounter an interface type name, but got something else.
% The following code would produce this error:
% \begin{verbatim}
% Type
%   TMyStream = Class(TStream,Integer)
% \end{verbatim}
type_h_mixed_signed_unsigned=04035_H_İmzalı ifadeleri ve şifreleri karıştırmak 64 bit sonuç verir
% If you divide (or calculate the modulus of) a signed expression by a longword (or vice versa),
% or if you have overflow and/or range checking turned on and use an arithmetic
% expression (+, -, *, div, mod) in which both signed numbers and longwords appear,
% then everything has to be evaluated in 64-bit arithmetic which is slower than normal
% 32-bit arithmetic. You can avoid this by typecasting one operand so it
% matches the result type of the other one.
type_w_mixed_signed_unsigned2=04036_W_İşaretli ifadelerin ve cardinals ler buraya karıştırılması, aralık kontrol hatasına neden olabilir 
% If you use a binary operator (and, or, xor) and one of
% the operands is a longword while the other one is a signed expression, then,
% if range checking is turned on, you may get a range check error because in
% such a case both operands are converted to longword before the operation is
% carried out. You can avoid this by typecasting one operand so it
% matches the result type of the other one.
type_e_typecast_wrong_size_for_assignment=04037_E_Typecast'in atamasında farklı boyutta ($1 -> $2) var
% Type casting to a type with a different size is not allowed when the variable is
% used in an assignment.
type_e_array_index_enums_with_assign_not_possible=04038_E_atamaları olan enums'lar array indeksi olarak kullanılamaz.
% When you declared an enumeration type which has C-like
% assignments, such as in the following:
% \begin{verbatim}
%   Tenum = (a,b,e:=5);
% \end{verbatim}
% you cannot use it as the index of an array.
type_e_classes_not_related=04039_E_Class veya Object tipleri "$1" ve "$2" ilişkili değil
% There is a typecast from one class or object to another while the class/object
% are not related. This will probably lead to errors.
type_w_classes_not_related=04040_W_Class tipi "$1" and "$2" ilişkili değil
% There is a typecast from one class to another while the classes
% are not related. This will probably lead to errors.
type_e_class_or_interface_type_expected=04041_E_Class veya interface tipi bekleniyor, ama "$1" aldı
% The compiler expected a class or interface name, but got another type or identifier.
type_e_type_is_not_completly_defined=04042_E_Tip "$1" tam olarak tanımlanmadı
% This error occurs when a type is not complete: i.e. a pointer type which points to
% an undefined type.
type_w_string_too_long=04043_W_string literal, kısa string uzunluktan daha fazla karakter içeriyor
% The size of the constant string, which is assigned to a shortstring,
% is longer than the maximum size of the shortstring (255 characters).
type_w_comparison_always_false=04044_W_Sabitlik ve ifade aralığı nedeniyle karşılaştırma her zaman yanlış olabilir
% There is a comparison between a constant and an expression where the constant is out of the
% valid range of values of the expression. Because of type promotion, the statement will always evaluate to
% false. Explicitly typecast the constant or the expression to the correct range to avoid this warning
% if you think the code is correct.
type_w_comparison_always_true=04045_W_Sabitlik ve ifade aralığı nedeniyle karşılaştırma her zaman doğru olabilir
% There is a comparison between a constant and an expression where the constant is out of the
% valid range of values of the expression. Because of type promotion, the statement will always evaluate to
% true. Explicitly typecast the constant or the expression to the correct range to avoid this warning
% if you think the code is correct.
type_w_instance_with_abstract=04046_W_"$2" soyut yöntemiyle "$1" sınıfı oluşturma
% An instance of a class is created which contains non-implemented abstract
% methods. This will probably lead to a runtime error 211 in the code if that
% routine is ever called. All abstract methods should be overridden.
type_h_in_range_check=04047_H_IN operatörünün sol işleneni bayt boyutunda olmalıdır
% The left operand of the \var{in} operator is not an ordinal or enumeration which fits
% within 8 bits. This may lead to range check errors. The \var{in} operator
% currently only supports a left operand which fits within a byte. In the case of
% enumerations, the size of an element of an enumeration can be controlled with
% the \var{\{\$PACKENUM\}} or \var{\{\$Zn\}} switches.
type_w_smaller_possible_range_check=04048_W_Tip boyutu uyuşmazlığı, olası veri kaybı/aralık kontrolü hatası
% There is an assignment to a smaller type than the source type. This means that
% this may cause a range-check error, or may lead to possible loss of data.
type_h_smaller_possible_range_check=04049_H_Tip boyutu uyuşmazlığı, olası veri kaybı/aralık kontrolü hatası
% There is an assignment to a smaller type than the source type. This means that
% this may cause a range-check error, or may lead to possible loss of data.
type_e_cant_take_address_of_abstract_method=04050_E_Soyut bir yöntemin adresi alınamaz
% An abstract method has no body, so the address of an abstract method cannot be taken.
type_e_assignment_not_allowed=04051_E_Resmi parametrelere ve açık dizilere atama mümkün değildir
% You are trying to assign a value to a formal (untyped var, const or out)
% parameter, or to an open array.
type_e_constant_expr_expected=04052_E_Sabit İfade bekleniyor
% The compiler expects an constant expression, but gets a variable expression.
type_e_operator_not_supported_for_types=04053_E_"$1" işlemi "$2" ve "$3" türleri için desteklenmiyor
% The operation is not allowed for the supplied types.
type_e_illegal_type_conversion=04054_E_Geçersiz tip dönüşümü: "$1" dan "$2" a
% When doing a type-cast, you must take care that the sizes of the variable and
% the destination type are the same.
type_h_pointer_to_longint_conv_not_portable=04055_H_Sıra sayıları ve işaretçiler arasındaki dönüşüm taşınabilir değildir
% If you typecast a pointer to a longint (or vice-versa), this code will not compile
% on a machine using 64 bits addressing.
type_w_pointer_to_longint_conv_not_portable=04056_W_Sıra sayıları ve işaretçiler arasındaki dönüşüm taşınabilir değildir
% If you typecast a pointer to an ordinal type of a different size (or vice-versa), this can
% cause problems. This is a warning to help in finding the 32-bit specific code where cardinal/longint is used
% to typecast pointers to ordinals. A solution is to use the ptrint/ptruint types instead.
type_e_cant_choose_overload_function=04057_E_Hangi aşırı yük fonksiyonunun çağrılacağını belirleyemiyor
% You're calling overloaded functions with a parameter that doesn't correspond
% to any of the declared function parameter lists. e.g. when you have declared
% a function with parameters \var{word} and \var{longint}, and then you call
% it with a parameter which is of type \var{integer}.
type_e_illegal_count_var=04058_E_Geçersiz sayaç değişkeni
% The type of a \var{for} loop variable must be an ordinal type.
% Loop variables cannot be reals or strings.
type_w_double_c_varargs=04059_W_C değişken bağımsız değişkeni için sabit gerçek değeri ikiye katlayarak, bunu önlemek için açık bir typecast ekleyin.
% In C, constant real values are double by default. For this reason, if you
% pass a constant real value to a variable argument part of a C function, FPC
% by default converts this constant to double as well. If you want to prevent
% this from happening, add an explicit typecast around the constant.
type_e_class_or_cominterface_type_expected=04060_E_Class veya COM arabirim türü bekleniyordu, ancak "$1" aldı
% Some operators, such as the AS operator, are only applicable to classes or COM interfaces.
type_e_no_const_packed_array=04061_E_Sabit paketlenmiş diziler henüz desteklenmiyor
% You cannot declare a (bit)packed array as a typed constant.
type_e_got_expected_packed_array=04062_E_Arg no için uyumsuz tür. $1: bekleniyordu "$2" aldı "(Bit)Packed Array"
% The compiler expects a (bit)packed array as the specified parameter.
type_e_got_expected_unpacked_array=04063_E_Arg no için uyumsuz tür. $1: bekleniyordu "$2" aldı "(not packed) Array"
% The compiler expects a regular (i.e., not packed) array as the specified parameter.
type_e_no_packed_inittable=04064_E_Paketlenmiş dizilerin elemanları başlatılması gereken türde olamaz
% Support for packed arrays of types that need initialization
% (such as ansistrings, or records which contain ansistrings) is not yet implemented.
type_e_no_const_packed_record=04065_E_Sabit paketlenmiş kayıtlar ve nesneler henüz desteklenmiyor
% You cannot declare a (bit)packed array as a typed constant at this time.
type_w_untyped_arithmetic_unportable=04066_W_Yazılmamış işaretçideki aritmetik "$1", {$T+} 'ye kadar kullanılamaz, typecast öner
% Addition/subtraction from an untyped pointer may work differently in \var{\{\$T+\}}.
% Use a typecast to a typed pointer.
type_e_cant_take_address_of_local_subroutine=04076_E_Yerel olarak işaretlenmiş bir alt rutinin adresini alamıyor
% The address of a subroutine marked as local cannot be taken.
type_e_cant_export_local=04077_E_Bir birimden yerel olarak işaretlenmiş alt rutin dışa aktarılamıyor
% A subroutine marked as local cannot be exported from a unit.
type_e_not_automatable=04078_E_Tür otomatik olarak değil: "$1"
% Sadece bayt, tamsayı, longint, smallint, para birimi, single, double, ansistring,
% widestring, tdatetime, varyant, olevariant, wordbool ve tüm arayüzler otomatikleştirilebilir.
type_h_convert_add_operands_to_prevent_overflow=04079_H_Eklemeden önce işlenenleri "$1" değerine dönüştürmek taşma hatalarını önleyebilir.
% İki tür eklemek taşma hatalarına neden olabilir. Sonucu daha büyük bir türe dönüştürdüğünüz için,
% eklemeyi yapmadan önce işlenenleri bu türe dönüştürerek bu hataları önleyebilirsiniz.
type_h_convert_sub_operands_to_prevent_overflow=04080_H_Çıkartmayı yapmadan önce işlenenleri "$1" değerine dönüştürmek taşma hatalarını önleyebilir.
% İki türün çıkarılması taşma hatalarına neden olabilir. Sonucu daha büyük bir türe dönüştürdüğünüz için,
% çıkarma işlemini gerçekleştirmeden önce işlenenleri bu türe dönüştürerek bu hataları önleyebilirsiniz.
type_h_convert_mul_operands_to_prevent_overflow=04081_H_Çarpma yapmadan önce işlenenleri "$1" değerine dönüştürmek taşma hatalarını önleyebilir.
% İki tipin çarpılması taşma hatalarına neden olabilir. Sonucu daha büyük bir türe dönüştürdüğünüz için,
% çarpımı yapmadan önce işlenenleri bu türe dönüştürerek bu hataları önleyebilirsiniz.
type_w_pointer_to_signed=04082_W_İşaretçileri imzalı tam sayılara dönüştürmek hatalı karşılaştırma sonuçlarına ve aralık hatalarına neden olabilir, bunun yerine işaretsiz bir tür kullanın.
% 32 bit makinelerdeki sanal adres alanı \$00000000 - \$ffffffff arasında çalışır.
% Birçok işletim sistemi \$80000000 'ın üzerinde bellek ayırmanıza izin verir. Örneğin, hem \windows hem de \linux, \$0000000 - \$bfffffff aralığında işaretçilere izin verir.
% İşaretçileri imzalı türlere dönüştürürseniz, taşma ve aralık kontrolü hatalarına neden olabilir,
% ayrıca \$80000000 < \$7fffffff. Bu, "if p>q" gibi kodda rasgele hatalara neden olabilir.
type_e_interface_has_no_guid=04083_E_$1 Interface tipinde geçerli bir GUID yok
% As operatörünü bir interface veya sınıfa uygularken, istenen interface (yani as operatörünün sağ işleci) geçerli bir GUID'e sahip olmalıdır.
type_e_invalid_objc_selector_name=04084_E_"$1" geçersiz seçici adı
% Bir Objective-C seçici boş olamaz, geçerli bir tanımlayıcı veya tek bir sütun olmalıdır
% ve en az bir sütun içeriyorsa, bir tanede de bitmesi gerekir.
type_e_expected_objc_method_but_got=04085_E_Beklenen Objective-C yöntemi, ancak $1 var
% Bir seçici, başka herhangi bir prosedür /fonksiyon/methot için değil, yalnızca Objective-C yöntemleri için oluşturulabilir.
type_e_expected_objc_method=04086_E_Beklenen Objective-C yöntemi veya sabit yöntem adı
% Bir seçici, yalnızca bir dize sabiti kullanarak ad belirtilerek veya geçerli kapsamda
% görünen bir Objective-C yöntemi tanımlayıcısı kullanılarak Objective-C yöntemleri için oluşturulabilir.
type_e_no_type_info=04087_E_Bu tür için hiçbir bilgi türü yok
% Tip bilgisi, değer aralıkları arasında boşluk bulunan numaralandırmalar gibi bazı tipler için üretilmez
% (bu, alt sınırı sıfırdan farklı olan numaralandırmaları içerir).
type_e_ordinal_or_string_expr_expected=04088_E_Ordinal veya string ifade bekleniyor
% İfade bir ordinal veya string türü olmalıdır.
type_e_string_expr_expected=04089_E_String ifadesi bekleniyor
% İfade bir String türü olmalıdır.
type_w_zero_to_nil=04090_W_0'ı NIL'ye dönüştürme
% İşaretçi başlatırken 0 yerine NIL kullanın.
type_e_protocol_type_expected=04091_E_Objective-C protokolü türü bekleniyordu, ancak "$1" aldı
% Derleyici bir protokol türü adı bekliyordu, ancak başka bir şey buldu.
type_e_objc_type_unsupported=04092_E_Objective-C çalışma zamanı ile etkileşim için "$1" tipi desteklenmiyor.
% Objective-C, çalışma zamanı tipi bilgisini (RTTI) yoğun şekilde kullanır. Bu format çalışma zamanının koruyucular tarafından tanımlanır
% ve bu nedenle olası tüm Object Pascal tiplerine uyarlanamaz. Özellikle, derleyici tarafından
% referans sayımına bağlı olan türler (örneğin, cevaplar ve belirli türden arayüzler),
% Objective-C sınıflarının alanları olarak kullanılamaz, doğrudan Objective-C yöntemlerine
% geçirilemezler ve \var kullanılarak kodlanamazlar. {objc\_encode}.
type_e_class_or_objcclass_type_expected=04093_E_Class veya objcclass türü bekleniyordu, ancak "$1" aldı
% Yalnızca \var{class} ve \var{objcclass} sınıf başvuru türlerini oluşturmak mümkündür.
type_e_objcclass_type_expected=04094_E_Objcclass türü bekleniyor
% Derleyici bir \var{objcclass} türü bekleniyordu
type_w_procvar_univ_conflicting_para=04095_W_Usul değişkenindeki zorlanmış univ parametre tipi çökmeye veya hafızanın bozulmasına neden olabilir: $1 ila $2
% \var{univ} parameters are implicitly compatible with all types of the same size,
% also in procedural variable definitions. That means that the following code is
% legal, because \var{single} and \var{longint} have the same size:
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
% This code may however crash on platforms that pass integers in registers and
% floating point values on the stack, because then the stack will be unbalanced.
% Note that this warning will not flagg all potentially dangerous situations.
% when \var{test} returns.
type_e_generics_cannot_reference_itself=04096_E_Jenerikliğin uzmanlık tip parametreleri şu anda uzmanlaşmış tipe referans veremez
% Recursive specializations of generics like \var{Type MyType = specialize MyGeneric<MyType>;} are not possible.
type_e_type_parameters_are_not_allowed_here=04097_E_Jenerik olmayan class/record/object prosedürü veya fonksiyonu tür parametrelerine izin verilmez
% Type parameters are only allowed for methods of generic classes, records or objects
type_e_generic_declaration_does_not_match=04098_E_"$1" nın genel bildirimi, önceki bildirimden farklı
% Generic declaration does not match the previous declaration
type_e_helper_type_expected=04099_E_Yardımcı türü bekleniyor
% The compiler expected a \var{class helper} type.
type_e_record_type_expected=04100_E_Kayıt türü bekleniyor
% The compiler expected a \var{record} type.
type_e_class_helper_must_extend_subclass=04101_E_Türetilmiş sınıf yardımcısı, "$1" alt sınıfını veya sınıfın kendisini genişletmelidir
% If a class helper inherits from another class helper the extended class must
% extend either the same class as the parent class helper or a subclass of it
type_e_record_helper_must_extend_same_record=04102_E_Türetilmiş kayıt veya yardımcı türü "$1" uzatmalıdır
% If a record helper inherits from another record helper it must extend the same
% record that the parent record helper extended.
type_e_procedures_return_no_value=04103_E_Geçersiz atama, prosedürler değer döndürmüyor
% This error occurs when one tries to assign the result of a procedure or destructor call.
% A procedure or destructor returns no value so this is not
% possible.
type_w_implicit_string_cast=04104_W_"$1" den "$2" ye kadar dolaylı string türü dönüşümü
% An implicit type conversion from an ansi string type to an unicode string type is
% encountered. To avoid this warning perform an explicit type conversion.
type_w_implicit_string_cast_loss=04105_W_"$1" den "$2" ye kadar potansiyel veri kaybı ile dolaylı string türü dönüşümü
% An implicit type conversion from an unicode string type to an ansi string type is
% encountered. This conversion can lose data since not all unicode characters may be represented in the codepage of
% destination string type.
type_w_explicit_string_cast=04106_-W_"$1" den "$2" ye kadar belli string türü
% An explicit typecast from an ansi string type to an unicode string type is
% encountered. This warning is off by default. You can turn it on to see all suspicious string conversions.
type_w_explicit_string_cast_loss=04107_-W_Belli string türü, "$1" den "$2" ye kadar potansiyel veri kaybına neden oldu
% An explicit typecast from an unicode string type to an ansi string type is
% encountered. This conversion can lose data since not all unicode characters may be represented in the codepage of
% destination string type. This warning is off by default. You can turn it on to see all the places with lossy string
% conversions.
type_w_unicode_data_loss=04108_W_Potansiyel veri kaybı ile Unicode sabit döküm
% Conversion from a WideChar to AnsiChar can lose data since now all unicode characters may be represented in the current
% system codepage
% You can nest function definitions only 31 levels deep.
type_e_range_check_error_bounds=04109_E_Sabitleri değerlendirirken aralık kontrol hatası ($1 $2 ile $3 arasında olmalıdır)
type_w_range_check_error_bounds=04110_W_Sabitleri değerlendirirken aralık kontrol hatası ($1 $2 ile $3 arasında olmalıdır)
% The constants are outside their allowed range.
type_e_type_not_allowed_for_default=04111_E_Bu tür varsayılan() içsel için desteklenmiyor
% Some types like for example Text and File Of X are not supported by the Default intrinsic.
type_e_java_class_method_not_static_virtual=04112_E_JVM sanal sınıf yöntemleri statik olamaz
% Virtual class methods cannot be static when targetting the JVM platform, because
% the self pointer is required for correct dispatching.
type_e_invalid_final_assignment=04113_E_Final (class) alanları yalnızca kendi sınıflarındaki (class) constructor a atanabilir
% It is only possible to assign a value to a final (class) field inside a (class) constructor of its owning class.
type_e_no_managed_formal_assign_typecast=04114_E_Türetilmemiş parametrelerin yönetilen platformlara yazılması mümkün değildir, sadece bunlara bir değer atayın.
% On managed platforms, untyped parameters are translated by the compiler into
% the equivalent of \var{var x: BaseClassType}. Non-class-based types passed to
% such parameters are automatically wrapped (or boxed) in a class, and after the
% call the potentially modified value is assigned back to the original variable.
% On the caller side, changing untyped var/out parameters happens by simply assigning
% values to them (either class-based or primitive ones). On the caller side,
% they will be extracted and if their type does not match the original variable's,
% an exception will be raised.
type_e_no_managed_assign_generic_typecast=04115_E_Bir ifadenin atama tarafı, yönetilen platformlardaki bir üst yazıya yazılamaz
% Managed platforms guarantee type safety at the bytecode level. This means that the virtual machine must be able
% to statically determine that no type-unsafe assignments or operations occur. By assigning a parent class type to a
% variable of a child type by typecasting the assignment side to the parent class type, the type safety would no
% longer be guaranteed and the generated code would fail verification at run time time.
type_w_interface_lower_visibility=04116_-W_"$1" interface yöntemi, bir interface örneği üzerinden erişildiğinde "$2" görünürlüğünü public oluyor 
type_e_interface_lower_visibility=04117_E_"$1" interface yöntemi "$2" den daha yüksek bir görünürlük oranına (public) sahiptir
% All methods in an interface have always public visibility. That means that if
% an interface method is implemented using a (strict) protected or private method,
% this method is actually publicly accessible via the interface. On the JVM
% target this situation results in an error because the JVM rejects such
% attempts to circumvent the visibility rules. On other targets this is a
% warning that is disabled by default because such situations are common
% practice, but it can be enabled in case you are concerned with keeping your
% code compilable for the JVM target.
type_e_typeof_requires_vmt=04118_E_TYPEOF sadece VMT'li nesne tiplerinde kullanılabilir
% Typeof() intrinsic returns pointer to VMT of its argument. It cannot be used on object types that do not have VMT.
type_e_invalid_default_value=04119_E_"$1" türünde bir parametre için varsayılan bir değer tanımlamak mümkün değildir
% Parameters declared as structured types, such as files, variants, non-dynamic
% arrays and TP-style objects, cannot have a default value.
type_e_type_not_allowed_for_type_helper=04120_E_"$1" tipi bir yardımcı tarafından genişletilemiyor
% Types like procedural variables cannot be extended by type helpers
type_e_procedure_must_be_far=04121_E_Adresini almaya izin vermek için prosedür ya da fonksiyon çok uzak olmalıdır: "$1"
% In certain i8086 memory models (medium, large and huge), procedures and functions
% have to be declared 'far' in order to allow their address to be taken.
type_w_instance_abstract_class=04122_W_"$1" abstract class bir örneğini oluşturma
% The specified class is declared as \var{abstract} and thus no instance of this class
% should be created. This is merely a warning for Delphi compatibility.
% \end{description}
#
# Symtable
#
# 05095 is the last used one
#
% \section{Symbol handling}
% This section lists all the messages that concern the handling of symbols.
% This means all things that have to do with procedure and variable names.
% \begin{description}
sym_e_id_not_found=05000_E_Tanımlayıcı bulunamadı "$1"
% The compiler doesn't know this symbol. Usually happens when you misspell
% the name of a variable or procedure, or when you forget to declare a
% variable.
sym_f_internal_error_in_symtablestack=05001_F_SymTableStack() içindeki iç hata
% An internal error occurred in the compiler; If you encounter such an error,
% please contact the developers and try to provide an exact description of
% the circumstances in which the error occurs.
sym_e_duplicate_id=05002_E_"$1" yinelenen tanımlayıcı
% The identifier was already declared in the current scope.
sym_h_duplicate_id_where=05003_H_Tanımlayıcı zaten $1 satırında, $2 satırında tanımlandı
% The identifier was already declared in a previous scope.
sym_e_unknown_id=05004_E_Bilinmeyen tanımlayıcı "$1"
% The identifier encountered has not been declared, or is used outside the
% scope where it is defined.
sym_e_forward_not_resolved=05005_E_İleri bildirim "$1" çözülmedi
% This can happen in two cases:
% \begin{itemize}
% \item You declare a function in the \var{interface} part, or
% with a \var{forward} directive, but do not implement it.
% \item You reference a type which isn't declared in the current \var{type}
% block.
% \end{itemize}
sym_e_error_in_type_def=05007_E_Tür tanımında hata
% There is an error in your definition of a new array type.
% One of the range delimiters in an array declaration is erroneous.
% For example, \var{Array [1..1.25]} will trigger this error.
sym_e_forward_type_not_resolved=05009_E_İletme türü "$1" çözülmedi
% A symbol was forward defined, but no declaration was encountered.
sym_e_only_static_in_static=05010_E_Statik yöntemlerde veya dış yöntemlerde yalnızca statik değişkenler kullanılabilir.
% A static method of an object can only access static variables.
sym_e_type_must_be_rec_or_object_or_class=05012_E_Kayıt veya nesne veya sınıf türü bekleniyor
% The variable or expression isn't of the type \var{record} or \var{object} or \var{class}.
sym_e_no_instance_of_abstract_object=05013_E_Soyut yöntemle sınıfların veya nesnelerin örneklerine izin verilmez
% You are trying to generate an instance of a class which has an abstract
% method that wasn't overridden.
sym_w_label_not_defined=05014_W_Label tanımlı değil "$1"
% A label was declared, but not defined.
sym_e_label_used_and_not_defined=05015_E_Label kullanılmış ancak "$1" tanımlı değil
% A label was declared and used, but not defined.
sym_e_ill_label_decl=05016_E_Illegal label tanımı
% This error should never happen; it occurs if a label is defined outside a
% procedure or function.
sym_e_goto_and_label_not_supported=05017_E_GOTO ve LABEL desteklenmez (-Sg anahtarını kullanın)
% You must use the -Sg switch to compile a program which has \var{label}s
% and \var{goto} statements. By default, \var{label} and \var{goto} are not
% supported.
sym_e_label_not_found=05018_E_Label bulunamadı
% A \var{goto label} was encountered, but the label wasn't declared.
sym_e_id_is_no_label_id=05019_E_Label tanımlanmamış
% The identifier specified after the \var{goto} isn't of type label.
sym_e_label_already_defined=05020_E_label zaten tanımlanmış
% You are defining a label twice. You can define a label only once.
sym_e_ill_type_decl_set=05021_E_Küme öğelerinin illegal tür bildirimi
% The declaration of a set contains an invalid type definition.
sym_e_class_forward_not_resolved=05022_E_İleri class tanımı "$1" çözülemedi
% Bir class oluşturdunuz, ancak uygulamamışsınız.
sym_n_unit_not_used=05023_H_Birim $2 içinde "$1" uniti kullanılmıyor
% Başvurulan birim \var{uses} kullanılmıyor.
sym_h_para_identifier_not_used=05024_H_Parametre "$1" kullanılmıyor
% Parametre (yerel veya genel olarak) tanımlandı, 
% ancak kullanılmadı (yerel veya genel olarak).
sym_n_local_identifier_not_used=05025_N_Yerel değişken "$1" kullanılmadı
% Bir prosedür veya fonksiyonda bir değişken tanımladınız ancak kullanmadınız.
sym_h_para_identifier_only_set=05026_H_Parametre değeri "$1" atandı ancak hiç kullanılmadı
% Parametre (yerel veya global olarak) tanımlandı ve atandı. Ama (yerel veya global)kullanılmadı.
sym_n_local_identifier_only_set=05027_N_Yerel değişken "$1" atandı ancak hiç kullanılmadı
% Bir prosedür veya fonksiyonda bir değişken tanımladınız ancak kullanmadınız.
sym_h_local_symbol_not_used=05028_H_Yerel $1 "$2" sembolü kullanılmıyor
% Sembol kullanılmadı
sym_n_private_identifier_not_used=05029_N_Private alanı "$1.$2"  hiç kullanılmamış
% Private alanı tanımladınız ama kodunuzda kullanmadınız.
sym_n_private_identifier_only_set=05030_N_Private alanı "$1.$2" atandı ancak hiç kullanılmadı
% Private alanı tanımladınız ancak asla okunmadı.
sym_n_private_method_not_used=05031_N_Private method "$1.$2" hiç kullanılmamış
% Private metodu tanımladınız ama kodunuzda kullanmadınız.
sym_e_set_expected=05032_E_Beklenen türü ayarla
% Değişken veya ifade, \var{set} türünde değil. Bu bir \var{in} ifadesinde olur.
sym_w_function_result_not_set=05033_W_Fonksiyon sonucu ayarlanmamış gibi görünüyor
% You can get this warning if the compiler thinks that a function return
% value is not set. This will not be displayed for assembler procedures,
% or procedures that contain assembler blocks.
sym_w_wrong_C_pack=05034_W_"$1" türü C için geçerli, kayıtta doğru şekilde hizalanmamış
% Arrays with sizes not multiples of 4 will be wrongly aligned
% for C structures.
sym_e_illegal_field=05035_E_Bilinmeyen kayıt alanı tanımlayıcısı "$1"
% The field doesn't exist in the record/object definition.
sym_w_uninitialized_local_variable=05036_W_"$1" yerel değişkeni başlatılmadı gibi görünüyor
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. it appears in the right-hand side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assignment).
sym_w_uninitialized_variable=05037_W_"$1" değişkeni başlatılmadı gibi görünüyor
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. it appears in the right-hand side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assignment).
sym_e_id_no_member=05038_E_tanımlayıcı numarası "$1" üyesi yok
% This error is generated when an identifier of a record,
% field or method is accessed while it is not defined.
sym_h_param_list=05039_H_Bulunan bildirim: $1
% You get this when you use the \var{-vh} switch.In the case of an overloaded procedure
% not being found. Then all candidate overloaded procedures are
% listed, with their parameter lists.
sym_e_segment_too_large=05040_E_Veri öğesi çok büyük
% You get this when you declare a data element whose size exceeds the
% prescribed limit (2 Gb on 80386+/68020+ processors).
sym_e_no_matching_implementation_found=05042_E_"$1" Interface yöntemi için eşleşen bir uygulama bulunamadı
% There was no matching method found which could implement the interface
% method. Check argument types and result type of the methods.
sym_w_deprecated_symbol=05043_W_"$1" simgesi kullanımdan kaldırıldı
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{deprecated} is used. Deprecated symbols may no longer
% be available in newer versions of the unit / library. Use of this symbol
% should be avoided as much as possible.
sym_w_non_portable_symbol=05044_W_"$1" sembolü taşınabilir değil
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{platform} is used. This symbol's value, use
% and availability is platform specific and should not be used
% if the source code must be portable.
sym_w_non_implemented_symbol=05055_W_"$1" sembolü uygulanmadı
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{unimplemented} is used. This symbol is defined,
% but is not yet implemented on this specific platform.
sym_e_cant_create_unique_type=05056_E_Bu türden benzersiz tür oluşturulamıyor
% Only simple types like ordinal, float and string types are supported when
% redefining a type with \var{type newtype = type oldtype;}.
sym_h_uninitialized_local_variable=05057_H_"$1" yerel değişkeni başlatılmadı
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. it appears in the right-hand side of an expression) when it
% was not initialized first (i.e. it did not appear in the left-hand side of an
% assignment).
sym_h_uninitialized_variable=05058_H_"$1" değişkeni başlatılmadı gibi görünüyor
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. it appears in the right-hand side of an expression) when it
% was not initialized first (i.e. t did not appear in the left-hand side of an
% assignment).
sym_w_function_result_uninitialized=05059_W_Fonksiyonda sonuç değişkeni başlatılmadı gibi görünüyor
% This message is displayed if the compiler thinks that the function result
% variable will be used (i.e. it appears in the right-hand side of an expression)
% before it is initialized (i.e. before it appeared in the left-hand side of an
% assignment).
sym_h_function_result_uninitialized=05060_H_Fonksiyonda sonuç değişkeni başlatılmadı gibi görünüyor
% This message is displayed if the compiler thinks that the function result
% variable will be used (i.e. it appears in the right-hand side of an expression)
% before it is initialized (i.e. it appears in the left-hand side of an
% assignment)
sym_w_identifier_only_read=05061_W_Değişken "$1" okundu ancak hiçbir yerde atanmadı
% You have read the value of a variable, but nowhere assigned a value to
% it.
sym_h_abstract_method_list=05062_H_Bulunan abstract methodu: $1
% When getting a warning about constructing a class/object with abstract methods
% you get this hint to assist you in finding the affected method.
sym_w_experimental_symbol=05063_W_"$1" sembolü deneysel
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{experimental} is used. Experimental symbols
% might disappear or change semantics in future versions. Usage of this symbol
% should be avoided as much as possible.
sym_w_forward_not_resolved=05064_W_"$1" iletme bildirimi çözülmedi, harici kabul edildi
% This happens if you declare a function in the \var{interface} of a unit in macpas mode,
% but do not implement it.
sym_w_library_symbol=05065_W_"$1" sembolü bir kütüphaneye aittir
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{library} is used. Library symbols may not be
% available in other libraries.
sym_w_deprecated_symbol_with_msg=05066_W_"$1" sembolü kullanımdan kaldırıldı: "$2"
% This means that a symbol (a variable, routine, etc...) which is
% declared as \var{deprecated} is used. Deprecated symbols may no longer
% be available in newer versions of the unit / library. Use of this symbol
% should be avoided as much as possible.
sym_e_no_enumerator=05067_E_"$1" türü için bir numaralandırıcı bulunamıyor
% This means that compiler cannot find an apropriate enumerator to use in the for-in loop.
% To create an enumerator you need to defind an operator enumerator or add a public or published
% GetEnumerator method to the class or object definition.
sym_e_no_enumerator_move=05068_E_"$1" numaralandırıcısında "MoveNext" yöntemi bulunamıyor
% This means that compiler cannot find a public MoveNext method with the Boolean return type in
% the enumerator class or object definition.
sym_e_no_enumerator_current=05069_E_"$1" numaralandırıcısında "Current" özelliği bulunamıyor
% This means that compiler cannot find a public Current property in  the enumerator class or object
% definition.
sym_e_objc_para_mismatch=05070_E_İleti dizgisinde bildirilen parametrelerin sayısı ile kolon sayısı arasındaki uyuşmazlık var.
% In Objective-C, a message name automatically contains as many colons as parameters.
% In order to prevent mistakes when specifying the message name in FPC, the compiler
% checks whether this is also the case here. Note that in case of messages taking a
% variable number of arguments translated to FPC via an \var{array of const} parameter,
% this final \var{array of const} parameter is not counted. Neither are the hidden
% \var{self} and \var{\_cmd} parameters.
sym_n_private_type_not_used=05071_N_Private tipi "$1.$2" hiç kullanılmamış
% The indicated private type is declared but is never used in the code.
sym_n_private_const_not_used=05072_N_Private sabiti "$1.$2" hiç kullanılmamış
% The indicated private const is declared but is never used in the code.
sym_n_private_property_not_used=05073_N_Private "$1.$2" özelliği hiç kullanılmamış
% The indicated private property is declared but is never used in the code.
sym_w_deprecated_unit=05074_W_"$1" Birimi kullanımdan kaldırıldı
% This means that a unit which is
% declared as \var{deprecated} is used. Deprecated units may no longer
% be available in newer versions of the library. Use of this unit
% should be avoided as much as possible.
sym_w_deprecated_unit_with_msg=05075_W_"$1" Birimi kullanımdan kaldırıldı: "$2"
% This means that a unit which is
% declared as \var{deprecated} is used. Deprecated units may no longer
% be available in newer versions of the library. Use of this unit
% should be avoided as much as possible.
sym_w_non_portable_unit=05076_W_"$1" Birimi taşınabilir değil
% This means that a unit which is
% declared as \var{platform} is used. This unit use
% and availability is platform specific and should not be used
% if the source code must be portable.
sym_w_library_unit=05077_W_"$1" Birimi bir kütüphaneye ait
% This means that a unit which is
% declared as \var{library} is used. Library units may not be
% available in other libraries.
sym_w_non_implemented_unit=05078_W_"$1" Birimi uygulanmadı
% This means that a unit which is
% declared as \var{unimplemented} is used. This unit is defined,
% but is not yet implemented on this specific platform.
sym_w_experimental_unit=05079_W_"$1" Birimi deneysel
% This means that a unit which is
% declared as \var{experimental} is used. Experimental units
% might disappear or change semantics in future versions. Usage of this unit
% should be avoided as much as possible.
sym_e_formal_class_not_resolved=05080_E_Resmen bildirilen "$1" sınıfının tam tanımı kapsam dahilinde değildir. Tam tanımını içeren birimi Uses yan tümcesine ekleyin.
% Objecive-C and Java classes can be imported formally, without using the the unit in which it is fully declared.
% This enables making forward references to such classes and breaking circular dependencies amongst units.
% However, as soon as you wish to actually do something with an entity of this class type (such as
% access one of its fields, send a message to it, or use it to inherit from), the compiler requires the full definition
% of the class to be in scope.
sym_e_interprocgoto_into_init_final_code_not_allowed=05081_E_Ünitelerin initialization veya finalization bloklarına giremez
% Gotos into initialization or finalization blockse of units are not allowed.
sym_e_external_class_name_mismatch1=05082_E_"$2" resmi sınıfı için geçersiz "$1" harici adı
sym_e_external_class_name_mismatch2=05083_E_Burada "$1" harici adına sahip tam sınıf tanımı
% When a class is declared using a formal external definition, the actual external
% definition (if any) must specify the same external name as the formal definition
% (since both definitions refer to the same actual class type).
sym_w_library_overload=05084_W_Olası kütüphane çatışması: "$2" kütüphanesinden "$1" sembolü de "$3" kütüphanesinde bulundu
% Some OS do not have library specific namespaces, for those
% OS, the function declared as "external 'libname' name 'funcname'",
% the 'libname' part is only a hint, funcname might also be loaded
% by another library. This warning appears if 'funcname' is used twice
% with two different library names.
sym_e_duplicate_id_create_java_constructor=05085_E_"$1" tarafından zaten kullanılan bir tanımlayıcı olduğundan, "Create" oluşturucu ekleyemezsiniz.
% Java does not automatically add inherited constructors to child classes, so that they can be hidden.
% However, if a class does not explicitly declare at least one constructor, the compiler is
% required to add a public, parameterless constructor. In Java, constructors are nameless,
% but in FPC they are all called ``Create''. Therefore, if you do not add a constructor to
% a Java class and furthermore use the ``Create'' identifier for another entity (e.g., a field,
% or a parameterless method), the compiler cannot satisfy this requirement.
sym_e_no_matching_inherited_parameterless_constructor=05086_E_Class için varsayılan constructor oluşturulamıyor, çünkü üst parametresiz constructor yok
% Java does not automatically add inherited constructors to child classes, so that they can be hidden.
% However, if a class does not explicitly declare at least one constructor, the compiler is
% required to add a public, parameterless constructor. This compiler must then call
% the parameterless constructor from the parent class inside this added constructor.
% This is however impossible if the parent class does not declare such a constructor.
% In this case you must add a valid constructor yourself.
sym_d_adding_helper_for=05087_D_$1 için yardımcı ekleme
% A helper for the mentioned type is added to the current scope
sym_e_param_list=05088_E_Bulunan deklerasyon: $1
% This message shows all overloaded declarations in case of an error.
sym_w_uninitialized_managed_local_variable=05089_W_Yönetilen bir türün "$1" yerel değişkeni başlatılmadı
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. it appears in the right-hand side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assignment). Since the variable is managed, i. e. implicitly initialized by the compiler, this might be intended behaviour and
% does not necessarily mean that the code is wrong.
sym_w_uninitialized_managed_variable=05090_W_Yönetilen bir türün "$1" değişkeni başlatılmadı
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. it appears in the right-hand side of an expression) when it
% was not initialized first (i.e. appeared in the left-hand side of an
% assignment). Since the variable is managed, i. e. implicitly initialized by the compiler, this might be intended behaviour and
% does not necessarily mean that the code is wrong.
sym_h_uninitialized_managed_local_variable=05091_H_Yönetilen bir türün "$1" yerel değişkeni başlatılmadı
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. it appears in the right-hand side of an expression) when it
% was not initialized first (i.e. it did not appear in the left-hand side of an
% assignment). Since the variable is managed, i. e. implicitly initialized by the compiler, this might be intended behaviour and
% does not necessarily mean that the code is wrong.
sym_h_uninitialized_managed_variable=05092_H_Yönetilen bir türün "$1" yerel değişkeni başlatılmadı
% This message is displayed if the compiler thinks that a variable will
% be used (i.e. it appears in the right-hand side of an expression) when it
% was not initialized first (i.e. t did not appear in the left-hand side of an
% assignment). Since the variable is managed, i. e. implicitly initialized by the compiler, this might be intended behaviour and
% does not necessarily mean that the code is wrong.
sym_w_managed_function_result_uninitialized=05093_W_Yönetilen bir türün fonksiyon sonuç değişkeni başlatılmadı gibi görünüyor
% This message is displayed if the compiler thinks that the function result
% variable will be used (i.e. it appears in the right-hand side of an expression)
% before it is initialized (i.e. before it appeared in the left-hand side of an
% assignment). Since the variable is managed, i. e. implicitly initialized by the compiler, this might be intended behaviour and
% does not necessarily mean that the code is wrong.
sym_h_managed_function_result_uninitialized=05094_H_Yönetilen bir türün fonksiyon sonuç değişkeni başlatılmadı gibi görünüyor
% This message is displayed if the compiler thinks that the function result
% variable will be used (i.e. it appears in the right-hand side of an expression)
% before it is initialized (i.e. it appears in the left-hand side of an
% assignment). Since the variable is managed, i. e. implicitly initialized by the compiler, this might be intended behaviour and
% does not necessarily mean that the code is wrong.
sym_w_duplicate_id=05095_W_"$1" yinelenen tanımlayıcısı
% The identifier was already declared in an Objective-C category that's in the
% same scope as the current identifier. This is a warning instead of an error,
% because while this hides the identifier from the category, there are often
% many unused categories in scope.
% \end{description}
#
# Codegenerator
#
# 06049 is the last used one
#
% \section{Code generator messages}
% This section lists all messages that can be displayed if the code
% generator encounters an error condition.
% \begin{description}
cg_e_parasize_too_big=06009_E_Parametre listesi boyutu 65535 baytı aşıyor
% The I386 processor limits the parameter list to 65535 bytes. (The \var{RET}
% instruction causes this.)
cg_e_file_must_call_by_reference=06012_E_Dosya türleri değişken parametreler olmalıdır
% You cannot specify files as value parameters, i.e., they must always be
% declared \var{var} parameters.
cg_e_cant_use_far_pointer_there=06013_E_Uzaktaki bir işaretçinin kullanımına izin verilmez
% Free Pascal doesn't support far pointers, so you cannot take the address of
% an expression which has a far reference as a result. The \var{mem} construct
% has a far reference as a result, so the following code will produce this
% error:
% \begin{verbatim}
% var p : pointer;
% ...
% p:=@mem[a000:000];
% \end{verbatim}
cg_e_dont_call_exported_direct=06015_E_EXPORT bildirilen fonksiyonlar çağrılamaz
% No longer in use.
cg_w_member_cd_call_from_method=06016_W_Olası illegal constructor veya destructor
% The compiler detected that a constructor or destructor is called within a
% a method. This will probably lead to problems, since constructors / destructors
% require parameters on entry.
cg_n_inefficient_code=06017_N_Verimsiz kod
% Your statement seems dubious to the compiler.
cg_w_unreachable_code=06018_W_ulaşılamaz kod
% You specified a construct which will never be executed. Example:
% \begin{verbatim}
% while false do
%   begin
%   {.. code ...}
%   end;
% \end{verbatim}
cg_e_cant_call_abstract_method=06020_E_Abstract methodları doğrudan çağrılamaz
% You cannot call an abstract method directly. Instead, you must call an
% overriding child method, because an abstract method isn't implemented.
cg_d_register_weight=06027_DL_Kayıt $1 uzunluk $2 $3
% Debugging message. Shown when the compiler considers a variable for
% keeping in the registers.
cg_d_stackframe_omited=06029_DL_Yığın çerçevesi atlandı
% Some procedure/functions do not need a complete stack-frame, so it is omitted.
% This message will be displayed when the {-vd} switch is used.
cg_e_unable_inline_object_methods=06031_E_Object veya class yöntemleri satır içi olamaz.
% You cannot have inlined object methods.
cg_e_unable_inline_procvar=06032_E_Procvar çağrıları satır içi olamaz.
% A procedure with a procedural variable call cannot be inlined.
cg_e_no_code_for_inline_stored=06033_E_Satır içi prosedür için kayıtlı kod yok
% The compiler couldn't store code for the inline procedure.
cg_e_can_access_element_zero=06035_E_Bir ansi/wide- veya longstring öğesinin sıfırına erişilemiyor, bunun yerine (set) uzunluk kullanın
% You should use \var{setlength} to set the length of an ansi/wide/longstring
% and \var{length} to get the length of such string type.
cg_e_cannot_call_cons_dest_inside_with=06037_E_Constructors veya destructors 'with' yan tümcesinde çağrılamaz 
% Inside a \var{with} clause you cannot call a constructor or destructor for the
% object you have in the \var{with} clause.
cg_e_cannot_call_message_direct=06038_E_Mesaj işleyicisi yöntemleri doğrudan çağrılamıyor
% A message method handler method cannot be called directly if it contains an
% explicit \var{Self} argument.
cg_e_goto_inout_of_exception_block=06039_E_Bir istisna bloğuna veya dışına atla
% It is not allowed to jump in or outside of an exception block like \var{try..finally..end;}.
% For example, the following code will produce this error:

% \begin{verbatim}
% label 1;
%
% ...
%
% try
%    if not(final) then
%      goto 1;   // this line will cause an error
% finally
%   ...
% end;
% 1:
% ...
% \end{verbatim}
cg_e_control_flow_outside_finally=06040_E_Kontrol akış ifadelerine son blokta izin verilmiyor
% It isn't allowed to use the control flow statements \var{break},
% \var{continue} and \var{exit}
% inside a finally statement. The following example shows the problem:
% \begin{verbatim}
% ...
%   try
%      p;
%   finally
%      ...
%      exit;  // This exit ISN'T allowed
%   end;
% ...
%
% \end{verbatim}
% If the procedure \var{p} raises an exception the finally block is
% executed. If the execution reaches the exit, it's unclear what to do:
% exit the procedure or search for another exception handler.
cg_w_parasize_too_big=06041_W_Parametre boyutu, belirli cpu'nun limitini aşıyor
% This indicates that you are declaring more than 64K of parameters, which
% might not be supported on other processor targets.
cg_w_localsize_too_big=06042_W_Yerel değişken büyüklüğü belirli cpu'nun limitini aşıyor
% This indicates that you are declaring more than 32K of local variables, which
% might not be supported on other processor targets.
cg_e_localsize_too_big=06043_E_Yerel değişkenlerin boyutu desteklenen sınırı aşıyor
% This indicates that you are declaring more than 32K of local variables, which
% is not supported by this processor.
cg_e_break_not_allowed=06044_E_BREAK izin verilmedi
% You're trying to use \var{break} outside a loop construction.
cg_e_continue_not_allowed=06045_E_CONTINUE izin verilmedi
% You're trying to use \var{continue} outside a loop construction.
cg_f_unknown_compilerproc=06046_F_Bilinmeyen derleyici proc "$1". Doğru çalışma zamanı kitaplığını kullanıp kullanmadığınızı kontrol edin.
% The compiler expects that the runtime library contains certain subroutines. If you see this error
% and you didn't change the runtime library code, it's very likely that the runtime library
% you're using doesn't match the compiler in use. If you changed the runtime library this error means
% that you removed a subroutine which the compiler needs for internal use.
cg_f_unknown_system_type=06047_F_"$1" sistem türü bulunamıyor. Doğru çalışma zamanı kitaplığını kullanıp kullanmadığınızı kontrol edin.
% The compiler expects that the runtime library contains certain type definitions. If you see this error
% and you didn't change the runtime library code, it's very likely that the runtime library
% you're using doesn't match the compiler in use. If you changed the runtime library this error means
% that you removed a type which the compiler needs for internal use.
cg_h_inherited_ignored=06048_H_Inherited çağrısı abstract methodunda yok sayıldı.
% This message appears only in Delphi mode when you call an abstract method
% of a parent class via \var{inherited;}. The call is then ignored.
cg_e_goto_label_not_found=06049_E_Goto label "$1" tanımlanmadı ya da optimize edilmedi
% The label used in the goto definition is not defined or optimized away by the
% unreachable code elemination.
cg_f_unknown_type_in_unit=06050_F_"$2" tipinde "$1" tipi bulunamıyor. Doğru çalışma zamanı kitaplığını kullanıp kullanmadığınızı kontrol edin.
% The compiler expects that the runtime library contains certain type definitions. If you see this error
% and you didn't change the runtime library code, it's very likely that the runtime library
% you're using doesn't match the compiler in use. If you changed the runtime library this error means
% that you removed a type which the compiler needs for internal use.
cg_e_interprocedural_goto_only_to_outer_scope_allowed=06051_E_İnterprocedural gotos yalnızca dış alt rutinlere izin verilir
% Gotos between subroutines are only allowed if the goto jumps from an inner to an outer subroutine or
% from a subroutine to the main program
cg_e_labels_cannot_defined_outside_declaration_scope=06052_E_Label bildirildiği şekilde aynı kapsamda tanımlanmalıdır
% In ISO mode, labels must be defined in the same scope as they are declared.
cg_e_goto_across_procedures_with_exceptions_not_allowed=06053_E_Goto kullanarak explicit veya implicit istisnalar çerçeveleri içeren bırakma prosedürlerine izin verilmez
% Non-local gotos might not be used to leave procedures using exceptions either implicitly or explicitly. Procedures
% which use automated types like ansistrings or class constructurs are affected by this too.
cg_e_mod_only_defined_for_pos_quotient=06054_E_ISO modunda, mod operatörü sadece pozitif bölüm için tanımlanmıştır.
% In ISO pascal, only positive values are allowed for the quotient: \var{n mod m} is only valid if \var{m>0}.
cg_d_autoinlining=06055_DL_Otomatik satır içi: $1
% Due to auto inlining turned on, the compiler auto inlines this subroutine.
cg_e_function_not_support_by_selected_instruction_set=06056_E_Kullanılan fonksiyon, seçilen komut seti tarafından desteklenmiyor: $1
% Some functions cannot be implemented efficiently for certain instruction sets, one example is fused multiply/add.
% To avoid very inefficient code, the compiler complains in this case, so either select another instruction set
% or replace the function call by alternative code
%
% \end{description}
# EndOfTeX
#
# Assembler reader
#
# 07125 is the last used one
#
asmr_d_start_reading=07000_DL_$1 tarz assembler ayrıştırma işlemi başladı
% This informs you that an assembler block is being parsed
asmr_d_finish_reading=07001_DL_$1 tarz assembler ayrıştırma işlemi tamamlandı
% This informs you that an assembler block has finished.
asmr_e_none_label_contain_at=07002_E_Non-label desen içeriyor @
% A identifier which isn't a label cannot contain a @.
asmr_e_building_record_offset=07004_E_Kayıt ofset oluşturma hatası
% There has an error occured while building the offset of a record/object
% structure, this can happend when there is no field specified at all or
% an unknown field identifier is used.
asmr_e_offset_without_identifier=07005_E_Tanımlayıcı olmadan OFFSET kullanımı
% You can only use OFFSET with an identifier. Other syntaxes are not
% supported
asmr_e_type_without_identifier=07006_E_Tanımlayıcı olmadan TYPE kullanımı
% You can only use TYPE with an identifier. Other syntaxes are not
% supported
asmr_e_no_local_or_para_allowed=07007_E_Burada yerel değişken veya parametreler kullanılamaz
% You cannot use a local variable or parameter here, mostly because the
% addressing of locals and parameters is done using the frame pointer register so the
% address cannot be obtained directly.
asmr_e_need_offset=07008_E_burada OFSET kullanmanız gerekir
% You need to use OFFSET <id> here to get the address of the identifier.
asmr_e_need_dollar=07009_E_burada $ kullanmanız gerekir
% You need to use $<id> here to get the address of the identifier.
asmr_e_cant_have_multiple_relocatable_symbols=07010_E_Birden çok yer değiştirebilen sembol kullanılamaz
% You cannot have more than one relocatable symbol (variable/typed constant)
% in one argument.
asmr_e_only_add_relocatable_symbol=07011_E_Yer değiştirilebilen sembol yalnızca eklenebilir
% Relocatable symbols (variable/typed constant) cannot be used with other
% operators. Only addition is allowed.
asmr_e_invalid_constant_expression=07012_E_Geçersiz sabit ifade
% There is an error in the constant expression.
asmr_e_relocatable_symbol_not_allowed=07013_E_Değiştirilebilir sembole izin verilmiyor
% You cannot use a relocatable symbol (variable/typed constant) here.
asmr_e_invalid_reference_syntax=07014_E_Geçersiz referans sözdizimi
% There is an error in the reference.
asmr_e_local_para_unreachable=07015_E_Bu koddan $1 e ulaşamazsınız
% You cannot read directly the value of a local variable or parameter
% of a higher level procedure in assembler code (except for
% local assembler code without parameter nor locals).
asmr_e_local_label_not_allowed_as_ref=07016_E_Yerel semboller/Label ler referans olarak izin verilmez
% You cannot use local symbols/labels as references
asmr_e_wrong_base_index=07017_E_Geçersiz taban ve dizin kaydı kullanımı
% There is an error with the base and index register, they are
% probably incorrect
asmr_w_possible_object_field_bug=07018_W_Nesne alan işlemesinde olası hata
% Fields of objects or classes can be reached directly in normal or objfpc
% modes but TP and Delphi modes treat the field name as a simple offset.
asmr_e_wrong_scale_factor=07019_E_Yanlış ölçek faktörü belirtildi
% The scale factor given is wrong, only 1,2,4 and 8 are allowed
asmr_e_multiple_index=07020_E_Çoklu indeks kaydı kullanımı
% You are trying to use more than one index register
asmr_e_invalid_operand_type=07021_E_Geçersiz işlenen tür
% The operand type doesn't match with the opcode used
asmr_e_invalid_string_as_opcode_operand=07022_E_Geçersiz string işlenen işlemkodu: $1
% The string specified as operand is not correct with this opcode
asmr_w_CODE_and_DATA_not_supported=07023_W_@CODE ve @DATA desteklenmiyor
% @CODE and @DATA are unsupported and are ignored.
asmr_e_null_label_ref_not_allowed=07024_E_Boş label referanslarına izin verilmiyor
asmr_e_expr_zero_divide=07025_E_Asm değerlendiricisinde sıfıra böl
% There is a division by zero in a constant expression
asmr_e_expr_illegal=07026_E_Geçersiz ifade
% There is an illegal expression in a constant expression
asmr_e_escape_seq_ignored=07027_E_yok sayılan kaçış dizisi: $1
% There is a C-styled string, but the escape sequence in the string
% is unknown, and is therefore ignored
asmr_e_invalid_symbol_ref=07028_E_Geçersiz sembol referansı
asmr_w_fwait_emu_prob=07029_W_Fwait emu387 ile öykünme sorunlarına neden olabilir
asmr_w_fadd_to_faddp=07030_W_$1 olmadan işlenen $1P 'ye çevrildi
asmr_w_enter_not_supported_by_linux=07031_W_ENTER komutu Linux çekirdeği tarafından desteklenmiyor
% ENTER instruction can generate a stack page fault that is not
% caught correctly by the i386 Linux page handler.
asmr_w_calling_overload_func=07032_W_Assembler'da overloda fonksiyonu çağırıldı
% There is a call to an overloaded method in the assembler block,
% this might be the sign there is a problem
asmr_e_unsupported_symbol_type=07033_E_İşlenen için desteklenmeyen sembol türü
asmr_e_constant_out_of_bounds=07034_E_Sabit değer sınırların dışında
asmr_e_error_converting_decimal=07035_E_Decimal dönüştürme hatası $1
% A constant decimal value does not have the correct syntax
asmr_e_error_converting_octal=07036_E_Octal dönüştürme hatası $1
% A constant octal value does not have the correct syntax
asmr_e_error_converting_binary=07037_E_Binary dönüştürme hatası $1
% A constant binary value does not have the correct syntax
asmr_e_error_converting_hexadecimal=07038_E_Hexadecimal dönüştürme hatası $1
% A constant hexadecimal value does not have the correct syntax
asmr_h_direct_global_to_mangled=07039_H_$1 dan çevrilmiş $2
asmr_w_direct_global_is_overloaded_func=07040_W_$1 aşırı yüklenmiş bir fonksiyonla ilişkili
asmr_e_cannot_use_SELF_outside_a_method=07041_E_SELF bir yöntem dışında kullanılamaz
% There is a reference to the \var{self} symbol while it is not
% allowed. \var{self} can only be referenced inside methods
asmr_e_cannot_use_OLDEBP_outside_nested_procedure=07042_E_OLDEBP iç içe geçmiş bir yordamın dışında kullanılamaz
% There is a reference to the \var{oldebp} symbol while it is not
% allowed. \var{oldebp} can only be referenced inside nested routines
asmr_e_void_function=07043_W_Prosedürler asm kodundaki herhangi bir değeri döndüremez
% Trying to return a value while in a procedure. A procedure
% does not have any return value
asmr_e_SEG_not_supported=07044_E_SEG desteklenmiyor
asmr_e_size_suffix_and_dest_dont_match=07045_E_Boyut soneki ve hedef veya kaynak boyutu eşleşmiyor
% The register size and the opcode size suffix don't match. This is
% probably an error in the assembler statement
asmr_w_size_suffix_and_dest_dont_match=07046_W_Boyut soneki ve hedef veya kaynak boyutu eşleşmiyor
% The register size and the opcode size suffix don't match. This is
% probably an error in the assembler statement
asmr_e_syntax_error=07047_E_Assembler sözdizimi hatası
% There is an assembler syntax error
asmr_e_invalid_opcode_and_operand=07048_E_işlem kodu ve işlenenlerde geçersiz kombinasyon
% The opcode cannot be used with this type of operand
asmr_e_syn_operand=07049_E_Assembler işlenende sözdizimi hatası
asmr_e_syn_constant=07050_E_Assembler sabit sözdizimi hatası
asmr_e_invalid_string_expression=07051_E_Geçersiz String ifadesi
asmr_w_const32bit_for_address=07052_W_imleç üzerinde olmayan adres için $1 sembolüyle sabit
% A constant expression represents an address which does not fit
% into a pointer. The address is probably incorrect
asmr_e_unknown_opcode=07053_E_Tanınmayan işlem kodu $1
% This opcode is not known
asmr_e_invalid_or_missing_opcode=07054_E_Geçersiz veya eksik işlem kodu
asmr_e_invalid_prefix_and_opcode=07055_E_Önek ve işlem kodunda geçersiz kombinasyon: $1
asmr_e_invalid_override_and_opcode=07056_E_Override ve işlem kodunda geçersiz kombinasyon: $1
asmr_e_too_many_operands=07057_E_Çok fazla işlenen satır
% There are too many operands for this opcode. Check your
% assembler syntax
asmr_w_near_ignored=07058_W_NEAR yok sayıldı
asmr_w_far_ignored=07059_W_FAR yok sayıldı
asmr_e_dup_local_sym=07060_E_Yenilenen yerel sembol $1
asmr_e_unknown_local_sym=07061_E_Bilinmeyen yerel sembol $1
asmr_e_unknown_label_identifier=07062_E_Bilinmeyen label tanımlayıcısı $1
asmr_e_invalid_register=07063_E_Geçersiz kayıt ismi
% There is an unknown register name used as operand.
asmr_e_invalid_fpu_register=07064_E_Geçersiz float noktası. kayıt ismi+
% There is an unknown register name used as operand.
asmr_w_modulo_not_supported=07066_W_Modulo desteklenmiyor
asmr_e_invalid_float_const=07067_E_Geçersiz float ifadesi $1
% The floating point constant declared in an assembler block is
% invalid.
asmr_e_invalid_float_expr=07068_E_Geçersiz float ifadesi
% The floating point expression declared in an assembler block is
% invalid.
asmr_e_wrong_sym_type=07069_E_Yanlış sembol tipi
asmr_e_cannot_index_relative_var=07070_E_Yerel bir var veya parametre bir register ile indekslenemiyor
% Trying to index using a base register a symbol which is already relative
% to a register. This is not possible, and will probably lead to crashes.
asmr_e_invalid_seg_override=07071_E_Geçersiz segment override ifadesi
asmr_w_id_supposed_external=07072_W_Tanmlayıcı $1 güya harici
% There is a reference to an undefined symbol. This will not result
% in an error, since the symbol might be external, but may cause
% problems at link time if the symbol is not defined anywhere.
asmr_e_string_not_allowed_as_const=07073_E_Strings sabit olarak izin verilmez
% Character strings are not allowed as constants.
asmr_e_no_var_type_specified=07074_E_Değişken türü belirtilmedi
% The syntax expects a type idenfitifer after the dot, but
% none was found.
asmr_w_assembler_code_not_returned_to_text=07075_E_assembler kodu metin bölümüne döndürülmedi
% There was a directive in the assembler block to change sections,
% but there is a missing return to the text section at the end
% of the assembler block. This might cause errors during link time.
asmr_e_not_directive_or_local_symbol=07076_E_Yönerge veya yerel sembol değil $1
% Bu sembol tanımsız.
asmr_w_using_defined_as_local=07077_E_Yerel bir Label tanımlanmış bir isim olarak kullanma
asmr_e_dollar_without_identifier=07078_E_Dolar işareti bir tanımlayıcı olmadan kullanılır
% A constant expression has an identifier which does not start with
% the $ symbol.
asmr_w_32bit_const_for_address=07079_W_Adres için oluşturulan 32 bit sabit
% A constant was used as an address. This is probably an error,
% since using absolute addresses will probably not work.
asmr_n_align_is_target_specific=07080_N_.align  hedefe özeldir, .balign veya .p2align kullanın
% Using the .align directive is platform specific, and its meaning will vary
% from one platform to another.
asmr_e_cannot_access_field_directly_for_parameters=07081_E_Parametreler için alanlara doğrudan erişilemiyor
% You should load the parameter first into a register and then access the
% fields using that register.
asmr_e_cannot_access_object_field_directly=07082_E_objects/classes alanlarına doğrudan erişilemiyor
% You should load the self pointer first into a register and then access the
% fields using the register as base. By default the self pointer is available
% in the esi register on i386.
asmr_e_unable_to_determine_reference_size=07083_E_Boyut belirtilmedi ve işlenenlerin boyutunu belirleyemedi
% You should specify explicitly a size for the reference, because
% the compiler is unable to determine what size (byte,word,dword,etc.) it
% should use for the reference.
asmr_e_cannot_use_RESULT_here=07084_E_RESULT bu fonksiyonda kullanılamaz
% Some functions which return complex types cannot use the \var{result}
% keyword.
asmr_w_adding_explicit_args_fXX=07086_W_"$1" operand olmadan çevrilmiş "$1 %st,%st(1)"
asmr_w_adding_explicit_first_arg_fXX=07087_W_"$1 %st(n)" diline çevrildi "$1 %st,%st(n)"
asmr_w_adding_explicit_second_arg_fXX=07088_W_"$1 %st(n)" diline çevrildi "$1 %st(n),%st"
asmr_e_invalid_char_smaller=07089_E_Char < buraya izin verilmiyor
% The shift operator requires the << characters. Only one
% of those characters was found.
asmr_e_invalid_char_greater=07090_E_Char > buraya izin verilmiyor
% The shift operator requires the >> characters. Only one
% of those characters was found.
asmr_w_align_not_supported=07093_W_ALIGN desteklenmiyor
asmr_e_no_inc_and_dec_together=07094_E_Inc ve Dec birlikte olamaz
% Trying to use an increment and a decrement within the same
% opcode on the 680x0. This is impossible.
asmr_e_invalid_reg_list_in_movem=07095_E_Movem için geçersiz kayıt
% Trying to use the \var{movem} opcode with invalid registers
% to save or restore.
asmr_e_invalid_reg_list_for_opcode=07096_E_Opcode için geçersiz kayıt
asmr_e_higher_cpu_mode_required=07097_E_Daha yüksek cpu modu gerekli ($1)
% Trying to use an instruction which is not supported in the current
% cpu mode. Use a higher cpu generation to be able to use this
% opcode in your assembler block
asmr_w_unable_to_determine_reference_size_using_dword=07098_W_Boyut belirtilmedi ve işlenenlerin boyutunu belirleyemiyor, varsayılan olarak DWORD kullanıyor
% You should specify explicitly a size for the reference, because
% the compiler is unable to determine what size (byte,word,dword,etc.) it
% should use for the reference. This warning is only used in Delphi mode where
% it falls back to use DWORD as default.
asmr_e_illegal_shifterop_syntax=07099_E_Bir kaydırıcı işleneni ayrıştırmaya çalışırken sözdizimi hatası
% ARM only; ARM assembler supports a so called shifter operand. The used syntax isn't
% a valid shifter operand. Example for an operation with shifter operand:
% \begin{verbatim}
% asm
%   orr     r2,r2,r2,lsl #8
% end;
% \end{verbatim}
asmr_e_packed_element=07100_E_Paketlenen bileşenin adresi bayt sınırında değil
% Packed components (record fields and array elements) may start at an arbitrary
% bit inside a byte. On CPU which do not support bit-addressable memory (which
% includes all currently supported CPUs by FPC) you will therefore get an error
% message when trying to index arrays with elements whose size is not a multiple
% of 8 bits. The same goes for accessing record fields with such an address.
% multiple of 8 bits.
asmr_w_unable_to_determine_reference_size_using_byte=07101_W_Boyut belirtilmedi ve işlenenlerin boyutunu belirleyemedi, BYTE'yi varsayılan olarak kullandı
% You should specify explicitly a size for the reference, because
% the compiler is unable to determine what size (byte,word,dword,etc.) it
% should use for the reference. This warning is only used in Delphi mode where
% it falls back to use BYTE as default.
asmr_w_no_direct_ebp_for_parameter=07102_W_Burada geçersiz olan parametreler için + offset (% ebp) kullanımı
% Using direct 8(%ebp) reference for function/procedure parameters is invalid
% if parameters are in registers.
asmr_w_direct_ebp_for_parameter_regcall=07103_W_+Ofset (% ebp) kullanımı, normal kurallarla uyumlu değildir
% Using direct 8(%ebp) reference for function/procedure parameters is invalid
% if parameters are in registers.
asmr_w_direct_ebp_neg_offset=07104_W_Yerel değişken erişimi için -offset (% ebp) kullanımı önerilmez.
% Using -8(%ebp) to access a local variable is not recommended
asmr_w_direct_esp_neg_offset=07105_W_-Offset (% esp) kullanımı, erişim çökmeye neden olabilir veya değer kaybedilebilir
% Using -8(%esp) to access a local stack is not recommended, as
% this stack portion can be overwritten by any function calls or interrupts.
asmr_e_no_vmtoffset_possible=07106_E_VMTOffset sanal bir yöntemle birlikte kullanılmalıdır ve "$1" sanal değil
% Only virtual methods have VMT offsets
asmr_e_need_pic_ref=07107_E_PIC oluşturma, ancak başvuru PIC için güvenli değil
% The compiler has been configured to generate position-independent code
% (PIC), but there are position-dependent references in the current
% handwritten assembler instruction.
asmr_e_mixing_regtypes=07108_E_Bir kayıt kümesindeki tüm kayıtlar aynı tür ve genişlikte olmalıdır
% Instructions on the ARM architecture that take a register set as argument require that all registers
% in this set are of the same kind (e.g., integer, vfp) and width (e.g., single precision, double precision).
asmr_e_empty_regset=07109_E_Bir kayıt seti boş olamaz
% Instructions on the ARM architecture that take a register set as argument require that such a set
% contains at least one register.
asmr_w_useless_got_for_local=07110_W_@GOTPCREL yerel semboller için yararsız ve potansiyel olarak tehlikelidir
% The use of @GOTPCREL supposes an extra indirection that is
% not present if the symbol is local, which might lead to wrong asembler code
asmr_w_general_segment_with_constant=07111_W_Genel amaçlı segment sicili ile sabit
% General purpose register should not have constant offsets
% as OS memory allocation might not be compatible with that.
asmr_e_bad_seh_directive_offset=07112_E_$1 için geçersiz ofset değeri
% Win64 SEH directives have certain restrictions on possible offset values, e.g. they should
% be positive and have 3 or 4 low bits clear.
asmr_e_bad_seh_directive_register=07113_E_$1 için geçersiz kayıt
% Win64 SEH directives accept only 64-bit integer registers or XMM registers.
asmr_e_seh_in_pure_asm_only=07114_E_SEH direktiflerine yalnızca salt assembler prosedürlerinde izin verilir
% Win64 SEH directives are allowed only in pure assembler procedures, not in assembler
% blocks of regular procedures.
asmr_e_unsupported_directive=07115_E_Geçerli hedef için "$1" yönergesi desteklenmiyor
asmr_e_complex_function_result_location=07116_E_"Nostackframe" kullanıldığında, bu fonksiyonun sonuç konumu doğrudan tek bir işleyicide kodlanamaz
% Functions declared with the \var{nostackframe} modifier do not have a stack frame, and hence
% do not have a local variable to hold the temporary function result.  Accesses to the function
% result in such routines directly go to the registers or memory location that will be used
% to return their result to the caller.  In some cases this result may be spread over multiple
% registers, in which case it is not possible to access the location via its symbolic name.
% You have to directly use the appropriate register names in this case.
asmr_e_wrong_gotpcrel_intel_syntax=07117_E_Intel assembler sözdizimindeki GOTPCREL referansları bir taban veya indeks kaydı içeremez ve ofsetleri 0 olmalıdır.
% The syntax for a gotpcrel PIC memory expression in Intel assembler syntax is
% \verb*[global_symbol wrt ..gotpcrel]*
asmr_e_no_gotpcrel_support=07118_E_Mevcut hedef GOTPCREL yer değiştirmesini desteklemiyor
% Not all targets support position-independent code using a global offset table.
% Use a different way to access symbols in a position-indepent way in these cases.
asmr_w_global_access_without_got=07119_W_Dışa aktarılan/genel sembollere GOT üzerinden erişilmelidir
% Global symbols (symbols from the unit interface, or defined in a program
% or library) should be accessed via the GOT when generating position-indepent code.
asmr_w_check_mem_operand_size=07120_W_"$1" bellek işlemcisinin boyutunu kontrol et
% The size of memory operand is possible invalid. This is
% probably an error in the assembler statement
asmr_w_check_mem_operand_size3=07121_W_Bellek işlenenin boyutunu kontrol edin "$1: bellek işleneni boyutu $2 bit, ancak beklenen [$3 bit]
% The size of memory operand is possible invalid. This is
% probably an error in the assembler statement
asmr_w_check_mem_operand_size_offset=07122_W_Bellek işlenenin boyutunu kontrol edin "$1: bellek işleneni boyutu $2 bit, ancak bekleniyor [$3 bit + $3 bayt ofset]
% The size of memory operand is possible invalid. This is
% probably an error in the assembler statement
asmr_w_check_mem_operand_negative_offset=07123_W_"$1: bellek işleneni ofsetinin negatif "$2 bayt" olduğunu kontrol edin
% The offset of memory operand is possible invalid. This is
% probably an error in the assembler statement
asmr_w_check_mem_operand_automap_multiple_size=07124_W_"$1: bellek boyutunun boş olduğunu, ancak bellek boyutunun farklı tanımları olduğunu kontrol edin = >> harita $2 (en küçük seçenek)"
% The size of memory operand is empty and we have different definitions of possible memory sizes. Check it!
asmr_e_invalid_ref_register=07125_E_Bellek referansı ifadesinde geçersiz kayıt kullanılmış: "$1"
% FPU, vector and sometimes integer registers cannot be used in memory reference
% expressions, due to limitations of the cpu architecture or simple because
% it is not meaningful.

#
# Assembler/binary writers
#
# 08026 is the last used one
#
asmw_f_too_many_asm_files=08000_F_Çok fazla assembler dosyası
% With smartlinking enabled, there are too many assembler
% files generated. Disable smartlinking.
asmw_f_assembler_output_not_supported=08001_F_Seçilen assembler çıkışı desteklenmiyor
asmw_f_comp_not_supported=08002_F_Comp desteklenmiyor
asmw_f_direct_not_supported=08003_F_Binary yazarlar için doğrudan destek yok
% Direct assembler mode is not supported for binary writers.
asmw_e_alloc_data_only_in_bss=08004_E_Verilerin tahsisine yalnızca bss bölümünde izin verilir.
asmw_f_no_binary_writer_selected=08005_F_Hiçbir binary yazar seçilmedi
asmw_e_opcode_not_in_table=08006_E_Asm: $1 opcode tablosunda yok
asmw_e_invalid_opcode_and_operands=08007_E_Asm: $1 opcode ve operandların geçersiz kombinasyonu
asmw_e_16bit_not_supported=08008_E_Asm: 16 Bit referansları desteklenmiyor
asmw_e_invalid_effective_address=08009_E_Asm: Geçersiz etkili adres
asmw_e_immediate_or_reference_expected=08010_E_Asm: Hemen veya referans bekleniyor
asmw_e_value_exceeds_bounds=08011_E_Asm: $1 değer sınırları aşıyor $2
asmw_e_short_jmp_out_of_range=08012_E_Asm: Kısa atlama menzil dışında $1
asmw_e_undefined_label=08013_E_Asm: Tanımsız label $1
asmw_e_comp_not_supported=08014_E_Asm: Comp türü bu hedef için desteklenmiyor
asmw_e_extended_not_supported=08015_E_Asm: Extended türü bu hedef için desteklenmiyor
asmw_e_duplicate_label=08016_E_Asm: yenilenen label $1
asmw_e_redefined_label=08017_E_Asm: $1 yeniden tanımlanmış label
asmw_e_first_defined_label=08018_E_Asm: İlk önce burada tanımlandı
asmw_e_invalid_register=08019_E_Asm: $1 geçersiz kayıt
asmw_e_16bit_32bit_not_supported=08020_E_Asm: 16 veya 32 Bit referansları desteklenmiyor
asmw_e_64bit_not_supported=08021_E_Asm: 64 Bit işlenenler desteklenmiyor
asmw_e_bad_reg_with_rex=08022_E_Asm: AH, BH, CH veya DH, REX öneki gerektiren bir talimatta kullanılamaz.
% x86_64 only: instruction encoding of this platform does not allow using
% 8086 high byte registers (AH,BH,CH or DH) together with REX prefix in a single instruction.
% The REX prefix is required whenever the instruction operand size is 64 bits, or
% when it uses one of extended x86_64 registers (R8-R15 or XMM8-XMM15).
asmw_e_missing_endprologue=08023_E_Eksik .seh_endprologue yönergesi
% x86_64-win64 only: Normally, SEH directives are handled internally by compiler.
% However, in pure assembler procedures .seh_endprologue directive is required
% if other SEH directives are present.
asmw_e_prologue_too_large=08024_E_Fonksiyon prolog 255 baytı aşıyor
% x86_64-win64: .seh_prologue directive must be placed within 255 bytes from function start.
asmw_e_handlerdata_no_handler=08025_E_.seh_handlerdata yönergesi önceki .seh_handler yönergesi
% x86_64-win64: If .seh_handlerdata directive is used, then a .seh_handler directive must be
% present earlier in the same function.
asmw_f_too_many_relocations=08026_F_$1 bölümünün yer değiştirme sayısı 65535'i aşıyor
% Legacy COFF targets limit number of relocations per section to 65535 because they use a 2-byte field
% to store the relocation count. Targets using newer PECOFF format do not have this limitation.

#
# Executing linker/assembler
#
# 09035 is the last used one
#
# BeginOfTeX
%
% \section{Errors of assembling/linking stage}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
exec_w_source_os_redefined=09000_W_Kaynak işletim sistemi yeniden tanımlandı
% The source operating system is redefined.
exec_i_assembling_pipe=09001_I_Assembling (pipe) $1
% Assembling using a pipe to an external assembler.
exec_d_cant_create_asmfile=09002_E_Assembler dosyası oluşturulamıyor: $1
% The mentioned file cannot be created. Check if you have
% access permissions to create this file.
exec_e_cant_create_objectfile=09003_E_Nesne dosyası oluşturulamıyor: $1 (hata kodu: $2)
% The mentioned file cannot be created. Check if you have
% got access permissions to create this file.
exec_e_cant_create_archivefile=09004_E_Arşiv dosyası oluşturulamıyor: $1
% The mentioned file cannot be created. Check if you have
% access permissions to create this file.
exec_e_assembler_not_found=09005_E_Assembler $1 bulunamadı, harici Assemblere geçildi
% The assembler program was not found. The compiler will produce a script that
% can be used to assemble and link the program.
exec_t_using_assembler=09006_T_Assembler kullanarak: $1
% An informational message saying which assembler is being used.
exec_e_error_while_assembling=09007_E_$1 çıkış kodu oluşturulurken hata oluştu
% There was an error while assembling the file using an external assembler.
% Consult the documentation of the assembler tool to find out more information
% on this error.
exec_e_cant_call_assembler=09008_E_Assembler çağrılamıyor, $1 harici montaja geçişte hata oluştu
% An error occurred when calling an external assembler. The compiler will produce a script that
% can be used to assemble and link the program.
exec_i_assembling=09009_I_Assembling $1
% An informational message stating which file is being assembled.
exec_i_assembling_smart=09010_I_1 $ smartlink ile Assembling
% An informational message stating which file is being assembled using smartlinking.
exec_w_objfile_not_found=09011_W_$1 nesnesi bulunamadı, Bağlantı başarısız olabilir!
% One of the object files is missing, and linking will probably fail.
% Check your paths.
exec_w_libfile_not_found=09012_W_Kütüphane $1 bulunamadı, Bağlantı başarısız olabilir!
% One of the library files is missing, and linking will probably fail.
% Check your paths.
exec_e_error_while_linking=09013_E_Bağlantı kurulurken hata oluştu
% Generic error while linking.
exec_e_cant_call_linker=09014_E_Linker arayamıyor, harici linke geçiyor
% An error occurred when calling an external linker. The compiler will produce a script that
% can be used to assemble and link the program.
exec_i_linking=09015_I_Bağlanıyor $1
% Hangi program veya kütüphanenin bağlantılı olduğunu gösteren bilgi amaçlı bir mesaj.
exec_e_util_not_found=09016_E_$1 kullanılamaz, harici bağlantıya geçilir
% An external tool was not found. The compiler will produce a script that
% can be used to assemble and link or postprocess the program.
exec_t_using_util=09017_T_Util $1 kullanarak
% An informational message, showing which external program (usually a postprocessor) is being used.
exec_e_exe_not_supported=09018_E_Çalıştırılabilir Dosyaların Oluşturulması Desteklenmiyor
% Creating executable programs is not supported for this platform, because it was
% not yet implemented in the compiler.
exec_e_dll_not_supported=09019_E_Dinamik/Paylaşılan Kütüphanelerin oluşturulması desteklenmiyor
% Creating dynamically loadable libraries is not supported for this platform, because it was
% not yet implemented in the compiler.
exec_e_static_lib_not_supported=09035_E_Statik Kütüphanelerin oluşturulması desteklenmiyor
% Creating static libraries is not supported for this platform, because it was
% not yet implemented in the compiler.
exec_i_closing_script=09020_I_Komut dosyası $1 kapatılıyor
% Informational message showing when writing of the external assembling and linking script is finished.
exec_e_res_not_found=09021_E_"$1" kaynak derleyicisi bulunamadı, harici moda geçiyor
% An external resource compiler was not found. The compiler will produce a script that
% can be used to assemble, compile resources and link or postprocess the program.
exec_i_compilingresource=09022_I_Kaynak $1 derleniyor
% An informational message, showing which resource is being compiled.
exec_t_unit_not_static_linkable_switch_to_smart=09023_T_$1 birimi statik olarak bağlanamıyor, akıllı bağlantıya geçiliyor
% Static linking was requested, but a unit which is not statically linkable was used.
exec_t_unit_not_smart_linkable_switch_to_static=09024_T_$1 birimi akıllı bağlantı oluşturulamıyor, statik bağlantıya geçiyor
% Smart linking was requested, but a unit which is not smart-linkable was used.
exec_t_unit_not_shared_linkable_switch_to_static=09025_T_$1 birimi paylaşılamaz, statik bağlantıya geçilir
% Shared linking was requested, but a unit which is not shared-linkable was used.
exec_e_unit_not_smart_or_static_linkable=09026_E_$1 birimi akıllı veya statik bağlı olamaz
% Smart or static linking was requested, but a unit which cannot be used for either  was used.
exec_e_unit_not_shared_or_static_linkable=09027_E_$1 birimi paylaşılamaz veya statik bağlantı kurulamaz
% Shared or static linking was requested, but a unit which cannot be used for either  was used.
exec_d_resbin_params=09028_D_"$1" kaynak derleyicisini "$2" ile komut satırı olarak çağırmak
% An informational message showing which command line is used for the resource compiler.
exec_e_error_while_compiling_resources=09029_E_Kaynakları derlerken hata
% The resource compiler or converter returned an error.
exec_e_cant_call_resource_compiler=09030_E_Harici moda geçerek kaynak derleyiciyi "$1" arayamazsınız
% An error occurred when calling a resource compiler. The compiler will produce
% a script that can be used to assemble, compile resources and link or
% postprocess the program.
exec_e_cant_open_resource_file=09031_E_"$1" kaynak dosyası açılamıyor
% An error occurred resource file cannot be opened.
exec_e_cant_write_resource_file=09032_E_"$1" kaynak dosyası yazılamıyor
% An error occurred resource file cannot be written.
exec_n_backquote_cat_file_not_found=09033_N_Dosya "$1" geri alıntılanan cat komutu için bulunamadı
% The compiler did not find the file that should be expanded into linker parameters
exec_w_init_file_not_found=09034_W_"$1" bulunamadı, bu muhtemelen bir bağlantı hatasına neden olacaktır.
% The compiler adds certain startup code files to the linker only when they are found.
% If they are not found, they are not added and this might cause a linking failure.
%
%\end{description}
# EndOfTeX

#
# Executable information
#
# 09134 is the last used one
#
# BeginOfTeX
% \section{Executable information messages.}
% This section lists all messages that the compiler emits when an executable program is produced,
% and only when the internal linker is used.
% \begin{description}
execinfo_f_cant_process_executable=09128_F_Çalıştırılabilir işlem gönderilemiyor $1
% Fatal error when the compiler is unable to post-process an executable.
execinfo_f_cant_open_executable=09129_F_$1 çalıştırılabilir dosyasını açamılamıyor
% Fatal error when the compiler cannot open the file for the executable.
execinfo_x_codesize=09130_X_Kod Boyutu: $1 bayt
% Informational message showing the size of the produced code section.
execinfo_x_initdatasize=09131_X_Başlatılan verinin boyutu: $1 bayt
% Informational message showing the size of the initialized data section.
execinfo_x_uninitdatasize=09132_X_Başlatılmamış verilerin boyutu: $1 bayt
% Informational message showing the size of the uninitialized data section.
execinfo_x_stackreserve=09133_X_Yığın alanı ayrıldı: $1 bayt
% Informational message showing the stack size that the compiler reserved for the executable.
execinfo_x_stackcommit=09134_X_İşlenen yığın alanı: $1 bayt
% Informational message showing the stack size that the compiler committed for the executable.
%\end{description}
# EndOfTeX

#
# Internal linker messages
#
# 09200 is the last used one
#
# BeginOfTeX
% \section{Linker messages}
% This section lists messages produced by internal linker.
% \begin{description}
link_f_executable_too_big=09200_F_Yürütülebilir resim boyutu $1 hedef için çok büyük.
% Fatal error when resulting executable is too big.
link_w_32bit_absolute_reloc=09201_W_"$1" nesne dosyası, "$2" sembolüne 32-bit mutlak yer değiştirme içerir.
% Warning when 64-bit object file contains 32-bit absolute relocations.
% In such case an executable image can be loaded into lower 4Gb of
% address space only.
%\end{description}
# EndOfTeX

#
# Unit loading
#
# 10062 is the last used one
#
# BeginOfTeX
% \section{Unit loading messages.}
% This section lists all messages that can occur when the compiler is
% loading a unit from disk into memory. Many of these messages are
% informational messages.
% \begin{description}
unit_t_unitsearch=10000_T_Unitearama: $1
% When you use the \var{-vt} option, the compiler tells you where it tries to find
% unit files.
unit_t_ppu_loading=10001_T_PPU yükleniyor $1
% When the \var{-vt} switch is used, the compiler tells you
% what units it loads.
unit_u_ppu_name=10002_U_PPU Adı: $1
% When you use the \var{-vu} flag, the unit name is shown.
unit_u_ppu_flags=10003_U_PPU bayrağı: $1
% When you use the \var{-vu} flag, the unit flags are shown.
unit_u_ppu_crc=10004_U_PPU Crc: $1
% When you use the \var{-vu} flag, the unit CRC check is shown.
unit_u_ppu_time=10005_U_PPU Zamanı: $1
% When you use the \var{-vu} flag, the time the unit was compiled is shown.
unit_u_ppu_file_too_short=10006_U_PPU Dosya çok kısa
% The ppufile is too short, not all declarations are present.
unit_u_ppu_invalid_header=10007_U_PPU Geçersiz Başlık (PPU da begin yok)
% A unit file contains as the first three bytes the ASCII codes of the characters \var{PPU}.
unit_u_ppu_invalid_version=10008_U_PPU Geçersiz Versiyon $1
% This unit file was compiled with a different version of the compiler, and
% cannot be read.
unit_u_ppu_invalid_processor=10009_U_PPU başka bir işlemci için derlendi
% This unit file was compiled for a different processor type, and
% cannot be read.
unit_u_ppu_invalid_target=10010_U_PPU başka bir hedef için derlendi
% This unit file was compiled for a different target, and
% cannot be read.
unit_u_ppu_source=10011_U_PPU Kaynağı: $1
% When you use the \var{-vu} flag, the unit source file name is shown.
unit_u_ppu_write=10012_U_Yazılıyor $1
% When you specify the \var{-vu} switch, the compiler will tell you where it
% writes the unit file.
unit_f_ppu_cannot_write=10013_F_PPU-Dosyası yazılamıyor
% An error occurred when writing the unit file.
unit_f_ppu_read_error=10014_F_PPU-Dosyası Okunamıyor
% This means that the unit file was corrupted, and contains invalid
% information. Recompilation will be necessary.
unit_f_ppu_read_unexpected_end=10015_F_PPU-Dosyasında Beklenmeyen end
% Beklenmedik dosya sonu. Bu, PPU dosyasının bozuk olduğu anlamına gelebilir.
unit_f_ppu_invalid_entry=10016_F_Geçersiz PPU-Dosya girişi: $1
% The unit the compiler is trying to read is corrupted, or generated with a
% newer version of the compiler.
unit_f_ppu_dbx_count_problem=10017_F_PPU DBX sayım sorunu
% There is an inconsistency in the debugging information of the unit.
unit_e_illegal_unit_name=10018_E_Illegal unite adı: $1
% The name of the unit does not match the file name.
unit_f_too_much_units=10019_F_Çok fazla unitler
% \fpc has a limit of 1024 units in a program. You can change this behavior
% by changing the \var{maxunits} constant in the \file{fmodule.pas} file of the
% compiler, and recompiling the compiler.
unit_f_circular_unit_reference=10020_F_$1 ile $2 arasında dairesel birim referansı
% Two units are using each other in the interface part. This is only allowed
% in the \var{implementation} part. At least one unit must contain the other one
% in the \var{implementation} section.
unit_f_cant_compile_unit=10021_F_Birim $1 derlenemiyor, kaynak yok
% A unit was found that needs to be recompiled, but no sources are
% available.
unit_f_cant_find_ppu=10022_F_$2 tarafından kullanılan $1 birimini bulamıyor
% You tried to use a unit of which the PPU file isn't found by the
% compiler. Check your configuration file for the unit paths.
unit_w_unit_name_error=10023_W_$1 Birimi bulunamadı, ancak $2 var
% This error message is no longer used.
unit_f_unit_name_error=10024_F_$1 birimi arandı, ancak $2 bulundu
% \dos truncation of 8 letters for unit PPU files
% may lead to problems when unit name is longer than 8 letters.
unit_w_switch_us_missed=10025_W_Sistem biriminin derlenmesi -Us anahtarını gerektirir
% When recompiling the system unit (it needs special treatment), the
% \var{-Us} switch must be specified.
unit_f_errors_in_unit=10026_F_Modül derlenirken $1 hatası oluştu, durdu
% When the compiler encounters a fatal error or too many errors in a module
% then it stops with this message.
unit_u_load_unit=10027_U_$1 ($2) biriminden $3 yükle
% When you use the \var{-vu} flag, which unit is loaded from which unit is
% shown.
unit_u_recompile_crc_change=10028_U_$1 yeniden derlenirken, sağlama toplamı $2 olarak değiştirildi
% The unit is recompiled because the checksum of a unit it depends on has
% changed.
unit_u_recompile_source_found_alone=10029_U_$1 yeniden derleniyor, yalnızca kaynak bulundu
% When you use the \var{-vu} flag, these messages tell you why the current
% unit is recompiled.
unit_u_recompile_staticlib_is_older=10030_U_Yeniden derleme uniti, statik lib ppufile'den daha eski
% When you use the \var{-vu} flag, the compiler warns if the static library
% of the unit is older than the unit file itself.
unit_u_recompile_sharedlib_is_older=10031_U_Yeniden derleme uniti, paylaşılan lib, ppufile'den daha eski
% When you use the \var{-vu} flag, the compiler warns if the shared library
% of the unit is older than the unit file itself.
unit_u_recompile_obj_and_asm_older=10032_U_Yeniden derleme uniti, obj ve asm ppufile'den daha eski
% When you use the \var{-vu} flag, the compiler warns if the assembler or
% object file of the unit is older than the unit file itself.
unit_u_recompile_obj_older_than_asm=10033_U_Yeniden derleme uniti, obj asm daha eski
% When you use the \var{-vu} flag, the compiler warns if the assembler
% file of the unit is older than the object file of the unit.
unit_u_parsing_interface=10034_U_$1 interface ayrıştırılıyor
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the interface part of the unit.
unit_u_parsing_implementation=10035_U_$1 implementation ayrıştırılıyor
% When you use the \var{-vu} flag, the compiler warns that it starts
% parsing the implementation part of the unit.
unit_u_second_load_unit=10036_U_$1 ünite için ikinci yükleme
% When you use the \var{-vu} flag, the compiler warns that it starts
% recompiling a unit for the second time. This can happen with
% interdependent units.
unit_u_check_time=10037_U_PPU dosyası kontrolü $1 zaman $2
% When you use the \var{-vu} flag, the compiler shows the filename and
% date and time of the file on which a recompile depends.
### The following two error msgs is currently disabled.
#unit_h_cond_not_set_in_last_compile=10038_H_$2 son derlemede koşullu $1 başlangıçta belirlenmedi
#% when recompilation of an unit is required the compiler will check that
#% the same conditionals are set for the recompiliation. The compiler has
#% found a conditional that currently is defined, but was not used the last
#% time the unit was compiled.
#unit_h_cond_set_in_last_compile=10039_H_Koşullu $1, başlangıçta $2 olan son derlemede ayarlandı
#% when recompilation of an unit is required the compiler will check that
#% the same conditionals are set for the recompiliation. The compiler has
#% found a conditional that was used the last time the unit was compiled, but
#% the conditional is currently not defined.
unit_w_cant_compile_unit_with_changed_incfile=10040_W_$1 birimi derlenemiyor, ancak değiştirilmiş dosyalar içeriyor
% A unit was found to have modified include files, but
% some source files were not found, so recompilation is impossible.
unit_u_source_modified=10041_U_$1 dosyası $2 PPU dosyası oluşturmak için kullanılandan daha yeni
% A modified source file for a compiler unit was found.
unit_u_ppu_invalid_fpumode=10042_U_Farklı bir FPU modu ile derlenmiş bir birimi kullanmaya çalışıyorsunuz
% Trying to compile code while using units which were not compiled with
% the same floating point format mode. Either all code should be compiled
% with FPU emulation on, or with FPU emulation off.
unit_u_loading_interface_units=10043_U_interface unitleri $1'den yükleniyor
% When you use the \var{-vu} flag, the compiler warns that it is starting
% to load the units defined in the interface part of the unit.
unit_u_loading_implementation_units=10044_U_implementation unitleri $1'den yükleniyor
% When you use the \var{-vu} flag, the compiler warns that it is starting
% to load the units defined in the implementation part of the unit.
unit_u_interface_crc_changed=10045_U_Interface CRC $1 unitleri için değiştirildi
% When you use the \var{-vu} flag, the compiler warns that the
% CRC calculated for the interface has been changed after the implementation
% has been parsed.
unit_u_implementation_crc_changed=10046_U_Implementation CRC $1 unitleri için değiştirildi
% When you use the \var{-vu} flag, the compiler warns that the
% CRC calculated has been changed after the implementation
% has been parsed.
unit_u_finished_compiling=10047_U_$1 birimi derlemesi tamamlandı
% When you use the \var{-vu} flag, the compiler warns that it
% has finished compiling the unit.
unit_u_add_depend_to=10048_U_Bağımlılık ekleme: $1, $2’ye bağlıdır
% When you use the \var{-vu} flag, the compiler warns that it
% has added a dependency between the two units.
unit_u_no_reload_is_caller=10049_U_Yeniden yükleme yok, arayan: $1
% When you use the \var{-vu} flag, the compiler warns that it
% will not reload the unit because it is the unit that wants
% to load this unit.
unit_u_no_reload_in_second_compile=10050_U_Yeniden yükleme yok, zaten ikinci derlemede: $1
% When you use the \var{-vu} flag, the compiler warns that it
% will not reload the unit because it is already in a second recompile.
unit_u_flag_for_reload=10051_U_Yeniden yüklemek için bayrak: $1
% When you use the \var{-vu} flag, the compiler warns that it
% has to reload the unit.
unit_u_forced_reload=10052_U_Zorla yeniden yükleniyor
% When you use the \var{-vu} flag, the compiler warns that it
% is reloading the unit because it was required.
unit_u_previous_state=10053_U_Önceki durumu :$1 dan $2
% When you use the \var{-vu} flag, the compiler shows the
% previous state of the unit.
unit_u_second_compile_unit=10054_U_$1 Zaten derleniyor, ikinci derleme ayarı
% When you use the \var{-vu} flag, the compiler warns that it is starting
% to recompile a unit for the second time. This can happen with interdependent
% units.
unit_u_loading_unit=10055_U_$1 Birimi yükleniyor
% When you use the \var{-vu} flag, the compiler warns that it starts
% loading the unit.
unit_u_finished_loading_unit=10056_U_$1 Birimi yüklenmesi tamamlandı
% When you use the \var{-vu} flag, the compiler warns that it finished
% loading the unit.
unit_u_registering_new_unit=10057_U_$1 Yeni birim kaydediliyor
% When you use the \var{-vu} flag, the compiler warns that it has
% found a new unit and is registering it in the internal lists.
unit_u_reresolving_unit=10058_U_$1 Birimi yeniden çözümleniyor
% When you use the \var{-vu} flag, the compiler warns that it
% has to recalculate the internal data of the unit.
unit_u_skipping_reresolving_unit=10059_U_$1 yeniden çözme birimi atlanıyor, hala kullanılan birimler yükleniyor
% When you use the \var{-vu} flag, the compiler warns that it is
% skipping the recalculation of the internal data of the unit
% because there is no data to recalculate.
unit_u_unload_resunit=10060_U_Kaynak birimi $1 çıkartılıyor (gerekli değil)
% When you use the \var{-vu} flag, the compiler warns that it is unloading the
% resource handling unit, since no resources are used.
unit_e_different_wpo_file=10061_E_$1 birimi farklı bir bütün program optimizasyon geri besleme girişi ($2, $3) kullanılarak derlendi; wpo olmadan yeniden derleyin veya bu derleme çağrısı için aynı wpo geribildirim giriş dosyasını kullanın
% When a unit has been compiled using a particular whole program optimization (wpo) feedback file (\var{-FW<x>} \var{-OW<x>}),
% this compiled version of the unit is specialised for that particular compilation scenario and cannot be used in
% any other context. It has to be recompiled before you can use it in another program or with another wpo feedback input file.
unit_u_indirect_crc_changed=10062_U_Dolaylı interface (objects/classes) CRC, birim $1 olarak değiştirildi
% When you use the \var{-vu} flag, the compiler warns that the
% indirect CRC calculated for the unit (this is the CRC of all classes/objects/interfaces/$\ldots$
% in the interfaces of units directly or indirectly used by this unit in the interface) has been changed after the
% implementation has been parsed.
unit_u_ppu_invalid_memory_model=10063_U_PPU, başka bir i8086 bellek modeli için derlendi
% This unit file was compiled for a different i8086 memory model and
% cannot be read.
% \end{description}
# EndOfTeX

#
#  Options
#
# 11057 is the last used one
#
option_usage=11000_O_$1 [options] <inputfile> [options]
# BeginOfTeX
%
% \section{Command line handling errors}
% This section lists errors that occur when the compiler is processing the
% command line or handling the configuration files.
% \begin{description}
option_only_one_source_support=11001_W_Yalnızca bir kaynak dosya desteklenir, kaynak dosya "$1" den "$2" ye derlenir
% You can specify only one source file on the command line. The last
% one will be compiled, others will be ignored. This may indicate that
% you forgot a \var{'-'} sign.
option_def_only_for_os2=11002_W_DEF dosyası sadece OS/2 için oluşturulabilir
% This option can only be specified when you're compiling for OS/2.
option_no_nested_response_file=11003_E_iç içe yanıt dosyaları desteklenmiyor
% You cannot nest response files with the \var{@file} command line option.
option_no_source_found=11004_F_Komut satırında kaynak dosya adı yok
% The compiler expects a source file name on the command line.
option_no_option_found=11005_N_$1 yapılandırma dosyasında seçenek yok
% The compiler didn't find any option in that config file.
option_illegal_para=11006_E_Illegal parametre: $1
% You specified an unknown option.
option_help_pages_para=11007_H_-? yardım sayfaları yazar
% When an unknown option is given, this message is diplayed.
option_too_many_cfg_files=11008_F_Çok fazla yapılandırma dosyası iç içe geçmiş
% You can only nest up to 16 config files.
option_unable_open_file=11009_F_$1 dosyası açılamıyor
% The option file cannot be found.
option_reading_further_from=11010_D_$1 'dan daha fazla seçenek okunuyor
% Displayed when you have notes turned on, and the compiler switches
% to another options file.
option_target_is_already_set=11011_W_Hedef zaten ayarlanmış: $1
% Displayed if more than one \var{-T} option is specified.
option_no_shared_lib_under_dos=11012_W_Paylaşılan lib'ler DOS platformunda desteklenmiyor, statik değere dönüyor
% If you specify \var{-CD} for the \dos platform, this message is displayed.
% The compiler supports only static libraries under \dos.
option_too_many_ifdef=11013_F_Seçenekler dosyasında $1 satırındaki $2 çok fazla #IF(N)DEF ile karşılaştı
% The \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_many_endif=11014_F_Seçenekler dosyasında $1 satırındaki $2 beklenmeyen #ENDIF ile karşılaştı
% The \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_too_less_endif=11015_F_Seçenekler dosyasının sonunu koşullu açın
% The \var{\#IF(N)DEF} statements in the options file are not balanced with
% the \var{\#ENDIF} statements.
option_no_debug_support=11016_W_Hata ayıklama bilgisi üretimi bu yürütülebilir dosya tarafından desteklenmiyor
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_no_debug_support_recompile_fpc=11017_H_-dGDB ile yeniden derlemeyi deneyin
% It is possible to have a compiler executable that doesn't support
% the generation of debugging info. If you use such an executable with the
% \var{-g} switch, this warning will be displayed.
option_obsolete_switch=11018_W_Eski anahtarı $1 kullanıyorsunuz
% This warns you when you use a switch that is not needed/supported anymore.
% It is recommended that you remove the switch to overcome problems in the
% future, when the meaning of the switch may change.
option_obsolete_switch_use_new=11019_W_$1 eski anahtarını kullanıyorsanız, lütfen $2 kullanın
% This warns you when you use a switch that is not supported anymore. You
% must now use the second switch instead.
% It is recommended that you change the switch to overcome problems in the
% future, when the meaning of the switch may change.
option_switch_bin_to_src_assembler=11020_N_Assembler anahtarlama varsayılan kaynak yazma
% This notifies you that the assembler has been changed because you used the
% -a switch, which cannot be used with a binary assembler writer.
option_incompatible_asm=11021_W_Assembler "$1" seçili çıktısı "$2" ile uyumlu değil
option_asm_forced=11022_W_"$1" assembler zorla kullanımı
% The assembler output selected cannot generate
% object files with the correct format. Therefore, the
% default assembler for this target is used instead.
option_using_file=11026_T_$1 dosyasından okuma seçenekleri
% Options are also read from this file.
option_using_env=11027_T_Çevreden okuma seçenekleri $1
% Options are also read from this environment string.
option_handling_option=11028_D_Taşıma seçeneği "$1"
% Debug info that an option is found and will be handled.
option_help_press_enter=11029_O_*** enter'a bas ***
% Message shown when help is shown page per page. When pressing the ENTER
% Key, the next page of help is shown. If you press q and then ENTER, the
% compiler exits.
option_start_reading_configfile=11030_H_$1 yapılandırma dosyasını okumaya başla
% Start of configuration file parsing.
option_end_reading_configfile=11031_H_$1 yapılandırma dosyasını okumayı sonlandır
% End of configuration file parsing.
option_interpreting_option=11032_D_çeviri seçeneği "$1"
% The compiler is interpreting an option
option_interpreting_firstpass_option=11036_D_"$1" firstpass seçeneğinin yorumlanması
% The compiler is interpreting an option for the first time.
option_interpreting_file_option=11033_D_"$1" dosya seçeneğinin yorumlanması
% The compiler is interpreting an option which it read from the configuration file.
option_read_config_file=11034_D_"$1" yapılandırma dosyası okunuyor
% The compiler is starting to read the configuration file.
option_found_file=11035_D_kaynak dosya adı bulundu "$1"
% Additional information about options.
% Displayed when you have the debug option turned on.
option_code_page_not_available=11039_E_Bilinmeyen kod sayfası "$1"
% An unknown codepage for the source files was requested.
% The compiler is compiled with support for several codepages built-in.
% The requested codepage is not in that list. You will need to recompile
% the compiler with support for the codepage you need.
option_config_is_dir=11040_F_$1 yapılandırma dosyası bir dizindir
% Directories cannot be used as configuration files.
option_confict_asm_debug=11041_W_"$1" seçilmiş Assembler çıkışı hata ayıklama bilgisi üretemiyor, hata ayıklama devre dışı
% The selected assembler output cannot generate
% debugging information, debugging option is therefore disabled.
option_ppc386_deprecated=11042_W_Ppc386.cfg kullanımı kullanımdan kaldırılmıştır, lütfen yerine fpc.cfg kullanın
% Using ppc386.cfg is still supported for historical reasons, however, for a multiplatform
% system the naming makes no sense anymore. Please continue to use fpc.cfg instead.
option_else_without_if=11043_F_Seçenekler $2 dosyasında $1 satırındaki #ELSE yönergesinde #IF(N)DEF bulunamadı
% An \var{\#ELSE} statement was found in the options file without a matching \var{\#IF(N)DEF} statement.
option_unsupported_target=11044_F_"$1" seçeneği mevcut hedef platformda desteklenmiyor veya henüz desteklenmiyor
% Not all options are supported or implemented for all target platforms. This message informs you that a chosen
% option is incompatible with the currently selected target platform.
option_unsupported_target_for_feature=11045_F_"$1" özelliği seçilen hedef platformda desteklenmiyor veya henüz desteklenmiyor.
% Not all features are supported or implemented for all target platforms. This message informs you that a chosen
% feature is incompatible with the currently selected target platform.
option_dwarf_smart_linking=11046_N_DWARF hata ayıklama bilgisi bu hedefe akıllı bağlantı ile kullanılamaz, statik bağlantıya geçilir
% Smart linking is currently incompatble with DWARF debug information on most
% platforms, so smart linking is disabled in such cases.
option_ignored_target=11047_W_Mevcut hedef platform için "$1" seçeneği dikkate alınmaz.
% Not all options are supported or implemented for all target platforms. This message informs you that a chosen
% option is ignored for the currently selected target platform.
option_debug_external_unsupported=11048_W_Harici hata ayıklama bilgilerini devre dışı bırakma çünkü seçilen hedef/hata ayıklama biçimi birleşimi için desteklenmiyor.
% Not all debug formats can be stored in an external file on all platforms. In particular, on
% Mac OS X only DWARF debug information can be stored externally.
option_dwarf_smartlink_creation=11049_N_DWARF hata ayıklama bilgisi, statik derleme oluşturma işlemini engelleyerek harici assembler ile akıllı bağlantı kurarak kullanılamaz.
% Smart linking is currently incompatble with DWARF debug information on most
% platforms, so smart linking is disabled in such cases.
option_invalid_macosx_deployment_target=11050_E_MACOSX_DEPLOYMENT_TARGET ortam değişkeni için geçersiz değer: $1
option_invalid_iphoneos_deployment_target=11051_E_IPHONEOS_DEPLOYMENT_TARGET ortam değişkeni için geçersiz değer: $1
% On Mac OS X, the MACOSX\_DEPLOYMENT\_TARGET/IPHONEOS\_DEPLOYMENT\_TARGET environment variable can be
% used to set the default target OS version. In case of Mac OS X, it has to be of the format
% XY.Z or XY.Z.AB with X, Y,Z , A and B all digits from 0-9.
% In case of iOS, it has to be X.Z.A, where X, Z and A can all be either 1 or 2
% digits from 0-9.
option_illegal_fpu_eabihf=11052_E_EABIHF ABI hedefini kullanırken FPU tipinde VFPV2, VFPV3 veya VFPV3_D16 kullanmalısınız
% The EABIHF (VFP hardfloat) ABI target can only be used with VFP FPUs.
option_w_unsupported_debug_format=11053_W_Seçilen hata ayıklama formatı, geçerli ayarda değiştirilmeyip, geçerli hedefte desteklenmiyor
% Not all targets support all debug formats (in particular, Stabs is not supported on 64 bit targets).
option_missing_arg=11054_E_"$1" argümanı eksik
% Displayed when parameter must be followed by an argument.
option_malformed_para=11055_E_Hatalı biçimlendirilmiş parametre: $1
% Given argument is not valid for parameter.
option_smart_link_requires_external_linker=11056_W_Akıllı bağlantı harici bağlayıcı gerektirir
option_com_files_require_tiny_model=11057_E_Geçerli bellek modelinde .COM dosyaları oluşturmak desteklenmez. Yalnızca küçük bellek modeli .COM dosyalarını oluşturmayı destekler.


%\end{description}
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
wpo_cant_find_file=12000_F_"$1" program optimizasyon geri bildirim dosyası açılamıyor
% The compiler cannot open the specified feedback file with whole program optimization information.
wpo_begin_processing=12001_D_Tüm program optimizasyon bilgilerinin "$1" wpo geri bildirim dosyasında işlenmesi
% The compiler starts processing whole program optimization information found in the named file.
wpo_end_processing=12002_D_Tüm program optimizasyon bilgilerini "$1" wpo geri bildirim dosyasında işlemeyi tamamladı
% The compiler has finished processing the whole program optimization information found in the named file.
wpo_expected_section=12003_E_Beklenen bölüm başlığı, ancak wpo geri bildirim "$2" dosyasının $1 satırında
% The compiler expected a section header in the whole program optimization file (starting with \%),
% but did not find it.
wpo_no_section_handler=12004_W_Tüm program optimizasyon bölümü "$2" için wpo geri bildirim dosyasının $1 satırında kayıtlı hiçbir işleyici yok, yok sayılıyor
% The compiler has no handler to deal with the mentioned whole program optimization information
% section, and will therefore ignore it and skip to the next section.
wpo_found_section=12005_D_"$2" hakkında bilgi içeren tüm program optimizasyon bölümünde "$1" bulundu
% The compiler encountered a section with whole program optimization information, and according
% to its handler this section contains information usable for the mentioned purpose.
wpo_no_input_specified=12006_F_Seçilen tüm program optimizasyonları önceden oluşturulmuş bir geribildirim dosyası gerektirir (belirtmek için -Fw kullanın)
% The compiler needs information gathered during a previous compilation run to perform the selected
% whole program optimizations. You can specify the location of the feedback file containing this
% information using the -Fw switch.
wpo_not_enough_info=12007_E_"$1" bütün program optimizasyonunu gerçekleştirmek için toplanan bilgi gerekmez
% While you pointed the compiler to a file containing whole program optimization feedback, it
% did not contain the information necessary to perform the selected optimizations. You most likely
% have to recompile the program using the appropate -OWxxx switch.
wpo_no_output_specified=12008_F_Üretilen bilgiyi (-FW kullanarak) saklamak için bir bütün program optimizasyon geri bildirim dosyası belirtin
% You have to specify the feedback file in which the compiler has to store the whole program optimization
% feedback that is generated during the compilation run. This can be done using the -FW switch.
wpo_output_without_info_gen=12009_E_Herhangi bir program optimizasyon bilgisinin tamamı oluşturulmadı, ancak bir geri bildirim dosyası belirlendi (-FW kullanarak)
% The compiler was instructed to store whole program optimization feedback into a file specified using -FW,
% but not to actually generated any whole program optimization feedback. The classes of to be
% generated information can be speciied using -OWxxx.
wpo_input_without_info_use=12010_E_Tüm program optimizasyonlarını yapmamakla birlikte, bir giriş geri besleme dosyası belirtildi (-Fw kullanarak)
% The compiler was not instructed to perform any whole program optimizations (no -Owxxx parameters),
% but nevertheless an input file with such feedback was specified (using -Fwyyy). Since this can
% indicate that you forgot to specify an -Owxxx parameter, the compiler generates an error in this case.
wpo_skipping_unnecessary_section=12011_D_İstenen optimizasyonlar için gerekli olmadığından tüm program optimizasyon bölümünü "$1" atlama
% The whole program optimization feedback file contains a section with information that is not
% required by the selected whole program optimizations.
wpo_duplicate_wpotype=12012_W_"$2" bölümündeki bilgileri kullanarak geri bildirim giriş dosyasındaki "$1" bilgisini geçersiz kılma
% The feedback file contains multiple sections that provide the same class of information (e.g.,
% information about which virtual methods can be devirtualized). In this case, the information in last encountered
% section is used. Turn on debugging output (-vd) to see which class of information is provided by each section.
wpo_cannot_extract_live_symbol_info_strip=12013_E_Sembol soyunurken sembol canlılığı bilgisi programdan çıkarılamaz, -Xs- kullanın
% Certain symbol liveness collectors extract the symbol information from the linked program. If the symbol information
% is stripped (option -Xs), this is not possible.
wpo_cannot_extract_live_symbol_info_no_link=12014_E_Programdan sembol canlılığı bilgisi alınamıyor, Bağlanamıyor 
% Certain symbol liveness collectors extract the symbol information from the linked program. If the program is not
% linked by the compiler, this is not possible.
wpo_cannot_find_symbol_progs=12015_F_Bağlantılı programdan sembol canlılığı bilgilerini çıkarmak için "$1" veya "$$" bulunamıyor
% Certain symbol liveness collectors need a helper program to extract the symbol information from the linked program.
% This helper program is normally 'nm', which is part of the GNU binutils.
wpo_error_reading_symbol_file=12016_E_"$1" tarafından üretilen sembol canlılığı bilgileri okunurken hata oluştu
% An error occurred during the reading of the symbol liveness file that was generated using the 'nm' or 'objdump' program. The reason
% can be that it was shorter than expected, or that its format was not understood.
wpo_error_executing_symbol_prog=12017_F_Bağlantılı programdan sembol bilgilerini almak için "$1" (çıkış kodu: $2) çalıştırma hatası
% Certain symbol liveness collectors need a helper program to extract the symbol information from the linked program.
% The helper program produced the reported error code when it was run on the linked program.
wpo_symbol_live_info_needs_smart_linking=12018_E_Sembol canlılığı bilgilerinin toplanması yalnızca akıllı bağlantı kullanırken yardımcı olabilir, -CX -XX kullanın
% Bir sembolün canlı olup olmadığı, nihai bağlantılı programda bulunup bulunmadığına bakarak belirlenir.
% Akıllı bağlantı / ölü kod soyma işlemi olmadan, tüm simgeler, gerçekte kullanılıp kullanılmadığına bakılmaksızın her zaman dahil edilir.
% Bu durumda, tüm semboller canlı olarak görülecektir, bu da optimizasyonu etkisiz hale getirir.
wpo_cant_create_feedback_file=12019_E_"$1" belirtilen tüm program optimizasyon geribildirim dosyası oluşturulamıyor
% Derleyici, tüm program optimizasyon bilgisini saklamak için -FW parametresini kullanarak belirtilen dosyayı oluşturamıyor.
%\end{description}
# EndOfTeX


#
# Logo (option -l)
#
option_logo=11023_[
Free Pascal Compiler version $FPCFULLVERSION [$FPCDATE] for $FPCCPU
Copyright (c) 1993-2024 by Florian Klaempfl and others
]

#
# Info (option -i)
#
option_info=11024_[
Free Pascal Compiler version $FPCVERSION

Compiler date      : $FPCDATE
Compiler CPU target: $FPCCPU

Supported targets (targets marked with '{*}' are under development):
  $OSTARGETS

Supported CPU instruction sets:
  $INSTRUCTIONSETS

Supported FPU instruction sets:
  $FPUINSTRUCTIONSETS

Supported inline assembler modes:
  $ASMMODES

Recognized compiler and RTL features:
  $FEATURELIST

Supported ABI targets:
  $ABITARGETS

Supported Optimizations:
  $OPTIMIZATIONS

Supported Whole Program Optimizations:
  All
  $WPOPTIMIZATIONS

Supported Microcontroller types:$\n  $CONTROLLERTYPES$\n
This program comes under the GNU General Public Licence
For more information read COPYING.v2

Please report bugs in our bug tracker on:
                 http://bugs.freepascal.org

More information may be found on our WWW pages (including directions
for mailing lists useful for asking questions or discussing potential
new features, etc.):
                 http://www.freepascal.org
]

#
# Help pages (option -? and -h)
#
# The first character on the line indicates who will display this
# line, the current possibilities are :
#    * = every target
#    3 = 80x86 targets
#    4 = x86_64
#    6 = 680x0 targets
#    8 = 8086 (16-bit) targets
#    A = ARM
#    e = in extended debug mode only
#    F = help for the 'fpc' binary (independent of the target compiler)
#    I = VIS
#    J = JVM
#    M = MIPS (MIPSEB) targets
#    m = MIPSEL targets
#    P = PowerPC targets
#    p = PowerPC64 targets
#    S = Sparc targets
#    V = AVR
# The second character also indicates who will display this line,
# (if the above character was TRUE) the current possibilities are :
#    * = everyone
#    g = with GDB info supported by the compiler
#    O = OS/2
#    L = UNIX systems
#  The third character represents the indentation level.
#
option_help_pages=11025_[
F*0*_Only options valid for the default or selected platform are listed.
**0*_Put + after a boolean switch option to enable it, - to disable it.
**1@<x>_Read compiler options from <x> in addition to the default fpc.cfg
**1a_The compiler does not delete the generated assembler file
**2al_List sourcecode lines in assembler file
**2an_List node info in assembler file (-dEXTDEBUG compiler)
**2ao_Add an extra option to external assembler call (ignored for internal)
*L2ap_Use pipes instead of creating temporary assembler files
**2ar_List register allocation/release info in assembler file
**2at_List temp allocation/release info in assembler file
**1A<x>_Output format:
**2Adefault_Use default assembler
3*2Aas_Assemble using GNU AS
3*2Amacho_Mach-O (Darwin, Intel 32 bit) using internal writer
8*2Anasm_Assemble using Nasm
8*2Anasmobj_Assemble using Nasm
3*2Anasm_Assemble using Nasm
3*2Anasmcoff_COFF (Go32v2) file using Nasm
3*2Anasmelf_ELF32 (Linux) file using Nasm
3*2Anasmwin32_Win32 object file using Nasm
3*2Anasmwdosx_Win32/WDOSX object file using Nasm
3*2Anasmdarwin_macho32 object file using Nasm (experimental)
3*2Awasm_Obj file using Wasm (Watcom)
3*2Anasmobj_Obj file using Nasm
3*2Amasm_Obj file using Masm (Microsoft)
3*2Atasm_Obj file using Tasm (Borland)
3*2Aelf_ELF (Linux) using internal writer
3*2Acoff_COFF (Go32v2) using internal writer
3*2Apecoff_PE-COFF (Win32) using internal writer
3*2Ayasm_Assemble using Yasm (experimental)
4*2Aas_Assemble using GNU AS
4*2Agas_Assemble using GNU GAS
4*2Agas-darwin_Assemble darwin Mach-O64 using GNU GAS
4*2Amasm_Win64 object file using ml64 (Microsoft)
4*2Apecoff_PE-COFF (Win64) using internal writer
4*2Aelf_ELF (Linux-64bit) using internal writer
4*2Ayasm_Assemble using Yasm (experimental)
4*2Anasm_Assemble  using Nasm (experimental)
4*2Anasmwin64_Assemble Win64 object file using Nasm (experimental)
4*2Anasmelf_Assemble Linux-64bit object file using Nasm (experimental)
4*2Anasmdarwin_Assemble darwin macho64 object file using Nasm (experimental)
6*2Aas_Unix o-file using GNU AS
6*2Agas_GNU Motorola assembler
6*2Amit_MIT Syntax (old GAS)
6*2Amot_Standard Motorola assembler
A*2Aas_Assemble using GNU AS
P*2Aas_Assemble using GNU AS
S*2Aas_Assemble using GNU AS
**1b_Generate browser info
**2bl_Generate local symbol info
**1B_Build all modules
**1C<x>_Code generation options:
**2C3_Turn on ieee error checking for constants
**2Ca<x>_Select ABI; see fpc -i or fpc -ia for possible values
**2Cb_Generate code for a big-endian variant of the target architecture
**2Cc<x>_Set default calling convention to <x>
**2CD_Create also dynamic library (not supported)
**2Ce_Compilation with emulated floating point opcodes
**2Cf<x>_Select fpu instruction set to use; see fpc -i or fpc -if for possible values
**2CF<x>_Minimal floating point constant precision (default, 32, 64)
**2Cg_Generate PIC code
**2Ch<n>_<n> bytes heap (between 1023 and 67107840)
**2Ci_IO-checking
A*2CI<x>_Select instruction set on ARM: ARM or THUMB
**2Cn_Omit linking stage
P*2CN_Generate nil-pointer checks (AIX-only)
**2Co_Check overflow of integer operations
**2CO_Check for possible overflow of integer operations
**2Cp<x>_Select instruction set; see fpc -i or fpc -ic for possible values
**2CP<x>=<y>_ packing settings
**3CPPACKSET=<y>_ <y> set allocation: 0, 1 or DEFAULT or NORMAL, 2, 4 and 8
**3CPPACKENUM=<y>_ <y> enum packing: 0, 1, 2 and 4 or DEFAULT or NORMAL
**3CPPACKRECORD=<y>_ <y> record packing: 0 or DEFAULT or NORMAL, 1, 2, 4, 8, 16 and 32
**2Cr_Range checking
**2CR_Verify object method call validity
**2Cs<n>_Set stack checking size to <n>
**2Ct_Stack checking (for testing only, see manual)
8*2CT<x>_Target-specific code generation options
3*2CT<x>_Target-specific code generation options
4*2CT<x>_Target-specific code generation options
p*2CT<x>_Target-specific code generation options
P*2CT<x>_Target-specific code generation options
J*2CT<x>_Target-specific code generation options
A*2CT<x>_Target-specific code generation options
p*3CTsmalltoc_ Generate smaller TOCs at the expense of execution speed (AIX)
P*3CTsmalltoc_ Generate smaller TOCs at the expense of execution speed (AIX)
J*3CTautogetterprefix=X_  Automatically create getters for properties with prefix X (empty string disables)
J*3CTautosetterprefix=X_  Automatically create setters for properties with prefix X (empty string disables)
8*3CTcld_                 Emit a CLD instruction before using the x86 string instructions
3*3CTcld_                 Emit a CLD instruction before using the x86 string instructions
4*3CTcld_                 Emit a CLD instruction before using the x86 string instructions
J*3CTcompactintarrayinit_ Generate smaller (but potentially slower) code for initializing integer array constants
J*3CTenumfieldinit_       Initialize enumeration fields in constructors to enumtype(0), after calling inherited constructors
J*3CTinitlocals_          Initialize local variables that trigger a JVM bytecode verification error if used uninitialized (slows down code)
J*3CTlowercaseprocstart_  Lowercase the first character of procedure/function/method names
A*3CTthumbinterworking_ Generate Thumb interworking-safe code if possible
J*2Cv_Var/out parameter copy-out checking
**2CX_Create also smartlinked library
**1d<x>_Defines the symbol <x>
**1D_Generate a DEF file
**2Dd<x>_Set description to <x>
**2Dv<x>_Set DLL version to <x>
*O2Dw_PM application
**1e<x>_Set path to executable
**1E_Same as -Cn
**1fPIC_Same as -Cg
**1F<x>_Set file names and paths:
**2Fa<x>[,y]_(for a program) load units <x> and [y] before uses is parsed
**2Fc<x>_Set input codepage to <x>
**2FC<x>_Set RC compiler binary name to <x>
**2Fd_Disable the compiler's internal directory cache
**2FD<x>_Set the directory where to search for compiler utilities
**2Fe<x>_Redirect error output to <x>
**2Ff<x>_Add <x> to framework path (Darwin only)
**2FE<x>_Set exe/unit output path to <x>
**2Fi<x>_Add <x> to include path
**2Fl<x>_Add <x> to library path
**2FL<x>_Use <x> as dynamic linker
**2Fm<x>_Load unicode conversion table from <x>.txt in the compiler dir
**2FM<x>_Set the directory where to search for unicode binary files
**2Fo<x>_Add <x> to object path
**2Fr<x>_Load error message file <x>
**2FR<x>_Set resource (.res) linker to <x>
**2Fu<x>_Add <x> to unit path
**2FU<x>_Set unit output path to <x>, overrides -FE
**2FW<x>_Store generated whole-program optimization feedback in <x>
**2Fw<x>_Load previously stored whole-program optimization feedback from <x>
*g1g_Generate debug information (default format for target)
*g2gc_Generate checks for pointers
*g2gh_Use heaptrace unit (for memory leak/corruption debugging)
*g2gl_Use line info unit (show more info with backtraces)
*g2go<x>_Set debug information options
*g3godwarfsets_ Enable DWARF 'set' type debug information (breaks gdb < 6.5)
*g3gostabsabsincludes_ Store absolute/full include file paths in Stabs
*g3godwarfmethodclassprefix_ Prefix method names in DWARF with class name
*g3godwarfcpp_ Simulate C++ debug information in DWARF
*g2gp_Preserve case in stabs symbol names
*g2gs_Generate Stabs debug information
*g2gt_Trash local variables (to detect uninitialized uses; multiple 't' changes the trashing value)
*g2gv_Generates programs traceable with Valgrind
*g2gw_Generate DWARFv2 debug information (same as -gw2)
*g2gw2_Generate DWARFv2 debug information
*g2gw3_Generate DWARFv3 debug information
*g2gw4_Generate DWARFv4 debug information (experimental)
**1i_Information
**2iD_Return compiler date
**2iSO_Return compiler OS
**2iSP_Return compiler host processor
**2iTO_Return target OS
**2iTP_Return target processor
**2iV_Return short compiler version
**2iW_Return full compiler version
**2ia_Return list of supported ABI targets
**2ic_Return list of supported CPU instruction sets
**2if_Return list of supported FPU instruction sets
**2ii_Return list of supported inline assembler modes
**2io_Return list of supported optimizations
**2ir_Return list of recognized compiler and RTL features
**2it_Return list of supported targets
**2iu_Return list of supported microcontroller types
**2iw_Return list of supported whole program optimizations
**1I<x>_Add <x> to include path
**1k<x>_Pass <x> to the linker
**1l_Write logo
**1M<x>_Set language mode to <x>
**2Mfpc_Free Pascal dialect (default)
**2Mobjfpc_FPC mode with Object Pascal support
**2Mdelphi_Delphi 7 compatibility mode
**2Mtp_TP/BP 7.0 compatibility mode
**2Mmacpas_Macintosh Pascal dialects compatibility mode
**2Miso_ISO 7185 mode
**1n_Do not read the default config files
**1o<x>_Change the name of the executable produced to <x>
**1O<x>_Optimizations:
**2O-_Disable optimizations
**2O1_Level 1 optimizations (quick and debugger friendly)
**2O2_Level 2 optimizations (-O1 + quick optimizations)
**2O3_Level 3 optimizations (-O2 + slow optimizations)
**2O4_Level 4 optimizations (-O3 + optimizations which might have unexpected side effects)
**2Oa<x>=<y>_Set alignment
**2Oo[NO]<x>_Enable or disable optimizations; see fpc -i or fpc -io for possible values
**2Op<x>_Set target cpu for optimizing; see fpc -i or fpc -ic for possible values
**2OW<x>_Generate whole-program optimization feedback for optimization <x>; see fpc -i or fpc -iw for possible values
**2Ow<x>_Perform whole-program optimization <x>; see fpc -i or fpc -iw for possible values
**2Os_Optimize for size rather than speed
**1pg_Generate profile code for gprof (defines FPC_PROFILE)
F*1P<x>_Target CPU / compiler related options:
F*2PB_Show default compiler binary
F*2PP_Show default target cpu
F*2P<x>_Set target CPU (arm,avr,i386,jvm,m68k,mips,mipsel,powerpc,powerpc64,sparc,x86_64)
**1R<x>_Assembler reading style:
**2Rdefault_Use default assembler for target
3*2Ratt_Read AT&T style assembler
3*2Rintel_Read Intel style assembler
6*2RMOT_Read motorola style assembler
**1S<x>_Syntax options:
**2S2_Same as -Mobjfpc
**2Sc_Support operators like C (*=,+=,/= and -=)
**2Sa_Turn on assertions
**2Sd_Same as -Mdelphi
**2Se<x>_Error options. <x> is a combination of the following:
**3*_<n> : Compiler halts after the <n> errors (default is 1)
**3*_w : Compiler also halts after warnings
**3*_n : Compiler also halts after notes
**3*_h : Compiler also halts after hints
**2Sf_Enable certain features in compiler and RTL; see fpc -i or fpc -ir for possible values)
**2Sg_Enable LABEL and GOTO (default in -Mtp and -Mdelphi)
**2Sh_Use reference counted strings (ansistring by default) instead of shortstrings
**2Si_Turn on inlining of procedures/functions declared as "inline"
**2Sk_Load fpcylix unit
**2SI<x>_Set interface style to <x>
**3SIcom_COM compatible interface (default)
**3SIcorba_CORBA compatible interface
**2Sm_Support macros like C (global)
**2So_Same as -Mtp
**2Ss_Constructor name must be init (destructor must be done)
**2Sv_Support vector processing (use CPU vector extensions if available)
**2Sx_Enable exception keywords (default in Delphi/ObjFPC modes)
**2Sy_@<pointer> returns a typed pointer, same as $T+
**1s_Do not call assembler and linker
**2sh_Generate script to link on host
**2st_Generate script to link on target
**2sr_Skip register allocation phase (use with -alr)
**1T<x>_Target operating system:
3*2Tandroid_Android
3*2Taros_AROS
3*2Tbeos_BeOS
3*2Tdarwin_Darwin/Mac OS X
3*2Tembedded_Embedded
3*2Temx_OS/2 via EMX (including EMX/RSX extender)
3*2Tfreebsd_FreeBSD
3*2Tgo32v2_Version 2 of DJ Delorie DOS extender
3*2Thaiku_Haiku
3*2Tiphonesim_ iPhoneSimulator from iOS SDK 3.2+ (older versions: -Tdarwin)
3*2Tlinux_Linux
3*2Tnativent_Native NT API (experimental)
3*2Tnetbsd_NetBSD
3*2Tnetware_Novell Netware Module (clib)
3*2Tnetwlibc_Novell Netware Module (libc)
3*2Topenbsd_OpenBSD
3*2Tos2_OS/2 / eComStation
3*2Tsunos_SunOS/Solaris
3*2Tsymbian_Symbian OS
3*2Tsolaris_Solaris
3*2Twatcom_Watcom compatible DOS extender
3*2Twdosx_WDOSX DOS extender
3*2Twin32_Windows 32 Bit
3*2Twince_Windows CE
4*2Tdarwin_Darwin/Mac OS X
4*2Tfreebsd_FreeBSD
4*2Tlinux_Linux
4*2Tnetbsd_NetBSD
4*2Topenbsd_OpenBSD
4*2Tsolaris_Solaris
4*2Twin64_Win64 (64 bit Windows systems)
6*2Tamiga_Commodore Amiga
6*2Tatari_Atari ST/STe/TT
6*2Tlinux_Linux
6*2Tpalmos_PalmOS
8*2Tmsdos_MS-DOS (and compatible)
A*2Tandroid_Android
A*2Tdarwin_Darwin/iPhoneOS/iOS
A*2Tembedded_Embedded
A*2Tgba_Game Boy Advance
A*2Tlinux_Linux
A*2Tnds_Nintendo DS
A*2Tsymbian_Symbian
A*2Twince_Windows CE
J*2Tandroid_Android
J*2Tjava_Java
m*2Tembedded_Embedded
M*2Tandroid_Android
M*2Tembedded_Embedded
M*2Tlinux_Linux
P*2Taix_AIX
P*2Tamiga_AmigaOS
P*2Tdarwin_Darwin/Mac OS X
P*2Tlinux_Linux
P*2Tmacos_Mac OS (classic)
P*2Tmorphos_MorphOS
P*2Tnetbsd_NetBSD
P*2Twii_Wii
p*2Taix_AIX
p*2Tdarwin_Darwin/Mac OS X
p*2Tlinux_Linux
S*2Tsolaris_Solaris
S*2Tlinux_Linux
V*2Tembedded_Embedded
**1u<x>_Undefines the symbol <x>
**1U_Unit options:
**2Un_Do not check where the unit name matches the file name
**2Ur_Generate release unit files (never automatically recompiled)
**2Us_Compile a system unit
**1v<x>_Be verbose. <x> is a combination of the following letters:
**2*_e : Show errors (default)       0 : Show nothing (except errors)
**2*_w : Show warnings               u : Show unit info
**2*_n : Show notes                  t : Show tried/used files
**2*_h : Show hints                  c : Show conditionals
**2*_i : Show general info           d : Show debug info
**2*_l : Show linenumbers            r : Rhide/GCC compatibility mode
**2*_s : Show time stamps            q : Show message numbers
**2*_a : Show everything             x : Show info about invoked tools
**2*_b : Write file names messages   p : Write tree.log with parse tree
**2*_    with full path              v : Write fpcdebug.txt with
**2*_z : Write output to stderr          lots of debugging info
**2*_m<x>,<y> : Do not show messages numbered <x> and <y>
F*1V<x>_Append '-<x>' to the used compiler binary name (e.g. for version)
**1W<x>_Target-specific options (targets)
3*2WA_Specify native type application (Windows)
4*2WA_Specify native type application (Windows)
A*2WA_Specify native type application (Windows)
3*2Wb_Create a bundle instead of a library (Darwin)
P*2Wb_Create a bundle instead of a library (Darwin)
p*2Wb_Create a bundle instead of a library (Darwin)
A*2Wb_Create a bundle instead of a library (Darwin)
4*2Wb_Create a bundle instead of a library (Darwin)
3*2WB_Create a relocatable image (Windows, Symbian)
3*2WBxxxx_Set image base to xxxx (Windows, Symbian)
4*2WB_Create a relocatable image (Windows)
4*2WBxxxx_Set image base to xxxx (Windows)
A*2WB_Create a relocatable image (Windows, Symbian)
A*2WBxxxx_Set image base to xxxx (Windows, Symbian)
3*2WC_Specify console type application (EMX, OS/2, Windows)
4*2WC_Specify console type application (Windows)
A*2WC_Specify console type application (Windows)
P*2WC_Specify console type application (Classic Mac OS)
3*2WD_Use DEFFILE to export functions of DLL or EXE (Windows)
4*2WD_Use DEFFILE to export functions of DLL or EXE (Windows)
A*2WD_Use DEFFILE to export functions of DLL or EXE (Windows)
3*2We_Use external resources (Darwin)
4*2We_Use external resources (Darwin)
A*2We_Use external resources (Darwin)
P*2We_Use external resources (Darwin)
p*2We_Use external resources (Darwin)
3*2WF_Specify full-screen type application (EMX, OS/2)
3*2WG_Specify graphic type application (EMX, OS/2, Windows)
4*2WG_Specify graphic type application (Windows)
A*2WG_Specify graphic type application (Windows)
P*2WG_Specify graphic type application (Classic Mac OS)
3*2Wi_Use internal resources (Darwin)
4*2Wi_Use internal resources (Darwin)
A*2Wi_Use internal resources (Darwin)
P*2Wi_Use internal resources (Darwin)
p*2Wi_Use internal resources (Darwin)
3*2WI_Turn on/off the usage of import sections (Windows)
4*2WI_Turn on/off the usage of import sections (Windows)
A*2WI_Turn on/off the usage of import sections (Windows)
8*2Wm<x>_Set memory model
8*3WmTiny_Tiny memory model
8*3WmSmall_Small memory model (default)
8*3WmMedium_Medium memory model
8*3WmCompact_Compact memory model
8*3WmLarge_Large memory model
3*2WM<x>_Minimum Mac OS X deployment version: 10.4, 10.5.1, ... (Darwin)
4*2WM<x>_Minimum Mac OS X deployment version: 10.4, 10.5.1, ... (Darwin)
p*2WM<x>_Minimum Mac OS X deployment version: 10.4, 10.5.1, ... (Darwin)
P*2WM<x>_Minimum Mac OS X deployment version: 10.4, 10.5.1, ... (Darwin)
3*2WN_Do not generate relocation code, needed for debugging (Windows)
4*2WN_Do not generate relocation code, needed for debugging (Windows)
A*2WN_Do not generate relocation code, needed for debugging (Windows)
A*2Wpxxxx_Specify the controller type; see fpc -i or fpc -iu for possible values
m*2Wpxxxx_Specify the controller type; see fpc -i or fpc -iu for possible values
V*2Wpxxxx_Specify the controller type; see fpc -i or fpc -iu for possible values
3*2WP<x>_Minimum iOS deployment version: 3.0, 5.0.1, ... (iphonesim)
A*2WP<x>_Minimum iOS deployment version: 3.0, 5.0.1, ... (Darwin)
3*2WR_Generate relocation code (Windows)
4*2WR_Generate relocation code (Windows)
A*2WR_Generate relocation code (Windows)
8*2Wt<x>_Set the target executable format
8*3Wtexe_Create a DOS .EXE file (default)
8*3Wtcom_Create a DOS .COM file (requires tiny memory model)
P*2WT_Specify MPW tool type application (Classic Mac OS)
**2WX_Enable executable stack (Linux)
**1X_Executable options:
**2Xc_Pass --shared/-dynamic to the linker (BeOS, Darwin, FreeBSD, Linux)
**2Xd_Do not search default library path (sometimes required for cross-compiling when not using -XR)
**2Xe_Use external linker
**2Xf_Substitute pthread library name for linking (BSD)
**2Xg_Create debuginfo in a separate file and add a debuglink section to executable
**2XD_Try to link units dynamically      (defines FPC_LINK_DYNAMIC)
**2Xi_Use internal linker
**2XLA_Define library substitutions for linking
**2XLO_Define order of library linking
**2XLD_Exclude default order of standard libraries
**2Xm_Generate link map
**2XM<x>_Set the name of the 'main' program routine (default is 'main')
**2Xn_Use target system native linker instead of GNU ld (Solaris, AIX)
F*2Xp<x>_First search for the compiler binary in the directory <x>
**2XP<x>_Prepend the binutils names with the prefix <x>
**2Xr<x>_Set the linker's rlink-path to <x> (needed for cross compile, see the ld manual for more information) (BeOS, Linux)
**2XR<x>_Prepend <x> to all linker search paths (BeOS, Darwin, FreeBSD, Linux, Mac OS, Solaris)
**2Xs_Strip all symbols from executable
**2XS_Try to link units statically (default, defines FPC_LINK_STATIC)
**2Xt_Link with static libraries (-static is passed to linker)
**2Xv_Generate table for Virtual Entry calls
**2XX_Try to smartlink units             (defines FPC_LINK_SMART)
**1*_
**1?_Bu yardımı göster
**1h_Beklemeden bu yardımı gösterir
]

#
# The End...
