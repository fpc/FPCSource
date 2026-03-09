%{
(*
Vorspann
 ****************************************************************************)

unit rcparser;

{$I rcparserfn.inc}

%}

%token _ILLEGAL
%token _NUMBER _QUOTEDSTR _QUOTEDSTRL
%token _STR_StringFileInfo _STR_VarFileInfo _STR_Translation
%token _BEGIN _END _ID _NSWPSTR
%token _LANGUAGE _CHARACTERISTICS _VERSION _MOVEABLE _FIXED _PURE _IMPURE _PRELOAD _LOADONCALL _DISCARDABLE
%token _BITMAP _CURSOR _ICON _STRINGTABLE _VERSIONINFO
%token _ANICURSOR _ANIICON _DLGINCLUDE _DLGINIT _HTML _MANIFEST _MESSAGETABLE _PLUGPLAY _RCDATA _VXD
%token _FILEVERSION _PRODUCTVERSION _FILEFLAGSMASK _FILEFLAGS _FILEOS _FILETYPE _FILESUBTYPE _BLOCK _VALUE
%token _ACCELERATORS _DIALOG _DIALOGEX _MENU _MENUEX
%token _CAPTION _CLASS _EXSTYLE _STYLE _FONT _NOT
%token _CONTROL _LTEXT _RTEXT _CTEXT
%token _PUSHBUTTON _DEFPUSHBUTTON _CHECKBOX _AUTOCHECKBOX
%token _RADIOBUTTON _AUTORADIOBUTTON _AUTO3STATE _STATE3 _GROUPBOX
%token _EDITTEXT _COMBOBOX _LISTBOX _SCROLLBAR

%type <rcnumtype> numpos numexpr numeral
%type <rcstrtype> ident_string long_string non_whitespace_string long_stringfn
%type <TResourceDesc> resid rcdataid
%type <TMemoryStream> raw_data raw_item
%type <TFileStream> filename_string

%left '|'
%left '^'
%left '&'
%left '+' '-'
%left '*' '/' '%'
%right '~' _NOT _NUMNEG

%%

rcfile
    : /* empty */
    | rcfile defnstatement
    ;

defnstatement
    : resourcedef
    | languagedef
    ;

resourcedef
    : res_stringtable
    | res_bitmap
    | res_cursor
    | res_icon
    | res_version
    | res_rcdata
    | res_dialog
    ;

res_bitmap
    : resid _BITMAP { create_resource($1, RT_BITMAP); } suboptions filename_string            { TBitmapResource(aktresource).SetCustomBitmapDataStream($5); }

res_cursor
    : resid _CURSOR { create_resource($1, RT_CURSOR); } suboptions filename_string            { TGroupCursorResource(aktresource).SetCustomItemDataStream($5); }

res_icon
    : resid _ICON { create_resource($1, RT_ICON); } suboptions filename_string                { TGroupIconResource(aktresource).SetCustomItemDataStream($5); }

res_version
    : resid _VERSIONINFO { create_resource($1, RT_VERSION); } version_fixed _BEGIN version_blocks _END

version_fixed
    : /* empty */
    | version_fixed _FILEVERSION numeral ',' numeral ',' numeral ',' numeral                  { TVersionResource(aktresource).FixedInfo.FileVersion:= make_version($3.v, $5.v, $7.v, $9.v); }
    | version_fixed _PRODUCTVERSION numeral ',' numeral ',' numeral ',' numeral               { TVersionResource(aktresource).FixedInfo.ProductVersion:= make_version($3.v, $5.v, $7.v, $9.v); }
    | version_fixed _FILEFLAGSMASK numpos                                                     { TVersionResource(aktresource).FixedInfo.FileFlagsMask:= $3.v; }
    | version_fixed _FILEFLAGS numpos                                                         { TVersionResource(aktresource).FixedInfo.FileFlags:= $3.v; }
    | version_fixed _FILEOS numpos                                                            { TVersionResource(aktresource).FixedInfo.FileOS:= $3.v; }
    | version_fixed _FILETYPE numpos                                                          { TVersionResource(aktresource).FixedInfo.FileType:= $3.v; }
    | version_fixed _FILESUBTYPE numpos                                                       { TVersionResource(aktresource).FixedInfo.FileSubType:= $3.v; }
    ;

version_blocks
    : /* empty */
    | version_blocks _BLOCK _STR_StringFileInfo _BEGIN ver_strings_lang _END
    | version_blocks _BLOCK _STR_VarFileInfo _BEGIN ver_translation_data _END
    ;

ver_strings_lang
    : /* empty */
    | ver_strings_lang _BLOCK long_string _BEGIN                                              { version_string_tab_begin($3.v^); }
                                          ver_strings_data _END
    ;

ver_strings_data
    : /* empty */
    | ver_strings_data _VALUE long_string ',' long_string                                     { version_string_tab_add($3.v^, $5.v^); }
    ;

ver_translation_data
    : _VALUE _STR_Translation ',' ver_translation_pair
    ;

ver_translation_pair
    : numeral ',' numeral                                                                     { version_var_translation_add($1.v, $3.v); }
    | ver_translation_pair ',' numeral ',' numeral                                            { version_var_translation_add($3.v, $5.v); }
    ;

res_rcdata
    : resid rcdataid { create_resource($1, $2); } suboptions filename_string                  { aktresource.SetCustomRawDataStream($5); }
    | resid rcdataid { create_resource($1, $2); } suboptions _BEGIN raw_data _END             { aktresource.SetCustomRawDataStream($6); }
    ;

res_stringtable
    : _STRINGTABLE { stringtable_begin(); } suboptions _BEGIN stringtable_data _END { stringtable_end(); }

stringtable_data
    : /* empty */
    | stringtable_data stringtable_entry
    ;

stringtable_entry
    : numeral ',' long_string                      { stringtable_add($1.v, $3.v^); }
    | numeral long_string                          { stringtable_add($1.v, $2.v^); }
    ;

res_dialog
    : resid _DIALOG { dialog_begin($1, false); } suboptions
      numexpr ',' numexpr ',' numexpr ',' numexpr
      { dialog_set_coords($5.v, $7.v, $9.v, $11.v); }
      dialog_options _BEGIN dialog_controls _END
      { dialog_end(); }
    | resid _DIALOGEX { dialog_begin($1, true); } suboptions
      numexpr ',' numexpr ',' numexpr ',' numexpr opt_helpid
      { dialog_set_coords($5.v, $7.v, $9.v, $11.v); }
      dialog_options _BEGIN dialog_controls _END
      { dialog_end(); }
    ;

opt_helpid
    : /* empty */
    | ',' numexpr                                  { dialog_set_helpid($2.v); }
    ;

dialog_options
    : /* empty */
    | dialog_options dialog_option
    ;

dialog_option
    : _CAPTION long_string                         { dialog_set_caption($2.v^); }
    | _STYLE numexpr                               { dialog_set_style($2.v); }
    | _EXSTYLE numexpr                             { dialog_set_exstyle($2.v); }
    | _EXSTYLE '=' numexpr                         { dialog_set_exstyle($3.v); }
    | _FONT numexpr ',' long_string opt_font_ex    { dialog_set_font($2.v, $4.v^); }
    | _CLASS long_string                           { dialog_set_class($2.v^); }
    | _CLASS numexpr                               { dialog_set_class_ord($2.v); }
    | _MENU ident_string                           { dialog_set_menu($2.v^); }
    | _MENU numexpr                                { dialog_set_menu_ord($2.v); }
    ;

opt_font_ex
    : /* empty */
    | ',' numexpr ',' numexpr ',' numexpr          { dialog_set_font_ex_params($2.v, $4.v, $6.v); }
    ;

dialog_controls
    : /* empty */
    | dialog_controls dialog_control
    ;

dialog_control
    : _CONTROL long_string ',' numexpr ',' ident_string ',' numexpr ','
      numexpr ',' numexpr ',' numexpr ',' numexpr opt_exstyle
      { dialog_add_control_generic($4.v, $10.v, $12.v, $14.v, $16.v, $8.v, $2.v^, $6.v^); }
    | _LTEXT long_string ',' numexpr ',' numexpr ',' numexpr ','
      numexpr ',' numexpr opt_style_exstyle
      { dialog_add_control_std($4.v, $6.v, $8.v, $10.v, $12.v,
          WS_CHILD or WS_VISIBLE or WS_GROUP or SS_LEFT, CTL_STATIC, $2.v^); }
    | _RTEXT long_string ',' numexpr ',' numexpr ',' numexpr ','
      numexpr ',' numexpr opt_style_exstyle
      { dialog_add_control_std($4.v, $6.v, $8.v, $10.v, $12.v,
          WS_CHILD or WS_VISIBLE or WS_GROUP or SS_RIGHT, CTL_STATIC, $2.v^); }
    | _CTEXT long_string ',' numexpr ',' numexpr ',' numexpr ','
      numexpr ',' numexpr opt_style_exstyle
      { dialog_add_control_std($4.v, $6.v, $8.v, $10.v, $12.v,
          WS_CHILD or WS_VISIBLE or WS_GROUP or SS_CENTER, CTL_STATIC, $2.v^); }
    | _PUSHBUTTON long_string ',' numexpr ',' numexpr ',' numexpr ','
      numexpr ',' numexpr opt_style_exstyle
      { dialog_add_control_std($4.v, $6.v, $8.v, $10.v, $12.v,
          WS_CHILD or WS_VISIBLE or WS_TABSTOP or BS_PUSHBUTTON, CTL_BUTTON, $2.v^); }
    | _DEFPUSHBUTTON long_string ',' numexpr ',' numexpr ',' numexpr ','
      numexpr ',' numexpr opt_style_exstyle
      { dialog_add_control_std($4.v, $6.v, $8.v, $10.v, $12.v,
          WS_CHILD or WS_VISIBLE or WS_TABSTOP or BS_DEFPUSHBUTTON, CTL_BUTTON, $2.v^); }
    | _CHECKBOX long_string ',' numexpr ',' numexpr ',' numexpr ','
      numexpr ',' numexpr opt_style_exstyle
      { dialog_add_control_std($4.v, $6.v, $8.v, $10.v, $12.v,
          WS_CHILD or WS_VISIBLE or WS_TABSTOP or BS_CHECKBOX, CTL_BUTTON, $2.v^); }
    | _AUTOCHECKBOX long_string ',' numexpr ',' numexpr ',' numexpr ','
      numexpr ',' numexpr opt_style_exstyle
      { dialog_add_control_std($4.v, $6.v, $8.v, $10.v, $12.v,
          WS_CHILD or WS_VISIBLE or WS_TABSTOP or BS_AUTOCHECKBOX, CTL_BUTTON, $2.v^); }
    | _RADIOBUTTON long_string ',' numexpr ',' numexpr ',' numexpr ','
      numexpr ',' numexpr opt_style_exstyle
      { dialog_add_control_std($4.v, $6.v, $8.v, $10.v, $12.v,
          WS_CHILD or WS_VISIBLE or WS_TABSTOP or BS_RADIOBUTTON, CTL_BUTTON, $2.v^); }
    | _AUTORADIOBUTTON long_string ',' numexpr ',' numexpr ',' numexpr ','
      numexpr ',' numexpr opt_style_exstyle
      { dialog_add_control_std($4.v, $6.v, $8.v, $10.v, $12.v,
          WS_CHILD or WS_VISIBLE or WS_TABSTOP or BS_AUTORADIOBUTTON, CTL_BUTTON, $2.v^); }
    | _AUTO3STATE long_string ',' numexpr ',' numexpr ',' numexpr ','
      numexpr ',' numexpr opt_style_exstyle
      { dialog_add_control_std($4.v, $6.v, $8.v, $10.v, $12.v,
          WS_CHILD or WS_VISIBLE or WS_TABSTOP or BS_AUTO3STATE, CTL_BUTTON, $2.v^); }
    | _STATE3 long_string ',' numexpr ',' numexpr ',' numexpr ','
      numexpr ',' numexpr opt_style_exstyle
      { dialog_add_control_std($4.v, $6.v, $8.v, $10.v, $12.v,
          WS_CHILD or WS_VISIBLE or WS_TABSTOP or BS_3STATE, CTL_BUTTON, $2.v^); }
    | _GROUPBOX long_string ',' numexpr ',' numexpr ',' numexpr ','
      numexpr ',' numexpr opt_style_exstyle
      { dialog_add_control_std($4.v, $6.v, $8.v, $10.v, $12.v,
          WS_CHILD or WS_VISIBLE or BS_GROUPBOX, CTL_BUTTON, $2.v^); }
    | _EDITTEXT numexpr ',' numexpr ',' numexpr ',' numexpr ',' numexpr
      opt_style_exstyle
      { dialog_add_control_std($2.v, $4.v, $6.v, $8.v, $10.v,
          WS_CHILD or WS_VISIBLE or WS_TABSTOP or WS_BORDER, CTL_EDIT, ''); }
    | _COMBOBOX numexpr ',' numexpr ',' numexpr ',' numexpr ',' numexpr
      opt_style_exstyle
      { dialog_add_control_std($2.v, $4.v, $6.v, $8.v, $10.v,
          WS_CHILD or WS_VISIBLE or WS_TABSTOP, CTL_COMBOBOX, ''); }
    | _LISTBOX numexpr ',' numexpr ',' numexpr ',' numexpr ',' numexpr
      opt_style_exstyle
      { dialog_add_control_std($2.v, $4.v, $6.v, $8.v, $10.v,
          WS_CHILD or WS_VISIBLE or WS_TABSTOP or WS_BORDER or LBS_NOTIFY, CTL_LISTBOX, ''); }
    | _SCROLLBAR numexpr ',' numexpr ',' numexpr ',' numexpr ',' numexpr
      opt_style_exstyle
      { dialog_add_control_std($2.v, $4.v, $6.v, $8.v, $10.v,
          WS_CHILD or WS_VISIBLE or WS_TABSTOP, CTL_SCROLLBAR, ''); }
    | _ICON long_string ',' numexpr ',' numexpr ',' numexpr
      { dialog_ctrl_reset; } icon_params
      { dialog_add_control_icon($4.v, $6.v, $8.v, $2.v^); }
    ;

opt_style_exstyle
    : /* empty */                                  { dialog_ctrl_reset; }
    | ',' numexpr                                  { dialog_ctrl_reset; dialog_ctrl_set_style($2.v); }
    | ',' numexpr ',' numexpr                      { dialog_ctrl_reset; dialog_ctrl_set_style($2.v); dialog_ctrl_set_exstyle($4.v); }
    ;

opt_exstyle
    : /* empty */
    | ',' numexpr                                  { dialog_ctrl_set_exstyle($2.v); }
    ;

icon_params
    : /* empty */
    | ',' numexpr ',' numexpr                      { dialog_ctrl_set_wh($2.v, $4.v); }
    | ',' numexpr ',' numexpr ',' numexpr          { dialog_ctrl_set_wh($2.v, $4.v); dialog_ctrl_set_style($6.v); }
    | ',' numexpr ',' numexpr ',' numexpr ',' numexpr
      { dialog_ctrl_set_wh($2.v, $4.v); dialog_ctrl_set_style($6.v); dialog_ctrl_set_exstyle($8.v); }
    ;

rcdataid
    : _ANICURSOR                                   { $$:= TResourceDesc.Create(RT_ANICURSOR); }
    | _ANIICON                                     { $$:= TResourceDesc.Create(RT_ANIICON); }
    | _DLGINCLUDE                                  { $$:= TResourceDesc.Create(RT_DLGINCLUDE); }
    | _DLGINIT                                     { $$:= TResourceDesc.Create(RT_DLGINIT); }
    | _HTML                                        { $$:= TResourceDesc.Create(23); }
    | _MANIFEST                                    { $$:= TResourceDesc.Create(RT_MANIFEST); }
    | _MESSAGETABLE                                { $$:= TResourceDesc.Create(RT_MESSAGETABLE); }
    | _PLUGPLAY                                    { $$:= TResourceDesc.Create(RT_PLUGPLAY); }
    | _RCDATA                                      { $$:= TResourceDesc.Create(RT_RCDATA); }
    | _VXD                                         { $$:= TResourceDesc.Create(RT_VXD); }
    | resid
    ;

resid
    : numpos                                       { $$:= TResourceDesc.Create($1.v); }
    | ident_string                                 { $$:= TResourceDesc.Create($1.v^); }
    ;

suboptions
    : /* empty */
    | suboptions suboption
    ;

suboption
    : _LANGUAGE numpos ',' numpos                  { change_lang_id(MakeLangID($2.v, $4.v)); }
    | _CHARACTERISTICS numpos                      { aktresource.Characteristics:= $2.v; }
    | _VERSION numpos                              { aktresource.Version:= $2.v; }
    | _MOVEABLE                                    { aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_MOVEABLE; }
    | _FIXED                                       { aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_MOVEABLE; }
    | _PURE                                        { aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PURE; }
    | _IMPURE                                      { aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PURE; }
    | _PRELOAD                                     { aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PRELOAD; }
    | _LOADONCALL                                  { aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PRELOAD; }
    | _DISCARDABLE                                 { aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_DISCARDABLE; }
    ;

languagedef
    : _LANGUAGE numpos ',' numpos                  { language:= MakeLangID($2.v, $4.v); }

numpos
    : numexpr
    ;

numeral
    : _NUMBER                                      { $$:= str_to_num(yytext); }
    ;

numexpr
    : numeral
    | '(' numexpr ')'                              { $$:= $2; }
    | '~' numexpr %prec '~'                        { $$.v:= not $2.v; $$.long:= $2.long; }
    | _NOT numexpr %prec _NOT                      { $$.v:= not $2.v; $$.long:= $2.long; }
    | '-' numexpr %prec _NUMNEG                    { $$.v:= -$2.v; $$.long:= $2.long; }
    | numexpr '*' numexpr                          { $$.v:= $1.v * $3.v; $$.long:= $1.long or $3.long; }
    | numexpr '/' numexpr                          { $$.v:= $1.v div Max(1, $3.v); $$.long:= $1.long or $3.long; }
    | numexpr '%' numexpr                          { $$.v:= $1.v mod Max(1, $3.v); $$.long:= $1.long or $3.long; }
    | numexpr '+' numexpr                          { $$.v:= $1.v + $3.v; $$.long:= $1.long or $3.long; }
    | numexpr '-' numexpr                          { $$.v:= $1.v - $3.v; $$.long:= $1.long or $3.long; }
    | numexpr '&' numexpr                          { $$.v:= $1.v and $3.v; $$.long:= $1.long or $3.long; }
    | numexpr '^' numexpr                          { $$.v:= $1.v xor $3.v; $$.long:= $1.long or $3.long; }
    | numexpr '|' numexpr                          { $$.v:= $1.v or $3.v; $$.long:= $1.long or $3.long; }
    ;

ident_string
    : _ID                                          { string_new($$, yytext, opt_code_page); }
    | long_string
    ;

non_whitespace_string
    : _NSWPSTR                                     { string_new($$, yytext, opt_code_page); }
    ;

filename_string
    : long_stringfn                                { $$:= TFileStream.Create($1.v^, fmOpenRead or fmShareDenyWrite); }
    | non_whitespace_string                        { $$:= TFileStream.Create($1.v^, fmOpenRead or fmShareDenyWrite); }
    ;

long_stringfn
    : _QUOTEDSTR                                   { string_new_uni($$, @strbuf[0], strbuflen, opt_code_page, false); }
    | _QUOTEDSTRL                                  { string_new_uni($$, @strbuf[0], strbuflen, CP_UTF16, false); }
    | _STR_StringFileInfo                          { string_new($$, yytext, opt_code_page); }
    | _STR_VarFileInfo                             { string_new($$, yytext, opt_code_page); }
    | _STR_Translation                             { string_new($$, yytext, opt_code_page); }
    ;

long_string
    : _QUOTEDSTR                                   { string_new_uni($$, @strbuf[0], strbuflen, opt_code_page, true); }
    | _QUOTEDSTRL                                  { string_new_uni($$, @strbuf[0], strbuflen, CP_UTF16, true); }
    | _STR_StringFileInfo                          { string_new($$, yytext, opt_code_page); }
    | _STR_VarFileInfo                             { string_new($$, yytext, opt_code_page); }
    | _STR_Translation                             { string_new($$, yytext, opt_code_page); }
    ;

raw_data
    :                                              { $$:= TMemoryStream.Create; }
      raw_item
    | raw_data ',' { $$:= $1; } raw_item
    ;

raw_item
    : /* empty */
      {
        $$:= $<TMemoryStream>0;
      }
    | long_string
      {
        $$:= $<TMemoryStream>0;
        raw_write_string($$, $1);
      }
    | numeral
      {
        $$:= $<TMemoryStream>0;
        raw_write_int($$, $1);
      }
    ;

%%

{$I rclex.inc}
begin
  bufptr:= 0;
  lexlib.get_char:= @rc_get_char;
  lexlib.unget_char:= @rc_unget_char;
end.
