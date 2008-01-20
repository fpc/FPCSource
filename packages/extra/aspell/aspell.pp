
// Aspell bindings
// Copyright (C)2006-2007 Pavel Kanzelsberger, All Rights Reserved
// www.kanzelsberger.com, e-mail: pavel at kanzelsberger dot com

// This library is free software. You can modify/redistribute it under
// the terms of a modified LGPL license (see files LICENSE and
// LICENSE.addon

unit aspell;

interface

uses
  cTypes, DynLibs;

{$IFDEF Linux}
  {$DEFINE Dynamic}
  const aspelllib='/usr/lib/libaspell.so.15';
{$ENDIF}

{$IFDEF FreeBSD}
  {$DEFINE Static}
  const aspelllib='/usr/local/lib/libaspell.so.15';
{$ENDIF}

{$IFDEF darwin}
 {$DEFINE Dynamic}
 const aspelllib='libaspell.dylib';
{$ENDIF}

{$IFDEF windows}
 {$DEFINE Dynamic}
 const aspelllib='aspell-%s.dll';
{$ENDIF}

{$IFDEF BeOS}
 {$DEFINE Dynamic}
 const aspelllib='/boot/home/config/lib/libaspell.so';
{$ENDIF}

{$IFDEF SkyOS}
 {$DEFINE Static}
 {$LINKLIB aspell}
 const aspelllib='aspell';
{$ENDIF}

type aspellconfig=pointer;
     aspelldictinfolist=pointer;
     aspelldictinfoenumeration=pointer;
     aspellstringlist=pointer;
     aspellcanhaveerror=pointer;
     aspellspeller=pointer;
     aspellwordlist=pointer;
     aspellstringenumeration=pointer;

     taspellmoduleinfo=record name:pchar;
                              order_num:double;
                              lib_dir:pchar;
                              dict_dirs:aspellstringlist;
                              dict_exts:aspellstringlist;
                              end;
     aspellmoduleinfo=^taspellmoduleinfo;

     taspelldictinfo=record name,code,jargon:pchar;
                            size:cint32;
                            size_str:pchar;
                            module:aspellmoduleinfo;
                            end;
     aspelldictinfo=^taspelldictinfo;

var aspellpresent:longbool;
    alib: TLibHandle;

   {$IFDEF Dynamic}
    new_aspell_config:function():aspellconfig; cdecl;
    get_aspell_dict_info_list:function (config:aspellconfig):aspelldictinfolist; cdecl;
    delete_aspell_config:procedure (config:aspellconfig); cdecl;
    aspell_dict_info_list_elements:function (ths:aspelldictinfolist):aspelldictinfoenumeration; cdecl;
    aspell_dict_info_enumeration_next:function (ths:aspelldictinfoenumeration):aspelldictinfo; cdecl;
    aspell_dict_info_enumeration_at_end:function (ths:aspelldictinfoenumeration):cint32; cdecl;
    delete_aspell_dict_info_enumeration:procedure (ths:aspelldictinfoenumeration); cdecl;
    aspell_config_replace:function (config:aspellconfig;key,value:pchar):cint32; cdecl;
    new_aspell_speller:function (config:aspellconfig):aspellcanhaveerror; cdecl;
    aspell_error_number:function (ths:aspellcanhaveerror):word; cdecl;
    to_aspell_speller:function (ths:aspellcanhaveerror):aspellspeller; cdecl;
    aspell_speller_check:function (ths:aspellspeller;myword:pchar;wordsize:cint32):cint32; cdecl;
    aspell_speller_suggest:function (ths:aspellspeller;myword:pchar;wordsize:cint32):aspellwordlist; cdecl;
    aspell_word_list_elements:function (ths:aspellwordlist):aspellstringenumeration; cdecl;
    aspell_string_enumeration_next:function (ths:aspellstringenumeration):pchar; cdecl;
    delete_aspell_string_enumeration:procedure (ths:aspellstringenumeration); cdecl;
    aspell_speller_add_to_session:function (ths:aspellspeller;myword:pchar;wordsize:cint32):cint32; cdecl;
    aspell_speller_add_to_personal:function (ths:aspellspeller;myword:pchar;wordsize:cint32):cint32; cdecl;
    aspell_speller_store_replacement:function (ths:aspellspeller;mis:pchar;missize:cint32;cor:pchar;corsize:cint32):cint32; cdecl;
    aspell_speller_save_all_word_lists:function (ths:aspellspeller):cint32; cdecl;
    delete_aspell_speller:procedure (ths:aspellspeller); cdecl;
   {$ENDIF}

procedure aspell_init(const libn: ansistring);
procedure aspell_done;

{$IFDEF Static}
 function new_aspell_config():aspellconfig; cdecl; external aspelllib;
 function get_aspell_dict_info_list (config:aspellconfig):aspelldictinfolist; cdecl; external aspelllib;
procedure delete_aspell_config (config:aspellconfig); cdecl; external aspelllib;
 function aspell_dict_info_list_elements (ths:aspelldictinfolist):aspelldictinfoenumeration; cdecl; external aspelllib;
 function aspell_dict_info_enumeration_next (ths:aspelldictinfoenumeration):aspelldictinfo; cdecl; external aspelllib;
 function aspell_dict_info_enumeration_at_end (ths:aspelldictinfoenumeration):cint32; cdecl; external aspelllib;
procedure delete_aspell_dict_info_enumeration (ths:aspelldictinfoenumeration); cdecl; external aspelllib;
 function aspell_config_replace (config:aspellconfig;key,value:pchar):cint32; cdecl; external aspelllib;
 function new_aspell_speller (config:aspellconfig):aspellcanhaveerror; cdecl; external aspelllib;
 function aspell_error_number (ths:aspellcanhaveerror):word; cdecl; external aspelllib;
 function to_aspell_speller (ths:aspellcanhaveerror):aspellspeller; cdecl; external aspelllib;
 function aspell_speller_check (ths:aspellspeller;myword:pchar;wordsize:cint32):cint32; cdecl; external aspelllib;
 function aspell_speller_suggest (ths:aspellspeller;myword:pchar;wordsize:cint32):aspellwordlist; cdecl; external aspelllib;
 function aspell_word_list_elements (ths:aspellwordlist):aspellstringenumeration; cdecl; external aspelllib;
 function aspell_string_enumeration_next (ths:aspellstringenumeration):pchar; cdecl; external aspelllib;
procedure delete_aspell_string_enumeration (ths:aspellstringenumeration); cdecl; external aspelllib;
 function aspell_speller_add_to_session (ths:aspellspeller;myword:pchar;wordsize:cint32):cint32; cdecl; external aspelllib;
 function aspell_speller_add_to_personal (ths:aspellspeller;myword:pchar;wordsize:cint32):cint32; cdecl; external aspelllib;
 function aspell_speller_store_replacement (ths:aspellspeller;mis:pchar;missize:cint32;cor:pchar;corsize:cint32):cint32; cdecl; external aspelllib;
 function aspell_speller_save_all_word_lists (ths:aspellspeller):cint32; cdecl; external aspelllib;
procedure delete_aspell_speller (ths:aspellspeller); cdecl; external aspelllib;
{$ENDIF}

implementation

{$ifdef Dynamic}
  {$ifdef windows}
uses
  SysUtils;
  {$endif}
{$endif}

{$IFDEF Dynamic}
function loadsymbol (name:pchar;proc:pointer):longbool;
var addr:pointer;     
    tproc:^pointer;
 begin
 loadsymbol:=false;
 addr:=GetProcedureAddress(alib,name);

 if addr=nil then
  exit;

 tproc:=proc;
 tproc^:=addr;
 loadsymbol:=true;
 end;
{$ENDIF}

procedure aspell_init(const libn: ansistring);
var
  mylib:string;
  {$ifdef windows}
  bversion,path:string;
  version:dword;
  {$endif}
 begin
{$IFDEF Static}
 aspellpresent:=true;
{$ELSE}
 aspellpresent:=false;
 mylib:=libn;

{$IFDEF windows}
 bversion:=RegistryQueryValue (regLocalMachine,'SOFTWARE\Aspell','AspellVersion');
 move (bversion[1],version,4);
 path:=RegistryQueryValue (regLocalMachine,'SOFTWARE\Aspell','Path');
 // will work if they passed %s, won't bork if they passed absolute
 mylib:=path + PathDelim + StringReplace(libn, '%s', IntToStr(Version), [rfReplaceAll]);
{$ENDIF}

 alib := LoadLibrary(mylib);
 if alib = NilHandle then
  exit;

 if loadsymbol ('new_aspell_config',@new_aspell_config)=false then exit;
 if loadsymbol ('get_aspell_dict_info_list',@get_aspell_dict_info_list)=false then exit;
 if loadsymbol ('delete_aspell_config',@delete_aspell_config)=false then exit;
 if loadsymbol ('aspell_dict_info_list_elements',@aspell_dict_info_list_elements)=false then exit;
 if loadsymbol ('aspell_dict_info_enumeration_next',@aspell_dict_info_enumeration_next)=false then exit;
 if loadsymbol ('aspell_dict_info_enumeration_at_end',@aspell_dict_info_enumeration_at_end)=false then exit;
 if loadsymbol ('delete_aspell_dict_info_enumeration',@delete_aspell_dict_info_enumeration)=false then exit;
 if loadsymbol ('aspell_config_replace',@aspell_config_replace)=false then exit;
 if loadsymbol ('new_aspell_speller',@new_aspell_speller)=false then exit;
 if loadsymbol ('aspell_error_number',@aspell_error_number)=false then exit;
 if loadsymbol ('to_aspell_speller',@to_aspell_speller)=false then exit;
 if loadsymbol ('aspell_speller_check',@aspell_speller_check)=false then exit;
 if loadsymbol ('aspell_speller_suggest',@aspell_speller_suggest)=false then exit;
 if loadsymbol ('aspell_word_list_elements',@aspell_word_list_elements)=false then exit;
 if loadsymbol ('aspell_string_enumeration_next',@aspell_string_enumeration_next)=false then exit;
 if loadsymbol ('delete_aspell_string_enumeration',@delete_aspell_string_enumeration)=false then exit;
 if loadsymbol ('aspell_speller_add_to_session',@aspell_speller_add_to_session)=false then exit;
 if loadsymbol ('aspell_speller_add_to_personal',@aspell_speller_add_to_personal)=false then exit;
 if loadsymbol ('aspell_speller_store_replacement',@aspell_speller_store_replacement)=false then exit;
 if loadsymbol ('aspell_speller_save_all_word_lists',@aspell_speller_save_all_word_lists)=false then exit;
 if loadsymbol ('delete_aspell_speller',@delete_aspell_speller)=false then exit;
 
 aspellpresent:=true;
{$ENDIF}
 end;
procedure aspell_done;
 begin
{$IFDEF Dynamic}
 if aspellpresent then
   UnloadLibrary(alib);
{$ENDIF}
 end;
 
initialization
 aspell_init(aspelllib);
 
finalization
 aspell_done;

end.
