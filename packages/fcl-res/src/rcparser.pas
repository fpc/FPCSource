
(* Yacc parser template (TP Yacc V3.0), V1.2 6-17-91 AG *)

(* global definitions: *)

(*
Vorspann
 ****************************************************************************)

unit rcparser;

{$I rcparserfn.inc}

const _ILLEGAL = 257;
const _NUMBER = 258;
const _QUOTEDSTR = 259;
const _QUOTEDSTRL = 260;
const _STR_StringFileInfo = 261;
const _STR_VarFileInfo = 262;
const _STR_Translation = 263;
const _BEGIN = 264;
const _END = 265;
const _ID = 266;
const _LANGUAGE = 267;
const _CHARACTERISTICS = 268;
const _VERSION = 269;
const _MOVEABLE = 270;
const _FIXED = 271;
const _PURE = 272;
const _IMPURE = 273;
const _PRELOAD = 274;
const _LOADONCALL = 275;
const _DISCARDABLE = 276;
const _BITMAP = 277;
const _CURSOR = 278;
const _ICON = 279;
const _STRINGTABLE = 280;
const _VERSIONINFO = 281;
const _ANICURSOR = 282;
const _ANIICON = 283;
const _DLGINCLUDE = 284;
const _DLGINIT = 285;
const _HTML = 286;
const _MANIFEST = 287;
const _MESSAGETABLE = 288;
const _PLUGPLAY = 289;
const _RCDATA = 290;
const _VXD = 291;
const _FILEVERSION = 292;
const _PRODUCTVERSION = 293;
const _FILEFLAGSMASK = 294;
const _FILEFLAGS = 295;
const _FILEOS = 296;
const _FILETYPE = 297;
const _FILESUBTYPE = 298;
const _BLOCK = 299;
const _VALUE = 300;
const _ACCELERATORS = 301;
const _DIALOG = 302;
const _DIALOGEX = 303;
const _MENU = 304;
const _MENUEX = 305;
const _NUMNEG = 306;

type YYSType = record case Integer of
                 1 : ( yyTFileStream : TFileStream );
                 2 : ( yyTMemoryStream : TMemoryStream );
                 3 : ( yyTResourceDesc : TResourceDesc );
                 4 : ( yyrcnumtype : rcnumtype );
                 5 : ( yyrcstrtype : rcstrtype );
               end(*YYSType*);

var yylval : YYSType;

function yylex : Integer; forward;

function yyparse : Integer;

var yystate, yysp, yyn : Integer;
    yys : array [1..yymaxdepth] of Integer;
    yyv : array [1..yymaxdepth] of YYSType;
    yyval : YYSType;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)
begin
  (* actions: *)
  case yyruleno of
   1 : begin
       end;
   2 : begin
         yyval := yyv[yysp-1];
       end;
   3 : begin
         yyval := yyv[yysp-0];
       end;
   4 : begin
         yyval := yyv[yysp-0];
       end;
   5 : begin
         yyval := yyv[yysp-0];
       end;
   6 : begin
         yyval := yyv[yysp-0];
       end;
   7 : begin
         yyval := yyv[yysp-0];
       end;
   8 : begin
         yyval := yyv[yysp-0];
       end;
   9 : begin
         yyval := yyv[yysp-0];
       end;
  10 : begin
         yyval := yyv[yysp-0];
       end;
  11 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, RT_BITMAP); 
       end;
  12 : begin
         TBitmapResource(aktresource).SetCustomBitmapDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  13 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, RT_CURSOR); 
       end;
  14 : begin
         TGroupCursorResource(aktresource).SetCustomItemDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  15 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, RT_ICON); 
       end;
  16 : begin
         TGroupIconResource(aktresource).SetCustomItemDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  17 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, RT_VERSION); 
       end;
  18 : begin
         yyval := yyv[yysp-6];
       end;
  19 : begin
       end;
  20 : begin
         TVersionResource(aktresource).FixedInfo.FileVersion:= make_version(yyv[yysp-6].yyrcnumtype.v, yyv[yysp-4].yyrcnumtype.v, yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  21 : begin
         TVersionResource(aktresource).FixedInfo.ProductVersion:= make_version(yyv[yysp-6].yyrcnumtype.v, yyv[yysp-4].yyrcnumtype.v, yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  22 : begin
         TVersionResource(aktresource).FixedInfo.FileFlagsMask:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  23 : begin
         TVersionResource(aktresource).FixedInfo.FileFlags:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  24 : begin
         TVersionResource(aktresource).FixedInfo.FileOS:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  25 : begin
         TVersionResource(aktresource).FixedInfo.FileType:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  26 : begin
         TVersionResource(aktresource).FixedInfo.FileSubType:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  27 : begin
       end;
  28 : begin
         yyval := yyv[yysp-5];
       end;
  29 : begin
         yyval := yyv[yysp-5];
       end;
  30 : begin
       end;
  31 : begin
         version_string_tab_begin(yyv[yysp-1].yyrcstrtype.v^); 
       end;
  32 : begin
         yyval := yyv[yysp-6];
       end;
  33 : begin
       end;
  34 : begin
         version_string_tab_add(yyv[yysp-2].yyrcstrtype.v^, yyv[yysp-0].yyrcstrtype.v^); 
       end;
  35 : begin
         yyval := yyv[yysp-3];
       end;
  36 : begin
         version_var_translation_add(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  37 : begin
         version_var_translation_add(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  38 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, yyv[yysp-0].yyTResourceDesc); 
       end;
  39 : begin
         aktresource.SetCustomRawDataStream(yyv[yysp-0].yyTFileStream); 
       end;
  40 : begin
         create_resource(yyv[yysp-1].yyTResourceDesc, yyv[yysp-0].yyTResourceDesc); 
       end;
  41 : begin
         aktresource.SetCustomRawDataStream(yyv[yysp-1].yyTMemoryStream); 
       end;
  42 : begin
         stringtable_begin(); 
       end;
  43 : begin
         stringtable_end(); 
       end;
  44 : begin
       end;
  45 : begin
         yyval := yyv[yysp-1];
       end;
  46 : begin
         stringtable_add(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcstrtype.v^); 
       end;
  47 : begin
         stringtable_add(yyv[yysp-1].yyrcnumtype.v, yyv[yysp-0].yyrcstrtype.v^); 
       end;
  48 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_ANICURSOR); 
       end;
  49 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_ANIICON); 
       end;
  50 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_DLGINCLUDE); 
       end;
  51 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_DLGINIT); 
       end;
  52 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(23); 
       end;
  53 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_MANIFEST); 
       end;
  54 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_MESSAGETABLE); 
       end;
  55 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_PLUGPLAY); 
       end;
  56 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_RCDATA); 
       end;
  57 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(RT_VXD); 
       end;
  58 : begin
         yyval := yyv[yysp-0];
       end;
  59 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(yyv[yysp-0].yyrcnumtype.v); 
       end;
  60 : begin
         yyval.yyTResourceDesc:= TResourceDesc.Create(yyv[yysp-0].yyrcstrtype.v^); 
       end;
  61 : begin
       end;
  62 : begin
         yyval := yyv[yysp-1];
       end;
  63 : begin
         change_lang_id(MakeLangID(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v)); 
       end;
  64 : begin
         aktresource.Characteristics:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  65 : begin
         aktresource.Version:= yyv[yysp-0].yyrcnumtype.v; 
       end;
  66 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_MOVEABLE; 
       end;
  67 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_MOVEABLE; 
       end;
  68 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PURE; 
       end;
  69 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PURE; 
       end;
  70 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_PRELOAD; 
       end;
  71 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags and not MF_PRELOAD; 
       end;
  72 : begin
         aktresource.MemoryFlags:= aktresource.MemoryFlags or MF_DISCARDABLE; 
       end;
  73 : begin
         language:= MakeLangID(yyv[yysp-2].yyrcnumtype.v, yyv[yysp-0].yyrcnumtype.v); 
       end;
  74 : begin
         yyval := yyv[yysp-0];
       end;
  75 : begin
         yyval.yyrcnumtype:= str_to_num(yytext); 
       end;
  76 : begin
         yyval := yyv[yysp-0];
       end;
  77 : begin
         yyval.yyrcnumtype:= yyv[yysp-1].yyrcnumtype; 
       end;
  78 : begin
         yyval.yyrcnumtype.v:= not yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-0].yyrcnumtype.long; 
       end;
  79 : begin
         yyval.yyrcnumtype.v:= -yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-0].yyrcnumtype.long; 
       end;
  80 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v * yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  81 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v div Max(1, yyv[yysp-0].yyrcnumtype.v); yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  82 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v mod Max(1, yyv[yysp-0].yyrcnumtype.v); yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  83 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v + yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  84 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v - yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  85 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v and yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  86 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v xor yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  87 : begin
         yyval.yyrcnumtype.v:= yyv[yysp-2].yyrcnumtype.v or yyv[yysp-0].yyrcnumtype.v; yyval.yyrcnumtype.long:= yyv[yysp-2].yyrcnumtype.long or yyv[yysp-0].yyrcnumtype.long; 
       end;
  88 : begin
         string_new(yyval.yyrcstrtype, yytext, opt_code_page); 
       end;
  89 : begin
         yyval := yyv[yysp-0];
       end;
  90 : begin
         yyval.yyTFileStream:= TFileStream.Create(ExpandFileName(yyv[yysp-0].yyrcstrtype.v^), fmOpenRead or fmShareDenyWrite);
       end;
  91 : begin
         string_new_uni(yyval.yyrcstrtype, @strbuf[0], strbuflen, opt_code_page, true); 
       end;
  92 : begin
         string_new_uni(yyval.yyrcstrtype, @strbuf[0], strbuflen, CP_UTF16, true); 
       end;
  93 : begin
         string_new(yyval.yyrcstrtype, yytext, opt_code_page); 
       end;
  94 : begin
         string_new(yyval.yyrcstrtype, yytext, opt_code_page); 
       end;
  95 : begin
         string_new(yyval.yyrcstrtype, yytext, opt_code_page); 
       end;
  96 : begin
         yyval.yyTMemoryStream:= TMemoryStream.Create; 
       end;
  97 : begin
         yyval := yyv[yysp-1];
       end;
  98 : begin
         yyval.yyTMemoryStream:= yyv[yysp-1].yyTMemoryStream; 
       end;
  99 : begin
         yyval := yyv[yysp-3];
       end;
 100 : begin
         
         yyval.yyTMemoryStream:= yyv[yysp-0].yyTMemoryStream;
         
       end;
 101 : begin
         
         yyval.yyTMemoryStream:= yyv[yysp-1].yyTMemoryStream;
         raw_write_string(yyval.yyTMemoryStream, yyv[yysp-0].yyrcstrtype);
         
       end;
 102 : begin
         
         yyval.yyTMemoryStream:= yyv[yysp-1].yyTMemoryStream;
         raw_write_int(yyval.yyTMemoryStream, yyv[yysp-0].yyrcnumtype);
         
       end;
  end;
end(*yyaction*);

(* parse table: *)

type YYARec = record
                sym, act : Integer;
              end;
     YYRRec = record
                len, sym : Integer;
              end;

const

yynacts   = 643;
yyngotos  = 141;
yynstates = 180;
yynrules  = 102;

yya : array [1..yynacts] of YYARec = (
{ 0: }
{ 1: }
  ( sym: 0; act: 0 ),
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 263; act: 25 ),
  ( sym: 266; act: 26 ),
  ( sym: 267; act: 27 ),
  ( sym: 280; act: 28 ),
{ 2: }
{ 3: }
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
{ 11: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 263; act: 25 ),
  ( sym: 266; act: 26 ),
  ( sym: 277; act: 31 ),
  ( sym: 278; act: 32 ),
  ( sym: 279; act: 33 ),
  ( sym: 281; act: 34 ),
  ( sym: 282; act: 35 ),
  ( sym: 283; act: 36 ),
  ( sym: 284; act: 37 ),
  ( sym: 285; act: 38 ),
  ( sym: 286; act: 39 ),
  ( sym: 287; act: 40 ),
  ( sym: 288; act: 41 ),
  ( sym: 289; act: 42 ),
  ( sym: 290; act: 43 ),
  ( sym: 291; act: 44 ),
{ 12: }
{ 13: }
{ 14: }
{ 15: }
  ( sym: 37; act: 45 ),
  ( sym: 38; act: 46 ),
  ( sym: 42; act: 47 ),
  ( sym: 43; act: 48 ),
  ( sym: 45; act: 49 ),
  ( sym: 47; act: 50 ),
  ( sym: 94; act: 51 ),
  ( sym: 124; act: 52 ),
  ( sym: 0; act: -74 ),
  ( sym: 40; act: -74 ),
  ( sym: 44; act: -74 ),
  ( sym: 126; act: -74 ),
  ( sym: 258; act: -74 ),
  ( sym: 259; act: -74 ),
  ( sym: 260; act: -74 ),
  ( sym: 261; act: -74 ),
  ( sym: 262; act: -74 ),
  ( sym: 263; act: -74 ),
  ( sym: 264; act: -74 ),
  ( sym: 266; act: -74 ),
  ( sym: 267; act: -74 ),
  ( sym: 268; act: -74 ),
  ( sym: 269; act: -74 ),
  ( sym: 270; act: -74 ),
  ( sym: 271; act: -74 ),
  ( sym: 272; act: -74 ),
  ( sym: 273; act: -74 ),
  ( sym: 274; act: -74 ),
  ( sym: 275; act: -74 ),
  ( sym: 276; act: -74 ),
  ( sym: 277; act: -74 ),
  ( sym: 278; act: -74 ),
  ( sym: 279; act: -74 ),
  ( sym: 280; act: -74 ),
  ( sym: 281; act: -74 ),
  ( sym: 282; act: -74 ),
  ( sym: 283; act: -74 ),
  ( sym: 284; act: -74 ),
  ( sym: 285; act: -74 ),
  ( sym: 286; act: -74 ),
  ( sym: 287; act: -74 ),
  ( sym: 288; act: -74 ),
  ( sym: 289; act: -74 ),
  ( sym: 290; act: -74 ),
  ( sym: 291; act: -74 ),
  ( sym: 292; act: -74 ),
  ( sym: 293; act: -74 ),
  ( sym: 294; act: -74 ),
  ( sym: 295; act: -74 ),
  ( sym: 296; act: -74 ),
  ( sym: 297; act: -74 ),
  ( sym: 298; act: -74 ),
{ 16: }
{ 17: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 18: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 19: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
{ 25: }
{ 26: }
{ 27: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 28: }
{ 29: }
  ( sym: 259; act: -38 ),
  ( sym: 260; act: -38 ),
  ( sym: 261; act: -38 ),
  ( sym: 262; act: -38 ),
  ( sym: 263; act: -38 ),
  ( sym: 267; act: -38 ),
  ( sym: 268; act: -38 ),
  ( sym: 269; act: -38 ),
  ( sym: 270; act: -38 ),
  ( sym: 271; act: -38 ),
  ( sym: 272; act: -38 ),
  ( sym: 273; act: -38 ),
  ( sym: 274; act: -38 ),
  ( sym: 275; act: -38 ),
  ( sym: 276; act: -38 ),
  ( sym: 264; act: -40 ),
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
{ 41: }
{ 42: }
{ 43: }
{ 44: }
{ 45: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 46: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 47: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 48: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 49: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 50: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 51: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 52: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 53: }
  ( sym: 37; act: 45 ),
  ( sym: 38; act: 46 ),
  ( sym: 41; act: 72 ),
  ( sym: 42; act: 47 ),
  ( sym: 43; act: 48 ),
  ( sym: 45; act: 49 ),
  ( sym: 47; act: 50 ),
  ( sym: 94; act: 51 ),
  ( sym: 124; act: 52 ),
{ 54: }
{ 55: }
{ 56: }
  ( sym: 44; act: 73 ),
{ 57: }
{ 58: }
{ 59: }
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
  ( sym: 37; act: 45 ),
  ( sym: 42; act: 47 ),
  ( sym: 43; act: 48 ),
  ( sym: 45; act: 49 ),
  ( sym: 47; act: 50 ),
  ( sym: 0; act: -85 ),
  ( sym: 38; act: -85 ),
  ( sym: 40; act: -85 ),
  ( sym: 41; act: -85 ),
  ( sym: 44; act: -85 ),
  ( sym: 94; act: -85 ),
  ( sym: 124; act: -85 ),
  ( sym: 126; act: -85 ),
  ( sym: 258; act: -85 ),
  ( sym: 259; act: -85 ),
  ( sym: 260; act: -85 ),
  ( sym: 261; act: -85 ),
  ( sym: 262; act: -85 ),
  ( sym: 263; act: -85 ),
  ( sym: 264; act: -85 ),
  ( sym: 266; act: -85 ),
  ( sym: 267; act: -85 ),
  ( sym: 268; act: -85 ),
  ( sym: 269; act: -85 ),
  ( sym: 270; act: -85 ),
  ( sym: 271; act: -85 ),
  ( sym: 272; act: -85 ),
  ( sym: 273; act: -85 ),
  ( sym: 274; act: -85 ),
  ( sym: 275; act: -85 ),
  ( sym: 276; act: -85 ),
  ( sym: 277; act: -85 ),
  ( sym: 278; act: -85 ),
  ( sym: 279; act: -85 ),
  ( sym: 280; act: -85 ),
  ( sym: 281; act: -85 ),
  ( sym: 282; act: -85 ),
  ( sym: 283; act: -85 ),
  ( sym: 284; act: -85 ),
  ( sym: 285; act: -85 ),
  ( sym: 286; act: -85 ),
  ( sym: 287; act: -85 ),
  ( sym: 288; act: -85 ),
  ( sym: 289; act: -85 ),
  ( sym: 290; act: -85 ),
  ( sym: 291; act: -85 ),
  ( sym: 292; act: -85 ),
  ( sym: 293; act: -85 ),
  ( sym: 294; act: -85 ),
  ( sym: 295; act: -85 ),
  ( sym: 296; act: -85 ),
  ( sym: 297; act: -85 ),
  ( sym: 298; act: -85 ),
{ 66: }
{ 67: }
  ( sym: 37; act: 45 ),
  ( sym: 42; act: 47 ),
  ( sym: 47; act: 50 ),
  ( sym: 0; act: -83 ),
  ( sym: 38; act: -83 ),
  ( sym: 40; act: -83 ),
  ( sym: 41; act: -83 ),
  ( sym: 43; act: -83 ),
  ( sym: 44; act: -83 ),
  ( sym: 45; act: -83 ),
  ( sym: 94; act: -83 ),
  ( sym: 124; act: -83 ),
  ( sym: 126; act: -83 ),
  ( sym: 258; act: -83 ),
  ( sym: 259; act: -83 ),
  ( sym: 260; act: -83 ),
  ( sym: 261; act: -83 ),
  ( sym: 262; act: -83 ),
  ( sym: 263; act: -83 ),
  ( sym: 264; act: -83 ),
  ( sym: 266; act: -83 ),
  ( sym: 267; act: -83 ),
  ( sym: 268; act: -83 ),
  ( sym: 269; act: -83 ),
  ( sym: 270; act: -83 ),
  ( sym: 271; act: -83 ),
  ( sym: 272; act: -83 ),
  ( sym: 273; act: -83 ),
  ( sym: 274; act: -83 ),
  ( sym: 275; act: -83 ),
  ( sym: 276; act: -83 ),
  ( sym: 277; act: -83 ),
  ( sym: 278; act: -83 ),
  ( sym: 279; act: -83 ),
  ( sym: 280; act: -83 ),
  ( sym: 281; act: -83 ),
  ( sym: 282; act: -83 ),
  ( sym: 283; act: -83 ),
  ( sym: 284; act: -83 ),
  ( sym: 285; act: -83 ),
  ( sym: 286; act: -83 ),
  ( sym: 287; act: -83 ),
  ( sym: 288; act: -83 ),
  ( sym: 289; act: -83 ),
  ( sym: 290; act: -83 ),
  ( sym: 291; act: -83 ),
  ( sym: 292; act: -83 ),
  ( sym: 293; act: -83 ),
  ( sym: 294; act: -83 ),
  ( sym: 295; act: -83 ),
  ( sym: 296; act: -83 ),
  ( sym: 297; act: -83 ),
  ( sym: 298; act: -83 ),
{ 68: }
  ( sym: 37; act: 45 ),
  ( sym: 42; act: 47 ),
  ( sym: 47; act: 50 ),
  ( sym: 0; act: -84 ),
  ( sym: 38; act: -84 ),
  ( sym: 40; act: -84 ),
  ( sym: 41; act: -84 ),
  ( sym: 43; act: -84 ),
  ( sym: 44; act: -84 ),
  ( sym: 45; act: -84 ),
  ( sym: 94; act: -84 ),
  ( sym: 124; act: -84 ),
  ( sym: 126; act: -84 ),
  ( sym: 258; act: -84 ),
  ( sym: 259; act: -84 ),
  ( sym: 260; act: -84 ),
  ( sym: 261; act: -84 ),
  ( sym: 262; act: -84 ),
  ( sym: 263; act: -84 ),
  ( sym: 264; act: -84 ),
  ( sym: 266; act: -84 ),
  ( sym: 267; act: -84 ),
  ( sym: 268; act: -84 ),
  ( sym: 269; act: -84 ),
  ( sym: 270; act: -84 ),
  ( sym: 271; act: -84 ),
  ( sym: 272; act: -84 ),
  ( sym: 273; act: -84 ),
  ( sym: 274; act: -84 ),
  ( sym: 275; act: -84 ),
  ( sym: 276; act: -84 ),
  ( sym: 277; act: -84 ),
  ( sym: 278; act: -84 ),
  ( sym: 279; act: -84 ),
  ( sym: 280; act: -84 ),
  ( sym: 281; act: -84 ),
  ( sym: 282; act: -84 ),
  ( sym: 283; act: -84 ),
  ( sym: 284; act: -84 ),
  ( sym: 285; act: -84 ),
  ( sym: 286; act: -84 ),
  ( sym: 287; act: -84 ),
  ( sym: 288; act: -84 ),
  ( sym: 289; act: -84 ),
  ( sym: 290; act: -84 ),
  ( sym: 291; act: -84 ),
  ( sym: 292; act: -84 ),
  ( sym: 293; act: -84 ),
  ( sym: 294; act: -84 ),
  ( sym: 295; act: -84 ),
  ( sym: 296; act: -84 ),
  ( sym: 297; act: -84 ),
  ( sym: 298; act: -84 ),
{ 69: }
{ 70: }
  ( sym: 37; act: 45 ),
  ( sym: 38; act: 46 ),
  ( sym: 42; act: 47 ),
  ( sym: 43; act: 48 ),
  ( sym: 45; act: 49 ),
  ( sym: 47; act: 50 ),
  ( sym: 0; act: -86 ),
  ( sym: 40; act: -86 ),
  ( sym: 41; act: -86 ),
  ( sym: 44; act: -86 ),
  ( sym: 94; act: -86 ),
  ( sym: 124; act: -86 ),
  ( sym: 126; act: -86 ),
  ( sym: 258; act: -86 ),
  ( sym: 259; act: -86 ),
  ( sym: 260; act: -86 ),
  ( sym: 261; act: -86 ),
  ( sym: 262; act: -86 ),
  ( sym: 263; act: -86 ),
  ( sym: 264; act: -86 ),
  ( sym: 266; act: -86 ),
  ( sym: 267; act: -86 ),
  ( sym: 268; act: -86 ),
  ( sym: 269; act: -86 ),
  ( sym: 270; act: -86 ),
  ( sym: 271; act: -86 ),
  ( sym: 272; act: -86 ),
  ( sym: 273; act: -86 ),
  ( sym: 274; act: -86 ),
  ( sym: 275; act: -86 ),
  ( sym: 276; act: -86 ),
  ( sym: 277; act: -86 ),
  ( sym: 278; act: -86 ),
  ( sym: 279; act: -86 ),
  ( sym: 280; act: -86 ),
  ( sym: 281; act: -86 ),
  ( sym: 282; act: -86 ),
  ( sym: 283; act: -86 ),
  ( sym: 284; act: -86 ),
  ( sym: 285; act: -86 ),
  ( sym: 286; act: -86 ),
  ( sym: 287; act: -86 ),
  ( sym: 288; act: -86 ),
  ( sym: 289; act: -86 ),
  ( sym: 290; act: -86 ),
  ( sym: 291; act: -86 ),
  ( sym: 292; act: -86 ),
  ( sym: 293; act: -86 ),
  ( sym: 294; act: -86 ),
  ( sym: 295; act: -86 ),
  ( sym: 296; act: -86 ),
  ( sym: 297; act: -86 ),
  ( sym: 298; act: -86 ),
{ 71: }
  ( sym: 37; act: 45 ),
  ( sym: 38; act: 46 ),
  ( sym: 42; act: 47 ),
  ( sym: 43; act: 48 ),
  ( sym: 45; act: 49 ),
  ( sym: 47; act: 50 ),
  ( sym: 94; act: 51 ),
  ( sym: 0; act: -87 ),
  ( sym: 40; act: -87 ),
  ( sym: 41; act: -87 ),
  ( sym: 44; act: -87 ),
  ( sym: 124; act: -87 ),
  ( sym: 126; act: -87 ),
  ( sym: 258; act: -87 ),
  ( sym: 259; act: -87 ),
  ( sym: 260; act: -87 ),
  ( sym: 261; act: -87 ),
  ( sym: 262; act: -87 ),
  ( sym: 263; act: -87 ),
  ( sym: 264; act: -87 ),
  ( sym: 266; act: -87 ),
  ( sym: 267; act: -87 ),
  ( sym: 268; act: -87 ),
  ( sym: 269; act: -87 ),
  ( sym: 270; act: -87 ),
  ( sym: 271; act: -87 ),
  ( sym: 272; act: -87 ),
  ( sym: 273; act: -87 ),
  ( sym: 274; act: -87 ),
  ( sym: 275; act: -87 ),
  ( sym: 276; act: -87 ),
  ( sym: 277; act: -87 ),
  ( sym: 278; act: -87 ),
  ( sym: 279; act: -87 ),
  ( sym: 280; act: -87 ),
  ( sym: 281; act: -87 ),
  ( sym: 282; act: -87 ),
  ( sym: 283; act: -87 ),
  ( sym: 284; act: -87 ),
  ( sym: 285; act: -87 ),
  ( sym: 286; act: -87 ),
  ( sym: 287; act: -87 ),
  ( sym: 288; act: -87 ),
  ( sym: 289; act: -87 ),
  ( sym: 290; act: -87 ),
  ( sym: 291; act: -87 ),
  ( sym: 292; act: -87 ),
  ( sym: 293; act: -87 ),
  ( sym: 294; act: -87 ),
  ( sym: 295; act: -87 ),
  ( sym: 296; act: -87 ),
  ( sym: 297; act: -87 ),
  ( sym: 298; act: -87 ),
{ 72: }
{ 73: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 74: }
  ( sym: 264; act: 83 ),
  ( sym: 267; act: 84 ),
  ( sym: 268; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 270; act: 87 ),
  ( sym: 271; act: 88 ),
  ( sym: 272; act: 89 ),
  ( sym: 273; act: 90 ),
  ( sym: 274; act: 91 ),
  ( sym: 275; act: 92 ),
  ( sym: 276; act: 93 ),
{ 75: }
  ( sym: 264; act: 94 ),
  ( sym: 267; act: 84 ),
  ( sym: 268; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 270; act: 87 ),
  ( sym: 271; act: 88 ),
  ( sym: 272; act: 89 ),
  ( sym: 273; act: 90 ),
  ( sym: 274; act: 91 ),
  ( sym: 275; act: 92 ),
  ( sym: 276; act: 93 ),
{ 76: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 263; act: 25 ),
  ( sym: 267; act: 84 ),
  ( sym: 268; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 270; act: 87 ),
  ( sym: 271; act: 88 ),
  ( sym: 272; act: 89 ),
  ( sym: 273; act: 90 ),
  ( sym: 274; act: 91 ),
  ( sym: 275; act: 92 ),
  ( sym: 276; act: 93 ),
{ 77: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 263; act: 25 ),
  ( sym: 267; act: 84 ),
  ( sym: 268; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 270; act: 87 ),
  ( sym: 271; act: 88 ),
  ( sym: 272; act: 89 ),
  ( sym: 273; act: 90 ),
  ( sym: 274; act: 91 ),
  ( sym: 275; act: 92 ),
  ( sym: 276; act: 93 ),
{ 78: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 263; act: 25 ),
  ( sym: 267; act: 84 ),
  ( sym: 268; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 270; act: 87 ),
  ( sym: 271; act: 88 ),
  ( sym: 272; act: 89 ),
  ( sym: 273; act: 90 ),
  ( sym: 274; act: 91 ),
  ( sym: 275; act: 92 ),
  ( sym: 276; act: 93 ),
{ 79: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 263; act: 25 ),
  ( sym: 267; act: 84 ),
  ( sym: 268; act: 85 ),
  ( sym: 269; act: 86 ),
  ( sym: 270; act: 87 ),
  ( sym: 271; act: 88 ),
  ( sym: 272; act: 89 ),
  ( sym: 273; act: 90 ),
  ( sym: 274; act: 91 ),
  ( sym: 275; act: 92 ),
  ( sym: 276; act: 93 ),
{ 80: }
  ( sym: 264; act: 100 ),
  ( sym: 292; act: 101 ),
  ( sym: 293; act: 102 ),
  ( sym: 294; act: 103 ),
  ( sym: 295; act: 104 ),
  ( sym: 296; act: 105 ),
  ( sym: 297; act: 106 ),
  ( sym: 298; act: 107 ),
{ 81: }
{ 82: }
{ 83: }
{ 84: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 85: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 86: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 87: }
{ 88: }
{ 89: }
{ 90: }
{ 91: }
{ 92: }
{ 93: }
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
{ 99: }
{ 100: }
{ 101: }
  ( sym: 258; act: 20 ),
{ 102: }
  ( sym: 258; act: 20 ),
{ 103: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 104: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 105: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 106: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 107: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 108: }
  ( sym: 258; act: 20 ),
  ( sym: 265; act: 124 ),
{ 109: }
  ( sym: 44; act: 125 ),
{ 110: }
{ 111: }
{ 112: }
  ( sym: 258; act: 20 ),
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 263; act: 25 ),
  ( sym: 44; act: -100 ),
  ( sym: 265; act: -100 ),
{ 113: }
  ( sym: 44; act: 129 ),
  ( sym: 265; act: 130 ),
{ 114: }
  ( sym: 265; act: 131 ),
  ( sym: 299; act: 132 ),
{ 115: }
  ( sym: 44; act: 133 ),
{ 116: }
  ( sym: 44; act: 134 ),
{ 117: }
{ 118: }
{ 119: }
{ 120: }
{ 121: }
{ 122: }
{ 123: }
  ( sym: 44; act: 136 ),
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 263; act: 25 ),
{ 124: }
{ 125: }
  ( sym: 40; act: 17 ),
  ( sym: 45; act: 18 ),
  ( sym: 126; act: 19 ),
  ( sym: 258; act: 20 ),
{ 126: }
{ 127: }
{ 128: }
{ 129: }
{ 130: }
{ 131: }
{ 132: }
  ( sym: 261; act: 139 ),
  ( sym: 262; act: 140 ),
{ 133: }
  ( sym: 258; act: 20 ),
{ 134: }
  ( sym: 258; act: 20 ),
{ 135: }
{ 136: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 263; act: 25 ),
{ 137: }
{ 138: }
  ( sym: 258; act: 20 ),
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 263; act: 25 ),
  ( sym: 44; act: -100 ),
  ( sym: 265; act: -100 ),
{ 139: }
  ( sym: 264; act: 145 ),
{ 140: }
  ( sym: 264; act: 146 ),
{ 141: }
  ( sym: 44; act: 147 ),
{ 142: }
  ( sym: 44; act: 148 ),
{ 143: }
{ 144: }
{ 145: }
{ 146: }
  ( sym: 300; act: 151 ),
{ 147: }
  ( sym: 258; act: 20 ),
{ 148: }
  ( sym: 258; act: 20 ),
{ 149: }
  ( sym: 265; act: 154 ),
  ( sym: 299; act: 155 ),
{ 150: }
  ( sym: 265; act: 156 ),
{ 151: }
  ( sym: 263; act: 157 ),
{ 152: }
  ( sym: 44; act: 158 ),
{ 153: }
  ( sym: 44; act: 159 ),
{ 154: }
{ 155: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 263; act: 25 ),
{ 156: }
{ 157: }
  ( sym: 44; act: 161 ),
{ 158: }
  ( sym: 258; act: 20 ),
{ 159: }
  ( sym: 258; act: 20 ),
{ 160: }
  ( sym: 264; act: 164 ),
{ 161: }
  ( sym: 258; act: 20 ),
{ 162: }
{ 163: }
{ 164: }
{ 165: }
  ( sym: 44; act: 168 ),
  ( sym: 265; act: -35 ),
{ 166: }
  ( sym: 44; act: 169 ),
{ 167: }
{ 168: }
  ( sym: 258; act: 20 ),
{ 169: }
  ( sym: 258; act: 20 ),
{ 170: }
  ( sym: 265; act: 173 ),
  ( sym: 300; act: 174 ),
{ 171: }
  ( sym: 44; act: 175 ),
{ 172: }
{ 173: }
{ 174: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 263; act: 25 ),
{ 175: }
  ( sym: 258; act: 20 ),
{ 176: }
  ( sym: 44; act: 178 ),
{ 177: }
{ 178: }
  ( sym: 259; act: 21 ),
  ( sym: 260; act: 22 ),
  ( sym: 261; act: 23 ),
  ( sym: 262; act: 24 ),
  ( sym: 263; act: 25 )
{ 179: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -12; act: 1 ),
{ 1: }
  ( sym: -21; act: 2 ),
  ( sym: -20; act: 3 ),
  ( sym: -19; act: 4 ),
  ( sym: -18; act: 5 ),
  ( sym: -17; act: 6 ),
  ( sym: -16; act: 7 ),
  ( sym: -15; act: 8 ),
  ( sym: -14; act: 9 ),
  ( sym: -13; act: 10 ),
  ( sym: -7; act: 11 ),
  ( sym: -6; act: 12 ),
  ( sym: -5; act: 13 ),
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 16 ),
{ 2: }
{ 3: }
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
{ 11: }
  ( sym: -8; act: 29 ),
  ( sym: -7; act: 30 ),
  ( sym: -6; act: 12 ),
  ( sym: -5; act: 13 ),
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 16 ),
{ 12: }
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 53 ),
{ 18: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 54 ),
{ 19: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 55 ),
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
{ 25: }
{ 26: }
{ 27: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 56 ),
{ 28: }
  ( sym: -36; act: 57 ),
{ 29: }
  ( sym: -35; act: 58 ),
  ( sym: -34; act: 59 ),
{ 30: }
{ 31: }
  ( sym: -23; act: 60 ),
{ 32: }
  ( sym: -24; act: 61 ),
{ 33: }
  ( sym: -25; act: 62 ),
{ 34: }
  ( sym: -27; act: 63 ),
{ 35: }
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
{ 41: }
{ 42: }
{ 43: }
{ 44: }
{ 45: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 64 ),
{ 46: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 65 ),
{ 47: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 66 ),
{ 48: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 67 ),
{ 49: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 68 ),
{ 50: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 69 ),
{ 51: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 70 ),
{ 52: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 71 ),
{ 53: }
{ 54: }
{ 55: }
{ 56: }
{ 57: }
  ( sym: -22; act: 74 ),
{ 58: }
  ( sym: -22; act: 75 ),
{ 59: }
  ( sym: -22; act: 76 ),
{ 60: }
  ( sym: -22; act: 77 ),
{ 61: }
  ( sym: -22; act: 78 ),
{ 62: }
  ( sym: -22; act: 79 ),
{ 63: }
  ( sym: -26; act: 80 ),
{ 64: }
{ 65: }
{ 66: }
{ 67: }
{ 68: }
{ 69: }
{ 70: }
{ 71: }
{ 72: }
{ 73: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 81 ),
{ 74: }
  ( sym: -39; act: 82 ),
{ 75: }
  ( sym: -39; act: 82 ),
{ 76: }
  ( sym: -39; act: 82 ),
  ( sym: -11; act: 95 ),
  ( sym: -6; act: 96 ),
{ 77: }
  ( sym: -39; act: 82 ),
  ( sym: -11; act: 97 ),
  ( sym: -6; act: 96 ),
{ 78: }
  ( sym: -39; act: 82 ),
  ( sym: -11; act: 98 ),
  ( sym: -6; act: 96 ),
{ 79: }
  ( sym: -39; act: 82 ),
  ( sym: -11; act: 99 ),
  ( sym: -6; act: 96 ),
{ 80: }
{ 81: }
{ 82: }
{ 83: }
  ( sym: -37; act: 108 ),
{ 84: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 109 ),
{ 85: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 110 ),
{ 86: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 111 ),
{ 87: }
{ 88: }
{ 89: }
{ 90: }
{ 91: }
{ 92: }
{ 93: }
{ 94: }
  ( sym: -40; act: 112 ),
  ( sym: -9; act: 113 ),
{ 95: }
{ 96: }
{ 97: }
{ 98: }
{ 99: }
{ 100: }
  ( sym: -28; act: 114 ),
{ 101: }
  ( sym: -4; act: 115 ),
{ 102: }
  ( sym: -4; act: 116 ),
{ 103: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 117 ),
{ 104: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 118 ),
{ 105: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 119 ),
{ 106: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 120 ),
{ 107: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 121 ),
{ 108: }
  ( sym: -38; act: 122 ),
  ( sym: -4; act: 123 ),
{ 109: }
{ 110: }
{ 111: }
{ 112: }
  ( sym: -10; act: 126 ),
  ( sym: -6; act: 127 ),
  ( sym: -4; act: 128 ),
{ 113: }
{ 114: }
{ 115: }
{ 116: }
{ 117: }
{ 118: }
{ 119: }
{ 120: }
{ 121: }
{ 122: }
{ 123: }
  ( sym: -6; act: 135 ),
{ 124: }
{ 125: }
  ( sym: -4; act: 14 ),
  ( sym: -3; act: 15 ),
  ( sym: -2; act: 137 ),
{ 126: }
{ 127: }
{ 128: }
{ 129: }
  ( sym: -41; act: 138 ),
{ 130: }
{ 131: }
{ 132: }
{ 133: }
  ( sym: -4; act: 141 ),
{ 134: }
  ( sym: -4; act: 142 ),
{ 135: }
{ 136: }
  ( sym: -6; act: 143 ),
{ 137: }
{ 138: }
  ( sym: -10; act: 144 ),
  ( sym: -6; act: 127 ),
  ( sym: -4; act: 128 ),
{ 139: }
{ 140: }
{ 141: }
{ 142: }
{ 143: }
{ 144: }
{ 145: }
  ( sym: -29; act: 149 ),
{ 146: }
  ( sym: -30; act: 150 ),
{ 147: }
  ( sym: -4; act: 152 ),
{ 148: }
  ( sym: -4; act: 153 ),
{ 149: }
{ 150: }
{ 151: }
{ 152: }
{ 153: }
{ 154: }
{ 155: }
  ( sym: -6; act: 160 ),
{ 156: }
{ 157: }
{ 158: }
  ( sym: -4; act: 162 ),
{ 159: }
  ( sym: -4; act: 163 ),
{ 160: }
{ 161: }
  ( sym: -33; act: 165 ),
  ( sym: -4; act: 166 ),
{ 162: }
{ 163: }
{ 164: }
  ( sym: -32; act: 167 ),
{ 165: }
{ 166: }
{ 167: }
  ( sym: -31; act: 170 ),
{ 168: }
  ( sym: -4; act: 171 ),
{ 169: }
  ( sym: -4; act: 172 ),
{ 170: }
{ 171: }
{ 172: }
{ 173: }
{ 174: }
  ( sym: -6; act: 176 ),
{ 175: }
  ( sym: -4; act: 177 ),
{ 176: }
{ 177: }
{ 178: }
  ( sym: -6; act: 179 )
{ 179: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } -1,
{ 1: } 0,
{ 2: } -10,
{ 3: } -9,
{ 4: } -8,
{ 5: } -7,
{ 6: } -6,
{ 7: } -5,
{ 8: } -4,
{ 9: } -3,
{ 10: } -2,
{ 11: } 0,
{ 12: } -89,
{ 13: } -60,
{ 14: } -76,
{ 15: } 0,
{ 16: } -59,
{ 17: } 0,
{ 18: } 0,
{ 19: } 0,
{ 20: } -75,
{ 21: } -91,
{ 22: } -92,
{ 23: } -93,
{ 24: } -94,
{ 25: } -95,
{ 26: } -88,
{ 27: } 0,
{ 28: } -42,
{ 29: } 0,
{ 30: } -58,
{ 31: } -11,
{ 32: } -13,
{ 33: } -15,
{ 34: } -17,
{ 35: } -48,
{ 36: } -49,
{ 37: } -50,
{ 38: } -51,
{ 39: } -52,
{ 40: } -53,
{ 41: } -54,
{ 42: } -55,
{ 43: } -56,
{ 44: } -57,
{ 45: } 0,
{ 46: } 0,
{ 47: } 0,
{ 48: } 0,
{ 49: } 0,
{ 50: } 0,
{ 51: } 0,
{ 52: } 0,
{ 53: } 0,
{ 54: } -79,
{ 55: } -78,
{ 56: } 0,
{ 57: } -61,
{ 58: } -61,
{ 59: } -61,
{ 60: } -61,
{ 61: } -61,
{ 62: } -61,
{ 63: } -19,
{ 64: } -82,
{ 65: } 0,
{ 66: } -80,
{ 67: } 0,
{ 68: } 0,
{ 69: } -81,
{ 70: } 0,
{ 71: } 0,
{ 72: } -77,
{ 73: } 0,
{ 74: } 0,
{ 75: } 0,
{ 76: } 0,
{ 77: } 0,
{ 78: } 0,
{ 79: } 0,
{ 80: } 0,
{ 81: } -73,
{ 82: } -62,
{ 83: } -44,
{ 84: } 0,
{ 85: } 0,
{ 86: } 0,
{ 87: } -66,
{ 88: } -67,
{ 89: } -68,
{ 90: } -69,
{ 91: } -70,
{ 92: } -71,
{ 93: } -72,
{ 94: } -96,
{ 95: } -39,
{ 96: } -90,
{ 97: } -12,
{ 98: } -14,
{ 99: } -16,
{ 100: } -27,
{ 101: } 0,
{ 102: } 0,
{ 103: } 0,
{ 104: } 0,
{ 105: } 0,
{ 106: } 0,
{ 107: } 0,
{ 108: } 0,
{ 109: } 0,
{ 110: } -64,
{ 111: } -65,
{ 112: } 0,
{ 113: } 0,
{ 114: } 0,
{ 115: } 0,
{ 116: } 0,
{ 117: } -22,
{ 118: } -23,
{ 119: } -24,
{ 120: } -25,
{ 121: } -26,
{ 122: } -45,
{ 123: } 0,
{ 124: } -43,
{ 125: } 0,
{ 126: } -97,
{ 127: } -101,
{ 128: } -102,
{ 129: } -98,
{ 130: } -41,
{ 131: } -18,
{ 132: } 0,
{ 133: } 0,
{ 134: } 0,
{ 135: } -47,
{ 136: } 0,
{ 137: } -63,
{ 138: } 0,
{ 139: } 0,
{ 140: } 0,
{ 141: } 0,
{ 142: } 0,
{ 143: } -46,
{ 144: } -99,
{ 145: } -30,
{ 146: } 0,
{ 147: } 0,
{ 148: } 0,
{ 149: } 0,
{ 150: } 0,
{ 151: } 0,
{ 152: } 0,
{ 153: } 0,
{ 154: } -28,
{ 155: } 0,
{ 156: } -29,
{ 157: } 0,
{ 158: } 0,
{ 159: } 0,
{ 160: } 0,
{ 161: } 0,
{ 162: } -20,
{ 163: } -21,
{ 164: } -31,
{ 165: } 0,
{ 166: } 0,
{ 167: } -33,
{ 168: } 0,
{ 169: } 0,
{ 170: } 0,
{ 171: } 0,
{ 172: } -36,
{ 173: } -32,
{ 174: } 0,
{ 175: } 0,
{ 176: } 0,
{ 177: } -37,
{ 178: } 0,
{ 179: } -34
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 14,
{ 3: } 14,
{ 4: } 14,
{ 5: } 14,
{ 6: } 14,
{ 7: } 14,
{ 8: } 14,
{ 9: } 14,
{ 10: } 14,
{ 11: } 14,
{ 12: } 38,
{ 13: } 38,
{ 14: } 38,
{ 15: } 38,
{ 16: } 90,
{ 17: } 90,
{ 18: } 94,
{ 19: } 98,
{ 20: } 102,
{ 21: } 102,
{ 22: } 102,
{ 23: } 102,
{ 24: } 102,
{ 25: } 102,
{ 26: } 102,
{ 27: } 102,
{ 28: } 106,
{ 29: } 106,
{ 30: } 122,
{ 31: } 122,
{ 32: } 122,
{ 33: } 122,
{ 34: } 122,
{ 35: } 122,
{ 36: } 122,
{ 37: } 122,
{ 38: } 122,
{ 39: } 122,
{ 40: } 122,
{ 41: } 122,
{ 42: } 122,
{ 43: } 122,
{ 44: } 122,
{ 45: } 122,
{ 46: } 126,
{ 47: } 130,
{ 48: } 134,
{ 49: } 138,
{ 50: } 142,
{ 51: } 146,
{ 52: } 150,
{ 53: } 154,
{ 54: } 163,
{ 55: } 163,
{ 56: } 163,
{ 57: } 164,
{ 58: } 164,
{ 59: } 164,
{ 60: } 164,
{ 61: } 164,
{ 62: } 164,
{ 63: } 164,
{ 64: } 164,
{ 65: } 164,
{ 66: } 217,
{ 67: } 217,
{ 68: } 270,
{ 69: } 323,
{ 70: } 323,
{ 71: } 376,
{ 72: } 429,
{ 73: } 429,
{ 74: } 433,
{ 75: } 444,
{ 76: } 455,
{ 77: } 470,
{ 78: } 485,
{ 79: } 500,
{ 80: } 515,
{ 81: } 523,
{ 82: } 523,
{ 83: } 523,
{ 84: } 523,
{ 85: } 527,
{ 86: } 531,
{ 87: } 535,
{ 88: } 535,
{ 89: } 535,
{ 90: } 535,
{ 91: } 535,
{ 92: } 535,
{ 93: } 535,
{ 94: } 535,
{ 95: } 535,
{ 96: } 535,
{ 97: } 535,
{ 98: } 535,
{ 99: } 535,
{ 100: } 535,
{ 101: } 535,
{ 102: } 536,
{ 103: } 537,
{ 104: } 541,
{ 105: } 545,
{ 106: } 549,
{ 107: } 553,
{ 108: } 557,
{ 109: } 559,
{ 110: } 560,
{ 111: } 560,
{ 112: } 560,
{ 113: } 568,
{ 114: } 570,
{ 115: } 572,
{ 116: } 573,
{ 117: } 574,
{ 118: } 574,
{ 119: } 574,
{ 120: } 574,
{ 121: } 574,
{ 122: } 574,
{ 123: } 574,
{ 124: } 580,
{ 125: } 580,
{ 126: } 584,
{ 127: } 584,
{ 128: } 584,
{ 129: } 584,
{ 130: } 584,
{ 131: } 584,
{ 132: } 584,
{ 133: } 586,
{ 134: } 587,
{ 135: } 588,
{ 136: } 588,
{ 137: } 593,
{ 138: } 593,
{ 139: } 601,
{ 140: } 602,
{ 141: } 603,
{ 142: } 604,
{ 143: } 605,
{ 144: } 605,
{ 145: } 605,
{ 146: } 605,
{ 147: } 606,
{ 148: } 607,
{ 149: } 608,
{ 150: } 610,
{ 151: } 611,
{ 152: } 612,
{ 153: } 613,
{ 154: } 614,
{ 155: } 614,
{ 156: } 619,
{ 157: } 619,
{ 158: } 620,
{ 159: } 621,
{ 160: } 622,
{ 161: } 623,
{ 162: } 624,
{ 163: } 624,
{ 164: } 624,
{ 165: } 624,
{ 166: } 626,
{ 167: } 627,
{ 168: } 627,
{ 169: } 628,
{ 170: } 629,
{ 171: } 631,
{ 172: } 632,
{ 173: } 632,
{ 174: } 632,
{ 175: } 637,
{ 176: } 638,
{ 177: } 639,
{ 178: } 639,
{ 179: } 644
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 13,
{ 2: } 13,
{ 3: } 13,
{ 4: } 13,
{ 5: } 13,
{ 6: } 13,
{ 7: } 13,
{ 8: } 13,
{ 9: } 13,
{ 10: } 13,
{ 11: } 37,
{ 12: } 37,
{ 13: } 37,
{ 14: } 37,
{ 15: } 89,
{ 16: } 89,
{ 17: } 93,
{ 18: } 97,
{ 19: } 101,
{ 20: } 101,
{ 21: } 101,
{ 22: } 101,
{ 23: } 101,
{ 24: } 101,
{ 25: } 101,
{ 26: } 101,
{ 27: } 105,
{ 28: } 105,
{ 29: } 121,
{ 30: } 121,
{ 31: } 121,
{ 32: } 121,
{ 33: } 121,
{ 34: } 121,
{ 35: } 121,
{ 36: } 121,
{ 37: } 121,
{ 38: } 121,
{ 39: } 121,
{ 40: } 121,
{ 41: } 121,
{ 42: } 121,
{ 43: } 121,
{ 44: } 121,
{ 45: } 125,
{ 46: } 129,
{ 47: } 133,
{ 48: } 137,
{ 49: } 141,
{ 50: } 145,
{ 51: } 149,
{ 52: } 153,
{ 53: } 162,
{ 54: } 162,
{ 55: } 162,
{ 56: } 163,
{ 57: } 163,
{ 58: } 163,
{ 59: } 163,
{ 60: } 163,
{ 61: } 163,
{ 62: } 163,
{ 63: } 163,
{ 64: } 163,
{ 65: } 216,
{ 66: } 216,
{ 67: } 269,
{ 68: } 322,
{ 69: } 322,
{ 70: } 375,
{ 71: } 428,
{ 72: } 428,
{ 73: } 432,
{ 74: } 443,
{ 75: } 454,
{ 76: } 469,
{ 77: } 484,
{ 78: } 499,
{ 79: } 514,
{ 80: } 522,
{ 81: } 522,
{ 82: } 522,
{ 83: } 522,
{ 84: } 526,
{ 85: } 530,
{ 86: } 534,
{ 87: } 534,
{ 88: } 534,
{ 89: } 534,
{ 90: } 534,
{ 91: } 534,
{ 92: } 534,
{ 93: } 534,
{ 94: } 534,
{ 95: } 534,
{ 96: } 534,
{ 97: } 534,
{ 98: } 534,
{ 99: } 534,
{ 100: } 534,
{ 101: } 535,
{ 102: } 536,
{ 103: } 540,
{ 104: } 544,
{ 105: } 548,
{ 106: } 552,
{ 107: } 556,
{ 108: } 558,
{ 109: } 559,
{ 110: } 559,
{ 111: } 559,
{ 112: } 567,
{ 113: } 569,
{ 114: } 571,
{ 115: } 572,
{ 116: } 573,
{ 117: } 573,
{ 118: } 573,
{ 119: } 573,
{ 120: } 573,
{ 121: } 573,
{ 122: } 573,
{ 123: } 579,
{ 124: } 579,
{ 125: } 583,
{ 126: } 583,
{ 127: } 583,
{ 128: } 583,
{ 129: } 583,
{ 130: } 583,
{ 131: } 583,
{ 132: } 585,
{ 133: } 586,
{ 134: } 587,
{ 135: } 587,
{ 136: } 592,
{ 137: } 592,
{ 138: } 600,
{ 139: } 601,
{ 140: } 602,
{ 141: } 603,
{ 142: } 604,
{ 143: } 604,
{ 144: } 604,
{ 145: } 604,
{ 146: } 605,
{ 147: } 606,
{ 148: } 607,
{ 149: } 609,
{ 150: } 610,
{ 151: } 611,
{ 152: } 612,
{ 153: } 613,
{ 154: } 613,
{ 155: } 618,
{ 156: } 618,
{ 157: } 619,
{ 158: } 620,
{ 159: } 621,
{ 160: } 622,
{ 161: } 623,
{ 162: } 623,
{ 163: } 623,
{ 164: } 623,
{ 165: } 625,
{ 166: } 626,
{ 167: } 626,
{ 168: } 627,
{ 169: } 628,
{ 170: } 630,
{ 171: } 631,
{ 172: } 631,
{ 173: } 631,
{ 174: } 636,
{ 175: } 637,
{ 176: } 638,
{ 177: } 638,
{ 178: } 643,
{ 179: } 643
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 17,
{ 3: } 17,
{ 4: } 17,
{ 5: } 17,
{ 6: } 17,
{ 7: } 17,
{ 8: } 17,
{ 9: } 17,
{ 10: } 17,
{ 11: } 17,
{ 12: } 24,
{ 13: } 24,
{ 14: } 24,
{ 15: } 24,
{ 16: } 24,
{ 17: } 24,
{ 18: } 26,
{ 19: } 28,
{ 20: } 30,
{ 21: } 30,
{ 22: } 30,
{ 23: } 30,
{ 24: } 30,
{ 25: } 30,
{ 26: } 30,
{ 27: } 30,
{ 28: } 33,
{ 29: } 34,
{ 30: } 36,
{ 31: } 36,
{ 32: } 37,
{ 33: } 38,
{ 34: } 39,
{ 35: } 40,
{ 36: } 40,
{ 37: } 40,
{ 38: } 40,
{ 39: } 40,
{ 40: } 40,
{ 41: } 40,
{ 42: } 40,
{ 43: } 40,
{ 44: } 40,
{ 45: } 40,
{ 46: } 42,
{ 47: } 44,
{ 48: } 46,
{ 49: } 48,
{ 50: } 50,
{ 51: } 52,
{ 52: } 54,
{ 53: } 56,
{ 54: } 56,
{ 55: } 56,
{ 56: } 56,
{ 57: } 56,
{ 58: } 57,
{ 59: } 58,
{ 60: } 59,
{ 61: } 60,
{ 62: } 61,
{ 63: } 62,
{ 64: } 63,
{ 65: } 63,
{ 66: } 63,
{ 67: } 63,
{ 68: } 63,
{ 69: } 63,
{ 70: } 63,
{ 71: } 63,
{ 72: } 63,
{ 73: } 63,
{ 74: } 66,
{ 75: } 67,
{ 76: } 68,
{ 77: } 71,
{ 78: } 74,
{ 79: } 77,
{ 80: } 80,
{ 81: } 80,
{ 82: } 80,
{ 83: } 80,
{ 84: } 81,
{ 85: } 84,
{ 86: } 87,
{ 87: } 90,
{ 88: } 90,
{ 89: } 90,
{ 90: } 90,
{ 91: } 90,
{ 92: } 90,
{ 93: } 90,
{ 94: } 90,
{ 95: } 92,
{ 96: } 92,
{ 97: } 92,
{ 98: } 92,
{ 99: } 92,
{ 100: } 92,
{ 101: } 93,
{ 102: } 94,
{ 103: } 95,
{ 104: } 98,
{ 105: } 101,
{ 106: } 104,
{ 107: } 107,
{ 108: } 110,
{ 109: } 112,
{ 110: } 112,
{ 111: } 112,
{ 112: } 112,
{ 113: } 115,
{ 114: } 115,
{ 115: } 115,
{ 116: } 115,
{ 117: } 115,
{ 118: } 115,
{ 119: } 115,
{ 120: } 115,
{ 121: } 115,
{ 122: } 115,
{ 123: } 115,
{ 124: } 116,
{ 125: } 116,
{ 126: } 119,
{ 127: } 119,
{ 128: } 119,
{ 129: } 119,
{ 130: } 120,
{ 131: } 120,
{ 132: } 120,
{ 133: } 120,
{ 134: } 121,
{ 135: } 122,
{ 136: } 122,
{ 137: } 123,
{ 138: } 123,
{ 139: } 126,
{ 140: } 126,
{ 141: } 126,
{ 142: } 126,
{ 143: } 126,
{ 144: } 126,
{ 145: } 126,
{ 146: } 127,
{ 147: } 128,
{ 148: } 129,
{ 149: } 130,
{ 150: } 130,
{ 151: } 130,
{ 152: } 130,
{ 153: } 130,
{ 154: } 130,
{ 155: } 130,
{ 156: } 131,
{ 157: } 131,
{ 158: } 131,
{ 159: } 132,
{ 160: } 133,
{ 161: } 133,
{ 162: } 135,
{ 163: } 135,
{ 164: } 135,
{ 165: } 136,
{ 166: } 136,
{ 167: } 136,
{ 168: } 137,
{ 169: } 138,
{ 170: } 139,
{ 171: } 139,
{ 172: } 139,
{ 173: } 139,
{ 174: } 139,
{ 175: } 140,
{ 176: } 141,
{ 177: } 141,
{ 178: } 141,
{ 179: } 142
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 16,
{ 2: } 16,
{ 3: } 16,
{ 4: } 16,
{ 5: } 16,
{ 6: } 16,
{ 7: } 16,
{ 8: } 16,
{ 9: } 16,
{ 10: } 16,
{ 11: } 23,
{ 12: } 23,
{ 13: } 23,
{ 14: } 23,
{ 15: } 23,
{ 16: } 23,
{ 17: } 25,
{ 18: } 27,
{ 19: } 29,
{ 20: } 29,
{ 21: } 29,
{ 22: } 29,
{ 23: } 29,
{ 24: } 29,
{ 25: } 29,
{ 26: } 29,
{ 27: } 32,
{ 28: } 33,
{ 29: } 35,
{ 30: } 35,
{ 31: } 36,
{ 32: } 37,
{ 33: } 38,
{ 34: } 39,
{ 35: } 39,
{ 36: } 39,
{ 37: } 39,
{ 38: } 39,
{ 39: } 39,
{ 40: } 39,
{ 41: } 39,
{ 42: } 39,
{ 43: } 39,
{ 44: } 39,
{ 45: } 41,
{ 46: } 43,
{ 47: } 45,
{ 48: } 47,
{ 49: } 49,
{ 50: } 51,
{ 51: } 53,
{ 52: } 55,
{ 53: } 55,
{ 54: } 55,
{ 55: } 55,
{ 56: } 55,
{ 57: } 56,
{ 58: } 57,
{ 59: } 58,
{ 60: } 59,
{ 61: } 60,
{ 62: } 61,
{ 63: } 62,
{ 64: } 62,
{ 65: } 62,
{ 66: } 62,
{ 67: } 62,
{ 68: } 62,
{ 69: } 62,
{ 70: } 62,
{ 71: } 62,
{ 72: } 62,
{ 73: } 65,
{ 74: } 66,
{ 75: } 67,
{ 76: } 70,
{ 77: } 73,
{ 78: } 76,
{ 79: } 79,
{ 80: } 79,
{ 81: } 79,
{ 82: } 79,
{ 83: } 80,
{ 84: } 83,
{ 85: } 86,
{ 86: } 89,
{ 87: } 89,
{ 88: } 89,
{ 89: } 89,
{ 90: } 89,
{ 91: } 89,
{ 92: } 89,
{ 93: } 89,
{ 94: } 91,
{ 95: } 91,
{ 96: } 91,
{ 97: } 91,
{ 98: } 91,
{ 99: } 91,
{ 100: } 92,
{ 101: } 93,
{ 102: } 94,
{ 103: } 97,
{ 104: } 100,
{ 105: } 103,
{ 106: } 106,
{ 107: } 109,
{ 108: } 111,
{ 109: } 111,
{ 110: } 111,
{ 111: } 111,
{ 112: } 114,
{ 113: } 114,
{ 114: } 114,
{ 115: } 114,
{ 116: } 114,
{ 117: } 114,
{ 118: } 114,
{ 119: } 114,
{ 120: } 114,
{ 121: } 114,
{ 122: } 114,
{ 123: } 115,
{ 124: } 115,
{ 125: } 118,
{ 126: } 118,
{ 127: } 118,
{ 128: } 118,
{ 129: } 119,
{ 130: } 119,
{ 131: } 119,
{ 132: } 119,
{ 133: } 120,
{ 134: } 121,
{ 135: } 121,
{ 136: } 122,
{ 137: } 122,
{ 138: } 125,
{ 139: } 125,
{ 140: } 125,
{ 141: } 125,
{ 142: } 125,
{ 143: } 125,
{ 144: } 125,
{ 145: } 126,
{ 146: } 127,
{ 147: } 128,
{ 148: } 129,
{ 149: } 129,
{ 150: } 129,
{ 151: } 129,
{ 152: } 129,
{ 153: } 129,
{ 154: } 129,
{ 155: } 130,
{ 156: } 130,
{ 157: } 130,
{ 158: } 131,
{ 159: } 132,
{ 160: } 132,
{ 161: } 134,
{ 162: } 134,
{ 163: } 134,
{ 164: } 135,
{ 165: } 135,
{ 166: } 135,
{ 167: } 136,
{ 168: } 137,
{ 169: } 138,
{ 170: } 138,
{ 171: } 138,
{ 172: } 138,
{ 173: } 138,
{ 174: } 139,
{ 175: } 140,
{ 176: } 140,
{ 177: } 140,
{ 178: } 141,
{ 179: } 141
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 0; sym: -12 ),
{ 2: } ( len: 2; sym: -12 ),
{ 3: } ( len: 1; sym: -13 ),
{ 4: } ( len: 1; sym: -13 ),
{ 5: } ( len: 1; sym: -14 ),
{ 6: } ( len: 1; sym: -14 ),
{ 7: } ( len: 1; sym: -14 ),
{ 8: } ( len: 1; sym: -14 ),
{ 9: } ( len: 1; sym: -14 ),
{ 10: } ( len: 1; sym: -14 ),
{ 11: } ( len: 0; sym: -23 ),
{ 12: } ( len: 5; sym: -17 ),
{ 13: } ( len: 0; sym: -24 ),
{ 14: } ( len: 5; sym: -18 ),
{ 15: } ( len: 0; sym: -25 ),
{ 16: } ( len: 5; sym: -19 ),
{ 17: } ( len: 0; sym: -27 ),
{ 18: } ( len: 7; sym: -20 ),
{ 19: } ( len: 0; sym: -26 ),
{ 20: } ( len: 9; sym: -26 ),
{ 21: } ( len: 9; sym: -26 ),
{ 22: } ( len: 3; sym: -26 ),
{ 23: } ( len: 3; sym: -26 ),
{ 24: } ( len: 3; sym: -26 ),
{ 25: } ( len: 3; sym: -26 ),
{ 26: } ( len: 3; sym: -26 ),
{ 27: } ( len: 0; sym: -28 ),
{ 28: } ( len: 6; sym: -28 ),
{ 29: } ( len: 6; sym: -28 ),
{ 30: } ( len: 0; sym: -29 ),
{ 31: } ( len: 0; sym: -32 ),
{ 32: } ( len: 7; sym: -29 ),
{ 33: } ( len: 0; sym: -31 ),
{ 34: } ( len: 5; sym: -31 ),
{ 35: } ( len: 4; sym: -30 ),
{ 36: } ( len: 3; sym: -33 ),
{ 37: } ( len: 5; sym: -33 ),
{ 38: } ( len: 0; sym: -34 ),
{ 39: } ( len: 5; sym: -21 ),
{ 40: } ( len: 0; sym: -35 ),
{ 41: } ( len: 7; sym: -21 ),
{ 42: } ( len: 0; sym: -36 ),
{ 43: } ( len: 6; sym: -16 ),
{ 44: } ( len: 0; sym: -37 ),
{ 45: } ( len: 2; sym: -37 ),
{ 46: } ( len: 3; sym: -38 ),
{ 47: } ( len: 2; sym: -38 ),
{ 48: } ( len: 1; sym: -8 ),
{ 49: } ( len: 1; sym: -8 ),
{ 50: } ( len: 1; sym: -8 ),
{ 51: } ( len: 1; sym: -8 ),
{ 52: } ( len: 1; sym: -8 ),
{ 53: } ( len: 1; sym: -8 ),
{ 54: } ( len: 1; sym: -8 ),
{ 55: } ( len: 1; sym: -8 ),
{ 56: } ( len: 1; sym: -8 ),
{ 57: } ( len: 1; sym: -8 ),
{ 58: } ( len: 1; sym: -8 ),
{ 59: } ( len: 1; sym: -7 ),
{ 60: } ( len: 1; sym: -7 ),
{ 61: } ( len: 0; sym: -22 ),
{ 62: } ( len: 2; sym: -22 ),
{ 63: } ( len: 4; sym: -39 ),
{ 64: } ( len: 2; sym: -39 ),
{ 65: } ( len: 2; sym: -39 ),
{ 66: } ( len: 1; sym: -39 ),
{ 67: } ( len: 1; sym: -39 ),
{ 68: } ( len: 1; sym: -39 ),
{ 69: } ( len: 1; sym: -39 ),
{ 70: } ( len: 1; sym: -39 ),
{ 71: } ( len: 1; sym: -39 ),
{ 72: } ( len: 1; sym: -39 ),
{ 73: } ( len: 4; sym: -15 ),
{ 74: } ( len: 1; sym: -2 ),
{ 75: } ( len: 1; sym: -4 ),
{ 76: } ( len: 1; sym: -3 ),
{ 77: } ( len: 3; sym: -3 ),
{ 78: } ( len: 2; sym: -3 ),
{ 79: } ( len: 2; sym: -3 ),
{ 80: } ( len: 3; sym: -3 ),
{ 81: } ( len: 3; sym: -3 ),
{ 82: } ( len: 3; sym: -3 ),
{ 83: } ( len: 3; sym: -3 ),
{ 84: } ( len: 3; sym: -3 ),
{ 85: } ( len: 3; sym: -3 ),
{ 86: } ( len: 3; sym: -3 ),
{ 87: } ( len: 3; sym: -3 ),
{ 88: } ( len: 1; sym: -5 ),
{ 89: } ( len: 1; sym: -5 ),
{ 90: } ( len: 1; sym: -11 ),
{ 91: } ( len: 1; sym: -6 ),
{ 92: } ( len: 1; sym: -6 ),
{ 93: } ( len: 1; sym: -6 ),
{ 94: } ( len: 1; sym: -6 ),
{ 95: } ( len: 1; sym: -6 ),
{ 96: } ( len: 0; sym: -40 ),
{ 97: } ( len: 2; sym: -9 ),
{ 98: } ( len: 0; sym: -41 ),
{ 99: } ( len: 4; sym: -9 ),
{ 100: } ( len: 0; sym: -10 ),
{ 101: } ( len: 1; sym: -10 ),
{ 102: } ( len: 1; sym: -10 )
);


const _error = 256; (* error token *)

function yyact(state, sym : Integer; var act : Integer) : Boolean;
  (* search action table *)
  var k : Integer;
  begin
    k := yyal[state];
    while (k<=yyah[state]) and (yya[k].sym<>sym) do inc(k);
    if k>yyah[state] then
      yyact := false
    else
      begin
        act := yya[k].act;
        yyact := true;
      end;
  end(*yyact*);

function yygoto(state, sym : Integer; var nstate : Integer) : Boolean;
  (* search goto table *)
  var k : Integer;
  begin
    k := yygl[state];
    while (k<=yygh[state]) and (yyg[k].sym<>sym) do inc(k);
    if k>yygh[state] then
      yygoto := false
    else
      begin
        nstate := yyg[k].act;
        yygoto := true;
      end;
  end(*yygoto*);

label parse, next, error, errlab, shift, reduce, accept, abort;

begin(*yyparse*)

  (* initialize: *)

  yystate := 0; yychar := -1; yynerrs := 0; yyerrflag := 0; yysp := 0;

{$ifdef yydebug}
  yydebug := true;
{$else}
  yydebug := false;
{$endif}

parse:

  (* push state and value: *)

  inc(yysp);
  if yysp>yymaxdepth then
    begin
      yyerror('yyparse stack overflow');
      goto abort;
    end;
  yys[yysp] := yystate; yyv[yysp] := yyval;

next:

  if (yyd[yystate]=0) and (yychar=-1) then
    (* get next symbol *)
    begin
      yychar := yylex; if yychar<0 then yychar := 0;
    end;

  if yydebug then writeln('state ', yystate, ', char ', yychar);

  (* determine parse action: *)

  yyn := yyd[yystate];
  if yyn<>0 then goto reduce; (* simple state *)

  (* no default action; search parse table *)

  if not yyact(yystate, yychar, yyn) then goto error
  else if yyn>0 then                      goto shift
  else if yyn<0 then                      goto reduce
  else                                    goto accept;

error:

  (* error; start error recovery: *)

  if yyerrflag=0 then yyerror('syntax error');

errlab:

  if yyerrflag=0 then inc(yynerrs);     (* new error *)

  if yyerrflag<=2 then                  (* incomplete recovery; try again *)
    begin
      yyerrflag := 3;
      (* uncover a state with shift action on error token *)
      while (yysp>0) and not ( yyact(yys[yysp], _error, yyn) and
                               (yyn>0) ) do
        begin
          if yydebug then
            if yysp>1 then
              writeln('error recovery pops state ', yys[yysp], ', uncovers ',
                      yys[yysp-1])
            else
              writeln('error recovery fails ... abort');
          dec(yysp);
        end;
      if yysp=0 then goto abort; (* parser has fallen from stack; abort *)
      yystate := yyn;            (* simulate shift on error *)
      goto parse;
    end
  else                                  (* no shift yet; discard symbol *)
    begin
      if yydebug then writeln('error recovery discards char ', yychar);
      if yychar=0 then goto abort; (* end of input; abort *)
      yychar := -1; goto next;     (* clear lookahead char and try again *)
    end;

shift:

  (* go to new state, clear lookahead character: *)

  yystate := yyn; yychar := -1; yyval := yylval;
  if yyerrflag>0 then dec(yyerrflag);

  goto parse;

reduce:

  (* execute action, pop rule from stack, and go to next state: *)

  if yydebug then writeln('reduce ', -yyn);

  yyflag := yyfnone; yyaction(-yyn);
  dec(yysp, yyr[-yyn].len);
  if yygoto(yys[yysp], yyr[-yyn].sym, yyn) then yystate := yyn;

  (* handle action calls to yyaccept, yyabort and yyerror: *)

  case yyflag of
    yyfaccept : goto accept;
    yyfabort  : goto abort;
    yyferror  : goto errlab;
  end;

  goto parse;

accept:

  yyparse := 0; exit;

abort:

  yyparse := 1; exit;

end(*yyparse*);


{$I rclex.inc}
begin
  bufptr:= 0;
  lexlib.get_char:= @rc_get_char;
  lexlib.unget_char:= @rc_unget_char;
end.
