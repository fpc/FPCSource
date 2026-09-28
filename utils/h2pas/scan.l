%{
{
    Copyright (c) 1998-2000 by Florian Klaempfl

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

 ****************************************************************************}


unit scan;
{$H+}
{$GOTO ON}

  interface

  uses
   h2plexlib,h2pyacclib;

    function yylex : integer;
    function act_token : string;

  implementation

    uses
       h2poptions, h2pconst, scanbase;

%}

D [0-9]
%%

"/*"                    HandleMultiLineComment;
"//"                    HandleSingleLineComment;
\"[^\"]*\"              if NotInCPlusBlock then return(CSTRING) else skip_until_eol;
\'[^\']*\'              if NotInCPlusBlock then return(CSTRING) else skip_until_eol;
"L"\"[^\"]*\"           CheckLongString;
"L"\'[^\']*\'           CheckLongString;
{D}+[Uu]?[Ll]?[Ll]?     HandleLongInteger;
"0x"[0-9A-Fa-f]*[Uu]?[Ll]?[Ll]?
                        HandleHexLongInteger;
{D}+(\.{D}+)?([Ee][+-]?{D}+)?
                        HandleNumber;
"->"                    HandleDeref;
"-"                     if NotInCPlusBlock then return(MINUS) else skip_until_eol;
"=="                    if NotInCPlusBlock then return(EQUAL) else skip_until_eol;
"!="                    if NotInCPlusBlock then return(UNEQUAL) else skip_until_eol;
">="                    if NotInCPlusBlock then return(GTE) else skip_until_eol;
"<="                    if NotInCPlusBlock then return(LTE) else skip_until_eol;
">>"                    if NotInCPlusBlock then return(_SHR) else skip_until_eol;
"##"                    if NotInCPlusBlock then return(STICK) else skip_until_eol;
"<<"                    if NotInCPlusBlock then return(_SHL) else skip_until_eol;
">"                     if NotInCPlusBlock then return(GT) else skip_until_eol;
"<"                     if NotInCPlusBlock then return(LT) else skip_until_eol;
"|"                     if NotInCPlusBlock then return(_OR) else skip_until_eol;
"&"                     if NotInCPlusBlock then return(_AND) else skip_until_eol;
"~"                     if NotInCPlusBlock then return(_NOT) else skip_until_eol; (* inverse, but handled as not operation *)
"!"                     if NotInCPlusBlock then return(_NOT) else skip_until_eol;
"/"                     if NotInCPlusBlock then return(_SLASH) else skip_until_eol;
"+"                     if NotInCPlusBlock then return(_PLUS) else skip_until_eol;
"?"                     if NotInCPlusBlock then return(QUESTIONMARK) else skip_until_eol;
":"                     if NotInCPlusBlock then return(COLON) else skip_until_eol;
","                     if NotInCPlusBlock then return(COMMA) else skip_until_eol;
"["                     if NotInCPlusBlock then return(LECKKLAMMER) else skip_until_eol;
"]"                     if NotInCPlusBlock then return(RECKKLAMMER) else skip_until_eol;
"("                     if NotInCPlusBlock then
                           begin
                             inc(arglevel);
                             return(LKLAMMER);
                           end
                        else
                           skip_until_eol;
")"                     if NotInCPlusBlock then
                           begin
                             dec(arglevel);
                             return(RKLAMMER);
                           end
                         else
                           skip_until_eol;
"*"                     if NotInCPlusBlock then return(STAR) else skip_until_eol;
"..."                   if NotInCPlusBlock then return(ELLIPSIS) else skip_until_eol;
"."                     if NotInCPlusBlock then
                          if in_define then
                            return(POINT)
                          else
                            return(256);
"="                     if NotInCPlusBlock then return(_ASSIGN) else skip_until_eol;
"extern"                if NotInCPlusBlock then return(EXTERN) else skip_until_eol;
"STDCALL"               HandleCallingConvention(STDCALL);
"CDECL"                 HandleCallingConvention(CDECL);
"PASCAL"                HandleCallingConvention(PASCAL);
"PACKED"                HandleCallingConvention(_PACKED);
"WINAPI"                HandleCallingConvention(WINAPI);
"SYS_TRAP"              HandlePalmPilotCallingConvention;
"WINGDIAPI"             HandleCallingConvention(WINGDIAPI);
"CALLBACK"              HandleCallingConvention(CALLBACK);
"EXPENTRY"              HandleCallingConvention(CALLBACK);
"void"                  if NotInCPlusBlock then return(VOID) else skip_until_eol;
"VOID"                  if NotInCPlusBlock then return(VOID) else skip_until_eol;
"#ifdef"[ \t]*"__cplusplus"[ \t]*\n"extern \"C\" {"\n"#endif"
                        HandlePreProcStripConditional(false);
"#ifdef"[ \t]*"cplusplus"[ \t]*\n"extern \"C\" {"\n"#endif"
                        HandlePreProcStripConditional(false);
"#ifdef"[ \t]*"__cplusplus"[ \t]*\n"}"\n"#endif"
                        HandlePreProcStripConditional(true);
"#ifdef"[ \t]*"cplusplus"[ \t]*\n"}"\n"#endif"
                        HandlePreProcStripConditional(true);
"#ifdef"[ \t]*"cplusplus"[ \t]*
                        EnterCplusPlus;
"#ifdef"[ \t]*"__cplusplus"[ \t]*
                        EnterCplusPlus;
"#ifdef"[ \t]           HandlePreProcIfDef;
"#"[ \t]*"else"         HandlePreProcElse;
"#"[ \t]*"endif"        HandlePreProcEndif;
"#"[ \t]*"elif"         HandlePreProcElIf;
"#"[ \t]*"undef"        HandlePreProcUndef;
"#"[ \t]*"error"        HandlePreProcError;
"#"[ \t]*"include"      HandlePreProcInclude;
"#"[ \t]*"if"           HandlePreProcIf;
"# "[0-9]+" "           HandlePreProcLineInfo;
"#"[ \t]*"pragma"       HandlePreProcPragma;
"#"[ \t]*"define"       HandlePreProcDefine;
"char"                  if NotInCPlusBlock then return(_CHAR) else skip_until_eol;
"union"                 if NotInCPlusBlock then return(UNION) else skip_until_eol;
"enum"                  if NotInCPlusBlock then return(ENUM) else skip_until_eol;
"struct"                if NotInCPlusBlock then return(STRUCT) else skip_until_eol;
"{"                     if NotInCPlusBlock then return(LGKLAMMER) else skip_until_eol;
"}"                     if NotInCPlusBlock then return(RGKLAMMER) else skip_until_eol;
"typedef"               if NotInCPlusBlock then return(TYPEDEF) else skip_until_eol;
"int"                   if NotInCPlusBlock then return(INT) else skip_until_eol;
"short"                 if NotInCPlusBlock then return(SHORT) else skip_until_eol;
"long"                  if NotInCPlusBlock then return(LONG) else skip_until_eol;
"signed"                if NotInCPlusBlock then return(SIGNED) else skip_until_eol;
"unsigned"              if NotInCPlusBlock then return(UNSIGNED) else skip_until_eol;
"__int8"                if NotInCPlusBlock then return(INT8) else skip_until_eol;
"__int16"               if NotInCPlusBlock then return(INT16) else skip_until_eol;
"__int32"               if NotInCPlusBlock then return(INT32) else skip_until_eol;
"__int64"               if NotInCPlusBlock then return(INT64) else skip_until_eol;
"int8"                  if NotInCPlusBlock then return(INT8) else skip_until_eol;
"int16"                 if NotInCPlusBlock then return(INT16) else skip_until_eol;
"int32"                 if NotInCPlusBlock then return(INT32) else skip_until_eol;
"int64"                 if NotInCPlusBlock then return(INT64) else skip_until_eol;
"float"                 if NotInCPlusBlock then return(FLOAT) else skip_until_eol;
"double"                if NotInCPlusBlock then return(_DOUBLE) else skip_until_eol;
"const"                 if NotInCPlusBlock then return(_CONST) else skip_until_eol;
"CONST"                 if NotInCPlusBlock then return(_CONST) else skip_until_eol;
"FAR"                   if NotInCPlusBlock then return(_FAR) else skip_until_eol;
"far"                   if NotInCPlusBlock then return(_FAR) else skip_until_eol;
"NEAR"                  if NotInCPlusBlock then return(_NEAR) else skip_until_eol;
"near"                  if NotInCPlusBlock then return(_NEAR) else skip_until_eol;
"HUGE"                  if NotInCPlusBlock then return(_HUGE) else skip_until_eol;
"huge"                  if NotInCPlusBlock then return(_HUGE) else skip_until_eol;
"while"                 if NotInCPlusBlock then return(_WHILE) else skip_until_eol;
[A-Za-z_][A-Za-z0-9_]*  HandleIdentifier;
";"                     if NotInCPlusBlock then return(SEMICOLON) else skip_until_eol;
[ \f\t]                 HandleWhiteSpace;
\n                      HandleEOL;
\\$                     HandleContinuation;
.                       HandleIllegalCharacter;
%%


function act_token : string;
begin
  act_token:=yytext;
end;

end.
