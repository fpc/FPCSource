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

uses strings,scanbase, h2plexlib,h2pyacclib;

function yylex : integer;
function act_token : string;

implementation

uses h2poptions, converu;

Procedure HandleCPlusPlus (aValue : integer); inline;

begin
  if NotInCPlusBlock then return(aValue) else skip_until_eol;
end;

%}

D [0-9]
%%

"/*"                    HandleMultiLineComment;
"//"                    HandleSingleLineComment;
\"[^\"]*\"              HandleCPlusPlus(CSTRING);
\'[^\']*\'              HandleCPlusPlus(CSTRING);
"L"\"[^\"]*\"           CheckLongString;
"L"\'[^\']*\'           CheckLongString;
{D}+[Uu]?[Ll]?[Ll]?     HandleLongInteger;
"0x"[0-9A-Fa-f]*[Uu]?[Ll]?[Ll]? 
                        HandleHexLongInteger;
{D}+(\.{D}+)?([Ee][+-]?{D}+)?   
                        HandleNumber;
"->"                    HandleDeref;
"-"                     HandleCPlusPlus(MINUS);
"=="                    HandleCPlusPlus(EQUAL);
"!="                    HandleCPlusPlus(UNEQUAL);
">="                    HandleCPlusPlus(GTE);
"<="                    HandleCPlusPlus(LTE);
">>"                    HandleCPlusPlus(_SHR);
"##"                    HandleCPlusPlus(STICK);
"<<"                    HandleCPlusPlus(_SHL);
">"                     HandleCPlusPlus(GT);
"<"                     HandleCPlusPlus(LT);
"|"                     HandleCPlusPlus(_OR);
"&"                     HandleCPlusPlus(_AND);
"~"                     HandleCPlusPlus(_NOT); (* inverse, but handled as not operation *)
"!"                     HandleCPlusPlus(_NOT);
"/"                     HandleCPlusPlus(_SLASH);
"+"                     HandleCPlusPlus(_PLUS);
"?"                     HandleCPlusPlus(QUESTIONMARK);
":"                     HandleCPlusPlus(COLON);
","                     HandleCPlusPlus(COMMA);
"["                     HandleCPlusPlus(LECKKLAMMER);
"]"                     HandleCPlusPlus(RECKKLAMMER);
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
"*"                     HandleCPlusPlus(STAR);
"..."                   HandleCPlusPlus(ELLIPSIS);
"."                     if NotInCPlusBlock then
                          if in_define then
                            return(POINT)
                          else
                            return(256);
"="                     HandleCPlusPlus(_ASSIGN);
"extern"                HandleCPlusPlus(EXTERN);
"STDCALL"               HandleCallingConvention(STDCALL);
"CDECL"                 HandleCallingConvention(CDECL);
"PASCAL"                HandleCallingConvention(PASCAL);
"PACKED"                HandleCallingConvention(_PACKED); // Not exactly calling convention, but follows same logic.
"WINAPI"                HandleCallingConvention(WINAPI);
"SYS_TRAP"              HandlePalmPilotCallingConvention;
"WINGDIAPI"             HandleCallingConvention(WINGDIAPI);
"CALLBACK"              HandleCallingConvention(CALLBACK);
"EXPENTRY"              HandleCallingConvention(CALLBACK); // Callback, really ?
"void"                  HandleCPlusPlus(VOID);
"VOID"                  HandleCPlusPlus(VOID);
"#ifdef"[ \t]*"__cplusplus"[ \t]*\n"extern \"C\" {"\n"#endif"
                        HandlePreProcStripConditional(False);
"#ifdef"[ \t]*"cplusplus"[ \t]*\n"extern \"C\" {"\n"#endif"
                        HandlePreProcStripConditional(False);
"#ifdef"[ \t]*"__cplusplus"[ \t]*\n"}"\n"#endif"
                        HandlePreProcStripConditional(True);
"#ifdef"[ \t]*"cplusplus"[ \t]*\n"}"\n"#endif"
                        HandlePreProcStripConditional(True);
"#ifdef"[ \t]*"cplusplus"[ \t]* 
                        EnterCplusplus;
"#ifdef"[ \t]*"__cplusplus"[ \t]* 
                        EnterCplusplus;
"#ifdef"[ \t]           HandlePreProcIfDef;
"#"[ \t]*"else"         HandlePreProcElse;
"#"[ \t]*"endif"        HandlePreProcEndif;
"#"[ \t]*"elif"         HandlePreProcElif;
"#"[ \t]*"undef"        HandlePreProcUndef;
"#"[ \t]*"error"        HandlePreProcError;
"#"[ \t]*"include"      HandlePreProcInclude;
"#"[ \t]*"if"           HandlePreProcIf;
"# "[0-9]+" "           HandlePreProcLineInfo;
"#"[ \t]*"pragma"       HandlePreprocPragma;
"#"[ \t]*"define"       HandlePreProcDefine;
"char"                  HandleCPlusPlus(_CHAR);
"union"                 HandleCPlusPlus(UNION);
"enum"                  HandleCPlusPlus(ENUM);
"struct"                HandleCPlusPlus(STRUCT);
"{"                     HandleCPlusPlus(LGKLAMMER);
"}"                     HandleCPlusPlus(RGKLAMMER);
"typedef"               HandleCPlusPlus(TYPEDEF);
"int"                   HandleCPlusPlus(INT);
"short"                 HandleCPlusPlus(SHORT);
"long"                  HandleCPlusPlus(LONG);
"signed"                HandleCPlusPlus(SIGNED);
"unsigned"              HandleCPlusPlus(UNSIGNED);
"__int8"                HandleCPlusPlus(INT8);
"__int16"               HandleCPlusPlus(INT16);
"__int32"               HandleCPlusPlus(INT32);
"__int64"               HandleCPlusPlus(INT64);
"int8"                  HandleCPlusPlus(INT8);
"int16"                 HandleCPlusPlus(INT16);
"int32"                 HandleCPlusPlus(INT32);
"int64"                 HandleCPlusPlus(INT64);
"float"                 HandleCPlusPlus(FLOAT);
"const"                 HandleCPlusPlus(_CONST);
"CONST"                 HandleCPlusPlus(_CONST);
"FAR"                   HandleCPlusPlus(_FAR);
"far"                   HandleCPlusPlus(_FAR);
"NEAR"                  HandleCPlusPlus(_NEAR);
"near"                  HandleCPlusPlus(_NEAR);
"HUGE"                  HandleCPlusPlus(_HUGE);
"huge"                  HandleCPlusPlus(_HUGE);
"while"                 HandleCPlusPlus(_WHILE);
[A-Za-z_][A-Za-z0-9_]*  HandleIdentifier;
";"                     HandleCPlusPlus(SEMICOLON);
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