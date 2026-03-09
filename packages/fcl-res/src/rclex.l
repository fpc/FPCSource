%{
var
  kwtmp: integer;

const
  KeywordDefs: array [0..61] of TIdentMapEntry = (
    // attribs
    (Value: _LANGUAGE;             Name: 'LANGUAGE'),
    (Value: _CHARACTERISTICS;      Name: 'CHARACTERISTICS'),
    (Value: _VERSION;              Name: 'VERSION'),
    (Value: _MOVEABLE;             Name: 'MOVEABLE'),
    (Value: _FIXED;                Name: 'FIXED'),
    (Value: _PURE;                 Name: 'PURE'),
    (Value: _IMPURE;               Name: 'IMPURE'),
    (Value: _PRELOAD;              Name: 'PRELOAD'),
    (Value: _LOADONCALL;           Name: 'LOADONCALL'),
    (Value: _DISCARDABLE;          Name: 'DISCARDABLE'),
    // resource names
    (Value: _ANICURSOR;            Name: 'ANICURSOR'),
    (Value: _ANIICON;              Name: 'ANIICON'),
    (Value: _BITMAP;               Name: 'BITMAP'),
    (Value: _CURSOR;               Name: 'CURSOR'),
    (Value: _DLGINCLUDE;           Name: 'DLGINCLUDE'),
    (Value: _DLGINIT;              Name: 'DLGINIT'),
    (Value: _HTML;                 Name: 'HTML'),
    (Value: _ICON;                 Name: 'ICON'),
    (Value: _MANIFEST;             Name: 'MANIFEST'),
    (Value: _MESSAGETABLE;         Name: 'MESSAGETABLE'),
    (Value: _PLUGPLAY;             Name: 'PLUGPLAY'),
    (Value: _STRINGTABLE;          Name: 'STRINGTABLE'),
    (Value: _RCDATA;               Name: 'RCDATA'),
    (Value: _VERSIONINFO;          Name: 'VERSIONINFO'),
    (Value: _VXD;                  Name: 'VXD'),
    // file version fields names
    (Value: _FILEVERSION;          Name: 'FILEVERSION'),
    (Value: _PRODUCTVERSION;       Name: 'PRODUCTVERSION'),
    (Value: _FILEFLAGSMASK;        Name: 'FILEFLAGSMASK'),
    (Value: _FILEFLAGS;            Name: 'FILEFLAGS'),
    (Value: _FILEOS;               Name: 'FILEOS'),
    (Value: _FILETYPE;             Name: 'FILETYPE'),
    (Value: _FILESUBTYPE;          Name: 'FILESUBTYPE'),
    (Value: _BLOCK;                Name: 'BLOCK'),
    (Value: _VALUE;                Name: 'VALUE'),
    // resource types (previously declared as tokens but not in KeywordDefs)
    (Value: _DIALOG;               Name: 'DIALOG'),
    (Value: _DIALOGEX;             Name: 'DIALOGEX'),
    (Value: _MENU;                 Name: 'MENU'),
    (Value: _ACCELERATORS;         Name: 'ACCELERATORS'),
    (Value: _MENUEX;               Name: 'MENUEX'),
    // dialog option keywords
    (Value: _CAPTION;              Name: 'CAPTION'),
    (Value: _CLASS;                Name: 'CLASS'),
    (Value: _EXSTYLE;              Name: 'EXSTYLE'),
    (Value: _STYLE;                Name: 'STYLE'),
    (Value: _FONT;                 Name: 'FONT'),
    // style operator
    (Value: _NOT;                  Name: 'NOT'),
    // dialog control keywords
    (Value: _CONTROL;              Name: 'CONTROL'),
    (Value: _LTEXT;                Name: 'LTEXT'),
    (Value: _RTEXT;                Name: 'RTEXT'),
    (Value: _CTEXT;                Name: 'CTEXT'),
    (Value: _PUSHBUTTON;           Name: 'PUSHBUTTON'),
    (Value: _DEFPUSHBUTTON;        Name: 'DEFPUSHBUTTON'),
    (Value: _EDITTEXT;             Name: 'EDITTEXT'),
    (Value: _CHECKBOX;             Name: 'CHECKBOX'),
    (Value: _AUTOCHECKBOX;         Name: 'AUTOCHECKBOX'),
    (Value: _RADIOBUTTON;          Name: 'RADIOBUTTON'),
    (Value: _AUTORADIOBUTTON;      Name: 'AUTORADIOBUTTON'),
    (Value: _AUTO3STATE;           Name: 'AUTO3STATE'),
    (Value: _STATE3;               Name: 'STATE3'),
    (Value: _GROUPBOX;             Name: 'GROUPBOX'),
    (Value: _COMBOBOX;             Name: 'COMBOBOX'),
    (Value: _LISTBOX;              Name: 'LISTBOX'),
    (Value: _SCROLLBAR;            Name: 'SCROLLBAR')
  );

function KeywordToInt(k: string; out kw: integer): boolean;
var
  i: integer;
begin
  Result:= False;
  for i:= low(KeywordDefs) to high(KeywordDefs) do begin
    if k = KeywordDefs[i].Name then begin
      kw:= KeywordDefs[i].Value;
      Exit(True);
    end;
  end;
end;

%}
%x INCOMLINE INCOMMENT INSTRING INSTRINGL

O [0-7]
D [0-9]
H [0-9a-fA-F]
IDENT [a-zA-Z_]([a-zA-Z0-9_])*
NSWPSTR ([a-zA-Z0-9_\:\.\\])*

%%

"//"                    start(INCOMLINE);
<INCOMLINE>\n           begin start(0); unget_char(nl); end;
<INCOMLINE>.            yymore;

"/*"                    start(INCOMMENT);
<INCOMMENT>.            ;
<INCOMMENT>"*/"         start(0);
<INCOMMENT>\0           return(_ILLEGAL);

{D}+L?                  return(_NUMBER);
0x{H}+L?                return(_NUMBER);
0o{O}+L?                return(_NUMBER);
L\"                               begin start(INSTRINGL); strbuf_begin(); end;
\"                                begin start(INSTRING); strbuf_begin(); end;
<INSTRING,INSTRINGL>\"\"          strbuf_append('"');
<INSTRING>\"                      begin
                                    start(0);
                                    return(_QUOTEDSTR);
                                  end;
<INSTRINGL>\"                     begin
                                    start(0);
                                    return(_QUOTEDSTRL);
                                  end;
<INSTRING,INSTRINGL>\\\n          ;
<INSTRING,INSTRINGL>\n            return(_ILLEGAL);
<INSTRING,INSTRINGL>.             strbuf_append(yytext);
\"StringFileInfo\"      begin yytext:= 'StringFileInfo'; return(_STR_StringFileInfo); end;
\"VarFileInfo\"         begin yytext:= 'VarFileInfo'; return(_STR_VarFileInfo); end;
\"Translation\"         begin yytext:= 'Translation'; return(_STR_Translation); end;

BEGIN|{                 return(_BEGIN);
END|}                   return(_END);
{IDENT}                 begin
                          if ypreproc.isdefine(yytext) then begin
                            unget_char(' ');
                            unget_string(ypreproc.getdefine(yytext));
                          end else
                            if KeywordToInt(yytext, kwtmp) then
                              return(kwtmp)
                            else
                              return(_ID);
                         end;
{NSWPSTR}         return(_NSWPSTR);
[ \t\n\f]               ;
[,()|^&+\-*/%~=]        returnc(yytext[1]);
.                       return(_ILLEGAL);
%%

// end.





