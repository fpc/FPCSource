%{
var
  kwtmp: integer;
  strbuf: string;

const
  KeywordDefs: array [0..23] of TIdentMapEntry = (
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
    (Value: _VXD;                  Name: 'VXD')
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

D [0-9]
H [0-9a-fA-F]
IDENT [a-zA-Z_]([a-zA-Z0-9_])*

%%

"//"                    start(INCOMLINE);
<INCOMLINE>\n           begin start(0); unget_char(nl); end;
<INCOMLINE>.            yymore;

"/*"                    start(INCOMMENT);
<INCOMMENT>.            ;
<INCOMMENT>"*/"         start(0);
<INCOMMENT>\0           return(_ILLEGAL);

{D}*L?                  return(_NUMDECIMAL);
0x{H}*L?                return(_NUMHEX);
\"                      begin start(INSTRING); strbuf:= ''; end;
<INSTRING>\"\"          strbuf:= strbuf + '"';
<INSTRING>\"            begin
                          start(0);
                          yytext:= strbuf;
                          return(_QUOTEDSTR);
                        end;
<INSTRING>\\\n          ;
<INSTRING>\n            return(_ILLEGAL);
<INSTRING>.             strbuf:= strbuf + yytext;

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
[ \t\n\f]               ;
","                     returnc(',');
.                       return(_ILLEGAL);
%%

// end.





