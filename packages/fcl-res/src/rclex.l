%{
var
  kwtmp: integer;
  strbuf: string;

const
  KeywordDefs: array [0..0] of TIdentMapEntry = (
    (Value: _LANGUAGE; Name: 'LANGUAGE')
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

[1-9]([0-9])*           return(_NUMDECIMAL);
0x{H}*                  return(_NUMHEX);
[1-9]([0-9])*L          return(_NUMDECIMALL);
0x{H}*L                 return(_NUMHEXL);
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





