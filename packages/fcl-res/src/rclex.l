
%x INCOMLINE INCOMMENT INSTRING

D [0-9]    
H [0-9a-fA-F]

%%

"//"                    start(INCOMLINE);
<INCOMLINE>\n           begin start(0); unget_char(nl); end;
<INCOMLINE>.            yymore;

"/*"                    start(INCOMMENT);
<INCOMMENT>.            ;
<INCOMMENT>"*/"         start(0);
<INCOMMENT>\0           return(ILLEGAL);

[a-zA-Z_]([a-zA-Z0-9_])* begin
                           if ypreproc.isdefine(yytext) then begin
                             unget_char(' ');
                             unget_string(ypreproc.getdefine(yytext));
                           end else
                             return(ID);
                         end;
.                        return(ID);
%{
(*
[ \t\n\f]               ;
#define
#else
#endif
#ifdef
#ifndef
#include
#undef
              
.                       begin
                           writeln(erroutput, 'Illegal character in line ',yylineno, ' col ', yycolno);
                           writeln(erroutput, '"',yyline,'"');
                           return(ILLEGAL);
                        end;          
*)
%}
%%

// end.





