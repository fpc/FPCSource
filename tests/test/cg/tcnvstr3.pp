{ Type conversion program for char -> string     }
{ possible types widechar -> widestring          }
{                widechar -> shortstring         }
{                widechar -> ansistring          }
{ possible types char     -> widestring          }
{                char     -> shortstring         }
{                char     -> ansistring          }

{$ifdef fpc}
{$mode objfpc}
  {$ifndef ver1_0}
    {$define haswidestring}
  {$endif}
{$else}
  {$ifndef ver70}
    {$define haswidestring}
  {$endif}
{$endif}

procedure fail;
 begin
   WriteLn('Failure!');
   halt(1);
 end;

var
 str_ansi : ansistring;
 str_short : shortstring;
{$ifdef haswidestring}
 str_wide : widestring;
 wc : widechar;
{$endif haswidestring}
 c: char;
 _result : boolean;
Begin
 {********************** char/widechar -> shortstring *******************}
 Write('widechar/char -> shortstring...');
 {* normal char *}
 _result := true;
 { empty string -> shortstring  }
 str_short := '';
 if str_short <> '' then
   _result := false;
 { constant char -> shortstring }
 str_short := 'c';
 if str_short <> 'c' then
   _result := false;
 { normal char   -> shortstring }
 str_short := '';
 c:='c';
 str_short:=c;
 if str_short <> 'c' then
   _result := false;
 {* wide char *}
{$ifdef haswidestring}
 { constant char -> shortstring }
 str_short := shortstring(widechar('c'));
 if str_short <> 'c' then
   _result := false;
{$endif}
 { wide char   -> shortstring }
{ This should not compile - at least it does not compile under Delphi }
{ str_short := '';
 wc:='c';
 str_short:=wc;
 if str_short <> 'c' then
   _result := false;}


 if _result then
   WriteLn('Success!')
 else
   fail;
 {********************** char/widechar -> ansistring *******************}
 Write('widechar/char -> ansistring...');
 {* normal char *}
 _result := true;
 { empty string -> ansistring  }
 str_ansi := '';
 if str_ansi <> '' then
   _result := false;
 { constant char -> ansistring }
 str_ansi := 'c';
 if str_ansi <> 'c' then
   _result := false;
 { normal char   -> ansistring }
 str_ansi := '';
 c:='c';
 str_ansi:=c;
 if str_ansi <> 'c' then
   _result := false;
 {* wide char *}
{$ifdef haswidestring}
 { constant char -> ansistring }
 str_ansi := widechar('c');
 if str_ansi <> 'c' then
   _result := false;
 { normal char   -> ansistring }
 str_ansi := '';
 wc:='c';
 str_ansi:=wc;
 if str_ansi <> 'c' then
   _result := false;
{$endif}

 if _result then
   WriteLn('Success!')
 else
   fail;
{}
{$ifdef haswidestring}
 {********************** char/widechar -> widestring *******************}
 Write('widechar/char -> widestring...');
 {* normal char *}
 _result := true;
 { empty string -> widestring  }
 str_wide := '';
 if str_wide <> '' then
   _result := false;
 { constant char -> widestring }
 str_wide := 'c';
 if str_wide <> 'c' then
   _result := false;
 { normal char   -> widestring }
 str_wide := '';
 c:='c';
 str_wide:=c;
 if str_wide <> 'c' then
   _result := false;
 {* wide char *}
 { constant char -> widestring }
 str_wide := widechar('c');
 if str_wide <> 'c' then
   _result := false;
 { normal char   -> widestring }
 str_wide := '';
 wc:='c';
 str_wide:=wc;
 if str_wide <> 'c' then
   _result := false;


 if _result then
   WriteLn('Success!')
 else
   fail;
{$endif haswidestring}
end.
