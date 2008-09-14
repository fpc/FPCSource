program decrefcrash;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 {$ifdef FPC}{$ifdef linux}cthreads,{$endif}{$endif}
 sysutils;

const
 maxdatasize = $7fffffff;
type
{$ifdef VER2_2}
 msechar = widechar;
 msestring = widestring;
{$else VER2_2}
 msechar = unicodechar;
 msestring = unicodestring;
{$endif VER2_2}
 msecharaty = array[0..maxdatasize div sizeof(msechar)-1] of msechar;
 pmsecharaty = ^msecharaty;

procedure replacechar1(var dest: msestring; a,b: msechar);
  //replaces a by b
var
 int1: integer;
begin
 uniquestring(dest);
 for int1:= 0 to length(dest)-1 do begin
  if pmsecharaty(dest)^[int1] = a then begin
   pmsecharaty(dest)^[int1]:= b;
  end;
 end;
end;


function winfilepath(dirname,filename: msestring): msestring;
begin
 writeln((pptrint(pointer(dirname))-2)^);
 flush(output);
 writeln((pptrint(pointer(filename))-2)^);
 flush(output);
 replacechar1(dirname,msechar('/'),msechar('\'));
 replacechar1(filename,msechar('/'),msechar('\'));
 if (length(dirname) >= 3) and (dirname[1] = '\') and (dirname[3] = ':') then begin
  dirname[1]:= dirname[2]; // '/c:' -> 'c:\'
  dirname[2]:= ':';
  dirname[3]:= '\';
  if (dirname[4] = '\') and (length(dirname) > 4) then begin
   move(dirname[5],dirname[4],(length(dirname) - 4)*sizeof(msechar));
   setlength(dirname,length(dirname) - 1);
  end;
 end;
 if filename <> '' then begin
  if dirname = '' then begin
   result:= '.\'+filename;
  end
  else begin
   if dirname[length(dirname)] <> '\' then begin
    result:= dirname + '\' + filename;
   end
   else begin
    result:= dirname + filename;
   end;
  end;
 end
 else begin
  result:= dirname;
 end;
end;

var
 mstr1,mstr2: msestring;
begin
 mstr2:= 'C:\Dokumente und Einstellungen\mseca\Anwendungsdaten\.mseide';
 mstr1:= winfilepath(mstr2,'*');
end.
