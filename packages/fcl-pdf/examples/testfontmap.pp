program testfontmap;

{$ifndef FPC}
{$apptype CONSOLE}
{$endif}

uses dynlibs,types,fpttf;

var
  lst:TStringDynArray;

procedure dump(const lst:TStringDynArray);
var i:integer;
begin
  for i:=0 to high(lst) do
  writeln('#',i,' ',lst[i]);
  writeln();
end;

begin
  if TFontmapper.find('Courier New','bold italic',lst) then
    dump(lst);
  
  if TFontmapper.find('Arial','',lst) then
    dump(lst);

  if TFontmapper.find('Verdana','bold',lst) then
    dump(lst);

  if TFontmapper.find('FreeSans','italic',lst) then
    dump(lst);
 

end.