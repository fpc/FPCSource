{$mode objfpc}
{$h+}
program testmime;

uses classes,fpmimetypes;

Var
  L : TstringList;
  I : integer;
  FN : String;
    
begin
  FN:=Paramstr(1);
{$ifdef unix}  
  if (FN='') then
    FN:='/etc/mime.types';
{$endif}  
  MimeTypes.LoadFromFile(FN);
  L:=TStringList.Create;
  try
    MimeTypes.GetKNownMimeTypes(L);
    For I:=0 to L.Count-1 do
      Writeln('Type ',i,' : ',L[i],' : ',MimeTypes.GetMimeExtensions(L[i]));
    MimeTypes.GetKnownExtensions(L);
    For I:=0 to L.Count-1 do
      Writeln('Extension ',i,' : ',L[i],' : ',MimeTypes.GetMimeType(L[i]));
  finally
    L.Free;
  end;

end.