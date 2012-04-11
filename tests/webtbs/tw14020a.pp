{ %target=linux,freebsd,darwin,solaris,haiku,aix }

program project1;

uses
  SysUtils;


procedure cleanup;
var
  f: file;
begin
  assign(f,'tw14020ad/.hidden');
  erase(f);
  rmdir('tw14020ad');
end;


var
  rSearch: TSearchRec;
  lsName, lsSearch: String;
  bDone:   Boolean;
  f: file;
begin
  createdir('tw14020ad');
  assign(f,'tw14020ad/.hidden');
  rewrite(f);
  close(f);

  { for all files in the dir }
  lsSearch := 'tw14020ad/' + AllFilesMask; //fails to find anything

  FillChar(rSearch, Sizeof(TSearchRec), 0);
  bDone := (FindFirst(lsSearch, faHidden, rSearch) <> 0);

  while not bDone do
  begin
    lsName := rSearch.Name;
    Assert(lsName <> '');
    if (rSearch.Attr and faDirectory > 0) then
      continue;

    { if we find our file, it's ok }
    if (pos('.hidden',lsName)<>0) then
      if (rSearch.Attr and faHidden) <> 0 then
        begin
          findclose(rsearch);
          cleanup;
          halt(0);
        end
      else
        begin
          findclose(rsearch);
          cleanup;
          halt(1);
        end;
    bDone := (FindNext(rSearch) <> 0);
    Assert(bDone or (rSearch.Name <> lsName));
  end;
  FindClose(rSearch);
  cleanup;
  halt(1);
end.
