{ %target=linux,freebsd,darwin,solaris,haiku,aix }

program project1;

uses
  SysUtils;

var
  rSearch: TSearchRec;
  lsName, lsSearch: String;
  bDone:   Boolean;
begin
  { for all files in the dir }
  lsSearch := './' + AllFilesMask; //fails to find anything
  WriteLn(lsSearch);

  FillChar(rSearch, Sizeof(TSearchRec), 0);
  bDone := (FindFirst(lsSearch, 0, rSearch) <> 0);

  while not bDone do
  begin
    lsName := rSearch.Name;
    Assert(lsName <> '');
    if (rSearch.Attr and faDirectory > 0) then
      continue;

    { if we find one file, it's ok }
    findclose(rsearch);
    halt(0);

    bDone := (FindNext(rSearch) <> 0);
    Assert(bDone or (rSearch.Name <> lsName));
  end;
  FindClose(rSearch);
  halt(1);
end.
