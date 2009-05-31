program access_dir;

{$apptype arm9}
{$mode objfpc}

uses
  nds9, fat, ctypes;

var
	i: integer;
  filename: string[255];
	handle: P_FILE;
  st: stat;
  dir: PDIR_ITER;


begin
	consoleDemoInit();

  printf('fatInit()...');
	if (fatInitDefault()) then
  begin
    printf(#9 + 'Success' + #10);

    dir := diropen('/');

    if  (dir = nil) then
      iprintf ('Unable to open the directory.'#10)
    else 
    begin
      while dirnext(dir, pchar(@filename), @st) = 0 do
      begin
        // st.st_mode & _IFDIR indicates a directory
        if (st.st_mode and $4000) <> 0 then
          iprintf ('%s: %s'#10, ' DIR', pchar(@filename)) 
        else
          iprintf ('%s: %s'#10, 'FILE', pchar(@filename)); 
      end;
    end;
  end else
    printf(#9 + 'Failure' + #10);

  while true do;

end.
