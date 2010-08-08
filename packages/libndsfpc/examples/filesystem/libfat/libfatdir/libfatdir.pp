program libfatdir;

uses
  ctypes, nds9, fat;


var
  MyDir: PDir;
  pent: pdirent;
  statbuf: Tstat;

begin
	// Initialise the console, required for printf
	consoleDemoInit();
	
	if (fatInitDefault()) then
	begin

	
		MyDir := opendir('/');

		if (MyDir) <> nil then
		begin
      repeat
        pent := readdir(MyDir);
    		_stat(pent^.d_name, statbuf);
    		if (strcmp('.', pent^.d_name) = 0) or (strcmp('..', pent^.d_name) = 0) then
	        		continue;
    		if (S_ISDIR(statbuf.st_mode)) then
	        		iprintf('%s <dir>'#10, pent^.d_name);
    		if not (S_ISDIR(statbuf.st_mode)) then
	        		iprintf('%s %ld'#10, pent^.d_name, statbuf.st_size);
      until pent = nil;
			closedir(MyDir);
		end else
    begin
			iprintf ('opendir() failure; terminating'#10);
		end;

	end else 
	begin
		iprintf('fatInitDefault failure: terminating'#10);
	end;

	while true do
		swiWaitForVBlank();

end.
