program eeprom;

uses
  ctypes, nds9, sysutils;

var
  header1: array [0..511] of cchar;
  header2: array [0..511] of cchar;
  vtype, vsize: integer;
  data: array [0..511] of cchar;

procedure pause();
begin
	iprintf('Press start...'#10);
	while true do
	begin
		scanKeys();
		if (keysDown() and KEY_START)<>0 then
			exit;
		swiWaitForVBlank();
	end;
	scanKeys();
end;

var
  x, y: integer;
  c: u8;

begin
	consoleDemoInit();

	iprintf('Reading cart info...'#10);

	sysSetBusOwners(true, true); // give ARM9 access to the cart

	while true do
	begin
		// Read the header twice to verify.
		// If the card is being encrypted, we will get random junk
    cardReadHeader(@header1);
    cardReadHeader(@header2);

		// Make sure we got the same data twice
    while (CompareMem(@header1, @header2, 32)) do
    begin
      // If not, the card needs ejected and reinserted into the DS
      iprintf('Please eject & reinsert DS card.'#10);
      pause();
      cardReadHeader(@header1);
      cardReadHeader(@header2);
    end;

		// Add a null char right after the game title, so we can print it
		header1[32] :=  cchar(#0);
  
    // Read some various info about the EEPROM
    vtype := cardEepromGetType();
    vsize := cardEepromGetSize();
  
    iprintf('Game ID: %s'#10, header1[0]);
    iprintf('EEPROM:'#10);
    iprintf(' Type: %d'#10, vtype);
    iprintf(' Size: %d'#10, vsize);
    pause();

    // Read the first 512 bytes of EEPROM
    cardReadEeprom(0, @data, 512, vtype);

    iprintf('First 160 bytes of EEPROM: '#10);
    
    // Print 20 rows of 8 bytes each
    for y := 0 to 19 do
    begin
      // print 8 bytes as hex
      for x := 0 to 7 do
      begin
        iprintf('%02x ', data[y*8 + x]);
      end;
  
      // print 8 bytes as characters
      for x := 0 to 7 do
      begin
        c := data[y*8 + x];
        if not (char(c) in [#0..#7, #14..#31, #127]) then // only display if it's a printable character
          iprintf('%c', c)
        else
          iprintf('.');
      end;
    end;
  
    iprintf('Insert a new card to read again'#10);
    pause();
  end;
end.
