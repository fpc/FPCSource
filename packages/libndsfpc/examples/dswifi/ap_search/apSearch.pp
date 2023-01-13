program apSearch;

{$mode objfpc}

uses
  ctypes, nds9, dswifi9;

var
  ap: pWifi_AccessPoint;

function findAP(): pWifi_AccessPoint; 
var
	selected, i, count, displaytop: integer;
	ap2: Wifi_AccessPoint;
  pressed: cint;
  displayend: integer;
  s1, s2: ansistring;
begin
  selected := 0;  
  count := 0;
  displaytop := 0;
  
  Wifi_ScanMode(); //this allows us to search for APs
  
  pressed := 0;
  
  while ((pressed and KEY_A) = 0) do
  begin
    scanKeys();
    
    pressed := keysDown();
    
    if (pressed and KEY_START) <> 0 then exit;
    
    //find out how many APs there are in the area
    count := Wifi_GetNumAP();
    
    consoleClear();
    
    iprintf('%d APs detected'#10, count);

		displayend := displaytop + 10;
		if (displayend > count) then displayend := count;
    
    //display the APs to the user
    for i := displaytop to displayend - 1 do
    begin
      Wifi_GetAPData(i, ap);
      // display the name of the AP
      if i = selected then
        s1 := '*' 
      else
        s1 := ' ';
      
      if (ap.flags and WFLAG_APDATA_WEP) <> 0 then
        s2 := 'Yes ' 
      else
        s2 :=  'No ';
      
      iprintf('%s %.29s'#10'  Wep:%s Sig:%i'#10, s1, pcchar(ap^.ssid), s2, ap.rssi * 100 div $D0);
      
    end;
    
		//move the selection asterick
		if ((pressed and KEY_UP) <> 0) and (selected > 0) then 
      dec(selected);

		if ((pressed and KEY_DOWN) <> 0) and (selected < (count-1)) then 
      inc(selected);

		swiWaitForVBlank();
	end;

	//user has made a choice so grab the ap and return it
	Wifi_GetAPData(selected, ap);
  result := ap;
end;

//---------------------------------------------------------------------------------
procedure keyPressed(c: cint);
begin
  if (c > 0) then
    iprintf('%c', c);
end;

var
  ap3: pWifi_AccessPoint;
  status: integer;
  kb: pKeyboard;
  oldStatus: integer;
	url: array [0..255] of AnsiChar;
  host: phostent;
  wepkey = array [0..63] of AnsiChar;
  wepmode: cint;
  len: integer;
  ip: cuint32;
  quit: integer;
  pressed: cint;
begin
  Wifi_InitDefault(false);
  consoleDemoInit(); 
  new(kb);
  kb := keyboardDemoInit();
  kb^.OnKeyPressed := @keyPressed;

  while true do
  begin
    status := integer(ASSOCSTATUS_DISCONNECTED);
    consoleClear();
    consoleSetWindow(nil, 0,0,32,24);   

    ap3 := findAp();

    consoleClear();
    consoleSetWindow(nil, 0,0,32,10);

    iprintf('Connecting to %s'#10, pcchar(ap3^.ssid));

    //this tells the wifi lib to use dhcp for everything
    Wifi_SetIP(0,0,0,0,0);
    wepmode := WEPMODE_NONE;

    if (ap3^.flags and WFLAG_APDATA_WEP) <> 0 then
    begin
      iprintf('Enter Wep Key'#10);
      while (wepmode = WEPMODE_NONE) do
      begin
        scanf('%s', wepkey);
        if (strlen(wepkey) = 13) then
          wepmode := WEPMODE_128BIT;
        else if (strlen(wepkey) = 5) then
          wepmode := WEPMODE_40BIT;
        else 
          iprintf('Invalid key!'#10);
      end;
      Wifi_ConnectAP(ap3, wepmode, 0, pcuint8(wepkey));
    end else 
      Wifi_ConnectAP(ap3, integer(WEPMODE_NONE), 0, nil);

    consoleClear();
    while (status <> ASSOCSTATUS_ASSOCIATED) and (status <> ASSOCSTATUS_CANNOTCONNECT) do
    begin
      status := Wifi_AssocStatus();
      len := strlen(ASSOCSTATUS_STRINGS[status]);
      iprintf(#27'[0;0H\x1b[K');
      iprintf(#27'[0;%dH%s', (32-len) div 2, ASSOCSTATUS_STRINGS[status]);
      
      scanKeys();
      
      if (keysDown() and KEY_B) <> 0 then break;
      
      swiWaitForVBlank();
    end;
   

    if (status = ASSOCSTATUS_ASSOCIATED) then
    begin
      ip := Wifi_GetIP();

      iprintf(#10'ip: [%li.%li.%li.%li]'#10, (ip ) and $FF, (ip shr 8) and $FF, (ip shr 16) and $FF, (ip shr 24) and $FF);
			while true do 
      begin

				scanf('%s', url);

				if (strcmp(url, 'quit') = 0) then break;

				host := gethostbyname(url);

				if(host <> nil) then
					iprintf('IP (%s) : %s'#10,  url, inet_ntoa(pin_addr(host)^.h_addr_list[0]))
				else
					iprintf('Could not resolve'#10);

				swiWaitForVBlank();
			end;
		end else 
			iprintf(#10'Connection failed!'#10);

		quit := 0;
		iprintf('Press A to try again, B to quit.');
		while true do
   begin
			swiWaitForVBlank();
			scanKeys();
			pressed := keysDown();
			if(pressed and KEY_B) <> 0 then quit := 1;
			if(pressed and (KEY_A or KEY_B)) <> 0 then break;
		end;
		if (quit) <> 0 then break;
   
  end;

end.
