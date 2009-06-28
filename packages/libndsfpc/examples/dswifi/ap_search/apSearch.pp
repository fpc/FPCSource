program apSearch;

{$apptype arm9}
{$define ARM9}

{$mode objfpc}


uses
  ctypes, nds9, dswifi9;

procedure findAP(ap: pWifi_AccessPoint); 
var
	selected, i, count: integer;
	ap2: Wifi_AccessPoint;
begin
  selected := 0;  
  count := 0;

  Wifi_ScanMode(); //this allows us to search for APs
  
  while ((keysDown() and KEY_A) = 0) do
  begin
    scanKeys();
    
    //find out how many APs there are in the area
    count := Wifi_GetNumAP();
    consoleClear();
    
    iprintf('Number of APs found: %d'#10, count);
    
    //display the APs to the user
    for i := 0 to count - 1 do
    begin
      Wifi_GetAPData(i, @ap2);
      // display the name of the AP
      if i = selected then
        iprintf('%s %s'#10, '*', pcchar(ap2.ssid))
      else
        iprintf('%s %s'#10, ' ', pcchar(ap2.ssid));
      
    end;

		//move the selection asterick
		if ((keysDown() and KEY_UP) <> 0) and (selected > 0) then 
      dec(selected);

		if ((keysDown() and KEY_DOWN) <> 0) and (selected < (count-1)) then 
      inc(selected);

		swiWaitForVBlank();
	end;

	//user has made a choice so grab the ap and return it
	Wifi_GetAPData(selected, ap);

end;

//---------------------------------------------------------------------------------
function keyPressed(c: cint): pointer;
begin
  if (c > 0) then
    iprintf('%c', c);
end;

var
  ap3: pWifi_AccessPoint;
  status: integer;
  kb: pKeyboard;
  oldStatus: integer;
	url: array [0..255] of char;
  host: phostent;

begin
  status := integer(ASSOCSTATUS_DISCONNECTED);
  
  consoleDemoInit(); 

  new(kb);
  kb := keyboardDemoInit();
  kb^.OnKeyPressed := @keyPressed;
  
  Wifi_InitDefault(false);
  
  findAP(ap3);
  	
  iprintf('Connecting to %s'#10, pcchar(ap3^.ssid));
  
  //this tells the wifi lib to use dhcp for everything
  Wifi_SetIP(0,0,0,0,0);	
  
  Wifi_ConnectAP(ap3, integer(WEPMODE_NONE), 0, nil);

  while (status <> integer(ASSOCSTATUS_ASSOCIATED)) and (status <> integer(ASSOCSTATUS_CANNOTCONNECT)) do
  begin
    oldStatus := status;
    
    status := Wifi_AssocStatus();
    if oldStatus <> status then
      iprintf('%s', pchar(@ASSOCSTATUS_STRINGS[status]))
    else 
      iprintf('%s', '.');
    
    swiWaitForVBlank();
  end;

	consoleClear();
	consoleSetWindow(nil, 0,0,32,10);


  while true do
  begin
    iprintf('Url? ');
    
    scanf('%s', url);
    
    host := gethostbyname(url);
    
    if (host) <> nil then
    	iprintf('IP (%s) : %s'#10,  url, inet_ntoa(in_addr(host^.h_addr_list^)))
    else
    	iprintf('Could not resolve'#10);
    
    swiWaitForVBlank();
  end;

end.
