program autoconnect;

{$mode objfpc}

uses
  ctypes, nds9, dswifi9;

var
	ip, gateway, mask, dns1, dns2: in_addr;

begin

	consoleDemoInit();  //setup the sub screen for printing

	iprintf(#10#10#9'Simple Wifi Connection Demo'#10#10);
	iprintf('Connecting via WFC data ...'#10);

	if not Wifi_InitDefault(WFC_CONNECT) then
		iprintf('Failed to connect!')
	else 
	begin
		iprintf('Connected'#10#10);

		ip := in_addr(Wifi_GetIPInfo(@gateway, @mask, @dns1, @dns2));
		
		iprintf('ip     : %s'#10, inet_ntoa(ip));
		iprintf('gateway: %s'#10, inet_ntoa(gateway));
		iprintf('mask   : %s'#10, inet_ntoa(mask));
		iprintf('dns1   : %s'#10, inet_ntoa(dns1));
		iprintf('dns2   : %s'#10, inet_ntoa(dns2));
	end;

	while true do
		swiWaitForVBlank();
end.

