program sockettest;

{$J+}
{$macro on}
{$mode objfpc}

uses
  cmem, ctypes, network, gccore;

function strncmp(__s1:Pchar; __s2:Pchar; __n:cardinal):longint;cdecl;external;


var
  xfb: pcuint32 = nil;
  rmode: PGXRModeObj = nil;
  httd_handle: lwp_t;
	hits: cint = 0; 

var 
  http_200: pchar = 'HTTP/1.1 200 OK\r\n';
  indexdata: pchar = '<html> \' +
              '  <head><title>A test page</title></head> \' +
              '  <body> \' +
              '    This small test page has had %d hits. \' +
              '  </body> \' +
              '</html>';
  http_html_hdr: pchar = 'Content-type: text/html\r\n\r\n';
  http_get_index: pchar = 'GET / HTTP/1.1\r\n';

function httpd(arg: pointer): pointer; cdecl;
var
	sock, csock: cint32;
	ret: cint;
	clientlen: cuint32;
	client: sockaddr_in;
	server: sockaddr_in;
	temp: array [0..1025] of cchar;
begin	
	clientlen := sizeof(client);

	sock := net_socket (AF_INET, SOCK_STREAM, IPPROTO_IP);

	if (sock = INVALID_SOCKET) then
      printf ('Cannot create a socket!'#10)
  else 
  begin
		memset(@server, 0, sizeof(server));
		memset(@client, 0, sizeof(client));

		server.sin_family := AF_INET;
		server.sin_port := {htons}(80);
		server.sin_addr.s_addr := INADDR_ANY;
		ret := net_bind(sock, psockaddr(@server), sizeof(server));
		if ret <> 0 then
			printf('Error %d binding socket!'#10, ret)
    else 
    begin
			if ret = net_listen(sock, 5) then
				printf('Error %d listening!'#10, ret)
    else 
    begin
		  while true do
		  begin
					csock := net_accept (sock, psockaddr(@client), @clientlen);
					if csock < 0 then
					begin
						printf('Error connecting socket %d!'#10, csock);
						while true do;
					end;

					printf('Connecting port %d from %s'#10, client.sin_port, inet_ntoa(client.sin_addr));
					memset(@temp, 0, 1026);
					ret := net_recv(csock, @temp, 1024, 0);
					printf('Received %d bytes'#10, ret);

					if strncmp(@temp, http_get_index, strlen(http_get_index)) = 0 then
					begin
						inc(hits);
						net_send(csock, http_200, strlen(http_200), 0);
						net_send(csock, http_html_hdr, strlen(http_html_hdr), 0);
						sprintf(@temp, indexdata, hits);
						net_send(csock, @temp, strlen(@temp), 0);
					end;

					net_close(csock);

				end;
			end;
		end;
	end;
	result := nil;
end;

function initialise(): pointer;
var
	framebuffer: pcuint32;
begin
	VIDEO_Init();
	WPAD_Init();
	
	rmode := VIDEO_GetPreferredMode(nil);
	framebuffer := (SYS_AllocateFramebuffer(rmode));
	console_init(framebuffer,20,20,rmode^.fbWidth,rmode^.xfbHeight,rmode^.fbWidth*VI_DISPLAY_PIX_SZ);
	
	VIDEO_Configure(rmode);
	VIDEO_SetNextFramebuffer(framebuffer);
	VIDEO_SetBlack(FALSE);
	VIDEO_Flush();
	VIDEO_WaitVSync();
	if(rmode^.viTVMode and VI_NON_INTERLACE) <> 0 then VIDEO_WaitVSync();

	result := framebuffer;

end;


var
  ret: cint32;
	localip: array [0..15] of cchar;
	gateway: array [0..15] of cchar;
	netmask: array [0..15] of cchar;
begin
	
	xfb := initialise();

	printf(#10'libogc network demo'#10);
	printf('Configuring network ...'#10);

	// Configure the network interface
	ret := if_config(localip, netmask, gateway, TRUE);
	if (ret >= 0) then 
	begin
		printf('network configured, ip: %s, gw: %s, mask %s'#10, @localip, @gateway, @netmask);

		LWP_CreateThread(@httd_handle,	// thread handle 
        							@httpd,			// code  
        							@localip,		// arg pointer for thread 
        							nil,			  // stack base  
        							16*1024,		// stack size 
        							50				  // thread priority 
                    );
  end else 
		printf ('network configuration failed!'#10);

	while true do
	begin

		VIDEO_WaitVSync();
		WPAD_ScanPads();
		if (WPAD_ButtonsDown(0) and WPAD_BUTTON_HOME) <> 0 then 
			exit;
		
	end;

end.

