program httpget;

{$mode objfpc}

uses
  ctypes, nds9, dswifi9;

procedure getHttp(url: pchar);
const
  // store the HTTP request for later
  request_text =  'GET /dswifi/example1.php HTTP/1.1\r\n' + 'Host: www.akkit.org\r\n' + 'User-Agent: Nintendo DS\r\n\r\n';
var
  myhost: phostent;
  my_socket: cint;
  sain: sockaddr_in;
  recvd_len: cint;
  incoming_buffer: array [0..255] of char;
begin
  // Let's send a simple HTTP request to a server and print the results!

  // Find the IP address of the server, with gethostbyname
  myhost := gethostbyname(url);
  iprintf('Found IP Address!'#10);

  // Create a TCP socket
  my_socket := socket(AF_INET, SOCK_STREAM, 0);
  iprintf('Created Socket!'#10);

  // Tell the socket to connect to the IP address we found, on port 80 (HTTP)
  sain.sin_family := AF_INET;
  sain.sin_port := htons(80);
  sain.sin_addr.s_addr := culong(Pointer(myhost^.h_addr_list^)^);
  connect(my_socket, psockaddr(@sain), sizeof(sain));
  iprintf('Connected to server!'#10);

  // send our request
  send(my_socket, pchar(request_text), strlen(request_text), 0);
  iprintf('Sent our request!'#10);

  // Print incoming data
  iprintf('Printing incoming data:'#10);

  repeat
    recvd_len := recv( my_socket, @incoming_buffer, 255, 0);
    if (recvd_len > 0) then // data was received!
    begin
      incoming_buffer[recvd_len] := #0; // null-terminate
      iprintf(incoming_buffer);
    end;
  until recvd_len <= 0;

  iprintf('Other side closed connection!' + #10);
  shutdown(my_socket, 0); // good practice to shutdown the socket.
  closesocket(my_socket); // remove the socket.
end;

begin
	consoleDemoInit();  //setup the sub screen for printing

	iprintf(#10#10#9'Simple Wifi Connection Demo'#10#10);
	iprintf('Connecting via WFC data ...'#10);

	if not Wifi_InitDefault(WFC_CONNECT) then
		iprintf('Failed to connect!')
	else
	begin
		iprintf('Connected'#10#10);
		getHttp('www.akkit.org');
	end;

	while true do
		swiWaitForVBlank();
end.
