
const
   _NETINET_ETHER_H = 1;

function ether_ntoa(__addr:Pether_addr):PAnsiChar;cdecl;external clib name 'ether_ntoa';
function ether_ntoa_r(__addr:Pether_addr; __buf:PAnsiChar):PAnsiChar;cdecl;external clib name 'ether_ntoa_r';
function ether_aton(__asc:PAnsiChar):Pether_addr;cdecl;external clib name 'ether_aton';
function ether_aton_r(__asc:PAnsiChar; __addr:Pether_addr):Pether_addr;cdecl;external clib name 'ether_aton_r';
function ether_ntohost(__hostname:PAnsiChar; __addr:Pether_addr):longint;cdecl;external clib name 'ether_ntohost';
function ether_hostton(__hostname:PAnsiChar; __addr:Pether_addr):longint;cdecl;external clib name 'ether_hostton';
function ether_line(__line:PAnsiChar; __addr:Pether_addr; __hostname:PAnsiChar):longint;cdecl;external clib name 'ether_line';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
