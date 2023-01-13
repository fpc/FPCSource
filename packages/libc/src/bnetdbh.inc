
type
   Pnetent = ^netent;
   netent = record
        n_name : PAnsiChar;
        n_aliases : ^PAnsiChar;
        n_addrtype : longint;
        n_net : uint32_t;
     end;
   PPNetEnt = ^Pnetent;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TNetEnt = netent;
