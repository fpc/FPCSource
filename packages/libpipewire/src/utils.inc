

type
  Tpw_destroy_t = procedure (object_:pointer);cdecl;


function pw_split_walk(str:Pchar; delimiter:Pchar; len: Psize_t; state:PPchar):Pchar;cdecl;external libpipewirename name 'pw_split_walk';

function pw_split_strv(str:Pchar; delimiter:Pchar; max_tokens:longint; n_tokens:Plongint):PPchar;cdecl;external libpipewirename name 'pw_split_strv';

procedure pw_free_strv(str:PPchar);cdecl;external libpipewirename name 'pw_free_strv';

function pw_strip(str:Pchar; whitespace:Pchar):Pchar;cdecl;external libpipewirename name 'pw_strip';

function pw_getrandom(buf:pointer; buflen:Tsize_t; flags:dword):Tssize_t;cdecl;external libpipewirename name 'pw_getrandom';


