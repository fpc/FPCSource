
type
  Ppw_buffers = ^Tpw_buffers;


  Tpw_buffers = record
    mem: Ppw_memblock;
    buffers: ^Pspa_buffer;
    n_buffers: Tuint32_t;
    flags: Tuint32_t;
  end;


function pw_buffers_negotiate(context: Ppw_context; flags: Tuint32_t; outnode: Pspa_node; out_port_id: Tuint32_t; innode: Pspa_node; in_port_id: Tuint32_t;
  Result: Ppw_buffers) : longint; cdecl; external libpipewirename Name 'pw_buffers_negotiate';

procedure pw_buffers_clear(buffers: Ppw_buffers); cdecl; external libpipewirename Name 'pw_buffers_clear';


