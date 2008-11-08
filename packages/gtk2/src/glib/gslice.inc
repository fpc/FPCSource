// included by glib2.pas

// slices - fast allocation/release of small memory blocks

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  TGSliceConfig = (
    G_SLICE_CONFIG_ALWAYS_MALLOC = 1,
    G_SLICE_CONFIG_BYPASS_MAGAZINES,
    G_SLICE_CONFIG_WORKING_SET_MSECS,
    G_SLICE_CONFIG_COLOR_INCREMENT,
    G_SLICE_CONFIG_CHUNK_SIZES,
    G_SLICE_CONFIG_CONTENTION_COUNTER
  ) ;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

function g_slice_alloc(block_size:gsize):gpointer;cdecl;external gliblib;
function g_slice_alloc0(block_size:gsize):gpointer;cdecl;external gliblib;
function g_slice_copy(block_size:gsize; mem_block:gconstpointer):gpointer;cdecl;external gliblib;
procedure g_slice_free1(block_size:gsize; mem_block:gpointer);cdecl;external gliblib;
procedure g_slice_free_chain_with_offset(block_size:gsize; mem_chain:gpointer; next_offset:gsize);cdecl;external gliblib;

{
  MemoryBlockType *
         g_slice_dup                    (MemoryBlockType,
  	                                 MemoryBlockType *mem_block);
         g_slice_free                   (MemoryBlockType,
  	                                 MemoryBlockType *mem_block);
         g_slice_free_chain             (MemoryBlockType,
                                         MemoryBlockType *first_chain_block,
                                         memory_block_next_field);
   pseudo prototypes for the macro
   definitions following below.
}

procedure g_slice_set_config(ckey:TGSliceConfig; value:gint64);cdecl;external gliblib;
function g_slice_get_config(ckey:TGSliceConfig):gint64;cdecl;external gliblib;
function g_slice_get_config_state(ckey:TGSliceConfig; address:gint64; n_values:pguint):Pgint64;cdecl;external gliblib;

{$ENDIF read_interface_rest}

{$IFDEF read_implementation}
{$ENDIF read_implementation}

// included by glib2.pas


