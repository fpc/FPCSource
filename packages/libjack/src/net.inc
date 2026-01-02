(*
  Copyright (C) 2009-2010 Grame

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*)

{$ifndef __net_h__}
{$define __net_h__}

//#ifdef __cplusplus
//extern "C"
//{
//#endif

//#include <jack/systemdeps.h>
{$I systemdeps.inc}
//#include <jack/types.h>
//#include <jack/weakmacros.h>
{$I weakmacros.inc}

const
  DEFAULT_MULTICAST_IP = '225.3.19.154';
  DEFAULT_PORT         = 19000;
  DEFAULT_MTU          = 1500;
  MASTER_NAME_SIZE     = 256;

// Possible error codes

const
  NO_ERROR          =  0;
  SOCKET_ERROR      = -1;
  SYNC_PACKET_ERROR = -2;
  DATA_PACKET_ERROR = -3;

  RESTART_CB_API = 1;

type
  JackNetEncoder = (

    JackFloatEncoder = 0,   // samples are transmitted as float
    JackIntEncoder = 1,     // samples are transmitted as 16 bits integer
    JackCeltEncoder = 2,    // samples are transmitted using CELT codec (http://www.celt-codec.org/)
    JackOpusEncoder = 3     // samples are transmitted using OPUS codec (http://www.opus-codec.org/)
  );

  PPjack_slave_t = ^Pjack_slave_t;
  Pjack_slave_t = ^jack_slave_t;
  jack_slave_t = record
    audio_input: cint;    // from master or to slave (-1 to take master audio physical inputs)
    audio_output: cint;   // to master or from slave (-1 to take master audio physical outputs)
    midi_input: cint;     // from master or to slave (-1 to take master MIDI physical inputs)
    midi_output: cint;    // to master or from slave (-1 to take master MIDI physical outputs)
    mtu: cint;            // network Maximum Transmission Unit
    time_out: cint;       // in second, -1 means infinite
    encoder: cint;        // encoder type (one of JackNetEncoder)
    kbps: cint;           // KB per second for CELT or OPUS codec
    latency: cint;        // network latency in number of buffers
  end;

  PPjack_master_t = ^Pjack_master_t;
  Pjack_master_t = ^jack_master_t;
  jack_master_t = record
    audio_input: cint;                                  // master audio physical outputs (-1 to take slave wanted audio inputs)
    audio_output: cint;                                 // master audio physical inputs (-1 to take slave wanted audio outputs)
    midi_input: cint;                                   // master MIDI physical outputs (-1 to take slave wanted MIDI inputs)
    midi_output: cint;                                  // master MIDI physical inputs (-1 to take slave wanted MIDI outputs)
    buffer_size: jack_nframes_t;                        // master buffer size
    sample_rate: jack_nframes_t;                        // master sample rate
    master_name: array [0..MASTER_NAME_SIZE-1] of Char; // master machine name
    time_out: cint;                                     // in second, -1 means infinite
    partial_cycle: cint;                                // if 'true', partial buffers will be used
  end;

(**
 *  jack_net_slave_t is an opaque type. You may only access it using the
 *  API provided.
 *)
  PPjack_net_slave_t = ^Pjack_net_slave_t;
  Pjack_net_slave_t = ^jack_net_slave_t;
  jack_net_slave_t = record end;

 (**
 * Open a network connection with the master machine.
 *
 * @param ip the multicast address of the master
 * @param port the connection port
 * @param name the JACK client name
 * @param request a connection request structure
 * @param result a connection result structure
 *
 * @return Opaque net handle if successful or NULL in case of error.
 *)
function jack_net_slave_open(const ip: PChar; port: cint; const name: PChar; request: Pjack_slave_t; result: Pjack_master_t): Pjack_net_slave_t; cdecl; external libjacknet;

(**
 * Close the network connection with the master machine.
 *
 * @param net the network connection to be closed
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
function jack_net_slave_close(net: Pjack_net_slave_t): cint; cdecl; external libjacknet;

(**
 * Prototype for Process callback.
 *
 * @param nframes buffer size
 * @param audio_input number of audio inputs
 * @param audio_input_buffer an array of audio input buffers (from master)
 * @param midi_input number of MIDI inputs
 * @param midi_input_buffer an array of MIDI input buffers (from master)
 * @param audio_output number of audio outputs
 * @param audio_output_buffer an array of audio output buffers (to master)
 * @param midi_output number of MIDI outputs
 * @param midi_output_buffer an array of MIDI output buffers (to master)
 * @param arg pointer to a client supplied structure supplied by jack_set_net_process_callback()
 *
 * @return zero on success, non-zero on error
 *)
type
  TJackNetSlaveProcessCallback = function(buffer_size: jack_nframes_t;
                                          audio_input: cint;
                                          audio_input_buffer: PPcfloat;
                                          midi_input: cint;
                                          midi_input_buffer: PPointer;
                                          audio_output: cint;
                                          audio_output_buffer: PPcfloat;
                                          midi_output: cint;
                                          midi_output_buffer: PPointer;
                                          data: Pointer): cint; cdecl;

(**
 * Set network process callback.
 *
 * @param net the network connection
 * @param net_callback the process callback
 * @param arg pointer to a client supplied structure
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
function jack_set_net_slave_process_callback(net: Pjack_net_slave_t; net_callback: TJackNetSlaveProcessCallback; arg: Pointer): cint; cdecl; external libjacknet;

(**
 * Start processing thread, the net_callback will start to be called.
 *
 * @param net the network connection
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
function jack_net_slave_activate(net: Pjack_net_slave_t): cint; cdecl; external libjacknet;

(**
 * Stop processing thread.
 *
 * @param net the network connection
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
function jack_net_slave_deactivate(net: Pjack_net_slave_t): cint; cdecl; external libjacknet;

(**
 * Test if slave is still active.
 *
 * @param net the network connection
 *
 * @return a boolean
 *)
function jack_net_slave_is_active(net: Pjack_net_slave_t): cint; cdecl; external libjacknet;

(**
 * Prototype for BufferSize callback.
 *
 * @param nframes buffer size
 * @param arg pointer to a client supplied structure supplied by jack_set_net_buffer_size_callback()
 *
 * @return zero on success, non-zero on error
 *)
type
  TJackNetSlaveBufferSizeCallback = function(nframes: jack_nframes_t; arg: Pointer): cint; cdecl;

(**
 * Set network buffer size callback.
 *
 * @param net the network connection
 * @param bufsize_callback the buffer size callback
 * @param arg pointer to a client supplied structure
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
function jack_set_net_slave_buffer_size_callback(net: Pjack_net_slave_t; bufsize_callback: TJackNetSlaveBufferSizeCallback; arg: Pointer): cint; cdecl; external libjacknet;

(**
 * Prototype for SampleRate callback.
 *
 * @param nframes sample rate
 * @param arg pointer to a client supplied structure supplied by jack_set_net_sample_rate_callback()
 *
 * @return zero on success, non-zero on error
 *)
type
  TJackNetSlaveSampleRateCallback = function(nframes: jack_nframes_t; arg: Pointer): cint; cdecl;

(**
 * Set network sample rate callback.
 *
 * @param net the network connection
 * @param samplerate_callback the sample rate callback
 * @param arg pointer to a client supplied structure
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
function jack_set_net_slave_sample_rate_callback(net: Pjack_net_slave_t; samplerate_callback: TJackNetSlaveSampleRateCallback; arg: Pointer): cint; cdecl; external libjacknet;

(**
 * Prototype for server Shutdown callback (if not set, the client will just restart, waiting for an available master again).
 *
 * @param arg pointer to a client supplied structure supplied by jack_set_net_shutdown_callback()
 *)
type
  TJackNetSlaveShutdownCallback = procedure(arg: Pointer); cdecl;

(**
 * Set network shutdown callback.
 *
 * @param net the network connection
 * @param shutdown_callback the shutdown callback
 * @param arg pointer to a client supplied structure
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
function jack_set_net_slave_shutdown_callback(net: Pjack_net_slave_t; shutdown_callback: TJackNetSlaveShutdownCallback; arg: Pointer): cint; cdecl; JACK_NET_OPTIONAL_WEAK_DEPRECATED_EXPORT;

(**
 * Prototype for server Restart callback : this is the new preferable way to be notified when the master has disappeared.
 * The client may want to retry connecting a certain number of time (which will be done using the time_out value given in jack_net_slave_open)
 * by returning 0. Otherwise returning a non-zero error code will definitely close the connection
 * (and jack_net_slave_is_active will later on return false).
 * If both Shutdown and Restart are supplied, Restart callback will be used.
 *
 * @param arg pointer to a client supplied structure supplied by jack_set_net_restart_callback()
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
type
  TJackNetSlaveRestartCallback = function(arg: Pointer): cint; cdecl;

(**
 * Set network restart callback.
 *
 * @param net the network connection
 * @param restart_callback the shutdown callback
 * @param arg pointer to a client supplied structure
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
function jack_set_net_slave_restart_callback(net: Pjack_net_slave_t; restart_callback: TJackNetSlaveRestartCallback; arg: Pointer): cint; cdecl; JACK_NET_OPTIONAL_WEAK_EXPORT;

(**
 * Prototype for server Error callback.
 *
 * @param error_code an error code (see "Possible error codes")
 * @param arg pointer to a client supplied structure supplied by jack_set_net_error_callback()
 *)
type
  TJackNetSlaveErrorCallback = procedure(error_code: cint; arg: Pointer); cdecl;

(**
 * Set error restart callback.
 *
 * @param net the network connection
 * @param error_callback the error callback
 * @param arg pointer to a client supplied structure
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
function jack_set_net_slave_error_callback(net: Pjack_net_slave_t; error_callback: TJackNetSlaveErrorCallback; arg: Pointer): cint; cdecl; JACK_NET_OPTIONAL_WEAK_EXPORT;

(**
 *  jack_net_master_t is an opaque type, you may only access it using the API provided.
 *)
type
  PPjack_net_master_t = ^Pjack_net_master_t;
  Pjack_net_master_t = ^jack_net_master_t;
  jack_net_master_t = record end;

 (**
 * Open a network connection with the slave machine.
 *
 * @param ip the multicast address of the master
 * @param port the connection port
 * @param request a connection request structure
 * @param result a connection result structure
 *
 * @return Opaque net handle if successful or NULL in case of error.
 *)
function jack_net_master_open(const ip: PChar; port: cint; request: Pjack_master_t; result: Pjack_slave_t): Pjack_net_master_t; cdecl; external libjacknet;

(**
 * Close the network connection with the slave machine.
 *
 * @param net the network connection to be closed
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
function jack_net_master_close(net: Pjack_net_master_t): cint; cdecl; external libjacknet;

(**
 * Receive sync and data from the network (complete buffer).
 *
 * @param net the network connection
 * @param audio_input number of audio inputs
 * @param audio_input_buffer an array of audio input buffers
 * @param midi_input number of MIDI inputs
 * @param midi_input_buffer an array of MIDI input buffers
 *
 * @return zero on success, non-zero on error
 *)
function jack_net_master_recv(net: Pjack_net_master_t; audio_input: cint; audio_input_buffer: PPcfloat; midi_input: cint; midi_input_buffer: PPointer): cint; cdecl; external libjacknet;

(**
 * Receive sync and data from the network (incomplete buffer).
 *
 * @param net the network connection
 * @param audio_input number of audio inputs
 * @param audio_input_buffer an array of audio input buffers
 * @param midi_input number of MIDI inputs
 * @param midi_input_buffer an array of MIDI input buffers
 * @param frames the number of frames to receive
 *
 * @return zero on success, non-zero on error
 *)
function jack_net_master_recv_slice(net: Pjack_net_master_t; audio_input: cint; audio_input_buffer: PPcfloat; midi_input: cint; midi_input_buffer: PPointer; frames: cint): cint; cdecl; external libjacknet;

(**
 * Send sync and data to the network (complete buffer).
 *
 * @param net the network connection
 * @param audio_output number of audio outputs
 * @param audio_output_buffer an array of audio output buffers
 * @param midi_output number of MIDI outputs
 * @param midi_output_buffer an array of MIDI output buffers
 *
 * @return zero on success, non-zero on error
 *)
function jack_net_master_send(net: Pjack_net_master_t; audio_output: cint; audio_output_buffer: PPcfloat; midi_output: cint; midi_output_buffer: PPointer): cint; cdecl; external libjacknet;

(**
 * Send sync and data to the network (incomplete buffer).
 *
 * @param net the network connection
 * @param audio_output number of audio outputs
 * @param audio_output_buffer an array of audio output buffers
 * @param midi_output number of MIDI outputs
 * @param midi_output_buffer an array of MIDI output buffers
 * @param frames the number of frames to send
 *
 * @return zero on success, non-zero on error
 *)
function jack_net_master_send_slice(net: Pjack_net_master_t; audio_output: cint; audio_output_buffer: PPcfloat; midi_output: cint; midi_output_buffer: PPointer; frames: cint): cint; cdecl; external libjacknet;

// Experimental Adapter API

(**
 *  jack_adapter_t is an opaque type, you may only access it using the API provided.
 *)
type
  PPjack_adapter_t = ^Pjack_adapter_t;
  Pjack_adapter_t = ^jack_adapter_t;
  jack_adapter_t = record end;

(**
 * Create an adapter.
 *
 * @param input number of audio inputs
 * @param output of audio outputs
 * @param host_buffer_size the host buffer size in frames
 * @param host_sample_rate the host buffer sample rate
 * @param adapted_buffer_size the adapted buffer size in frames
 * @param adapted_sample_rate the adapted buffer sample rate
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
function jack_create_adapter(input: cint; output: cint;
                                    host_buffer_size: jack_nframes_t;
                                    host_sample_rate: jack_nframes_t;
                                    adapted_buffer_size: jack_nframes_t;
                                    adapted_sample_rate: jack_nframes_t): Pjack_adapter_t; cdecl; external libjacknet;

(**
 * Destroy an adapter.
 *
 * @param adapter the adapter to be destroyed
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
function jack_destroy_adapter(adapter: Pjack_adapter_t): cint; cdecl; external libjacknet;

(**
 * Flush internal state of an adapter.
 *
 * @param adapter the adapter to be flushed
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
procedure jack_flush_adapter(adapter: Pjack_adapter_t); cdecl; external libjacknet;

(**
 * Push input to and pull output from adapter ringbuffer.
 *
 * @param adapter the adapter
 * @param input an array of audio input buffers
 * @param output an array of audio output buffers
 * @param frames number of frames
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
function jack_adapter_push_and_pull(adapter: jack_adapter_t; input: PPcfloat; output: PPcfloat; frames: cuint): cint; cdecl; external libjacknet;

(**
 * Pull input from and push output to adapter ringbuffer.
 *
 * @param adapter the adapter
 * @param input an array of audio input buffers
 * @param output an array of audio output buffers
 * @param frames number of frames
 *
 * @return 0 on success, otherwise a non-zero error code
 *)
function jack_adapter_pull_and_push(adapter: Pjack_adapter_t; input: PPcfloat; output: PPcfloat; frames: cuint): cint; cdecl; external libjacknet;

//#ifdef __cplusplus
//}
//#endif

{$endif __net_h__}
