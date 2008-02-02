{ Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 }

{ called before any config is read }
procedure ap_init_vhost_config(p: PPool);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ called after the config has been read }
procedure ap_fini_vhost_config(p: PPool; main_server: Pserver_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ handle addresses in <VirtualHost> statement }
function ap_parse_vhost_addrs(p: PPool; const hostname: PChar; s: Pserver_rec): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ handle NameVirtualHost directive }
//function ap_set_name_virtual_host(cmd: Pcmd_parms; dummy: Pointer; arg: PChar): PChar;
// cdecl; external LibHTTPD;

{ given an ip address only, give our best guess as to what vhost it is }
procedure ap_update_vhost_given_ip(conn: Pconn_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ The above is never enough, and this is always called after the headers
 * have been read.  It may change r->server.
 }
procedure ap_update_vhost_from_headers(r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ return 1 if the host:port matches any of the aliases of r->server
 * return 0 otherwise
 }
function ap_matches_request_vhost(r: Prequest_rec; const host: PChar;
 port: cuint): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

