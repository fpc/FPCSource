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

{
 * util_uri.h: External Interface of util_uri.c
 }

type
  schemes_t = record
    name: PChar;
    default_port: cushort;
  end;

const
  DEFAULT_FTP_DATA_PORT	= 20;
  DEFAULT_FTP_PORT	= 21;
  DEFAULT_GOPHER_PORT	= 70;
  DEFAULT_NNTP_PORT	= 119;
  DEFAULT_WAIS_PORT	= 210;
  DEFAULT_SNEWS_PORT	= 563;
  DEFAULT_PROSPERO_PORT	= 1525;	{ WARNING: conflict w/Oracle }

  DEFAULT_URI_SCHEME = 'http';

{ Flags passed to unparse_uri_components(): }
  UNP_OMITSITEPART    = (1 shl 0);	{ suppress "scheme://user@site:port" }
  UNP_OMITUSER 	      = (1 shl 1);	{ Just omit user }
  UNP_OMITPASSWORD    = (1 shl 2);	{ Just omit password }
  UNP_OMITUSERINFO    = (UNP_OMITUSER or UNP_OMITPASSWORD);	{ omit "user:password@" part }
  UNP_REVEALPASSWORD  = (1 shl 3);	{ Show plain text password (default: show XXXXXXXX) }
  UNP_OMITPATHINFO    = (1 shl 4);	{ Show "scheme://user@site:port" only }
  UNP_OMITQUERY	      = (1 shl 5);	{ Omit the "?queryarg" from the path }

type
  hostent = record end;
  Phostent = ^hostent;

  uri_components = record
    scheme: PChar;		{ scheme ("http"/"ftp"/...) }
    hostinfo: PChar;            { combined [user[:password]@]host[:port] }
    user: PChar;		{ user name, as in http://user:passwd@host:port/ }
    password: PChar;		{ password, as in http://user:passwd@host:port/ }
    hostname: PChar;		{ hostname from URI (or from Host: header) }
    port_str: PChar;		{ port string (integer representation is in "port") }
    path: PChar;		{ the request path (or "/" if only scheme://host was given) }
    query: PChar;		{ Everything after a '?' in the path, if present }
    fragment: PChar;		{ Trailing "#fragment" string, if present }

    hostent: Phostent;

    port: cushort;	{ The port number, numeric, valid only if port_str != NULL }
    
//    unsigned is_initialized:1;

//    unsigned dns_looked_up:1;
//    unsigned dns_resolved:1;

  end;

{ util_uri.c }
{API_EXPORT(unsigned short) ap_default_port_for_scheme(const char *scheme_str);
API_EXPORT(unsigned short) ap_default_port_for_request(const request_rec *r);
API_EXPORT(struct hostent *) ap_pduphostent(pool *p, const struct hostent *hp);
API_EXPORT(struct hostent *) ap_pgethostbyname(pool *p, const char *hostname);
API_EXPORT(char *) ap_unparse_uri_components(pool *p, const uri_components *uptr,
    unsigned flags);
API_EXPORT(int) ap_parse_uri_components(pool *p, const char *uri, uri_components *uptr);
API_EXPORT(int) ap_parse_hostinfo_components(pool *p, const char *hostinfo, uri_components *uptr);}
{ called by the core in main() }
//extern void ap_util_uri_init(void);

