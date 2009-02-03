(*
 * Summary: minimal FTP implementation
 * Description: minimal FTP implementation allowing to fetch resources
 *              like external subset.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_FTP_ENABLED}

{$IFDEF TYPE}
(**
 * ftpListCallback: 
 * @userData:  user provided data for the callback
 * @filename:  the file name (including "->" when links are shown)
 * @attrib:  the attribute string
 * @owner:  the owner string
 * @group:  the group string
 * @size:  the file size
 * @links:  the link count
 * @year:  the year
 * @month:  the month
 * @day:  the day
 * @hour:  the hour
 * @minute:  the minute
 *
 * A callback for the xmlNanoFTPList command.
 * Note that only one of year and day:minute are specified.
 *)
  ftpListCallback = procedure(userData: pointer; filename, attrib, owner, group: pchar; size: culong; links, year: cint; month: pchar; day, hour, minute: cint); EXTDECL;

(**
 * ftpDataCallback: 
 * @userData: the user provided context
 * @data: the data received
 * @len: its size in bytes
 *
 * A callback for the xmlNanoFTPGet command.
 *)
  ftpDataCallback = procedure(userData: pointer; data: pchar; len: cint); EXTDECL;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Init
 *)
procedure xmlNanoFTPInit; EXTDECL; external xml2lib;
procedure xmlNanoFTPCleanup; EXTDECL; external xml2lib;

(*
 * Creating/freeing contexts.
 *)
function xmlNanoFTPNewCtxt(URL: pchar): pointer; EXTDECL; external xml2lib;
procedure xmlNanoFTPFreeCtxt(ctx: pointer); EXTDECL; external xml2lib;
function xmlNanoFTPConnectTo(server: pchar; port: cint): pointer; EXTDECL; external xml2lib;

(*
 * Opening/closing session connections.
 *)
function xmlNanoFTPOpen(URL: pchar): pointer; EXTDECL; external xml2lib;
function xmlNanoFTPConnect(ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlNanoFTPClose(ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlNanoFTPQuit(ctx: pointer): cint; EXTDECL; external xml2lib;
procedure xmlNanoFTPScanProxy(URL: pchar); EXTDECL; external xml2lib;
procedure xmlNanoFTPProxy(host: pchar; port: cint; user, passwd: pchar; _type: cint); EXTDECL; external xml2lib;
function xmlNanoFTPUpdateURL(ctx: pointer; URL: pchar): cint; EXTDECL; external xml2lib;

(*
 * Rather internal commands.
 *)
function xmlNanoFTPGetResponse(ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlNanoFTPCheckResponse(ctx: pointer): cint; EXTDECL; external xml2lib;

(*
 * CD/DIR/GET handlers.
 *)
function xmlNanoFTPCwd(ctx: pointer; directory: pchar): cint; EXTDECL; external xml2lib;
function xmlNanoFTPGetResponse(ctx: pointer; _file: pchar): cint; EXTDECL; external xml2lib;
function xmlNanoFTPGetConnection(ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlNanoFTPCloseConnection(ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlNanoFTPList(ctx: pointer; callback: ftpListCallback; userData: pointer; filename: pchar): cint; EXTDECL; external xml2lib;
function xmlNanoFTPGetSocket(ctx: pointer; filename: pchar): cint; EXTDECL; external xml2lib;
function xmlNanoFTPGet(ctx: pointer; callback: ftpDataCallback; userData: pointer; filename: pchar): cint; EXTDECL; external xml2lib;
function xmlNanoFTPRead(ctx: pointer; dest: pointer; len: cint): cint; EXTDECL; external xml2lib;

{$ENDIF}
{$ENDIF} (* LIBXML_FTP_ENABLED *)
