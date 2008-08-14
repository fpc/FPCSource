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
  ftpListCallback = procedure(userData: pointer; filename, attrib, owner, group: pchar; size: culong; links, year: cint; month: pchar; day, hour, minute: cint); XMLCALL;

(**
 * ftpDataCallback: 
 * @userData: the user provided context
 * @data: the data received
 * @len: its size in bytes
 *
 * A callback for the xmlNanoFTPGet command.
 *)
  ftpDataCallback = procedure(userData: pointer; data: pchar; len: cint); XMLCALL;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Init
 *)
procedure xmlNanoFTPInit; XMLCALL; XMLPUBFUN;
procedure xmlNanoFTPCleanup; XMLCALL; XMLPUBFUN;

(*
 * Creating/freeing contexts.
 *)
function xmlNanoFTPNewCtxt(URL: pchar): pointer; XMLCALL; XMLPUBFUN;
procedure xmlNanoFTPFreeCtxt(ctx: pointer); XMLCALL; XMLPUBFUN;
function xmlNanoFTPConnectTo(server: pchar; port: cint): pointer; XMLCALL; XMLPUBFUN;

(*
 * Opening/closing session connections.
 *)
function xmlNanoFTPOpen(URL: pchar): pointer; XMLCALL; XMLPUBFUN;
function xmlNanoFTPConnect(ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlNanoFTPClose(ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlNanoFTPQuit(ctx: pointer): cint; XMLCALL; XMLPUBFUN;
procedure xmlNanoFTPScanProxy(URL: pchar); XMLCALL; XMLPUBFUN;
procedure xmlNanoFTPProxy(host: pchar; port: cint; user, passwd: pchar; _type: cint); XMLCALL; XMLPUBFUN;
function xmlNanoFTPUpdateURL(ctx: pointer; URL: pchar): cint; XMLCALL; XMLPUBFUN;

(*
 * Rather internal commands.
 *)
function xmlNanoFTPGetResponse(ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlNanoFTPCheckResponse(ctx: pointer): cint; XMLCALL; XMLPUBFUN;

(*
 * CD/DIR/GET handlers.
 *)
function xmlNanoFTPCwd(ctx: pointer; directory: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlNanoFTPGetResponse(ctx: pointer; _file: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlNanoFTPGetConnection(ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlNanoFTPCloseConnection(ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlNanoFTPList(ctx: pointer; callback: ftpListCallback; userData: pointer; filename: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlNanoFTPGetSocket(ctx: pointer; filename: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlNanoFTPGet(ctx: pointer; callback: ftpDataCallback; userData: pointer; filename: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlNanoFTPRead(ctx: pointer; dest: pointer; len: cint): cint; XMLCALL; XMLPUBFUN;

{$ENDIF}
{$ENDIF} (* LIBXML_FTP_ENABLED *)
