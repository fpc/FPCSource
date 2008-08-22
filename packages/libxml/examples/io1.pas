(**
 * section: InputOutput
 * synopsis: Example of custom Input/Output
 * purpose: Demonstrate the use of xmlRegisterInputCallbacks
 *          to build a custom I/O layer, this is used in an
 *          XInclude method context to show how dynamic document can
 *          be built in a clean way.
 * usage: io1
 * test: io1 > io1.tmp ; diff io1.tmp io1.res ; rm -f io1.tmp
 * author: Daniel Veillard
 * copy: see Copyright for the status of this software.
 *)

program io1;

{$mode objfpc}

uses
  ctypes,
  libxml2,
  exutils;

const
  include: pchar =
    '<?xml version=''1.0''?>'#10+
    '<document xmlns:xi="http://www.w3.org/2003/XInclude">'#10+
      '<p>List of people:</p>'#10+
      '<xi:include href="sql:select_name_from_people"/>'#10+
    '</document>'#10;


(**
 * sqlMatch:
 * @URI: an URI to test
 *
 * Check for an sql: query
 *
 * Returns 1 if yes and 0 if another Input module should be used
 *)
function sqlMatch(URI: pchar): cint; cdecl;
begin
  if assigned(URI) {and (strncmp(URI, 'sql:', 4) = 0)} then
    result := 1
  else
    result := 0;
end;

(**
 * sqlOpen:
 * @URI: an URI to test
 *
 * Return a pointer to the sql: query handler, in this example simply
 * the current pointer...
 *
 * Returns an Input context or NULL in case or error
 *)
function sqlOpen(URI: pchar): pointer; cdecl;
begin
  if not assigned(URI) or (strncmp(URI, 'sql:', 4) <> 0) then
    exit(nil);

  
end;
{
    if ((URI == NULL) || (strncmp(URI, "sql:", 4)))
        return(NULL);
    cur = result;
    rlen = strlen(result);
    return((void *) cur);
}

(**
 * sqlClose:
 * @context: the read context
 *
 * Close the sql: query handler
 *
 * Returns 0 or -1 in case of error
 *)
function sqlClose(context: pointer): cint; cdecl;
begin
end; {
    if (context == NULL) return(-1);
    cur = NULL;
    rlen = 0;
    return(0);
}

(**
 * sqlRead:
 * @context: the read context
 * @buffer: where to store data
 * @len: number of bytes to read
 *
 * Implement an sql: query read.
 *
 * Returns the number of bytes read or -1 in case of error
 *)
function sqlRead(context: pointer; buffer: pchar; len: cint): cint; cdecl;
begin
end;
{
   const char *ptr = (const char *) context;

   if ((context == NULL) || (buffer == NULL) || (len < 0))
       return(-1);

   if (len > rlen) len = rlen;
   memcpy(buffer, ptr, len);
   rlen -= len;
   return(len);
}


var
  doc: xmlDocPtr;

begin
  (*
   * this initialize the library and check potential ABI mismatches
   * between the version it was compiled for and the actual shared
   * library used.
   *)
  LIBXML_TEST_VERSION;

  (*
   * register the new I/O handlers
   *)
  if xmlRegisterInputCallbacks(@sqlMatch, @sqlOpen, @sqlRead, @sqlClose) < 0 then
  begin
    printfn('failed to register SQL handler');
    halt(1);
  end;

  (*
   * parse include into a document
   *)
  doc := xmlReadMemory(include, strlen(include), 'include.xml', nil, 0);
  if doc = nil then
  begin
    printfn('failed to parse the including file');
    halt(1);
  end;

  (*
   * apply the XInclude process, this should trigger the I/O just
   * registered.
   *)
  if xmlXIncludeProcess(doc) <= 0 then
  begin
    printfn('XInclude processing failed');
    halt(1);
  end;

  (*
   * save the output for checking to stdout
   *)
//  xmlDocDump(stdout, doc);

  (*
   * Free the document
   *)
  xmlFreeDoc(doc);

  (*
   * Cleanup function for the XML library.
   *)
  xmlCleanupParser();
end.