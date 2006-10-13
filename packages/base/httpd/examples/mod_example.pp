{ Copyright 1999-2005 The Apache Software Foundation or its licensors, as
 * applicable.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
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
 * Apache example module.  Provide demonstrations of how modules do things.
 * It is not meant to be used in a production server.  Since it participates
 * in all of the processing phases, it could conceivable interfere with
 * the proper operation of other modules -- particularly the ones related
 * to security.
 *
 * In the interest of brevity, all functions and structures internal to
 * this module, but which may have counterparts in *real* modules, are
 * prefixed with 'x_' instead of 'example_'.
 }
library mod_example;

{$i define.inc}

uses
  Classes, SysUtils, httpd, apr, aprutil;

var
 example_module: module; {$ifdef Unix} public name 'example_module'; {$endif}
 default_module_ptr: Pmodule;

{$ifdef WINDOWS}
exports
 example_module name 'example_module';
{$endif}

const
  MODULE_NAME = 'mod_example.so';

{--------------------------------------------------------------------------}
{                                                                          }
{ Data declarations.                                                       }
{                                                                          }
{ Here are the static cells and structure declarations private to our      }
{ module.                                                                  }
{                                                                          }
{--------------------------------------------------------------------------}

{
 * Sample configuration record.  Used for both per-directory and per-server
 * configuration data.
 *
 * It's perfectly reasonable to have two different structures for the two
 * different environments.  The same command handlers will be called for
 * both, though, so the handlers need to be able to tell them apart.  One
 * possibility is for both structures to start with an int which is 0 for
 * one and 1 for the other.
 *
 * Note that while the per-directory and per-server configuration records are
 * available to most of the module handlers, they should be treated as
 * READ-ONLY by all except the command and merge handlers.  Sometimes handlers
 * are handed a record that applies to the current location by implication or
 * inheritance, and modifying it will change the rules for other locations.
 }
const
  CONFIG_MODE_SERVER = 1;
  CONFIG_MODE_DIRECTORY = 2;
  CONFIG_MODE_COMBO = 3;     { Shouldn't ever happen. }

type
  x_cfg = record
    cmode: Integer;              { Environment to which record applies
                                 * (directory, server, or combination).
                                 }
    local: Integer;                  { Boolean: "Example" directive declared
                                 * here?
                                 }
    congenital: Integer;             { Boolean: did we inherit an "Example"? }
    trace: PChar;                { Pointer to trace string. }
    loc: PChar;                  { Location to which this record applies. }
  end;

  Px_cfg = ^x_cfg;

{
 * Let's set up a module-local static cell to point to the accreting callback
 * trace.  As each API callback is made to us, we'll tack on the particulars
 * to whatever we've already recorded.  To avoid massive memory bloat as
 * directories are walked again and again, we record the routine/environment
 * the first time (non-request context only), and ignore subsequent calls for
 * the same routine/environment.
 }
var
  trace: PChar = nil;
  static_calls_made: Papr_table_t = nil;

{
 * To avoid leaking memory from pools other than the per-request one, we
 * allocate a module-private pool, and then use a sub-pool of that which gets
 * freed each time we modify the trace.  That way previous layers of trace
 * data don't get lost.
 }
  x_pool: Papr_pool_t = nil;
  x_subpool: Papr_pool_t = nil;

{--------------------------------------------------------------------------}
{                                                                          }
{ The following pseudo-prototype declarations illustrate the parameters    }
{ passed to command handlers for the different types of directive          }
{ syntax.  If an argument was specified in the directive definition        }
{ (look for "command_rec" below), it's available to the command handler    }
{ via the (void *) info field in the cmd_parms argument passed to the      }
{ handler (cmd->info for the examples below).                              }
{                                                                          }
{--------------------------------------------------------------------------}

{
 * Command handler for a NO_ARGS directive.  Declared in the command_rec
 * list with
 *   AP_INIT_NO_ARGS("directive", function, mconfig, where, help)
 *
 * static const char *handle_NO_ARGS(cmd_parms *cmd, void *mconfig);
 }

{
 * Command handler for a RAW_ARGS directive.  The "args" argument is the text
 * of the commandline following the directive itself.  Declared in the
 * command_rec list with
 *   AP_INIT_RAW_ARGS("directive", function, mconfig, where, help)
 *
 * static const char *handle_RAW_ARGS(cmd_parms *cmd, void *mconfig,
 *                                    const char *args);
 }

{
 * Command handler for a FLAG directive.  The single parameter is passed in
 * "bool", which is either zero or not for Off or On respectively.
 * Declared in the command_rec list with
 *   AP_INIT_FLAG("directive", function, mconfig, where, help)
 *
 * static const char *handle_FLAG(cmd_parms *cmd, void *mconfig, int bool);
 }

{
 * Command handler for a TAKE1 directive.  The single parameter is passed in
 * "word1".  Declared in the command_rec list with
 *   AP_INIT_TAKE1("directive", function, mconfig, where, help)
 *
 * static const char *handle_TAKE1(cmd_parms *cmd, void *mconfig,
 *                                 char *word1);
 }

{
 * Command handler for a TAKE2 directive.  TAKE2 commands must always have
 * exactly two arguments.  Declared in the command_rec list with
 *   AP_INIT_TAKE2("directive", function, mconfig, where, help)
 *
 * static const char *handle_TAKE2(cmd_parms *cmd, void *mconfig,
 *                                 char *word1, char *word2);
 }

{
 * Command handler for a TAKE3 directive.  Like TAKE2, these must have exactly
 * three arguments, or the parser complains and doesn't bother calling us.
 * Declared in the command_rec list with
 *   AP_INIT_TAKE3("directive", function, mconfig, where, help)
 *
 * static const char *handle_TAKE3(cmd_parms *cmd, void *mconfig,
 *                                 char *word1, char *word2, char *word3);
 }

{
 * Command handler for a TAKE12 directive.  These can take either one or two
 * arguments.
 * - word2 is a NULL pointer if no second argument was specified.
 * Declared in the command_rec list with
 *   AP_INIT_TAKE12("directive", function, mconfig, where, help)
 *
 * static const char *handle_TAKE12(cmd_parms *cmd, void *mconfig,
 *                                  char *word1, char *word2);
 }

{
 * Command handler for a TAKE123 directive.  A TAKE123 directive can be given,
 * as might be expected, one, two, or three arguments.
 * - word2 is a NULL pointer if no second argument was specified.
 * - word3 is a NULL pointer if no third argument was specified.
 * Declared in the command_rec list with
 *   AP_INIT_TAKE123("directive", function, mconfig, where, help)
 *
 * static const char *handle_TAKE123(cmd_parms *cmd, void *mconfig,
 *                                   char *word1, char *word2, char *word3);
 }

{
 * Command handler for a TAKE13 directive.  Either one or three arguments are
 * permitted - no two-parameters-only syntax is allowed.
 * - word2 and word3 are NULL pointers if only one argument was specified.
 * Declared in the command_rec list with
 *   AP_INIT_TAKE13("directive", function, mconfig, where, help)
 *
 * static const char *handle_TAKE13(cmd_parms *cmd, void *mconfig,
 *                                  char *word1, char *word2, char *word3);
 }

{
 * Command handler for a TAKE23 directive.  At least two and as many as three
 * arguments must be specified.
 * - word3 is a NULL pointer if no third argument was specified.
 * Declared in the command_rec list with
 *   AP_INIT_TAKE23("directive", function, mconfig, where, help)
 *
 * static const char *handle_TAKE23(cmd_parms *cmd, void *mconfig,
 *                                  char *word1, char *word2, char *word3);
 }

{
 * Command handler for a ITERATE directive.
 * - Handler is called once for each of n arguments given to the directive.
 * - word1 points to each argument in turn.
 * Declared in the command_rec list with
 *   AP_INIT_ITERATE("directive", function, mconfig, where, help)
 *
 * static const char *handle_ITERATE(cmd_parms *cmd, void *mconfig,
 *                                   char *word1);
 }

{
 * Command handler for a ITERATE2 directive.
 * - Handler is called once for each of the second and subsequent arguments
 *   given to the directive.
 * - word1 is the same for each call for a particular directive instance (the
 *   first argument).
 * - word2 points to each of the second and subsequent arguments in turn.
 * Declared in the command_rec list with
 *   AP_INIT_ITERATE2("directive", function, mconfig, where, help)
 *
 * static const char *handle_ITERATE2(cmd_parms *cmd, void *mconfig,
 *                                    char *word1, char *word2);
 }

{--------------------------------------------------------------------------}
{                                                                          }
{ These routines are strictly internal to this module, and support its     }
{ operation.  They are not referenced by any external portion of the       }
{ server.                                                                  }
{                                                                          }
{--------------------------------------------------------------------------}

{
 * Locate our directory configuration record for the current request.
 }
function our_dconfig(const r: Prequest_rec): Px_cfg; cdecl;
begin
  Result := Px_cfg(ap_get_module_config(r^.per_dir_config, @example_module));
end;

//#if 0
{
 * Locate our server configuration record for the specified server.
 }
function our_sconfig(const s: Pserver_rec): Px_cfg; cdecl;
begin
  Result := Px_cfg(ap_get_module_config(s^.module_config, @example_module));
end;

{
 * Likewise for our configuration record for the specified request.
 }
function our_rconfig(const r: Prequest_rec): Px_cfg; cdecl;
begin
  Result := Px_cfg(ap_get_module_config(r^.request_config, @example_module));
end;
//#endif

{
 * Likewise for our configuration record for a connection.
 }
function our_cconfig(const c: Pconn_rec): Px_cfg; cdecl;
begin
  Result := Px_cfg(ap_get_module_config(c^.conn_config, @example_module));
end;

{
 * This routine sets up some module-wide cells if they haven't been already.
 }
procedure setup_module_cells; cdecl;
begin
    {
     * If we haven't already allocated our module-private pool, do so now.
     }
    if (x_pool = nil) then apr_pool_create(@x_pool, nil);

    {
     * Likewise for the table of routine/environment pairs we visit outside of
     * request context.
     }
    if (static_calls_made = nil) then static_calls_made := apr_table_make(x_pool, 16);
end;

{
 * This routine is used to add a trace of a callback to the list.  We're
 * passed the server record (if available), the request record (if available),
 * a pointer to our private configuration record (if available) for the
 * environment to which the callback is supposed to apply, and some text.  We
 * turn this into a textual representation and add it to the tail of the list.
 * The list can be displayed by the x_handler() routine.
 *
 * If the call occurs within a request context (i.e., we're passed a request
 * record), we put the trace into the request apr_pool_t and attach it to the
 * request via the notes mechanism.  Otherwise, the trace gets added
 * to the static (non-request-specific) list.
 *
 * Note that the r^.notes table is only for storing strings; if you need to
 * maintain per-request data of any other type, you need to use another
 * mechanism.
 }

const
  TRACE_NOTE = 'example-trace';

  EXAMPLE_LOG_EACH = 0;

procedure trace_add(s: Pserver_rec; r: Prequest_rec; mconfig: Px_cfg; const note: PChar); cdecl;
var
  sofar, addon, where, trace_copy, key: PChar;
  p: Papr_pool_t;
begin
    {
     * Make sure our pools and tables are set up - we need 'em.
     }
    setup_module_cells();
    {
     * Now, if we're in request-context, we use the request pool.
     }
    if (r <> nil) then
    begin
        p := r^.pool;
        trace_copy := apr_table_get(r^.notes, TRACE_NOTE);
        if (trace_copy = nil) then trace_copy := '';
    end
    else
    begin
        {
         * We're not in request context, so the trace gets attached to our
         * module-wide pool.  We do the create/destroy every time we're called
         * in non-request context; this avoids leaking memory in some of
         * the subsequent calls that allocate memory only once (such as the
         * key formation below).
         *
         * Make a new sub-pool and copy any existing trace to it.  Point the
         * trace cell at the copied value.
         }
        apr_pool_create(@p, x_pool);
        if (trace <> nil) then trace := apr_pstrdup(p, trace);

        {
         * Now, if we have a sub-pool from before, nuke it and replace with
         * the one we just allocated.
         }
        if (x_subpool <> nil) then apr_pool_destroy(x_subpool);

        x_subpool := p;
        trace_copy := trace;
    end;
    {
     * If we weren't passed a configuration record, we can't figure out to
     * what location this call applies.  This only happens for co-routines
     * that don't operate in a particular directory or server context.  If we
     * got a valid record, extract the location (directory or server) to which
     * it applies.
     }
    {
       Translation note. The part bellow is commented because there is an unidentified
      problem with it.
     }
    {if (mconfig <> nil) then where := mconfig^.loc
    else} where := 'nowhere';
    if (where = nil) then where := '';
    {
     * Now, if we're not in request context, see if we've been called with
     * this particular combination before.  The apr_table_t is allocated in the
     * module's private pool, which doesn't get destroyed.
     }
    if (r = nil) then
    begin
        key := apr_pstrcat(p, [note, PChar(':'), where, nil]);

        if (apr_table_get(static_calls_made, key) <> nil) then
            {
             * Been here, done this.
             }
            Exit
        else
            {
             * First time for this combination of routine and environment -
             * log it so we don't do it again.
             }
            apr_table_set(static_calls_made, key, 'been here');
    end;
    addon := apr_pstrcat(p, [
                        PChar('   <li>' + LineEnding +
                        '    <dl>' + LineEnding +
                        '     <dt><samp>'), note, PChar('</samp></dt>' + LineEnding +
                        '     <dd><samp>['), where, PChar(']</samp></dd>' + LineEnding +
                        '    </dl>' + LineEnding +
                        '   </li>' + LineEnding),
                        nil]);

    if (trace_copy = nil) then sofar := '' else sofar := trace_copy;
    
    trace_copy := apr_pstrcat(p, [sofar, addon, nil]);
    if (r <> nil) then apr_table_set(r^.notes, TRACE_NOTE, trace_copy)
    else trace := trace_copy;

    {
     * You *could* change the following if you wanted to see the calling
     * sequence reported in the server's error_log, but beware - almost all of
     * these co-routines are called for every single request, and the impact
     * on the size (and readability) of the error_log is considerable.
     }
    if ((EXAMPLE_LOG_EACH = 0) and (s <> nil)) then
      ap_log_error(MODULE_NAME, 438, APLOG_DEBUG, 0, s, 'mod_example: ', [note]);
end;

{--------------------------------------------------------------------------}
{ We prototyped the various syntax for command handlers (routines that     }
{ are called when the configuration parser detects a directive declared    }
{ by our module) earlier.  Now we actually declare a "real" routine that   }
{ will be invoked by the parser when our "real" directive is               }
{ encountered.                                                             }
{                                                                          }
{ If a command handler encounters a problem processing the directive, it   }
{ signals this fact by returning a non-NULL pointer to a string            }
{ describing the problem.                                                  }
{                                                                          }
{ The magic return value DECLINE_CMD is used to deal with directives       }
{ that might be declared by multiple modules.  If the command handler      }
{ returns NULL, the directive was processed; if it returns DECLINE_CMD,    }
{ the next module (if any) that declares the directive is given a chance   }
{ at it.  If it returns any other value, it's treated as the text of an    }
{ error message.                                                           }
{--------------------------------------------------------------------------}
{
 * Command handler for the NO_ARGS "Example" directive.  All we do is mark the
 * call in the trace log, and flag the applicability of the directive to the
 * current location in that location's configuration record.
 }
function cmd_example(cmd: Pcmd_parms; mconfig: Pointer): PChar; cdecl;
var
  cfg: Px_cfg;
begin
  cfg := Px_cfg(mconfig);

  { "Example Wuz Here" }
  cfg^.local := 1;
  trace_add(cmd^.server, nil, cfg, 'cmd_example()');
  Result := nil;
end;

{--------------------------------------------------------------------------}
{                                                                          }
{ Now we declare our content handlers, which are invoked when the server   }
{ encounters a document which our module is supposed to have a chance to   }
{ see.  (See mod_mime's SetHandler and AddHandler directives, and the      }
{ mod_info and mod_status examples, for more details.)                     }
{                                                                          }
{ Since content handlers are dumping data directly into the connection     }
{ (using the r*() routines, such as rputs() and rprintf()) without         }
{ intervention by other parts of the server, they need to make             }
{ sure any accumulated HTTP headers are sent first.  This is done by       }
{ calling send_http_header().  Otherwise, no header will be sent at all,   }
{ and the output sent to the client will actually be HTTP-uncompliant.     }
{--------------------------------------------------------------------------}
{
 * Sample content handler.  All this does is display the call list that has
 * been built up so far.
 *
 * The return value instructs the caller concerning what happened and what to
 * do next:
 *  OK ("we did our thing")
 *  DECLINED ("this isn't something with which we want to get involved")
 *  HTTP_mumble ("an error status should be reported")
 }
function x_handler(r: Prequest_rec): Integer; cdecl;
var
  dcfg: Px_cfg;
  tempstr: PChar;
begin
    tempstr := 'Undefined';

    if not SameText(r^.handler, 'example-handler') then
    begin
      Result := DECLINED;
      Exit;
    end;

    dcfg := our_dconfig(r);
//    trace_add(r^.server, r, dcfg, 'x_handler()');
    {
     * We're about to start sending content, so we need to force the HTTP
     * headers to be sent at this point.  Otherwise, no headers will be sent
     * at all.  We can set any we like first, of course.  **NOTE** Here's
     * where you set the "Content-type" header, and you do so by putting it in
     * r^.content_type, *not* r^.headers_out("Content-type").  If you don't
     * set it, it will be filled in with the server's default type (typically
     * "text/plain").  You *must* also ensure that r^.content_type is lower
     * case.
     *
     * We also need to start a timer so the server can know if the connexion
     * is broken.
     }
    ap_set_content_type(r, 'text/html');
    {
     * If we're only supposed to send header information (HEAD request), we're
     * already there.
     }
    if (r^.header_only <> 0) then
    begin
      Result := OK;
      Exit;
    end;

    {
     * Now send our actual output.  Since we tagged this as being
     * "text/html", we need to embed any HTML.
     }
    ap_rputs(DOCTYPE_HTML_3_2, r);
    ap_rputs('<HTML>' + LineEnding, r);
    ap_rputs(' <HEAD>' + LineEnding, r);
    ap_rputs('  <TITLE>mod_example Module Content-Handler Output' + LineEnding, r);
    ap_rputs('  </TITLE>' + LineEnding, r);
    ap_rputs(' </HEAD>' + LineEnding, r);
    ap_rputs(' <BODY>' + LineEnding, r);
    ap_rputs('  <H1><SAMP>mod_example</SAMP> Module Content-Handler Output' + LineEnding, r);
    ap_rputs('  </H1>' + LineEnding, r);
    ap_rputs('  <P>' + LineEnding, r);
    ap_rprintf(r, '  Apache HTTP Server version: "%s"' + LineEnding, [ap_get_server_version()]);
    ap_rputs('  <BR>' + LineEnding, r);
    ap_rprintf(r, '  Server built: "%s"' + LineEnding, [ap_get_server_built()]);
    ap_rputs('  </P>' + LineEnding, r);;
    ap_rputs('  <P>' + LineEnding, r);
    ap_rputs('  The format for the callback trace is:' + LineEnding, r);
    ap_rputs('  </P>' + LineEnding, r);
    ap_rputs('  <DL>' + LineEnding, r);
    ap_rputs('   <DT><EM>n</EM>.<SAMP>&lt;routine-name&gt;', r);
    ap_rputs('(&lt;routine-data&gt;)</SAMP>' + LineEnding, r);
    ap_rputs('   </DT>' + LineEnding, r);
    ap_rputs('   <DD><SAMP>[&lt;applies-to&gt;]</SAMP>' + LineEnding, r);
    ap_rputs('   </DD>' + LineEnding, r);
    ap_rputs('  </DL>' + LineEnding, r);
    ap_rputs('  <P>' + LineEnding, r);
    ap_rputs('  The <SAMP>&lt;routine-data&gt;</SAMP> is supplied by' + LineEnding, r);
    ap_rputs('  the routine when it requests the trace,' + LineEnding, r);
    ap_rputs('  and the <SAMP>&lt;applies-to&gt;</SAMP> is extracted' + LineEnding, r);
    ap_rputs('  from the configuration record at the time of the trace.' + LineEnding, r);
    ap_rputs('  <STRONG>SVR()</STRONG> indicates a server environment' + LineEnding, r);
    ap_rputs('  (blank means the main or default server, otherwise it''s' + LineEnding, r);
    ap_rputs('  the name of the VirtualHost); <STRONG>DIR()</STRONG>' + LineEnding, r);
    ap_rputs('  indicates a location in the URL or filesystem' + LineEnding, r);
    ap_rputs('  namespace.' + LineEnding, r);
    ap_rputs('  </P>' + LineEnding, r);
    ap_rprintf(r, '  <H2>Static callbacks so far:</H2>' + LineEnding +
     '  <OL>' + LineEnding + '%s  </OL>' + LineEnding, [trace]);
    ap_rputs('  <H2>Request-specific callbacks so far:</H2>' + LineEnding, r);
    ap_rprintf(r, '  <OL>' + LineEnding + '%s  </OL>' + LineEnding, [apr_table_get(r^.notes, TRACE_NOTE)]);
    ap_rputs('  <H2>Environment for <EM>this</EM> call:</H2>' + LineEnding, r);
    ap_rputs('  <UL>' + LineEnding, r);
//    ap_rprintf(r, '   <LI>Applies-to: <SAMP>%s</SAMP>' + LineEnding + '   </LI>' + LineEnding, [dcfg^.loc]);

//    if dcfg^.local = 0 then tempstr := 'NO' else tempstr := 'Yes';

    ap_rprintf(r, '   <LI>"Example" directive declared here: %s' + LineEnding + '   </LI>' + LineEnding,
     [tempstr]);

//    if dcfg^.congenital = 0 then tempstr := 'NO' else tempstr := 'Yes';

    ap_rprintf(r, '   <LI>"Example" inherited: %s' + LineEnding + '   </LI>' + LineEnding, [tempstr]);
    ap_rputs('  </UL>' + LineEnding, r);
    ap_rputs(' </BODY>' + LineEnding, r);
    ap_rputs('</HTML>' + LineEnding, r);
    {
     * We're all done, so cancel the timeout we set.  Since this is probably
     * the end of the request we *could* assume this would be done during
     * post-processing - but it's possible that another handler might be
     * called and inherit our outstanding timer.  Not good; to each its own.
     }
    {
     * We did what we wanted to do, so tell the rest of the server we
     * succeeded.
     }
    Result := OK;
end;

{--------------------------------------------------------------------------}
{                                                                          }
{ Now let's declare routines for each of the callback phase in order.      }
{ (That's the order in which they're listed in the callback list, *not     }
{ the order in which the server calls them!  See the command_rec           }
{ declaration near the bottom of this file.)  Note that these may be       }
{ called for situations that don't relate primarily to our function - in   }
{ other words, the fixup handler shouldn't assume that the request has     }
{ to do with "example" stuff.                                              }
{                                                                          }
{ With the exception of the content handler, all of our routines will be   }
{ called for each request, unless an earlier handler from another module   }
{ aborted the sequence.                                                    }
{                                                                          }
{ Handlers that are declared as "int" can return the following:            }
{                                                                          }
{  OK          Handler accepted the request and did its thing with it.     }
{  DECLINED    Handler took no action.                                     }
{  HTTP_mumble Handler looked at request and found it wanting.             }
{                                                                          }
{ What the server does after calling a module handler depends upon the     }
{ handler's return value.  In all cases, if the handler returns            }
{ DECLINED, the server will continue to the next module with an handler    }
{ for the current phase.  However, if the handler return a non-OK,         }
{ non-DECLINED status, the server aborts the request right there.  If      }
{ the handler returns OK, the server's next action is phase-specific;      }
{ see the individual handler comments below for details.                   }
{                                                                          }
{--------------------------------------------------------------------------}
{
 * This function is called during server initialisation.  Any information
 * that needs to be recorded must be in static cells, since there's no
 * configuration record.
 *
 * There is no return value.
 }

{
 * This function is called when an heavy-weight process (such as a child) is
 * being run down or destroyed.  As with the child initialisation function,
 * any information that needs to be recorded must be in static cells, since
 * there's no configuration record.
 *
 * There is no return value.
 }

{
 * This function is called during server initialisation when an heavy-weight
 * process (such as a child) is being initialised.  As with the
 * module initialisation function, any information that needs to be recorded
 * must be in static cells, since there's no configuration record.
 *
 * There is no return value.
 }

{
 * This function gets called to create a per-directory configuration
 * record.  This will be called for the "default" server environment, and for
 * each directory for which the parser finds any of our directives applicable.
 * If a directory doesn't have any of our directives involved (i.e., they
 * aren't in the .htaccess file, or a <Location>, <Directory>, or related
 * block), this routine will *not* be called - the configuration for the
 * closest ancestor is used.
 *
 * The return value is a pointer to the created module-specific
 * structure.
 }
function x_create_dir_config(p: Papr_pool_t; dirspec: PChar): Pointer; cdecl;
var
  cfg: Px_cfg;
  dname: PChar;
begin
  dname := dirspec;

    {
     * Allocate the space for our record from the pool supplied.
     }
    cfg := Px_cfg(apr_pcalloc(p, sizeof(x_cfg)));
    {
     * Now fill in the defaults.  If there are any `parent' configuration
     * records, they'll get merged as part of a separate callback.
     }
    cfg^.local := 0;
    cfg^.congenital := 0;
    cfg^.cmode := CONFIG_MODE_DIRECTORY;
    {
     * Finally, add our trace to the callback list.
     }
    if dname = nil then dname := '';
    cfg^.loc := apr_pstrcat(p, [PChar('DIR('), dname, PChar(')'), nil]);
    trace_add(nil, nil, cfg, 'x_create_dir_config()');
    Result := Pointer(cfg);
end;

{
 * This function gets called to merge two per-directory configuration
 * records.  This is typically done to cope with things like .htaccess files
 * or <Location> directives for directories that are beneath one for which a
 * configuration record was already created.  The routine has the
 * responsibility of creating a new record and merging the contents of the
 * other two into it appropriately.  If the module doesn't declare a merge
 * routine, the record for the closest ancestor location (that has one) is
 * used exclusively.
 *
 * The routine MUST NOT modify any of its arguments!
 *
 * The return value is a pointer to the created module-specific structure
 * containing the merged values.
 }
function x_merge_dir_config(p: Papr_pool_t;
 parent_conf, newloc_conf: Pointer): Pointer; cdecl;
var
  merged_config, pconf, nconf: Px_cfg;
  note: PChar;
begin
    merged_config := Px_cfg(apr_pcalloc(p, sizeof(x_cfg)));
    pconf := Px_cfg(parent_conf);
    nconf := Px_cfg(newloc_conf);

    {
     * Some things get copied directly from the more-specific record, rather
     * than getting merged.
     }
    merged_config^.local := nconf^.local;
    merged_config^.loc := apr_pstrdup(p, nconf^.loc);
    {
     * Others, like the setting of the `congenital' flag, get ORed in.  The
     * setting of that particular flag, for instance, is TRUE if it was ever
     * true anywhere in the upstream configuration.
     }
    merged_config^.congenital := (pconf^.congenital or pconf^.local);
    {
     * If we're merging records for two different types of environment (server
     * and directory), mark the new record appropriately.  Otherwise, inherit
     * the current value.
     }
    if pconf^.cmode = nconf^.cmode then
     merged_config^.cmode := pconf^.cmode
    else merged_config^.cmode := CONFIG_MODE_COMBO;
    {
     * Now just record our being called in the trace list.  Include the
     * locations we were asked to merge.
     }
    note := apr_pstrcat(p, [PChar('x_merge_dir_config("'), pconf^.loc, PChar('","'),
     nconf^.loc, PChar('")'), nil]);
    trace_add(nil, nil, merged_config, note);
    Result := Pointer(merged_config);
end;

{
 * This function gets called to create a per-server configuration
 * record.  It will always be called for the "default" server.
 *
 * The return value is a pointer to the created module-specific
 * structure.
 }
function x_create_server_config(p: Papr_pool_t; s: Pserver_rec): Pointer; cdecl;
var
  cfg: Px_cfg;
  sname: PChar;
begin
    sname := s^.server_hostname;

    {
     * As with the x_create_dir_config() reoutine, we allocate and fill
     * in an empty record.
     }
    cfg := Px_cfg(apr_pcalloc(p, sizeof(x_cfg)));
    cfg^.local := 0;
    cfg^.congenital := 0;
    cfg^.cmode := CONFIG_MODE_SERVER;
    {
     * Note that we were called in the trace list.
     }
    if sname = nil then sname := '';
    cfg^.loc := apr_pstrcat(p, [PChar('SVR('), sname, PChar(')'), nil]);
    trace_add(s, nil, cfg, 'x_create_server_config()');
    Result := Pointer(cfg);
end;

{
 * This function gets called to merge two per-server configuration
 * records.  This is typically done to cope with things like virtual hosts and
 * the default server configuration  The routine has the responsibility of
 * creating a new record and merging the contents of the other two into it
 * appropriately.  If the module doesn't declare a merge routine, the more
 * specific existing record is used exclusively.
 *
 * The routine MUST NOT modify any of its arguments!
 *
 * The return value is a pointer to the created module-specific structure
 * containing the merged values.
 }
function x_merge_server_config(p: Papr_pool_t;
 server1_conf, server2_conf: Pointer): Pointer; cdecl;
var
  merged_config, s1conf, s2conf: Px_cfg;
  note: PChar;
begin
    merged_config := Px_cfg(apr_pcalloc(p, sizeof(x_cfg)));
    s1conf := Px_cfg(server1_conf);
    s2conf := Px_cfg(server2_conf);

    {
     * Our inheritance rules are our own, and part of our module's semantics.
     * Basically, just note whence we came.
     }
    if s1conf^.cmode = s2conf^.cmode then
     merged_config^.cmode := s1conf^.cmode
    else merged_config^.cmode := CONFIG_MODE_COMBO;
    
    merged_config^.local := s2conf^.local;
    merged_config^.congenital := (s1conf^.congenital or s1conf^.local);
    merged_config^.loc := apr_pstrdup(p, s2conf^.loc);
    {
     * Trace our call, including what we were asked to merge.
     }
    note := apr_pstrcat(p, [PChar('x_merge_server_config("'), s1conf^.loc, PChar('","'),
     s2conf^.loc, PChar('")'), nil]);
    trace_add(nil, nil, merged_config, note);
    Result := Pointer(merged_config);
end;

{
 * This routine is called before the server processes the configuration
 * files.  There is no return value.
 }
function x_pre_config(pconf, plog, ptemp: Papr_pool_t): Integer; cdecl;
begin
    {
     * Log the call and exit.
     }
    trace_add(nil, nil, nil, 'x_pre_config()');

    Result := OK;
end;

{
 * This routine is called to perform any module-specific fixing of header
 * fields, et cetera.  It is invoked just before any content-handler.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, the
 * server will still call any remaining modules with an handler for this
 * phase.
 }
function x_post_config(pconf, plog, ptemp: Papr_pool_t; s: Pserver_rec): Integer; cdecl;
begin
    {
     * Log the call and exit.
     }
    trace_add(nil, nil, nil, 'x_post_config()');
    Result := OK;
end;

{
 * This routine is called to perform any module-specific log file
 * openings. It is invoked just before the post_config phase
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, the
 * server will still call any remaining modules with an handler for this
 * phase.
 }
function x_open_logs(pconf, plog, ptemp: Papr_pool_t; s: Pserver_rec): Integer; cdecl;
begin
    {
     * Log the call and exit.
     }
    trace_add(s, nil, nil, 'x_open_logs()');
    Result := OK;
end;

{
 * All our process-death routine does is add its trace to the log.
 }
function x_child_exit(data: Pointer): apr_status_t; cdecl;
var
  note, sname: PChar;
  s: Pserver_rec;
begin
    s := data;
    sname := s^.server_hostname;

    {
     * The arbitrary text we add to our trace entry indicates for which server
     * we're being called.
     }
    if sname = nil then sname := '';
    note := apr_pstrcat(s^.process^.pool, [PChar('x_child_exit('), sname, PChar(')'), nil]);
    trace_add(s, nil, nil, note);
    Result := APR_SUCCESS;
end;

{
 * All our process initialiser does is add its trace to the log.
 }
procedure x_child_init(p: Papr_pool_t; s: Pserver_rec); cdecl;
var
  note, sname: PChar;
begin
  sname := s^.server_hostname;

    {
     * Set up any module cells that ought to be initialised.
     }
    setup_module_cells();
    {
     * The arbitrary text we add to our trace entry indicates for which server
     * we're being called.
     }
    if sname = nil then sname := '';
    note := apr_pstrcat(p, [PChar('x_child_init('), sname, PChar(')'), nil]);
    trace_add(s, nil, nil, note);

    apr_pool_cleanup_register(p, s, @x_child_exit, @x_child_exit);
end;

{
 * XXX: This routine is called XXX
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, the
 * server will still call any remaining modules with an handler for this
 * phase.
 }
//#if 0
function x_http_method(const r: Prequest_rec): PChar; cdecl;
var
  cfg: Px_cfg;
begin
  cfg := our_dconfig(r);

  // Log the call and exit.

  trace_add(r^.server, nil, cfg, 'x_http_method()');
  Result := 'foo';
end;

{
 * XXX: This routine is called XXX
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, the
 * server will still call any remaining modules with an handler for this
 * phase.
 }
function x_default_port(const r: Prequest_rec): apr_port_t; cdecl;
var
  cfg: Px_cfg;
begin
    cfg := our_dconfig(r);
    {
     * Log the call and exit.
     }
    trace_add(r^.server, nil, cfg, 'x_default_port()');
  Result := 80;
end;
//#endif {0}

{
 * XXX: This routine is called XXX
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, the
 * server will still call any remaining modules with an handler for this
 * phase.
 }
procedure x_insert_filter(r: Prequest_rec); cdecl;
var
  cfg: Px_cfg;
begin
    cfg := our_dconfig(r);
    {
     * Log the call and exit.
     }
    trace_add(r^.server, nil, cfg, 'x_insert_filter()');
end;

{
 * XXX: This routine is called XXX
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, the
 * server will still call any remaining modules with an handler for this
 * phase.
 }
function x_quick_handler(r: Prequest_rec; lookup_uri: Integer): Integer; cdecl;
var
  cfg: Px_cfg;
begin
  cfg := our_dconfig(r);
  
  { Log the call and exit. }
  
  trace_add(r^.server, nil, cfg, 'x_post_config()');
    
  Result := DECLINED;
end;

{
 * This routine is called just after the server accepts the connection,
 * but before it is handed off to a protocol module to be served.  The point
 * of this hook is to allow modules an opportunity to modify the connection
 * as soon as possible. The core server uses this phase to setup the
 * connection record based on the type of connection that is being used.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, the
 * server will still call any remaining modules with an handler for this
 * phase.
 }
function x_pre_connection(c: Pconn_rec; csd: Pointer): Integer; cdecl;
var
  cfg: Px_cfg;
begin
  cfg := our_cconfig(c);

{$ifdef 0}
  {
   * Log the call and exit.
   }
  trace_add(r^.server, nil, cfg, 'x_post_config()');
{$endif}

  Result := OK;
end;

{ This routine is used to actually process the connection that was received.
 * Only protocol modules should implement this hook, as it gives them an
 * opportunity to replace the standard HTTP processing with processing for
 * some other protocol.  Both echo and POP3 modules are available as
 * examples.
 *
 * The return VALUE is OK, DECLINED, or HTTP_mumble.  If we return OK, no
 * further modules are called for this phase.
 }
function x_process_connection(c: Pconn_rec): Integer; cdecl;
begin
  Result := DECLINED;
end;

{
 * This routine is called after the request has been read but before any other
 * phases have been processed.  This allows us to make decisions based upon
 * the input header fields.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, no
 * further modules are called for this phase.
 }
function x_post_read_request(r: Prequest_rec): Integer; cdecl;
var
  cfg: Px_cfg;
begin
  cfg := our_dconfig(r);
  {
   * We don't actually *do* anything here, except note the fact that we were
   * called.
   }
  trace_add(r^.server, r, cfg, 'x_post_read_request()');
  Result := DECLINED;
end;

{
 * This routine gives our module an opportunity to translate the URI into an
 * actual filename.  If we don't do anything special, the server's default
 * rules (Alias directives and the like) will continue to be followed.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, no
 * further modules are called for this phase.
 }
function x_translate_handler(r: Prequest_rec): Integer; cdecl;
var
  cfg: Px_cfg;
begin
  cfg := our_dconfig(r);
  {
   * We don't actually *do* anything here, except note the fact that we were
   * called.
   }
  trace_add(r^.server, r, cfg, 'x_translate_handler()');
    
  Result := DECLINED;
end;

{
 * this routine gives our module another chance to examine the request
 * headers and to take special action. This is the first phase whose
 * hooks' configuration directives can appear inside the <Directory>
 * and similar sections, because at this stage the URI has been mapped
 * to the filename. For example this phase can be used to block evil
 * clients, while little resources were wasted on these.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK,
 * the server will still call any remaining modules with an handler
 * for this phase.
 }
function x_header_parser_handler(r: Prequest_rec): Integer; cdecl;
var
  cfg: Px_cfg;
begin
  cfg := our_dconfig(r);
  {
   * We don't actually *do* anything here, except note the fact that we were
   * called.
   }
  trace_add(r^.server, r, cfg, 'header_parser_handler()');
  
  Result := DECLINED;
end;


{
 * This routine is called to check the authentication information sent with
 * the request (such as looking up the user in a database and verifying that
 * the [encrypted] password sent matches the one in the database).
 *
 * The return value is OK, DECLINED, or some HTTP_mumble error (typically
 * HTTP_UNAUTHORIZED).  If we return OK, no other modules are given a chance
 * at the request during this phase.
 }
function x_check_user_id(r: Prequest_rec): Integer; cdecl;
var
  cfg: Px_cfg;
begin
  cfg := our_dconfig(r);

  { Don't do anything except log the call. }
  
  trace_add(r^.server, r, cfg, 'x_check_user_id()');

  Result := DECLINED;
end;

{
 * This routine is called to check to see if the resource being requested
 * requires authorisation.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, no
 * other modules are called during this phase.
 *
 * If *all* modules return DECLINED, the request is aborted with a server
 * error.
 }
function x_auth_checker(r: Prequest_rec): Integer; cdecl;
var
  cfg: Px_cfg;
begin
  cfg := our_dconfig(r);
    
  { * Log the call and return OK, or access will be denied (even though we
    * didn't actually do anything). }
      
  trace_add(r^.server, r, cfg, 'x_auth_checker()');
  
  Result := DECLINED;
end;

{
 * This routine is called to check for any module-specific restrictions placed
 * upon the requested resource.  (See the mod_access module for an example.)
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  All modules with an
 * handler for this phase are called regardless of whether their predecessors
 * return OK or DECLINED.  The first one to return any other status, however,
 * will abort the sequence (and the request) as usual.
 }
function x_access_checker(r: Prequest_rec): Integer; cdecl;
var
  cfg: Px_cfg;
begin
  cfg := our_dconfig(r);
  trace_add(r^.server, r, cfg, 'x_access_checker()');
    
  Result := DECLINED;
end;

{
 * This routine is called to determine and/or set the various document type
 * information bits, like Content-type (via r^.content_type), language, et
 * cetera.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, no
 * further modules are given a chance at the request for this phase.
 }
function x_type_checker(r: Prequest_rec): Integer; cdecl;
var
  cfg: Px_cfg;
begin
  cfg := our_dconfig(r);

  {  * Log the call, but don't do anything else - and report truthfully that
     * we didn't do anything. }
     
  trace_add(r^.server, r, cfg, 'x_type_checker()');
    
  Result := DECLINED;
end;

{
 * This routine is called to perform any module-specific fixing of header
 * fields, et cetera.  It is invoked just before any content-handler.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, the
 * server will still call any remaining modules with an handler for this
 * phase.
 }
function x_fixer_upper(r: Prequest_rec): Integer; cdecl;
var
  cfg: Px_cfg;
begin
  cfg := our_dconfig(r);

  { Log the call and exit. }

  trace_add(r^.server, r, cfg, 'x_fixer_upper()');
    
  Result := OK;
end;

{
 * This routine is called to perform any module-specific logging activities
 * over and above the normal server things.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, any
 * remaining modules with an handler for this phase will still be called.
 }
function x_logger(r: Prequest_rec): Integer; cdecl;
var
  cfg: Px_cfg;
begin
  cfg := our_dconfig(r);
  trace_add(r^.server, r, cfg, 'x_logger()');
  Result := DECLINED;
end;

{--------------------------------------------------------------------------}
{                                                                          }
{ Which functions are responsible for which hooks in the server.           }
{                                                                          }
{--------------------------------------------------------------------------}
{
 * Each function our module provides to handle a particular hook is
 * specified here.  The functions are registered using
 * ap_hook_foo(name, predecessors, successors, position)
 * where foo is the name of the hook.
 *
 * The args are as follows:
 * name         ^. the name of the function to call.
 * predecessors ^. a list of modules whose calls to this hook must be
 *                 invoked before this module.
 * successors   ^. a list of modules whose calls to this hook must be
 *                 invoked after this module.
 * position     ^. The relative position of this module.  One of
 *                 APR_HOOK_FIRST, APR_HOOK_MIDDLE, or APR_HOOK_LAST.
 *                 Most modules will use APR_HOOK_MIDDLE.  If multiple
 *                 modules use the same relative position, Apache will
 *                 determine which to call first.
 *                 If your module relies on another module to run first,
 *                 or another module running after yours, use the
 *                 predecessors and/or successors.
 *
 * The number in brackets indicates the order in which the routine is called
 * during request processing.  Note that not all routines are necessarily
 * called (such as if a resource doesn't have access restrictions).
 * The actual delivery of content to the browser [9] is not handled by
 * a hook; see the handler declarations below.
 }
procedure x_register_hooks(p: Papr_pool_t); cdecl;
begin
    ap_hook_pre_config(@x_pre_config, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_post_config(@x_post_config, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_open_logs(@x_open_logs, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_child_init(@x_child_init, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_handler(@x_handler, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_quick_handler(@x_quick_handler, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_pre_connection(@x_pre_connection, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_process_connection(@x_process_connection, nil, nil, APR_HOOK_MIDDLE);
    { [1] post read_request handling }
    ap_hook_post_read_request(@x_post_read_request, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_log_transaction(@x_logger, nil, nil, APR_HOOK_MIDDLE);
{$ifdef 0}
    ap_hook_http_method(x_http_method, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_default_port(x_default_port, nil, nil, APR_HOOK_MIDDLE);
{$endif}
    ap_hook_translate_name(@x_translate_handler, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_header_parser(@x_header_parser_handler, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_check_user_id(@x_check_user_id, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_fixups(@x_fixer_upper, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_type_checker(@x_type_checker, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_access_checker(@x_access_checker, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_auth_checker(@x_auth_checker, nil, nil, APR_HOOK_MIDDLE);
    ap_hook_insert_filter(@x_insert_filter, nil, nil, APR_HOOK_MIDDLE);
end;

{--------------------------------------------------------------------------}
{                                                                          }
{ All of the routines have been declared now.  Here's the list of          }
{ directives specific to our module, and information about where they      }
{ may appear and how the command parser should pass them to us for         }
{ processing.  Note that care must be taken to ensure that there are NO    }
{ collisions of directive names between modules.                           }
{                                                                          }
{--------------------------------------------------------------------------}
var
  x_cmds: command_rec;

{--------------------------------------------------------------------------}
{                                                                          }
{ Finally, the list of callback routines and data structures that provide  }
{ the static hooks into our module from the other parts of the server.     }
{                                                                          }
{--------------------------------------------------------------------------}
{
 * Module definition for configuration.  If a particular callback is not
 * needed, replace its routine name below with the word NULL.
 }

begin
  default_module_ptr := @example_module;
  FillChar(default_module_ptr^, SizeOf(default_module_ptr^), 0);
  
  STANDARD20_MODULE_STUFF(default_module_ptr^);
  
  { List of directives specific to our module. }
  
  with x_cmds do
  begin
    name := 'Example';
    func := @cmd_example;
    cmd_data := nil;
    req_override := OR_OPTIONS;
    args_how := NO_ARGS; // Or RAW_ARGS ?
    errmsg := 'Example directive - no arguments';
  end;

  with example_module do
  begin
    name := MODULE_NAME;
    magic := MODULE_MAGIC_COOKIE;
    create_dir_config := @x_create_dir_config;      { per-directory config creator }
    merge_dir_config := @x_merge_dir_config;        { dir config merger }
    create_server_config := @x_create_server_config;{ server config creator }
    merge_server_config := @x_merge_server_config;  { server config merger }
    cmds := @x_cmds;                                { command table }
    register_hooks := @x_register_hooks;            { set up other request processing hooks }
  end;
end.

