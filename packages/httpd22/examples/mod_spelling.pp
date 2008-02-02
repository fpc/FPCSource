{* Copyright 1999-2005 The Apache Software Foundation or its licensors, as
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
 *}
library mod_spelling;

{$i define.inc}

uses SysUtils, Classes, httpd, apr;

var
  speling_module: module; {$ifdef Unix} cvar; public; {$endif}
  default_module_ptr: Pmodule;

const
  MODULE_NAME = 'mod_speling.so';

{*******************************************************************
*  Free Pascal only supports exporting variables on Windows
*******************************************************************}
{$ifdef WINDOWS}
exports
  speling_module name 'spelling_module';
{$endif}

{#include "apr.h"
#include "apr_file_io.h"
#include "apr_strings.h"
#include "apr_lib.h"

#define APR_WANT_STRFUNC
#include "apr_want.h"

#define WANT_BASENAME_MATCH

#include "httpd.h"
#include "http_core.h"
#include "http_config.h"
#include "http_request.h"
#include "http_log.h" }

{* mod_speling.c - by Alexei Kosut <akosut@organic.com> June, 1996
 *
 * Translated to pascal by Felipe Monteiro de Carvalho - July, 2006
 *
 * This module is transparent, and simple. It attempts to correct
 * misspellings of URLs that users might have entered, namely by checking
 * capitalizations. If it finds a match, it sends a redirect.
 *
 * 08-Aug-1997 <Martin.Kraemer@Mch.SNI.De>
 * o Upgraded module interface to apache_1.3a2-dev API (more NULL's in
 *   speling_module).
 * o Integrated tcsh's "spelling correction" routine which allows one
 *   misspelling (character insertion/omission/typo/transposition).
 *   Rewrote it to ignore case as well. This ought to catch the majority
 *   of misspelled requests.
 * o Commented out the second pass where files' suffixes are stripped.
 *   Given the better hit rate of the first pass, this rather ugly
 *   (request index.html, receive index.db ?!?!) solution can be
 *   omitted.
 * o wrote a "kind of" html page for mod_speling
 *
 * Activate it with "CheckSpelling On"
 }

type
  spconfig = record
    enabled: Integer;
  end;
  
  Pspconfig = ^spconfig;

{
 * Create a configuration specific to this module for a server or directory
 * location, and fill it with the default settings.
 *
 * The API says that in the absence of a merge function, the record for the
 * closest ancestor is used exclusively.  That's what we want, so we don't
 * bother to have such a function.
 }

function mkconfig(p: Papr_pool_t): Pointer;
var
  cfg: Pspconfig;
begin
  cfg := apr_pcalloc(p, sizeof(spconfig));
  cfg^.enabled := 0;
  Result := cfg;
end;

{
 * Respond to a callback to create configuration record for a server or
 * vhost environment.
 }
function create_mconfig_for_server(p: Papr_pool_t; s: Pserver_rec): Pointer; cdecl;
begin
  Result := mkconfig(p);
end;

{
 * Respond to a callback to create a config record for a specific directory.
 }
function create_mconfig_for_directory(p: Papr_pool_t; dir: PChar): Pointer; cdecl;
begin
  Result := mkconfig(p);
end;

{
 * Handler for the CheckSpelling directive, which is FLAG.
 }
function set_speling(cmd: Pcmd_parms; mconfig: Pointer; arg: Integer): PChar; cdecl;
var
  cfg: Pspconfig;
begin
  cfg := Pspconfig(mconfig);
  cfg^.enabled := arg;
  Result := nil;
end;

  {* Define the directives specific to this module.  This structure is referenced
   * later by the 'module' structure. }

var
  speling_cmds: command_rec;

type
  sp_reason = (
    SP_IDENTICAL = 0,
    SP_MISCAPITALIZED = 1,
    SP_TRANSPOSITION = 2,
    SP_MISSINGCHAR = 3,
    SP_EXTRACHAR = 4,
    SP_SIMPLETYPO = 5,
    SP_VERYDIFFERENT = 6
  );

const
  sp_reason_str: array [0..7] of PChar =
  (
    'identical',
    'miscapitalized',
    'transposed characters',
    'character missing',
    'extra character',
    'mistyped character',
    'common basename',
    nil
  );

type
  misspelled_file = record
    name: PChar;
    quality: sp_reason;
  end;
  
  Pmisspelled_file = ^misspelled_file;

{
 * spdist() is taken from Kernighan & Pike,
 *  _The_UNIX_Programming_Environment_
 * and adapted somewhat to correspond better to psychological reality.
 * (Note the changes to the return values)
 *
 * According to Pollock and Zamora, CACM April 1984 (V. 27, No. 4),
 * page 363, the correct order for this is:
 * OMISSION = TRANSPOSITION > INSERTION > SUBSTITUTION
 * thus, it was exactly backwards in the old version. -- PWP
 *
 * This routine was taken out of tcsh's spelling correction code
 * (tcsh-6.07.04) and re-converted to apache data types ("char" type
 * instead of tcsh's NLS'ed "Char"). Plus it now ignores the case
 * during comparisons, so is a "approximate strcasecmp()".
 * NOTE that is still allows only _one_ real "typo",
 * it does NOT try to correct multiple errors.
 }
{
  Extra notes about how this function works:
  
  * s and t are supposed different
  
  * s
}
function spdist(const cs, ct: PChar): sp_reason;
var
  s, t, i, j: PChar;
begin
  s := cs;
  t := ct;

  while apr_tolower(s^) = apr_tolower(t^) do
  begin
    if t^ = #0 then
    begin
      Result := SP_MISCAPITALIZED;   { exact match (sans case) }
      Exit;
    end;

    Inc(s);
    Inc(t);
  end;

  if s^ <> #0 then
  begin
    if t^ <> #0 then
    begin
      i := s;
      Inc(i);
      j := t;
      Inc(j);
    
      if Integer(i^) and Integer(j^) <> 0 then
       if (apr_tolower(s^) = apr_tolower(j^)) and (apr_tolower(t^) = apr_tolower(i^)) then
       begin
         Inc(i);
         Inc(j);
        
         if stricomp(i, j) = 0 then
         begin
           Result := SP_TRANSPOSITION;        { transposition }
           Exit;
         end;
       end;
      
      Dec(i);
      Dec(j);
      
      if (stricomp(i, j) = 0) then
      begin
        Result := SP_SIMPLETYPO;   { 1 char mismatch }
        Exit;
      end;
    end;
    
    
    if (stricomp(i, t) = 0) then
    begin
      Result := SP_EXTRACHAR;        { extra character }
      Exit;
    end;
  end;

  if (t^ <> #0) and (stricomp(s, t + 1) = 0) then
  begin
    Result := SP_MISSINGCHAR;  { missing character }
    Exit;
  end;

  Result := SP_VERYDIFFERENT;    { distance too large to fix. }
end;

function sort_by_quality(left, rite: Pointer): Integer;
begin
  Result := Integer(Pmisspelled_file(left)^.quality) - Integer(Pmisspelled_file(rite)^.quality);
end;

function check_speling(r: Prequest_rec): Integer; cdecl;
var
  cfg: Pspconfig;
  good, bad, postgood, url: PChar;
  dirent: apr_finfo_t;
  filoc, dotloc, urlen, pglen: Integer;
  candidates: Papr_array_header_t = nil;
  dir: Papr_dir_t;
  q: sp_reason;
  sp_new, variant_, nvariant_: Pmisspelled_file;
  nuri, ref, vuri, reason: PChar;
  i, entloc: Integer;
  p, sub_pool: Papr_pool_t;
  notes: Papr_table_t;
  t, v: Papr_array_header_t;
  List: TList;
  plist: Pointer;
begin
  cfg := Pspconfig(ap_get_module_config(r^.per_dir_config, @speling_module));
  if (cfg^ .enabled = 0) then
  begin
    Result := DECLINED;
    Exit;
  end;

  { We only want to worry about GETs }
  if (r^.method_number <> M_GET) then
  begin
    Result := DECLINED;
    Exit;
  end;

  { We've already got a file of some kind or another }
  if (Integer(r^.finfo.filetype) <> 0) then
  begin
    Result := DECLINED;
    Exit;
  end;

  { Not a file request }
  if (r^.proxyreq>0) or not assigned(r^.filename) then
  begin
    Result := DECLINED;
    Exit;
  end;

  { This is a sub request - don't mess with it }
  if (r^.main <> nil) then
  begin
    Result := DECLINED;
    Exit;
  end;

  {
   * The request should end up looking like this:
   * r->uri: /correct-url/mispelling/more
   * r->filename: /correct-file/mispelling r->path_info: /more
   *
   * So we do this in steps. First break r->filename into two pieces
   }

  filoc := ap_rind(r^.filename, '/');
  {
   * Don't do anything if the request doesn't contain a slash, or
   * requests "/"
   }
  if (filoc = -1) or (strcomp(r^.uri, '/') = 0) then
  begin
    Result := DECLINED;
    Exit;
  end;

  { good = /correct-file }
  good := apr_pstrndup(r^.pool, r^.filename, filoc);
  { bad = mispelling }
  bad := apr_pstrdup(r^.pool, r^.filename + filoc + 1);
  { postgood = mispelling/more }
  postgood := apr_pstrcat(r^.pool, [bad, r^.path_info, nil]);

  urlen := strlen(r^.uri);
  pglen := strlen(postgood);

  { Check to see if the URL pieces add up }
  if (strcomp(postgood, r^.uri + (urlen - pglen))) <> 0 then
  begin
    Result := DECLINED;
    Exit;
  end;

  { url = /correct-url }
  url := apr_pstrndup(r^.pool, r^.uri, (urlen - pglen));

  { Now open the directory and do ourselves a check... }
  if (apr_dir_open(@dir, good, r^.pool) <> APR_SUCCESS) then
      { Oops, not a directory... }
  begin
    Result := DECLINED;
    Exit;
  end;

  candidates := apr_array_make(r^.pool, 2, sizeof(misspelled_file));

  dotloc := ap_ind(bad, '.');

  if (dotloc = -1) then dotloc := strlen(bad);

  while (apr_dir_read(@dirent, APR_FINFO_DIRENT, dir) = APR_SUCCESS) do
  begin
    {
     * If we end up with a "fixed" URL which is identical to the
     * requested one, we must have found a broken symlink or some such.
     * Do _not_ try to redirect this, it causes a loop!
     }
    if (strcomp(bad, dirent.name) = 0) then
    begin
      apr_dir_close(dir);
      Result := OK;
    end

    {
     * miscapitalization errors are checked first (like, e.g., lower case
     * file, upper case request)
     }
    else if (stricomp(bad, dirent.name) = 0) then
    begin
      sp_new := Pmisspelled_file(apr_array_push(candidates));
      sp_new^.name := apr_pstrdup(r^.pool, dirent.name);
      sp_new^.quality := SP_MISCAPITALIZED;
    end

    {
     * simple typing errors are checked next (like, e.g.,
     * missing/extra/transposed char)
     }
    else if (spdist(bad, dirent.name) <> SP_VERYDIFFERENT) then
    begin
      q := spdist(bad, dirent.name);
        
      sp_new := Pmisspelled_file(apr_array_push(candidates));
      sp_new^.name := apr_pstrdup(r^.pool, dirent.name);
      sp_new^.quality := q;
    end

    {
     * The spdist() should have found the majority of the misspelled
     * requests.  It is of questionable use to continue looking for
     * files with the same base name, but potentially of totally wrong
     * type (index.html <-> index.db).
     * I would propose to not set the WANT_BASENAME_MATCH define.
     *      08-Aug-1997 <Martin.Kraemer@Mch.SNI.De>
     *
     * However, Alexei replied giving some reasons to add it anyway:
     * > Oh, by the way, I remembered why having the
     * > extension-stripping-and-matching stuff is a good idea:
     * >
     * > If you're using MultiViews, and have a file named foobar.html,
     * > which you refer to as "foobar", and someone tried to access
     * > "Foobar", mod_speling won't find it, because it won't find
     * > anything matching that spelling. With the extension-munging,
     * > it would locate "foobar.html". Not perfect, but I ran into
     * > that problem when I first wrote the module.
     }
    else
    begin
{$ifdef WANT_BASENAME_MATCH}
      {
       * Okay... we didn't find anything. Now we take out the hard-core
       * power tools. There are several cases here. Someone might have
       * entered a wrong extension (.htm instead of .html or vice
       * versa) or the document could be negotiated. At any rate, now
       * we just compare stuff before the first dot. If it matches, we
       * figure we got us a match. This can result in wrong things if
       * there are files of different content types but the same prefix
       * (e.g. foo.gif and foo.html) This code will pick the first one
       * it finds. Better than a Not Found, though.
       }
      entloc := ap_ind(dirent.name, '.');
      if (entloc = -1) then entloc := strlen(dirent.name);

      if ((dotloc = entloc) and not strncasecmp(bad, dirent.name, dotloc)) then
      begin
	sp_new := Pmisspelled_file(apr_array_push(candidates));
        sp_new^.name := apr_pstrdup(r^.pool, dirent.name);
        sp_new^.quality := SP_VERYDIFFERENT;
      end;
{$endif}
    end;
  end;
    
  apr_dir_close(dir);

  if (candidates^.nelts <> 0) then
  begin
    { Wow... we found us a mispelling. Construct a fixed url }
    variant_ := Pmisspelled_file(candidates^.elts);

    ref := apr_table_get(r^.headers_in, 'Referer');

    List := TList.Create;
    
    try
      for i := 0 to candidates^.nelts - 1 do
      begin
        plist := Pointer(candidates^.elts);
        Inc(plist, sizeof(misspelled_file));
        List.Add(plist);
      end;

      List.Sort(@sort_by_quality);
    finally
      List.Free;
    end;

    {
     * Conditions for immediate redirection:
     *     a) the first candidate was not found by stripping the suffix
     * AND b) there exists only one candidate OR the best match is not
     *         ambiguous
     * then return a redirection right away.
     }
    nvariant_ := variant_;
    Inc(nvariant_, sizeof(misspelled_file));
     
    if (variant_^.quality <> SP_VERYDIFFERENT) and ( (candidates^.nelts = 1)
     or (Integer(variant_^.quality) <> Integer(nvariant_^.quality))) then
    begin
      nuri := ap_escape_uri(r^.pool, apr_pstrcat(r^.pool, [url,
						     variant_^.name,
						     r^.path_info, nil]));
      if (r^.parsed_uri.query^ <> #0) then
       nuri := apr_pstrcat(r^.pool, [nuri, PChar('?'), r^.parsed_uri.query, nil]);

      apr_table_setn(r^.headers_out, 'Location',
			  ap_construct_url(r^.pool, nuri, r));

      if ref^ <> #0 then
       ap_log_rerror(MODULE_NAME, 506, APLOG_INFO, APR_SUCCESS,
        r, 'Fixed spelling: %s to %s from %s',  [r^.uri, nuri, ref])
      else
       ap_log_rerror(MODULE_NAME, 506, APLOG_INFO, APR_SUCCESS,
        r, 'Fixed spelling: %s to %s',  [r^.uri, nuri, ref]);

      Result := HTTP_MOVED_PERMANENTLY;
      Exit;
    end
    {
     * Otherwise, a "[300] Multiple Choices" list with the variants is
     * returned.
     }
    else
    begin
      if (r^.main = nil) then
      begin
        p := r^.pool;
        notes := r^.notes;
      end
      else
      begin
        p := r^.main^.pool;
        notes := r^.main^.notes;
      end;

      if (apr_pool_create(@sub_pool, p) <> APR_SUCCESS) then
      begin
        Result := DECLINED;
        Exit;
      end;
          
      t := apr_array_make(sub_pool, candidates^.nelts * 8 + 8, sizeof(PChar));
      v := apr_array_make(sub_pool, candidates^.nelts * 5, sizeof(PChar));

       { Generate the response text. }

      PPChar(apr_array_push(t))^ := 'The document name you requested (<code>';
      PPChar(apr_array_push(t))^ := ap_escape_html(sub_pool, r^.uri);
      PPChar(apr_array_push(t))^ :=
		   '</code>) could not be found on this server.' + LineEnding +
		   'However, we found documents with names similar ' +
		   'to the one you requested.<p>' +
		   'Available documents:' + LineEnding + '<ul>' + LineEnding;

            for i := 0 to candidates^.nelts -1 do
            begin
		reason := sp_reason_str[Integer(variant_[i].quality)];
                { The format isn't very neat... }
                if r^.parsed_uri.query <> nil then
                 vuri := apr_pstrcat(sub_pool, [url, variant_[i].name, r^.path_info,
                  '?', r^.parsed_uri.query, nil])
                else vuri := apr_pstrcat(sub_pool, [url, variant_[i].name, r^.path_info,
		 PChar(''), PChar(''), nil]);
   
		PPChar(apr_array_push(v))^ := '"';
		PPChar(apr_array_push(v))^ := ap_escape_uri(sub_pool, vuri);
		PPChar(apr_array_push(v))^ := '";"';
		PPChar(apr_array_push(v))^ := reason;
		PPChar(apr_array_push(v))^ := '"';

		PPChar(apr_array_push(t))^ := '<li><a href="';
		PPChar(apr_array_push(t))^ := ap_escape_uri(sub_pool, vuri);
		PPChar(apr_array_push(t))^ := '">';
		PPChar(apr_array_push(t))^ := ap_escape_html(sub_pool, vuri);
		PPChar(apr_array_push(t))^ := '</a> (';
		PPChar(apr_array_push(t))^ := reason;
		PPChar(apr_array_push(t))^ := ')' + LineEnding;

                {
                 * when we have printed the "close matches" and there are
                 * more "distant matches" (matched by stripping the suffix),
                 * then we insert an additional separator text to suggest
                 * that the user LOOK CLOSELY whether these are really the
                 * files she wanted.
                 }
                if (i > 0) and (i < candidates^.nelts - 1)
                    and (variant_[i].quality <> SP_VERYDIFFERENT)
                    and (variant_[i + 1].quality = SP_VERYDIFFERENT) then
                 PPChar(apr_array_push(t))^ :=
		  '</ul>' + LineEnding + 'Furthermore, the following related ' +
                  'documents were found:' + LineEnding + '<ul>' + LineEnding;
            end;
            
	    PPChar(apr_array_push(t))^ := '</ul>' + LineEnding;

            { If we know there was a referring page, add a note: }
            if (ref <> nil) then
            begin
              PPChar(apr_array_push(t))^ :=
	       'Please consider informing the owner of the <a href="';
	      PPChar(apr_array_push(t))^ := ap_escape_uri(sub_pool, ref);
              PPChar(apr_array_push(t))^ := '">referring page</a> about the broken link.' + LineEnding;
            end;

            { Pass our apr_table_t to http_protocol.c (see mod_negotiation): }
            apr_table_setn(notes, 'variant-list', apr_array_pstrcat(p, t, #0));

	    apr_table_mergen(r^.subprocess_env, 'VARIANTS', apr_array_pstrcat(p, v, ','));

	    apr_pool_destroy(sub_pool);

            if ref <> '' then
             ap_log_rerror(MODULE_NAME, 609, APLOG_INFO, 0, r,
	      'Spelling fix: %s: %d candidates from %s', [r^.uri, candidates^.nelts, ref])
            else ap_log_rerror(MODULE_NAME, 609, APLOG_INFO, 0, r,
	      'Spelling fix: %s: %d candidates', [r^.uri, candidates^.nelts, ref]);

            Result := HTTP_MULTIPLE_CHOICES;
        end;
    end;

    Result := OK;
end;

procedure register_hooks_(p: Papr_pool_t); cdecl;
begin
  ap_hook_fixups(@check_speling, nil, nil, APR_HOOK_LAST);
end;

begin
  default_module_ptr := @speling_module;
  FillChar(default_module_ptr^, SizeOf(default_module_ptr^), 0);

  STANDARD20_MODULE_STUFF(default_module_ptr^);

  {* Define the directives specific to this module.  This structure is referenced
   * later by the 'module' structure. }
   
  with speling_cmds do
  begin
    name := 'CheckSpelling';
    func := cmd_func(@set_speling);
    cmd_data := nil;
    req_override := OR_OPTIONS;
    args_how := FLAG;
    errmsg := 'whether or not to fix miscapitalized/misspelled requests';
  end;

  with speling_module do
  begin
    name := MODULE_NAME;
    magic := MODULE_MAGIC_COOKIE;
    create_dir_config := @create_mconfig_for_directory;    { per-directory config creator }
    merge_dir_config := nil;     { dir config merger }
    create_server_config := @create_mconfig_for_server; { server config creator }
    merge_server_config := nil;  { server config merger }
    cmds := @speling_cmds;                 { command table }
    register_hooks := @register_hooks_; { set up other request processing hooks }
  end;
end.

