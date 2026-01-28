{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

{**
 * \file include/conf.h
 * \brief Application interface library for the ALSA driver
 * \author Jaroslav Kysela <perex@perex.cz>
 * \author Abramo Bagnara <abramo@alsa-project.org>
 * \author Takashi Iwai <tiwai@suse.de>
 * \date 1998-2001
 *
 * Application interface library for the ALSA driver
 *}
{*
 *   This library is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU Lesser General Public License as
 *   published by the Free Software Foundation; either version 2.1 of
 *   the License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 *
 *}

//#ifndef __ALSA_CONF_H
//#define __ALSA_CONF_H

//#ifdef __cplusplus
//extern "C" {
//#endif

(**
 *  \defgroup Config Configuration Interface
 *  The configuration functions and types allow you to read, enumerate,
 *  modify and write the contents of ALSA configuration files.
 *  \{
 *)

//const
{ \brief \c dlsym version for the config evaluate callback. }
//#define SND_CONFIG_DLSYM_VERSION_EVALUATE	_dlsym_config_evaluate_001
{ \brief \c dlsym version for the config hook callback. }
//#define SND_CONFIG_DLSYM_VERSION_HOOK		_dlsym_config_hook_001

{ Configuration node type. }
type
  snd_config_type_t = (
    { Integer number. }
    SND_CONFIG_TYPE_INTEGER,
    { 64 bit integer number. }
    SND_CONFIG_TYPE_INTEGER64,
    { Real number. }
    SND_CONFIG_TYPE_REAL,
    { Character string. }
    SND_CONFIG_TYPE_STRING,
    { Pointer (runtime only, cannot be saved). }
    SND_CONFIG_TYPE_POINTER,
    { Compound node. }
    SND_CONFIG_TYPE_COMPOUND = 1024);

(**
 * \brief Internal structure for a configuration node object.
 *
 * The ALSA library uses a pointer to this structure as a handle to a
 * configuration node. Applications don't access its contents directly.
 *)
  PPsnd_config_t = ^Psnd_config_t;
  Psnd_config_t = ^snd_config_t;
  snd_config_t = record
  end;

(**
 * \brief type for a configuration compound iterator.
 *
 * The ALSA library uses this pointer type as a handle to a configuration
 * compound iterator. Applications don't directly access the contents of
 * the structure pointed to by this type.
 *)
  Psnd_config_iterator_t = ^snd_config_iterator_t;
  snd_config_iterator_t = ^_snd_config_iterator;
  _snd_config_iterator = record
  end;

(**
 * \brief Internal structure for a configuration private update object.
 *
 * The ALSA library uses this structure to save private update information.
 *)
  PPsnd_config_update_t = ^Psnd_config_update_t;
  Psnd_config_update_t = ^snd_config_update_t;
  snd_config_update_t = record
  end;

var
  snd_config: Psnd_config_t; external libasound;

function snd_config_top(config: PPsnd_config_t): cint; cdecl; external libasound;

function snd_config_load(config: Psnd_config_t; _in: Psnd_input_t): cint; cdecl; external libasound;
function snd_config_load_override(config: Psnd_config_t; _in: Psnd_input_t): cint; cdecl; external libasound;
function snd_config_save(config: Psnd_config_t; _out: Psnd_output_t): cint; cdecl; external libasound;
function snd_config_update: cint; cdecl; external libasound;
function snd_config_update_r(top: PPsnd_config_t; update: PPsnd_config_update_t; path: PChar): cint; cdecl; external libasound;
function snd_config_update_free(update: Psnd_config_update_t): cint; cdecl; external libasound;
function snd_config_update_free_global: cint; cdecl; external libasound;

function snd_config_search(config: Psnd_config_t; key: PChar;
		      result: PPsnd_config_t): cint; cdecl; external libasound;
function snd_config_searchv(config: Psnd_config_t;
		       result: PPsnd_config_t; dot_dot_dot: array of const): cint; cdecl; external libasound;
function snd_config_search_definition(config : Psnd_config_t;
				 base, key: PChar;
				 result: PPsnd_config_t) : cint; cdecl; external libasound;

function snd_config_expand(config, root: Psnd_config_t;
		      args: PChar; private_data: Psnd_config_t;
		      result: PPsnd_config_t): cint; cdecl; external libasound;
function snd_config_evaluate(config, root,
			private_data: Psnd_config_t; result: PPsnd_config_t): cint; cdecl; external libasound;

function snd_config_add(config, leaf: Psnd_config_t): cint; cdecl; external libasound;
function snd_config_delete(config: Psnd_config_t): cint; cdecl; external libasound;
function snd_config_delete_compound_members(config: Psnd_config_t): cint; cdecl; external libasound;
function snd_config_copy(dst: PPsnd_config_t; src: Psnd_config_t): cint; cdecl; external libasound;

function snd_config_make(config: PPsnd_config_t; key: PChar;
		    _type: snd_config_type_t): cint; cdecl; external libasound;
function snd_config_make_integer(config: PPsnd_config_t; key: PChar): cint; cdecl; external libasound;
function snd_config_make_integer64(config: PPsnd_config_t; key: PChar): cint; cdecl; external libasound;
function snd_config_make_real(config: PPsnd_config_t; key: PChar): cint; cdecl; external libasound;
function snd_config_make_string(config: PPsnd_config_t; key: PChar): cint; cdecl; external libasound;
function snd_config_make_pointer(config: PPsnd_config_t; key: PChar): cint; cdecl; external libasound;
function snd_config_make_compound(config: PPsnd_config_t; key: PChar; join: cint): cint; cdecl; external libasound;

function snd_config_imake_integer(config: PPsnd_config_t; key: PChar; value: clong): cint; cdecl; external libasound;
function snd_config_imake_integer64(config: PPsnd_config_t; key: PChar; value: clonglong): cint; cdecl; external libasound;
function snd_config_imake_real(config: PPsnd_config_t; key: PChar; value: cdouble): cint; cdecl; external libasound;
function snd_config_imake_string(config: PPsnd_config_t; key: PChar; ascii: PChar): cint; cdecl; external libasound;
function snd_config_imake_safe_string(config: PPsnd_config_t; key: PChar; ascii: PChar): cint; cdecl; external libasound;
function snd_config_imake_pointer(config: PPsnd_config_t; key: PChar; ptr: Pointer): cint; cdecl; external libasound;

function snd_config_get_type(config: Psnd_config_t): snd_config_type_t; cdecl; external libasound;

function snd_config_set_id(config: Psnd_config_t; id: PChar): cint; cdecl; external libasound;
function snd_config_set_integer(config: Psnd_config_t; value: clong): cint; cdecl; external libasound;
function snd_config_set_integer64(config: Psnd_config_t; value: clonglong): cint; cdecl; external libasound;
function snd_config_set_real(config: Psnd_config_t; value: cdouble): cint; cdecl; external libasound;
function snd_config_set_string(config: Psnd_config_t; value: PChar): cint; cdecl; external libasound;
function snd_config_set_ascii(config: Psnd_config_t; ascii: PChar): cint; cdecl; external libasound;
function snd_config_set_pointer(config: Psnd_config_t; ptr: Pointer): cint; cdecl; external libasound;
function snd_config_get_id(config: Psnd_config_t; value: PPChar): cint; cdecl; external libasound;
function snd_config_get_integer(config: Psnd_config_t; value: Pclong): cint; cdecl; external libasound;
function snd_config_get_integer64(config: Psnd_config_t; value: Pclonglong): cint; cdecl; external libasound;
function snd_config_get_real(config: Psnd_config_t; value: Pcdouble): cint; cdecl; external libasound;
function snd_config_get_ireal(config: Psnd_config_t; value: Pcdouble): cint; cdecl; external libasound;
function snd_config_get_string(config: Psnd_config_t; value: PPChar): cint; cdecl; external libasound;
function snd_config_get_ascii(config: Psnd_config_t; value: PPChar): cint; cdecl; external libasound;
function snd_config_get_pointer(config: Psnd_config_t; value: PPointer): cint; cdecl; external libasound;
function snd_config_test_id(config: Psnd_config_t; id: PChar): cint; cdecl; external libasound;

function snd_config_iterator_first(node: Psnd_config_t): snd_config_iterator_t; cdecl; external libasound;
function snd_config_iterator_next(iterator: snd_config_iterator_t): snd_config_iterator_t; cdecl; external libasound;
function snd_config_iterator_end(node: Psnd_config_t): snd_config_iterator_t; cdecl; external libasound;
function snd_config_iterator_entry(iterator: snd_config_iterator_t): Psnd_config_t; cdecl; external libasound;

(**
 * \brief Helper macro to iterate over the children of a compound node.
 * \param[in,out] pos Iterator variable for the current node.
 * \param[in,out] next Temporary iterator variable for the next node.
 * \param[in] node Handle to the compound configuration node to iterate over.
 *
 * Use this macro like a \c for statement, e.g.:
 * \code
 * snd_config_iterator_t pos, next;
 * snd_config_for_each(pos, next, node) {
 *     snd_config_t *entry = snd_config_iterator_entry(pos);
 *     ...
 * }
 * \endcode
 *
 * This macro allows deleting or removing the current node.
 *)
//#define snd_config_for_each(pos, next, node) \
//	for (pos = snd_config_iterator_first(node), next = snd_config_iterator_next(pos); pos != snd_config_iterator_end(node); pos = next, next = snd_config_iterator_next(pos))

{ Misc functions }

function snd_config_get_bool_ascii(ascii: PChar): cint; cdecl; external libasound;
function snd_config_get_bool(conf: Psnd_config_t): cint; cdecl; external libasound;
function snd_config_get_ctl_iface_ascii(ascii: PChar): cint; cdecl; external libasound;
function snd_config_get_ctl_iface(conf: Psnd_config_t): cint; cdecl; external libasound;

{ Names functions }

(**
 * Device-name list element
 *)
type
  PPsnd_devname_t = ^Psnd_devname_t;
  Psnd_devname_t = ^snd_devname_t;
  snd_devname_t = record
    name : PChar;           { Device name string }
    comment : PChar;        { Comments }
    next : Psnd_devname_t;  { Next pointer }
  end;

function snd_names_list(iface: PChar; list: PPsnd_devname_t): cint; cdecl; external libasound;
procedure snd_names_list_free(list: Psnd_devname_t); cdecl; external libasound;

///** \} */

//#ifdef __cplusplus
//}
//#endif

//#endif /* __ALSA_CONF_H */
