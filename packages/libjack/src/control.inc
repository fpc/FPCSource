(* -*- Mode: C ; c-basic-offset: 4 -*- *)
(*
  JACK control API

  Copyright (C) 2008 Nedko Arnaudov
  Copyright (C) 2008 GRAME

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*)
(**
 * @file   jack/control.h
 * @ingroup publicheader
 * @brief  JACK control API
 *
 *)

{$ifndef JACKCTL_H__2EEDAD78_DF4C_4B26_83B7_4FF1A446A47E__INCLUDED}
{$define JACKCTL_H__2EEDAD78_DF4C_4B26_83B7_4FF1A446A47E__INCLUDED}

//#include <jack/types.h>
//#include <jack/jslist.h>
//#include <jack/systemdeps.h>
{$I systemdeps.inc}
//#if !defined(sun) && !defined(__sun__)
//#include <stdbool.h>
//#endif

(** Parameter types, intentionally similar to jack_driver_param_type_t *)
type
  PPjackctl_param_type_t = ^Pjackctl_param_type_t;
  Pjackctl_param_type_t = ^jackctl_param_type_t;
  jackctl_param_type_t = (
    JackParamInt = 1,			(**< @brief value type is a signed integer *)
    JackParamUInt,				(**< @brief value type is an unsigned integer *)
    JackParamChar,				(**< @brief value type is a char *)
    JackParamString,			(**< @brief value type is a string with max size of ::JACK_PARAM_STRING_MAX+1 chars *)
    JackParamBool				(**< @brief value type is a boolean *)
  );

(** Driver types *)
  PPjackctl_driver_type_t = ^Pjackctl_driver_type_t;
  Pjackctl_driver_type_t = ^jackctl_driver_type_t;
  jackctl_driver_type_t = (
    JackMaster = 1,         (**< @brief master driver *)
    JackSlave               (**< @brief slave driver *)
  );

(** @brief Max value that jackctl_param_type_t type can have *)
const
  JACK_PARAM_MAX = (Ord(JackParamBool) + 1);

(** @brief Max length of string parameter value, excluding terminating null char *)
  JACK_PARAM_STRING_MAX = 127;

(** @brief Type for parameter value *)
(* intentionally similar to jack_driver_param_value_t *)
type
  PPjackctl_parameter_value = ^Pjackctl_parameter_value;
  Pjackctl_parameter_value = ^jackctl_parameter_value;
  jackctl_parameter_value = record
    case Integer of
      0: (ui: uint32_t); (**< @brief member used for ::JackParamUInt *)
      1: (i: int32_t);					(**< @brief member used for ::JackParamInt *)
      2: (c: cchar);						(**< @brief member used for ::JackParamChar *)
      3: (str: array [0..JACK_PARAM_STRING_MAX] of char); (**< @brief member used for ::JackParamString *)
      4: (b: cbool);				(**< @brief member used for ::JackParamBool *)
  end;

(** opaque type for server object *)
  PPjackctl_server_t = ^Pjackctl_server_t;
  Pjackctl_server_t = ^jackctl_server_t;
  jackctl_server_t = record end;

(** opaque type for driver object *)
  PPjackctl_driver_t = ^Pjackctl_driver_t;
  Pjackctl_driver_t = ^jackctl_driver_t;
  jackctl_driver_t = record end;

(** opaque type for internal client object *)
  PPjackctl_internal_t = ^Pjackctl_internal_t;
  Pjackctl_internal_t = ^jackctl_internal_t;
  jackctl_internal_t = record end;

(** opaque type for parameter object *)
  PPjackctl_parameter_t = ^Pjackctl_parameter_t;
  Pjackctl_parameter_t = ^jackctl_parameter_t;
  jackctl_parameter_t = record end;

(** opaque type for sigmask object *)
  PPjackctl_sigmask_t = ^Pjackctl_sigmask_t;
  Pjackctl_sigmask_t = ^jackctl_sigmask_t;
  jackctl_sigmask_t = record end;

//#ifdef __cplusplus
//extern "C" {
//#endif
//#if 0
//} /* Adjust editor indent */
//#endif

(**
 * @defgroup ControlAPI The API for starting and controlling a JACK server
 * @{
 *)

(**
 * Call this function to setup process signal handling. As a general
 * rule, it is required for proper operation for the server object.
 *
 * @param flags signals setup flags, use 0 for none. Currently no
 * flags are defined
 *
 * @return the configurated signal set.
 *)

function jackctl_setup_signals(
    flags: cuint): Pjackctl_sigmask_t; cdecl; external libjackserver;

(**
 * Call this function to wait on a signal set.
 *
 * @param signals signals set to wait on
 *)
procedure
jackctl_wait_signals(
    signals: Pjackctl_sigmask_t); cdecl; external libjackserver;

(**
 * \bold THIS FUNCTION IS DEPRECATED AND SHOULD NOT BE USED IN
 *  NEW JACK PROJECTS
 *
 * @deprecated Please use jackctl_server_create2().
 *)

type
  TOnDeviceAcquireProc = function(const device_name: PChar): cbool; cdecl;
  TOnDeviceReleaseProc = procedure(const device_name: PChar); cdecl;

function jackctl_server_create(
    on_device_acquire: TOnDeviceAcquireProc;
    on_device_release: TOnDeviceReleaseProc): Pjackctl_server_t; cdecl; external libjackserver;

(**
 * Call this function to create server object.
 *
 * @param on_device_acquire - Optional callback to be called before device is acquired. If false is returned, device usage will fail
 * @param on_device_release - Optional callback to be called after device is released.
 * @param on_device_reservation_loop - Optional callback to be called when looping/idling the reservation.
 *
 * @return server object handle, NULL if creation of server object
 * failed. Successfully created server object must be destroyed with
 * paired call to ::jackctl_server_destroy
 *)
type
  TOnDeviceReservationLoopProc = procedure; cdecl;

function
jackctl_server_create2(
    on_device_acquire: TOnDeviceAcquireProc;
    on_device_release: TOnDeviceReleaseProc;
    on_device_reservation_loop: TOnDeviceReservationLoopProc): Pjackctl_server_t; cdecl; external libjackserver;

(**
 * Call this function to destroy server object.
 *
 * @param server server object handle to destroy
 *)
procedure
jackctl_server_destroy(
	server: Pjackctl_server_t); cdecl; external libjackserver;

(**
 * Call this function to open JACK server
 *
 * @param server server object handle
 * @param driver driver to use
 *
 * @return success status: true - success, false - fail
 *)
function
jackctl_server_open(
    server: Pjackctl_server_t;
    driver: Pjackctl_driver_t): cbool; cdecl; external libjackserver;

(**
 * Call this function to start JACK server
 *
 * @param server server object handle
 *
 * @return success status: true - success, false - fail
 *)
function
jackctl_server_start(
    server: Pjackctl_server_t): cbool; cdecl; external libjackserver;

(**
 * Call this function to stop JACK server
 *
 * @param server server object handle
 *
 * @return success status: true - success, false - fail
 *)
function
jackctl_server_stop(
	server: Pjackctl_server_t): cbool; cdecl; external libjackserver;

(**
 * Call this function to close JACK server
 *
 * @param server server object handle
 *
 * @return success status: true - success, false - fail
 *)
function
jackctl_server_close(
	server: Pjackctl_server_t): cbool; cdecl; external libjackserver;

(**
 * Call this function to get list of available drivers. List node data
 * pointers is a driver object handle (::jackctl_driver_t).
 *
 * @param server server object handle to get drivers for
 *
 * @return Single linked list of driver object handles. Must not be
 * modified. Always same for same server object.
 *)
function
jackctl_server_get_drivers_list(
	server: Pjackctl_server_t): PJSList; cdecl; external libjackserver;

(**
 * Call this function to get list of server parameters. List node data
 * pointers is a parameter object handle (::jackctl_parameter_t).
 *
 * @param server server object handle to get parameters for
 *
 * @return Single linked list of parameter object handles. Must not be
 * modified. Always same for same server object.
 *)
function
jackctl_server_get_parameters(
	server: Pjackctl_server_t): PJSList; cdecl; external libjackserver;

(**
 * Call this function to get list of available internal clients. List node data
 * pointers is a internal client object handle (::jackctl_internal_t).
 *
 * @param server server object handle to get internal clients for
 *
 * @return Single linked list of internal client object handles. Must not be
 * modified. Always same for same server object.
 *)
function
jackctl_server_get_internals_list(
	server: Pjackctl_server_t): PJSList; cdecl; external libjackserver;

(**
 * Call this function to load one internal client.
 * (can be used when the server is running)
 *
 * @param server server object handle
 * @param internal internal to use
 *
 * @return success status: true - success, false - fail
 *)
function
jackctl_server_load_internal(
    server: Pjackctl_server_t;
    internal: Pjackctl_internal_t): cbool; cdecl; external libjackserver;

(**
 * Call this function to unload one internal client.
 * (can be used when the server is running)
 *
 * @param server server object handle
 * @param internal internal to unload
 *
 * @return success status: true - success, false - fail
 *)
function
jackctl_server_unload_internal(
    server: Pjackctl_server_t;
    internal: Pjackctl_internal_t): cbool; cdecl; external libjackserver;

(**
 * Call this function to load a session file.
 * (can be used when the server is running)
 *
 * @param server server object handle
 * @param file the session file to load, containing a list of
 * internal clients and connections to be made.
 *
 * @return success status: true - success, false - fail
 *)
function jackctl_server_load_session_file(
    server_ptr: Pjackctl_server_t;
    const _file: PChar): cbool; cdecl; external libjackserver;

(**
 * Call this function to add a slave in the driver slave list.
 * (cannot be used when the server is running that is between
 * jackctl_server_start and jackctl_server_stop)
 *
 * @param server server object handle
 * @param driver driver to add in the driver slave list.
 *
 * @return success status: true - success, false - fail
 *)
function
jackctl_server_add_slave(server: Pjackctl_server_t;
                         driver: Pjackctl_driver_t): cbool; cdecl; external libjackserver;

(**
 * Call this function to remove a slave from the driver slave list.
 * (cannot be used when the server is running that is between
 * jackctl_server_start and jackctl_server_stop)
 *
 * @param server server object handle
 * @param driver driver to remove from the driver slave list.
 *
 * @return success status: true - success, false - fail
 *)
function
jackctl_server_remove_slave(server: Pjackctl_server_t;
                            driver: Pjackctl_driver_t): cbool; cdecl; external libjackserver;

(**
 * Call this function to switch master driver.
 *
 * @param server server object handle
 * @param driver driver to switch to
 *
 * @return success status: true - success, false - fail
 *)
function
jackctl_server_switch_master(server: Pjackctl_server_t;
                             driver: Pjackctl_driver_t): cbool; cdecl; external libjackserver;


(**
 * Call this function to get name of driver.
 *
 * @param driver driver object handle to get name of
 *
 * @return driver name. Must not be modified. Always same for same
 * driver object.
 *)
function
jackctl_driver_get_name(
	driver: Pjackctl_driver_t): PChar; cdecl; external libjackserver;

(**
 * Call this function to get type of driver.
 *
 * @param driver driver object handle to get name of
 *
 * @return driver type. Must not be modified. Always same for same
 * driver object.
 *)
function
jackctl_driver_get_type(
	driver: Pjackctl_driver_t): jackctl_driver_type_t; cdecl; external libjackserver;

(**
 * Call this function to get list of driver parameters. List node data
 * pointers is a parameter object handle (::jackctl_parameter_t).
 *
 * @param driver driver object handle to get parameters for
 *
 * @return Single linked list of parameter object handles. Must not be
 * modified. Always same for same driver object.
 *)
function
jackctl_driver_get_parameters(
	driver: Pjackctl_driver_t): PJSList; cdecl; external libjackserver;

(**
 * Call this function to parse parameters for a driver.
 *
 * @param driver driver object handle
 * @param argc parameter list len
 * @param argv parameter list, as an array of char*
 *
 * @return success status: true - success, false - fail
 *)
function
jackctl_driver_params_parse(
    driver: Pjackctl_driver_t;
    argc: cint;
    argv: PPChar): cint; cdecl; external libjackserver;

(**
 * Call this function to get name of internal client.
 *
 * @param internal internal object handle to get name of
 *
 * @return internal name. Must not be modified. Always same for same
 * internal object.
 *)
function
jackctl_internal_get_name(
	internal: Pjackctl_internal_t): PChar; cdecl; external libjackserver;

(**
 * Call this function to get list of internal parameters. List node data
 * pointers is a parameter object handle (::jackctl_parameter_t).
 *
 * @param internal internal object handle to get parameters for
 *
 * @return Single linked list of parameter object handles. Must not be
 * modified. Always same for same internal object.
 *)
function
jackctl_internal_get_parameters(
	internal: Pjackctl_internal_t): PJSList; cdecl; external libjackserver;

(**
 * Call this function to get parameter name.
 *
 * @param parameter parameter object handle to get name of
 *
 * @return parameter name. Must not be modified. Always same for same
 * parameter object.
 *)
function
jackctl_parameter_get_name(
	parameter: Pjackctl_parameter_t): PChar; cdecl; external libjackserver;

(**
 * Call this function to get parameter short description.
 *
 * @param parameter parameter object handle to get short description of
 *
 * @return parameter short description. Must not be modified. Always
 * same for same parameter object.
 *)
function
jackctl_parameter_get_short_description(
	parameter: Pjackctl_parameter_t): PChar; cdecl; external libjackserver;

(**
 * Call this function to get parameter long description.
 *
 * @param parameter parameter object handle to get long description of
 *
 * @return parameter long description. Must not be modified. Always
 * same for same parameter object.
 *)
function
jackctl_parameter_get_long_description(
	parameter: Pjackctl_parameter_t): PChar; cdecl; external libjackserver;

(**
 * Call this function to get parameter type.
 *
 * @param parameter parameter object handle to get type of
 *
 * @return parameter type. Always same for same parameter object.
 *)
function
jackctl_parameter_get_type(
	parameter: Pjackctl_parameter_t): jackctl_param_type_t; cdecl; external libjackserver;

(**
 * Call this function to get parameter character.
 *
 * @param parameter parameter object handle to get character of
 *
 * @return character.
 *)
function
jackctl_parameter_get_id(
	parameter: Pjackctl_parameter_t): Char; cdecl; external libjackserver;

(**
 * Call this function to check whether parameter has been set, or its
 * default value is being used.
 *
 * @param parameter parameter object handle to check
 *
 * @return true - parameter is set, false - parameter is using default
 * value.
 *)
function
jackctl_parameter_is_set(
	parameter: Pjackctl_parameter_t): cbool; cdecl; external libjackserver;

(**
 * Call this function to reset parameter to its default value.
 *
 * @param parameter parameter object handle to reset value of
 *
 * @return success status: true - success, false - fail
 *)
function
jackctl_parameter_reset(
	parameter: Pjackctl_parameter_t): cbool; cdecl; external libjackserver;

(**
 * Call this function to get parameter value.
 *
 * @param parameter parameter object handle to get value of
 *
 * @return parameter value.
 *)
function
jackctl_parameter_get_value(
	parameter: Pjackctl_parameter_t): jackctl_parameter_value; cdecl; external libjackserver;

(**
 * Call this function to set parameter value.
 *
 * @param parameter parameter object handle to get value of
 * @param value_ptr pointer to variable containing parameter value
 *
 * @return success status: true - success, false - fail
 *)
function
jackctl_parameter_set_value(
	parameter: Pjackctl_parameter_t;
	const value_ptr: Pjackctl_parameter_value): cbool; cdecl; external libjackserver;

(**
 * Call this function to get parameter default value.
 *
 * @param parameter parameter object handle to get default value of
 *
 * @return parameter default value.
 *)
function
jackctl_parameter_get_default_value(
	parameter: Pjackctl_parameter_t): jackctl_parameter_value; cdecl; external libjackserver;

(**
 * Call this function check whether parameter has range constraint.
 *
 * @param parameter object handle of parameter to check
 *
 * @return whether parameter has range constraint.
 *)
function
jackctl_parameter_has_range_constraint(
	parameter: Pjackctl_parameter_t): cbool; cdecl; external libjackserver;

(**
 * Call this function check whether parameter has enumeration constraint.
 *
 * @param parameter object handle of parameter to check
 *
 * @return whether parameter has enumeration constraint.
 *)
function
jackctl_parameter_has_enum_constraint(
	parameter: Pjackctl_parameter_t): cbool; cdecl; external libjackserver;

(**
 * Call this function get how many enumeration values parameter has.
 *
 * @param parameter object handle of parameter
 *
 * @return number of enumeration values
 *)
function
jackctl_parameter_get_enum_constraints_count(
	parameter: Pjackctl_parameter_t): uint32_t; cdecl; external libjackserver;

(**
 * Call this function to get parameter enumeration value.
 *
 * @param parameter object handle of parameter
 * @param index index of parameter enumeration value
 *
 * @return enumeration value.
 *)
function
jackctl_parameter_get_enum_constraint_value(
	parameter: Pjackctl_parameter_t;
	index: uint32_t): jackctl_parameter_value; cdecl; external libjackserver;

(**
 * Call this function to get parameter enumeration value description.
 *
 * @param parameter object handle of parameter
 * @param index index of parameter enumeration value
 *
 * @return enumeration value description.
 *)
function
jackctl_parameter_get_enum_constraint_description(
	parameter: jackctl_parameter_t;
	index: uint32_t): PChar; cdecl; external libjackserver;

(**
 * Call this function to get parameter range.
 *
 * @param parameter object handle of parameter
 * @param min_ptr pointer to variable receiving parameter minimum value
 * @param max_ptr pointer to variable receiving parameter maximum value
 *)
procedure
jackctl_parameter_get_range_constraint(
	parameter: jackctl_parameter_t;
	min_ptr: Pjackctl_parameter_value;
	max_ptr: Pjackctl_parameter_value); cdecl; external libjackserver;

(**
 * Call this function to check whether parameter constraint is strict,
 * i.e. whether supplying non-matching value will not work for sure.
 *
 * @param parameter parameter object handle to check
 *
 * @return whether parameter constraint is strict.
 *)
function
jackctl_parameter_constraint_is_strict(
	parameter: Pjackctl_parameter_t): cbool; cdecl; external libjackserver;

(**
 * Call this function to check whether parameter has fake values,
 * i.e. values have no user meaningful meaning and only value
 * description is meaningful to user.
 *
 * @param parameter parameter object handle to check
 *
 * @return whether parameter constraint is strict.
 *)
function
jackctl_parameter_constraint_is_fake_value(
	parameter: Pjackctl_parameter_t): cbool; cdecl; external libjackserver;

(**
 * Call this function to log an error message.
 *
 * @param format string
 *)
procedure
jack_error(const format: PChar); cdecl; varargs; external libjackserver;

(**
 * Call this function to log an information message.
 *
 * @param format string
 *)
procedure
jack_info(const format: PChar); cdecl; varargs; external libjackserver;

(**
 * Call this function to log an information message but only when
 * verbose mode is enabled.
 *
 * @param format string
 *)
procedure
jack_log(const format: PChar); cdecl; varargs; external libjackserver;

///**@}*/

//#if 0
//{ /* Adjust editor indent */
//#endif
//#ifdef __cplusplus
//} /* extern "C" */
//#endif

{$endif JACKCTL_H__2EEDAD78_DF4C_4B26_83B7_4FF1A446A47E__INCLUDED}
