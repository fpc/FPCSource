{* Licensed to the Apache Software Foundation (ASF) under one or more
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
 *}

//fndef APR_USER_H
//efine APR_USER_H

{**
 * @file apr_user.h
 * @brief APR User ID Services 
 *}

//#include "apr.h"
//#include "apr_errno.h"
//#include "apr_pools.h"

(**
 * @defgroup apr_user User and Group ID Services
 * @ingroup APR 
 * @{
 *)

{**
 * Structure for determining user ownership.
 *}
type
{$ifdef WINDOWS}
  apr_uid_t = PSID;
{$else}
  apr_uid_t = uid_t;
{$endif}
  Papr_uid_t = ^apr_uid_t;

{**
 * Structure for determining group ownership.
 *}
{$ifdef WINDOWS}
  apr_gid_t = PSID;
{$else}
  apr_gid_t = gid_t;
{$endif}
  Papr_gid_t = ^apr_gid_t;

//#if APR_HAS_USER

{**
 * Get the userid (and groupid) of the calling process
 * @param userid   Returns the user id
 * @param groupid  Returns the user's group id
 * @param p The pool from which to allocate working space
 * @remark This function is available only if APR_HAS_USER is defined.
 *}
//APR_DECLARE(apr_status_t) apr_uid_current(apr_uid_t *userid,
//                                          apr_gid_t *groupid,
//                                          apr_pool_t *p);
function apr_uid_current(userid: Papr_uid_t;
                         groupid: Papr_gid_t;
                         p: Papr_pool_t): apr_status_t;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibAPR name LibNamePrefix + 'apr_uid_current' + LibSuff12;

{**
 * Get the user name for a specified userid
 * @param username Pointer to new string containing user name (on output)
 * @param userid The userid
 * @param p The pool from which to allocate the string
 * @remark This function is available only if APR_HAS_USER is defined.
 *}
//APR_DECLARE(apr_status_t) apr_uid_name_get(AnsiChar **username, apr_uid_t userid,
//                                           apr_pool_t *p);
function apr_uid_name_get(username: PPAnsiChar; userid: apr_uid_t;
                          p: Papr_pool_t): apr_status_t;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibAPR name LibNamePrefix + 'apr_uid_name_get' + LibSuff12;


{**
 * Get the userid (and groupid) for the specified username
 * @param userid   Returns the user id
 * @param groupid  Returns the user's group id
 * @param username The username to lookup
 * @param p The pool from which to allocate working space
 * @remark This function is available only if APR_HAS_USER is defined.
 *}
//APR_DECLARE(apr_status_t) apr_uid_get(apr_uid_t *userid, apr_gid_t *groupid,
//                                      const AnsiChar *username, apr_pool_t *p);
function apr_uid_get(userid: Papr_uid_t; groupid: Papr_gid_t;
                     const username: PAnsiChar; p: Papr_pool_t): apr_status_t;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibAPR name LibNamePrefix + 'apr_uid_get' + LibSuff16;

{**
 * Get the home directory for the named user
 * @param dirname Pointer to new string containing directory name (on output)
 * @param username The named user
 * @param p The pool from which to allocate the string
 * @remark This function is available only if APR_HAS_USER is defined.
 *}
//APR_DECLARE(apr_status_t) apr_uid_homepath_get(AnsiChar **dirname,
//                                               const AnsiChar *username,
//                                               apr_pool_t *p);
function apr_uid_homepath_get(dirname: PPAnsiChar;
                              const username: PAnsiChar;
                              p: Papr_pool_t): apr_status_t;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibAPR name LibNamePrefix + 'apr_uid_homepath_get' + LibSuff12;

{**
 * Compare two user identifiers for equality.
 * @param left One uid to test
 * @param right Another uid to test
 * @return APR_SUCCESS if the apr_uid_t strutures identify the same user,
 * APR_EMISMATCH if not, APR_BADARG if an apr_uid_t is invalid.
 * @remark This function is available only if APR_HAS_USER is defined.
 *}
//#if defined(WIN32)
//APR_DECLARE(apr_status_t) apr_uid_compare(apr_uid_t left, apr_uid_t right);
//#else
//#define apr_uid_compare(left,right) (((left) == (right)) ? APR_SUCCESS : APR_EMISMATCH)
//#endif

{**
 * Get the group name for a specified groupid
 * @param groupname Pointer to new string containing group name (on output)
 * @param groupid The groupid
 * @param p The pool from which to allocate the string
 * @remark This function is available only if APR_HAS_USER is defined.
 *}
//APR_DECLARE(apr_status_t) apr_gid_name_get(AnsiChar **groupname,
//                                             apr_gid_t groupid, apr_pool_t *p);
function apr_gid_name_get(groupname: PPAnsiChar;
                          groupid: apr_gid_t; p: Papr_pool_t): apr_status_t;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibAPR name LibNamePrefix + 'apr_gid_name_get' + LibSuff12;


{**
 * Get the groupid for a specified group name
 * @param groupid Pointer to the group id (on output)
 * @param groupname The group name to look up
 * @param p The pool from which to allocate the string
 * @remark This function is available only if APR_HAS_USER is defined.
 *}
//APR_DECLARE(apr_status_t) apr_gid_get(apr_gid_t *groupid,
//                                      const AnsiChar *groupname, apr_pool_t *p);
function apr_gid_get(groupid: Papr_gid_t;
                     const groupname: PAnsiChar; p: Papr_pool_t): apr_status_t;
   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   external LibAPR name LibNamePrefix + 'apr_gid_get' + LibSuff12;

{**
 * Compare two group identifiers for equality.
 * @param left One gid to test
 * @param right Another gid to test
 * @return APR_SUCCESS if the apr_gid_t strutures identify the same group,
 * APR_EMISMATCH if not, APR_BADARG if an apr_gid_t is invalid.
 * @remark This function is available only if APR_HAS_USER is defined.
 *}
//#if defined(WIN32)
//APR_DECLARE(apr_status_t) apr_gid_compare(apr_gid_t left, apr_gid_t right);
//#else
//#define apr_gid_compare(left,right) (((left) == (right)) ? APR_SUCCESS : APR_EMISMATCH)
//#endif

//#endif  /* ! APR_HAS_USER */

(** @} *)

//#endif  /* ! APR_USER_H */
