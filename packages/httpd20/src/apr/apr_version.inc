{ Copyright 2000-2005 The Apache Software Foundation or its licensors, as
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

//#include "apr.h"

{
 * @file apr_version.h
 * @brief APR Versioning Interface
 * 
 * APR's Version
 *
 * There are several different mechanisms for accessing the version. There
 * is a string form, and a set of numbers; in addition, there are constants
 * which can be compiled into your application, and you can query the library
 * being used for its actual version.
 *
 * Note that it is possible for an application to detect that it has been
 * compiled against a different version of APR by use of the compile-time
 * constants and the use of the run-time query function.
 *
 * APR version numbering follows the guidelines specified in:
 *
 *     http://apr.apache.org/versioning.html
 }

{ The numeric compile-time version constants. These constants are the
 * authoritative version numbers for APR. 
 }

{ major version
 * Major API changes that could cause compatibility problems for older
 * programs such as structure size changes.  No binary compatibility is
 * possible across a change in the major version.
 }
const
  APR_MAJOR_VERSION = 0;

{
 * Minor API changes that do not cause binary compatibility problems.
 * Should be reset to 0 when upgrading APR_MAJOR_VERSION
 }
  APR_MINOR_VERSION = 9;

{ patch level }
  APR_PATCH_VERSION = 12;

{
 *  This symbol is defined for internal, "development" copies of APR. This
 *  symbol will be #undef'd for releases. 
 }
{.$undef APR_IS_DEV_VERSION}


{ The formatted string of APR's version }
{#define APR_VERSION_STRING \
     APR_STRINGIFY(APR_MAJOR_VERSION) "." \
     APR_STRINGIFY(APR_MINOR_VERSION) "." \
     APR_STRINGIFY(APR_PATCH_VERSION) \
     APR_IS_DEV_STRING}


{
 * The numeric version information is broken out into fields within this 
 * structure. 
 }
type
  apr_version_t = record
    major: Integer;      {< major number }
    minor: Integer;      {< minor number }
    patch: Integer;      {< patch number }
    is_dev: Integer;     {< is development (1 or 0) }
  end;

  Papr_version_t = ^apr_version_t;

{
 * Return APR's version information information in a numeric form.
 *
 *  @param pvsn Pointer to a version structure for returning the version
 *              information.
 }
procedure apr_version(pvsn: Papr_version_t);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_version' + LibSuff4;

{ Return APR's version information as a string. }
function apr_version_string: PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_version_string' + LibSuff0;


{ Internal: string form of the "is dev" flag }
{#ifdef APR_IS_DEV_VERSION
#define APR_IS_DEV_STRING "-dev"
#else}
const
  APR_IS_DEV_STRING = '';
//#endif

