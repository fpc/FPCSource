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
 * @package Provider API
 }

//#include "ap_config.h"

type
  ap_list_provider_names_t = record
    provider_name: PChar;
  end;

{
 * This function is used to register a provider with the global
 * provider pool.
 * @param pool The pool to create any storage from
 * @param provider_group The group to store the provider in
 * @param provider_name The name for this provider
 * @param provider_version The version for this provider
 * @param provider Opaque structure for this provider
 * @return APR_SUCCESS if all went well
 }
function ap_register_provider(pool: Papr_pool_t;
 const provider_group, provider_name, provider_version: PChar;
 const provider: Pointer): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_register_provider' + LibSuff20;

{
 * This function is used to retrieve a provider from the global
 * provider pool.
 * @param provider_group The group to look for this provider in
 * @param provider_name The name for the provider
 * @param provider_version The version for the provider
 * @return provider pointer to provider if found, NULL otherwise
 }
function ap_lookup_provider(provider_group, provider_name, provider_version: PChar): Pointer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_lookup_provider' + LibSuff12;

{
 * This function is used to retrieve a list (array) of provider
 * names from the specified group with the specified version.
 * @param pool The pool to create any storage from
 * @param provider_group The group to look for this provider in
 * @param provider_version The version for the provider
 * @return pointer to array of ap_list_provider_names_t of provider names (could be empty)
 }
function ap_list_provider_names(pool: Papr_pool_t;
 const provider_group, provider_version: PChar): Papr_array_header_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_list_provider_names' + LibSuff12;

