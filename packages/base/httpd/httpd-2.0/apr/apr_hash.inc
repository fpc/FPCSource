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

{
 * @file apr_hash.h
 * @brief APR Hash Tables
 }

//#include "apr_pools.h"

{
 * @defgroup apr_hash Hash Tables
 * @ingroup APR 
 * @
 }

{
 * When passing a key to apr_hash_set or apr_hash_get, this value can be
 * passed to indicate a string-valued key, and have apr_hash compute the
 * length automatically.
 *
 * @remark apr_hash will use strlen(key) for the length. The null-terminator
 *         is not included in the hash value (why throw a constant in?).
 *         Since the hash table merely references the provided key (rather
 *         than copying it), apr_hash_this() will return the null-term'd key.
 }
const
  APR_HASH_KEY_STRING = -1;

{
 * Abstract type for hash tables.
 }
type
  apr_hash_t = record end;
  Papr_hash_t = ^apr_hash_t;

{
 * Abstract type for scanning hash tables.
 }
  apr_hash_index_t = record end;
  Papr_hash_index_t = ^apr_hash_index_t;

{
 * Create a hash table.
 * @param pool The pool to allocate the hash table out of
 * @return The hash table just created
  }
function apr_hash_make(pool: Papr_pool_t): Papr_hash_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_hash_make' + LibSuff4;

{
 * Make a copy of a hash table
 * @param pool The pool from which to allocate the new hash table
 * @param h The hash table to clone
 * @return The hash table just created
 * @remark Makes a shallow copy
 }
function apr_hash_copy(pool: Papr_pool_t; const h: Papr_hash_t): Papr_hash_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_hash_copy' + LibSuff8;

{
 * Associate a value with a key in a hash table.
 * @param ht The hash table
 * @param key Pointer to the key
 * @param klen Length of the key. Can be APR_HASH_KEY_STRING to use the string length.
 * @param val Value to associate with the key
 * @remark If the value is NULL the hash entry is deleted.
 }
procedure apr_hash_set(ht: Papr_hash_t; const key: Pointer;
 klen: apr_size_t; const val: Pointer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_hash_set' + LibSuff16;

{
 * Look up the value associated with a key in a hash table.
 * @param ht The hash table
 * @param key Pointer to the key
 * @param klen Length of the key. Can be APR_HASH_KEY_STRING to use the string length.
 * @return Returns NULL if the key is not present.
 }
function apr_hash_get(ht: Papr_hash_t; const key: Pointer;
 klen: apr_size_t): Pointer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_hash_get' + LibSuff12;

{
 * Start iterating over the entries in a hash table.
 * @param p The pool to allocate the apr_hash_index_t iterator. If this
 *          pool is NULL, then an internal, non-thread-safe iterator is used.
 * @param ht The hash table
 * @remark  There is no restriction on adding or deleting hash entries during
 * an iteration (although the results may be unpredictable unless all you do
 * is delete the current entry) and multiple iterations can be in
 * progress at the same time.

 * @example
 }
{
 * <PRE>
 * 
 * int sum_values(apr_pool_t *p, apr_hash_t *ht)
 * (
 *     apr_hash_index_t *hi;
 *     void *val;
 *     int sum = 0;
 *     for (hi = apr_hash_first(p, ht); hi; hi = apr_hash_next(hi)) (
 *         apr_hash_this(hi, NULL, NULL, &val);
 *         sum += *(int * )val;
 *     )
 *     return sum;
 * )
 * </PRE>
 }
function apr_hash_first(p: Papr_pool_t; ht: Papr_hash_t): Papr_hash_index_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_hash_first' + LibSuff8;

{
 * Continue iterating over the entries in a hash table.
 * @param hi The iteration state
 * @return a pointer to the updated iteration state.  NULL if there are no more  
 *         entries.
 }
function apr_hash_next(hi: Papr_hash_index_t): Papr_hash_index_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_hash_next' + LibSuff4;

{
 * Get the current entry's details from the iteration state.
 * @param hi The iteration state
 * @param key Return pointer for the pointer to the key.
 * @param klen Return pointer for the key length.
 * @param val Return pointer for the associated value.
 * @remark The return pointers should point to a variable that will be set to the
 *         corresponding data, or they may be NULL if the data isn't interesting.
 }
procedure apr_hash_this(hi: Papr_hash_index_t; const key: PPointer;
 klen: Papr_size_t; val: PPointer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_hash_this' + LibSuff16;

{
 * Get the number of key/value pairs in the hash table.
 * @param ht The hash table
 * @return The number of key/value pairs in the hash table.
 }
function apr_hash_count(ht: Papr_hash_t): cuint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_hash_count' + LibSuff4;

{
 * Merge two hash tables into one new hash table. The values of the overlay
 * hash override the values of the base if both have the same key.
 * @param p The pool to use for the new hash table
 * @param overlay The table to add to the initial table
 * @param base The table that represents the initial values of the new table
 * @return A new hash table containing all of the data from the two passed in
 }
function apr_hash_overlay(p: Papr_pool_t;
 const overlay, base: Papr_hash_t): Papr_hash_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_hash_overlay' + LibSuff12;

{
 * Merge two hash tables into one new hash table. If the same key
 * is present in both tables, call the supplied merge function to
 * produce a merged value for the key in the new table.
 * @param p The pool to use for the new hash table
 * @param h1 The first of the tables to merge
 * @param h2 The second of the tables to merge
 * @param merger A callback function to merge values, or NULL to
 *  make values from h1 override values from h2 (same semantics as
 *  apr_hash_overlay())
 * @param data Client data to pass to the merger function
 * @return A new hash table containing all of the data from the two passed in
 }
type
  apr_hash_merge_t = function (p: Papr_pool_t; const key: Pointer; klen: apr_size_t;
   const h1_val, h2_val, data: Pointer): Pointer; cdecl;

function apr_hash_merge(p: Papr_pool_t;
 const h1, h2: Papr_hash_t;
 merger: apr_hash_merge_t; const data: Pointer): Papr_hash_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_hash_merge' + LibSuff20;

{
 * Get a pointer to the pool which the hash table was created in
 }
//APR_POOL_DECLARE_ACCESSOR(hash);

