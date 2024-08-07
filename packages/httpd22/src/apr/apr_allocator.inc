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
 * @file apr_allocator.h
 * @brief APR Internal Memory Allocation
 }

{#include "apr.h"
#include "apr_errno.h"
#define APR_WANT_MEMFUNC < For no good reason?
#include "apr_want.h"}

{
 * @defgroup apr_allocator Internal Memory Allocation
 * @ingroup APR 
 }

type
{ the allocator structure }

  apr_allocator_t = record end;
  Papr_allocator_t = ^apr_allocator_t;
  PPapr_allocator_t = ^Papr_allocator_t;

{ the structure which holds information about the allocation }

  Papr_memnode_t = ^apr_memnode_t;
  PPapr_memnode_t = ^Papr_memnode_t;

{ basic memory node structure
 * @note The next, ref and first_avail fields are available for use by the
 *       caller of apr_allocator_alloc(), the remaining fields are read-only.
 *       The next field has to be used with caution and sensibly set when the
 *       memnode is passed back to apr_allocator_free().  See apr_allocator_free()
 *       for details.
 *       The ref and first_avail fields will be properly restored by
 *       apr_allocator_free().
 }

  apr_memnode_t = record
    next: Papr_memnode_t;            {< next memnode }
    ref: PPapr_memnode_t;            {< reference to self }
    index: apr_uint32_t;             {< size }
    free_index: apr_uint32_t;        {< how much free }
    first_avail: PAnsiChar;              {< pointer to first free memory }
    endp: PAnsiChar;                     {< pointer to end of free memory }
  end;

{ The base size of a memory node - aligned.  }
//#define APR_MEMNODE_T_SIZE APR_ALIGN_DEFAULT(sizeof(apr_memnode_t))

{ Symbolic constants }
const
  APR_ALLOCATOR_MAX_FREE_UNLIMITED = 0;

{
 * Create a new allocator
 * @param allocator The allocator we have just created.
 *
 }
function apr_allocator_create(allocator: PPapr_allocator_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_allocator_create' + LibSuff4;

{
 * Destroy an allocator
 * @param allocator The allocator to be destroyed
 * @remark Any memnodes not given back to the allocator prior to destroying
 *         will _not_ be free()d.
 }
procedure apr_allocator_destroy(allocator: Papr_allocator_t);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_allocator_destroy' + LibSuff4;

{
 * Allocate a block of mem from the allocator
 * @param allocator The allocator to allocate from
 * @param size The size of the mem to allocate (excluding the
 *        memnode structure)
 }
function apr_allocator_alloc(allocator: Papr_allocator_t;
 size: apr_size_t): Papr_memnode_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_allocator_alloc' + LibSuff8;

{
 * Free a list of blocks of mem, giving them back to the allocator.
 * The list is typically terminated by a memnode with its next field
 * set to NULL.
 * @param allocator The allocator to give the mem back to
 * @param memnode The memory node to return
 }
procedure apr_allocator_free(allocator: Papr_allocator_t; memnode: Papr_memnode_t);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_allocator_free' + LibSuff8;

//#include "apr_pools.h"

{
 * Set the owner of the allocator
 * @param allocator The allocator to set the owner for
 * @param pool The pool that is to own the allocator
 * @remark Typically pool is the highest level pool using the allocator
 }
{
 * XXX: see if we can come up with something a bit better.  Currently
 * you can make a pool an owner, but if the pool doesn't use the allocator
 * the allocator will never be destroyed.
 }
procedure apr_allocator_owner_set(allocator: Papr_allocator_t; pool: Papr_pool_t);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_allocator_owner_set' + LibSuff8;

{
 * Get the current owner of the allocator
 * @param allocator The allocator to get the owner from
 }
function apr_allocator_owner_get(allocator: Papr_allocator_t): Papr_pool_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_allocator_owner_get' + LibSuff4;

{
 * Set the current threshold at which the allocator should start
 * giving blocks back to the system.
 * @param allocator The allocator the set the threshold on
 * @param size The threshold.  0 == unlimited.
 }
procedure apr_allocator_max_free_set(allocator: Papr_allocator_t; size: apr_size_t);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_allocator_max_free_set' + LibSuff8;

{#include "apr_thread_mutex.h"}

{$ifdef APR_HAS_THREADS}
{
 * Set a mutex for the allocator to use
 * @param allocator The allocator to set the mutex for
 * @param mutex The mutex
 }
APR_DECLARE(void) apr_allocator_mutex_set(apr_allocator_t *allocator,
                                          apr_thread_mutex_t *mutex);

{
 * Get the mutex currently set for the allocator
 * @param allocator The allocator
 }
APR_DECLARE(apr_thread_mutex_t *) apr_allocator_mutex_get(
                                      apr_allocator_t *allocator);

{$endif} { APR_HAS_THREADS }

