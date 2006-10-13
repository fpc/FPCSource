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
 * @file apr_signal.h 
 * @brief APR Signal Handling
 }

{#include "apr.h"
#include "apr_pools.h"

#if APR_HAVE_SIGNAL_H
#include <signal.h>
#endif}

{
 * @defgroup apr_signal Handling
 * @ingroup APR 
 }

{$if defined(APR_HAVE_SIGACTION) or defined(DOXYGEN)}

{$ifdef DARWIN}
{ work around Darwin header file bugs
 *   http://www.opensource.apple.com/bugs/X/BSD%20Kernel/2657228.html
 }
#undef SIG_DFL
#undef SIG_IGN
#undef SIG_ERR
#define SIG_DFL (void ( *)(int))0
#define SIG_IGN (void ( *)(int))1
#define SIG_ERR (void ( *)(int))-1
{$endif}

{ Function prototype for signal handlers }
type apr_sigfunc_t = procedure(para: cint);

{
 * Set the signal handler function for a given signal
 * @param signo The signal (eg... SIGWINCH)
 * @param func the function to get called
 }
function apr_signal(signo: Integer; func: apr_sigfunc_t): Papr_sigfunc_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_signal' + LibSuff8;

{if defined(SIG_IGN) && !defined(SIG_ERR)
#define SIG_ERR ((apr_sigfunc_t *) -1)
#endif

#else} { !APR_HAVE_SIGACTION }
{#define apr_signal(a, b) signal(a, b)
#endif}


{
 * Get the description for a specific signal number
 * @param signum The signal number
 * @return The description of the signal
 }
function apr_signal_description_get(signo: Integer): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_signal_description_get' + LibSuff4;

{ @deprecated @see apr_signal_description_get }
function apr_signal_get_description(signo: Integer): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_signal_get_description' + LibSuff4;

{
 * APR-private function for initializing the signal package
 * @internal
 * @param pglobal The internal, global pool
 }
//void apr_signal_init(apr_pool_t *pglobal);

{$endif}
