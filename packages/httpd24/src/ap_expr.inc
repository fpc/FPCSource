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

(**
 * @file ap_expr.h
 * @brief Expression parser
 *
 * @defgroup AP_EXPR Expression parser
 * @ingroup  APACHE_CORE
 * @{
 *)

//#ifndef AP_EXPR_H
//#define AP_EXPR_H

//#include "httpd.h"
//#include "http_config.h"
//#include "ap_regex.h"


//** A node in the expression parse tree */
//typedef struct ap_expr_node ap_expr_t;
type
  ap_expr_node_op_e = (
      op_NOP,
      op_True, op_False,
      op_Not, op_Or, op_And,
      op_Comp,
      op_EQ, op_NE, op_LT, op_LE, op_GT, op_GE, op_IN,
      op_REG, op_NRE,
      op_STR_EQ, op_STR_NE, op_STR_LT, op_STR_LE, op_STR_GT, op_STR_GE,
      op_Concat,
      op_Digit, op_String, op_Regex, op_RegexBackref,
      op_Var,
      op_ListElement,
      {*
       * call external functions/operators.
       * The info node contains the function pointer and some function specific
       * info.
       * For Binary operators, the Call node links to the Info node and the
       * Args node, which in turn links to the left and right operand.
       * For all other variants, the Call node links to the Info node and the
       * argument.
       *}
      op_UnaryOpCall, op_UnaryOpInfo,
      op_BinaryOpCall, op_BinaryOpInfo, op_BinaryOpArgs,
      op_StringFuncCall, op_StringFuncInfo,
      op_ListFuncCall, op_ListFuncInfo
  );

  ap_expr_node = record        {fpc -> from httpd-X-X-X/server/util_expr_private.h}
    node_op: ap_expr_node_op_e;
    node_arg1: Pointer;
    node_arg2: Pointer;
  end;
  ap_expr_t = ap_expr_node;
  Pap_expr_t = ^ap_expr_t;

//** Struct describing a parsed expression */
  Pap_expr_info_t = ^ap_expr_info_t;
  ap_expr_info_t = record
    //** The root of the actual expression parse tree */
    root_node: Pap_expr_t;
    {** The filename where the expression has been defined (for logging).
     *  May be NULL
     *}
    filename: PChar;
    //** The line number where the expression has been defined (for logging). */
    line_number: cuint;
    //** Flags relevant for the expression, see AP_EXPR_FLAG_* */
    flags: cuint;
    //** The module that is used for loglevel configuration */
    module_index: Integer;
  end; {ap_expr_info_t}

{** Use ssl_expr compatibility mode (changes the meaning of the comparison
 * operators)
 *}
const
  AP_EXPR_FLAG_SSL_EXPR_COMPAT = 1;
//** Don't add siginificant request headers to the Vary response header */
  AP_EXPR_FLAG_DONT_VARY       = 2;
{** Don't allow functions/vars that bypass the current request's access
 *  restrictions or would otherwise leak confidential information.
 *  Used by e.g. mod_include.
 *}
  AP_EXPR_FLAG_RESTRICTED      = 4;
//** Expression evaluates to a string, not to a bool */
  AP_EXPR_FLAG_STRING_RESULT   = 8;


{**
 * Evaluate a parse tree, simple interface
 * @param r The current request
 * @param expr The expression to be evaluated
 * @param err Where an error message should be stored
 * @return > 0 if expression evaluates to true, == 0 if false, < 0 on error
 * @note err will be set to NULL on success, or to an error message on error
 * @note request headers used during evaluation will be added to the Vary:
 *       response header, unless ::AP_EXPR_FLAG_DONT_VARY is set.
 *}
//AP_DECLARE(int) ap_expr_exec(request_rec *r, const ap_expr_info_t *expr,
//                             const char **err);
function ap_expr_exec(r: Prequest_rec; const expr: Pap_expr_info_t; err: PPChar): Integer;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_expr_exec' + LibSuff12;

{**
 * Evaluate a parse tree, with access to regexp backreference
 * @param r The current request
 * @param expr The expression to be evaluated
 * @param nmatch size of the regex match vector pmatch
 * @param pmatch information about regex matches
 * @param source the string that pmatch applies to
 * @param err Where an error message should be stored
 * @return > 0 if expression evaluates to true, == 0 if false, < 0 on error
 * @note err will be set to NULL on success, or to an error message on error
 * @note nmatch/pmatch/source can be used both to make previous matches
 *       available to ap_expr_exec_re and to use ap_expr_exec_re's matches
 *       later on.
 * @note request headers used during evaluation will be added to the Vary:
 *       response header, unless ::AP_EXPR_FLAG_DONT_VARY is set.
 *}
//AP_DECLARE(int) ap_expr_exec_re(request_rec *r, const ap_expr_info_t *expr,
//                                apr_size_t nmatch, ap_regmatch_t *pmatch,
//                                const char **source, const char **err);
function ap_expr_exec_re(r: Prequest_rec; const expr: Pap_expr_info_t;
                         nmatch: apr_size_t; pmatch: Pap_regmatch_t;
                         const source: PPChar; const err: PPChar): Integer;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_expr_exec_re' + LibSuff24;

//** Context used during evaluation of a parse tree, created by ap_expr_exec */
type
  Pap_expr_eval_ctx_t = ^ap_expr_eval_ctx_t;
  ap_expr_eval_ctx_t = record
    //** the current request */
    r : Prequest_rec;
    //** the current connection */
    c : Pconn_rec;
    //** the current connection */
    s : Pserver_rec;
    //** the pool to use */
    p : Papr_pool_t;
    //** where to store the error string */
    err : PPchar;
    //** ap_expr_info_t for the expression */
    info : Pap_expr_info_t;
    //** regex match information for back references */
    re_pmatch : Pap_regmatch_t;
    //** size of the vector pointed to by re_pmatch */
    re_nmatch : apr_size_t;
    //** the string corresponding to the re_pmatch */
    re_source : PPchar;
    {** A string where the comma separated names of headers are stored
     * to be later added to the Vary: header. If NULL, the caller is not
     * interested in this information.
     *}
    vary_this : PPchar;
    //** where to store the result string */
    result_string : PPchar;
    //** Arbitrary context data provided by the caller for custom functions */
    data : pointer;
    //** The current recursion level */
    reclvl : Integer;
  end; {ap_expr_eval_ctx_t}

{**
 * Evaluate a parse tree, full featured version
 * @param ctx The evaluation context with all data filled in
 * @return > 0 if expression evaluates to true, == 0 if false, < 0 on error
 * @note *ctx->err will be set to NULL on success, or to an error message on
 *       error
 * @note request headers used during evaluation will be added to the Vary:
 *       response header if ctx->vary_this is set.
 *}
//AP_DECLARE(int) ap_expr_exec_ctx(ap_expr_eval_ctx_t *ctx);
function ap_expr_exec_ctx(ctx: Pap_expr_eval_ctx_t): Integer;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_expr_exec_ctx' + LibSuff4;

{**
 * Evaluate a parse tree of a string valued expression
 * @param r The current request
 * @param expr The expression to be evaluated
 * @param err Where an error message should be stored
 * @return The result string, NULL on error
 * @note err will be set to NULL on success, or to an error message on error
 * @note request headers used during evaluation will be added to the Vary:
 *       response header, unless ::AP_EXPR_FLAG_DONT_VARY is set.
 *}
//AP_DECLARE(const char *) ap_expr_str_exec(request_rec *r,
//                                          const ap_expr_info_t *expr,
//                                          const char **err);
function ap_expr_str_exec(r: Prequest_rec;
                          const expr: Pap_expr_info_t;
                          const err: PPChar): PChar;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_expr_str_exec' + LibSuff12;

{**
 * Evaluate a parse tree of a string valued expression
 * @param r The current request
 * @param expr The expression to be evaluated
 * @param nmatch size of the regex match vector pmatch
 * @param pmatch information about regex matches
 * @param source the string that pmatch applies to
 * @param err Where an error message should be stored
 * @return The result string, NULL on error
 * @note err will be set to NULL on success, or to an error message on error
 * @note nmatch/pmatch/source can be used both to make previous matches
 *       available to ap_expr_exec_re and to use ap_expr_exec_re's matches
 *       later on.
 * @note request headers used during evaluation will be added to the Vary:
 *       response header, unless ::AP_EXPR_FLAG_DONT_VARY is set.
 *}
//AP_DECLARE(const char *) ap_expr_str_exec_re(request_rec *r,
//                                             const ap_expr_info_t *expr,
//                                             apr_size_t nmatch,
//                                             ap_regmatch_t *pmatch,
//                                             const char **source,
//                                             const char **err);
function ap_expr_str_exec_re(r: Prequest_rec;
                          const expr: Pap_expr_info_t;
                          nmatch: apr_size_t;
                          pmatch: Pap_regmatch_t;
                          const source: PPChar;
                          const err: PPChar): PChar;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_expr_str_exec_re' + LibSuff24;


{**
 * The parser can be extended with variable lookup, functions, and
 * and operators.
 *
 * During parsing, the parser calls the lookup function to resolve a
 * name into a function pointer and an opaque context for the function.
 * If the argument to a function or operator is constant, the lookup function
 * may also parse that argument and store the parsed data in the context.
 *
 * The default lookup function is the hook ::ap_expr_lookup_default which just
 * calls ap_run_expr_lookup. Modules can use it to make functions and
 * variables generally available.
 *
 * An ap_expr consumer can also provide its own custom lookup function to
 * modify the set of variables and functions that are available. The custom
 * lookup function can in turn call 'ap_run_expr_lookup'.
 *}

{** Unary operator, takes one string argument and returns a bool value.
 * The name must have the form '-z' (one letter only).
 * @param ctx The evaluation context
 * @param data An opaque context provided by the lookup hook function
 * @param arg The (right) operand
 * @return 0 or 1
 *}
//typedef int ap_expr_op_unary_t(ap_expr_eval_ctx_t *ctx, const void *data,
//                               const char *arg);
type
  ap_expr_op_unary_t = function(ctx: Pap_expr_eval_ctx_t; const data: Pointer;
                                const arg: PChar): Integer; cdecl;

{** Binary operator, takes two string arguments and returns a bool value.
 * The name must have the form '-cmp' (at least two letters).
 * @param ctx The evaluation context
 * @param data An opaque context provided by the lookup hook function
 * @param arg1 The left operand
 * @param arg2 The right operand
 * @return 0 or 1
 *}
//typedef int ap_expr_op_binary_t(ap_expr_eval_ctx_t *ctx, const void *data,
//                                const char *arg1, const char *arg2);
  ap_expr_op_binary_t = function(ctx: Pap_expr_eval_ctx_t; const data: Pointer;
                                 const arg1: PChar; const arg2: PChar): Integer; cdecl;

{** String valued function, takes a string argument and returns a string
 * @param ctx The evaluation context
 * @param data An opaque context provided by the lookup hook function
 * @param arg The argument
 * @return The functions result string, may be NULL for 'empty string'
 *}
//typedef const char *(ap_expr_string_func_t)(ap_expr_eval_ctx_t *ctx,
//                                            const void *data,
//                                            const char *arg);
  ap_expr_string_func_t = function(ctx: Pap_expr_eval_ctx_t;
                                   const data: Pointer;
                                   const arg: PChar): PChar; cdecl;

{** List valued function, takes a string argument and returns a list of strings
 * Can currently only be called following the builtin '-in' operator.
 * @param ctx The evaluation context
 * @param data An opaque context provided by the lookup hook function
 * @param arg The argument
 * @return The functions result list of strings, may be NULL for 'empty array'
 *}
//typedef apr_array_header_t *(ap_expr_list_func_t)(ap_expr_eval_ctx_t *ctx,
//                                                  const void *data,
//                                                  const char *arg);
  ap_expr_list_func_t = function(ctx: Pap_expr_eval_ctx_t;
                                 const data: Pointer;
                                 const arg: PChar): Papr_array_header_t;

{** Variable lookup function, takes no argument and returns a string
 * @param ctx The evaluation context
 * @param data An opaque context provided by the lookup hook function
 * @return The expanded variable
 *}
//typedef const char *(ap_expr_var_func_t)(ap_expr_eval_ctx_t *ctx,
//                                         const void *data);
  ap_expr_var_func_t = function(ctx: Pap_expr_eval_ctx_t;
                                const data: Pointer): PChar; cdecl;

const
  AP_EXPR_FUNC_VAR        = 0;
  AP_EXPR_FUNC_STRING     = 1;
  AP_EXPR_FUNC_LIST       = 2;
  AP_EXPR_FUNC_OP_UNARY   = 3;
  AP_EXPR_FUNC_OP_BINARY  = 4;

//** parameter struct passed to the lookup hook functions */
type
  Pap_expr_lookup_parms = ^ap_expr_lookup_parms;
  ap_expr_lookup_parms = record
    //** type of the looked up object */
    type_: Integer;
//#define AP_EXPR_FUNC_VAR        0   {fpc -> consts are moved up}
//#define AP_EXPR_FUNC_STRING     1
//#define AP_EXPR_FUNC_LIST       2
//#define AP_EXPR_FUNC_OP_UNARY   3
//#define AP_EXPR_FUNC_OP_BINARY  4
    //** name of the looked up object */
    name: PChar;

    flags: Integer;

    pool: Papr_pool_t;
    ptemp: Papr_pool_t;

    //** where to store the function pointer */
    func: PPointer;
    //** where to store the function's context */
    data: PPointer;
    //** where to store the error message (if any) */
    err: PPChar;

    {** arg for pre-parsing (only if a simple string).
     *  For binary ops, this is the right argument. *}
    arg: PChar;
  end; {ap_expr_lookup_parms}

{** Function for looking up the provider function for a variable, operator
 *  or function in an expression.
 *  @param parms The parameter struct, also determins where the result is
 *               stored.
 *  @return OK on success,
 *          !OK on failure,
 *          DECLINED if the requested name is not handled by this function
 *}
//typedef int (ap_expr_lookup_fn_t)(ap_expr_lookup_parms *parms);
type
  ap_expr_lookup_fn_t = function(parms: ap_expr_lookup_parms): Integer; cdecl;

{** Default lookup function which just calls ap_run_expr_lookup().
 *  ap_run_expr_lookup cannot be used directly because it has the wrong
 *  calling convention under Windows.
 *}
//AP_DECLARE_NONSTD(int) ap_expr_lookup_default(ap_expr_lookup_parms *parms);
function ap_expr_lookup_default(parms: Pap_expr_lookup_parms): Integer; cdecl;
  external LibHTTPD name LibNamePrefix + 'ap_expr_lookup_default';

//AP_DECLARE_HOOK(int, expr_lookup, (ap_expr_lookup_parms *parms))
{macro ignored for now}

{**
 * Parse an expression into a parse tree
 * @param pool Pool
 * @param ptemp temp pool
 * @param info The ap_expr_info_t struct (with values filled in)
 * @param expr The expression string to parse
 * @param lookup_fn The lookup function to use, NULL for default
 * @return NULL on success, error message on error.
 *         A pointer to the resulting parse tree will be stored in
 *         info->root_node.
 *}
//AP_DECLARE(const char *) ap_expr_parse(apr_pool_t *pool, apr_pool_t *ptemp,
//                                       ap_expr_info_t *info, const char *expr,
//                                       ap_expr_lookup_fn_t *lookup_fn);
function ap_expr_parse(pool, ptemp: Papr_pool_t;
                       info: Pap_expr_info_t;
                       const expr: PChar;
                       lookup_fn: ap_expr_lookup_fn_t): PChar;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_expr_parse' + LibSuff20;

{**
 * High level interface to ap_expr_parse that also creates ap_expr_info_t and
 * uses info from cmd_parms to fill in most of it.
 * @param cmd The cmd_parms struct
 * @param expr The expression string to parse
 * @param flags The flags to use, see AP_EXPR_FLAG_*
 * @param err Set to NULL on success, error message on error
 * @param lookup_fn The lookup function used to lookup vars, functions, and
 *        operators
 * @param module_index The module_index to set for the expression
 * @return The parsed expression
 * @note Usually ap_expr_parse_cmd() should be used
 *}
//AP_DECLARE(ap_expr_info_t *) ap_expr_parse_cmd_mi(const cmd_parms *cmd,
//                                                  const char *expr,
//                                                  unsigned int flags,
//                                                  const char **err,
//                                                  ap_expr_lookup_fn_t *lookup_fn,
//                                                  int module_index);
function ap_expr_parse_cmd_mi(cmd: Pcmd_parms;
                              const expr: PChar;
                              flags: cuint;
                              const err: PPChar;
                              lookup_fn: ap_expr_lookup_fn_t;
                              module_index: Integer): PChar;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_expr_parse_cmd_mi' + LibSuff24;

{**
 * Convenience wrapper for ap_expr_parse_cmd_mi() that sets
 * module_index = APLOG_MODULE_INDEX
 *}
//#define ap_expr_parse_cmd(cmd, expr, flags, err, lookup_fn) \
//        ap_expr_parse_cmd_mi(cmd, expr, flags, err, lookup_fn, APLOG_MODULE_INDEX)

 {**
  * Internal initialisation of ap_expr (for httpd internal use)
  *}
//void ap_expr_init(apr_pool_t *pool);

//#endif /* AP_EXPR_H */
//** @} */
