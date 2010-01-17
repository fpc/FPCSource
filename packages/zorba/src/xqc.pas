(*
 * Copyright 2006-2008 The FLWOR Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

{
  Translation of the zorba headers for FreePascal
  Copyright(C) 2009 by Ivo Steinmann
}

unit xqc;

{$mode objfpc}{$H+}
{$PACKRECORDS C}
{$MACRO ON}

interface

uses
  Classes,
  ctypes;

{$IFDEF UNIX}
  {$DEFINE extdecl:=cdecl}
{$ENDIF}
{$IFDEF WINDOWS}
  {$DEFINE extdecl:=stdcall}
{$ENDIF}

{$i xqc_error.inc}
{$i xqc_static_context_consts.inc}

type
  pfile = pointer;

  XQC_Implementation = ^XQC_Implementation_s;
  XQC_Implementation_Ref = ^XQC_Implementation;

  XQC_Query = ^XQC_Query_s;
  XQC_Query_Ref = ^XQC_Query;

  XQC_StaticContext = ^XQC_StaticContext_s;
  XQC_StaticContext_Ref = ^XQC_StaticContext;

  XQC_DynamicContext = ^XQC_DynamicContext_s;
  XQC_DynamicContext_Ref = ^XQC_DynamicContext;

  XQC_Sequence = ^XQC_Sequence_s;
  XQC_Sequence_Ref = ^XQC_Sequence;

  XQC_Item = ^XQC_Item_s;
  XQC_Item_Ref = ^XQC_Item;

  XQC_ItemFactory = ^XQC_ItemFactory_s;
  XQC_ItemFactory_Ref = ^XQC_ItemFactory;

  XQC_Collection = ^XQC_Collection_s;
  XQC_Collection_Ref = ^XQC_Collection;

  XQC_DataManager = ^XQC_DataManager_s;
  XQC_DataManager_Ref = ^XQC_DataManager;

  XQC_OutputStream = ^XQC_OutputStream_s;
  XQC_InputStream = ^XQC_InputStream_s;
  XQC_ErrorHandler = ^XQC_ErrorHandler_s;


// external functions
  external_function_init = procedure(out user_data: Pointer; global_user_data: Pointer); extdecl;

  external_function_next = function(args: XQC_Sequence; argc: cint; out res: XQC_Item;
    user_data: Pointer; global_user_data: Pointer): XQUERY_ERROR; extdecl;

  external_function_release = procedure(user_data: Pointer; global_user_data: Pointer); extdecl;


(**
 * The ::XQC_Implementation struct provides factory functions for parsing queries.
 * An XQC_Implementation object is thread-safe and can be used by multiple threads
 * of execution at the same time.
 *
 * Creating an XQC_Implementation object can be done using the zorba_implementation function.
 * Once created, the user is responsible for freeing the object by calling
 * the free() function.
 * The XQC_Implementation object should not be freed before all objects created using it's
 * functions have been freed - doing so causes undefined behaviour.
 *)
  XQC_Implementation_s = record
   (**
    * Creates a static context suitable for use in the parse() and parse_file()
    * functions. The user is responsible for freeing the ::XQC_StaticContext object returned by calling
    * XQC_StaticContext::free().
    *
    * \param implementation The XQC_Implementation that this function pointer is a member of
    * \param[out] context The newly created XQC_StaticContext object.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQP0019_INTERNAL_ERROR
    *)
    create_context: function(impl: XQC_Implementation; out context: XQC_StaticContext): XQUERY_ERROR; extdecl;

   (**
    * Prepares a query from a string, returning an ::XQC_Query object.
    * The user is responsible for freeing the ::XQC_Query object
    * returned by calling XQC_Query::free().
    *
    * \param implementation The XQC_Implementation that this function pointer is a member of.
    * \param string The query to prepare as a string.
    * \param context The initial static context for this query, or null to use the default
    *        static context.
    * \param handler An optional error handler whose <code>error</code> function is called
    *                if preparing the query fails.
    * \param[out] expression The resulting prepared expression.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQP0019_INTERNAL_ERROR
    * \retval An XQuery static or type error (e.g. XPST*, XPTY* )
    *)
    prepare: function(impl: XQC_Implementation; query_string: pchar; context: XQC_StaticContext;
      handler: XQC_ErrorHandler; out query: XQC_Query): XQUERY_ERROR; extdecl;

    (**
     * Prepares a query from a FILE pointer, returning an ::XQC_Query object.
     * The user remains responsible for closing the file after parsing.
     * The user is responsible for freeing the ::XQC_Query object returned by
     * calling XQC_Query::free().
     *
     * \param implementation The XQC_Implementation that this function pointer is a member of.
     * \param file The file containing the query to prepare.
     * \param context The initial static context for this query, or null to use the default
     *        static context.
     * \param handler An optional error handler whose <code>error</code> function is called
     *                if preparing the query fails.
     * \param[out] expression The resulting prepared expression.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval An XQuery static or type error (e.g. XPST*, XPTY* )
     *)
    prepare_file: function(impl: XQC_Implementation; query_file: pfile; context: XQC_StaticContext;
      handler: XQC_ErrorHandler; out query: XQC_Query): XQUERY_ERROR; extdecl;

    (**
     * Prepares a query from a ::XQC_InputStream, returning an ::XQC_Query object.
     * The user is responsible for freeing the ::XQC_Query object returned by
     * calling XQC_Query::free().
     *
     * \param implementation The XQC_Implementation that this function pointer is a member of.
     * \param stream The input stream returning the query to prepare.
     *               free will be called on the XQC_InputStream after the query has been read.
     * \param context The initial static context for this query, or null to use the default
     *        static context.
     * \param handler An optional error handler whose <code>error</code> function is called
     *                if preparing the query fails.
     * \param[out] expression The resulting prepared expression.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval An XQuery static or type error (e.g. XPST*, XPTY* )
     *)
    prepare_stream: function(impl: XQC_Implementation; stream: XQC_InputStream; context: XQC_StaticContext;
      handler: XQC_ErrorHandler; out query: XQC_Query): XQUERY_ERROR; extdecl;


    (**
     * Creates an item wrapper suitable for use in the ::XQC_Sequence::next function or
     * the ::XQC_ItemFactory::create functions.
     * The user is responsible for freeing the XQC_Item object returned by calling
     * XQC_Item::free().
     *
     * \param implementation The XQC_Implementation that this function pointer is a member of
     * \param[out] item The newly created XQC_Item wrapper object.
     *
  	 * \retval ::XQC_NO_ERROR
  	 * \retval ::XQP0019_INTERNAL_ERROR
     *)
    create_item: function(impl: XQC_Implementation; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a XQC_ItemFactory that can be used for creating items, i.e. instances of the
     * XQuery data model (XDM).
     * The user is responsible for freeing the XQC_ItemFactory object returned by calling
     * XQC_ItemFactory::free().
     *
     * \param implementation The XQC_Implementation that this function pointer is a member of
     * \param[out] factory The newly created XQC_ItemFactory  object.
     *
  	 * \retval ::XQC_NO_ERROR
  	 * \retval ::XQP0019_INTERNAL_ERROR
     *)
    item_factory: function(impl: XQC_Implementation; out factory: XQC_ItemFactory): XQUERY_ERROR; extdecl;


    (**
     * Creates a XQC_DataManager that can be used for managing collections and documents.
     * The user is responsible for freeing the XQC_DataManager object returned by calling
     * XQC_DataManager::free().
     *
     * \param implementation The XQC_Implementation that this function pointer is a member of
     * \param[out] data_manager The newly created XQC_DataManager object.
     *
  	 * \retval ::XQC_NO_ERROR
  	 * \retval ::XQP0019_INTERNAL_ERROR
     *)
    data_manager: function(impl: XQC_Implementation; out data_manager: XQC_DataManager): XQUERY_ERROR; extdecl;


    (**
     * Called to free the resources associated with the XQC_Implementation.
     *
     * \param implementation The XQC_Implementation that this function pointer is a member of
     *)
    free: procedure(impl: XQC_Implementation); extdecl;

    (**
     * for internal use only
     *)
    data: pointer;
  end;


(**
 * The ::XQC_Expression struct represents a pre-parsed query, and allows the user to execute
 * that query any number of times. An ::XQC_Expression object is thread-safe and
 * can be used by multiple threads of execution at the same time.
 *
 * ::XQC_Expression objects are created by calling the XQC_Implementation::prepare() or
 * XQC_Implementation::prepare_file() functions.
 * Once created, the user is responsible for freeing the object by calling the free() function.
 * The ::XQC_Expression object should be freed before the ::XQC_Implementation object
 * that created it.
 *)
 XQC_Query_s = record
    (**
     * This function returns the dynamic context that belongs to this query and
     * is used during query execution.
     * The context can be used, for example, to set values of external variables,
     * the default collation, or the current datetime.
     * It is only available if the query has been compiled, otherwise
     * an error is reported. Moreover, the context must not be modified during the
     * execution of a query (i.e. if a ResultIterator is opened).
  	 * The user is responsible for freeing the ::XQC_DynamicContext object returned by calling
  	 * XQC_DynamicContext::free().
     *
  	 * \retval ::XQC_NO_ERROR
  	 * \retval ::XQP0019_INTERNAL_ERROR
     *)
    get_dynamic_context: function(query: XQC_Query; out context: XQC_DynamicContext): XQUERY_ERROR; extdecl;


    (**
     * This function returns the static context that belongs to this query.
     * The static context is only available if the query has been compiled, otherwise
     * an error is reported.
     * The context has all the components and values that have been set by the either
     * the static context that was passed when creating the query and and those that
     * were set in the prolog of the query.
     * Note that after compilation of the query the static context is a read only structure.
  	 * The user is responsible for freeing the ::XQC_StaticContext object returned by calling
  	 * XQC_StaticContext::free().
     *
  	 * \retval ::XQC_NO_ERROR
  	 * \retval ::XQP0019_INTERNAL_ERROR
     *)
    get_static_context: function(query: XQC_Query; out context: XQC_StaticContext): XQUERY_ERROR; extdecl;


   (**
    * Executes the query represented by the XQC_Query object and prints the serialized XML
    * output to the given FILE pointer. The user remains responsible for closing
    * the file.
    *
    * \param query The XQC_Query that this function pointer is a member of.
    * \param file The FILE pointer to print the serialized result to.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQP0019_INTERNAL_ERROR
    * \retval ::API0023_CANNOT_SERIALIZE_UPDATE_QUERY
    * \retval An XQuery dynamic or type error (e.g. XPDY*, XPTY* )
    *)
    execute: function(query: XQC_Query; fileptr: pfile): XQUERY_ERROR; extdecl;


   (**
    * Executes the query represented by the XQC_Query object and prints the serialized
    * output to the given FILE pointer. The target format of the serialization is
    * specified by the passed serializer options.
    *
    * \param query The XQC_Query that this function pointer is a member of.
    * \param options The Zorba_SerializerOptions_t that specifies serializer options.
    * \param file The FILE pointer to print the serialized result to.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQP0019_INTERNAL_ERROR
    * \retval ::API0023_CANNOT_SERIALIZE_UPDATE_QUERY
    * \retval An XQuery dynamic or type error (e.g. XPDY*, XPTY* )
    *)
    serialize_file: function(query: XQC_Query; options: pointer; fileptr: pfile): XQUERY_ERROR; extdecl;


   (**
    * Executes the query represented by the XQC_Query object and writes the serialized
    * output to the given ::XQC_OutputStream. The target format of the serialization is
    * specified by the passed serializer options.
    *
    * \param query The XQC_Query that this function pointer is a member of.
    * \param options The Zorba_SerializerOptions_t that specifies serializer options.
    * \param file The XQC_OutputStream to print the serialized result to.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQP0019_INTERNAL_ERROR
    * \retval ::API0023_CANNOT_SERIALIZE_UPDATE_QUERY
    * \retval An XQuery dynamic or type error (e.g. XPDY*, XPTY*)
    serialize_stream: function(query: XQC_Query; options: pointer; stream: XQC_OutputStream): XQUERY_ERROR; extdecl;


   (**
    * Checks if the query is an updating query.
    *
    * \param query The XQC_Query that this function pionter is a member of.
    *
    * \retval 1 if query is updating query, else 0
    *)
    is_update_query: function(query: XQC_Query): cint; extdecl;


   (**
    * Applies the updates declared in the query represented by the XQC_Query object.
    *
    * \param query The XQC_Query that this function pointer is a member of.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQP0019_INTERNAL_ERROR
    * \retval ::API0023_CANNOT_SERIALIZE_UPDATE_QUERY
    * \retval An XQuery dynamic or type error (e.g. XPDY*, XPTY*)
    apply_updates: function(query: XQC_Query): XQUERY_ERROR; extdecl;


   (**
    * Executes the query represented by the XQC_Query object.
    * An ::XQC_Sequence object is returned which can be used to examine the results
    * of the query execution. The user is responsible for freeing the
    * ::XQC_Sequence object returned by calling XQC_Sequence::free().
    *
    * \param query The XQC_Query that this function pointer is a member of.
    * \param[out] sequence The newly created XQC_Sequence object.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQP0019_INTERNAL_ERROR
    * \retval ::API0024_CANNOT_ITERATE_OVER_UPDATE_QUERY
    * \retval An XQuery dynamic or type error (e.g. XPDY*, XPTY* )
    *)
    sequence: function(query: XQC_Query; out sequence: XQC_Sequence): XQUERY_ERROR; extdecl;


    (**
     * Sets the error handler whose <code>error</code> function is called
     * if an error occurs when executing the query.
     * The user keeps the ownership of this object and is required to freeing
     * the aquired resources.
     *)
    set_error_handler: procedure(query: XQC_Query; handler: XQC_ErrorHandler); extdecl;


   (**
    * Called to free the resources associated with the XQC_Query.
    *
    * \param query The XQC_Query that this function pointer is a member of.
    *)
    free: procedure(query: XQC_Query); extdecl;


    (**
     * for internal use only
     *)
    data: pointer;
  end;


(**
 * The ::XQC_StaticContext struct provides a way to specify values for the static context of
 * the query to be prepared. An ::XQC_StaticContext object is not thread-safe - threads should
 * each use their own instance of a ::XQC_StaticContext object (see create_child_context).
 *
 * ::XQC_StaticContext objects are created by calling the XQC_Implementation::create_context()
 * function. Once created, the user is responsible for freeing the object by calling
 * the free() function.
 * The ::XQC_StaticContext object should be freed before the ::XQC_Implementation object that
 * created it.
 *)
  XQC_StaticContext_s = record
   (**
    * Creates a child context of the given static context.
    * A child context contains the same information as it's parent context but
    * it allows the user to override and add information.
    * The user is responsible for freeing the ::XQC_StaticContext object returned by calling
    * XQC_StaticContext::free().
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param[out] child_context The newly created XQC_StaticContext object which is
    *             a child of the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQP0019_INTERNAL_ERROR
    *)
    create_child_context: function(context: XQC_StaticContext; out child_context: XQC_StaticContext): XQUERY_ERROR; extdecl;


   (**
    * Adds a (prefix, uri) pair to the set of statically known namespaces of
    * the given context.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of.
    * \param prefix The prefix of the namespace to add to the given XQC_StaticContext.
    * \param uri    The uri of the namespace to add to the given XQC_StaticContext.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    declare_ns: function(context: XQC_StaticContext; const prefix: pchar; const uri: ppchar): XQUERY_ERROR; extdecl;


   (**
    * Returns the namespace uri that belongs to the given prefix.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param prefix The prefix of the namespace to add to the given XQC_StaticContext.
    * \param[out] result_ns The namespace uri of the namespace registered with the given prefix.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    get_ns_by_prefix: function(context: XQC_StaticContext; const prefix: pchar; out result_ns: ppchar): XQUERY_ERROR; extdecl;


    (**
     * Sets the value of the default namespace for elements and types.
     *
  	 * \param context The XQC_StaticContext that this function pointer is a member of
     * \param uri The uri of the default element and type namespace to set in the given context.
     *
  	 * \retval ::XQC_NO_ERROR
  	 * \retval ::XQC_INTERNAL_ERROR
     *)
    set_default_element_and_type_ns: function(context: XQC_StaticContext; const uri: pchar): XQUERY_ERROR; extdecl;


   (**
    * Returns the default namespace for elements and types.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param[out] uri The uri of the default element and type namespace that is set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    get_default_element_and_type_ns: function(context: XQC_StaticContext; out uri: pchar): XQUERY_ERROR; extdecl;


   (**
    * Sets the default namespace for functions.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param uri The uri of the default function namespace to set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    set_default_function_ns: function(context: XQC_StaticContext; const uri: pchar): XQUERY_ERROR; extdecl;


   (**
    * Returns the default namespace for functions set in this static context.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param[out] uri The uri of the default function namespace that is set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    get_default_function_ns: function(context: XQC_StaticContext; out uri: pchar): XQUERY_ERROR; extdecl;


    (**
     * Add a collation URI.
     * The URI specifies the locale and collation strength of the collation that is added.
     * A valid collation URI must begin with http://www.flworfound.org/collations/.
     * This prefix is followed by a collation strength (i.e. PRIMARY, SECONDARY, TERTIARY,
     * QUATTERNARY, or IDENTICAL) followed by a '/'.
     * After the strength a lower-case two- or three-letter ISO-639 language code must follow.
     * The URI may end with an upper-case two-letter ISO-3166.
     * For example, http://www.flworfound.org/collations/PRIMARY/en/US
     * specifies an english language with US begin the country..
     *
     * Internally, ICU is used for comparing strings. For detailed description see
     * http://www.icu-project.org/apiref/icu4c/classCollator.html
     * and http://www.icu-project.org/apiref/icu4c/classLocale.html
     *
     * \param context The XQC_StaticContext that this function pointer is a member of
     * \param uri The URI of the collation to add.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQST0038
     * \retval ::XQC_INTERNAL_ERROR
     *)
    add_collation: function(context: XQC_StaticContext; const uri: pchar): XQUERY_ERROR; extdecl;


    (**
     * Set the URI of the default collation.
     * (see http://www.w3.org/TR/xquery/#static_context)
     *
     * \param context The XQC_StaticContext that this function pointer is a member of
     * \param uri The URI of the default collation to set
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQST0038
     * \retval ::XQC_INTERNAL_ERROR
     *)
    set_default_collation: function(context: XQC_StaticContext; const uri: pchar): XQUERY_ERROR; extdecl;


    (**
     * Get the URI of the default collation. The uri returned is valid
     * as long as the corresponding XQC_StaticContext object is valid.
     *
     * \param context The XQC_StaticContext that this function pointer is a member of
     * \param[out] uri The URI of the default collation that is currently set in the given context.
     *)
    get_default_collation: function(context: XQC_StaticContext; out uri: pchar): XQUERY_ERROR; extdecl;


    (**
    * Sets the XQuery processor's version to either xquery_version_1_0 or xquery_version_1_1.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param mode The xquery_version_t to set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    set_xquery_version: function(context: XQC_StaticContext; mode: xquery_version_t): XQUERY_ERROR; extdecl;


   (**
    * Returns the XQuery processor's version that is set in the given static context.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param[out] mode The xquery_version_t that is set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    get_xquery_version: function(context: XQC_StaticContext; out mode: xquery_version_t): XQUERY_ERROR; extdecl;


   (**
    * Sets the XPath 1.0 compatibility mode to either xpath1_0 or xpath2_0.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param mode The xpath1_0compatib_mode_t to set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    set_xpath1_0_mode: function(context: XQC_StaticContext; mode: xpath1_0compatib_mode_t): XQUERY_ERROR; extdecl;


   (**
    * Returns the XPath 1.0 compatibility that is set in the given static context.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param[out] mode The xpath1_0compatib_mode_t that is set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    get_xpath1_0_mode: function(context: XQC_StaticContext; out mode: xpath1_0compatib_mode_t): XQUERY_ERROR; extdecl;


   (**
    * Sets the construction mode to either preserve_cons or strip_cons.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param mode The construction_mode_t to set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    set_construction_mode: function(context: XQC_StaticContext; mode: construction_mode_t): XQUERY_ERROR; extdecl;


   (**
    * Returns the construction mode that is set in the given static context.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param[out] mode The construction_mode_t that is set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    get_construction_mode: function(context: XQC_StaticContext; out mode: construction_mode_t): XQUERY_ERROR; extdecl;


   (**
    * Sets the ordering mode to either order or unordered.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param mode The ordering_mode_t to set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    set_ordering_mode: function(context: XQC_StaticContext; mode: ordering_mode_t): XQUERY_ERROR; extdecl;


   (**
    * Returns the ordering mode that is set in the given static context.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param[out] mode The ordering_mode_t that is set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    get_ordering_mode: function(context: XQC_StaticContext; out mode: ordering_mode_t): XQUERY_ERROR; extdecl;


   (**
    * Sets the default order mode for empty sequences to either empty_least or
    * empty_greatest
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param mode The order_empty_mode_t to set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    set_default_order_empty_sequences: function(context: XQC_StaticContext; mode: order_empty_mode_t): XQUERY_ERROR; extdecl;


   (**
    * Returns the default order mode for empty sequences that is set in the given
    * static context.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param[out] mode The order_empty_mode_t that is set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    get_default_order_empty_sequences: function(context: XQC_StaticContext; out mode: order_empty_mode_t): XQUERY_ERROR; extdecl;


   (**
    * Sets the boundary space policy to either preserve_space or strip_space.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param mode The boundary_space_mode_t to set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    set_boundary_space_policy: function(context: XQC_StaticContext; mode: boundary_space_mode_t): XQUERY_ERROR; extdecl;


   (**
    * Returns the boundary space policy that is set in the given static context.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param[out] mode The boundary_space_mode_t that is set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    get_boundary_space_policy: function(context: XQC_StaticContext; out mode: boundary_space_mode_t): XQUERY_ERROR; extdecl;


   (**
    * Sets the copy namespace mode which consists of the preserve and the inherit mode.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param preserve The preserve_mode_t to set in the given context.
    * \param inherit The inherit_mode_t to set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    set_copy_ns_mode: function(context: XQC_StaticContext; mode: preserve_mode_t; inherit: inherit_mode_t): XQUERY_ERROR; extdecl;


   (**
    * Returns the copy namespace mode as a pair consisting of the preserve and the inherit
    * mode.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param[out] preserve The preserve_mode_t that is set in the given context.
    * \param[out] inherit The inherit_mode_t that is set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    get_copy_ns_mode: function(context: XQC_StaticContext; out mode: preserve_mode_t; out inherit: inherit_mode_t): XQUERY_ERROR; extdecl;


   (**
    * Sets the base uri in the given static context.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param base_uri The base uri to set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    set_base_uri: function(context: XQC_StaticContext; const base_uri: pchar): XQUERY_ERROR; extdecl;


   (**
    * Returns the base uri that is set in the given static context.
    * The returned base uri is only valid as long as the corresponding static context
    * is valid.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    * \param[out] base_uri The base uri that is set in the given context.
    *
    * \retval ::XQC_NO_ERROR
    * \retval ::XQC_INTERNAL_ERROR
    *)
    get_base_uri: function(context: XQC_StaticContext; out base_uri: pchar): XQUERY_ERROR; extdecl;


    (**
     * Register an external function that can be called within a query.
     * One external function consists of three function parameters, i.e. init, next, and release.
     *
     * \param context The XQC_StaticContext that this function pointer is a member of
     * \param uri The URI of the external function to add.
     * \param localname The localname of the function to add.
     * \param init A callback function pointer that is called once when the external function
     *             is initialized. The init function gets the global_user_data pointer
     *             as parameter.
     * \param next A callback function pointer that is called each time the corresponding
     *             XQuery function is executed.
     * \param release A callback function pointer that is called once when the external function
     *                is deinitialized.
     * \param global_user_data User specific data that is passed to the init function as a parameter.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::API0019_FUNCTION_ALREADY_REGISTERED,
     * \retval ::XQC_INTERNAL_ERROR
     *)
    register_external_function: function(context: XQC_StaticContext; const uri, localname: pchar; init: external_function_init;
      next: external_function_next; release: external_function_release; global_user_data: pointer): XQUERY_ERROR; extdecl;


   (**
    * Called to free the resources associated with the XQC_StaticContext.
    *
    * \param context The XQC_StaticContext that this function pointer is a member of
    *)
    free: procedure(context: XQC_StaticContext); extdecl;


    (**
     * for internal use only
     *)
    data: Pointer;
  end;


(**
 * An object of the type ::XQC_DynamicContext contains the information that is available at the
 * time the query is executed.  It contains the information that is defined in the %XQuery
 * specification (see http://www.w3.org/TR/xquery/#eval_context).
 * An instance of this struct can be retrieved by calling the <code>get_dynamic_context</code> function
 * of an ::XQC_Query object.
 *)
  XQC_DynamicContext_s = record
    (**
     * Sets the context item to the given ::XQC_Item.
     *
     * \param context The XQC_DynamicContext that this function pointer is a member of
     * \param value The XQC_Item for the context item.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQC_INTERNAL_ERROR
     *)
    set_context_item: function(context: XQC_DynamicContext; value: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Sets the context item to the document given by the FILE pointer.
     * The provided document is accessible by the provided doc_uri.
     *
     * \param context The XQC_DynamicContext that this function pointer is a member of
     * \param doc_uri The URI referencing the given document
     * \param document The document to which the context item should be set as a FILE pointer.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0016_LOADER_IO_ERROR,
     * \retval ::XQP0017_LOADER_PARSING_ERROR,
     * \retval ::XQC_INTERNAL_ERROR
     *)
    set_context_document: function(context: XQC_DynamicContext; const doc_uri: pchar; document: pfile): XQUERY_ERROR; extdecl;


    (**
     * Sets the external variable to the value given.
     *
     * \param context The XQC_DynamicContext that this function pointer is a member of
     * \param qname The qname of the external variable to set
     * \param value The XQC_Item value for the variable.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQC_INTERNAL_ERROR
     *)
    set_variable_item: function(context: XQC_DynamicContext; const qname: pchar; value: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Sets the external variable to the sequence given.
     *
     * \param context The XQC_DynamicContext that this function pointer is a member of
     * \param qname The qname of the external variable to set
     * \param value The XQC_Sequence value for the variable.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQC_INTERNAL_ERROR
     *)
    set_variable_sequence: function(context: XQC_DynamicContext; const qname: pchar; value: XQC_Sequence): XQUERY_ERROR; extdecl;


    (**
     * Sets the external variable to the document given by the FILE pointer.
     *
     * \param context The XQC_DynamicContext that this function pointer is a member of
     * \param var_qname The qname of the external variable to set
     * \param doc_uri The URI referencing the given document
     * \param document The document to which the context item should be set as a FILE pointer.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0016_LOADER_IO_ERROR,
     * \retval ::XQP0017_LOADER_PARSING_ERROR,
     * \retval ::XQC_INTERNAL_ERROR
     *)
    set_variable_document: function(context: XQC_DynamicContext; const var_qname, doc_uri: pchar; document: pfile): XQUERY_ERROR; extdecl;


    (**
     * Sets the implicit timezone parameter.
     *
     * \param context The XQC_DynamicContext that this function pointer is a member of
     * \param timezone The implicit timezone to set
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQC_INTERNAL_ERROR
     *)
    set_implicit_timezone: function(context: XQC_DynamicContext; timezone: cint): XQUERY_ERROR; extdecl;


    (**
     * Defines the value of the default collection that is used when calling the
     * fn:collection function without a parameter.
     *
     * \param context The XQC_DynamicContext that this function pointer is a member of
     * \param collection_uri the URI of the collection used by the fn:collection function.
     * \retval ::XQC_NO_ERROR
     * \retval ::XQC_INTERNAL_ERROR
     *)
    set_default_collection: function(context: XQC_DynamicContext; collection_uri: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Called to free the resources associated with the XQC_DynamicContext.
     *
     * \param context The XQC_DynamicContext that this function pointer is a member of
     *)
    free: procedure(context: XQC_DynamicContext); extdecl;


    (**
     * for internal use only
     *)
    data: pointer;
  end;


(**
 * This struct is Zorba's representation of an Item as defined in the
 * XQuery 1.0 and XPath 2.0 Data Model (XDM); see http://www.w3.org/TR/xpath-datamodel/.
 *
 * Instances of the XDM are a sequence, i.e. an ordered collection of zero or more items.
 * In the Zorba API, a sequence is represented by the XQC_Sequence struct.
 *
 * The Item class is the union of all XQuery node and atomic types.
 * The class provides functions to access the information of an Item. Note that not
 * all functions are defined on every Item kind. If a function is called on an Item that
 * does not provide the function called, an XQP0024_FUNCTION_NOT_IMPLEMENTED_FOR_ITEMTYPE error
 * is raised.
 *
 * A new atomic Item can be created using the ItemFactory. A new node Item should be created
 * by the result of a query.
 *)
  XQC_Item_s = record
    (**
     * The string value is the string that is extracted by calling the fn:string function
     * on the Item (see http://www.w3.org/TR/xpath-functions/#func-string).
     * Note that this function is available for all types of Items.
     *
     * \param item The XQC_Item that this function pointer is a member of
     * \param[out] string_value The string-value of the given item.
     *             This string is valid as long as the given item is valid.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0024_FUNCTION_NOT_IMPLEMENTED_FOR_ITEMTYPE
     *)
    string_value: function(item: XQC_Item; out string_value: pchar): XQUERY_ERROR; extdecl;


    (**
     * Get the (optional) value of a QName's prefix.
     * Note that this function is only available for Items of type QName.
     *
     * \param item The XQC_Item that this function pointer is a member of
     * \param[out] prefix The prefix of the given QName item.
     *             This string is valid as long as the given item is valid.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0024_FUNCTION_NOT_IMPLEMENTED_FOR_ITEMTYPE
     *)
    prefix: function(item: XQC_Item; out prefix: pchar): XQUERY_ERROR; extdecl;


    (**
     * Get the (optional) value of a QName's namespace.
     * Note that this function is only available for Items of type QName.
     *
     * \param item The XQC_Item that this function pointer is a member of
     * \param[out] namespace The namespace of the given QName item.
     *             This string is valid as long as the given item is valid.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0024_FUNCTION_NOT_IMPLEMENTED_FOR_ITEMTYPE
     *)
    ns: function(item: XQC_Item; out ns: pchar): XQUERY_ERROR; extdecl;


    (**
     * Get the value of a QName's localname.
     * Note that this function is only available for Items of type QName.
     *
     * \param item The XQC_Item that this function pointer is a member of
     * \param[out] localname The localname of the given QName item.
     *             This string is valid as long as the given item is valid.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0024_FUNCTION_NOT_IMPLEMENTED_FOR_ITEMTYPE
     *)
    localname: function(item: XQC_Item; out local_name: pchar): XQUERY_ERROR; extdecl;


    (**
     * Get the bool value of the boolean Item.
     * Note that this function is only available for Items of type boolean.
     *
     * \param item The XQC_Item that this function pointer is a member of
     * \param[out] bool_value 1 if the boolean value of the given item is true, 0 otherwise.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0024_FUNCTION_NOT_IMPLEMENTED_FOR_ITEMTYPE
     *)
    boolean_value: function(item: XQC_Item; out bool_value: cint): XQUERY_ERROR; extdecl;


    (**
     * Check if the value of the Item is not a number (NaN).
     * Note that this function is implemented for all item types but may only return
     * 1 for a numeric item (e.g. Double or Float).
     *
     * \param item The XQC_Item that this function pointer is a member of
     * \param[out] is_nan 1 if the given item is not a number, 0 otherwise.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0024_FUNCTION_NOT_IMPLEMENTED_FOR_ITEMTYPE
     *)
    nan: function(item: XQC_Item; out is_nan: cint): XQUERY_ERROR; extdecl;


    (**
      Check if the value of the Item is positive or negative infinity.
     * Note that this function is only available for numeric items (e.g. Double or Float).
     *
     * \param item The XQC_Item that this function pointer is a member of
     * \param[out] inf 1 if the given item is +/-INF, 0 otherwise.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0024_FUNCTION_NOT_IMPLEMENTED_FOR_ITEMTYPE
     *)
    pos_or_neg_inf: function(item: XQC_Item; out pos_or_neg_inf: cint): XQUERY_ERROR; extdecl;


    (**
     * Called to free the resources associated with the XQC_Item.
     *
     * \param item The XQC_Item that this function pointer is a member of
     *)
    free: procedure(item: XQC_Item); extdecl;


    (**
     * for internal use only
     *)
    data: pointer;
  end;


(**
 * An instance of this class can be obtained by calling <code>item_factory</code> function
 * of an ::XQC_Implementation object.
 *
 * Each <code>create_XXX</code> function of this struct creates an ::XQC_Item of an XML Schema item.
 * Each of the functions takes either NULL or a valid XQC_Item wrapper. The latter is created
 * by calling <code>XQC_Implementation::create_item</code>. In both cases, the user is responsible
 * for freeing the object by calling the XQC_Item::free() function.
 *)
  XQC_ItemFactory_s = record
    (**
     * Creates a String Item see [http://www.w3.org/TR/xmlschema-2/#string].
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param str The string as a char pointer.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_string: function(factory: XQC_ItemFactory; const str: pchar; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates an AnyURI Item see [http://www.w3.org/TR/xmlschema-2/#anyURI]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param str The uri as a char pointer.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_anyuri: function(factory: XQC_ItemFactory; const str: pchar; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a QName Item see [http://www.w3.org/TR/xmlschema-2/#QName]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param str The uri as a char pointer.
     * \param localname The localname as a char pointer.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_qname2: function(factory: XQC_ItemFactory; const uri, localname: pchar; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a QName Item see [http://www.w3.org/TR/xmlschema-2/#QName]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param str The uri as a char pointer.
     * \param prefix The prefix as a char pointer.
     * \param localname The localname as a char pointer.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_qname3: function(factory: XQC_ItemFactory; const uri, prefix, localname: pchar; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Boolean Item see [http://www.w3.org/TR/xmlschema-2/#bool]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param boolean 0 for a boolean <code>false</code> boolean item, 1 otherwise.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_boolean: function(factory: XQC_ItemFactory; boolean: cint; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a NCName Item see [http://www.w3.org/TR/xmlschema-2/#NCName]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param ncname The NCName as a char pointer.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_ncname: function(factory: XQC_ItemFactory; const ncname: pchar; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Base64Binary Item see [http://www.w3.org/TR/xmlschema-2/#base64Binary]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param binary_data The binary data as a char pointer.
     * \param letter The length of the binary data.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_base64binary: function(factory: XQC_ItemFactory; const binary_data: pchar; length: csize_t; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Decimal Item see [http://www.w3.org/TR/xmlschema-2/#decimal]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param value The value as a doule
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_decimal: function(factory: XQC_ItemFactory; value: cdouble; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Decimal Item see [http://www.w3.org/TR/xmlschema-2/#decimal]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param value The value as a char pointer.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_decimal_char: function(factory: XQC_ItemFactory; const value: pchar; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates an Integer Item see [http://www.w3.org/TR/xmlschema-2/#integer]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param integer_value The value as a long long.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_integer: function(factory: XQC_ItemFactory; const integer_value: clonglong; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates an Integer Item see [http://www.w3.org/TR/xmlschema-2/#integer]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param integer_value The value as a char pointer.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_integer_char: function(factory: XQC_ItemFactory; const integer_value: pchar; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Long Item see [http://www.w3.org/TR/xmlschema-2/#long]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param long_value The value as a long long.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_long: function(factory: XQC_ItemFactory; const long_value: clonglong; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Int Item see [http://www.w3.org/TR/xmlschema-2/#int]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param int_value The value as an int.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_int: function(factory: XQC_ItemFactory; const int_value: cint; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Short Item see [http://www.w3.org/TR/xmlschema-2/#short]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param short_value The value as a short.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_short: function(factory: XQC_ItemFactory; const short_value: cshort; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Byte Item see [http://www.w3.org/TR/xmlschema-2/#byte]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param byte_value The byte value as a char.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_byte: function(factory: XQC_ItemFactory; const byte_value: cchar; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Date Item see [http://www.w3.org/TR/xmlschema-2/#date]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param date_value The date value as a char pointer.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_date_char: function(factory: XQC_ItemFactory; const date_value: pchar; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Date Item see [http://www.w3.org/TR/xmlschema-2/#date]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param year The year value as a short.
     * \param month The month value as a short.
     * \param day The day value as a short.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_date: function(factory: XQC_ItemFactory; const year, month, day: cshort; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a DateTime Item see [http://www.w3.org/TR/xmlschema-2/#dateTime]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param year The year value as a short.
     * \param month The month value as a short.
     * \param day The day value as a short.
     * \param hour The hour value as a short.
     * \param minute The minute value as a short.
     * \param seconds The seconds value as a short.
     * \param timezone_hours The timezone as a short.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_datetime: function(factory: XQC_ItemFactory; const year, month, day, hour, minute: cshort;
      seconds: cdouble; timezone_hours: cshort; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a DateTime Item see [http://www.w3.org/TR/xmlschema-2/#dateTime]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param datetime_value The string representation of the datetime value as a char pointer
     *                       (for example, 2002-10-10T12:00:00-05:00).
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_datetime_char: function(factory: XQC_ItemFactory; const datetime_value: pchar; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Double Item see [http://www.w3.org/TR/xmlschema-2/#double]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_double: function(factory: XQC_ItemFactory; const value: cdouble; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Double Item see [http://www.w3.org/TR/xmlschema-2/#double]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param value The value as a char pointer.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_double_char: function(factory: XQC_ItemFactory; const value: pchar; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Duration Item see [http://www.w3.org/TR/xmlschema-2/#duration]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param year The year value as a short.
     * \param month The month value as a short.
     * \param day The day value as a short.
     * \param hour The hour value as a short.
     * \param minute The minute value as a short.
     * \param seconds The seconds value as a short.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_duration: function(factory: XQC_ItemFactory; const year, month, day, hour, minute: cshort;
      seconds: cdouble; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Float Item see [http://www.w3.org/tr/xmlschema-2/#float]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param value The float value as a char pointer.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_float: function(factory: XQC_ItemFactory; const value: pchar; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a gDay Item see [http://www.w3.org/TR/xmlschema-2/#gDay]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param day The day value as a short.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_gday: function(factory: XQC_ItemFactory; day: cshort; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a gMonth Item see [http://www.w3.org/TR/xmlschema-2/#gMonth]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param month The month value as a short.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_gmonth: function(factory: XQC_ItemFactory; month: cshort; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a gMonthDay Item see [http://www.w3.org/TR/xmlschema-2/#gMonthDay]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param month The month value as a short.
     * \param day The day value as a short.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_gmonthday: function(factory: XQC_ItemFactory; month, day: cshort; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a gYear Item see [http://www.w3.org/TR/xmlschema-2/#gYear]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param year The year value as a short.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_gyear: function(factory: XQC_ItemFactory; year: cshort; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a gYearMonth Item see [http://www.w3.org/TR/xmlschema-2/#gYearMonth]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param year The year value as a short.
     * \param month The month value as a short.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_gyearmonth: function(factory: XQC_ItemFactory; year, month: cshort; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a HexBinary Item see [http://www.w3.org/TR/xmlschema-2/#hexBinary]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param hex_data The hex data as a char pointer.
     * \param size The size of the hex data.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_hexbinary: function(factory: XQC_ItemFactory; const hex_data: pchar; size: csize_t; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a negativeInteger Item see [http://www.w3.org/TR/xmlschema-2/#negativeInteger]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param value The negative integer as a long long value.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_negativeinteger: function(factory: XQC_ItemFactory; value: clonglong; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a nonNegativeInteger Item see [http://www.w3.org/TR/xmlschema-2/#nonNegativeInteger]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param value The non-negative integer as an unsigned long long value.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_nonnegativeinteger: function(factory: XQC_ItemFactory; value: culonglong; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a nonPositiveInteger Item see [http://www.w3.org/TR/xmlschema-2/#nonPositiveInteger]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param value The non-positive integer as a long long value.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_nonpositiveinteger: function(factory: XQC_ItemFactory; value: clonglong; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a positiveInteger Item see [http://www.w3.org/TR/xmlschema-2/#positiveInteger]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param value The positive integer as an unsigned long long value.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_positiveinteger: function(factory: XQC_ItemFactory; value: culonglong; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Time Item see [http://www.w3.org/TR/xmlschema-2/#time]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param value The time as a char pointer.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_time_char: function(factory: XQC_ItemFactory; const value: pchar; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Time Item see [http://www.w3.org/TR/xmlschema-2/#time]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param hour The hour as a short.
     * \param minute The minute as a short.
     * \param second The second as a short.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_time: function(factory: XQC_ItemFactory; hour, minute: cshort; second: cdouble; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a Time Item see [http://www.w3.org/TR/xmlschema-2/#time]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param hour The hour as a short.
     * \param minute The minute as a short.
     * \param second The second as a double.
     * \param timezone_hours The timezone hours as a short.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_time_timezone: function(factory: XQC_ItemFactory; hour, minute: cshort; second: cdouble;
      timezone_hours: cshort; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates an Unsigned Byte Item see [http://www.w3.org/TR/xmlschema-2/#unsignedByte]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param value The unsigned byte value as an unsigned char.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_unsignedbyte: function(factory: XQC_ItemFactory; value: cuchar; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates an unsigned int Item see [http://www.w3.org/TR/xmlschema-2/#unsignedInt]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param value The unsigned int value as an unsigned int.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_unsignedint: function(factory: XQC_ItemFactory; value: cuint; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates an unsignedLong Item see [http://www.w3.org/TR/xmlschema-2/#unsignedLong]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param value The unsigned long value as an unsigned long long.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_unsignedlong: function(factory: XQC_ItemFactory; value: culonglong; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Creates a unsignedShort Item see [http://www.w3.org/TR/xmlschema-2/#unsignedShort]
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param value The unsigned short value as an unsigned short.
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::XQP0025_COULD_NOT_CREATE_ITEM
     *)
    create_unsignedshort: function(factory: XQC_ItemFactory; value: cushort; out item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Called to free the resources associated with the XQC_ItemFactory.
     *
     * \param factory The XQC_ItemFactory that this function pointer is a member of
     * \param[out] item The item to create. This can either be a wrapper created using
     *                 ::XQC_ItemFactory::create_item or a pointer initialized to 0.
     *)
    free: procedure(factory: XQC_ItemFactory); extdecl;


    (**
     * for internal use only
     *)
    data: pointer;
  end;


(**
 * This struct represents an instance of the XQuery 1.0 and XPath 2.0 Data Model (XDM).
 *
 * See http://www.w3.org/TR/xpath-datamodel/.
 *)
  XQC_Sequence_s = record
    (**
     * Get the next item of the sequence.
     *
     * \param sequence The XQC_Sequence_s that this function pointer is a member of
     * \param[out] item The item wrapper that should contain the next item if XQ_NO_ERROR is returned
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval any XQuery type or dynamic error
     *)
    next: function(sequence: XQC_Sequence; item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * called to free the resources associated with the xqc_itemfactory.
     *
     * \param sequence the XQC_Sequence that this function pointer is a member of
     *)
    free: procedure(sequence: XQC_Sequence); extdecl;


    (**
     * for internal use only
     *)
    data: pointer;
  end;


(**
 * A Collection is a sequence of Node Items.
 *
 * Each Collection is created by the XmlDataManager and referenced by a URI.
 * The URI can be accessed in a query's fn:collection function.
 *)
  XQC_Collection_s = record
    (**
     * Get the URI of a collection as an anyURI Item.
     *
     * \param collection the XQC_Collection_s that this function pointer is a member of
     * \param[out] uri_item The uri item of the given collection. The user is responsible
     *             for freeing the object by calling the XQC_Item::free() function.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     *)
    get_name: function(collection: XQC_Collection; out name_item: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Adds a Node Item to the Collection
     *
     * \param collection the XQC_Collection_s that this function pointer is a member of
     * \param node The node item to add to the given collection.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::API0007_COLLECTION_ITEM_MUST_BE_A_NODE
     *)
    add_node: function(collection: XQC_Collection; node: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Deletes a Node Item from the given Collection
     *
     * \param collection the XQC_Collection_s that this function pointer is a member of
     * \param node The node item to delete from the given collection.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::API0007_COLLECTION_ITEM_MUST_BE_A_NODE
     *)
    delete_node: function(collection: XQC_Collection; node: XQC_Item): XQUERY_ERROR; extdecl;


    (**
     * Adds a sequence of Node Items to the Collection
     *
     * \param collection the XQC_Collection_s that this function pointer is a member of
     * \param sequence The sequence of node items to add to the given collection.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0019_INTERNAL_ERROR
     * \retval ::API0007_COLLECTION_ITEM_MUST_BE_A_NODE
     *)
    add_sequence: function(collection: XQC_Collection; sequence: XQC_Sequence): XQUERY_ERROR; extdecl;


    (**
     * Adds a document given by the FILE pointer to this collection.
     *
     * \param collection the XQC_Collection_s that this function pointer is a member of
     * \param doc The document to add as a FILE pointer.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0016_LOADER_IO_ERROR,
     * \retval ::XQP0017_LOADER_PARSING_ERROR,
     * \retval ::XQC_INTERNAL_ERROR
     *)
    add_document: function(collection: XQC_Collection; doc: pfile): XQUERY_ERROR; extdecl;


    (**
     * Adds a document given by the char pointer to this collection.
     *
     * \param collection the XQC_Collection_s that this function pointer is a member of
     * \param doc The document to add as a char pointer.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0016_LOADER_IO_ERROR,
     * \retval ::XQP0017_LOADER_PARSING_ERROR,
     * \retval ::XQC_INTERNAL_ERROR
     *)
    add_document_char: function(collection: XQC_Collection; const doc: pchar): XQUERY_ERROR; extdecl;


    (**
     * called to free the resources associated with the xqc_itemfactory.
     *
     * \param collection the XQC_Collection that this function pointer is a member of
     *)
    free: procedure(collection: XQC_Collection); extdecl;


    (**
     * for internal use only
     *)
    data: pointer;
  end;


(**
 * Using the XmlDataManager one can manage documents and collections.
 *
 * The XmlDataManager is a singelton instance. The instance can be accessed by calling
 * XQC_Implementation::data_manager. The XmlDataManager is thread-safe.
 *)
  XQC_DataManager_s = record
    (**
     * This function loads a document from the given FILE pointer. The document
     * is identified by the given URI.
     *
     * \param data_manager The XQC_DataManager that this function pointer is a member of
     * \param doc_uri The URI of the document to load.
     * \param document The document to load as a FILE pointer.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0016_LOADER_IO_ERROR,
     * \retval ::XQP0017_LOADER_PARSING_ERROR,
     * \retval ::XQC_INTERNAL_ERROR
     *)
    load_document: function(data_manager: XQC_DataManager; const doc_uri: pchar; document: pfile): XQUERY_ERROR; extdecl;


    (**
     * This function loads a document that is retrieved from the given URI location. The document
     * is identified by the given URI.
     *
     * \param data_manager The XQC_DataManager that this function pointer is a member of
     * \param location The URI of the document to load.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQP0016_LOADER_IO_ERROR,
     * \retval ::XQP0017_LOADER_PARSING_ERROR,
     * \retval ::XQC_INTERNAL_ERROR
     *)
    load_document_uri: function(data_manager: XQC_DataManager; const location: pchar): XQUERY_ERROR; extdecl;


    (**
     * Get the document identified by the given URI.
     *
     * \param data_manager The XQC_DataManager that this function pointer is a member of
     * \param document_uri The URI of the document to retrieve.
     * \param[out] doc The Item of the document to get.  The user is responsible
     *                 for freeing the object by calling the XQC_Item::free() function.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQC_INTERNAL_ERROR
     *)
    get_document: function(data_manager: XQC_DataManager; const document_uri: pchar; out doc: XQC_Item): XQUERY_ERROR; extdecl;


    (*
     * Delete the document identified by the given URI.
     *
     * \param data_manager The XQC_DataManager that this function pointer is a member of
     * \param document_uri The URI of the document to delete.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQC_INTERNAL_ERROR
     *)
    delete_document: function(data_manager: XQC_DataManager; const document_uri: pchar): XQUERY_ERROR; extdecl;


    (**
     * Create a new collection that is identified by the given URI.
     *
     * \param data_manager The XQC_DataManager that this function pointer is a member of
     * \param document_uri The URI of the collection to create.
     * \param[out] col The collection to create. The user is responsible
     *                 for freeing the object by calling the XQC_Item::free() function.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQC_INTERNAL_ERROR
     *)
    create_collection: function(data_manager: XQC_DataManager; const collection_uri: pchar; out col: XQC_Collection): XQUERY_ERROR; extdecl;


    (**
     * Get the collection that is identified by the given URI.
     *
     * \param data_manager The XQC_DataManager that this function pointer is a member of
     * \param document_uri The URI of the collection to retrieve.
     * \param[out] col The collection to retrieve. The user is responsible
     *                 for freeing the object by calling the XQC_Item::free() function.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQC_INTERNAL_ERROR
     *)
    get_collection: function(data_manager: XQC_DataManager; const collection_uri: pchar; out col: XQC_Collection): XQUERY_ERROR; extdecl;


    (**
     * Delete the collection that is identified by the given URI.
     *
     * \param data_manager The XQC_DataManager that this function pointer is a member of
     * \param document_uri The URI of the collection to delete.
     *
     * \retval ::XQC_NO_ERROR
     * \retval ::XQC_INTERNAL_ERROR
     *)
    delete_collection: function(data_manager: XQC_DataManager; const collection_uri: pchar): XQUERY_ERROR; extdecl;

    (**
     * Called to free the resources associated with the XQC_DataManager.
     *
     * \param context The XQC_DataManager that this function pointer is a member of
     *)
    free: procedure(data_manager: XQC_DataManager); extdecl;


    (**
     * for internal use only
     *)
    data: pointer;
  end;


(**
 * The ::XQC_OutputStream struct is designed to be passed to an XQC implementation in order
 * to return streaming data (i.e. the result of a query).
 *)
  XQC_OutputStream_s = record
    (**
     * The function is called to provide the streaming result of a query
     * in the buffer provided.
     *
     * \param stream The XQC_OutputStream that this function pointer is a member of
     * \param buf The buffer that contains the data
     * \param length The length of the contents in the buffer
     *)
    write: procedure(stream: XQC_OutputStream; const buf: pchar; length: cuint); extdecl;


    (**
     * Called to free the resources associated with the XQC_OutputStream.
     * Free is called by the implementation if it finished writing to the stream.
     *
     * \param stream The XQC_OutputStream that this function pointer is a member of
     *
     *)
    free: procedure(stream: XQC_OutputStream); extdecl;


    (**
     * for internal use only
     *)
    data: pointer;
  end;


(**
 * The ::XQC_InputStream struct is designed to be populated by users for the purpose
 * of streaming data into an XQC implementation.
 *)
  XQC_InputStream_s = record
     (**
      * The function called to read more of the input (e.g. the query). The function should read
      * the next chunk of input into the buffer provided, returning the length of the
      * data read.
      *
      * \param stream The XQC_InputStream that this function pointer is a member of
      * \param[out] buffer The buffer to read the data into
      * \param length The length of the buffer
      *
      * \return The number of bytes read - this will be less than length if the end of the input is reached
      *         or -1 if an error occured
      *
      *)
    read: function(stream: XQC_InputStream; const buf: pchar; length: cuint): cint; extdecl;


    (**
     * Called to free the resources associated with the XQC_InputStream.
     * The free function is called by the implementation if it finished reading from the stream.
     * This allows for lazy evaluation without the user needing to know when reading from the
     * stream has finished.
     *
     * \param stream The XQC_InputStream that this function pointer is a member of
     *
     *)
    free: procedure(stream: XQC_InputStream); extdecl;


    (**
     * for internal use only
     *)
    data: pointer;
  end;


(**
 * The ::XQC_ErrorHandler struct is designed to be populated by users for the purpose
 * of collecting more detailed error messages from an XQC implementation. An XQC_ErrorHandler
 * can be set for a query using the XQC_Query::set_error_handler() function.
 *
 * The XQC_ErrorHandler struct has no free() function pointer because the user remains
 * responsible for freeing the resources associated with this struct.
 *)
  XQC_ErrorHandler_s = record
    (**
     * The function is called when an error occurs. The function receives the components of the
     * error as arguments. When this function returns, the implementation will exit query parsing or
     * execution with the error enumeration value passed as an argument.
     *
     * \param handler The XQC_ErrorHandler that this function pointer is a member of
     * \param error The error as a value of the XQUERY_ERROR enum.
     * \param local_name The local name of the error or an empty string if no local_name is given
     *                   (e.g. for errors not defined in the spec).
     * \param description A detailed description of the error or an empty string if no description is available.
     * \param query_uri The uri of the query causing the error or an empty string if no uri is available for the query.
     * \param line The line number of the query where the error occured.
     * \param components The column number in the line in the query where the error occured.
     *)
    error: procedure(handler: XQC_ErrorHandler; error: XQUERY_ERROR; const local_name, description,
      query_uri: pchar; line, column: cuint); extdecl;


    (**
     * Can be used for user specific purposes.
     *)
    data: pointer;
  end;


(* helper functions *)
procedure xqc_errorhandler_error(handler: XQC_ErrorHandler; error: XQUERY_ERROR; const local_name,
  description, query_uri: pchar; line, column: cuint); extdecl;

const
  XQC_Console_ErrorHandler_impl: XQC_ErrorHandler_s = (error:@xqc_errorhandler_error; data: nil);
  XQC_Console_ErrorHandler: XQC_ErrorHandler = @XQC_Console_ErrorHandler_impl;

function XQC_InputStream_create(const Stream: TStream; const Owned: Boolean): XQC_InputStream;
function XQC_OutputStream_create(const Stream: TStream; const Owned: Boolean): XQC_OutputStream;

implementation

procedure xqc_errorhandler_error(handler: XQC_ErrorHandler; error: XQUERY_ERROR; const local_name,
  description, query_uri: pchar; line, column: cuint); extdecl;
begin
  WriteLn(query_uri, '(', line, ',', column, ') ', local_name, ': ', description);
end;


type
  PStreamData = ^TStreamData;
  TStreamData = record
    Stream: TStream;
    Owned: Boolean;
  end;

function xqc_inputstream_read(stream: XQC_InputStream; const buf: pchar; length: cuint): cint; extdecl;
begin
  if Assigned(buf) and (length > 0) then
    Result := PStreamData(stream^.data)^.Stream.Read(buf^, length)
  else
    Result := 0;
end;

procedure xqc_inputstream_free(stream: XQC_InputStream); extdecl;
begin
  if PStreamData(stream^.data)^.Owned then
    PStreamData(stream^.data)^.Stream.Free;
  FreeMem(stream);
end;

function XQC_InputStream_create(const Stream: TStream; const Owned: Boolean): XQC_InputStream;
begin
  if not Assigned(Stream) then
    Exit(nil);

  GetMem(Result, Sizeof(XQC_InputStream_s)+Sizeof(TStreamData));
  Result^.read := @xqc_inputstream_read;
  Result^.free := @xqc_inputstream_free;
  Result^.data := PChar(Result) + Sizeof(XQC_InputStream_s);
  PStreamData(Result^.data)^.Owned := Owned;
  PStreamData(Result^.data)^.Stream := Stream;
end;

procedure xqc_outputstream_write(stream: XQC_OutputStream; const buf: pchar; length: cuint); extdecl;
begin
  if Assigned(buf) and (length > 0) then
    PStreamData(stream^.data)^.Stream.Write(buf^, length);
end;

procedure xqc_outputstream_free(stream: XQC_OutputStream); extdecl;
begin
  if PStreamData(stream^.data)^.Owned then
    PStreamData(stream^.data)^.Stream.Free;
  FreeMem(stream);
end;

function XQC_OutputStream_create(const Stream: TStream; const Owned: Boolean): XQC_OutputStream;
begin
  if not Assigned(Stream) then
    Exit(nil);

  GetMem(Result, Sizeof(XQC_OutputStream_s)+Sizeof(TStreamData));
  Result^.write := @xqc_outputstream_write;
  Result^.free := @xqc_outputstream_free;
  Result^.data := PChar(Result) + Sizeof(XQC_OutputStream_s);
  PStreamData(Result^.data)^.Owned := Owned;
  PStreamData(Result^.data)^.Stream := Stream;
end;

end.
