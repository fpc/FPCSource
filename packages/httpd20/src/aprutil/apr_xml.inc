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
 * @file apr_xml.h
 * @brief APR-UTIL XML Library
 }

{
 * @defgroup APR_Util_XML XML 
 * @ingroup APR_Util
 }
{#include "apr_pools.h"
#include "apr_tables.h"
#include "apr_file_io.h"

#include "apu.h"}

{
 * @package Apache XML library
 }

{ -------------------------------------------------------------------- }

{ ### these will need to move at some point to a more logical spot }

{ @see apr_text }
type
  Papr_text = ^apr_text;

  { Structure to keep a linked list of pieces of text }

  apr_text = record
    { The current piece of text }
    text: PChar;
    { a pointer to the next piece of text }
    next: Papr_text;
  end;

{ @see apr_text_header }
  Papr_text_header = ^apr_text_header;

{ A list of pieces of text }
  apr_text_header = record
    { The first piece of text in the list }
    first: Papr_text;
    { The last piece of text in the list }
    last: Papr_text;
  end;

{
 * Append a piece of text to the end of a list
 * @param p The pool to allocate out of
 * @param hdr The text header to append to
 * @param text The new text to append
 }
procedure apr_text_append(p: Papr_pool_t; hdr: Papr_text_header;
 const text: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_text_append' + LibSuff12;

{ --------------------------------------------------------------------
**
** XML PARSING
}

{
** Qualified namespace values
**
** APR_XML_NS_DAV_ID
**    We always insert the "DAV:" namespace URI at the head of the
**    namespace array. This means that it will always be at ID==0,
**    making it much easier to test for.
**
** APR_XML_NS_NONE
**    This special ID is used for two situations:
**
**    1) The namespace prefix begins with "xml" (and we do not know
**       what it means). Namespace prefixes with "xml" (any case) as
**       their first three characters are reserved by the XML Namespaces
**       specification for future use. mod_dav will pass these through
**       unchanged. When this identifier is used, the prefix is LEFT in
**       the element/attribute name. Downstream processing should not
**       prepend another prefix.
**
**    2) The element/attribute does not have a namespace.
**
**       a) No prefix was used, and a default namespace has not been
**          defined.
**       b) No prefix was used, and the default namespace was specified
**          to mean "no namespace". This is done with a namespace
**          declaration of:  xmlns=""
**          (this declaration is typically used to override a previous
**          specification for the default namespace)
**
**       In these cases, we need to record that the elem/attr has no
**       namespace so that we will not attempt to prepend a prefix.
**       All namespaces that are used will have a prefix assigned to
**       them -- mod_dav will never set or use the default namespace
**       when generating XML. This means that "no prefix" will always
**       mean "no namespace".
**
**    In both cases, the XML generation will avoid prepending a prefix.
**    For the first case, this means the original prefix/name will be
**    inserted into the output stream. For the latter case, it means
**    the name will have no prefix, and since we never define a default
**    namespace, this means it will have no namespace.
**
** Note: currently, mod_dav understands the "xmlns" prefix and the
**     "xml:lang" attribute. These are handled specially (they aren't
**     left within the XML tree), so the APR_XML_NS_NONE value won't ever
**     really apply to these values.
}
const
  APR_XML_NS_DAV_ID	= 0;	{< namespace ID for "DAV:" }
  APR_XML_NS_NONE	= -10;	{< no namespace for this elem/attr }

  APR_XML_NS_ERROR_BASE	= -100;	{< used only during processing }
{ Is this namespace an error? }
//  APR_XML_NS_IS_ERROR(e)	((e) <= APR_XML_NS_ERROR_BASE)

type
{ @see apr_xml_attr }
  Papr_xml_attr = ^apr_xml_attr;
{ @see apr_xml_elem }
  Papr_xml_elem = ^apr_xml_elem;
{ @see apr_xml_doc }
  Papr_xml_doc = ^apr_xml_doc;
  PPapr_xml_doc = ^Papr_xml_doc;

{ apr_xml_attr: holds a parsed XML attribute }
  apr_xml_attr = record
    { attribute name }
    name: PChar;
    { index into namespace array }
    ns: Integer;

    { attribute value }
    value: PChar;

    { next attribute }
    next: Papr_xml_attr;
  end;

{ apr_xml_elem: holds a parsed XML element }
  apr_xml_elem = record
    { element name }
    name: PChar;
    { index into namespace array }
    ns: Integer;
    { xml:lang for attrs/contents }
    lang: PChar;

    { cdata right after start tag }
    first_cdata: apr_text_header;
    { cdata after MY end tag }
    following_cdata: apr_text_header;

    { parent element }
    parent: Papr_xml_elem;
    { next (sibling) element }
    next: Papr_xml_elem;
    { first child element }
    first_child: Papr_xml_elem;
    { first attribute }
    attr: Papr_xml_attr;

    { used only during parsing }
    { last child element }
    last_child: Papr_xml_elem;
    { namespaces scoped by this elem }
    ns_scope: Papr_xml_ns_scope;

    { used by modules during request processing }
    { Place for modules to store private data }
    priv: Pointer;
  end;

{ Is this XML element empty? }
//#define APR_XML_ELEM_IS_EMPTY(e) ((e)->first_child == NULL && \
//                                  (e)->first_cdata.first == NULL)

{ apr_xml_doc: holds a parsed XML document }
  apr_xml_doc = record
    { root element }
    root: Papr_xml_elem;
    { array of namespaces used }
    namespaces: Papr_array_header_t;
  end;

{ Opaque XML parser structure }
  apr_xml_parser = record end;
  Papr_xml_parser = ^apr_xml_parser;
  PPapr_xml_parser = ^Papr_xml_parser;

{
 * Create an XML parser
 * @param pool The pool for allocating the parser and the parse results.
 * @return The new parser.
 }
function apr_xml_parser_create(pool: Papr_pool_t): Papr_xml_parser;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xml_parser_create' + LibSuff4;

{
 * Parse a File, producing a xml_doc
 * @param p      The pool for allocating the parse results.
 * @param parser A pointer to *parser (needed so calling function can get
 *               errors), will be set to NULL on successfull completion.
 * @param ppdoc  A pointer to *apr_xml_doc (which has the parsed results in it)
 * @param xmlfd  A file to read from.
 * @param buffer_length Buffer length which would be suitable 
 * @return Any errors found during parsing.
 }
function apr_xml_parse_file(p: Papr_pool_t;
 parser: PPapr_xml_parser; ppdoc: PPapr_xml_doc;
 xmlfd: Papr_file_t; buffer_length: apr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xml_parse_file' + LibSuff20;

{
 * Feed input into the parser
 * @param parser The XML parser for parsing this data.
 * @param data The data to parse.
 * @param len The length of the data.
 * @return Any errors found during parsing.
 * @remark Use apr_xml_parser_geterror() to get more error information.
 }
function apr_xml_parser_feed(parser: Papr_xml_parser;
 const data: PChar; len: apr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xml_parser_feed' + LibSuff12;

{
 * Terminate the parsing and return the result
 * @param parser The XML parser for parsing this data.
 * @param pdoc The resulting parse information. May be NULL to simply
 *             terminate the parsing without fetching the info.
 * @return Any errors found during the final stage of parsing.
 * @remark Use apr_xml_parser_geterror() to get more error information.
 }
function apr_xml_parser_done(parser: Papr_xml_parser;
 pdoc: PPapr_xml_doc): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xml_parser_done' + LibSuff8;

{
 * Fetch additional error information from the parser.
 * @param parser The XML parser to query for errors.
 * @param errbuf A buffer for storing error text.
 * @param errbufsize The length of the error text buffer.
 * @return The error buffer
 }
function apr_xml_parser_geterror(parser: Papr_xml_parser;
 errbuf: PChar; errbufsize: apr_size_t): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xml_parser_geterror' + LibSuff12;

{
 * Converts an XML element tree to flat text 
 * @param p The pool to allocate out of
 * @param elem The XML element to convert
 * @param style How to covert the XML.  One of:
 * <PRE>
 *     APR_XML_X2T_FULL                start tag, contents, end tag 
 *     APR_XML_X2T_INNER               contents only 
 *     APR_XML_X2T_LANG_INNER          xml:lang + inner contents 
 *     APR_XML_X2T_FULL_NS_LANG        FULL + ns defns + xml:lang 
 * </PRE>
 * @param namespaces The namespace of the current XML element
 * @param ns_map Namespace mapping
 * @param pbuf Buffer to put the converted text into
 * @param psize Size of the converted text
 }
procedure apr_xml_to_text(p: Papr_pool_t; const elem: Papr_xml_elem;
 style: Integer; namespaces: Papr_array_header_t;
 ns_map: PInteger; const pbuf: PPChar; psize: Papr_size_t);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xml_to_text' + LibSuff28;

{ style argument values: }
const
  APR_XML_X2T_FULL         = 0;	{< start tag, contents, end tag }
  APR_XML_X2T_INNER        = 1;	{< contents only }
  APR_XML_X2T_LANG_INNER   = 2;	{< xml:lang + inner contents }
  APR_XML_X2T_FULL_NS_LANG = 3;	{< FULL + ns defns + xml:lang }

{
 * empty XML element
 * @param p The pool to allocate out of
 * @param elem The XML element to empty
 * @return the string that was stored in the XML element
 }
function apr_xml_empty_elem(p: Papr_pool_t; const elem: Papr_xml_elem): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xml_empty_elem' + LibSuff8;

{
 * quote an XML string
 * Replace '<', '>', and '&' with '&lt;', '&gt;', and '&amp;'.
 * @param p The pool to allocate out of
 * @param s The string to quote
 * @param quotes If quotes is true, then replace '"' with '&quot;'.
 * @return The quoted string
 * @note If the string does not contain special characters, it is not
 * duplicated into the pool and the original string is returned.
 }
function apr_xml_quote_string(p: Papr_pool_t; const s: PChar;
 quotes: Integer): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xml_quote_string' + LibSuff12;

{
 * Quote an XML element
 * @param p The pool to allocate out of
 * @param elem The element to quote
 }
procedure apr_xml_quote_elem(p: Papr_pool_t; elem: Papr_xml_elem);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xml_quote_elem' + LibSuff8;

{ manage an array of unique URIs: apr_xml_insert_uri() and APR_XML_URI_ITEM() }

{
 * return the URI's (existing) index, or insert it and return a new index 
 * @param uri_array array to insert into
 * @param uri The uri to insert
 * @return int The uri's index
 }
function apr_xml_insert_uri(uri_array: Papr_array_header_t;
 const uri: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xml_insert_uri' + LibSuff8;

{ Get the URI item for this XML element }
//#define APR_XML_GET_URI_ITEM(ary, i) (((const char * const *)(ary)->elts)[i])

