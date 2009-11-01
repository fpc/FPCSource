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


(** \brief The optimization level used for optimizing the query. *)
  Zorba_opt_level_t = (
    ZORBA_OPT_LEVEL_O0, (**< Don't use any optimization. *)
    ZORBA_OPT_LEVEL_O1  (**< Use basic optimizations
                             (e.g.\ removing sorting, removing duplicate elimination,
                             or constant folding). *)
  );

(** \brief Set of hints that can be passed to the query compiler.
 *
 * An instance of this class can be passed to the compileQuery function
 * of the Zorba class or the compile function of this class.
 * The members of this class represent hints that are passed to the
 * query compiler. For example, whether optimization of the query
 * should be done (O1) or not (O0).
 *
 * example_6 in file \link simple.cpp \endlink shows an example
 * how CompilerHints can be used.
 *)
  pZorba_CompilerHints_t = ^Zorba_CompilerHints_t;
  Zorba_CompilerHints_t = record
  (** \brief The optimization level that is used *)
    opt_level: Zorba_opt_level_t;
  (** \brief Treat the query as a library module *)
    lib_module: cbool;
  end;

  Zorba_serialization_method_t = (
    ZORBA_SERIALIZATION_METHOD_XML,
    ZORBA_SERIALIZATION_METHOD_HTML,
    ZORBA_SERIALIZATION_METHOD_XHTML,
    ZORBA_SERIALIZATION_METHOD_TEXT,
    ZORBA_SERIALIZATION_METHOD_JSON,
    ZORBA_SERIALIZATION_METHOD_JSONML
  );

  Zorba_byte_order_mark_t = (
    ZORBA_BYTE_ORDER_MARK_YES,
    ZORBA_BYTE_ORDER_MARK_NO
  );

  Zorba_escape_uri_attributes_t = (
    ZORBA_ESCAPE_URI_ATTRIBUTES_YES,
    ZORBA_ESCAPE_URI_ATTRIBUTES_NO
  );

  Zorba_include_content_type_t = (
    ZORBA_INCLUDE_CONTENT_TYPE_YES,
    ZORBA_INCLUDE_CONTENT_TYPE_NO
  );

  Zorba_indent_t = (
    ZORBA_INDENT_YES,
    ZORBA_INDENT_NO
  );

  Zorba_normalization_form_t = (
    ZORBA_NORMALIZATION_FORM_NFC,
    ZORBA_NORMALIZATION_FORM_NFD,
    ZORBA_NORMALIZATION_FORM_NFKC,
    ZORBA_NORMALIZATION_FORM_NFKD,
    ZORBA_NORMALIZATION_FORM_FULLY_normalized,
    ZORBA_NORMALIZATION_FORM_NONE
  );

  Zorba_omit_xml_declaration_t = (
    ZORBA_OMIT_XML_DECLARATION_YES,
    ZORBA_OMIT_XML_DECLARATION_NO
  );

  Zorba_standalone_t = (
    ZORBA_STANDALONE_YES,
    ZORBA_STANDALONE_NO,
    ZORBA_STANDALONE_OMIT
  );

  Zorba_undeclare_prefixes_t = (
    ZORBA_UNDECLARE_PREFIXES_YES,
    ZORBA_UNDECLARE_PREFIXES_NO
  );

  Zorba_binary_plan_format_t = (
    ZORBA_USE_BINARY_ARCHIVE,
    ZORBA_USE_XML_ARCHIVE
  );

  pZorba_SerializerOptions_t = ^Zorba_SerializerOptions_t;
  Zorba_SerializerOptions_t = record end;


(** \brief Helper function for C to set default values ComplilerHints struct.
 *
 * \retval Zorba_CompilerHints_t with default member values
 *)
{$IFDEF S}procedure{$ELSE}var{$ENDIF}Zorba_CompilerHints_default{$IFDEF D}: procedure{$ENDIF}
  (p: pZorba_CompilerHints_t); extdecl;{$IFDEF S}external zorbalib;{$ENDIF}


(** \brief Helper function to create a Zorba_SerializerOptions_t struct because
 *         of missing default constructor. C++ code can delete the
 *         returned Zorba_SerializerOptions_t* struct, while C code
 *         must call Zorba_SerializerOptions_free().
 *
 * \retval Zorba_CompilerHints_t with default member values
 *)
{$IFDEF S}function{$ELSE}var{$ENDIF}Zorba_SerializerOptions_default{$IFDEF D}: function{$ENDIF}:
  pZorba_SerializerOptions_t; extdecl;{$IFDEF S}external zorbalib;{$ENDIF}


(** \brief Helper function to delete a Zorba_SerializerOptions_t struct
 *
 * \retval Zorba_CompilerHints_t with default member values
 *)
{$IFDEF S}procedure{$ELSE}var{$ENDIF}Zorba_SerializerOptions_free{$IFDEF D}: procedure{$ENDIF}
  (serializerOptions: pZorba_SerializerOptions_t); extdecl;{$IFDEF S}external zorbalib;{$ENDIF}


(** \brief Helper function to set an option in a Zorba_SerializerOptions_t structure
 *
 * \param parameter the serializer parameter to be configured
 * \param value the value to which the parameter should be set
 * \retval Zorba_CompilerHints_t with default member values
 *)
{$IFDEF S}procedure{$ELSE}var{$ENDIF}Zorba_SerializerOptions_set{$IFDEF D}: procedure{$ENDIF}
  (serializerOptions: pZorba_SerializerOptions_t; const parameter: pcchar; const value: pcchar); extdecl;{$IFDEF S}external zorbalib;{$ENDIF}
