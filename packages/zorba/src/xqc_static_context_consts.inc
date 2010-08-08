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


type

(** \brief XPath 1.0 compatibility mode as defined in
 *         http://www.w3.org/TR/xquery/#static_context
 *)
  xpath1_0compatib_mode_t = (xpath2_0, xpath1_0);

(** \brief Ordering mode as defined in http://www.w3.org/TR/xquery/#static_context.
 *)
  ordering_mode_t = (ordered, unordered);

(** \brief Default order for empty sequences as defined in
 *         http://www.w3.org/TR/xquery/#static_context.
 *)
  order_empty_mode_t = (empty_greatest, empty_least);

(** \brief Inherit part of the Copy-namespace mode as defined in
 *         http://www.w3.org/TR/xquery/#static_context.
 *)
  inherit_mode_t  = (inherit_ns, no_inherit_ns);

(** \brief Preserve part of the Copy-namespace mode as defined in
 *         http://www.w3.org/TR/xquery/#static_context.
 *)
  preserve_mode_t = (preserve_ns, no_preserve_ns);

(** \brief Boundary-space policy as defined in http://www.w3.org/TR/xquery/#static_context.
 *)
  boundary_space_mode_t = (preserve_space, strip_space);

(** \brief Construction mode as defined in http://www.w3.org/TR/xquery/#static_context.
 *)
  construction_mode_t = (preserve_cons, strip_cons);

(** \brief The XQuery version, taken from the version declaration, an XQuery 1.1 feature.
 *)
  xquery_version_t = (xquery_version_1_0 = 100, xquery_version_1_1 = 110);

(** \brief Revalidation mode as defined in
 *         http://www.w3.org/TR/xquery-update-10/#id-revalidation-declaration
 *)
  validation_mode_t = (validate_skip, validate_lax, validate_strict);

