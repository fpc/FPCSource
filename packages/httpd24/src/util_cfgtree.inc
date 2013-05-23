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
 * @file  util_cfgtree.h
 * @brief Config Tree Package
 *
 * @defgroup APACHE_CORE_CONFIG_TREE Config Tree Package
 * @ingroup  APACHE_CORE_CONFIG
 * @{
 *)

{$ifndef AP_CONFTREE_H}
  {$define AP_CONFTREE_H}

//#include "ap_config.h"


type
  Pap_directive_t = ^ap_directive_t;
  PPap_directive_t = ^Pap_directive_t;

{**
 * @brief Structure used to build the config tree.
 *
 * The config tree only stores
 * the directives that will be active in the running server.  Directives
 * that contain other directions, such as &lt;Directory ...&gt; cause a sub-level
 * to be created, where the included directives are stored.  The closing
 * directive (&lt;/Directory&gt;) is not stored in the tree.
 *}
ap_directive_t = record
    {** The current directive *}
    directive: PChar;
    {** The arguments for the current directive, stored as a space
     *  separated list *}
    args: PChar;
    {** The next directive node in the tree *}
    next: Pap_directive_t;
    {** The first child node of this directive *}
    first_child: Pap_directive_t;
    {** The parent node of this directive *}
    parent: Pap_directive_t;

    {** directive's module can store add'l data here *}
    data: Pointer;

    {* ### these may go away in the future, but are needed for now *}
    {** The name of the file this directive was found in *}
    filename: PChar;
    {** The line number the directive was on *}
    line_num: Integer;

    {** A short-cut towards the last directive node in the tree.
     *  The value may not always be up-to-date but it always points to
     *  somewhere in the tree, nearer to the tail.
     *  This value is only set in the first node
     *}
    last: Pap_directive_t;
end;

{**
 * The root of the configuration tree
 *}
//AP_DECLARE_DATA extern ap_directive_t *ap_conftree;

{**
 * Add a node to the configuration tree.
 * @param parent The current parent node.  If the added node is a first_child,
                 then this is changed to the current node
 * @param current The current node
 * @param toadd The node to add to the tree
 * @param child Is the node to add a child node
 * @return the added node
 *}
//ap_directive_t *ap_add_node(ap_directive_t **parent, ap_directive_t *current,
//                            ap_directive_t *toadd, int child);


{$endif}
(** @} *)
