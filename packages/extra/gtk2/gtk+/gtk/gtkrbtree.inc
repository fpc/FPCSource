// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkRBNodeColor = ^TGtkRBNodeColor;
   TGtkRBNodeColor = longint;

   PGtkRBTree = ^TGtkRBTree;
   PGtkRBNode = ^TGtkRBNode;

   TGtkRBTreeTraverseFunc = procedure (tree:PGtkRBTree; node:PGtkRBNode; data:gpointer); cdecl;
   TGtkRBTree = record
        root : PGtkRBNode;
        _nil : PGtkRBNode;
        parent_tree : PGtkRBTree;
        parent_node : PGtkRBNode;
     end;

{ We keep track of whether the aggregate count of children plus 1
     for the node itself comes to an even number.  The parity flag is
     the total count of children mod 2, where the total count of
     children gets computed in the same way that the total offset gets
     computed. i.e. not the same as the "count" field below which
     doesn't include children. We could replace parity with a
     full-size int field here, and then take % 2 to get the parity flag,
     but that would use extra memory.
    }
{ count is the number of nodes beneath us, plus 1 for ourselves.
     i.e. node->left->count + node->right->count + 1
    }
{ this is the total of sizes of
     node->left, node->right, our own height, and the height
     of all trees in ->children, iff children exists because
     the thing is expanded.
    }
{ Child trees  }
   TGtkRBNode = record
        flag0 : word;
        left : PGtkRBNode;
        right : PGtkRBNode;
        parent : PGtkRBNode;
        count : gint;
        offset : gint;
        children : PGtkRBTree;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   GTK_RBNODE_BLACK = 1 shl 0;
   GTK_RBNODE_RED = 1 shl 1;
   GTK_RBNODE_IS_PARENT = 1 shl 2;
   GTK_RBNODE_IS_SELECTED = 1 shl 3;
   GTK_RBNODE_IS_PRELIT = 1 shl 4;
   GTK_RBNODE_IS_SEMI_COLLAPSED = 1 shl 5;
   GTK_RBNODE_IS_SEMI_EXPANDED = 1 shl 6;
   GTK_RBNODE_INVALID = 1 shl 7;
   GTK_RBNODE_COLUMN_INVALID = 1 shl 8;
   GTK_RBNODE_DESCENDANTS_INVALID = 1 shl 9;
   GTK_RBNODE_NON_COLORS = GTK_RBNODE_IS_PARENT
                         or GTK_RBNODE_IS_SELECTED
                         or GTK_RBNODE_IS_PRELIT
                         or GTK_RBNODE_IS_SEMI_COLLAPSED
                         or GTK_RBNODE_IS_SEMI_EXPANDED
                         or GTK_RBNODE_INVALID
                         or GTK_RBNODE_COLUMN_INVALID
                         or GTK_RBNODE_DESCENDANTS_INVALID;

const
   bm_TGtkRBNode_flags = $3FFF;
   bp_TGtkRBNode_flags = 0;
   bm_TGtkRBNode_parity = $4000;
   bp_TGtkRBNode_parity = 14;

function flags(a : PGtkRBNode) : guint;
procedure set_flags(a : PGtkRBNode; __flags : guint);
function parity(a : PGtkRBNode) : guint;
procedure set_parity(a : PGtkRBNode; __parity : guint);
function GTK_RBNODE_GET_COLOR(node: PGtkRBNode) : guint;
procedure GTK_RBNODE_SET_COLOR(node: PGtkRBNode; color: guint);
function GTK_RBNODE_GET_HEIGHT(node: PGtkRBNode) : gint;

procedure GTK_RBNODE_SET_FLAG(node: PGtkRBNode; flag: word);
procedure GTK_RBNODE_UNSET_FLAG(node: PGtkRBNode; flag: word);
function GTK_RBNODE_FLAG_SET(node: PGtkRBNode; flag : guint) : boolean;

procedure _gtk_rbtree_push_allocator(allocator:PGAllocator); cdecl; external gtklib;
procedure _gtk_rbtree_pop_allocator; cdecl; external gtklib;
function _gtk_rbtree_new:PGtkRBTree; cdecl; external gtklib;
procedure _gtk_rbtree_free(tree:PGtkRBTree); cdecl; external gtklib;
procedure _gtk_rbtree_remove(tree:PGtkRBTree); cdecl; external gtklib;
procedure _gtk_rbtree_destroy(tree:PGtkRBTree); cdecl; external gtklib;
function _gtk_rbtree_insert_before(tree:PGtkRBTree; node:PGtkRBNode; height:gint; valid:gboolean):PGtkRBNode; cdecl; external gtklib;
function _gtk_rbtree_insert_after(tree:PGtkRBTree; node:PGtkRBNode; height:gint; valid:gboolean):PGtkRBNode; cdecl; external gtklib;
procedure _gtk_rbtree_remove_node(tree:PGtkRBTree; node:PGtkRBNode); cdecl; external gtklib;
procedure _gtk_rbtree_reorder(tree:PGtkRBTree; new_order:Pgint; length:gint); cdecl; external gtklib;
function _gtk_rbtree_find_count(tree:PGtkRBTree; count:gint):PGtkRBNode; cdecl; external gtklib;
procedure _gtk_rbtree_node_set_height(tree:PGtkRBTree; node:PGtkRBNode; height:gint); cdecl; external gtklib;
procedure _gtk_rbtree_node_mark_invalid(tree:PGtkRBTree; node:PGtkRBNode); cdecl; external gtklib;
procedure _gtk_rbtree_node_mark_valid(tree:PGtkRBTree; node:PGtkRBNode); cdecl; external gtklib;
procedure _gtk_rbtree_column_invalid(tree:PGtkRBTree); cdecl; external gtklib;
procedure _gtk_rbtree_mark_invalid(tree:PGtkRBTree); cdecl; external gtklib;
procedure _gtk_rbtree_set_fixed_height(tree:PGtkRBTree; height:gint); cdecl; external gtklib;
function _gtk_rbtree_node_find_offset(tree:PGtkRBTree; node:PGtkRBNode):gint; cdecl; external gtklib;
function _gtk_rbtree_node_find_parity(tree:PGtkRBTree; node:PGtkRBNode):gint; cdecl; external gtklib;
function _gtk_rbtree_find_offset(tree:PGtkRBTree; offset:gint; var new_tree:PGtkRBTree; var new_node:PGtkRBNode):gint; cdecl; external gtklib;
procedure _gtk_rbtree_traverse(tree:PGtkRBTree; node:PGtkRBNode; order:TGTraverseType; func:TGtkRBTreeTraverseFunc; data:gpointer); cdecl; external gtklib;
function _gtk_rbtree_next(tree:PGtkRBTree; node:PGtkRBNode):PGtkRBNode; cdecl; external gtklib;
function _gtk_rbtree_prev(tree:PGtkRBTree; node:PGtkRBNode):PGtkRBNode; cdecl; external gtklib;
procedure _gtk_rbtree_next_full(tree:PGtkRBTree; node:PGtkRBNode; var new_tree:PGtkRBTree; var new_node:PGtkRBNode); cdecl; external gtklib;
procedure _gtk_rbtree_prev_full(tree:PGtkRBTree; node:PGtkRBNode; var new_tree:PGtkRBTree; var new_node:PGtkRBNode); cdecl; external gtklib;
function _gtk_rbtree_get_depth(tree:PGtkRBTree):gint; cdecl; external gtklib;
{ This func checks the integrity of the tree  }
{$ifdef G_ENABLE_DEBUG  }
procedure _gtk_rbtree_test(where:Pgchar; tree:PGtkRBTree); cdecl; external gtklib;
procedure _gtk_rbtree_debug_spew(tree:PGtkRBTree); cdecl; external gtklib;
{$endif}
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function flags(a : PGtkRBNode) : guint;
begin
   flags:=(a^.flag0 and bm_TGtkRBNode_flags) shr bp_TGtkRBNode_flags;
end;

procedure set_flags(a : PGtkRBNode; __flags : guint);
begin
   a^.flag0:=a^.flag0 or ((__flags shl bp_TGtkRBNode_flags) and bm_TGtkRBNode_flags);
end;

function parity(a : PGtkRBNode) : guint;
begin
   parity:=(a^.flag0 and bm_TGtkRBNode_parity) shr bp_TGtkRBNode_parity;
end;

procedure set_parity(a : PGtkRBNode; __parity : guint);
begin
   a^.flag0:=a^.flag0 or ((__parity shl bp_TGtkRBNode_parity) and bm_TGtkRBNode_parity);
end;

function GTK_RBNODE_GET_COLOR(node : PGtkRBNode) : guint;
begin
  if node=nil then
    Result:=GTK_RBNODE_BLACK
  else
  if (flags(node) and GTK_RBNODE_RED) = GTK_RBNODE_RED then
    Result:=GTK_RBNODE_RED
  else
    Result:=GTK_RBNODE_BLACK;
end;

procedure GTK_RBNODE_SET_COLOR(node : PGtkRBNode; color: guint);
begin
  if node=nil then exit;
  if ((flags(node) and color)<>color) then
    set_flags(node,flags(node) xor (GTK_RBNODE_RED or GTK_RBNODE_BLACK));
end;

function GTK_RBNODE_GET_HEIGHT(node : PGtkRBNode) : gint;
var
   if_local1 : gint;
begin
   if node^.children<>nil then
     if_local1:=node^.children^.root^.offset
   else
     if_local1:=0;
   GTK_RBNODE_GET_HEIGHT:=node^.offset -
     (node^.left^.offset + node^.right^.offset + if_local1);
end;

function GTK_RBNODE_FLAG_SET(node: PGtkRBNode; flag : guint) : boolean;
begin
   GTK_RBNODE_FLAG_SET:=(node<>nil) and ((flags(node) and flag) = flag);
end;

procedure GTK_RBNODE_SET_FLAG(node: PGtkRBNode; flag: word);
begin
  set_flags(node,flag or flags(node));
end;

procedure GTK_RBNODE_UNSET_FLAG(node: PGtkRBNode; flag: word);
begin
  set_flags(node,(not flag) and flags(node));
end;


{$ENDIF read_implementation}
// included by gtk2.pas

