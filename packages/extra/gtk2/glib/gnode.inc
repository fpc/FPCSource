{*
 * gnode.inc
 *
 * depends on gmem.inc
 *}


  type
     PGNode = ^TGNode;
     TGNode = record
          data     : gpointer;
          next     : PGNode;
          prev     : PGNode;
          parent   : PGNode;
          children : PGNode;
       end;

  { Tree traverse flags  }

     PGTraverseFlags = ^TGTraverseFlags;
     TGTraverseFlags = gint;

  const
     G_TRAVERSE_LEAFS      = 1 shl 0;
     G_TRAVERSE_NON_LEAFS  = 1 shl 1;
     G_TRAVERSE_ALL        = G_TRAVERSE_LEAFS or G_TRAVERSE_NON_LEAFS;
     G_TRAVERSE_MASK       = $03;

    { Tree traverse orders  }
    type

       PGTraverseType = ^TGTraverseType;
       TGTraverseType = (G_IN_ORDER,
                         G_PRE_ORDER,
                         G_POST_ORDER,
                         G_LEVEL_ORDER);

       TGNodeTraverseFunc = function (node:PGNode; data:gpointer):gboolean;cdecl;

       TGNodeForeachFunc = procedure (node:PGNode; data:gpointer);cdecl;

    { N-way tree implementation
      }

    function G_NODE_IS_ROOT (node: PGNode): boolean;

    function G_NODE_IS_LEAF (node: PGNode): boolean;

    procedure g_node_push_allocator(allocator:PGAllocator);cdecl;external gliblib name 'g_node_push_allocator';

    procedure g_node_pop_allocator;cdecl;external gliblib name 'g_node_pop_allocator';

    function  g_node_new(data:gpointer):PGNode;cdecl;external gliblib name 'g_node_new';

    procedure g_node_destroy(root:PGNode);cdecl;external gliblib name 'g_node_destroy';

    procedure g_node_unlink(node:PGNode);cdecl;external gliblib name 'g_node_unlink';

    function  g_node_copy(node:PGNode):PGNode;cdecl;external gliblib name 'g_node_copy';

    function  g_node_insert(parent:PGNode; position:gint; node:PGNode):PGNode;cdecl;external gliblib name 'g_node_insert';

    function  g_node_insert_before(parent:PGNode; sibling:PGNode; node:PGNode):PGNode;cdecl;external gliblib name 'g_node_insert_before';

    function  g_node_insert_after(parent:PGNode; sibling:PGNode; node:PGNode):PGNode;cdecl;external gliblib name 'g_node_insert_after';

    function  g_node_prepend(parent:PGNode; node:PGNode):PGNode;cdecl;external gliblib name 'g_node_prepend';

    function  g_node_n_nodes(root:PGNode; flags:TGTraverseFlags):guint;cdecl;external gliblib name 'g_node_n_nodes';

    function  g_node_get_root(node:PGNode):PGNode;cdecl;external gliblib name 'g_node_get_root';

    function  g_node_is_ancestor(node:PGNode; descendant:PGNode):gboolean;cdecl;external gliblib name 'g_node_is_ancestor';

    function g_node_depth(node:PGNode):guint;cdecl;external gliblib name 'g_node_depth';

    function g_node_find(root:PGNode; order:TGTraverseType; flags:TGTraverseFlags; data:gpointer):PGNode;cdecl;external gliblib name 'g_node_find';

    { convenience macros  }
    function  g_node_append (parent: PGNode; node: PGNode): PGNode;

    function  g_node_insert_data (parent: PGNode; position: gint; data: gpointer): PGNode;

    function  g_node_insert_data_before (parent: PGNode; sibling: PGNode; data: gpointer): PGNode;

    function  g_node_prepend_data (parent: PGNode; data: gpointer): PGNode;

    function  g_node_append_data (parent: PGNode; data: gpointer): PGNode;

    { traversal function, assumes that `node' is root
       (only traverses `node' and its subtree).
       this function is just a high level interface to
       low level traversal functions, optimized for speed.
      }
    function g_node_traverse (root      : PGNode;
                              order     : TGTraverseType;
                              flags     : TGTraverseFlags;
                              max_depth : gint;
                              func      : TGNodeTraverseFunc;
                              data      : gpointer):guint;cdecl;external gliblib name 'g_node_traverse';

     { return the maximum tree height starting with `node', this is an expensive
       operation, since we need to visit all nodes. this could be shortened by
       adding `guint height' to struct _GNode, but then again, this is not very
       often needed, and would make g_node_insert() more time consuming.
      }
    function  g_node_max_height(root:PGNode):guint;cdecl;external gliblib name 'g_node_max_height';

    procedure g_node_children_foreach(node:PGNode; flags:TGTraverseFlags; func:TGNodeForeachFunc; data:gpointer);cdecl;external gliblib name 'g_node_children_foreach';

    procedure g_node_reverse_children(node:PGNode);cdecl;external gliblib name 'g_node_reverse_children';

    function g_node_n_children(node:PGNode):guint;cdecl;external gliblib name 'g_node_n_children';

    function g_node_nth_child(node:PGNode; n:guint):PGNode;cdecl;external gliblib name 'g_node_nth_child';

    function g_node_last_child(node:PGNode):PGNode;cdecl;external gliblib name 'g_node_last_child';

    function g_node_find_child(node:PGNode; flags:TGTraverseFlags; data:gpointer):PGNode;cdecl;external gliblib name 'g_node_find_child';

    function g_node_child_position(node:PGNode; child:PGNode):gint;cdecl;external gliblib name 'g_node_child_position';

    function g_node_child_index(node:PGNode; data:gpointer):gint;cdecl;external gliblib name 'g_node_child_index';

    function g_node_first_sibling(node:PGNode):PGNode;cdecl;external gliblib name 'g_node_first_sibling';

    function g_node_last_sibling(node:PGNode):PGNode;cdecl;external gliblib name 'g_node_last_sibling';

    function g_node_prev_sibling (node : PGNode): PGNode;

    function g_node_next_sibling (node : PGNode): PGNode;

    function g_node_first_child (node : PGNode): PGNode;

