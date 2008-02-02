(* Stock Item and Icon Browser
 *
 * This source code for this demo doesn't demonstrate anything
 * particularly useful in applications. The purpose of the "demo" is
 * just to provide a handy place to browse the available stock icons
 * and stock items.
 *)

var
  stock_window : PGtkWidget;

type
  PStockItemInfo = ^TStockItemInfo;
  TStockItemInfo = record
                     id         : pgchar;
                     item       : TGtkStockItem;
                     small_icon : PGdkPixbuf;
                     macro      : pgchar;
                     accel_str  : pgchar;
                   end;


(* Make StockItemInfo a boxed type so we can automatically
 * manage memory
 *)

procedure stock_item_info_free (info : PStockItemInfo); cdecl;
begin
  g_free (info^.id);
  g_free (info^.macro);
  g_free (info^.accel_str);
  if info^.small_icon <> NULL then
    g_object_unref (pGObject(info^.small_icon));

  g_free (info);
end;

function stock_item_info_copy (src  : PStockItemInfo): PStockItemInfo; cdecl;
var
  info : PStockItemInfo;

begin
  info := g_malloc0 (sizeof(TStockItemInfo));
  info^.id := g_strdup (src^.id);
  info^.macro := g_strdup (src^.macro);
  info^.accel_str := g_strdup (src^.accel_str);

  info^.item := src^.item;

  info^.small_icon := src^.small_icon;
  if info^.small_icon <> NULL then
    g_object_ref (pGObject(info^.small_icon));

  stock_item_info_copy := info;
end;

var
  StockItemInfoType : GType;

function stock_item_info_get_type: GType;
begin

  if StockItemInfoType = 0 then
    StockItemInfoType := g_boxed_type_register_static ('StockItemInfo',
                                             TGBoxedCopyFunc(@stock_item_info_copy),
                                             TGBoxedFreeFunc (@stock_item_info_free));

  stock_item_info_get_type := StockItemInfoType;
end;

function STOCK_ITEM_INFO_TYPE: GType;
begin
  STOCK_ITEM_INFO_TYPE := stock_item_info_get_type;
end;


type
  PStockItemDisplay = ^TStockItemDisplay;
  TStockItemDisplay = record
                        type_label        : PGtkWidget;
                        macro_label       : PGtkWidget;
                        id_label          : PGtkWidget;
                        label_accel_label : PGtkWidget;
                        icon_image        : PGtkWidget;
                      end;


function id_to_macro (id : pgchar): pgchar;
var
  macro : PGString;
  cp    : pgchar;

begin
  (* gtk-foo-bar -> GTK_STOCK_FOO_BAR *)

  macro := g_string_new (NULL);
  cp := id;

  if StrLComp (cp, 'gtk-', 4) = 0 then
  begin
    g_string_append (macro, 'GTK_STOCK_');
    cp := cp + 4;
  end;

  while cp[0] <> #0 do
  begin
    if cp[0] = '-' then
      g_string_append_c (macro, '_')
    else if g_ascii_islower (cp[0]) then
        g_string_append_c (macro, g_ascii_toupper (cp[0]))
      else
        g_string_append_c (macro, cp[0]);

      inc(cp);
  end;

  id_to_macro := g_string_free (macro, FALSE);
end;

function CompareString (string1 : pgchar;
                        string2 : pgchar): gint; cdecl;
begin
  CompareString := StrComp (string1, string2);  // maped to strcomp
end;


function create_model : PGtkTreeModel;
var
  store       : PGtkListStore;
  ids,
  tmp_list    : PGSList;

  info        : TStockItemInfo;
  item        : TGtkStockItem;
  iter        : TGtkTreeIter;
  icon_set    : PGtkIconSet;

  sizes       : PGtkIconSize;
  i, n_sizes  : gint;
  size        : TGtkIconSize;

  w, h        : gint;
  scaled      : PGdkPixbuf;

begin
  store := gtk_list_store_new (1, [STOCK_ITEM_INFO_TYPE]);

  ids := gtk_stock_list_ids ();
  ids := g_slist_sort (ids, TGCompareFunc (@CompareString));
  tmp_list := ids;

  while tmp_list <> NULL do
  begin
    info.id := tmp_list^.data;

    if gtk_stock_lookup (info.id, @item) then
      info.item := item
    else begin
      info.item._label    := NULL;
      info.item.stock_id := NULL;
      info.item.modifier := 0;
      info.item.keyval   := 0;
      info.item.translation_domain := NULL;
    end;

    (* only show icons for stock IDs that have default icons *)
    icon_set := gtk_icon_factory_lookup_default (info.id);
    if icon_set <> NULL then
    begin
      sizes := NULL;
      n_sizes := 0;

      (* See what sizes this stock icon really exists at *)
      gtk_icon_set_get_sizes (icon_set, @sizes, @n_sizes);

      (* Use menu size if it exists, otherwise first size found *)
      size := sizes[0];
      i := 0;
      while i < n_sizes do
      begin
        if sizes[i] = GTK_ICON_SIZE_MENU then
        begin
          size := GTK_ICON_SIZE_MENU;
          break;
        end; {of size[i] = GTK_ICON_SIZE_MENU}
        inc(i);
      end; {of while}

      g_free (sizes);

      info.small_icon := gtk_widget_render_icon (stock_window, info.id, size, NULL);

      if size <> GTK_ICON_SIZE_MENU then
      begin
        (* Make the result the proper size for our thumbnail *)
        gtk_icon_size_lookup (GTK_ICON_SIZE_MENU, @w, @h);

        scaled := gdk_pixbuf_scale_simple (info.small_icon,
                                           w, h,
                                           GDK_INTERP_BILINEAR);

        g_object_unref (pGObject(info.small_icon));
        info.small_icon := scaled;
      end; {of size <> GTK_ICON_SIZE_MENU}
    end else {icon_set = NULL}
      info.small_icon := NULL;

    if info.item.keyval <> 0 then

      info.accel_str := gtk_accelerator_name (info.item.keyval,
                                                 info.item.modifier)

    else
      info.accel_str := g_strdup ('');


    info.macro := id_to_macro (info.id);

    gtk_list_store_append (store, @iter);
    gtk_list_store_set (store, @iter, [0, @info, -1]);

    g_free (info.macro);
    g_free (info.accel_str);

    if info.small_icon <> NULL then
      g_object_unref (pGObject(info.small_icon));

    tmp_list := g_slist_next (tmp_list);
  end; {of while tmp_list <> NULL}

  g_slist_foreach (ids, TGFunc(@g_free), NULL);
  g_slist_free (ids);

  create_model := pGtkTreeModel(store);
end;

(* Finds the largest size at which the given image stock id is
 * available. This would not be useful for a normal application
 *)

function get_largest_size (id : pgchar): TGtkIconSize;
var
  theset      : PGtkIconSet;
  sizes       : PGtkIconSize;
  n_sizes,
  i           : gint;
  best_size   : TGtkIconsize;
  best_pixels : gint;

  width,
  height      : gint;

begin
  theset := gtk_icon_factory_lookup_default (id);
  best_size := GTK_ICON_SIZE_INVALID;
  best_pixels := 0;

  gtk_icon_set_get_sizes (theset, @sizes, @n_sizes);

  for i:=0 to n_sizes-1 do
  begin
    gtk_icon_size_lookup (sizes[i], @width, @height);

    if (width * height) > best_pixels then
    begin
          best_size := sizes[i];
          best_pixels := width * height;
    end; {of if}
  end; {of for}

  g_free (sizes);

  get_largest_size := best_size;
end;

procedure selection_changed (selection : PGtkTreeSelection); cdecl;
var
  treeview : PGtkTreeView;
  display  : PStockItemDisplay;
  model    : PGtkTreeModel;
  iter     : TGtkTreeIter;

  info     : PStockItemInfo;
  str      : pgchar;

begin

  treeview := gtk_tree_selection_get_tree_view (selection);
  display := g_object_get_data (pGObject(treeview), 'stock-display');

  if gtk_tree_selection_get_selected (selection, @model, @iter) then
  begin
    gtk_tree_model_get (model, @iter, [0, @info, -1]);

    if (info^.small_icon <> NULL)  and (info^.item._label <> NULL) then
        gtk_label_set_text (pGtkLabel(display^.type_label), 'Icon and Item')
      else if info^.small_icon <> NULL then
        gtk_label_set_text (pGtkLabel(display^.type_label), 'Icon Only')
      else if info^.item._label <> NULL then
        gtk_label_set_text (pGtkLabel(display^.type_label), 'Item Only')
      else
        gtk_label_set_text (pGtkLabel(display^.type_label), '???????');

    gtk_label_set_text (pGtkLabel(display^.macro_label), info^.macro);
    gtk_label_set_text (pGtkLabel(display^.id_label), info^.id);

    if info^.item._label <> NULL then
    begin
      str := g_strdup_printf ('%s %s', [info^.item._label, info^.accel_str] );
      gtk_label_set_text_with_mnemonic (pGtkLabel(display^.label_accel_label), str);
      g_free (str);
    end else
      gtk_label_set_text (pGtkLabel(display^.label_accel_label), '');

    if info^.small_icon <> NULL then
      gtk_image_set_from_stock (pGtkImage(display^.icon_image), info^.id,
                                  get_largest_size (info^.id))
    else
      gtk_image_set_from_pixbuf (pGtkImage(display^.icon_image), NULL);

    stock_item_info_free (info);
  end { of if gtk_tree_selection_get_selected }
  else
  begin
    gtk_label_set_text (pGtkLabel(display^.type_label), 'No selected item');
    gtk_label_set_text (pGtkLabel(display^.macro_label), '');
    gtk_label_set_text (pGtkLabel(display^.id_label), '');
    gtk_label_set_text (pGtkLabel(display^.label_accel_label), '');
    gtk_image_set_from_pixbuf (pGtkImage(display^.icon_image), NULL);
  end;
end;

procedure macro_set_func_text (tree_column  : PGtkTreeViewColumn;
                               cell         : PGtkCellRenderer;
                               model        : PGtkTreeModel;
                               iter         : PGtkTreeIter;
                               data         : gpointer); cdecl;
var
  info : PStockItemInfo;

begin
  gtk_tree_model_get (model, iter, [0, @info, -1]);

  g_object_set (pGtkCellRenderer(cell), 'text', [info^.macro, NULL]);

  stock_item_info_free (info);
end;

procedure macro_set_func_pixbuf (tree_column  : PGtkTreeViewColumn;
                                 cell         : PGtkCellRenderer;
                                 model        : PGtkTreeModel;
                                 iter         : PGtkTreeIter;
                                 data         : gpointer); cdecl;
var
  info : PStockItemInfo;

begin
  gtk_tree_model_get (model, iter, [0, @info, -1]);

  g_object_set (pGtkCellRenderer(cell), 'pixbuf', [info^.small_icon,  NULL]);

  stock_item_info_free (info);
end;

procedure id_set_func (tree_column  : PGtkTreeViewColumn;
                       cell         : PGtkCellRenderer;
                       model        : PGtkTreeModel;
                       iter         : PGtkTreeIter;
                       data         : gpointer); cdecl;
var
  info : PStockItemInfo;

begin
  gtk_tree_model_get (model, iter, [0, @info, -1]);

  g_object_set (pGtkCellRenderer(cell), 'text', [info^.id, NULL]);

  stock_item_info_free (info);
end;

procedure accel_set_func (tree_column  : PGtkTreeViewColumn;
                          cell         : PGtkCellRenderer;
                          model        : PGtkTreeModel;
                          iter         : PGtkTreeIter;
                          data         : gpointer); cdecl;
var
  info : PStockItemInfo;

begin
  gtk_tree_model_get (model, iter, [0, @info, -1]);

  g_object_set (pGtkCellRenderer(cell),'text', [info^.accel_str,   NULL]);

  stock_item_info_free (info);
end;

procedure label_set_func (tree_column  : PGtkTreeViewColumn;
                          cell         : PGtkCellRenderer;
                          model        : PGtkTreeModel;
                          iter         : PGtkTreeIter;
                          data         : gpointer); cdecl;
var
  info : PStockItemInfo;

begin
  gtk_tree_model_get (model, iter, [0, @info, -1]);

  g_object_set (pGtkCellRenderer(cell), 'text', [info^.item._label, NULL]);

  stock_item_info_free (info);
end;

function do_stock_browser : PGtkWidget;
var
  frame,
  vbox,
  hbox,
  sw,
  treeview,
  align         : PGtkWidget;
  model         : PGtkTreeModel;
  cell_renderer : PGtkCellRenderer;
  display       : PStockItemDisplay;
  selection     : PGtkTreeSelection;
  column        : PGtkTreeViewColumn;

begin
  if stock_window = NULL then
    begin
      stock_window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
      gtk_window_set_title (pGtkWindow(stock_window), 'Stock Icons and Items');
      gtk_window_set_default_size (pGtkWindow(stock_window), -1, 500);

      g_signal_connect (stock_window, 'destroy', TGCallback(@gtk_widget_destroyed), @stock_window);
      gtk_container_set_border_width (pGtkContainer(stock_window), 8);

      hbox := gtk_hbox_new (FALSE, 8);
      gtk_container_add (pGtkContainer(stock_window), hbox);

      sw := gtk_scrolled_window_new (NULL, NULL);
      gtk_scrolled_window_set_policy (pGtkScrolledWindow(sw),
                                      GTK_POLICY_NEVER,
                                      GTK_POLICY_AUTOMATIC);
      gtk_box_pack_start (pGtkBox(hbox), sw, FALSE, FALSE, 0);

      model := create_model ();

      treeview := gtk_tree_view_new_with_model (model);

      g_object_unref (pGObject(model));

      gtk_container_add (pGtkContainer(sw), treeview);

      column := gtk_tree_view_column_new ();
      gtk_tree_view_column_set_title (column, 'Macro');

      cell_renderer := gtk_cell_renderer_pixbuf_new ();
      gtk_tree_view_column_pack_start (column,
                                       cell_renderer,
                                       FALSE);
      gtk_tree_view_column_set_cell_data_func (column, cell_renderer,
                                               @macro_set_func_pixbuf, NULL, NULL);
      cell_renderer := gtk_cell_renderer_text_new ();
      gtk_tree_view_column_pack_start (column,
                                       cell_renderer,
                                       TRUE);
      gtk_tree_view_column_set_cell_data_func (column, cell_renderer,
                                               @macro_set_func_text, NULL, NULL);

      gtk_tree_view_append_column (pGtkTreeView(treeview),  column);

      cell_renderer := gtk_cell_renderer_text_new ();

      gtk_tree_view_insert_column_with_data_func (pGtkTreeView(treeview),
                                                  -1,
                                                  'Label',
                                                  cell_renderer,
                                                  @label_set_func,
                                                  NULL,
                                                  NULL);

      gtk_tree_view_insert_column_with_data_func (pGtkTreeView(treeview),
                                                  -1,
                                                  'Accel',
                                                  cell_renderer,
                                                  @accel_set_func,
                                                  NULL,
                                                  NULL);

      gtk_tree_view_insert_column_with_data_func (pGtkTreeView(treeview),
                                                  -1,
                                                  'ID',
                                                  cell_renderer,
                                                  @id_set_func,
                                                  NULL,
                                                  NULL);

      align := gtk_alignment_new (0.5, 0.0, 0.0, 0.0);
      gtk_box_pack_end (pGtkBox(hbox), align, FALSE, FALSE, 0);

      frame := gtk_frame_new ('Selected Item');
      gtk_container_add (pGtkContainer(align), frame);

      vbox := gtk_vbox_new (FALSE, 8);
      gtk_container_set_border_width (pGtkContainer(vbox), 4);
      gtk_container_add (pGtkContainer(frame), vbox);

      display := g_malloc0 (sizeof(TStockItemDisplay));
      g_object_set_data_full (pGObject(treeview),
                              'stock-display',
                              display,
                              @g_free); (* free display with treeview *)

      display^.type_label  := gtk_label_new (NULL);
      display^.macro_label := gtk_label_new (NULL);
      display^.id_label    := gtk_label_new (NULL);
      display^.label_accel_label := gtk_label_new (NULL);
      display^.icon_image  := gtk_image_new_from_pixbuf (NULL); (* empty image *)

      gtk_box_pack_start (pGtkBox(vbox), display^.type_label,
                          FALSE, FALSE, 0);

      gtk_box_pack_start (pGtkBox(vbox), display^.icon_image,
                          FALSE, FALSE, 0);

      gtk_box_pack_start (pGtkBox(vbox), display^.label_accel_label,
                          FALSE, FALSE, 0);
      gtk_box_pack_start (pGtkBox(vbox), display^.macro_label,
                          FALSE, FALSE, 0);
      gtk_box_pack_start (pGtkBox(vbox), display^.id_label,
                          FALSE, FALSE, 0);

      selection := gtk_tree_view_get_selection (pGtkTreeView(treeview));
      gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

      g_signal_connect (pGObject(selection),
                        'changed',
                        TGCallback(@selection_changed),
                        NULL);
    end;

  if not GTK_WIDGET_VISIBLE (stock_window) then
    gtk_widget_show_all (stock_window)
  else begin
    gtk_widget_destroy (stock_window);
    stock_window := NULL;
  end;

  do_stock_browser := stock_window;
end;

