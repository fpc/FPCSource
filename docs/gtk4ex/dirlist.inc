
Procedure Scandirs(Path: String; Tree : PgtkTree;
                   Node: PGtkTreeItem ; SubSub : Boolean;
                   Window : PMainWindow); forward;


Function GetPathName(Item : PGtkTreeItem) : String;

Var P : PChar;
    PTree : PGtkTree;
begin
  gtk_label_get(PgtkLabel(PGtkBin(Item)^.Child),@P);
  Result:=StrPas(P);
  If (PGtkWidget(item)^.Parent<>Nil) then
    begin
    PTree:=PGtkTree(PgtkWidget(Item)^.Parent);
    If (Ptree^.Level<>0) Then
      Result:=AddTrailingSeparator(GetPathName(PgtkTreeItem(PTree^.Tree_Owner)))+Result
    end;
end;

Procedure DirSelect(Item : PGtkTreeItem; Window : PMainWindow);cdecl;

begin
  ShowDir(Window,GetPathName(Item));
end;

Procedure DirExpand(Item : PGtkTreeItem; Window : PMainWindow);cdecl;

Var
  Dir : String;
  SubTree : PGtkTree;
  SubNodes : PGList;
  Node : PGtkTreeItem;

begin
  SubTree:=PgtkTree(Item^.SubTree);
  SubNodes:=gtk_container_children(PGtkContainer(SubTree));
  While SubNodes<>Nil do
    begin
    Node:=PgtkTreeItem(SubNodes^.Data);
    If (Node^.SubTree<>Nil) then
      gtk_tree_item_remove_subtree(Node);
    Scandirs(GetPathName(Node),Nil,Node,False,Window);
    SubNodes:=g_list_remove_link(SubNodes,SubNodes);
    end;
end;

Procedure Scandirs(Path: String; Tree : PgtkTree;
                   Node: PGtkTreeItem ; SubSub : Boolean;
                   Window : PMainWindow);

Var
  NewTree : PGtkTree;
  NewNode : PGtkTreeItem;
  Info : TSearchRec;
  S,FP : AnsiString;

begin
  NewTree:=Nil;
  FP:=AddTrailingSeparator(Path);
  If FindFirst(FP+'*.*',faAnyfile,Info)=0 then
    Try
      repeat
        If ((Info.Attr and faDirectory)=faDirectory) then
          begin
          S:=Info.Name;
          If (S<>'.') and (S<>'..') then
            begin
            If (Node<>Nil) then
              begin
              If (NewTree=Nil) and (node<>Nil) then
                begin
                NewTree:=PGtkTree(gtk_tree_new);
                gtk_tree_item_set_subtree(Node,PGtkWidget(NewTree));
                end
              end
            else
              NewTree:=Tree;
            NewNode:=PGtkTreeItem(gtk_tree_item_new_with_label(Pchar(S)));
            gtk_tree_append(NewTree,PgtkWidget(NewNode));
            gtk_signal_connect(PGtkObject(NewNode),'select',
                               TGtkSignalFunc(@DirSelect),Window);
            gtk_signal_connect(PGtkObject(NewNode),'expand',
                               TGtkSignalFunc(@DirExpand),Window);
            If SubSub then
              ScanDirs(FP+S,Tree,NewNode,False,Window);
            gtk_widget_show(PGtkWidget(NewNode));
            end;
          end;
      until FindNext(Info)<>0;
    Finally
      FindClose(Info);
    end;
  gtk_widget_show(PGtkWidget(Node));
end;

Function NewDirtree (MainWindow : PMainWindow) : PGtkTree;

begin
  Result:=PGtkTree(gtk_tree_new());
  With MainWindow^ do
    begin
    TreeScrollWindow:=PGtkScrolledWindow(gtk_scrolled_window_new(Nil,Nil));
    gtk_widget_show(PGtkWidget(TreeScrollWindow));
    gtk_scrolled_window_set_policy(TreeScrollWindow,
                                 GTK_POLICY_AUTOMATIC,
                                 GTK_POLICY_AUTOMATIC);
    gtk_scrolled_window_add_with_viewport(TreeScrollWindow,PGtkWidget(Result));
    RootNode:=PGtkTreeItem(gtk_tree_Item_new_with_label(Pchar(PathSeparator)));
    gtk_tree_append(Result,PgtkWidget(RootNode));
    scandirs(PathSeparator,Result, RootNode,True,MainWindow);
    gtk_tree_item_expand(rootnode);
    end;
end;
