
Const
  DefCompare : TGtkCListCompareFunc = Nil;

Function FileCompareFunc(List:PGtkCList; Row1,Row2 : PGtkCListRow) : Longint; Cdecl;

Var
  SC : Longint;

begin
  SC:=List^.sort_column;
  If SC in [2,3] then
    begin
    SC:=SC-2;
    Result:=PLongint(Row1^.Data)[SC]-PLongint(Row2^.Data)[SC];
    end
  Else
    Result:=DefCompare(List,Row1,Row2);
end;

Procedure DestroySortData(FSD : Pointer);cdecl;

begin
  FreeMem(FSD);
end;

Procedure AddFileToList(List : PGtkCList; Info : TSearchRec);

Var
  Texts : Array[1..6] of AnsiString;
  FSD   : PLongint;
  I     : longint;

begin
  Texts[1]:=ExtractFileName(Info.Name);
  Texts[2]:=ExtractFileExt(Info.Name);
  Texts[3]:=FileSizeToString(Info.Size);
  Texts[4]:=DateTimeToStr(FileDateToDateTime(Info.Time));
  Texts[5]:=FileAttrsToString(Info.Attr);
  Texts[6]:='';
  i:=gtk_clist_append(List,@Texts[1]);
  FSD:=GetMem(2*SizeOf(Longint));
  FSD[0]:=Info.Size;
  FSD[1]:=Info.Time;
  gtk_clist_set_row_data_full (List,I,FSD,@DestroySortData);
end;

Function FillList(List : PGtkCList; Const Dir,Mask : String) : Integer;

Var
  Info : TSearchRec;
  Size : Int64;
  I : longint;

begin
  Result:=0;
  Size:=0;
  gtk_clist_freeze(List);
  Try
    gtk_clist_clear(List);
    If FindFirst (AddTrailingSeparator(Dir)+Mask,faAnyFile,Info)=0 then
      Repeat
        Inc(Size,Info.Size);
        AddFileToList(List,Info);
        Inc(Result);
      Until FindNext(Info)<>0;
    FindClose(info);
  finally
    For I:=0 to 4 do
      gtk_clist_set_column_width(List,i,gtk_clist_optimal_column_width(List,i));
    gtk_clist_thaw(List)
  end;
end;

Procedure ShowPopup(Widget : PGtkWidget; Event : PGdkEventButton; Window : PMainWindow);cdecl;

begin
  if (event^.thetype=GDK_BUTTON_PRESS) and (event^.button=3) then
    begin
    gtk_menu_popup(Window^.PMFiles,Nil,Nil,Nil,NIl,3,event^.time);
    end;
end;

Procedure FileColumnClick(List : PGtkCList;Column:gint; Window : PMainWindow);cdecl;

Var
  I  : longint;
  NS : TGtkSortType;

begin
  If Column<>List^.sort_column Then
    begin
    gtk_clist_set_sort_type(List,GTK_SORT_ASCENDING);
    gtk_clist_set_sort_column(list,Column);
    end
  else
    begin
    If (List^.Sort_type=GTK_SORT_ASCENDING) Then
      NS:=GTK_SORT_DESCENDING
    else
      NS:=GTK_SORT_ASCENDING;
    gtk_clist_set_sort_type(List,NS);
    end;
  gtk_clist_sort(list);
end;

Function NewFileList(MainWindow : PMainWindow) : PGtkClist;

Const
  Titles : Array[1..6] of pchar = ('Name','ext','Size','Date','Attributes','');

begin
  MainWindow^.ListScrollWindow:=PGtkScrolledWindow(gtk_scrolled_window_new(Nil,Nil));
  gtk_scrolled_window_set_policy(MainWindow^.ListScrollWindow,
                                 GTK_POLICY_AUTOMATIC,
                                 GTK_POLICY_AUTOMATIC);
  Result:=PGtkClist(Gtk_Clist_New_with_titles(6,@Titles));
  gtk_signal_connect(PgtkObject(Result),'button_press_event',TGtkSignalFunc(@ShowPopup),MainWindow);
  gtk_signal_connect(PgtkObject(Result),'click_column',TGtkSignalFunc(@FileColumnClick),MainWindow);
  gtk_Container_add(PGTKContainer(MainWindow^.ListScrollWindow),PGtkWidget(Result));
  gtk_clist_set_shadow_type(Result,GTK_SHADOW_ETCHED_OUT);
  gtk_clist_set_column_justification(result,2,GTK_JUSTIFY_RIGHT);
  gtk_clist_set_selection_mode(Result,GTK_SELECTION_MULTIPLE);
  gtk_clist_set_auto_sort(Result,True);
  If DefCompare=Nil then
    DefCompare:=Result^.compare;
  gtk_clist_set_compare_func(Result,TGtkCListCompareFunc(@FileCompareFunc));
end;

Procedure ToggleFileListTitles(Sender : PGtkCheckMenuItem;Window : PMainWindow);cdecl;

begin
  If active(Sender^)=0 then
    gtk_clist_column_titles_show(Window^.FileList)
  else
    gtk_clist_column_titles_hide(Window^.FileList)
end;

Procedure ToggleFileListColumns(Sender : PGtkCheckMenuItem;Window : PMainWindow);cdecl;

Var Col : Longint;

begin
  With Window^ do
    If Sender=MIShowExt Then
      Col:=1
    else if Sender=MiShowSize Then
      Col:=2
    else if Sender=MIShowDate then
      Col:=3
    else
      Col:=4;
   gtk_clist_set_column_visibility(Window^.FileList,Col,(Active(Sender^)=0));
end;

Procedure GetFileSelection (List : PGtkClist; Selection : TStrings);

Var
  SList : PGList;
  Index : Longint;
  P : PChar;

begin
  Selection.Clear;
  Slist:=List^.Selection;
  While SList<>nil do
    begin
    Index:=Longint(SList^.Data);
    gtk_clist_get_text(List,Index,0,@p);
    Selection.Add(StrPas(p));
    SList:=g_list_next(SList);
    end;
end;

Function GetFileFirstSelection (List : PGtkClist) : String;

Var
  SList : PGList;
  Index : Longint;
  P : PChar;

begin
  REsult:='';
  Slist:=List^.Selection;
  If SList<>nil then
    begin
    Index:=Longint(SList^.Data);
    gtk_clist_get_text(List,Index,0,@p);
    Result:=StrPas(p);
    end;
end;

Function GetFileSelectionCount (List : PGtkClist) : Longint;

Var
  SList : PGList;

begin
  Slist:=List^.Selection;
  Result:=0;
  While SList<>nil do
    begin
    Inc(Result);
    SList:=g_list_next(SList);
    end;
end;

Procedure RefreshFileView(Window : PMainWindow);

begin
  With Window^ do
    FillList(FileList,FDir,FMask);
end;