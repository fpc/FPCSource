{$mode objfpc}
{$h+}

unit pgEditor;

interface

uses SysUtils,Classes,gdk,fpgtk,gtk,fpgtkext,dom,xmlread,xmlwrite;

Type
  TTagType = (ttBold,ttItalic,ttUnderline,ttParagraph,ttVariable,ttRemark,
              ttNumberedList,ttUnnumberedList,ttListItem,ttTable,ttRow,
              ttCell,TTHeader,ttPre,ttCode,ttLink,ttFile);

  TNodeTreeItem = Class(TFPGtkTreeItem)
  Private
    Fnode : TDomElement;
  Public
    Constructor Create (ANode : TDomElement); virtual;
    Property Node : TDomElement Read Fnode;
  end;

  TModuleTreeItem = Class(TNodeTreeItem);

  TPackageTreeItem = Class(TNodeTreeItem)
  Private
    HaveSubTree : Boolean;
  Public
    Procedure CheckSubTree;
  end;

  TNodeSelectEvent = Procedure (Sender : TObject; Node : TDomElement) Of Object;
  
  TPackageEditor = Class(TFPGtkVPaned)
  Private
    FCurrentPackage,
    FCurrentElement,
    FCurrentModule : TDomElement;
    FOnSelectElement,
    FOnSelectPackage,
    FOnSelectionChanged,
//    FOnDeSelectElement,
    FOnSelectModule : TNodeSelectEvent;
    FDescriptionNode : TDomNode;
    FModuleTree : TFPGtkScrollTree;
    FElementTree : TFPGtkScrollTree;
    FModuleNode : TNodeTreeItem;
    FModified : Boolean;
    PMMenu : TFPgtkMenu;
    PMNode : TNodeTreeItem;
    FRenameMenu,
    FDeleteMenu : TFPgtkMenuItem;
    Procedure MenuRenameClick(Sender : TFPGtkWidget; Data : Pointer);
    Procedure MenuDeleteClick(Sender : TFPGtkWidget; Data : Pointer);
    Function  PopupModuleMenu(Sender:TFPgtkWidget; Event:PGdkEventButton; data:pointer): boolean;
    Procedure SelectModule(Sender : TFPGtkWidget; Data : Pointer);
    Procedure SelectPackage(Sender : TFPGtkWidget; Data : Pointer);
    Procedure SelectElement(Sender : TFPGtkWidget; Data : Pointer);
//    Procedure DeSelectElement(Sender : TFPGtkWidget; Data : Pointer);
//    Procedure DoSelectionChanged(Sender : TFPGtkWidget; Data : Pointer);
    Procedure ShowModuleElements(Module : TDomElement);
    Procedure SetCurrentModule(Value : TDomElement);
    Procedure SetCurrentPackage(Value : TDomElement);
    Function  FindPackageNode(P : TDomElement) : TNodeTreeItem;
    Function  FindModuleNodeInNode(M : TDomElement; N : TNodeTreeItem) : TNodeTreeItem;
    Function  FindElementNode(E : TDomElement; N : TNodeTreeItem) : TNodeTreeItem;
    Procedure SetCurrentElement(E : TDomElement);
    Function  CreateElementNode(E : TDomelement) : TNodeTreeItem;
    Procedure DeletePackage(N : TNodeTreeItem);
    Procedure DeleteModule(N : TNodeTreeItem);
    Procedure DeleteElement(N : TNodeTreeItem);
    Procedure RenamePackage(N : TNodeTreeItem);
    Procedure RenameModule(N : TNodeTreeItem);
    Procedure RenameElement(N : TNodeTreeItem);
    Function  GetSelectedNode : TNodeTreeItem;
    Procedure GetNameData(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);
    Function  NewName(ATitle : String;Var AName : String) : Boolean;
    Procedure ClearElements;
  Public
    Constructor Create;
    Procedure SetDescriptionNode (Value : TDomNode);
    Procedure Refresh;
    Procedure AddElement(E : TDomElement);
    Procedure DeletePackage(P : TDomElement);
    Procedure DeleteModule(M : TDomElement);
    Procedure DeleteElement(E : TDomElement);
    Procedure RenamePackage(P : TDomElement);
    Procedure RenameModule(M : TDomElement);
    Procedure RenameElement(E : TDomElement);
    Property ModuleTree : TFPGtkScrollTree Read FModuleTree;
    Property ElementTree : TFPGtkScrollTree Read FElementTree;
    Property DescriptionNode : TDomNode Read FDescriptionNode Write SetDescriptionNode;
    Property OnSelectModule : TNodeSelectEvent Read FOnSelectModule Write FOnSelectmodule;
    Property OnSelectPackage : TNodeSelectEvent Read FOnSelectPackage Write FOnSelectPackage;
//    Property OnDeSelectElement : TNodeSelectEvent Read FOnDeSelectElement Write FOnDeSelectElement;
    Property OnSelectElement : TNodeSelectEvent Read FOnSelectElement Write FOnSelectElement;
//    Property OnSelectionChanged : TNodeSelectEvent Read FOnSelectionChanged Write FOnSelectionChanged;
    Property CurrentPackage : TDomElement Read FCurrentPackage Write SetCurrentPackage;
    Property CurrentModule : TDomElement Read FCurrentModule Write SetCurrentModule;
    Property CurrentElement: TDomElement Read FCurrentElement  Write SetCurrentElement;
    Property Modified : Boolean Read FModified Write FModified;
  end;

  TElementEditor = Class(TFPGtkVBox)
  Private
    FShortNode,
    FDescrNode,
    FErrorsNode,
    FSeeAlsoNode,
    Felement : TDomElement;
    FLabel  : TFPGtkLabel;
    FShortEntry : TFPGtkEntry;
    FDescrMemo,
    FErrorsMemo,
    FSeeAlsoMemo : TFPGtkScrollText;
    FCurrentEditable : TFPGtkEditable;
    FModified : Boolean;
    Procedure GetNodes;
    Procedure SetElement (Value : TDomElement);
    Function CurrentEditable : TFPGtkEditable;
    Function OnEditableFocusIn(Sender : TFPGtkWidget; Event : PGDKEventFocus;Data : Pointer) : Boolean;
    Function GetCurrentSelection : String;
    Procedure OnTextModified(Sender : TFPGtkWidget; Data : Pointer);
  Public
    Procedure Refresh;
    Constructor Create;
    Function  TestSave(S : String) : Boolean;
    Function  CurrentXML : String;
    Function  Save : Boolean;
    Procedure DeleteElement;
    Procedure InsertTag (tagName : String);
    Procedure InsertTag (TagType : TTagType);
    Procedure InsertLink(LinkTarget,LinkText : String);
    Procedure InsertTable(Cols,Rows : Integer; UseHeader : Boolean);
    Property  Element : TDomElement Read FElement Write SetElement;
    Property CurrentSelection : String Read GetCurrentSelection;
    Property Modified : Boolean Read FModified Write FModified;
  end;


  TEditorPage = Class(TFPGtkHPaned)
  Private
    FChangingSelection : Boolean;
    FFileNameLabel : TFpGtkLabel;
    FDocument : TXMLDocument;
    FPackages : TPackageEditor;
    FElement : TElementEditor;
    FFileNAme : String;
    Procedure ElementSelected(Sender : TObject; Node : TDomElement) ;
//    Procedure SelectionChanged(Sender : TFPgtkWidget; Node : TDomElement) ;
//    Procedure ElementDeSelected(Sender : TObject; Node : TDomElement) ;
    Function GetCurrentSelection : String;
    Function GetCurrentPackage : TDomElement;
    Function GetCurrentModule : TDomElement;
    Function GetCurrentElement : TDomElement;
    Procedure SetCurrentModule(Value : TDomElement);
    Procedure SetCurrentPackage(Value : TDomElement);
    Procedure SetCurrentElement(Value : TDomElement);
    Procedure SetModified(Value : Boolean);
    Function  GetModified : Boolean;
    Function MakeBackup(FN : String) : Boolean;
  Public
    Constructor Create;
    Function  FirstPackage : TDomElement;
    Function  FirstModule(APackage : TDomElement) : TDomElement;
    Procedure DisplayDocument;
    Procedure ClearDocument;
    Procedure LoadFromFile(FN : String);
    Procedure LoadFromStream(S : TStream);
    Procedure SaveToFile(FN : String);
    Procedure SetFileName(FN : String);
    Procedure InsertTag(TagType : TTagType);
    Procedure InsertLink(LinkTarget,LinkText : String);
    Procedure InsertTable(Cols,Rows : Integer; UseHeader : Boolean);
    Procedure NewPackage(APackageName : String);
    Procedure NewModule(AModuleName : String);
    Procedure NewElement(AElementName : String);
    Procedure GetElementList(List : TStrings);
    Property FileName : String Read FFileName;
    Property CurrentSelection : String Read GetCurrentSelection;
    Property CurrentPackage : TDomElement Read GetCurrentPackage Write SetCurrentPackage;
    Property CurrentModule : TDomElement Read GetCurrentModule Write SetCurrentModule;
    Property CurrentElement : TDomElement Read GetCurrentElement Write SetCurrentElement;
    Property FileNameLabel : TFPgtkLabel Read FFileNameLabel Write FFileNameLabel;
    Property Modified : Boolean Read GetModified Write SetModified;
  end;

Const
  TagNames : Array[TTagType] of string = ('b','i','u','p','var','remark',
              'ol','ul','li','table','tr',
              'td','th','pre','code','link','file'
  );

  
implementation

uses fpdemsg,frmnewnode,fpdeopts;

{ ---------------------------------------------------------------------
  TElementEditor
  ---------------------------------------------------------------------}

  
Constructor TElementEditor.Create;

Var
  F1,F2 : TFPGtkVPaned;
  V : TFPgtkVBox;
  L : TFpGtkLabel;

begin
  Inherited Create;
  FLabel:=TFPgtkLabel.Create(SNoElement);
  PackStart(FLabel,False,false,0);
  L:=TFPGtkLabel.Create(SShortDescription);
  V:=TFPGtkVBox.Create;
  // Short
  FShortEntry:=TFPGtkEntry.Create;
//  FShortEntry.ConnectEnterNotify(@OnEnterEditable,Nil);
//  FShortEntry.ConnectStateChanged(@OnEditableStateChanged,Nil);
  FShortEntry.ConnectFocusInEvent(@OnEditableFocusIn,Nil);
  FShortEntry.ConnectChanged(@OnTextModified,Nil);
  V.PackStart(L,False,False,0);
  V.PackStart(FShortEntry,True,true,0);
  PackStart(V,false,false,0);
  // Errors
  L:=TFPGtkLabel.Create(SErrors);
  V:=TFPGtkVBox.Create;
  FErrorsMemo:=TFPGtkScrollText.Create;
//  FErrorsMemo.TheText.ConnectEnterNotify(@OnEnterEditable,Nil);
  FErrorsMemo.TheText.ConnectFocusInEvent(@OnEditableFocusIn,Nil);
  FErrorsMemo.TheText.ConnectChanged(@OnTextModified,Nil);
  FErrorsMemo.setusize(400,50);
  V.PackStart(L,False,False,0);
  V.PackStart(FErrorsMemo,True,true,0);
  F2:=TFPGtkVPaned.Create;
  F2.Pack1(V,True,True);
  // See Also
  L:=TFPGtkLabel.Create(SSeeAlso);
  V:=TFPGtkVBox.Create;
  FSeeAlsoMemo:=TFPGtkScrollText.Create;
//  FSeeAlsoMemo.TheText.ConnectEnterNotify(@OnEnterEditable,Nil);
  FSeeAlsoMemo.TheText.ConnectFocusInEvent(@OnEditableFocusIn,Nil);
   FSeeAlsoMemo.TheText.ConnectChanged(@OnTextModified,Nil);
  FSeeAlsoMemo.setusize(400,50);
  V.PackStart(L,False,False,0);
  V.PackStart(FSeeAlsoMemo,True,true,0);
  F2.Pack2(V,False,True);
  // Descr
  L:=TFPGtkLabel.Create(SDescription);
  V:=TFPGtkVBox.Create;
  FDescrMemo:=TFPGtkScrollText.Create;
//  FDescrMemo.TheText.ConnectEnterNotify(@OnEnterEditable,Nil);
  FDescrMemo.TheText.ConnectFocusInEvent(@OnEditableFocusIn,Nil);
  FDescrMemo.TheText.ConnectChanged(@OnTextModified,Nil);
  FDescrMemo.setusize(400,150);
  V.PackStart(L,False,False,0);
  V.PackStart(FDescrMemo,True,true,0);
  F1:=TFPGtkVPaned.Create;
  F1.Pack1(V,True,True);
  F1.Pack2(F2,False,True);
  PackStart(F1,true,true,0);
end;

Procedure TElementEditor.SetElement (Value : TDomElement);

begin
  If (Value<>FElement) then
    If not Modified or Save then
      begin
      FElement:=Value;
      Refresh;
      end;
end;

Procedure TElementEditor.DeleteElement;

begin
  FElement:=Nil;
  Refresh;
end;

Procedure TElementEditor.Refresh;

Var
  S : TSTringStream;

  Function NodeToString(E : TDomElement) : String;

  Var
    N : TDomNode;

  begin
    If (E=Nil) then
      Result:=''
    else
      begin  
      S.Seek(0,soFromBeginning);
      S.Size:=0;
      N:=E.FirstChild;
      While Assigned(N) do
        begin
        WriteXml(N,S);
        N:=N.NextSibling;  
        end;  
      Result:=S.Datastring;
    end;
  end;
  
  Function RemoveLineFeeds(S : String) : String;
  
  Var 
    I : Integer; 
  
  begin
    Result:=S;
    For I:=1 to Length(Result) do
      If Result[i] in [#10,#13] then
        Result[i]:=' ';
  end;
  
begin
  GetNodes;
  If Assigned(Felement) then
    FLabel.Text:=Format(SDataForElement,[FElement['name']])
  else
    FLabel.Text:=SNoElement; 
  S:=TStringStream.Create('');
  Try
    FShortEntry.Text:=RemoveLineFeeds(NodeToString(FShortNode));
    FDescrMemo.Text:=NodeToString(FDescrNode);
    FErrorsMemo.Text:=NodeToString(FErrorsNode);
    FSeeAlsoMemo.Text:=NodeToString(FSeeAlsoNode);
    FModified:=False;
  Finally
    S.Free;
  end;
end;

Function TElementeditor.TestSave(S : String) : Boolean;

Const
  Head = '<?xml version="1.0" encoding="ISO8859-1"?><fpdoc-descriptions>';
  Tail = '</fpdoc-descriptions>';
  SErrorSaving = 'There is an error in the documentation nodes:'+LineEnding+
                 '%s'+LineEnding+
                 'Please correct it first and try saving again.';
                 
Var
  D : TXMLDocument;
  SS : TStringStream;

begin
//  Writeln('In testsave');
  Result:=Length(S)=0;
  If Not Result then
    begin
    SS:=TStringStream.Create(Head+S+Tail);
    Try
      Try
        ReadXmlFile(D,SS);
        Result:=True;
      except
        On E : Exception do 
           MessageDlg(SErrorSaving,[E.Message],mtError,[mbOK],0)
      end;  
    finally 
      D.Free;
      SS.Free;
    end;
    end;
end;

Function  TElementEditor.CurrentXML : String;

  Function GetNodeString(NodeName,Value : String) : String; 
  
  begin
    Result:='';
    If (Value<>'') Then
      Result:=Format('<%s>%s</%s>',[NodeName,Value,NodeName])
    else If Not SkipEmptyNodes then
      result:='<'+NodeName+'/>';
  end;


begin
//  Writeln('In currentxml');
  Result:='';
  If Not Assigned(FElement) then 
    Exit;
  Result:=GetNodeString('short',Trim(FShortEntry.Text));
  Result:=Result+GetNodeString('descr',trim(FDescrMemo.Text));
  Result:=Result+GetNodeString('errors',trim(FErrorsMemo.Text));
  Result:=Result+GetNodeString('seealso',trim(FSeeAlsoMemo.Text));
//  Writeln('Currentxml : ',Result);
end;

Function TElementEditor.Save : Boolean;

Var
  SS : TStringStream;
  S : String;
  

begin
  Result:=False;
  S:=CurrentXML;
  If TestSave(S) then 
    begin
//    Writeln('Saving data');
    SS:=TStringStream.Create(S);
    Try 
      // Free child nodes.
      With FElement do
        While FirstChild<>Nil do
          RemoveChild(FirstChild);
      // Read them again from stream.  
      SS.Seek(0,soFromBeginning);
      ReadXMLFragment(FElement,SS);
      FModified:=False;
      Result:=True;
    Finally
      SS.Free;
    end;  
    end;
end;


Procedure TElementEditor.InsertTag (tagName : String);

Var
  PB,PE : Integer;

begin
  If Assigned(CurrentEditable) then 
    With CurrentEditable do
      begin
      PB:=SelectionStart;
      PE:=SelectionEnd;
      InsertText('</'+TagName+'>',PE);
      InsertText('<'+TagName+'>',PB);
      SelectionStart:=PB+Length(TagName)+2;
      FModified:=True;
      end;
end;

Procedure TElementEditor.InsertTag(TagType : TTagType);

begin
  InsertTag(TagNames[TagTYpe]);
end;

Procedure TElementEditor.InsertLink(LinkTarget,LinkText : String);

begin
  If CurrentEditable<>Nil then
    With CurrentEditable do
      begin
      If (LinkText<>'') then
        Selection:='<link id="'+LinkTarget+'">'+LinkText+'</link>'
      else
        Selection:='<link id="'+LinkTarget+'"/>';
      end;
end;

Procedure TElementEditor.InsertTable(Cols,Rows : Integer; UseHeader : Boolean);

Var
  I,J : Integer;
  R,T : String;

begin
  If (CurrentEditable=FDescrMemo.TheText) or
     (CurrentEditable=FErrorsMemo.TheText) then
    begin  
    R:='<tr>';
    For I:=1 to Cols do
      R:=R+'<td></td>';
    R:=R+'</tr>'+lineEnding;  
    T:='';
    If UseHeader then 
      begin
      Dec(Rows);
      T:='<tr>';
      For I:=1 to Cols do
        T:=T+'<th></th>';
      T:=T+'</tr>'+lineEnding;  
      end;
    For I:=1 to rows do
      T:=T+R;
    T:=LineEnding+'<table>'+LineEnding+T+'</table>'+LineEnding;  
    With CurrentEditable do
      Selection:=t;
    end;  
end;

Procedure TElementEditor.GetNodes;

Var 
  Node : TDomNode;
  S : String;
  
begin
  FShortNode:=Nil;
  FDescrNode:=Nil;
  FErrorsNode:=Nil;
  FSeeAlsoNode:=Nil;
  If Assigned(FElement) then
    begin
    Node:=FElement.FirstChild;
    While Assigned(Node) do
      begin
      If (Node.NodeType=ELEMENT_NODE) then
        begin
        S:=Node.NodeName;
        If S='short' then
          FShortNode:=TDomElement(Node)
        else if S='descr' then
          FDescrNode:=TDomElement(Node)
        else if S='errors' then
          FErrorsNode:=TDomElement(Node)
        else if S='seealso' then
          FSeeAlsoNode:=TDomElement(Node);
        end;
      Node:=Node.NextSibling;
      end;
    end;
end;

Function TElementEditor.CurrentEditable : TFPGtkEditable;

begin
  Result:=FCurrentEditable;
end;

Function TElementEditor.OnEditableFocusIn(Sender : TFPGtkWidget; Event : PGDKEventFocus;Data : Pointer) : Boolean;

begin
  FCurrentEditable:=Sender as TFPGtkEditable;
  Result:=False;
end;

Procedure TElementEditor.OnTextModified(Sender : TFPGtkWidget; Data : Pointer);

begin
  FModified:=True;
end;

Function TElementEditor.GetCurrentSelection : String;

begin
  If CurrentEditable=Nil then
    begin
    Writeln('No current editable');
    Result:=''
    end
  else  
    Result:=CurrentEditable.Selection;
end;

{ ---------------------------------------------------------------------
  TNodeTreeItem
  ---------------------------------------------------------------------}

Constructor TNodeTreeItem.Create (ANode : TDomElement);

begin
  Inherited Create;
  FNode:=Anode;
  Text:=ANode['name'];
end;

Procedure TPackageTreeItem.CheckSubTree;

begin
  If (SubTree=Nil) then
    SubTree:=TFPgtkTree.Create;
end;

{ ---------------------------------------------------------------------
  TPackageEditor
  ---------------------------------------------------------------------}

Constructor TPackageEditor.Create;

begin
  Inherited;
  FModuleTree:=TFPGtkScrollTree.Create;
  FElementTree:=TFPGtkScrollTree.Create;
//  FelementTree.Tree.ConnectSelectionChanged(@DoSelectionChanged,Nil);
  PMMenu:=TFPgtkMenu.Create;
  FRenameMenu:=NewMenuItem(SMenuRename,'','',@MenuRenameClick,Nil);
  FDeleteMenu:=NewMenuItem(SMenuDelete,'','',@MenuDeleteClick,Nil);
  PMMenu.Append(FRenameMenu);
  PMMenu.Append(FDeleteMenu);
  Add1(FModuleTree);
  Add2(FElementTree);
end;

function TPackageEditor.PopupModuleMenu(Sender:TFPgtkWidget; Event:PGdkEventButton; data:pointer): boolean;

begin
  PMNode:=Sender as TNodeTreeItem;
  Result:=(event^.thetype=GDK_BUTTON_PRESS) and (event^.button=3);
  If Result then
    PMMenu.Popup(3);
end;

Procedure TPackageEditor.SetDescriptionNode (Value : TDomNode);

begin
  FDescriptionNode:=Value;
  Refresh;
end;


{Procedure TPackageEditor.DoSelectionChanged(Sender : TFPGtkWidget; Data : Pointer);

Var
  N : TNodeTreeItem;
  E : TDomElement;

begin
//  Writeln('In doselectionchanged');
  If Assigned(FOnSelectionChanged) Then
    begin
//    Writeln('Getting selected node');
    N:=GetSelectedNode;
    If N<>Nil then
      E:=N.Node
    else
      E:=Nil;  
    FOnSelectionChanged(Sender,E);
    end;
end;
}
Procedure TPackageEditor.SelectModule(Sender : TFPGtkWidget; Data : Pointer);

Var 
  W : TFPGtkWidget;

begin
  FCurrentModule:=TDomElement(Data);
  FCurrentPackage:=FCurrentModule.ParentNode as TDomElement;
  ShowModuleElements(FCurrentModule);
  If Assigned(FOnSelectModule) then
    FOnSelectModule(Self,FCurrentModule);
end;

Procedure TPackageEditor.SelectPackage(Sender : TFPGtkWidget; Data : Pointer);

begin
  FCurrentPackage:=TDomElement(Data);
  If Assigned(FOnSelectPackage) then
    FOnSelectPackage(Self,FCurrentPackage);
end;

Function  TPackageEditor.GetSelectedNode : TNodeTreeItem;

Var
  G : TFPgtkGroup;
  
begin
  G:=TFPgtkGroup.Create;
  try  
    FModuleTree.Tree.GetSelection(G);
    If G.Count>0 then 
      try
        Result:=TObject(G[0]) as TNodeTreeItem;
      except
        Result:=Nil;
      end
    else
      Result:=Nil; 
 finally
   G.Free;
 end;
// Writeln('Getselectednode done'); 
end;

Procedure TPackageEditor.MenuRenameClick(Sender : TFPGtkWidget; Data : Pointer);

Var
  N : TNodeTreeItem;
  E : TDomElement;

begin
  N:=PMNode;
  If (N<>Nil) then
    If N.Node.NodeName='package' then
      RenamePackage(N)
    Else if N.Node.NodeName='module' then
      RenameModule(N)
    else  if N.Node.NodeName='element' then
      RenameElement(N)
end;


Procedure TPackageEditor.MenuDeleteClick(Sender : TFPGtkWidget; Data : Pointer);

Var
  N : TNodeTreeItem;
  E : TDomElement;

begin
  N:=PMNode;
  If (N<>Nil) then
    If N.Node.NodeName='package' then
      DeletePackage(N)
    Else if N.Node.NodeName='module' then
      DeleteModule(N)
    else if N.Node.NodeName='element' then
      DeleteElement(N);
end;


Procedure TPackageEditor.DeletePackage(N : TNodeTreeItem);

begin
  If (Not ConfirmDelete) or 
     (MessageDlg(SDeletePackage,[N.Node['name']],mtConfirmation,mbYesNo,0)=mrYes) then
    begin 
    N.Node.ParentNode.RemoveChild(N.Node);
    Refresh; 
    FModified:=True;
    end;
end;


Procedure TPackageEditor.DeleteModule(N : TNodeTreeItem);

begin
  If (Not ConfirmDelete) or 
     (MessageDlg(SDeleteModule,[N.Node['name']],mtConfirmation,mbYesNo,0)=mrYes) then
    begin 
    N.Node.ParentNode.RemoveChild(N.Node);
    Refresh; 
    FModified:=True;
    end;
end;

Procedure TPackageEditor.GetNameData(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);

type
  PString = ^AnsiString;

begin
  With (Sender as TNewNodeForm) do
    PString(Data)^:=FENodeName.Text;
end;

Function TPackageEditor.NewName(ATitle : String;Var AName : String) : Boolean;

Var
  S : String;

begin
  With TNewNodeForm.Create do
    begin
    Title:=ATitle;
    FENodeName.Text:=AName;
    Result:=Execute(Nil,@S,@GetNameData)=drOK;
    If Result Then 
      AName:=S;
    end;
end;

Procedure TPackageEditor.RenamePackage(N : TNodeTreeItem);

Var
  S : String;

begin
  S:=N.Node['name'];
  If NewName(SRenamePackage,S) then
    begin
    N.Node['name']:=S;
    N.Text:=S;
    FModified:=True;
    end;
end;


Procedure TPackageEditor.RenameModule(N : TNodeTreeItem);

Var 
  S : String;

begin
  S:=N.Node['name'];
  If NewName(SRenameModule,S) then
    begin
    N.Node['name']:=S;
    N.Text:=S;
    FModified:=True;
    end;
end;

Procedure TPackageEditor.RenameElement(N : TNodeTreeItem);

Var
  S : String;

begin
  S:=N.Node['name'];
  If NewName(SRenameElement,S) then
    begin
    N.Node['name']:=S;
    N.Text:=S;
    FModified:=True;
    end;
end;

Procedure TPackageEditor.SelectElement(Sender : TFPGtkWidget; Data : Pointer);


begin
  FCurrentElement:=TDomElement(Data);
  If Assigned(FOnSelectElement) then
    FOnSelectElement(Self,FCurrentElement);
end;

{
Procedure TPackageEditor.DeSelectElement(Sender : TFPGtkWidget; Data : Pointer);

Var
  M : TDomElement;

begin
//  Writeln('In Deselectelement');
  M:=TDomElement(Data);
  If Assigned(FOnDeSelectElement) then
    FOnDeSelectElement(Self,M);
end;
}

Function TPackageEditor.CreateElementNode(E : TDomelement) : TNodeTreeItem;

begin
  Result:=TNodeTreeItem.Create(E);
  Result.ConnectSelect(@SelectElement,E);
//  Result.ConnectDeselect(@DeselectElement,E);
end;

Procedure TPackageEditor.DeleteElement(N : TNodeTreeItem);

Var
  Reposition : Boolean;
  Index : Integer;
  
begin
  Reposition:=(N.Node=CurrentElement);
  With (FModuleNode.Subtree as TFPgtkTree) do
    begin
    Index:=ChildPosition(N);
    Remove(N);
    FModified:=True;
    If Reposition then
      SelectItem(Index);
    end;  
end;

Procedure TPackageEditor.DeleteElement(E : TDomElement);

Var
  N : TNodeTreeItem;
  
begin
  N:=FindElementNode(E,Nil);
  If (N<>Nil) then
    DeleteElement(N);
end;

Procedure TPackageEditor.DeletePackage(P : TDomElement);

Var
  N : TNodeTreeItem;

begin
  N:=FindPackageNode(P);
  If N<>NIl then
    DeletePackage(N);
end;


Procedure TPackageEditor.DeleteModule(M : TDomElement);

Var
  N : TNodeTreeItem;

begin
  N:=FindModuleNodeInNode(M,Nil);
  If N<>NIl then
    DeleteModule(N);
end;


Procedure TPackageEditor.RenamePackage(P : TDomElement);

Var
  N : TNodeTreeItem;

begin
  N:=FindPackageNode(P);
  If N<>NIl then
    RenamePackage(N);
end;


Procedure TPackageEditor.RenameModule(M : TDomElement);

Var
  N : TNodeTreeItem;

begin
  N:=FindModuleNodeInNode(M,Nil);
  If N<>NIl then
    RenameModule(N);
end;


Procedure TPackageEditor.RenameElement(E : TDomElement);

Var
  N : TNodeTreeItem;

begin
  N:=FindElementNode(E,Nil);
  If N<>NIl then
    RenameElement(N);
end;

Procedure TPackageEditor.ClearElements;

begin
  FElementTree.Tree.ClearItems(0,-1);
end;

Procedure TPackageEditor.ShowModuleElements(Module : TDomElement);

Var
  Node : TDomNode;
  ETreeNode : TNodeTreeItem;
  S : TStringList;
  I : Integer;

begin
  ClearElements;
  If Assigned(Module) then
    begin
    FModuleNode:=CreateElementNode(Module);
    FElementTree.Tree.Append(FModuleNode);
    FModuleNode.Show;
    FModuleNode.SubTree:=TFPGtkTree.Create;
    S:=TStringList.Create;
    Try
      S.Sorted:=True;
      Node:=Module.FirstChild;
      While Assigned(Node) do
        begin  
        If (Node.NodeType=ELEMENT_NODE) and (Node.NodeName='element') then
          begin
          ETreeNode:=CreateElementNode(TDomElement(Node));
          ETreeNode.ConnectButtonPressEvent(@PopupModuleMenu,Nil);
          S.AddObject(TDomElement(Node)['name'],ETreeNode);
          end;
        Node:=Node.NextSibling;
        end;
      For I:=0 to S.Count-1 do
        begin
        ETreeNode:=TNodeTreeItem(S.Objects[i]);
        (FModuleNode.SubTree as TFPgtKTree).Append(ETreeNode);
        //ETreeNode.Show;
        end;
      Finally
        S.Free;
      end;
    FModuleNode.Expand;
    // NOT (!!) FModuleNode.Select; this cannot be undone...
    FElementTree.Tree.SelectChild(FModuleNode);
    end;
  FelementTree.Tree.SelectionMode:=GTK_SELECTION_BROWSE;  

end;

Procedure TPackageEditor.Refresh;

var 
  Node,SubNode : TDomNode;
  FTreeNode : TPackageTreeItem;
  MTreeNode: TFPGtkTreeItem;

begin
  FModuleTree.Tree.ClearItems(0,-1);
  If Assigned(FDescriptionNode) then
    begin
    Node:=FDescriptionNode.FirstChild;
    While Assigned(Node) do
      begin  
      If (Node.NodeType=ELEMENT_NODE) and (Node.NodeName='package') then
        begin
        FTreeNode:=TPackageTreeItem.Create(TDomElement(Node));
        FTreeNode.ConnectSelect(@Selectpackage,Node);
        FTreeNode.ConnectButtonPressEvent(@PopupModuleMenu,Nil);
        FModuleTree.Tree.Append(FTreeNode);
        FTreeNode.Show;
        SubNode:=Node.FirstChild;
        While Assigned(SubNode) do
          begin
          If (SubNode.NodeType=ELEMENT_NODE) and (SubNode.NodeName='module') then
            begin
            MTreeNode:=TModuleTreeItem.Create(TDomElement(SubNode));
            MtreeNode.ConnectSelect(@SelectModule,SubNode);
            MTreeNode.ConnectButtonPressEvent(@PopupModuleMenu,Nil);
            FTreeNode.CheckSubtree;
            TFPGtkTree(FTreeNode.SubTree).Append(MTreeNode);
            //MTreeNode.Show;
            end;
          SubNode:=SubNode.NextSibling;  
          end;
        end;
        Node:=Node.NextSibling;
      end;
    end;
  CurrentModule:=Nil;  
  FModified:=False;
end;

Function TPackageEditor.FindPackageNode(P : TDomElement) : TNodeTreeItem;

Var
  N : TNodeTreeItem;
  G : TFPgtkWidgetGroup;
  I : Integer;
  
begin
  Result:=Nil;
  G:=FModuleTree.Tree.Children;
  I:=0;
  While (Result=Nil) and (I<G.Count) do
    begin
    If G.Items[i] is TNodeTreeItem then
      begin
      If TNodeTreeItem(G.items[i]).Node=P then 
        Result:=TNodeTreeItem(G.items[i]);
      end
    else
      Writeln('Child ',i,' of tee is not a node :',G.Items[i].ClassName);
    Inc(I);  
    end;  
  If (Result=Nil) then
    Raise Exception.CreateFmt(SErrNoNodeForPackage,[P['name']]);
end;

Function TPackageEditor.FindModuleNodeInNode(M : TDomElement; N : TNodeTreeItem) : TNodeTreeItem;

Var
  SN : TNodeTreeItem;
  G : TFPgtkWidgetGroup;
  I : Integer;
  
begin
  Result:=Nil;
  If (N<>Nil) then
    SN:=N
  else  
    SN:=FindPackageNode(M.ParentNode as TDomElement);
  If Assigned(SN) and Assigned(SN.SubTree) Then  
    begin
    G:=(SN.SubTree as TFpGtkTree).Children;
    I:=0;
    While (Result=Nil) and (I<G.Count) do
      begin
      If G.Items[i] is TNodeTreeItem then
        begin
        If TNodeTreeItem(G.items[i]).Node=M then 
          Result:=TNodeTreeItem(G.items[i]);
        end
      else
        Writeln('Child ',i,' of tree is not a node :',G.Items[i].ClassName);
      Inc(I);  
      end;  
    end;   
  If (Result=Nil) then
    Raise Exception.CreateFmt(SErrNoNodeForModule,[M['name']]);
end;

Function TPackageEditor.FindElementNode(E : TDomElement; N : TNodeTreeItem) : TNodeTreeItem;

Var
  SN : TNodeTreeItem;
  G : TFPgtkWidgetGroup;
  I : Integer;
  
begin
  Result:=Nil;
  If (N<>Nil) then
    SN:=N
  else  
    SN:=FModuleNode; // FindModuleNodeInNode(E.ParentNode as TDomElement,Nil);
  If E.NodeName='module' then
    Result:=FModuleNode
  else If Assigned(SN) and Assigned(SN.SubTree) Then  
    begin
    G:=(SN.SubTree as TFpGtkTree).Children;
    I:=0;
    While (Result=Nil) and (I<G.Count) do
      begin
      If G.Items[i] is TNodeTreeItem then
        begin
        If TNodeTreeItem(G.items[i]).Node=E then 
          Result:=TNodeTreeItem(G.items[i]);
        end
      else
        Writeln('Child ',i,' of tree is not a node :',G.Items[i].ClassName);
      Inc(I);  
      end;  
    end;   
  If (Result=Nil) then
    Raise Exception.CreateFmt(SErrNoNodeForElement,[E['name']]);
end;

Procedure TPackageEditor.AddElement(E : TDomElement);

Var 
  N : TNodeTreeItem;

begin
//  Writeln('Adding element ',E['name']);
  N:=CreateElementNode(E);
//  Writeln('Appending node element ',N.Node['name']);
  (FModuleNode.Subtree as TFpGtKTree).Append(N);
//  Writeln('Setting current node');
  CurrentElement:=E;
  FModified:=True;
end;

Procedure TPackageEditor.SetCurrentElement(E : TDomElement);

Var 
  N : TNodeTreeItem;

begin
//  Writeln('In setcurrentelement');
  If (E<>FCurrentElement) then
    begin
    If Assigned(FCurrentElement) then
      begin
      N:=FindElementNode(FCurrentElement,Nil);
      If Assigned(N) then
        N.Deselect;
      end;  
    CurrentModule:=E.ParentNode as TDomElement;
    N:=FindElementNode(E,Nil);
    With (FModuleNode.Subtree as TFPgtkTree) do
      SelectChild(N);
    end;
end;

Procedure TPackageEditor.SetCurrentModule(Value : TDomElement);

Var
  N1,N2 : TNodeTreeItem;

begin
  If (FCurrentModule<>Value) then
    begin
    FCurrentModule:=Value;
    If Assigned(Value) then
      begin
      FCurrentPackage:=Value.ParentNode as TDomElement;
      N1:=FindPackageNode(FCurrentPackage);
      N1.Expand;
      N2:=FindModuleNodeInNode(FCurrentModule,N1);
      (N1.SubTree as TFPgtkTree).SelectChild(N2);
      end
    Else
      ClearElements;  
    end;   
end;


Procedure TPackageEditor.SetCurrentPackage(Value : TDomElement);

Var
  N : TNodeTreeItem;

begin
  FCurrentPackage:=Value;
  N:=FindPackageNode(Value);
  N.Expand;
  N.Select;
end;


{ ---------------------------------------------------------------------
  TPageEditor
  ---------------------------------------------------------------------}

Constructor TEditorPage.Create;

begin
  Inherited Create;
  FPackages:=TPackageEditor.Create;
  FElement:=TElementEditor.Create;
//  Fpackages.OnSelectionChanged:=@SelectionChanged;
  FPackages.OnSelectElement:=@ElementSelected;
//  FPackages.OnDeselectElement:=@ElementDeSelected;
  Add1(FPackages);
  Add2(FElement);
end;



Procedure TEditorPage.ClearDocument;

begin
  if (FDocument<>nil) then
    begin
    FDocument.Free;
    FDocument:=Nil;
    end;
end;

Procedure TEditorPage.LoadFromFile(FN : String);

Var
  F : TFileStream;

begin
  ClearDocument;
  F:=TFileStream.Create(FN,fmOpenRead);
  Try
    SetFileName(FN);
    ReadXMLFile(FDocument,F);
    DisplayDocument;
  finally
    F.Free;
  end;
end;

Procedure TEditorPage.LoadFromStream(S : TStream);

begin
  ClearDocument;
  ReadXMLFile(FDocument,S);
  SetFileName(SNewDocument);
  DisplayDocument;
end;

Procedure TEditorPage.SetFileName(FN : String);

begin
  FFileName:=FN;
  FFileNameLabel.Text:=ChangeFileExt(ExtractFileName(FN),'');
end;

Function TEditorPage.MakeBackup(FN : String) : Boolean;

Var
  BN : String;

begin
  Result:=Not CreateBackup;
  If not Result then
    begin
    BN:=ChangeFileExt(FN,BackupExtension);
    Result:=RenameFile(FN,BN);
    end;
end;

Procedure TEditorPage.SaveToFile(FN : String);

begin
  If FElement.Modified then
    FElement.Save;
  If (not FileExists(FN)) or MakeBackup(FN) then
    begin
    WriteXMLFile(FDocument,FN);
    if (FN<>FFileName) then
      SetFileName(FN);
    end;  
end;

Procedure TEditorPage.DisplayDocument;

begin
  FPackages.DescriptionNode:=FDocument.DocumentElement;
end;

{
Procedure TEditorPage.SelectionChanged(Sender : TFPgtkWidget; Node : TDomElement) ;

//Procedure TEditorPage.SelectionChanged(Sender : TObject; Node : TDomElement) ;

Var
  OldNode : TDomElement;

begin
  If Not FChangingSelection then
    begin
    FChangingSelection:=True;
    Try
      OldNode:=FElement.Element;
      If (OldNode<>Node) then
        begin
        FElement.Element:=Node;
        {
          if the switch didn't succeed, it means that something went wrong
          when saving the old node, so we reselect the old node.
        } 
        If FElement.Element<>Node then
          begin
          FPackages.CurrentElement:=OldNode;
          end;
        end; 
    Finally
      FChangingSelection:=False;
    end;   
    end;
end;
}

Procedure TEditorPage.ElementSelected(Sender : TObject; Node : TDomElement) ;

Var
  OldNode : TDomElement;

begin
//  Writeln('In ElementSelected');
  OldNode:=FElement.Element;
  If OldNode<>Node then
    begin
    FElement.Element:=Node;
    {
      if the switch didn't succeed, it means that something went wrong
      when saving the old node, so we reselect the old node.
    } 
{    If FElement.Element<>Node then
      FPackages.CurrentElement:=OldNode;
}    end; 
end;

{
Procedure TEditorPage.ElementDeselected(Sender : TObject; Node : TDomElement) ;

begin
  With FElement do
    If Modified then
      Save;
end;
}

Procedure TEditorPage.InsertTag(TagType : TTagType);

begin
  FElement.InsertTag(TagType)
end;

Procedure TEditorPage.InsertLink(LinkTarget,LinkText : String);

begin
  FElement.InsertLink(LinkTarget,LinkText);
end;


Procedure TEditorPage.InsertTable(Cols,Rows : Integer; UseHeader : Boolean);

begin
  Felement.InsertTable(Cols,Rows,UseHeader);
end;

Function TEditorPage.GetCurrentSelection : String;

begin
  Result:=FElement.CurrentSelection;
end;

Procedure TEditorPage.NewPackage(APackageName : String);

Var
  P : TDomElement;

begin
  P:=FDocument.CreateElement('package');
  P['name']:=APAckageName;
  FDocument.DocumentElement.AppendChild(P);
  FPackages.Refresh;
  FPackages.FModified:=True;
  CurrentPackage:=P;
end;

Function TEditorPage.FirstPackage : TDomElement;

Var
  N : TDomNode;

begin
  N:=FDocument.DocumentElement.FirstChild;
  While (N<>Nil) and 
        Not ((N.NodeType=ELEMENT_NODE) and 
             (N.NodeName='package')) do
    N:=N.NextSibling;
  Result:=TDomElement(N);
end;

Function TEditorPage.FirstModule(APackage : TDomElement) : TDomElement;

Var 
  N : TDomNode;

begin
  N:=APAckage.FirstChild;
  While (N<>Nil) and 
        Not ((N.NodeType=ELEMENT_NODE) and 
               (N.NodeName='module')) do
      N:=N.NextSibling;
  Result:=TDomElement(N);
end;

Procedure TEditorPage.NewModule(AModuleName : String);

Var 
  M,P : TDomElement;
  
begin
  If CurrentPackage<>Nil then
    P:=CurrentPackage
  else
    P:=FirstPackage;
  If (P=Nil) then  
    Raise Exception.CreateFmt(SErrNoPackageForModule,[AModuleName]);
  M:=FDocument.CreateElement('module');
  M['name']:=AModuleName;
  P.AppendChild(M);
  FPackages.Refresh;
  FPackages.FModified:=True;
  CurrentModule:=M;
end;

Procedure TEditorPage.NewElement(AElementName : String);

Var 
  P,E,M : TDomElement;
  N : TDomNode;
  
begin
  If CurrentModule<>Nil then
    M:=CurrentModule
  else
    begin
    P:=FirstPackage;
    If P<>Nil then
      M:=FirstModule(P)
    else
      M:=Nil;
    If M<>Nil then
      CurrentModule:=M;  
    end;  
  If (M=Nil) then  
    Raise Exception.CreateFmt(SErrNoModuleForElement,[AElementName]);
  E:=FDocument.CreateElement('module');
  E['name']:=AElementName;
  M.AppendChild(E);
  FPackages.AddElement(E);
end;

Function TEditorPage.GetCurrentPackage : TDomElement;

begin
  Result:=FPackages.CurrentPackage;
end;


Function TEditorPage.GetCurrentModule : TDomElement;

begin
  Result:=FPackages.CurrentModule;
end;


Function TEditorPage.GetCurrentElement : TDomElement;
begin
  Result:=FElement.Element;
end;

Procedure TEditorPage.SetCurrentElement(Value : TDomElement);

begin
  FPackages.CurrentElement:=Value;
end;


Procedure TEditorPage.SetCurrentModule(Value : TDomElement);

begin
  FPackages.CurrentModule:=Value;
end;


Procedure TEditorPage.SetCurrentPackage(Value : TDomElement);

begin
  FPackages.CurrentPackage:=Value;
end;

Procedure TEditorPage.SetModified(Value : Boolean);

begin
end;

Function TEditorPage.GetModified : Boolean;

begin
  Result:=FPackages.Modified or FElement.Modified;
end;

Procedure TEditorPage.GetElementList(List : TStrings);

Var
  N : TDOmNode;

begin
 With List do
   begin
   Clear;
   If Assigned(CurrentModule) then
     begin
     N:=Currentmodule.FirstChild;
     While (N<>Nil) do
       begin
       If (N is TDomElement) and (N.NodeName='element') then
         Add(TDomElement(N)['name']);
       N:=N.NextSibling;  
       end;
     end;  
   end;
end;

end.
