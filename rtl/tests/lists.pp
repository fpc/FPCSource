unit lists;

interface

Type
  TLinkedListItem = Class
    Next : TLinkedListItem;
  end;
  TLinkedListItemClass = Class of TLinkedListItem;
  
  TLinkedList = Class
    Constructor Create(ItemClass : TLinkedListItemClass);
    Procedure Clear;
    Function Add : TLinkedListItem;
    Property Root : TLinkedListItem Read FRoot;
  end;

Implementation  

