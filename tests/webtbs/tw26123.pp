{ %NORUN }

program tw26123;

{$mode objfpc}

type
    generic TNode<data_type> = class // anything can go in this class
    end;

    generic TLinkedList<data_type> = class
      type
          specialized_TNode = specialize TNode<data_type>;
      public
        node : specialized_TNode;
    end;


    generic TExtendedLinkedList<data_type> = class (specialize TLinkedList<data_type>)
      public
        last_node : specialized_TNode;
    end;

begin
end.
