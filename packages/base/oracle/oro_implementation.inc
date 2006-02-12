
//implementation

  { was #define dname def_expr }
  function OCI_IND_NOTNULL : OCIInd;
      begin
         OCI_IND_NOTNULL:=OCIInd(0);
      end;

  { was #define dname def_expr }
  function OCI_IND_NULL : OCIInd;
      begin
         OCI_IND_NULL:=OCIInd(-(1));
      end;

  { was #define dname def_expr }
  function OCI_IND_BADNULL : OCIInd;
      begin
         OCI_IND_BADNULL:=OCIInd(-(2));
      end;

  { was #define dname def_expr }
  function OCI_IND_NOTNULLABLE : OCIInd;
      begin
         OCI_IND_NOTNULLABLE:=OCIInd(-(3));
      end;

  { was #define dname def_expr }
  function OCI_DURATION_BEGIN : OCIDuration;
      begin
         OCI_DURATION_BEGIN:=OCIDuration(10);
      end;

  { was #define dname def_expr }
  function OCI_DURATION_NULL : OCIDuration;
      begin
         OCI_DURATION_NULL:=OCIDuration(OCI_DURATION_BEGIN-1);
      end;

  { was #define dname def_expr }
  function OCI_DURATION_DEFAULT : OCIDuration;
      begin
         OCI_DURATION_DEFAULT:=OCIDuration(OCI_DURATION_BEGIN-2);
      end;

  { was #define dname def_expr }
  function OCI_DURATION_USER_CALLBACK : OCIDuration;
      begin
         OCI_DURATION_USER_CALLBACK:=OCIDuration(OCI_DURATION_BEGIN-3);
      end;

  { was #define dname def_expr }
  function OCI_DURATION_NEXT : OCIDuration;
      begin
         OCI_DURATION_NEXT:=OCIDuration(OCI_DURATION_BEGIN-4);
      end;

  { was #define dname def_expr }
  function OCI_DURATION_SESSION : OCIDuration;
      begin
         OCI_DURATION_SESSION:=OCIDuration(OCI_DURATION_BEGIN);
      end;

  { was #define dname def_expr }
  function OCI_DURATION_TRANS : OCIDuration;
      begin
         OCI_DURATION_TRANS:=OCIDuration(OCI_DURATION_BEGIN+1);
      end;

  { was #define dname def_expr }
  function OCI_DURATION_CALL : OCIDuration;
      begin
         OCI_DURATION_CALL:=OCIDuration(OCI_DURATION_BEGIN+2);
      end;

  { was #define dname def_expr }
  function OCI_DURATION_STATEMENT : OCIDuration;
      begin
         OCI_DURATION_STATEMENT:=OCIDuration(OCI_DURATION_BEGIN+3);
      end;

  { was #define dname def_expr }
  function OCI_DURATION_CALLOUT : OCIDuration;
      begin
         OCI_DURATION_CALLOUT:=OCIDuration(OCI_DURATION_BEGIN+4);
      end;

  { was #define dname def_expr }
  function OCI_DURATION_PROCESS : OCIDuration;
      begin
         OCI_DURATION_PROCESS:=OCIDuration(OCI_DURATION_BEGIN-5);
      end;

  { was #define dname def_expr }
  function OCI_OBJECTCOPY_NOREF : ub1;
      begin
         OCI_OBJECTCOPY_NOREF:=ub1($01);
      end;

  { was #define dname def_expr }
  function OCI_OBJECTFREE_FORCE : ub2;
      begin
         OCI_OBJECTFREE_FORCE:=ub2($0001);
      end;

  { was #define dname def_expr }
  function OCI_OBJECTFREE_NONULL : ub2;
      begin
         OCI_OBJECTFREE_NONULL:=ub2($0002);
      end;

  { was #define dname def_expr }
  function OCI_OBJECTFREE_HEADER : ub2;
      begin
         OCI_OBJECTFREE_HEADER:=ub2($0004);
      end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_OBJECT_IS_UPDATED(flag : longint) : longint;
    begin
//       OCI_OBJECT_IS_UPDATED:=bit(flag,OCI_OBJECT_UPDATED);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_OBJECT_IS_DELETED(flag : longint) : longint;
    begin
//       OCI_OBJECT_IS_DELETED:=bit(flag,OCI_OBJECT_DELETED);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_OBJECT_IS_NEW(flag : longint) : longint;
    begin
//       OCI_OBJECT_IS_NEW:=bit(flag,OCI_OBJECT_NEW);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_OBJECT_IS_DIRTY(flag : longint) : longint;
    begin
//       OCI_OBJECT_IS_DIRTY:=bit(flag,(OCI_OBJECT_UPDATED or OCI_OBJECT_NEW) or OCI_OBJECT_DELETED);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_INLINE(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_INLINE:=bit(flag,OCI_TYPEMETHOD_INLINE);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_CONSTANT(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_CONSTANT:=bit(flag,OCI_TYPEMETHOD_CONSTANT);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_VIRTUAL(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_VIRTUAL:=bit(flag,OCI_TYPEMETHOD_VIRTUAL);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_CONSTRUCTOR(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_CONSTRUCTOR:=bit(flag,OCI_TYPEMETHOD_CONSTRUCTOR);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_DESTRUCTOR(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_DESTRUCTOR:=bit(flag,OCI_TYPEMETHOD_DESTRUCTOR);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_OPERATOR(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_OPERATOR:=bit(flag,OCI_TYPEMETHOD_OPERATOR);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_SELFISH(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_SELFISH:=bit(flag,OCI_TYPEMETHOD_SELFISH);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_MAP(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_MAP:=bit(flag,OCI_TYPEMETHOD_MAP);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_ORDER(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_ORDER:=bit(flag,OCI_TYPEMETHOD_ORDER);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_RNDS(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_RNDS:=bit(flag,OCI_TYPEMETHOD_RNDS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_WNDS(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_WNDS:=bit(flag,OCI_TYPEMETHOD_WNDS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_RNPS(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_RNPS:=bit(flag,OCI_TYPEMETHOD_RNPS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_WNPS(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_WNPS:=bit(flag,OCI_TYPEMETHOD_WNPS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_ABSTRACT(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_ABSTRACT:=bit(flag,OCI_TYPEMETHOD_ABSTRACT);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_OVERRIDING(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_OVERRIDING:=bit(flag,OCI_TYPEMETHOD_OVERRIDING);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_PIPELINED(flag : longint) : longint;
    begin
//       OCI_METHOD_IS_PIPELINED:=bit(flag,OCI_TYPEMETHOD_PIPELINED);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_INLINE(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_INLINE:=bit(flag,OCI_TYPEMETHOD_INLINE);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_CONSTANT(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_CONSTANT:=bit(flag,OCI_TYPEMETHOD_CONSTANT);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_VIRTUAL(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_VIRTUAL:=bit(flag,OCI_TYPEMETHOD_VIRTUAL);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_CONSTRUCTOR(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_CONSTRUCTOR:=bit(flag,OCI_TYPEMETHOD_CONSTRUCTOR);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_DESTRUCTOR(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_DESTRUCTOR:=bit(flag,OCI_TYPEMETHOD_DESTRUCTOR);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_OPERATOR(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_OPERATOR:=bit(flag,OCI_TYPEMETHOD_OPERATOR);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_SELFISH(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_SELFISH:=bit(flag,OCI_TYPEMETHOD_SELFISH);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_MAP(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_MAP:=bit(flag,OCI_TYPEMETHOD_MAP);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_ORDER(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_ORDER:=bit(flag,OCI_TYPEMETHOD_ORDER);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_RNDS(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_RNDS:=bit(flag,OCI_TYPEMETHOD_RNDS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_WNDS(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_WNDS:=bit(flag,OCI_TYPEMETHOD_WNDS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_RNPS(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_RNPS:=bit(flag,OCI_TYPEMETHOD_RNPS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_WNPS(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_WNPS:=bit(flag,OCI_TYPEMETHOD_WNPS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_ABSTRACT(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_ABSTRACT:=bit(flag,OCI_TYPEMETHOD_ABSTRACT);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_OVERRIDING(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_OVERRIDING:=bit(flag,OCI_TYPEMETHOD_OVERRIDING);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_PIPELINED(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_IS_PIPELINED:=bit(flag,OCI_TYPEMETHOD_PIPELINED);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_INLINE(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_SET_INLINE:=bis(flag,OCI_TYPEMETHOD_INLINE);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_CONSTANT(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_SET_CONSTANT:=bis(flag,OCI_TYPEMETHOD_CONSTANT);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_VIRTUAL(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_SET_VIRTUAL:=bis(flag,OCI_TYPEMETHOD_VIRTUAL);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_CONSTRUCTOR(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_SET_CONSTRUCTOR:=bis(flag,OCI_TYPEMETHOD_CONSTRUCTOR);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_DESTRUCTOR(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_SET_DESTRUCTOR:=bis(flag,OCI_TYPEMETHOD_DESTRUCTOR);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_OPERATOR(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_SET_OPERATOR:=bis(flag,OCI_TYPEMETHOD_OPERATOR);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_SELFISH(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_SET_SELFISH:=bis(flag,OCI_TYPEMETHOD_SELFISH);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_MAP(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_SET_MAP:=bis(flag,OCI_TYPEMETHOD_MAP);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_ORDER(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_SET_ORDER:=bis(flag,OCI_TYPEMETHOD_ORDER);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_RNDS(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_SET_RNDS:=bis(flag,OCI_TYPEMETHOD_RNDS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_WNDS(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_SET_WNDS:=bis(flag,OCI_TYPEMETHOD_WNDS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_RNPS(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_SET_RNPS:=bis(flag,OCI_TYPEMETHOD_RNPS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_WNPS(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_SET_WNPS:=bis(flag,OCI_TYPEMETHOD_WNPS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_INLINE(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_CLEAR_INLINE:=bic(flag,OCI_TYPEMETHOD_INLINE);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_CONSTANT(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_CLEAR_CONSTANT:=bic(flag,OCI_TYPEMETHOD_CONSTANT);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_VIRTUAL(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_CLEAR_VIRTUAL:=bic(flag,OCI_TYPEMETHOD_VIRTUAL);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_CONSTRUCTOR(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_CLEAR_CONSTRUCTOR:=bic(flag,OCI_TYPEMETHOD_CONSTRUCTOR);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_DESTRUCTOR(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_CLEAR_DESTRUCTOR:=bic(flag,OCI_TYPEMETHOD_DESTRUCTOR);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_OPERATOR(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_CLEAR_OPERATOR:=bic(flag,OCI_TYPEMETHOD_OPERATOR);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_SELFISH(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_CLEAR_SELFISH:=bic(flag,OCI_TYPEMETHOD_SELFISH);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_MAP(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_CLEAR_MAP:=bic(flag,OCI_TYPEMETHOD_MAP);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_ORDER(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_CLEAR_ORDER:=bic(flag,OCI_TYPEMETHOD_ORDER);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_RNDS(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_CLEAR_RNDS:=bic(flag,OCI_TYPEMETHOD_RNDS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_WNDS(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_CLEAR_WNDS:=bic(flag,OCI_TYPEMETHOD_WNDS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_RNPS(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_CLEAR_RNPS:=bic(flag,OCI_TYPEMETHOD_RNPS);
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_WNPS(flag : longint) : longint;
    begin
//       OCI_TYPEMETHOD_CLEAR_WNPS:=bic(flag,OCI_TYPEMETHOD_WNPS);
    end;

  { was #define dname def_expr }
  function OCI_NUMBER_DEFAULTPREC : ub1;
      begin
         OCI_NUMBER_DEFAULTPREC:=ub1(0);
      end;

  { was #define dname def_expr }
  function OCI_NUMBER_DEFAULTSCALE : sb1;
      begin
         OCI_NUMBER_DEFAULTSCALE:=sb1(MAXSB1MINVAL);
      end;

  { was #define dname def_expr }
  function OCI_COHERENCY_NONE : OCIRefreshOpt;
      begin
         OCI_COHERENCY_NONE:=OCIRefreshOpt(2);
      end;

  { was #define dname def_expr }
  function OCI_COHERENCY_NULL : OCIRefreshOpt;
      begin
         OCI_COHERENCY_NULL:=OCIRefreshOpt(4);
      end;

  { was #define dname def_expr }
  function OCI_COHERENCY_ALWAYS : OCIRefreshOpt;
      begin
         OCI_COHERENCY_ALWAYS:=OCIRefreshOpt(5);
      end;


