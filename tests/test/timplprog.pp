{ %FAIL }
uses uimpluni1;

    Type
      BEC_Single_Error = record
         E : integer;
         M : string [80];
      end;

    Const
      BEC_Err_Msgs: array [0..1] of BEC_Single_Error =
       ((E : uimpluni1.ICanUseThis;  M : '[1] No Error'),
        (E : uimpluni2.ICantUseThis; M : '[10000] A Bug?'));

begin
end.
