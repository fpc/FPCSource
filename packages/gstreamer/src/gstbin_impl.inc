
Function GST_TYPE_BIN : TGType;

begin
  Result:=gst_bin_get_type ();
end;


function GST_IS_BIN(obj : Pointer) : boolean;
begin
  Result:=G_TYPE_CHECK_INSTANCE_TYPE (obj, GST_TYPE_BIN);
end;

Function GST_IS_BIN_CLASS(klass: Pointer) : Boolean;

begin
  Result:=G_TYPE_CHECK_CLASS_TYPE(klass, GST_TYPE_BIN);
end;

Function GST_BIN_GET_CLASS(obj : Pointer) : PGstBinClass;
begin
  Result:=PGstBinClass(G_TYPE_INSTANCE_GET_CLASS(obj, GST_TYPE_BIN))
end;

Function GST_BIN(Obj : Pointer) : PGstBin;

begin
  Result:=PGstBin(G_TYPE_CHECK_INSTANCE_CAST(obj, GST_TYPE_BIN))
end;

Function GST_BIN_CAST(obj : Pointer) : PGstBin;

begin
  Result:=PGstBin(obj)
end;

Function GST_BIN_CLASS(klass : Pointer) : PGstBinClass;

begin
   Result:=PGstBinClass(G_TYPE_CHECK_CLASS_CAST(klass,GST_TYPE_BIN));
end;
