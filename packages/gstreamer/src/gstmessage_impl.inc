
function gst_message_ref (msg : PGstMessage) : PGstmessage;

begin
   Result:=pGstMessage(gst_mini_object_ref(PGstMiniObject(msg)));
end;

procedure gst_message_unref (msg : PGstMessage);

begin
  gst_mini_object_unref(PGstMiniObject(msg));
end;


procedure gst_clear_message (msg_ptr : PPGstMessage);

var
  PP : PPGstMiniObject absolute msg_ptr;

begin
  if (msg_ptr<>Nil) then
     if PP^<>Nil then
       gst_mini_object_unref(PP^);
end;


function gst_message_copy (msg : PGstMessage) : PGstMessage;

begin
  Result:=PGstMessage(gst_mini_object_copy(PGstMiniObject(msg)));
end;


function gst_message_replace (old_message : PPGstMessage; new_message : PGstMessage) : gboolean;

begin
   Result:=gst_mini_object_replace (PPGstMiniObject(old_message), PGstMiniObject(new_message));
end;


Function gst_message_take (old_message : PPGstMessage; new_message : PGstMessage) :gboolean;
begin
   Result:=gst_mini_object_take(PPGstMiniObject(old_message),PGstMiniObject(new_message));
end;

Function GST_TYPE_MESSAGE : TGType;

begin
  Result:=_gst_message_type;
end;

function GST_IS_MESSAGE(obj : Pointer) : boolean;
begin
  Result:=G_TYPE_CHECK_INSTANCE_TYPE (obj, GST_TYPE_MESSAGE);
end;

Function GST_MESSAGE_CAST(obj : Pointer) : PGstMessage;

begin
  Result:=PGstMessage(obj)
end;


Function GST_MESSAGE(obj : Pointer) : PGstMessage;

begin
  Result:=GST_MESSAGE_CAST(obj);
end;

function GST_MESSAGE_GET_LOCK(message : PGstmessage): PGMutex;

begin
  Result:=@message^.lock;
end;

procedure GST_MESSAGE_LOCK(message : PGstMessage);

begin
  g_mutex_lock(GST_MESSAGE_GET_LOCK(message));
end;

procedure GST_MESSAGE_UNLOCK(message : PGstMessage);

begin
  g_mutex_unlock(GST_MESSAGE_GET_LOCK(message));
end;


function GST_MESSAGE_GET_COND(message : PGstmessage): PGCond;

begin
  Result:=@message^.cond;
end;


procedure GST_MESSAGE_WAIT(message : PGstMessage);

begin
  g_cond_wait(GST_MESSAGE_GET_COND(message),GST_MESSAGE_GET_LOCK(message))
end;

procedure GST_MESSAGE_SIGNAL(message : PGSTMessage);

begin
  g_cond_signal(GST_MESSAGE_GET_COND(message))
end;

Function GST_MESSAGE_TYPE(message : PGStMessage) : TGstMessageType;
begin
   Result:=message^._type;
end;


