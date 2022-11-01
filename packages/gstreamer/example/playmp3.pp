program playmp3;
{$mode objfpc}
{$h+}

uses sysutils, glib2, gst;

Var
  pipeline : PGstElement = Nil;
  filesrc : PGstElement = Nil;
  msg: PGstMessage = Nil;
  bus : PGstBus = Nil;
  msgError : PGError = Nil;
  error : PGError = Nil;
  dbg : PChar;
  FN : String;


begin
  if (ParamCount<>1) then
    begin
    Writeln ('usage: ',ExtractFileName(ParamSTr(0)),' <filename>');
    Halt(1);
    end;
  gst_init (@argc, @argv);


  pipeline:=gst_parse_launch ('filesrc name=my_filesrc !  mpegaudioparse ! mpg123audiodec ! audioconvert ! audioresample ! pulsesink', @error);
  if (pipeline=nil) then
    begin
    Writeln('Parse error: ', error^.message);
    Halt(2);
    end;

  filesrc := gst_bin_get_by_name (GST_BIN (pipeline), 'my_filesrc');
  FN:=ParamStr(1);
  g_object_set (filesrc, 'location', PChar(FN), NULL);
  g_object_unref (filesrc);

  gst_element_set_state (pipeline, GST_STATE_PLAYING);

  bus := gst_element_get_bus (pipeline);

  {* wait until we either get an EOS or an ERROR message. Note that in a real
   * program you would probably not use gst_bus_poll(), but rather set up an
   * async signal watch on the bus and run a main loop and connect to the
   * bus's signals to catch certain messages or all messages }

  msg:=gst_bus_poll (bus, TGstMessageType(Ord(GST_MESSAGE_EOS) or Ord (GST_MESSAGE_ERROR)), -1);

  case GST_MESSAGE_TYPE (msg) of
    GST_MESSAGE_EOS:
      Writeln('EOS');
    GST_MESSAGE_ERROR:
      begin
      gst_message_parse_error (msg, @msgError, @dbg);
      if (MsgError<>Nil) then
        begin
        Writeln('ERROR: ', MsgError^.message);
        g_error_free (MsgError);
        end;
      if (dbg<>Nil) then
        begin
        writeln('[Debug details: [', dbg,']');
        g_free (dbg);
        end;
      end;
  else
    Writeln('Unexpected message of type', GST_MESSAGE_TYPE (msg));
  end;

  gst_message_unref (msg);

  gst_element_set_state (pipeline, GST_STATE_NULL);
  gst_object_unref (pipeline);
  gst_object_unref (bus);


end.

