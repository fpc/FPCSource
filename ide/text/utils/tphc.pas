uses Objects,WHelp,WTPHWriter;

var W: THelpFileWriter;
    HF: TOAHelpFile;
    P: PTopic;
const Ctx = 32;

BEGIN
  W.Init('TEST.TPH',1);
  P:=W.CreateTopic(Ctx);
  W.AddTopicToIndex('IndexEntry',P);
  W.AddLineToTopic(P,'Hello world!');
  W.AddLineToTopic(P,'This is a '+hscLink+'sample'+hscLink+' help file.');
  W.AddLineToTopic(P,'And this is it''s 3rd line...');
  W.AddLinkToTopic(P,Ctx+1);
  P:=W.CreateTopic(Ctx+1);
  W.AddTopicToIndex('IndexEntry2',P);
  W.AddLineToTopic(P,'And this is an other topic!');
  W.AddLineToTopic(P,'>>>Back to the '+hscLink+'previous topic'+hscLink+'...');
  W.AddLinkToTopic(P,Ctx);
  W.WriteFile;
  W.Done;

  HF.Init('TEST.TPH',1);
  HF.LoadIndex;
  P:=HF.LoadTopic(Ctx);
  HF.Done;
END.