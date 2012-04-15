{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }
{ %opt=norun }

{$mode objfpc}
{$modeswitch objectivec1}

program tobjc39;
uses
  uobjc39;
	
type
 UITextInputProtocol = objcprotocol external name 'UITextInput'
 required
   procedure setInputDelegate(newValue: UITextInputDelegateProtocol); message 'setInputDelegate:';
   function inputDelegate: UITextInputDelegateProtocol; message 'inputDelegate';
   function tokenizer: UITextInputTokenizerProtocol; message 'tokenizer';
 end;

type
 UITextInputDelegateProtocol = objcprotocol external name 'UITextInputDelegate'
 end;

type
 UITextInputTokenizerProtocol = objcprotocol external name 'UITextInputTokenizer'
 end;

type
 UITextField = objcclass external (UITextInputProtocol)
 public
   procedure setInputDelegate(newValue: UITextInputDelegateProtocol); message 'setInputDelegate:';
   function inputDelegate: UITextInputDelegateProtocol; message 'inputDelegate';
   function tokenizer: UITextInputTokenizerProtocol; message 'tokenizer';
 end;

begin

end.
