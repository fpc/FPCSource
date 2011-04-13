Ext.ns("fpWeb");
fpWeb.LoginForm = Ext.extend (Ext.Window, {
  /* Control references */ 
  blogin : null,
  eusername : null,
  epassword : null,
  plock : null,
  fform : null,
  /* Callbacks */
  OnLogin : function (Provider,Response) {
    if (!Ext.isEmpty(Response.error)) {
      Ext.Msg.show({
        title : 'Login failed',
        msg : 'An error occurred during login: '+Response.error.message+'. Please try again.',
        icon : Ext.Msg.ERROR,
        buttons : Ext.Msg.OK
      });
    } else if (Response.result > 0) {
      // here code to switch to data editing
       window.location='users.html';
/*
       Ext.Msg.show({
        title : 'Login OK',
        msg : 'Your username/pasword was accepted. We will now proceed to the editing form',
        icon : Ext.Msg.ERROR,
        buttons : Ext.Msg.OK
    });
*/
    } else {
       Ext.Msg.show({
        title : 'Login failed',
        msg : 'Your username/pasword is incorrect. Please try again.',
        icon : Ext.Msg.ERROR,
        buttons : Ext.Msg.OK
    });
    }
  },
  loginbuttonclick : function (sender) {
    SessionManagement.Login(this.eusername.getValue(), this.epassword.getValue(),this.OnLogin.createDelegate(this));
  },
  focususer : function () {
    this.eusername.focus();
  },
  /* Build the actual form */
  constructor : function (config) {
    this.eusername = new Ext.form.TextField({
      name:"user",
      fieldLabel:"Login",
      inputType:"text"
    });
    this.epassword = new Ext.form.TextField({
      name:"pass",
      fieldLabel:"Password",
      inputType:"password"
    });
    this.blogin = new Ext.Button({
       text:"Login",
       handler : this.loginbuttonclick,
       scope : this
    });
    this.fform = new Ext.form.FormPanel({
      width: 350,
      labelWidth:150,
      border:false,
      xtype: "form",
      buttonAlign: "right",
      bodyStyle: "padding: 10px 15px",
      defaultType: "textfield",
      defaults: {width: 150},
      items: [this.eusername,this.epassword],
      buttons:[this.blogin],
      keys: {key: Ext.EventObject.ENTER,  
             handler: function(){  
               this.blogin.focus(); 
             }, 
             scope: this
      }
    });
    this.plock = new Ext.Panel({ 
      border:false,
      html:"<img src='login.png' width=114 height=128/>",
      width:114,
      height:128
    });
    Ext.apply(config, {
      title: "Login",
      width: 500,
      height: 200,
      plain: true,
      layout: "hbox",
      defaultButton: this.eusername,
      layoutConfig: {
        align : "middle",
        pack: "center"
      },
      closable: false,
      listeners: {
        'show' : { fn: this.focususer.createDelegate(this) }
      },
      items: [ this.fform, this.plock ]  
    });
    fpWeb.LoginForm.superclass.constructor.call(this,config);
  } /* constructor*/
});

