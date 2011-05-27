Ext.ns('fpWeb');

fpWeb.ShowPage = function () {
  var myproxy = new Ext.data.HttpProxy ( {
    api : {
      read: "combined.cgi/Provider/Users/Read/",
      update: "combined.cgi/Provider/Users/Update/",
      create: "combined.cgi/Provider/Users/Insert/",
      destroy: "combined.cgi/Provider/Users/Delete/"
    }
  });
  var myreader = new Ext.data.JsonReader ({
      root: "rows",
      successProperty : 'success',
      idProperty: "U_ID",
      messageProperty: 'message', // Must be specified here
      fields: ["U_ID","U_LOGIN","U_NAME","U_EMAIL", "U_PASSWORD"]
  });
  var mywriter = new Ext.data.JsonWriter({
      encode: true,
      writeAllFields: true,
      idProperty: "U_ID"
  }); 
  var data = new Ext.data.Store({
    proxy: myproxy,
    reader: myreader,
    writer: mywriter,
    autoSave: false,
    idProperty: "U_ID",
  });
  // Listen to errors.
  data.addListener('exception', function(proxy, type, action, options, res) {
    if (type === 'remote') {
        Ext.Msg.show({
            title: 'REMOTE EXCEPTION',
            msg: res.message, 
            icon: Ext.MessageBox.ERROR,
            buttons: Ext.Msg.OK
        });
    }
  });
  data.load({ params:{start: 0, limit: 30}});
  var grid = new Ext.grid.EditorGridPanel({
    renderTo: Ext.getBody(),
    frame: true,
    title: "Known users",
    height: 600,
    width: 800,
    store: data,
    columns: [
      {header: 'ID', dataIndex: "U_ID", sortable: true, hidden: true},
      {header: 'Login', dataIndex: "U_LOGIN", sortable: true, editor: new Ext.form.TextField({allowBlank: false})},
      {header: 'Name', dataIndex: "U_NAME", sortable: true, editor: new Ext.form.TextField({allowBlank: false}), width : 200},
      {header: 'Email', dataIndex: "U_EMAIL", sortable: true, editor: new Ext.form.TextField({allowBlank: false}), width : 200},
      {header: 'Password', dataIndex: "U_PASSWORD", sortable: true, editor: new Ext.form.TextField()},
    ],
    bbar: new Ext.PagingToolbar({
      pageSize: 30,
      store: data,
      displayInfo: true
    }),
    tbar : [{
            text: 'Add',
            iconCls: 'icon-add',
            handler: function(btn, ev) {
              var u = new grid.store.recordType();
              grid.stopEditing();
              grid.store.insert(0, u);
              grid.startEditing(0, 1);
            },
            scope: grid
        }, '-', {
            text: 'Delete',
            iconCls: 'icon-delete',
            handler: function(btn, ev) {
	      var index = grid.getSelectionModel().getSelectedCell();
	      if (!index) {
		  return false;
	      }
	      var rec = grid.store.getAt(index[0]);
	      grid.store.remove(rec);
	      },
            scope: grid
        }, '-', {
            text: 'Save',
            iconCls: 'icon-save',
            handler: function(btn, ev) {
	      grid.store.save();
	    },
            scope: grid
        },'->', {
          text: 'Log out',
          iconCls: 'logout',
          handler: function () {
            SessionManagement.Logout(function (provider,response) {
              if (response.result=='Bye') {
                window.location='combined.html';
              }
            });
          }
        }
	]
 //          F.ContentToStream(M);

  });
  grid.show();
}
Ext.onReady(fpWeb.ShowPage);
