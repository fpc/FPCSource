Ext.ns('FPC');

FPC.ShowPage = function () {
  var myproxy = new Ext.data.HttpProxy ( {
    api : {
      read: "extgrid.cgi/Provider/Read/name/?format=xml",
      update: "extgrid.cgi/Provider/Update/name/?format=xml",
      create: "extgrid.cgi/Provider/Insert/name/?format=xml",
      destroy: "extgrid.cgi/Provider/Delete/name/?format=xml",
    }
  });
  var myreader = new Ext.data.XmlReader ({
      totalProperty: "total",
      record: "row",
      idProperty: "ID",
      successProperty: "success",
      messageProperty: "message",
      fields: ["ID", "LOGIN", "NAME","EMAIL","LASTLOGIN"]
  });
  var mywriter = new Ext.data.XmlWriter({
      encode: true,
      writeAllFields: true,
      idProperty: "ID",
      root: "records"
  }); 
    var data = new Ext.data.Store({
    proxy: myproxy,
    reader: myreader,
    writer: mywriter,
    autoSave: false,
    idProperty: "ID",
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
      {header: 'ID', dataIndex: "ID", sortable: true, hidden: true},
      {header: 'Login', dataIndex: "LOGIN", sortable: true, editor: new Ext.form.TextField({allowBlank: false})},
      {header: 'Name', dataIndex: "NAME", sortable: true, editor: new Ext.form.TextField({allowBlank: false})},
      {header: 'Email', dataIndex: "EMAIL", sortable: true, editor: new Ext.form.TextField({allowBlank: false})},
      {header: 'Last login', dataIndex: "LASTLOGIN", sortable: true, editor: new Ext.form.TextField()},
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
        }
	]
  });
  grid.show();
}
Ext.onReady(FPC.ShowPage);
