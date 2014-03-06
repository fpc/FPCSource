Ext.require(['Ext.data.*', 'Ext.grid.*']);

Ext.define('Person', {
    extend: 'Ext.data.Model',
    fields: [{
        name: 'ID',
        type: 'int',
        useNull: true
    }, 'LOGIN', 'NAME', 'EMAIL'],
    validations: [{
        type: 'length',
        field: 'LOGIN',
        min: 1
    }, {
        type: 'length',
        field: 'NAME',
        min: 1
    }, {
        type: 'length',
        field: 'EMAIL',
        min: 1
    }]
});

Ext.onReady(function(){

    var store = Ext.create('Ext.data.Store', {
        autoLoad: true,
        autoSync: true,
        model: 'Person',
        proxy: {
            type: 'ajax',

			api: {
				  read: "/cgi-bin/extgrid.exe/Provider/Read/",
				  update: "/cgi-bin/extgrid.exe/Provider/Update/",
				  create: "/cgi-bin/extgrid.exe/Provider/Insert/",
				  destroy: "/cgi-bin/extgrid.exe/Provider/Delete/"
			},

            reader: {
                type: 'json',
                successProperty: 'success',
                root: 'rows',
                messageProperty: 'message'
            },
            writer: {
                type: 'json',
				encode: true,
                //writeAllFields: false,
                root: 'rows'
            }
        },
        listeners: {
            write: function(store, operation){
                var record = operation.getRecords()[0],
                    name = Ext.String.capitalize(operation.action),
                    verb;
                    
                if (name == 'Destroy') {
                    record = operation.records[0];
                    verb = 'delete';
                } else if (name == 'Update') {
                    verb = 'update';
                } else if (name == 'Create') {
                    verb = 'create';
                }
				Ext.example.msg(verb, Ext.String.format("{0} user: {1}", verb, record.get('NAME')));
            }
        }
    });
    
    var rowEditing = Ext.create('Ext.grid.plugin.RowEditing');
    
    var grid = Ext.create('Ext.grid.Panel', {
        renderTo: document.body,
        plugins: [rowEditing],
        width: 400,
        height: 300,
        frame: true,
        title: 'Users',
        store: store,
        iconCls: 'icon-user',
        columns: [
			{
            text: 'LOGIN',
            flex: 1,
            sortable: true,
            dataIndex: 'LOGIN',
            field: {
                xtype: 'textfield'
            }
        }, {
            header: 'NAME',
            width: 80,
            sortable: true,
            dataIndex: 'NAME',
            field: {
                xtype: 'textfield'
            }
        }, {
            text: 'EMAIL',
            width: 80,
            sortable: true,
            dataIndex: 'EMAIL',
            field: {
                xtype: 'textfield'
            }
        }, {
            text: 'LASTLOGIN',
            width: 80,
            sortable: true,
            dataIndex: 'LASTLOGIN',
            field: {
                xtype: 'textfield'
            }
        }],
        dockedItems: [{
            xtype: 'toolbar',
            items: [{
                text: 'Add',
                iconCls: 'icon-add',
                handler: function(){
                    // empty record
                    store.insert(0, new Person());
                    rowEditing.startEdit(0, 0);
                }
            }, '-', {
                itemId: 'delete',
                text: 'Delete',
                iconCls: 'icon-delete',
                disabled: true,
                handler: function(){
                    var selection = grid.getView().getSelectionModel().getSelection()[0];
                    if (selection) {
                        store.remove(selection);
                    }
                }
            }]
        }]
    });
    grid.getSelectionModel().on('selectionchange', function(selModel, selections){
        grid.down('#delete').setDisabled(selections.length === 0);
    });
});
