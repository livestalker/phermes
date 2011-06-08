Ext.define('Tracker.store.MarkersImg', {
    extend : 'Ext.data.Store',
    
    model: 'Tracker.model.MarkerImg',
    proxy: {
        type: 'ajax',
        url : '/listmarkersimg/',
        actionMethods: {
            create : 'POST',
            read   : 'POST',
            update : 'POST',
            destroy: 'POST'
        },
        reader: {
            type: 'json',
            root: 'markersimg',
            successProperty: 'success'
        }
    }
});