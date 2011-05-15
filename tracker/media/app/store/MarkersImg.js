Ext.define('Tracker.store.MarkersImg', {
    extend : 'Ext.data.Store',
    
    model: 'Tracker.model.MarkerImg',
    proxy: {
        type: 'ajax',
        url : '/listmarkers/',
        actionMethods: {
            create : 'POST',
            read   : 'POST',
            update : 'POST',
            destroy: 'POST'
        }
    },
    autoLoad: true
});