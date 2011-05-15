Ext.define('Tracker.model.MarkerImg', {
    extend: 'Ext.data.Model',
    fields: [
    {
        type: 'int', 
        name: 'marker_id'
    },

    {
        type: 'int', 
        name: 'width', 
        defaultValue: 16
    },

    {
        type: 'int', 
        name: 'height', 
        defaultValue: 16
    },

    {
        type: 'string', 
        name: 'url'
    },

    {
        type: 'string', 
        name: 'name'
    }
    ]
});