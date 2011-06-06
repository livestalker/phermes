Ext.define('Tracker.model.Device', {
    extend: 'Ext.data.Model',
    idProperty: 'device_id',
    fields: [
    {
        type: 'int',
        name: 'device_id'
    }
    ,
    {
        type: 'int',
        name: 'marker_id'
    }, 
    {
        type: 'string',
        name: 'imei'
    }, 
    {
        type: 'string',
        name: 'name'
    }, 
    {
        type: 'string',
        name: 'text'
    }]
});