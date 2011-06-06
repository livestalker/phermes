Ext.define('Tracker.model.CurrentGeo', {
    extend: 'Ext.data.Model',
    idProperty: 'device_id',
    fields: [
    {
        type: 'int',
        name: 'device_id'
    },
    {
        type: 'float',
        name: 'lat'
    }, 
    {
        type: 'float',
        name: 'lng'
    }, 
    {
        type: 'date',
        name: 'ts_time'
    }]
});