Ext.define('Tracker.model.CurrentGeo', {
    extend: 'Ext.data.Model',
    idProperty: 'id',
    fields: [
    {
        type: 'int',
        name: 'id'
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