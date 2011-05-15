Ext.define('Tracker.model.Device', {
    extend: 'Ext.data.Model',
    fields: ['device_id', 'marker_id', 'imei', 'name', 'text', 'long', 'lat', 'ts_time']
});