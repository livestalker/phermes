Ext.Loader.setConfig({
    enabled:true, 
    disableCaching: false
});

Ext.Loader.setPath({
    'Tracker': '/media/app'
});

Ext.require([
    'Tracker.view.TrackerViewport',
    'Tracker.controller.DeviceGridController',
    'Tracker.controller.DeviceGridToolBarController'
    ]);

Ext.application({
    name: 'Tracker',
    appFolder: 'app',
    autoCreateViewport: false,
    controllers: ['DeviceGridController', 'DeviceGridToolBarController'],
    launch: function() {
        console.log('App lunch!');
        Ext.create('Tracker.view.TrackerViewport');
    }

});