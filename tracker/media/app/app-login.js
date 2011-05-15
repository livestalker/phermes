Ext.Loader.setConfig({
    enabled:true, 
    disableCaching: false
});
Ext.Loader.setPath({
    'Tracker': '/media/app'
});

Ext.require([
    'Tracker.view.LoginViewport',
    'Tracker.controller.LoginController'
    ]);

Ext.application({
    name: 'Tracker',
    appFolder: 'app',
    autoCreateViewport: false,
    controllers: [
    'LoginController'
    ],
    launch: function() {
        console.log('App lunch!');
        Ext.create('Tracker.view.LoginViewport');
    }

});

