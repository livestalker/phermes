Ext.Loader.setConfig({enabled:true, disableCaching: false});
Ext.Loader.setPath({
            'Tracker': '/media/app'
        });

Ext.require([
    'Tracker.view.Viewport',
    'Tracker.view.LoginForm',
    'Tracker.view.FormErrorState',
    'Tracker.view.RegisterForm',
    'Tracker.controller.LoginController'
]);

Ext.application({
            name: 'Tracker',
            appFolder: 'app',
            controllers: [
                'LoginController'
            ],
            launch: function() {
                console.log('App lunch!');
            }

        });

