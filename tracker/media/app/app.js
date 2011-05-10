Ext.Loader.setConfig({enabled:true});
Ext.Loader.setPath({
            'Tracker': '/media/app'
        });

Ext.require([
    'Tracker.view.Viewport',
    'Tracker.view.LoginForm',
    'Tracker.view.FormErrorState',
    'Tracker.view.RegisterForm',
    'Tracker.controller.RegisterForm'
]);

Ext.application({
            name: 'Tracker',
            appFolder: 'app',
            controllers: [
                'RegisterForm'
            ],
            launch: function() {
                console.log('App lunch!');
            }

        });

