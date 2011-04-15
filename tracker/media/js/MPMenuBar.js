Ext.define('Ext.app.MPMenuBar', {
            extend: 'Ext.toolbar.Toolbar',
            alias: 'widget.mpmenubar',
            dock: 'top',
            padding: '0 0 5 0',
            enableOverflow: true,
            items: [
                '->',
                {
                    text: 'Help',
                    menu: {
                        items: [
                            {text: 'Documentation'},
                            {text: 'Feedback'},
                            {text: 'Features request'},
                            '-',
                            {text: 'About pHermes'}
                        ]}
                }
            ]
        }
);