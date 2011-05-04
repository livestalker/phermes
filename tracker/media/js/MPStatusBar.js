/**
 * @class Ext.app.MPStatusBar
 * @extends Ext.toolbar.Toolbar
 *
 Class for display status bar

 * @constructor
 * @param {Object} config The config object
 */
Ext.define('Ext.app.MPStatusBar', {
            extend: 'Ext.toolbar.Toolbar',
            alias: 'widget.mpstatusbar',
            dock: 'bottom',
            items: [
                'Status bar'
            ]
        }
);
