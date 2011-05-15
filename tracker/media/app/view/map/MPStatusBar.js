/**
 * @class Tracker.view.map.MPStatusBar
 * @extends Ext.toolbar.Toolbar
 *
 Class for display status bar

 * @constructor
 * @param {Object} config The config object
 */
Ext.define('Tracker.view.map.MPStatusBar', {
            extend: 'Ext.toolbar.Toolbar',
            alias: 'widget.mpstatusbar',
            dock: 'bottom',
            items: [
                'Status bar'
            ]
        }
);
