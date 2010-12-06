/**
 * Application
 *
 * @author    Alexey Grebenshchikov
 * @copyright (c) 2008, by Alexey Grebenshchikov
 * @date      14 october 2010
 * @version   0.1
 *
 * @license login.js is licensed under the terms of the Open Source
 * LGPL 3.0 license. Commercial use is permitted to the extent that the
 * code/component(s) do NOT become part of another Open Source or Commercially
 * licensed development library or toolkit without explicit permission.
 *
 * License details: http://www.gnu.org/licenses/lgpl.html
 */

/*global Ext, Application */

Ext.BLANK_IMAGE_URL = './ext/resources/images/default/s.gif';
Ext.ns('Application');

// custom control layouts
//------------------------------------------------------------------------------
/**
 * GridDevices
 */
Application.GridDevices = Ext.extend(Ext.grid.GridPanel, {
    id: 'devices-gridpanel',
    region:'north',
    title: 'GPS devices',    
    split: true,
    height: 300,
    autoScroll: true,
    //loadMask: true,
    columnLines: true,
    map: null,
    sm: new Ext.grid.RowSelectionModel({
        singleSelect:true
    }),
    autoExpandColumn: 'discription',
    colModel: new Ext.grid.ColumnModel({
        defaults: {
            width: 120,
            sortable: true
        },
        columns: [
        {
            id: 'name',
            header: 'Name',
            dataIndex: 'name'
        },
        {
            header: 'IMEI',
            sortable: false,
            dataIndex: 'imei',
            hidden: true
        },
        {
            id: 'discription',
            header: 'Discription',
            dataIndex: 'text'
        }]
    }),
    bbar: new Ext.Toolbar({
        enableOverflow: true,
        items: [{
            text: 'Add',
            iconCls: 'adddevice'
        },{
            text: 'Edit',
            iconCls: 'editdevice'
        },{
            text: 'Delete',
            iconCls: 'deldevice'
        }]
    }),
    /**
     * Initializes the component.
     */
    initComponent: function() {
        //configure store
        Ext.apply(this, {
            store: new Ext.data.JsonStore({
                // config of store
                proxy : new Ext.data.HttpProxy({
                    method: 'POST',
                    url: '/tracker/list'
                }),
                autoDestroy: true,
                autoLoad: false,
                storeId: 'devicesStore',
                // config of reader
                fields: ['device_id', 'imei', 'name', 'text', 'longitude', 'latitude', 'ts_time']
            }),
            updateTask: {
                scope: this,
                run: function() {
                    this.getStore().reload();
                },
                interval: 30000 //1 minute
            }
        });
        var runner = new Ext.util.TaskRunner();
        runner.start(this.updateTask);
        Application.GridDevices.superclass.initComponent.call(this, arguments);
    }, //eo initializes the component GridDevices
    /**
     * init map
     */
    setMap: function(map) {
        this.map = map;
    }
}); //eo GridDevices

Ext.reg('griddevices-xtype', Application.GridDevices);

//------------------------------------------------------------------------------
/**
 * DetailsPanel
 */

Application.DetailsPanel = Ext.extend(Ext.Panel, {
    id: 'details-panel',
    region: 'center',
    title: 'Details',    
    bodyStyle: 'padding-bottom:15px;background:#eee;',
    autoScroll: true,
    html: '<p class="details-info">When you select a layout from the tree, additional details will display here.</p>',
    discription: new Ext.XTemplate('')
}); //eo DetailsPanel

Ext.reg('detailspanel-xtype', Application.DetailsPanel);

//------------------------------------------------------------------------------
/**
 * DevicesPanel
 */

Application.DevicesPanel = Ext.extend(Ext.Panel, {
    layout: 'border',
    border: false,
    split: true,
    margins: '2 0 5 5',
    width: 300,
    map: null,
    devicesGrid: new Application.GridDevices,
    detailsPanel: new Application.DetailsPanel,
    detailsTemplate: new Ext.XTemplate('<p class="details-info">'
        + 'Name: {name}<br>'
        + 'Discription: {text}<br>'
        + 'IMEI: {imei}'
        + '</p>'
        + '<p class="details-info">'
        + 'Last position:<br>'
        + 'Longitude: {longitude} Latitude: {latitude}<br>'
        + 'Time: {ts_time}<br>'
        + '</p>'),

    /**
     * Initializes the component.
     */
    initComponent: function() {
        this.items = [this.devicesGrid, this.detailsPanel];
        this.devicesGrid.on({
            scope: this,
            rowclick: this.rowClickEvent
        });
        Application.DevicesPanel.superclass.initComponent.call(this, arguments);
    }, // eo initComponent
    
    // methods
    /**
     * init map
     */
    setMap: function(map) {
        this.map = map;
        this.devicesGrid.setMap(map);
    },

    // events
    /**
     * CellClick Event
     */
    rowClickEvent: function(grid, rowIndex, e) {
        var record = grid.getStore().getAt(rowIndex);
        this.detailsTemplate.overwrite(this.detailsPanel.body, record.data);
        this.map.refreshMarker(record);
    } //eo CellClick Event

}); //eo DevicesPanel

Ext.reg('devicespanel-xtype', Application.DevicesPanel);

//------------------------------------------------------------------------------
/**
 * MapPanel
 */
Application.MapPanel = Ext.extend(Ext.Panel, {
    id: 'content-panel',
    margins: '2 5 5 0',
    html: '<div id="map"></div>',
    map: null,
    // methods
    /**
     * set ref to map
     */
    setMap: function(map) {
        this.map = map;
    },
    /**
     * Initializes the component.
     */
    initComponent: function() {

        Application.DevicesPanel.superclass.initComponent.call(this, arguments);
    } // eo initComponent

}); //eo MapPanel

Ext.reg('mappanel-xtype', Application.MapPanel);

Application.app = function() {
    // do NOT access DOM from here; elements don't exist yet
    Ext.QuickTips.init();
    // private variables
    // private functions
    // public space

    return {
        // public properties, e.g. strings to translate

        // public methods
        init: function() {
            var devicePanel = new Application.DevicesPanel({
                region: 'west'
            });
            var mapPanel = new Application.MapPanel({
                region: 'center'
            });
            new Ext.Viewport({
                layout: 'border',
                title: 'Personal GPS tracking system',
                items: [devicePanel, mapPanel],
                renderTo: Ext.getBody()
            });
            var trackerMap = new Application.TrackerMap();
            devicePanel.setMap(trackerMap);
            mapPanel.setMap(trackerMap);
        }
    };
}(); //eo app

// application main entry point
Ext.onReady(Application.app.init, Application.app); // eo function onReady

// eof

function addMarker(map, layer, lon, lat, popupContentHTML) {

    var ll = new OpenLayers.LonLat(lon, lat);
    var feature = new OpenLayers.Feature(layer, ll);
    feature.closeBox = true;
    feature.popupClass = OpenLayers.Class(OpenLayers.Popup.FramedCloud, {
        minSize: new OpenLayers.Size(300, 180)
    } );
    feature.data.popupContentHTML = popupContentHTML;
    feature.data.overflow = "hidden";

    var marker = new OpenLayers.Marker(ll);
    marker.feature = feature;

    var markerClick = function(evt) {
        if (this.popup == null) {
            this.popup = this.createPopup(this.closeBox);
            map.addPopup(this.popup);
            this.popup.show();
        } else {
            this.popup.toggle();
        }
        OpenLayers.Event.stop(evt);
    };
    marker.events.register("mousedown", feature, markerClick);

    layer.addMarker(marker);
    map.addPopup(feature.createPopup(feature.closeBox));
}