/**
 * @class Tracker.view.map.MapPanel
 * @extends Ext.panel.Panel
 *
 Class for display and work with map

 * @constructor
 * @param {Object} config The config object
 */

Ext.require([
    'Tracker.view.map.MPMenuBar',
    'Tracker.view.map.MPStatusBar'
    ]);
    
Ext.define('Tracker.view.map.MapPanel', {
    extend: 'Ext.panel.Panel',
    alias: 'widget.mappanel',
    
    itemId: 'mappanel',
    border: false,
    bodyPadding: 10,
    map: undefined,
    devicesMarkers: {},
    viewProjection: new OpenLayers.Projection("EPSG:4326"),
    dockedItems: [
    {
        xtype: 'mpstatusbar'
    },
    {
        xtype: 'mpmenubar'
    }
    ],
    /**
             * Init MapPanel
             */
    initComponent : function() {
        var defConfig = {
        //            plain: true,
        //            zoomLevel: 3,
        //            yaw: 180,
        //            pitch: 0,
        //            zoom: 0,
        //            gmapType: 'map',
        //            border: false
        };
        Ext.applyIf(this, defConfig);
        this.addListener('resize', this.resizeMap);
        this.callParent();
    },
    /**
             * Fires after MapPanel render
             */
    afterRender : function() {
        var wh = this.ownerCt.getSize();
        Ext.applyIf(this, wh);
        this.callParent();

        var options = {
            projection: new OpenLayers.Projection('EPSG:900913'),
            displayProjection: new OpenLayers.Projection('EPSG:4326'),
            units: 'dd',
            minResolution: 'auto',
            maxResolution: 'auto',
            controls: [
            new OpenLayers.Control.OverviewMap(),
            new OpenLayers.Control.MousePosition(),
            new OpenLayers.Control.Navigation(),
            new OpenLayers.Control.PanZoomBar(),
            new OpenLayers.Control.Attribution()
            ]
        };
        this.map = new OpenLayers.Map(this.body.dom, options);
        /*        var planeStyleMap = new OpenLayers.StyleMap({
            externalGraphic: '/media/img/markers/std_marker.png',
            graphicWidth: 21,
            graphicHeight: 25,
            fillOpacity: 1,
            rotation: "${angle}"
        });*/
        this.baseLayer = new OpenLayers.Layer.OSM.Mapnik('Mapnik', blOptions);
        /*        this.devicesLayer = new OpenLayers.Layer.Vector('Devices', {
            projection: new OpenLayers.Projection("EPSG:4326"),
            visibility: true,
            displayInLayerSwitcher: false,
            styleMap: planeStyleMap
        });*/
        this.markerLayer = new OpenLayers.Layer.Markers('Markers');
        var blOptions = {
            eventListeners: {
        }
        };
        this.map.addLayers([this.baseLayer, this.markerLayer]);
        this.setCenter(37.650417, 55.757276, 5);
    },
    resizeMap: function() {
        if (typeof this.map == 'object')
            this.map.updateSize();
    },
    setCenter: function(lonlat) {
        //var lonlat = new OpenLayers.LonLat(lon, lat);
        //this.map.setCenter(lonlat.transform(this.viewProjection, this.map.projection), zoom);
        this.map.setCenter(lonlat);
    },  
    getMarkerLayer: function() {
        return this.markerLayer;
    },
    createLonLat: function(lng, lat) {
        return new OpenLayers.LonLat(lng, lat).transform(this.viewProjection, this.map.projection);        
    }
});