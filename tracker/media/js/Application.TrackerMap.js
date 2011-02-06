/**
 * TrackerMap
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

/* TrackerMap - map wrapper */

Ext.ns('Application');
Application.TrackerMap = Ext.extend(Object, {
    map             : null,                                     // map object
    baseLayer       : null,                                     // base layer
    devicesLayer    : null,                                     // layer fo devices markers
    viewProjection  : new OpenLayers.Projection("EPSG:4326"),   // default view projection
    devicesMarkers  : {},                                       // array of markers

    /**
     * constructor
     */
    constructor: function() {
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
        this.map = new OpenLayers.Map('map', options);
        this.baseLayer = new OpenLayers.Layer.OSM.Mapnik('Mapnik');
        var planeStyleMap = new OpenLayers.StyleMap({
            externalGraphic: "/images/marker.png",
            graphicWidth: 21,
            graphicHeight: 25,
            fillOpacity: 0.60,
            rotation: "${angle}"
        });
        this.devicesLayer = new OpenLayers.Layer.Vector('Devices', {
            projection: new OpenLayers.Projection("EPSG:4326"),
            visibility: true,
            displayInLayerSwitcher: false,
            styleMap: planeStyleMap
        });
        this.map.addLayers([this.baseLayer, this.devicesLayer]);
        this.setCenter(37.650417, 55.757276, 5);
    }, // eo constructor
    
    /**
     * set center of map
     */
    setCenter: function(lon, lat, zoom) {
        var lonlat = new OpenLayers.LonLat(lon, lat);
        this.map.setCenter(lonlat.transform(this.viewProjection,  this.map.projection), zoom);
    }, // eo setCenter
    
    /*
     * call when click on device in grid
     */
    refreshMarker: function(record) {
        var imei = record.data['imei'];
        var lat = parseFloat(record.data['latitude']);
        var lon = parseFloat(record.data['longitude']);
        var marker = null;
        var zoom = this.map.getZoom();
        if(this.devicesMarkers[imei] == null) {
            //marker = new OpenLayers.Marker(lonlat);
            var geometry = new OpenLayers.Geometry.Point(lon, lat).transform(this.viewProjection,  this.map.projection);
            marker = new OpenLayers.Feature.Vector(geometry, {
                angle: 0,
                poppedup: false
            });

            this.devicesMarkers[imei] = marker;
            this.devicesLayer.addFeatures(marker);
            this.setCenter(lon, lat, zoom);
        } else {
            var lonlat = new OpenLayers.LonLat(lon, lat).transform(this.viewProjection,  this.map.projection);
            marker = this.devicesMarkers[imei];
            marker.move(lonlat);
            this.setCenter(lon, lat, zoom);
        }
    },
    /*
     * get all devices markers
     */
    getDevicesMarkers: function() {
        return this.devicesMarkers;
    }
}); // eo TrackerMap