/**
 * @class Ext.app.MapPanel
 * @extends Ext.panel.Panel
 *
 Class for display and work with map

 * @constructor
 * @param {Object} config The config object
 */
Ext.define('Ext.app.MapPanel', {
            extend: 'Ext.panel.Panel',
            alias: 'widget.mappanel',

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
                var planeStyleMap = new OpenLayers.StyleMap({
                            externalGraphic: '/media/img/marker.png',
                            graphicWidth: 21,
                            graphicHeight: 25,
                            fillOpacity: 1,
                            rotation: "${angle}"
                        });
                this.baseLayer = new OpenLayers.Layer.OSM.Mapnik('Mapnik', blOptions);
                this.devicesLayer = new OpenLayers.Layer.Vector('Devices', {
                            projection: new OpenLayers.Projection("EPSG:4326"),
                            visibility: true,
                            displayInLayerSwitcher: false,
                            styleMap: planeStyleMap
                        });
                var blOptions = {
                    eventListeners: {
                    }
                };
                this.map.addLayers([this.baseLayer, this.devicesLayer]);
                this.setCenter(37.650417, 55.757276, 5);
            },
            /**
             * Update size of div (which contain map) when panel resize
             */
            resizeMap: function() {
                if (typeof this.map == 'object')
                    this.map.updateSize();
            },
            /**
             * Set center of map
             * @param {Number} lon Longitude
             * @param {Number} lat Latitude
             * @param {Number} zoom Scale of map
             */
            setCenter: function(lon, lat, zoom) {
                var lonlat = new OpenLayers.LonLat(lon, lat);
                this.map.setCenter(lonlat.transform(this.viewProjection, this.map.projection), zoom);
            },
            /**
             * Create marker or refresh position (if marker exist)
             * @param {Ext.data.Record} record
             */
            refreshMarker: function(record) {
                // TODO long issue
                var imei = record.data['imei'];
                var lat = parseFloat(record.data['lat']);
                var lon = parseFloat(record.data['long']);
                var marker = null;
                if (this.devicesMarkers[imei] == null) {
                    var geometry = new OpenLayers.Geometry.Point(lon, lat).transform(this.viewProjection, this.map.projection);
                    marker = new OpenLayers.Feature.Vector(geometry, {
                                angle: 0,
                                poppedup: false
                            });

                    this.devicesMarkers[imei] = marker;
                    this.devicesLayer.addFeatures(marker);
                } else {
                    var lonlat = new OpenLayers.LonLat(lon, lat).transform(this.viewProjection, this.map.projection);
                    marker = this.devicesMarkers[imei];
                    marker.move(lonlat);
                }
            },
            /**
             * Move map to marker place
             * @param {Ext.data.Record} record
             */
            selectMarker: function(record) {
                // TODO Optimize this place
                this.refreshMarker(record);
                var lat = parseFloat(record.data['lat']);
                var lon = parseFloat(record.data['long']);
                var zoom = this.map.getZoom();
                this.setCenter(lon, lat, zoom);
            },
            /**
             * Return array of device markers on device layer
             * @return {Array} deviceMarkers
             */
            getDevicesMarkers: function() {
                return this.devicesMarkers;
            }
        });