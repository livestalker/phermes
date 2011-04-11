Ext.define('Ext.app.MapPanel', {
    extend: 'Ext.panel.Panel',
    alias: 'widget.mappanel',
    bodyPadding: 10,
    viewProjection  : new OpenLayers.Projection("EPSG:4326"),
    
    initComponent : function(){        
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
    
    afterRender : function(){
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
        var blOptions = {
            eventListeners: {
            }
        }
        this.baseLayer = new OpenLayers.Layer.OSM.Mapnik('Mapnik', blOptions);
        this.map.addLayers([this.baseLayer]);
        this.setCenter(37.650417, 55.757276, 5);
    },
    resizeMap: function() {
        if (typeof this.map == 'object')
            this.map.updateSize();
    },
    
    setCenter: function(lon, lat, zoom) {
        var lonlat = new OpenLayers.LonLat(lon, lat);
        this.map.setCenter(lonlat.transform(this.viewProjection,  this.map.projection), zoom);
    }
});