Ext.define('Tracker.view.panel.InfoPanel', {
    extend: 'Ext.panel.Panel',
    alias: 'widget.infopanel',
    
    itemId: 'infopanel',
    id: 'infopanel',
    title: 'Details',
    bodyStyle: 'padding-bottom:15px;background:#eee;',            
    html: '<p class="details-info">When you select a device, additional details will display here.</p>',
    autoScroll: true
});