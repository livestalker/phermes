/**
 * @class Tracker.view.controls.MarkerImgComboBox
 * @extends Ext.form.field.ComboBox
 *
 Class for display and work with map markers.

 * @constructor
 * @param {Object} config The config object
 */

Ext.define('Tracker.view.controls.MarkerImgComboBox', {
    extend: 'Ext.form.field.ComboBox',
    alias: 'widget.markerimgcombobox',

    fieldLabel: 'Marker',
    displayField: 'name',
    valueField: 'marker_id',
    queryMode: 'local',
    editable: false,
    initComponent : function() {
        var defConfig = {
            store: {
                model: 'Tracker.model.MarkerImg',
                data: []
            }
        };
        Ext.applyIf(this, defConfig);
        this.callParent();
    },
    listConfig: {
        getInnerTpl: function() {
            var tpl = '<div class="x-combo-list-item">'
            + '<table><tbody><tr>'
            + '<td>'
            + '<div class="x-icon-combo-icon" style="width: {width}px; height: {height}px; background-image: url({url})"></div></td>'
            + '<td>{name} - {width}x{height}</td>'
            + '</tr></tbody></table>'
            + '</div>'
            return tpl;
        }
    },
    setData: function(data) {
        this.store.removeAll(true);
        this.store.add(data.items);
    }
});