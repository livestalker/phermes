/**
 * @class Ext.app.MarkerComboBox
 * @extends Ext.form.field.ComboBox
 *
 Class for display and work with tracker devices.

 * @constructor
 * @param {Object} config The config object
 */

Ext.define('Ext.app.MarkerComboBox', {
            extend: 'Ext.form.field.ComboBox',
            alias: 'widget.markercombobox',

            fieldLabel: 'Marker',
            displayField: 'name',
            queryMode: 'local',
            initComponent : function() {
                var defConfig = {
                    store: {
                        model: 'Marker',
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
                    //return '<div data-qtip="{name}. {slogan}">{name} ({abbr})</div>';
                    return tpl;
                }
            },
            setData: function(data) {
                this.store.removeAll(true);
                this.store.add(data.items);
            }
        });