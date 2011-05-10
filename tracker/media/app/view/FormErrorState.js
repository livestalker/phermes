/**
 * @class Tracker.view.FormErrorState
 * @extends Ext.container.Container
 *
 Class for display errors in form.

 * @constructor
 * @param {Object} config The config object
 */
Ext.define('Tracker.view.FormErrorState', {
    extend: 'Ext.container.Container',
    alias: 'widget.formerrorstate',
        
    baseCls: 'form-error-state',
    flex: 1,
    validText: 'Form is valid',
    invalidText: 'Form has errors',
    tipTpl: new Ext.XTemplate('<ul><tpl for="."><li><span class="field-name">{name}</span>: <span class="error">{error}</span></li></tpl></ul>'),

    getTip: function() {
        var tip = this.tip;
        if (!tip) {
            tip = this.tip = Ext.widget('tooltip', {
                target: this.el,
                title: 'Error Details:',
                autoHide: false,
                anchor: 'top',
                mouseOffset: [-11, -2],
                closable: true,
                cls: 'errors-tip'
            });
            tip.show();
        }
        return tip;
    },

    setErrors: function(errors) {
        var me = this,
        baseCls = me.baseCls,
        tip = me.getTip();

        errors = Ext.Array.from(errors);

        // Update CSS class and tooltip content
        if (errors.length) {
            me.addCls(baseCls + '-invalid');
            me.removeCls(baseCls + '-valid');
            me.update(me.invalidText);
            tip.setDisabled(false);
            tip.update(me.tipTpl.apply(errors));
        } else {
            me.addCls(baseCls + '-valid');
            me.removeCls(baseCls + '-invalid');
            me.update(me.validText);
            tip.setDisabled(true);
            tip.hide();
        }
    }
});