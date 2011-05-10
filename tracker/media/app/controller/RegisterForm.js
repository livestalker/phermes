Ext.define('Tracker.controller.RegisterForm', {
            extend: 'Ext.app.Controller',
            views: [
                'RegisterForm'
            ],
            refs: [
                {
                    ref: 'form',
                    selector: 'form'
                }
            ],
            init: function() {
                this.control({
                            'registerform': {
                                //fieldvaliditychange: this.updateErrorState(),
                                fielderrorchange: this.updateErrorState
                            }
                        });
            },
            updateErrorState: function() {
                var t = this.getForm();
                alert(t);
                //var t = this.getView();
                //alert(t);
                /*                var me = this,
                 errorCmp, fields, errors;

                 if (me.hasBeenDirty || me.getForm().isDirty()) { //prevents showing global error when form first loads
                 errorCmp = me.down('#formErrorRegister');
                 fields = me.getForm().getFields();
                 errors = [];
                 fields.each(function(field) {
                 var error = field.getErrors()[0];
                 if (error) {
                 errors.push({
                 name: field.getFieldLabel(),
                 error: error
                 });
                 }
                 });
                 errorCmp.setErrors(errors);
                 me.hasBeenDirty = true;
                 }*/
            }
        });