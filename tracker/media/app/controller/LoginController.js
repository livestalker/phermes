/**
 * @class Tracker.controller.LoginController
 * @extends Ext.app.Controller
 *
 Controller for handle login and register forms

 * @constructor
 * @param {Object} config The config object
 */
Ext.define('Tracker.controller.LoginController', {
    extend: 'Ext.app.Controller',
    refs: [
    {
        ref: 'loginForm',
        selector: 'loginform'
    },
    {
        ref: 'registerForm',
        selector: 'registerform'
    }
    ],
    views: [
    'form.RegisterForm',
    'form.LoginForm'
    ],
    init: function() {
        this.control({
            'registerform': {
                fieldvaliditychange: this.updateErrorState,
                fielderrorchange: this.updateErrorState
            },
            'loginform': {
                fieldvaliditychange: this.updateErrorState,
                fielderrorchange: this.updateErrorState
            },
            'loginform button': {
                click: this.login
            },
            'registerform button': {
                click: this.register
            }
        });
    },
    /**
     * Update error state of form (login or register form)
     * @param view
     */
    updateErrorState: function(view) {
        var errorCmp, fields, errors, errorStateId;
        if (view.hasBeenDirty || view.getForm().isDirty()) { //prevents showing global error when form first loads
            errorStateId = view.getErrorStateId();
            errorCmp = view.down(errorStateId);
            fields = view.getForm().getFields();
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
            view.hasBeenDirty = true;
        }
    },
    /**
     * Send data for login user
     */    
    login: function() {
        var form = this.getLoginForm().getForm();
        form.submit({
            clientValidation : true,
            url : '/',
            success : function(form, action) {
                // TODO make another redirect
                var redirect = '/tracker/';
                window.location = redirect;
            },
            failure : function(form, action) {
                if (action.failureType == 'server') {
                    obj = Ext.JSON.decode(action.response.responseText);
                    Ext.Msg.alert('Error!', obj.errors.reason);
                } else {
                    Ext.Msg.alert('Warning!', 'Authentication server is unreachable : ' + action.response.responseText);
                }
            }
        });

    },
    /**
     * Send data for registering user
     */
    register: function() {
        var form = this.getRegisterForm().getForm();
    }
});