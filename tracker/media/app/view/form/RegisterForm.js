/**
 * @class Tracker.view.form.RegisterForm
 * @extends Ext.form.FormPanel
 *
 Class for display register form

 * @constructor
 * @param {Object} config The config object
 */

Ext.require([
    'Tracker.view.form.FormErrorState',
    ]);
    
Ext.define('Tracker.view.form.RegisterForm', {
    extend: 'Ext.form.FormPanel',
    alias: 'widget.registerform',
    bodyPadding: 10,
    frame: true,
    title: 'Account Registration',

    defaults: {
        anchor: '100%'
    },
    fieldDefaults: {
        labelAlign: 'left',
        msgTarget: 'none',
        invalidCls: ''
    },
    items: [
    {
        xtype: 'textfield',
        name: 'rusername',
        fieldLabel: 'User Name',
        allowBlank: false,
        minLength: 6
    },
    {
        xtype: 'textfield',
        name: 'remail',
        fieldLabel: 'Email Address',
        vtype: 'email',
        allowBlank: false
    },
    {
        xtype: 'textfield',
        name: 'password1',
        fieldLabel: 'Password',
        inputType: 'password',
        style: 'margin-top:15px',
        allowBlank: false,
        minLength: 8
    },
    {
        xtype: 'textfield',
        name: 'password2',
        fieldLabel: 'Repeat Password',
        inputType: 'password',
        allowBlank: false,
        /**
                     * Custom validator implementation - checks that the value matches what was entered into
                     * the password1 field.
                     */
        validator: function(value) {
            var password1 = this.previousSibling('[name=password1]');
            return (value === password1.getValue()) ? true : 'Passwords do not match.'
        }
    },

    /*
                 * Terms of Use acceptance checkbox. Two things are special about this:
                 * 1) The boxLabel contains a HTML link to the Terms of Use page; a special click listener opens this
                 *    page in a modal Ext window for convenient viewing, and the Decline and Accept buttons in the window
                 *    update the checkbox's state automatically.
                 * 2) This checkbox is required, i.e. the form will not be able to be submitted unless the user has
                 *    checked the box. Ext does not have this type of validation built in for checkboxes, so we add a
                 *    custom getErrors method implementation.
                 */
    {
        xtype: 'checkboxfield',
        name: 'acceptTerms',
        fieldLabel: 'Terms of Use',
        hideLabel: true,
        style: 'margin-top:15px',
        boxLabel: 'I have read and accept the <a href="http://www.sencha.com/legal/terms-of-use/" class="terms">Terms of Use</a>.',

        // Listener to open the Terms of Use page link in a modal window
        listeners: {
            click: {
                element: 'boxLabelEl',
                fn: function(e) {
                    var target = e.getTarget('.terms'),
                    win;
                    if (target) {
                        win = Ext.widget('window', {
                            title: 'Terms of Use',
                            modal: true,
                            html: '<iframe src="' + target.href + '" width="950" height="500" style="border:0"></iframe>',
                            buttons: [
                            {
                                text: 'Decline',
                                handler: function() {
                                    this.up('window').close();
                                    formPanel.down('[name=acceptTerms]').setValue(false);
                                }
                            },
                            {
                                text: 'Accept',
                                handler: function() {
                                    this.up('window').close();
                                    formPanel.down('[name=acceptTerms]').setValue(true);
                                }
                            }
                            ]
                        });
                        win.show();
                        e.preventDefault();
                    }
                }
            }
        },

        // Custom validation logic - requires the checkbox to be checked
        getErrors: function() {
            return this.getValue() ? [] : ['You must accept the Terms of Use']
        }
    }
    ],

    dockedItems: [
    {
        xtype: 'container',
        dock: 'bottom',
        layout: {
            type: 'hbox',
            align: 'middle'
        },
        padding: '10 10 5',

        items: [
        {
            xtype: 'formerrorstate',
            id: 'formErrorRegister'
        },
        {
            xtype: 'button',
            formBind: true,
            disabled: true,
            text: 'Submit Registration',
            width: 140
        }
        ]
    }
    ],
    getErrorStateId: function() {
        return '#formErrorRegister';
    }
});