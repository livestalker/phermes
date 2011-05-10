/**
 * @class Tracker.view.LoginForm
 * @extends Ext.form.FormPanel
 *
 Class for display login form

 * @constructor
 * @param {Object} config The config object
 */
Ext.define('Tracker.view.LoginForm', {
            extend : 'Ext.form.FormPanel',
            alias : 'widget.loginform',
            bodyPadding : 10,
            frame : true,
            title : 'Login',

            defaults : {
                anchor : '100%'
            },
            fieldDefaults : {
                labelAlign : 'left',
                msgTarget : 'none',
                invalidCls : ''
            },
            items : [
                {
                    xtype : 'textfield',
                    name : 'username',
                    fieldLabel : 'User Name',
                    allowBlank : false,
                    minLength : 6
                },
                {
                    xtype : 'textfield',
                    name : 'password',
                    fieldLabel : 'Password',
                    inputType : 'password',
                    allowBlank : false
                    // minLength : 8
                    // TODO make min length of password
                }
            ],
            dockedItems : [
                {
                    xtype : 'container',
                    dock : 'bottom',
                    layout : {
                        type : 'hbox',
                        align : 'middle'
                    },
                    padding : '10 10 5',

                    items : [
                        {
                            xtype : 'formerrorstate',
                            id : 'formErrorLogin'
                        },
                        {
                            xtype : 'button',
                            formBind : true,
                            disabled : true,
                            text : 'Login',
                            width : 140
                        }
                    ]
                }
            ],
            getErrorStateId: function() {
                return '#formErrorLogin';
            }
        });