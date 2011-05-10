/**
 * @class Ext.app.LoginForm
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

	items : [ {
		xtype : 'textfield',
		name : 'username',
		fieldLabel : 'User Name',
		allowBlank : false,
		minLength : 6
	}, {
		xtype : 'textfield',
		name : 'password',
		fieldLabel : 'Password',
		inputType : 'password',
		allowBlank : false
		// minLength : 8
		// TODO make min length of password
	} ],

	/*
	 * Listen for validity change on the entire form and update the combined
	 * error icon
	 */
	listeners : {
		fieldvaliditychange : function() {
			this.updateErrorState();
		},
		fielderrorchange : function() {
			this.updateErrorState();
		}
	},
	updateErrorState : function() {
		var me = this, errorCmp, fields, errors;

		if (me.hasBeenDirty || me.getForm().isDirty()) { // prevents showing
															// global error when
															// form first loads
			errorCmp = me.down('#formErrorLogin');
			fields = me.getForm().getFields();
			errors = [];
			fields.each(function(field) {
				var error = field.getErrors()[0];
				if (error) {
					errors.push({
						name : field.getFieldLabel(),
						error : error
					});
				}
			});
			errorCmp.setErrors(errors);
			me.hasBeenDirty = true;
		}
	},

	dockedItems : [ {
		xtype : 'container',
		dock : 'bottom',
		layout : {
			type : 'hbox',
			align : 'middle'
		},
		padding : '10 10 5',

		items : [ {
			xtype : 'formerrorstate',
			id : 'formErrorLogin'
		}, {
			xtype : 'button',
			formBind : true,
			disabled : true,
			text : 'Login',
			width : 140,
			handler : function() {
				var form = this.up('form').getForm();

				/*
				 * Normally we would submit the form to the server here and
				 * handle the response...
				 */
				form.submit({
					clientValidation : true,
					url : '/',
					success : function(form, action) {
                        // TODO make another redirect
                        var redirect = '/tracker/';
                        window.location = redirect;
					},
					failure : function(form, action) {
		                if(action.failureType == 'server'){
		                    obj = Ext.JSON.decode(action.response.responseText);
		                    Ext.Msg.alert('Error!', obj.errors.reason);
		                }else{
		                    Ext.Msg.alert('Warning!', 'Authentication server is unreachable : ' + action.response.responseText);
		                }		                
		                //form.reset();
					}
				});
			}
		} ]
	} ]
});