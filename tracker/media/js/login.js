/**
 * An Login
 *
 * @author    Alexey Grebenshchikov
 * @copyright (c) 2008, by Alexey Grebenshchikov
 * @date      14 october 2010
 * @version   0.1
 *
 * @license login.js is licensed under the terms of the Open Source
 * LGPL 3.0 license. Commercial use is permitted to the extent that the
 * code/component(s) do NOT become part of another Open Source or Commercially
 * licensed development library or toolkit without explicit permission.
 *
 * License details: http://www.gnu.org/licenses/lgpl.html
 */

/*global Ext, Login */

Ext.BLANK_IMAGE_URL = './ext/resources/images/default/s.gif';
Ext.ns('Application');

// LoginPanel component
Application.LoginPanel = Ext.extend(Ext.FormPanel, {
    labelWidth:80,
    url:'/login/',
    frame:true,
    title:'Please Login',
    defaultType:'textfield',
    monitorValid:true,

    /**
     * Submit function
     */
    submit: function() {
        this.getForm().submit({
            url: this.url,
            method:'POST',
            scope: this,
            waitTitle:'Connecting',
            waitMsg:'Sending data...',
            success: function(){
                Ext.Msg.alert('Status', 'Login Successful!', function(btn, text){
                    if (btn == 'ok'){
                        //TODO заделать багу с редиректом
                        var redirect = '/tracker/';
                        window.location = redirect;
                    }
                });
            },
            failure: function(form, action){
                if(action.failureType == 'server'){
                    obj = Ext.util.JSON.decode(action.response.responseText);
                    Ext.Msg.alert('Login Failed!', obj.errors.reason);
                }else{
                    Ext.Msg.alert('Warning!', 'Authentication server is unreachable : ' + action.response.responseText);
                }
                this.getForm().reset();
            }
        });
    }, // eo submit function

    /**
     * Initializes the component.
     */
    initComponent: function() {
        var config = {
            buttons:[{
                text:'Login',
                formBind: true,
                scope: this,
                handler: this.submit
            }]
        };
        Ext.apply(this, Ext.apply(this.initialConfig, config));        
        Application.LoginPanel.superclass.initComponent.apply(this, arguments);
        this.items = [{
            fieldLabel:'Username',
            name:'username',
            allowBlank:false
        },{
            fieldLabel:'Password',
            name:'password',
            inputType:'password',
            allowBlank:false
        }];
        Application.LoginPanel.superclass.initComponent.call(this, arguments);
    } // eo initComponent

}); // eo LoginPanel component

Ext.reg('loginpanelxtype', Application.LoginPanel);

//login main entry point
Ext.onReady(function(){
    Ext.QuickTips.init();

    var win = new Ext.Window({
        layout:'fit',
        modal: true,
        width:300,
        height:150,
        closable: false,
        resizable: false,
        plain: true,
        border: false,
        items: {xtype: 'loginpanelxtype'}
    });
    win.show();
}); // eo function onReady

// eof