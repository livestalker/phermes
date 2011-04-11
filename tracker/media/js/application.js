/**
 * Application
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

application = function() {
    // do NOT access DOM from here; elements don't exist yet
    Ext.tip.QuickTips.init();
    // private variables
    // private functions
    // public space

    return {
        // public properties, e.g. strings to translate

        // public methods
        init: function() {
            Ext.create('Ext.app.Layout');
        }
    };
}(); //eo app

// application main entry point
Ext.onReady(application.init, application); // eo function onReady