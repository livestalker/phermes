Ext.Ajax.on('beforerequest', function (conn, options) {
    if (!(/^http:.*/.test(options.url) || /^https:.*/.test(options.url))) {
        options.headers = options.headers || {};
        options.headers["X-CSRFToken"] = Ext.util.Cookies.get('csrftoken');
    }
}, this);