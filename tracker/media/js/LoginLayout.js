Ext.require([ '*' ]);
Ext.onReady(function() {
	Ext.create('Ext.Viewport', {
		layout : {
			type : 'border',
			padding : 5
		},
		items : [ {
			region : 'north',
			bodyPadding : 10,
			border : false,
			frame : true
		}, {
			region : 'south',
			bodyPadding : 10,
			border : false,
			frame : true
		}, {
			region : 'center',
			border : false,
			styleHtmlContent : true,
			styleHtmlCls : 'bg-img'
		}, {
			region : 'east',
			width : 400,
			border : false,
			frame : true,
			layout : {
				type : 'vbox',
				padding : 5,
				align : 'stretch'
			},
			items : [ {
				xtype : 'loginform',
				style : 'margin-bottom:5px;'
			}, {
				xtype : 'registerform'
			} ]
		} ]
	});
});