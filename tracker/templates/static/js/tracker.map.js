//fields of device: val - id device, name - название девайса, placemark - position on map, show - show device
var devices = {};
var tracks = {};
var group = [];
var map = null;

$(document).ready(main);

function main() {
    map = new YMaps.Map(YMaps.jQuery("#YMapsID")[0]);
    map.setCenter(new YMaps.GeoPoint(37.64, 55.76), 10);
    initToolbar();

    $('#devices').tabs();

    $('#devices .device').each(function() {
        //define button variables
        var devButton = $(this).children('button:first');
        var showButton = devButton.next();
        var groupButton = showButton.next();
        var trackButton = groupButton.next().next();
        //define style for buttons
        devButton.button({
            icons: {
                primary: 'ui-icon-radio-on'
            }
        }).css({
            'width':'400px'
        });
        showButton.button({
            icons: {
                primary: 'ui-icon-flag'
            },
            text: false,
            disabled: true
        });
        groupButton.button({
            icons: {
                primary: 'ui-icon-pin-w'
            },
            text: false,
            disabled: true
        });
        trackButton.button({
            icons: {
                primary: 'ui-icon-transfer-e-w'
            },
            text: false,
            disabled: true
        });
        //init devices object
        devices[devButton.attr('id')] = {
            val: devButton.attr('id'),
            name: '',
            placemark: null,
            show: false
        };
        //event of device button
        devButton.click(function() {
            var id = $(this).attr('id');
            if (!devices[id].show) {
                devices[id].show = true;
                request(id);
                $(this).button( "option", "icons", {
                    primary:'ui-icon-check'
                });
                $(this).next().button( "option", "disabled", false );
                $(this).next().next().button( "option", "disabled", false );
                $(this).next().next().next().next().button( "option", "disabled", false );
            }else {
                devices[id].show = false;
                removePlacemark(id);
                $(this).button( "option", "icons", {
                    primary:'ui-icon-radio-on'
                });
                $(this).next().button( "option", "disabled", true );
                $(this).next().next().button( "option", "disabled", true );
                $(this).next().next().next().next().button( "option", "disabled", true );
            }
        });
        //event of show button
        showButton.click(function() {
            var id = getVal($(this));
            var placemark = devices[id].placemark;
            if (group.length != 0){
                showGroup();
            } else {
                map.setCenter(placemark.getGeoPoint(), map.getZoom());
            }
        });
        //event of group button
        groupButton.click(function() {
            var checked = $(this).is(':checked');
            if (checked) {
                $(this).button("option", "icons", {
                    primary:'ui-icon-pin-s'
                });
                showGroup();
            } else {
                $(this).button("option", "icons", {
                    primary:'ui-icon-pin-w'
                });
                showGroup();
            }
        });
        //event of track button
        trackButton.click(function() {
            var checked = $(this).is(':checked');
            if (checked) {
                $(this).button("option", "icons", {
                    primary:'ui-icon-transferthick-e-w'
                });
                showTrack($(this).val());
            } else {
                $(this).button("option", "icons", {
                    primary:'ui-icon-transfer-e-w'
                });
                delTrack($(this).val());
            }
        });
    });
}

//init toolbar of map
function initToolbar() {
    var toolBar = new YMaps.ToolBar();
    var zoomControl = new YMaps.Zoom();
    map.addControl(toolBar);
    map.addControl(zoomControl);
}

//request to db
function request(id) {
    $.post('/tracker/request', {
        id: id
    }, function(data) {
        //parse json data
        latitude = parseFloat(data['latitude']);
        longitude = parseFloat(data['longitude']);
        //set center map = logitude x latitude of object
        map.setCenter(new YMaps.GeoPoint(longitude, latitude), map.getZoom());
        var placemark = new YMaps.Placemark(new YMaps.GeoPoint(longitude, latitude));
        //content of baloon
        placemark.name = data['name'];
        placemark.description = "<b>Скорость: </b>100 км/ч";
        removePlacemark(id);
        // Добавляет метку на карту
        devices[id].name = data['name'];
        devices[id].placemark = placemark;
        map.addOverlay(placemark);
    }, 'json');
}

//remove placemark
function removePlacemark(id){
    map.removeOverlay(devices[id].placemark);
    devices[id].placemark = null;
    map.removeOverlay(tracks[id]);
    tracks[id] = null;
    $('#track' + id)
    .attr('checked', false)
    .button("option", "icons", {
        primary:'ui-icon-transfer-e-w'
    });
    $('#group' + id)
    .attr('checked', false)
    .button("option", "icons", {
        primary:'ui-icon-pin-w'
    });
}

//ie hack getVal for buttons
function getVal(element){
    if($.browser.msie) {
        var label = element.val();
        element.text('');
        var result = element.val();
        element.val(label);
        return result;
    } else
        return element.val();
}

function showGroup() {
    group = [];
    var i = 0;
    $('input[type=checkbox]').filter('input[id^=group]:checked').each(function() {
        var id = $(this).val();
        group[i++] = devices[id].placemark.getGeoPoint();
    });
    if (group.length >0 ) {
        var zoomOld = map.getZoom();
        var bound = new YMaps.GeoCollectionBounds(group);
        map.setBounds(bound);
        var zoomNew = map.getZoom();
        if (zoomOld <= zoomNew)
            map.setZoom(zoomOld);
        else
            map.setZoom(zoomNew - 1);
    }
}

function showTrack(id) {
    $.post('/tracker/track', {
        id: id
    }, function(data) {
        encodePoints(data);
    }, 'json');
}

function delTrack(id) {
    map.removeOverlay(tracks[id]);
    tracks[id] = null;
}

function encodePoints(data) {
    var history = data['history'],  // history from json
    device_id = data['device_id'],    // history from json
    points = [],                    // array of GeoPoints
    array = [],                     // template array
    prev = new YMaps.Point(0,0),    // prev point
    coef = 1000000,                 // coef
    visableString = '';

    //create array of GeoPoints
    for (var j = 0; j < history.length; j++) {
        points[j] = new YMaps.GeoPoint(history[j]['longitude'], history[j]['latitude']);
        visableString += 'A';
    }

    //processing points
    for (var i = 0, geoVector, currentPoint; i < points.length; i++) {
        currentPoint = points[i].copy();

        //find offset current point reletively previose point
        geoVector = currentPoint.diff(prev).neg();

        //multiply every point on coef and encoding
        array = array.concat(Base64.encode4bytes(geoVector.getX() * coef), Base64.encode4bytes(geoVector.getY() * coef));
        prev = currentPoint;
    }

    // all array encoding in Base64, and create poly line
    var polyline = YMaps.Polyline.fromEncodedPoints(Base64.encode(array), visableString);
    var s = new YMaps.Style();
    s.lineStyle = new YMaps.LineStyle();
    s.lineStyle.strokeColor = '0000FF55';
    s.lineStyle.strokeWidth = '5';
    YMaps.Styles.add('trackstyle', s);
    polyline.setStyle('trackstyle');    
    polyline.name = 'Маршрут: ' + devices[device_id].name;
    polyline.description = 'Discription';
    tracks[device_id] = polyline;
    map.addOverlay(polyline);
}

// call for work with Base64
// based on http://www.webtoolkit.info/
var Base64 = new function () {
    var _keyStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_=";

    this.encode4bytes = function (x) {
        var chr = [];
        for (var i = 0; i < 4; i++) {
            chr[i] = x & 0x000000ff;
            x = x >> 8;
        }
        return chr;
    }

    this.encode = function (input) {
        var output = "",
        chr1, chr2, chr3, enc1, enc2, enc3, enc4,
        i = 0,
        inputIsString = typeof input == "string";

        while (i < input.length) {
            chr1 = input[i++];
            chr2 = input[i++];
            chr3 = input[i++];

            enc1 = chr1 >> 2
            enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
            enc3 = ((chr2 & 15) << 2) | (chr3 >> 6)
            enc4 = chr3 & 63;

            if (isNaN(chr2)) {
                enc3 = enc4 = 64;
            } else if (isNaN(chr3)) {
                enc4 = 64;
            }

            output +=
            _keyStr.charAt(enc1) + _keyStr.charAt(enc2) +
            _keyStr.charAt(enc3) + _keyStr.charAt(enc4);

        }

        return output;
    }
}