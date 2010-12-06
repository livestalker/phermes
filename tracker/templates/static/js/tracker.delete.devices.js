/*
 * Удаление трекера и истории из базы данных
 */

$(document).ready(function() {    
    $('.delete-devices').click(function() {
        var device_id = $(this).attr('id');
        $("#dialog-confirm").dialog({
            resizable: false,
            height:160,
            modal: true,
            buttons: {
                'Удалить': function() {
                    $(this).dialog('close');
                    deleteRequest(device_id);                    
                },
                'Отмена': function() {
                    $(this).dialog('close');
                }
            }
        });
    });
});

function deleteRequest(device_id) {
    $.post('/tracker/delete', {
        device_id: device_id
    }, function(data) {
        var result = data['result'];
        if (result == 'ok') {
            var element = $('a#' + device_id)
                            .parent('td')
                            .parent('tr');
            element.css({'background-color' : 'red'});
            element.fadeOut(500, function() {
                $(this).remove();
            });

        }
    }, 'json');
}