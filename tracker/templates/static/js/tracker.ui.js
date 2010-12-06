$(document).ready(function() {
    $('.tracker-error').hide().fadeIn(1000);
    $('input:submit').button();
    //style for table
    $('tr:odd').addClass('odd');
    $('tr:even:gt(0)').addClass('even');
    $('tr:gt(0)').mouseover(function() {
        $(this).addClass('tr-in');
    });
    $('tr:gt(0)').mouseout(function() {
        $(this).removeClass('tr-in');
    });
});